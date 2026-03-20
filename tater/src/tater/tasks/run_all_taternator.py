import argparse
import datetime as dt
import getpass
import json
import pathlib
import pickle
import textwrap

from jobmon.client.tool import Tool

from tater.task_arguments import (
    add_run_all_args,
    get_run_all_args,
)
from tater.cache import (
    USHDCache,
    CauseRiskMetadataLoader,
)


class TaterPhaseWorkflows:
    """
    Handles running workflows for the various phases.
    """
    @classmethod
    def from_args_and_cache(cls, args, cache: USHDCache):
        phases = args.run_phases
        user = getpass.getuser()
        return cls(args, cache, user=user, phases=phases)

    def __init__(self, args, cache, *, user, phases):
        self.args = args
        self.cache = cache
        self.user = user
        self.phases = phases

        self.cached_data = self._get_cached_data()

    def create_workflow(self):
        """
        Create a workflow that runs all specified phases.
        """
        self.tool = self._create_tool()
        workflow = self.tool.create_workflow(
            name=self.args.workflow_name,
            max_concurrently_running=self.args.max_concurrent,
            workflow_args=self.args.workflow_args,
        )

        if 'most_detailed' in self.phases:
            most_detailed_lookup = self._add_most_detailed_tasks(self.tool, workflow, self.args)
        else:
            most_detailed_lookup = {}

        if 'loc_agg' in self.phases:
            loc_agg_lookup = self._add_loc_agg_tasks(self.tool, workflow, self.args, most_detailed_lookup)
        else:
            loc_agg_lookup = {}

        if 'cleanup' in self.phases:
            cleanup_lookup = self._add_cleanup_tasks(self.tool, workflow, self.args, loc_agg_lookup)
        else:
            cleanup_lookup = None

        if 'upload' in self.phases:
            self._add_upload_tasks(self.tool, workflow, self.args, cleanup_lookup)

        return workflow

    # helper methods
    def _create_tool(self):
        return Tool(name='tatornator')

    def _add_most_detailed_tasks(self, tool, workflow, args):
        most_detailed_command_template = textwrap.dedent("""
            run_taternator_most_detailed
                --tool_name burdenator
                --location_id {location_id}
                --year_id {year_id}
                --n_draws {n_draws}
                -o {versioned_out_dir}
                --cache_dir {cache_dir}
                --measure_ids {measure_ids}
                --fatal_version_id {fatal_version_id}
                --rake_version_id {rake_version_id}
                --nonfatal_run_id {nonfatal_run_id}
                --paf_compile_run_id {paf_compile_run_id}
                --extra_dim {extra_dim}
                --age_group_set_id {age_group_set_id}
                --gbd_release_id {gbd_release_id}                
            """).replace("\n", " ")

        if args.verbose:
            most_detailed_command_template += " --verbose"

        most_detailed_template = tool.get_task_template(
            default_compute_resources={
                'queue': args.queue,
                # provide 1 core to each measure + 1 core for the main process
                # note: program is parallelized via multiprocess.Pool over measure_id
                'cores': len(args.measure_ids) + 1,
                'memory': f"{3 * len(args.measure_ids)}G",
                'runtime': 60 * 60 * 3,
                'stdout': f"{args.versioned_out_dir}/log_most_detailed",
                'stderr': f"{args.versioned_out_dir}/log_most_detailed",
                'project': args.project,
            },
            template_name='run_taternator_most_detailed',
            default_cluster_name='slurm',
            command_template=most_detailed_command_template,
            node_args=['location_id', 'year_id'],
            task_args=[
                'n_draws',
                'versioned_out_dir',
                'cache_dir',
                'measure_ids',
                'fatal_version_id',
                'rake_version_id',
                'nonfatal_run_id',
                'paf_compile_run_id',
                'extra_dim',
                'age_group_set_id',
                'gbd_release_id',                
            ],
        )

        loc_tree, = self.cached_data['location_hierarchy']

        task_args_kw = dict(
            n_draws=args.n_draws,
            versioned_out_dir=args.versioned_out_dir,
            cache_dir=args.cache_dir,
            measure_ids=",".join(str(m_id) for m_id in args.measure_ids),
            fatal_version_id=args.fatal_version_id,
            rake_version_id=args.rake_version_id,
            nonfatal_run_id=args.nonfatal_run_id,
            paf_compile_run_id=args.paf_compile_run_id,
            extra_dim=args.extra_dim,
            age_group_set_id=args.age_group_set_id,
            gbd_release_id=args.gbd_release_id,
        )

        # create tasks. lump them into an easy lookup list for the following task loc_agg
        #
        # most_detailed runs by year/location (all measure_ids/rei_ids simultaneously)
        #
        # loc_agg runs by year/sex/measure/rei
        year_tasks = {}
        for year_id in args.year_ids:

            tasks = [most_detailed_template.create_task(location_id=loc_id,
                                                        year_id=year_id,
                                                        **task_args_kw)
                     for loc_id in loc_tree.leaves()
                     ]
            workflow.add_tasks(tasks)
            year_tasks[year_id] = tasks
        return year_tasks

    def _add_loc_agg_tasks(self, tool, workflow, args, lookup):
        loader = CauseRiskMetadataLoader.from_cache(self.cache)
        all_reis_by_sex = loader.get_all_reis_by_sex()
        # cast to int so the template doesn't receive e.g., '1.0' and error
        sex_rei_pairs = [(int(X.sex_id), int(X.rei_id))
                         for X in all_reis_by_sex[['sex_id', 'rei_id']].drop_duplicates().itertuples()]

        loc_agg_command_template = textwrap.dedent(
            """
            run_taternator_loc_agg
                --location_set_id {location_set_id}
                --year_id {year_id}
                --measure_id {measure_id}
                --sex_id {sex_id}
                --rei_id {rei_id}
                --data_root {data_root}
                --n_draws {n_draws}
                --extra_dim {extra_dim}
                --gbd_release_id {gbd_release_id}
        """).replace("\n", " ")

        if args.verbose:
            loc_agg_command_template += " --verbose"

        loc_agg_template = tool.get_task_template(
            default_compute_resources={
                'queue': args.queue,
                # wild guess here
                'cores': 10,
                # just YLL for whole USA took 0.8 GB
                # Round to 1G per measure
                'memory': f"{2 * len(args.measure_ids)}G",
                'runtime': 60 * 40,
                'stdout': f"{args.versioned_out_dir}/log_loc_agg",
                'stderr': f"{args.versioned_out_dir}/log_loc_agg",
                'project': args.project,
            },
            template_name='run_taternator_loc_agg',
            default_cluster_name='slurm',
            command_template=loc_agg_command_template,
            node_args=['location_set_id', 'year_id', 'measure_id', 'sex_id', 'rei_id'],
            task_args=[
                'data_root',
                'n_draws',
                'extra_dim',
                'gbd_release_id',
            ],
        )

        task_args_kw = dict(
            # note rename here
            data_root=args.versioned_out_dir,
            extra_dim=args.extra_dim,
            gbd_release_id=args.gbd_release_id,
            n_draws=args.n_draws,
            location_set_id=','.join(map(str, args.location_set_ids)),
        )

        # create tasks. lump them into an easy lookup list for the following task cleanup
        #
        # loc_agg runs by year/sex/measure/rei for all locations
        #
        # cleanup runs by year/measure/location
        measureyear_tasks = {}
        for year_id in args.year_ids:
            dependencies = lookup.get(year_id)
            for measure_id in args.measure_ids:
                tasks = [loc_agg_template.create_task(year_id=year_id,
                                                    sex_id=sex_id,
                                                    measure_id=measure_id,
                                                    rei_id=rei_id,
                                                    upstream_tasks=dependencies,
                                                    **task_args_kw)
                         for sex_id, rei_id
                         in sex_rei_pairs]
                workflow.add_tasks(tasks)
                measureyear_tasks[(measure_id, year_id)] = tasks
        return measureyear_tasks

    def _add_cleanup_tasks(self, tool, workflow, args, lookup):
        loader = CauseRiskMetadataLoader.from_cache(self.cache)
        all_reis_by_sex = loader.get_all_reis_by_sex()

        cleanup_command_template = textwrap.dedent(
            """
            run_taternator_cleanup
                --measure_id {measure_id}
                --fatal_version_id {fatal_version_id}
                --rake_version_id {rake_version_id}
                --extra_dim {extra_dim}
                --location_id {location_id}
                --year_id {year_id}
                -o {versioned_out_dir}
                --n_draws {n_draws}
                --gbd_release_id {gbd_release_id}
                --age_group_set_id {age_group_set_id}
                --paf_compile_run_id {paf_compile_run_id}

        """).replace("\n", " ")

        # this is a little ugly but the --verbose flag does not take args
        if args.verbose:
            cleanup_command_template += " --verbose"

        cleanup_template = tool.get_task_template(
            default_compute_resources={
                'queue': args.queue,
                # wild guess here
                'cores': 1,
                # Round to 1G per measure
                'memory': f"{10 * len(args.measure_ids)}G",
                'runtime': 60 * 60 * 3,
                'stdout': f"{args.versioned_out_dir}/log_cleanup",
                'stderr': f"{args.versioned_out_dir}/log_cleanup",
                'project': args.project,
            },
            template_name='run_taternator_cleanup',
            default_cluster_name='slurm',
            command_template=cleanup_command_template,
            node_args=['year_id', 'measure_id', 'location_id'],
            task_args=[
                'versioned_out_dir',
                'fatal_version_id',
                'rake_version_id',
                'age_group_set_id',
                'n_draws',
                'extra_dim',
                'gbd_release_id',
                'paf_compile_run_id',
            ],
        )

        task_args_kw = dict(
            # note rename here
            versioned_out_dir=args.versioned_out_dir,
            fatal_version_id=args.fatal_version_id,
            rake_version_id=args.rake_version_id,
            age_group_set_id=args.age_group_set_id,
            n_draws=args.n_draws,
            extra_dim=args.extra_dim,
            gbd_release_id=args.gbd_release_id,
            paf_compile_run_id=args.paf_compile_run_id,
        )

        if len(self.cached_data['location_hierarchy']) != 1:
            raise RuntimeError("cached location_hierarchy does not have a single tree")
        else:
            loc_tree, = self.cached_data['location_hierarchy']
        aggregate_loc_ids = [n.id for n in loc_tree.nodes if n not in loc_tree.leaves()]

        # create tasks. lump them into an easy lookup list for the following task upload
        #
        # cleanup runs by year/measure/location
        measureyear_tasks = {}
        for year_id in args.year_ids:
            for measure_id in args.measure_ids:
                dependencies = lookup.get((measure_id, year_id))
                tasks = [cleanup_template.create_task(year_id=year_id,
                                                      measure_id=measure_id,
                                                      location_id=location_id,
                                                      upstream_tasks=dependencies,
                                                      **task_args_kw)
                         for location_id
                         in aggregate_loc_ids]
                workflow.add_tasks(tasks)
                measureyear_tasks[(measure_id, year_id)] = tasks
        return measureyear_tasks

    def _add_upload_tasks(self, tool, workflow, args, lookup):
        raise RuntimeError("run_all_taternator not ready to run upload tasks")

    def _get_cached_data(self):
        """
        Get and return cached data for use in internal TaterPhaseWorkflows tasks.
        """
        # This method is a bit of a hack to overcome the fact that tater.cache.Cache is responsible for creating
        # cached files but tater.ushd_data_container.USHDDataContainer is responsible for loading the data from
        # those cached files.
        #
        # Rather than include the additional complexity of creating a USHDDataContainer we just load the minimum needed
        # for existing functions
        if 'create_cache' in self.phases:
            # NOTE: this is run for it's side effect in addition to being useful
            cached_data = self.cache.load_caches()
            return cached_data

        cached_data = {}

        def load_pickle(filename):
            full_path = f"{self.args.cache_dir}/{filename}"
            with open(full_path, 'rb') as inf:
                return pickle.load(inf)

        cached_data['location_hierarchy'] = [load_pickle(f"location_hierarchy_{lsid}.pickle")
                                             for lsid in self.args.location_set_ids]
        return cached_data


def save_metadata(metadata: dict, outf: pathlib.Path):
    """
    Save a dict of JSON-compatible data to Path.

    Rename existing file if necessary based off of it's modification time.
    """
    if outf.exists():
        creation_date = dt.datetime.fromtimestamp(outf.stat().st_mtime)
        # e.g., 'metadata.2022-09-21_101018.json'
        new_name = creation_date.strftime(f"{outf.stem}.%Y-%m-%d_%H%M%S{outf.suffix}")
        outf.rename(outf.parent / new_name)

    with outf.open('w') as outf:
        json.dump(metadata, outf, indent=4)


def main():
    import time

    start = time.time()

    parser = argparse.ArgumentParser(description="Run All Taternator.")
    parser = add_run_all_args(parser)
    args = get_run_all_args(parser)

    end = time.time()
    print(f"Start reading cache elap {end - start}")

    ushdCache = USHDCache(
        cache_dir=args.cache_dir,
        release_id=args.gbd_release_id,
        age_group_set_id=args.age_group_set_id,
        location_set_ids=args.location_set_ids,
        extra_dim=args.extra_dim,
        paf_compile_run_id=args.paf_compile_run_id,
        cause_set_ids=args.cause_set_ids,
        measure_ids=args.measure_ids,
        fatal_version_id=args.fatal_version_id,
        rake_version_id=args.rake_version_id,
        all_year_ids=args.year_ids,
    )
    end = time.time()
    print(f"Done reading cache elap {end - start}")
    workflow_builder = TaterPhaseWorkflows.from_args_and_cache(args, ushdCache)
    workflow = workflow_builder.create_workflow()

    end = time.time()
    print(f"Done building workflow elap {end - start}")

    if workflow.tasks:
        workflow.bind()
        # errors if workflow is not bound; cannot bind workflow w/out tasks
        workflow_id = workflow.workflow_id
    else:
        workflow_id = None

    metadata = {
        # If you prefer to have dict-like view of the attributes, you can use the standard Python idiom, vars()
        # https://docs.python.org/3/library/argparse.html#argparse.Namespace
        'cli-args': vars(args),
        'workflow': {
            'workflow_id': workflow_id,
            'workflow_name': workflow.name,
            'workflow_args': workflow.workflow_args,
        },
    }

    out_path = pathlib.Path(args.versioned_out_dir, 'metadata.json')
    save_metadata(metadata, out_path)

    if workflow.tasks:
        print(f"Starting workflow '{workflow.name}'")
        print(f"Pass --workflow_args={workflow.workflow_args}' to resume this workflow if it fails")
        workflow.run(fail_fast=False, 
                     resume=True, 
                     seconds_until_timeout = 3600 * 72)
    else:
        print("No tasks added to workflow - you probably specified --end_at create_cache")

    print("Done")
