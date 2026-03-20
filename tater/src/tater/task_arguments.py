"""
This module contains the methods for adding arguments to and getting arguments from the tater ArgumentParsers.

This is useful because the tater is using it's own CLI args in addition to re-using dalynator args.
"""

import sys
import os
import logging

from cluster_utils.loggers import create_logger_in_memory

from dalynator.makedirs_safely import makedirs_safely
import gbd.constants

from tater.utils.dalynator_arguments import (
    DalynatorArguments,
    VALID_BURDENATOR_MEASURES,
    add_data_root,
    add_measure_id,
    add_rei_id,
    add_sex_id,
    add_year_id,
    add_loc_id,
)
from tater.utils.ushd_arguments import USHDArguments

from tater.utils.dalynator_arguments import (
    add_output_version,
    add_measure_ids,
    add_verbose
)


TATER_PHASES = ['create_cache', 'most_detailed', 'loc_agg', 'cleanup']  # , 'upload']


def add_run_all_args(parser):
    parser.add_argument("--start_at", choices=TATER_PHASES, default=TATER_PHASES[0],
                        help=f"Phase to start from {TATER_PHASES}")
    parser.add_argument("--end_at", choices=TATER_PHASES, default=TATER_PHASES[-1],
                        help=f"Phase to start from {TATER_PHASES}")
    parser.add_argument('--queue', '-q', default='QUEUE', help='Queue to use')
    parser.add_argument('--project', '-p', default='PROJECT', help='Project to use')
    parser = USHDArguments.add_n_draws(parser)
    parser = USHDArguments.add_out_dir(parser)
    parser = add_output_version(parser)
    parser = USHDArguments.add_loc_set_ids(parser)
    parser = USHDArguments.add_cause_set_ids(parser)
    parser = USHDArguments.add_year_ids(parser)
    parser = add_measure_ids(parser)
    parser = USHDArguments.add_fatal_version_id(parser)
    parser = USHDArguments.add_rake_version_id(parser)
    parser = USHDArguments.add_nonfatal_run_id(parser)
    parser = USHDArguments.add_paf_compile_run_id(parser)
    parser = USHDArguments.add_gbd_release_id(parser)
    parser = USHDArguments.add_age_group_set_id(parser)
    parser = USHDArguments.add_extra_dim(parser)
    parser = USHDArguments.add_cache_dir(parser)
    parser = add_verbose(parser)
    parser = USHDArguments.workflow_name(parser)
    parser = USHDArguments.workflow_args(parser)
    parser = USHDArguments.max_concurrent_jobs(parser)
    return parser


def get_run_all_args(parser, cli_args=None):
    if cli_args is None:
        cli_args = sys.argv[1:]
    args = parser.parse_args(cli_args)

    args.versioned_out_dir = os.path.join(args.out_dir, str(args.output_version))
    # NOTE: if an absolute path is provided to --cache_dir then versioned_out_dir will be ignored
    args.cache_dir = os.path.join(args.versioned_out_dir, args.cache_dir)

    makedirs_safely(args.cache_dir)

    validate_measure_ids_and_input_runs(
        fatal_version_id=args.fatal_version_id,
        nonfatal_run_id=args.nonfatal_run_id,
        measure_ids=args.measure_ids,
    )

    # set args.run_phases to the list of phases to be run
    # also validate start is <= end
    start_phase_i = TATER_PHASES.index(args.start_at)
    end_phase_i = TATER_PHASES.index(args.end_at)
    if end_phase_i < start_phase_i:
        raise RuntimeError("--end_at phase occurs before --start_at phase! "
                           f"Phases are {TATER_PHASES} and you provided "
                           f"--start_at={args.start_at} and --end_at={args.end_at}")
    args.run_phases = TATER_PHASES[start_phase_i:end_phase_i+1]

    return args


def get_most_detailed_args(parser, cli_args=None):
    """Parses the command line using the parser and creates output directory
    and logger. Called by run_taternator_most_detailed. Not used by run_all.
    This is selectively copied from dalynator.get_input_args.get_args_and_create_dirs.
    """
    if cli_args is None:
        cli_args = sys.argv[1:]
    args = parser.parse_args(cli_args)

    # Store all years for each location in one directory
    args.log_dir = os.path.join(args.out_dir, "log_most_detailed", str(args.location_id))
    # ugly - mutate out_dir in-place
    args.out_dir = os.path.join(args.out_dir, "draws", str(args.location_id))

    validate_measure_ids_and_input_runs(
        fatal_version_id=args.fatal_version_id,
        nonfatal_run_id=args.nonfatal_run_id,
        measure_ids=args.measure_ids,
    )

    makedirs_safely(args.out_dir)
    makedirs_safely(args.log_dir)

    log_level = logging.DEBUG if args.verbose else logging.INFO
    args.logger = create_logger_in_memory(
        "dalynator",
        log_level,
        args.log_dir + "/daly_{}_{}.log".format(args.location_id, args.year_id),
        ["aggregator.aggregators", "jobmon"],
    )

    return args


def add_most_detailed_args(parser):
    """
    Appends USHD specific arguments for run_taternator_most_detailed.
    """
    parser = DalynatorArguments.add_by_reusing_arguments(parser)
    parser = USHDArguments.add_n_draws(parser)
    parser = USHDArguments.add_fatal_version_id(parser)
    parser = USHDArguments.add_rake_version_id(parser)
    parser = USHDArguments.add_nonfatal_run_id(parser)
    parser = USHDArguments.add_paf_compile_run_id(parser)
    parser = USHDArguments.add_extra_dim(parser)
    parser = USHDArguments.add_age_group_set_id(parser)
    parser = USHDArguments.add_gbd_release_id(parser)
    parser = USHDArguments.add_cache_dir(parser)
    return parser


def add_loc_agg_args(parser):
    parser = USHDArguments.add_extra_dim(parser)
    parser = add_year_id(parser)
    parser = add_rei_id(parser)
    parser = add_sex_id(parser)
    parser = add_measure_id(parser, choices=VALID_BURDENATOR_MEASURES)
    parser = add_data_root(parser)
    parser = USHDArguments.add_n_draws(parser)
    parser = USHDArguments.add_gbd_release_id(parser)
    parser = USHDArguments.add_loc_set_id(parser)
    parser = add_verbose(parser)
    return parser


def get_loc_agg_args(parser, cli_args=None):
    """
    Get loc_agg args.

    This involves parsing the CLI and adding some additional arguments.
    """
    if cli_args is None:
        cli_args = sys.argv[1:]
    args = parser.parse_args(cli_args)

    # create logger and assign to args.logger
    log_dir = os.path.join(args.data_root, 'log_loc_agg', str(args.year_id), str(args.measure_id))
    makedirs_safely(log_dir)
    log_filename = f"{args.measure_id}_{args.rei_id}_{args.year_id}_{args.sex_id}.log"
    log_level = logging.DEBUG if args.verbose else logging.INFO
    args.logger = create_logger_in_memory(
        'dalynator',
        log_level,
        os.path.join(log_dir, log_filename),
        ['aggregator.aggregators', 'jobmon'],  # also create/configure these loggers with the same settings
    )

    return args


def add_cleanup_args(parser):
    parser = add_measure_id(parser, choices=VALID_BURDENATOR_MEASURES)
    parser = USHDArguments.add_fatal_version_id(parser)
    parser = USHDArguments.add_rake_version_id(parser)
    parser = USHDArguments.add_extra_dim(parser)
    parser = add_year_id(parser)
    parser = add_loc_id(parser)
    parser = USHDArguments.add_out_dir(parser)
    parser = USHDArguments.add_n_draws(parser)
    parser = USHDArguments.add_gbd_release_id(parser)
    parser = USHDArguments.add_age_group_set_id(parser)
    parser = USHDArguments.add_paf_compile_run_id(parser)
    parser = add_verbose(parser)
    return parser


def get_cleanup_args(parser, cli_args=None):
    """
    Get cleanup args.

    This involves parsing the CLI and adding some additional arguments.
    """
    if cli_args is None:
        cli_args = sys.argv[1:]
    args = parser.parse_args(cli_args)

    # create logger and assign to args.logger
    log_dir = os.path.join(args.out_dir, 'log_cleanup', str(args.year_id), str(args.measure_id))
    makedirs_safely(log_dir)
    log_filename = f"{args.measure_id}_{args.location_id}_{args.year_id}.log"
    log_level = logging.DEBUG if args.verbose else logging.INFO
    args.logger = create_logger_in_memory(
        'dalynator',
        log_level,
        os.path.join(log_dir, log_filename),
        ['aggregator.aggregators', 'jobmon'],  # also create/configure these loggers with the same settings
    )

    return args


def validate_measure_ids_and_input_runs(*, measure_ids, fatal_version_id, nonfatal_run_id):
    """
    Validate required input model data is present for requested measures.
    """
    errors, err_template = [], "measure_id {} requires a {} for computation"

    measures = gbd.constants.measures

    no_fatal = fatal_version_id is None
    no_nonfatal = nonfatal_run_id is None

    if measures.DEATH in measure_ids and no_fatal:
        errors.append(err_template.format(measures.DEATH, "fatal_version_id"))
    if measures.YLL in measure_ids and no_fatal:
        errors.append(err_template.format(measures.YLL, "fatal_version_id"))
    if measures.YLD in measure_ids and no_nonfatal:
        errors.append(err_template.format(measures.YLD, "nonfatal_run_id"))
    if measures.DALY in measure_ids and (no_fatal or no_nonfatal):
        errors.append(err_template.format(measures.DALY, "fatal_version_id and nonfatal_run_id"))
    if errors:
        raise ValueError("\n".join(errors))
