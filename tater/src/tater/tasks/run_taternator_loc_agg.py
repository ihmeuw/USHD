import argparse
import os
import time

from dalynator.lib.tasks.run_pipeline_burdenator_loc_agg import (
    LocationAggregator,
    DEFAULT_INDEX_COLUMNS,
)

from tater.task_arguments import (
    add_loc_agg_args,
    get_loc_agg_args,
)
from tater.ushd_data_container import USHDDataContainer


# Subclass of LocationAggregator that overrides get_draw_sink to include year_id in the output file pattern.
# This is necessary because the central LocationAggregator assumes all years are run together and omits year_id from output filenames.
# In the taternator workflow, we run each year separately and require year_id in the output filenames for clarity and to avoid overwriting files.
class TaterLocationAggregator(LocationAggregator):
    def get_draw_sink(self):
        """Establishes the sink for location aggregation draws."""
        # Override the get_draw_sink method from dalynator to include year_id in the paths.
        out_pattern = (
            "{location_id}/{measure_id}/"
            "{measure_id}_{year_id}_{location_id}_"
            "{rei_id}_{sex_id}.h5"
        )
        from dalynator.data_container import remove_unwanted_stars
        from dalynator.lib.tasks.run_pipeline_burdenator_loc_agg import apply_regional_scalars
        from draw_sources.draw_sources import DrawSink
        import os
        draw_sink = DrawSink(
            params={
                "draw_dir": self.out_dir,
                "file_pattern": out_pattern,
                "h5_tablename": f"{self.n_draws}_draws",
            }
        )
        draw_sink.add_transform(
            apply_regional_scalars,
            regional_scalar_path=os.path.join(self.data_root, "cache"),
            region_locs=self.region_locs,
            value_cols=self.value_cols,
        )
        draw_sink.add_transform(
            remove_unwanted_stars, write_out_star_ids=self.write_out_star_ids
        )
        return draw_sink


def main() -> None:
    """
    Main function for executing a location aggregation task.

    This task will aggregate all "most detailed" locations in the specified
    location tree to generate estimates for all other locations. More
    concretely it will aggregate all Texas counties to generate Texas state
    outputs. It does this for each state and then computes a national value
    from the states.

    This task runs for a single year_id/sex_id/measure_id/rei_id combination.
    """
    start_time = time.time()
    parser = argparse.ArgumentParser("Run location aggregation by year/sex/measure/rei")
    parser = add_loc_agg_args(parser)

    args = get_loc_agg_args(parser)

    index_cols = DEFAULT_INDEX_COLUMNS + [args.extra_dim]

    # access the location hierarchies from the cache
    data_container = USHDDataContainer(
        {
            "location_set_id": args.location_set_id,
            "year_id": args.year_id,
            "sex_id": args.sex_id,
        },
        n_draws=args.n_draws,
        gbd_release_id=args.gbd_release_id,
        cache_dir=os.path.join(args.data_root, "cache"),
    )
    loctree_list = [data_container[f"location_hierarchy_{args.location_set_id}"]]
    args.logger.debug(f"Location Set ID: {args.location_set_id} and LocTree: {loctree_list}")

    # Ensure year_ids is always a list of a single integer, since we run per-year
    # with the taternator (we could change this, but would need to update the 
    # get_draw_sink method in TaterLocationAggregator -- in that case, we would
    # just use the dalynator function without defining a new class.).
    year_ids = [args.year_id] if not isinstance(args.year_id, list) else args.year_id
    if len(year_ids) != 1:
        raise ValueError(f"Expected a single year_id, got: {year_ids}")
    loc_agg = TaterLocationAggregator(
        year_ids=year_ids,
        rei_id=args.rei_id,
        sex_id=args.sex_id,
        measure_id=args.measure_id,
        n_draws=args.n_draws,
        data_root=args.data_root,
        region_locs=[],
        write_out_star_ids=False,
        index_cols=index_cols,
    )
    loc_agg.run_aggregation(loctree_list)
    end_time = time.time()
    args.logger.info(f"DONE taternator location agg pipeline elapsed seconds = {(end_time - start_time):.2f}")


if __name__ == "__main__":
    main()
