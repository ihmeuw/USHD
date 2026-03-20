import glob
import logging
import os
import time
import argparse
import pandas as pd
from typing import List, Optional

import gbd.constants as gbd
from cluster_utils.pandas_utils import get_index_columns

#from dalynator import write_summaries as write_sum
# future work to clean up in ushd-555
from tater.utils import write_summaries as write_sum
#from dalynator.lib.aggregation import (
from tater.utils.aggregation import (
    calculate_age_aggregates,
    aggregate_sexes,
    BASIC_AGGREGATES,
)
# from dalynator.lib.metric_conversion import convert_number_to_rate
from dalynator.lib.metric_conversion import _get_number_space_rows
from dalynator.lib.utils import get_index_draw_columns
from dalynator.compute_dalys import ComputeDalys
from dalynator.data_sink import HDFDataSink
# Commented out dalynator functions below have been brought into
# the tater repo to manage specific USHD age_group_id 37 and 38
# Leaving these so we have breadcrumbs back to their original versions
# for future work in ushd-555
from dalynator.tasks.run_pipeline_burdenator_most_detailed import (
    # back_calc_pafs,
    MPGlobals,
)
from tater.tasks.run_taternator_most_detailed import (
    convert_number_to_rate,
    back_calc_pafs,
)

from transforms.transforms import (
    DEFAULT_MERGE_COLUMNS,
    transform_metric,
)
from tater.ushd_data_container import USHDDataContainer
from tater.task_arguments import (
    add_cleanup_args,
    get_cleanup_args,
)

# Other programs look for this string.
SUCCESS_LOG_MESSAGE = "DONE write DF"

logger = logging.getLogger("dalynator.tasks.run_pipeline_burdenator_cleanup")


def aggregate_extra_dim(
    data_frame: pd.DataFrame,
    extra_dim: str,
    include_pre_df: bool = True,
    pop_merge_columns: List[str] = DEFAULT_MERGE_COLUMNS,
    pop_df: Optional[pd.DataFrame] = None,
) -> pd.DataFrame:
    """Aggregate extra_dim in number space, converting to and from number space if necessary.

    Args:
        data_frame: the input DataFrame. If in rate space, assumed to satisfy the
            requirements for transform_metric
        extra_dim (str): 'race' or 'edu'
        include_pre_df (bool): whether to return the original de-aggregated extra_dim
            groups in the results along with the aggregate data. Default is True
        pop_merge_columns: passed to transform_metric if data_frame is in rate space.
        pop_df: passed to transform_metric if data_frame is in rate space.

    Returns:
        extra_df: The extra-dim-aggregated dataframe, optionally including the de-aggregated input.

    """
    pre_extra_df = data_frame.copy()
    df_metrics = pre_extra_df.metric_id.unique()
    if len(df_metrics) > 1:
        raise ValueError(f"Can only combine extra_dim for one metric at a time. Got {df_metrics}")
    else:
        in_metric = df_metrics[0]

    # Summation should always be done in NUMBER space, so make sure
    # the input data frame is properly transformed ...
    if in_metric == gbd.metrics.RATE:
        if pop_df is None:
            raise ValueError(
                "aggregate_extra requires a population dataframe when in RATE space."
            )
        to_sum_df = transform_metric(
            df=pre_extra_df,
            from_id=gbd.metrics.RATE,
            to_id=gbd.metrics.NUMBER,
            pop_df=pop_df,
            merge_columns=pop_merge_columns + [extra_dim],
        )
    elif in_metric == gbd.metrics.NUMBER:
        to_sum_df = pre_extra_df
    else:
        raise ValueError(
            "aggregate_extra_dim can only work with metric_ids "
            f"{gbd.metrics.RATE} or {gbd.metrics.NUMBER}. Got {in_metric}."
        )

    index_cols, _ = get_index_draw_columns(data_frame)

    group_by_columns = list(set(index_cols) - {extra_dim})
    extra_df = to_sum_df.groupby(group_by_columns).sum()
    extra_df[extra_dim] = 1
    extra_df = extra_df.reset_index()

    # ... and back-transformed accordingly
    if in_metric == gbd.metrics.RATE:
        extra_df = transform_metric(
            df=extra_df,
            from_id=gbd.metrics.NUMBER,
            to_id=gbd.metrics.RATE,
            pop_df=pop_df,
            merge_columns=pop_merge_columns + [extra_dim],
        )
    extra_df["metric_id"] = in_metric

    if include_pre_df:
        extra_df = extra_df.append(pre_extra_df)
    return extra_df


def run_taternator_cleanup(out_dir, location_id, year_id, n_draws, measure_id,
                           fatal_version_id, rake_version_id, extra_dim, gbd_release_id,
                           age_group_set_id=None, paf_compile_run_id=None):
    """Take a set of aggregated results and reformat them into draws consistent
    with the most-detailed location draws.

    Args:
        out_dir (str): the root directory for this burdenator run
        location_id (int): location_id of the aggregate location
        year_id (int): year of the aggregate location
        n_draws (int): the number of draw columns in the H5 data frames,
            greater than zero
        measure_id (int): measure_id of the aggregate location
        fatal_version_id (int): model_version_id of model_draw_file table in ushd_fatal database.
        rake_version_id (int): rake_version_id of model_draw_file table in ushd_fatal database.
        extra_dim (str): 'race' or 'edu'
        gbd_release_id (int): The GBD release ID for this run (usually 15)
        age_group_set_id (int): Int or None specifying a specific age group set
            for querying age trees.
    """
    MPGlobals.logger = logger
    start_time = time.time()
    logger.info(f"START pipeline burdenator cleanup at {start_time}")
    logging.basicConfig(format='%(asctime)s %(message)s', datefmt='%m/%d/%Y %I:%M:%S %p')
    logging.warning('is when this event was logged.')

    # Get aggregated draws
    logger.info(f"start append files, time = {time.time()}")
    draw_dir = os.path.join(out_dir, 'draws')
    aggregated_draw_dir = os.path.join(out_dir, 'loc_agg_draws')
    # df contains Attribute Burden, which is in Number space.
    # It is a subset of the total count for the parent metric,
    # ie AB of YLL's for a cause attributable to a risk
    # (or to all known & unknown risks, ie rei_id == 0)

    # df is a list of data frames
    df = []
    metric = 'burden'

    input_file_pattern = (f"{aggregated_draw_dir}/{metric}/{location_id}/{measure_id}/"
                          f"{measure_id}_{year_id}_{location_id}_*.h5")
    logger.debug(f"Cleanup file pattern {input_file_pattern}")
    draw_files = glob.glob(input_file_pattern)

    for f in draw_files:
        logger.info(f"appending {f}")
        this_df = pd.read_hdf(f)
        dups = this_df[this_df.columns[this_df.columns.str.contains(f"_id|{extra_dim}")]].duplicated().any()
        if dups:
            msg = ("Duplicates found in location aggregate output "
                   f"file {f}. Failing this cleanup job")
            logger.error(msg)
            raise RuntimeError(msg)
        df.append(this_df)
    df = pd.concat(df)
    df['edu'] = 1

    logger.info(f"append files complete, time = {time.time()}")
    logger.info(f"columns appended df {get_index_columns(df)}")

    # Get cause envelope
    data_container = USHDDataContainer(
        {'location_id': location_id,
         'year_id': year_id},
        n_draws=n_draws,
        gbd_release_id=gbd_release_id,
        age_group_ids=[],
        fatal_version_id=fatal_version_id,
        rake_version_id=rake_version_id,
        paf_compile_run_id=paf_compile_run_id,
        extra_dim=extra_dim,
        cache_dir=os.path.join(out_dir, "cache"),
        age_group_set_id=age_group_set_id,
        )
    MPGlobals.data_container = data_container

    # cause_env_df has all-cause mortality/whatever, without risks
    if measure_id == gbd.measures.DEATH:
        cause_env_df = data_container['death']
    elif measure_id == gbd.measures.YLL:
        # This list is used in ushd_data_container.py to set YLLs to read
        data_container.l_acauses = pd.unique(df['cause_id'])
        cause_env_df = data_container['yll']
    elif measure_id == gbd.measures.YLD:
        cause_env_df = data_container['yld']
    elif measure_id == gbd.measures.DALY:
        # Get YLLs and YLDs
        yll_df = data_container['yll']
        yld_df = data_container['yld']
        yld_df = yld_df.loc[yld_df.measure_id == gbd.measures.YLD]
        # Compute DALYs
        index_cols, draw_cols = get_index_draw_columns(yld_df)
        daly_ce = ComputeDalys(yll_df, yld_df, draw_cols, index_cols)
        cause_env_df = daly_ce.get_data_frame()

    cause_env_df['rei_id'] = gbd.risk.TOTAL_ATTRIBUTABLE

    # Concatenate cause envelope with data
    # NOTE: this is done because we cannot guarantee there are aggregate age
    # and sex groups coming from the other central programs
    # This problem probably does not occurs in 2017, which is okay,
    # no work will be performed.
    most_detailed_age_groups = data_container["age_spans"]["age_group_id"]
    df = pd.concat([df, cause_env_df], sort=True)
    df = df.loc[((df['sex_id'].isin([gbd.sex.MALE, gbd.sex.FEMALE])) &
                (df['age_group_id'].isin(most_detailed_age_groups)) &
                (df['metric_id'] == gbd.metrics.NUMBER))]
    # Do sex aggregation
    index_cols, draw_cols = get_index_draw_columns(df)
    logger.info(f"start aggregating sexes, time = {time.time()}")
    df = aggregate_sexes(df)
    logger.info(f"aggregating ages sexes, time = {time.time()}")

    # Do extra_dim aggregation
    logger.info(f"start aggregating extra_dim = {extra_dim}, time = {time.time()}")
    df = aggregate_extra_dim(df, extra_dim=extra_dim)
    logger.info(f"aggregating extra_dim = {extra_dim} complete, time = {time.time()}")

    # Do age aggregation
    logger.info(f"start aggregating ages, time = {time.time()}")
    df = calculate_age_aggregates(
        data_frame=df,
        release_id=gbd_release_id,
        extra_aggregates=[37, 38],
        data_container=data_container,
        age_group_set_id=age_group_set_id,
    )
    logger.info(f"aggregating ages complete, time = {time.time()}")

    # Convert to rate space
    logger.info(f"start converting to rates, time = {time.time()}")
    df = convert_number_to_rate(df=df,
                                pop_df=data_container["pop"],
                                include_pre_df=True,
                                merge_columns=DEFAULT_MERGE_COLUMNS + [extra_dim])
    logger.info(f"converting to rates complete, time = {time.time()}")

    # df does not contain AB's any more, because they are RATES

    # Back-calculate PAFs
    logger.info(f"start back-calculating PAFs, time = {time.time()}")
    to_calc_pafs = ((df['metric_id'] == gbd.metrics.NUMBER) |
                    (df['age_group_id'] == gbd.age.AGE_STANDARDIZED) |
                    (df['age_group_id'] == 38))

    pafs_df = df.loc[to_calc_pafs].copy(deep=True)

    # back_calc_pafs is part of the most detailed pipeline, reused from here.
    # It is nice to reuse code, but the modularity feels wrong.
    # That function should "stand alone," not be owned by any single pipeline.
    pafs_df = back_calc_pafs(pafs_df, n_draws)
    df = pd.concat([df, pafs_df], sort=True)
    logger.info(f"back-calculating PAFs complete, time = {time.time()}")

    # Calculate and write out summaries as CSV files
    csv_dir = f"{draw_dir}/{location_id}/upload/"
    write_sum.write_summaries(location_id, year_id, csv_dir, df, index_cols,
                              do_risk_aggr=True, column_order=['measure_id', 'year_id', 'location_id',
                               'sex_id', 'race', 'age_group_id', 'cause_id', 'rei_id',
                               'metric_id', 'mean', 'upper', 'lower'])

    # Save draws
    df = df.loc[((df['sex_id'].isin([gbd.sex.MALE, gbd.sex.FEMALE])) &
                (df['age_group_id'].isin(most_detailed_age_groups)) &
                (df['metric_id'].isin([gbd.metrics.NUMBER,
                                      gbd.metrics.PERCENT])))]
    logger.info(f"start saving draws, time = {time.time()}")
    output_file_pattern = ('{location_id}/'
                           '{measure_id}_{location_id}_{year_id}.h5')
    output_file_path = output_file_pattern.format(
        location_id=location_id, year_id=year_id, measure_id=measure_id)
    filename = f"{draw_dir}/{output_file_path}"
    sink = HDFDataSink(filename,
                       complib="zlib",
                       complevel=1)
    sink.write(df)
    logger.info(f"saving output draws complete, time = {time.time()}")

    # End log
    end_time = time.time()
    elapsed = end_time - start_time
    logger.info(f"DONE cleanup pipeline at {end_time}, elapsed seconds= {elapsed}")
    logger.info(f"{SUCCESS_LOG_MESSAGE}")


def main():
    parser = argparse.ArgumentParser(description="Run Taternator cleanup.")
    parser = add_cleanup_args(parser)
    args = get_cleanup_args(parser)

    run_taternator_cleanup(args.out_dir, args.location_id, args.year_id,
                           args.n_draws, args.measure_id,
                           args.fatal_version_id, args.rake_version_id, args.extra_dim,
                           args.gbd_release_id, args.age_group_set_id,
                           args.paf_compile_run_id)
