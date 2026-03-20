import argparse
import functools
import logging
import time
import traceback
from multiprocessing import Pool
from typing import List

import gbd.constants as gbd
import pandas as pd
from core_maths.lib.aggregate import aggregate
from core_maths.summarize import get_summary
from cluster_utils.pandas_utils import get_index_columns

from dalynator.aggregate_causes import AggregateCauses
from dalynator.apply_pafs import (
    ApplyPAFs,
)
from dalynator.data_filter import PAFInputFilter
from dalynator.get_rei_type_id import get_rei_type_id_df
# Commented out dalynator functions below have been brought into
# the tater repo to manage specific USHD age_group_id 37 and 38
# Leaving these so we have breadcrumbs back to their original versions
# for future work in ushd-555
from dalynator.lib.metric_conversion import (
  # convert_number_to_rate,
  _get_number_space_rows
)
from dalynator.lib.utils import get_index_draw_columns
from dalynator.write_csv import write_csv
from tater.utils.aggregation import (
    calculate_age_aggregates,
)

from transforms.transforms import (
    DEFAULT_MERGE_COLUMNS,
    transform_metric,
)

# Commented out dalynator functions below have been brought into
# the tater repo to manage specific USHD age_group_id 37 and 38
# Leaving these so we have breadcrumbs back to their original versions
# for future work in ushd-555
from dalynator.tasks.run_pipeline_burdenator_most_detailed import (
    MPGlobals,
    # aggregate_dimensions as aggregate_dimensions_dalynator,
    aggregate_sexes_global_wrapper,
    #aggregate_ages,
    #back_calc_pafs,
    compute_dalys,
    get_dimensions,
    get_summ_filename,
    write_draws,
)

from transforms.transforms import transform_metric

from tater.ushd_data_container import USHDDataContainer
from tater.task_arguments import (
    add_most_detailed_args,
    get_most_detailed_args,
)


# Other programs look for this string.
SUCCESS_LOG_MESSAGE = "DONE write DF"

# CONSTANTS for switching rei vs. eti files
RISK_REI_TYPE = 1
ETI_REI_TYPE = 2

logger = logging.getLogger(__name__)

def convert_number_to_rate(
    df: pd.DataFrame,
    pop_df: pd.DataFrame,
    include_pre_df: bool,
    merge_columns: List[str] = DEFAULT_MERGE_COLUMNS,
) -> pd.DataFrame:
    """Takes a number-space DataFrame and converts it to rate space.

    Args:
        df: An input dataframe with metric_id column and some values in NUMBER space.
        pop_df: A population DataFrame, as described in
            transforms.transforms.transform_metric():  Needs
            columns "location_id", "year_id", "age_group_id", "sex_id", and
            either "population" or "pop_scaled".
        include_pre_df: includes any original non-rate space entries, and
            any AGE_STANDARDIZED entries
        merge_columns: The columns on which to merge the data and the population
            data frames. For GBD-use, the default is expected. Available as an arg
            for use where additional demographics are included.

    Returns:
        new_df: a dataframe with converted draw_column values, updated metric
            columns, and, optionally, the original number-space entries.

    Raises:
        AssertionError: if the pop_df does not reflect all demographics in df when converting
            to rate space.

    """
    number_df = _get_number_space_rows(df)

    # Convert data to rate space
    new_df = transform_metric(
        df=number_df,
        from_id=gbd.metrics.NUMBER,
        to_id=gbd.metrics.RATE,
        pop_df=pop_df,
        merge_columns=merge_columns,
    )
    if len(new_df) != len(number_df):
        raise AssertionError(
            "Rows before and after population merge do not match: "
            f"before {len(number_df)}, after {len(new_df)}. Not all "
            "demographic entries must have had matching entries "
            "in the population dataframe."
        )

    if include_pre_df:
        # retain non-rate and age-standardized entries in the input
        df = df.loc[
            (df[gbd.columns.AGE_GROUP_ID] == gbd.age.AGE_STANDARDIZED)
            | (df[gbd.columns.AGE_GROUP_ID] == 38)
            | (df[gbd.columns.METRIC_ID] != gbd.metrics.RATE)
        ]
        new_df = pd.concat([new_df, df])

    return new_df


def risk_attr_burden_to_paf(
    risk_cause_df: pd.DataFrame, hundred_percent_pafs_df: pd.DataFrame
) -> pd.DataFrame:
    """Converts risk-attributable burden to PAFs."""
    index_columns, value_columns = get_index_draw_columns(risk_cause_df)
    merge_columns = list(set(index_columns) - {"cause_id", "rei_id", "star_id"})
    # Get the cause-level envelope
    if "star_id" in risk_cause_df:
        burden_by_cause = risk_cause_df.query(
            "rei_id == @gbd.risk.TOTAL_ATTRIBUTABLE and "
            "star_id == @gbd.star.ANY_EVIDENCE_LEVEL"
        )
    else:
        burden_by_cause = risk_cause_df.query("rei_id == @gbd.risk.TOTAL_ATTRIBUTABLE")

    logger.info(
        "APPLY PAFS BEGIN burden_by_cause {}".format(
            get_index_draw_columns(burden_by_cause)[0]
        )
    )
    # Merge cause-level envelope onto data
    paf_df = risk_cause_df.merge(
        burden_by_cause, on=merge_columns + ["cause_id"], suffixes=("", "_bbc")
    )
    # Divide attributable burden by cause-level envelope
    bbc_vcs = ["{}_bbc".format(col) for col in value_columns]
    paf_df[value_columns] = paf_df[value_columns].values / paf_df[bbc_vcs].values
    paf_df[value_columns] = paf_df[value_columns].fillna(0)

    # Set certain cause-risk pairs to 100 % pafs
    if hundred_percent_pafs_df.empty:
        logger.debug("No hundred-percent PAFs detected")
    else:
        hundred_percent_pafs_df["full_paf"] = 1
        paf_df = pd.merge(
            paf_df, hundred_percent_pafs_df, on=["cause_id", "rei_id"], how="left"
        )

        # Skip over AGE_STANDARDIZED entries. Child age groups for this aggregate may have
        # hundred-percent PAFs, but, with age restrictions, the aggregation results in PAFs
        # < 100%. We don't want to attribute 100% for the entire aggregated age group.
        # NOTE: currently this will set PAFs to 1 for other age aggregates, for example
        # ALL_AGES.
        set_to_one = (paf_df["full_paf"] == 1) & (
            paf_df["age_group_id"] != gbd.age.AGE_STANDARDIZED
        )
        paf_rows = paf_df.loc[set_to_one].index.tolist()
        # for all the 100% pafs, make sure that the draws arent all equal
        # to 0. If they are all 0 they are not 100% attributable
        should_be_one_rows = paf_df.index.isin(paf_rows)
        not_actually_one_rows = (paf_df.loc[should_be_one_rows, value_columns] == 0).all(
            axis=1
        )
        paf_rows = list(
            set(paf_rows) - set(not_actually_one_rows[not_actually_one_rows].index)
        )
        paf_df.loc[paf_rows, value_columns] = 1.0

    # Change metric to percent
    paf_df["metric_id"] = gbd.metrics.PERCENT
    logger.info("APPLY PAFS END")
    # Keep only the columns we need
    if "star_id" in paf_df:
        keep_cols = merge_columns + ["cause_id", "rei_id", "star_id"] + value_columns
    else:
        keep_cols = merge_columns + ["cause_id", "rei_id"] + value_columns
    return paf_df[keep_cols]

def back_calc_pafs(df, n_draws):
    """
    Back calculate PAFs for each cause-risk pair
    We should only back-calc in number space, or age-standardized.
    """
    MPGlobals.logger.info("start back-calculating PAFs, n_draws = {}, time = "
                          "{}".format(n_draws, time.time()))
    index_cols = get_index_columns(df)
    MPGlobals.logger.info("BCP columns = {}".format(index_cols))
    pafs_df = risk_attr_burden_to_paf(
        df[df.metric_id == gbd.metrics.NUMBER],
        MPGlobals.data_container['cause_risk_metadata'],
    )
    age_std_pafs_df = risk_attr_burden_to_paf(
        df[(df.age_group_id == gbd.age.AGE_STANDARDIZED) | (df.age_group_id == 38)],
        MPGlobals.data_container['cause_risk_metadata'],
    )
    pafs_df = pd.concat([pafs_df, age_std_pafs_df])
    pafs_df = pafs_df.loc[pafs_df['rei_id'] != 0]
    MPGlobals.logger.info("back-calculating PAFs complete, time = "
                          "{}".format(time.time()))
    return pafs_df

def aggregate_ages(df):
    """Takes a DataFrame of 'most detailed' age groups, aggregates them
    to GBD compare groups. Function is immutable, and only returns the
    aggregate age groups.
    """
    #breakpoint()
    df = calculate_age_aggregates(
        data_frame=df,
        release_id=MPGlobals.data_container.release_id,
#        release_id=MPGlobals.data_container.release_id,
        #extra_aggregates=MPGlobals.data_container.age_group_ids,
        extra_aggregates=[37,38],
        data_container=MPGlobals.data_container,
        include_pre_df=False,
        age_group_set_id=MPGlobals.data_container.age_group_set_id,
    )
    MPGlobals.logger.debug("age_aggregation complete, df shape "
                           "{}".format(df.shape))
    return df

def aggregate_dimensions(df, index_cols=None):
    # Keep just the index columns
    if not index_cols:
        index_cols, _ = get_index_draw_columns(df)
    df = df.loc[df['metric_id'] == 1]
    df = get_dimensions(df, index_cols)

    # Create single draw column to aggregate (will be dropped at the end)
    df['draw_0'] = 1.0
    draw_cols = ['draw_0']

    # Aggregate

    ed_result = aggregate_by_extra_dim(df, extra_dim=MPGlobals.data_container.extra_dim)
    df = pd.concat([df, ed_result], sort=True)
    df_sagg = aggregate_sexes_global_wrapper(df)
    df = pd.concat([df, df_sagg], sort=True)
    df_aagg = aggregate_ages(df) # use USHD aggregate ages, not dalynator version
    df = pd.concat([df, df_aagg])

    df = df.loc[df['draw_0'] != 0]

    # Return data
    df = get_dimensions(df, index_cols)
    return df

def aggregate_by_extra_dim(df, extra_dim):
    metric = df['metric_id'].unique()
    if len(metric) == 1:
        metric, = metric
    else:
        raise RuntimeError(f"aggregate_by_extra_dim takes a df with only one distinct metric; got {metric}")

    if metric == gbd.metrics.NUMBER:
        out_metric = gbd.metrics.NUMBER
        to_agg = df
    elif metric == gbd.metrics.RATE:
        out_metric = gbd.metrics.RATE
        to_agg = transform_metric(
            df=df,
            from_id=metric,
            to_id=gbd.metrics.NUMBER,
            pop_df=MPGlobals.data_container['pop'],
            merge_columns=MPGlobals.data_container.pop_merge_columns,
        )
    else:
        raise RuntimeError(f"aggregate_by_extra_dim does not know how to aggregate metric_id {metric}")

    index_cols, draw_cols = get_index_draw_columns(df)
    index_cols.remove(extra_dim)

    to_agg['edu'] = 1
    aggregated = aggregate(to_agg, value_cols=draw_cols, index_cols=index_cols, aggregation_type='sum', weight_col=None)
    # both "all-edu" and "all-race" are 1
    aggregated[extra_dim] = 1

    if out_metric == gbd.metrics.NUMBER:
        result = aggregated
    elif out_metric == gbd.metrics.RATE:
        result = transform_metric(
            df=aggregated,
            from_id=gbd.metrics.NUMBER,
            to_id=out_metric,
            pop_df=MPGlobals.data_container['pop'],
            merge_columns=MPGlobals.data_container.pop_merge_columns,
        )

    return result


def match_with_dimensions(df, dimensions_df, merge_cols):
    dimensions_df = get_dimensions(dimensions_df, merge_cols)
    MPGlobals.logger.info(
        "match_with_dimensions df {}".format(get_index_columns(df)))
    MPGlobals.logger.info("match_with_dimensions dimensions_df {}".format(
        get_index_columns(dimensions_df)))
    df = pd.merge(df, dimensions_df, on=merge_cols)
    return df


def aggregate_summaries(num_df):
    """
    Take the base-case DataFrame containing data specific to

    * race/ethnicity OR level of educational attainment
    * most detailed ages
    * sexes

    Compute aggregates in number and rate space + PAF back calculations (number space only)

    Aggregates are computed using all combations of aggregating race/edu, age, and sex.
    """
    if not (num_df['metric_id'] == gbd.metrics.NUMBER).all():
        mids = num_df['metric_id'].unique().tolist()
        raise RuntimeError(f"Must aggregate numeric summaries (metric_id={gbd.metrics.NUMBER}). Got metric_ids {mids}")

    # setup some helpers to make this more readable
    agg_bed = functools.partial(aggregate_by_extra_dim, extra_dim=MPGlobals.data_container.extra_dim)

    agg_sex = aggregate_sexes_global_wrapper
    agg_age = aggregate_ages
    bcp = functools.partial(back_calc_pafs, n_draws=MPGlobals.data_container.n_draws)

    def aggregations(df):
        """
        Return all combinations of aggregations and the base df.

        Aggregate_sex(aggregate_age(df)) is equivalent ot aggregate_age(aggregate_sex(df))
        """
        # base cases
        all_ed = agg_bed(df)  # "all extra dimension" e.g., "all race"
        all_sex = agg_sex(df)
        all_age = agg_age(df)
        all_age_ed = agg_age(all_ed)
        all_age_sex = agg_age(all_sex)
        all_sex_ed = agg_sex(all_ed)
        all_age_sex_ed = agg_age(all_sex_ed)
        return (df, all_ed, all_sex, all_age, all_sex_ed, all_age_sex, all_age_ed, all_age_sex_ed)

    # compute aggregations without summarizing
    to_summarize = []
    to_summarize.extend(aggregations(num_df))
    to_summarize.extend([bcp(X) for X in to_summarize])

    rate_df = convert_number_to_rate(
        df=num_df,
        pop_df=MPGlobals.data_container['pop'],
        include_pre_df=False,
        merge_columns=MPGlobals.data_container.pop_merge_columns,
    )
    rate_aggregations = aggregations(rate_df)

    # age standardized summaries have already been calculated in the NUMBER space
    rate_aggregations = [X[(X.age_group_id != gbd.age.AGE_STANDARDIZED) & (X.age_group_id != 38)]
                         for X in rate_aggregations]
    to_summarize.extend(rate_aggregations)
    # summarize all the things
    index_cols, _ = get_index_draw_columns(num_df)
    summaries = [summarize_draws(X, index_cols=index_cols)
                 for X in to_summarize]

    return pd.concat(summaries)


def apply_pafs(pafs_filter_df, df, draw_cols, paf_index_columns, cause_index_columns, merge_columns, index_columns):
    """
    Apply PAFs to cause data df.

    This is a thin wrapper around the ApplyPAFs computation element.

    Returns DataFrame with PAF data applied to cause data.
    """
    ap = ApplyPAFs(
        pafs_filter_df,
        df,
        paf_data_columns=draw_cols,
        cause_data_columns=draw_cols,
        paf_index_columns=paf_index_columns,
        cause_index_columns=cause_index_columns,
        merge_columns=merge_columns,
        index_columns=index_columns,
    )
    return ap.get_data_frame()


def burdenate(key, paf_index_cols, draw_cols):
    """Apply pafs, write draws, and summarize burden for the given 'key.' The
    key here is a human-friendly label for the measures of interest: yll,
    yld, and death. Could look into using gbd.constants, but would in that case
    also want to update the DataContainer class for the same
    """
    # some column alignment
    #
    # merge_columns are the paf_index_columns LESS rei_id
    # these are used to merge the paf data and the cause_data
    # rei_id is removed because cause data doesn't have embedded risks (but PAF
    # data is specific to a risk/cause pair)
    merge_columns = [X for X in paf_index_cols
                     if X not in ('rei_id',)]
    # cause_index_columns are the merge_columns PLUS metric_id
    cause_index_columns = merge_columns + ['metric_id']
    # index columns are the union of paf and cause index cols
    # keep ordering consistent
    index_columns = list(paf_index_cols)
    index_columns.extend(X for X in cause_index_columns if X not in index_columns)

    meas_df = MPGlobals.data_container[key]
    n_draws = MPGlobals.data_container.n_draws
    agg_causes = MPGlobals.data_container.agg_causes
    MPGlobals.logger.info("burdenate n_draws {}".format(n_draws))

    # Apply PAFs to key (yll, yld, or death)
    # PAFS are always  a proportion, so metric is always the same,
    # and therefore not present in PAF files.

    # after they are applied, metric_id is present in the result
    meas_paf_df = apply_pafs(
        MPGlobals.pafs_filter.get_data_frame(),
        meas_df,
        draw_cols,
        paf_index_cols,
        cause_index_columns,
        merge_columns,
        index_columns,
    )

    if agg_causes:
        for cause_hierarchy in MPGlobals.data_container['cause_hierarchy']:
            ac = AggregateCauses(
                cause_hierarchy,
                meas_paf_df,
                index_columns=index_columns)
            meas_paf_df = ac.get_data_frame()
    # Add cause envelope to df_list
    meas_df['rei_id'] = gbd.risk.TOTAL_ATTRIBUTABLE
    meas_df = pd.concat([meas_df, meas_paf_df])
    del meas_paf_df

    # Concatenate cause envelope with data
    # NOTE: this is done because we cannot guarantee there are aggregate
    # age and sex groups coming from the other central programs
    meas_df = meas_df.loc[(
        (meas_df['sex_id'].isin([gbd.sex.MALE, gbd.sex.FEMALE])) &
        (meas_df['age_group_id'].isin(MPGlobals.most_detailed_age_groups)) &
        (meas_df['metric_id'] == gbd.metrics.NUMBER))]

    MPGlobals.logger.info("Writing draws {}".format(key))

    meas_df = pd.concat([meas_df, back_calc_pafs(meas_df, n_draws)])
    write_draws(meas_df, MPGlobals.args.out_dir, key,
                MPGlobals.args.location_id, MPGlobals.args.year_id,
                write_out_star_ids=False)
    MPGlobals.logger.info("Done writing draws {}".format(key))
    # By restricting to just NUMBER this function only returns Attributable
    # Burden, not back-calculated PAFs
    return {'key': key,
            'draws': meas_df[meas_df.metric_id == gbd.metrics.NUMBER]}

def burdenate_caught(key, index_cols, draw_cols):
    """Try/except wrapper so that burdenate can be used in a
    multiprocessing.Pool without individual Processes getting hung on
    exceptions
    """
    try:
        return burdenate(key, index_cols, draw_cols)
    except Exception as e:
        dump = traceback.format_exc()
        MPGlobals.logger.error("From traceback {}".format(dump))
        return e


# this function exists to support lists of arguments instead of single args
# the only meaningful difference is results = pool.starmap(...) instead of results = pool.map(...)
#
# if both summarize() and burdenate() were fixed it is believed that this function, burdenate_caught
# and summarize_caught could be removed
def map_and_raise(pool, func, arglist):
    """A wrapper around Pool.starmap that raises a RuntimeError if any of the
    sub-processes raise an Exception. Otherwise, closes the pool and returns
    the result.
    """
    results = pool.starmap(func, arglist)
    pool.close()
    pool.join()

    reszip = zip(arglist, results)
    exceptions = list(filter(lambda x: isinstance(x[1], Exception), reszip))
    if len(exceptions) > 0:
        exc_strs = ["Args: {}. Error: {}".format(e[0], e[1].__repr__())
                    for e in exceptions]
        exc_str = "\n".join(exc_strs)
        raise RuntimeError("Found errors in mapping '{}':"
                           "\n\n{}".format(func.__name__, exc_str))
    else:
        return results

def summarize_draws(df, index_cols):
    """
    Summarize the draws down to mean/lower/upper columns

    Also order columns consistently. This function is used for every dataset in
    aggregate_summaries so the ordering is somewhat helpful.
    """
    _, draw_cols = get_index_draw_columns(df)
    sumdf = get_summary(df, draw_cols)

    sumdf = sumdf.reset_index()

    # order columns explicitly for consistent outputs
    col_order = index_cols + ['mean', 'upper', 'lower']
    return sumdf[col_order]

def summarize_caught(key):
    """Try/except wrapper so that summaries can be produced in a
    multiprocessing.Pool without individual Processes getting hung on
    exceptions
    """
    try:
        meas_dict = [res for res in MPGlobals.results if res['key'] == key][0]
        meas_df = meas_dict['draws']
        summs = aggregate_summaries(meas_df)
        return summs
    except Exception as e:
        MPGlobals.logger.error("Summarize_caught Caught: {}\n".format(key))
        dump = traceback.format_exc()
        MPGlobals.logger.error("From traceback {}".format(dump))
        return e


def get_measures_and_load_data(measure_ids, data_container):
    """
    Translate ids to names and load cached data into the data_container prior to using multiprocessing.

    Loading data prior to multiprocessing means the forked process already has data in the internal cache
    and results in the data only being loaded once.
    """
    # Cache data and burdenate
    measure_names = []

    if gbd.measures['YLL'] in measure_ids:
        data_container['yll']
        measure_names.append('yll')
    if gbd.measures['YLD'] in measure_ids:
        data_container['yld']
        measure_names.append('yld')
    if gbd.measures['DEATH'] in measure_ids:
        data_container['death']
        measure_names.append('death')

    return measure_names


def run_pipeline_burdenator(args, data_container):
    """
    Run the entire burdenator pipeline. Typically called from
    run_all->qsub->run_pipeline->here

    Will raise ValueError if input files are not present.
    """
    # Start logger
    start_time = time.time()
    logger.info("START pipeline burdenator at {}".format(start_time))
    logger.info("START pipeline burdenator n_draws {}".format(args.n_draws))
    # Validate args before doing any heavy-lifting
    if not args.measure_ids:
        raise ValueError("must pass some measure_ids to burdenator runs ")

    # Share args across processes
    MPGlobals.args = args
    MPGlobals.logger = logger

    logger.info("START pipeline burdenator")

    # Get detailed ages
    MPGlobals.most_detailed_age_groups = data_container["age_spans"]["age_group_id"]  # noqa

    # Fetch PAF input from RF
    logger.info("start apply PAFs, time = {}".format(time.time()))
    paf_df = data_container['paf']
    index_cols, draw_cols = get_index_draw_columns(paf_df)

    # hack: add star_id so that it is present in index_columns so that, when
    # input data *does not* contain star_id, it can be removed from
    # keep_columns in dalynator.datafilter.PAFInputFilter in the
    # set_input_data_frame method
    pafs_filter = PAFInputFilter(draw_columns=draw_cols,
                                 index_columns=index_cols + ['star_id'])

    pafs_filter.set_input_data_frame(paf_df)
    MPGlobals.pafs_filter = pafs_filter

    # Cache data and burdenate
    MPGlobals.data_container = data_container
    pool_size = len(args.measure_ids)
    pool = Pool(pool_size)

    measure_names = get_measures_and_load_data(
        args.measure_ids, data_container)
    burdenate_arglist = [(measure_name, index_cols, draw_cols)
                         for measure_name
                         in measure_names]
    results = map_and_raise(pool, burdenate_caught, burdenate_arglist)

    # Compute DALYs and associated summaries, if requested
    if gbd.measures['DALY'] in args.measure_ids:
        if gbd.measures.YLL and gbd.measures.YLD not in args.measure_ids:
            raise ValueError("Can't compute risk-attributable DALYs unless "
                             "both ylls and ylds are also provided")
        measure_names.append('daly')
        yld_df = [i['draws'] for i in results if i['key'] == 'yld'][0]
        yll_df = [i['draws'] for i in results if i['key'] == 'yll'][0]
        daly_df = compute_dalys(yld_df[yld_df.measure_id == gbd.measures.YLD],
                                yll_df)
        results.append({'key': 'daly', 'draws': daly_df})

    # Write out meta-information for downstream aggregation step
    meta_df = pd.concat([get_dimensions(r['draws']) for r in results])

    # intentionally DO NOT pass index_cols as meta_df has a 'metric_id'
    # 1) index_cols does not have 'metric_id' - not part of PAF data
    # 2) 'metric_id' is required within this call and providing index_cols not containing it breaks things
    # 3) the function can correctly compute the index_cols to use
    meta_df = aggregate_dimensions(meta_df)

    # Set the results as a Global, for use in summarization Pool
    MPGlobals.results = results

    # Summarize
    pool_size = len(args.measure_ids)
    pool = Pool(pool_size)
    summarize_arglist = [[measure_name] for measure_name in measure_names]

    summ_df = map_and_raise(pool, summarize_caught, summarize_arglist)
    summ_df = pd.concat(summ_df)
    summ_df = match_with_dimensions(summ_df, meta_df, merge_cols=index_cols)
    summ_df.reset_index(drop=True, inplace=True)

    logger.info(
        "Risk attribution & daly computation complete, df shape {}".format(
            (summ_df.shape)))

    logger.info("  FINAL burdenator result shape {}".format(summ_df.shape))

    # Write out the year summaries as CSV files
    rei_types = get_rei_type_id_df()
    summ_df = summ_df.loc[summ_df['rei_id'] != 0]

    for measure_id in summ_df.measure_id.unique():
        for risk_type in [RISK_REI_TYPE, ETI_REI_TYPE]:

            # Get list of rei_ids of this type
            risks_of_type = rei_types[rei_types.rei_type_id == risk_type]
            risks_of_type = risks_of_type.rei_id.squeeze()

            # Compute filename
            summ_fn = get_summ_filename(args.out_dir, risk_type,
                                        args.location_id, args.year_id,
                                        measure_id)
            logger.info("Writing {}".format(summ_fn))
            # Write appropriate subset to file
            write_csv(summ_df[((summ_df.measure_id == measure_id) &
                               (summ_df.rei_id.isin(risks_of_type)))],
                      summ_fn,
                      write_out_star_ids=False,
                      )

    end_time = time.time()
    elapsed = end_time - start_time
    logger.info("DONE location-year pipeline at {}, elapsed seconds= "
                "{}".format(end_time, elapsed))
    logger.info("{}".format(SUCCESS_LOG_MESSAGE))

    return summ_df.shape


def main():
    parser = argparse.ArgumentParser(description="Run Taternator most detailed.")
    parser = add_most_detailed_args(parser)
    args = get_most_detailed_args(parser)

    # remove age groups we cannot calculate due to population data that isn't granular enough
    args.age_group_ids = list(args.age_group_ids)
    args.age_group_ids.remove(42)  # neonatal; < 28 days

    logger.debug("year_id= {}, location_id= {}".format(
        args.location_id, args.year_id))

    # Create a DataContainer, cache data to be shared across processes
    data_container = USHDDataContainer(
        cache_granularity_dict={'location_id': args.location_id,
                                'year_id': args.year_id},
        n_draws=args.n_draws,
        gbd_release_id=args.gbd_release_id,
        age_group_ids=args.age_group_ids,
        age_group_set_id=args.age_group_set_id,
        fatal_version_id=args.fatal_version_id,
        rake_version_id=args.rake_version_id,
        nonfatal_run_id=args.nonfatal_run_id,
        paf_compile_run_id=args.paf_compile_run_id,
        extra_dim=args.extra_dim,
        cache_dir=args.cache_dir,
    )

    import bdb
    try:
        shape = run_pipeline_burdenator(args, data_container)
        logger.debug(" shape= {}".format(shape))
    except (bdb.BdbQuit, KeyboardInterrupt):
        raise
    except Exception as e:  # noqa
        # https://stackoverflow.com/a/64523765
        import sys
        if not hasattr(sys, 'ps1'):  # if not run interactively
            raise
        import pdb
        import traceback
        traceback.print_exc()
        pdb.post_mortem()
