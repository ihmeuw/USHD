import pandas as pd
import numpy as np

from cod_prep.utils import report_if_merge_fail, misc
from cod_prep.claude.configurator import Configurator
from cod_prep.downloaders import (
    prep_child_to_available_parent_map,
    add_age_metadata,
    prep_child_to_available_parent_loc,
    get_all_related_causes,
    add_cause_metadata,
    add_code_metadata,
    add_location_metadata
)

CONF = Configurator('standard')


def assert_flagged_col(df):
    """Assert flagged column exists."""
    assert 'flagged' in df.columns, \
           "Must set 'flagged' column to 0 or 1 in dataframe to mark which " \
           "rows to correct; ok to flag all rows if all can be adjusted"
    data_flagged_set = set(df['flagged'].unique())
    flagged_set = {0, 1}
    assert len(data_flagged_set - flagged_set) == 0, \
        "Flagging mechanism produced more than just 0 or 1 in flagged"


def assert_pop_col(df, pop_col):
    """Assert population column exists."""
    assert pop_col in df.columns, \
        "Given pop_col '{}' not in " \
        "df.columns: {}".format(pop_col, df.columns)


def is_ref_age(x, ref_map, col1, col2):
    """Flag a row as a reference group.
    """
    return x[col1] in ref_map[x[col2]]


def assign_code_to_created_target_deaths(df, code_system_id,
                                         cause_meta_df):
    created = df[df['_merge'] == 'right_only']
    original = df[df['_merge'] != 'right_only']
    created = add_cause_metadata(created, 'acause',
                                 cause_meta_df=cause_meta_df)
    created['value'] = created['acause'].apply(lambda x: 'acause_' + x)
    created.drop(['code_id', 'acause'], axis=1, inplace=True)
    created = add_code_metadata(created, 'code_id',
                                code_system_id=code_system_id,
                                merge_col='value',
                                cache_dir=CONF.get_directory('db_cache'),
                                force_rerun=False, block_rerun=True)
    report_if_merge_fail(created, 'code_id', ['value'])
    df = original.append(created)
    df.drop(['_merge', 'value'], axis=1, inplace=True)
    return df


def flag_correct_dem_groups(df,
                            code_system_id,
                            cause_meta_df,
                            loc_meta_df,
                            age_meta_df,
                            rates_df,
                            cause_to_reference_ages_map,
                            move_gc_age_limits,
                            value_cols,
                            pop_col,
                            cause_selections_path,
                            correct_garbage):
    """Determine the observations that will be flagged for adjustment.
    """
    orig_cols = df.columns
    if not correct_garbage:
        # only need to evaluate this flagging criteria for non garbage
        rr_df, rr_group_cols = compare_to_global_rates(
            df,
            cause_to_reference_ages_map,
            cause_meta_df,
            value_cols,
            pop_col,
            rates_df,
        )

    mcs_df = get_master_cause_selections(cause_selections_path,
                                         loc_meta_df,
                                         cause_meta_df,
                                         correct_garbage)
    mcs_df['flagged'] = 1
    # add location-causes that we are correcting
    df = df.merge(mcs_df, on=['location_id', 'cause_id'], how='left')
    df['flagged'] = df['flagged'].fillna(0)
    if not correct_garbage:
        # now reduce flagged rows to those that also exceed the global ref rate
        df = df.merge(rr_df, on=rr_group_cols, how='left')
        df['exceeds_global'] = df['exceeds_global'].fillna(0)
        df['flagged'] = df['flagged'] * df['exceeds_global']
        # and drop that indicator
        df = df.drop('exceeds_global', axis=1)

    # only adjust deaths for certain age ranges
    df = add_age_metadata(df, ['age_group_years_start'],
                          age_meta_df=age_meta_df)
    df = get_age_cause_groups(df, cause_meta_df, correct_garbage,
                              move_gc_age_limits)
    df = df.drop(['age_group_years_start'], axis=1)

    # only correcting after certain years
    df.loc[df['year_id'] < 1980, 'flagged'] = 0

    assert set(df.columns) - set(orig_cols) == {'flagged'}
    return df


def get_master_cause_selections(cause_selections_path,
                                loc_meta_df,
                                cause_meta_df,
                                correct_garbage):
    """Retrieve location-causes that are being corrected."""
    # read versioned input file
    df = pd.read_csv(cause_selections_path)
    if correct_garbage:
        df = df.loc[df['cause_id'].isin([860, 743])]

    # drop any duplicates
    df = df.drop_duplicates()

    # expand countries to subnationals
    country_subnats = prep_child_to_available_parent_loc(
        loc_meta_df,
        loc_meta_df.query('level==3').location_id.unique(),
        min_level=3
    )
    df = df.loc[df.location_id.isin(country_subnats.location_id.unique().tolist())]
    df = df.rename(columns={'location_id': 'parent_location_id'})
    df = df.merge(country_subnats, how='left')
    df = df.drop('parent_location_id', axis=1)
    assert not df[['location_id', 'cause_id']].duplicated().any()
    return df


def compare_to_global_rates(df,
                            cause_to_reference_ages_map,
                            cause_meta_df,
                            value_cols,
                            pop_col,
                            rates_df):
    """Compare in data rates to the global age pattern."""
    br_group_cols = ['location_id', 'sex_id', 'cause_id']
    rr_group_cols = br_group_cols + ['age_group_id']
    rr_df = df.groupby(
        rr_group_cols,
        as_index=False
    )[value_cols + [pop_col]].sum()
    # calculate year-pooled baserates in-data
    rr_df = calculate_relative_rates(
        rr_df,
        cause_to_reference_ages_map,
        cause_meta_df,
        br_group_cols,
        value_cols,
        pop_col
    )

    rr_df = rr_df.merge(
        rates_df,
        on=['cause_id', 'age_group_id', 'sex_id'], how='left')
    rr_df.loc[
        rr_df['relrate_deaths'] > rr_df['rrateWLD'],
        'exceeds_global'
    ] = 1
    rr_df['exceeds_global'] = rr_df['exceeds_global'].fillna(0)
    rr_df = rr_df[rr_group_cols + ['exceeds_global']]
    return rr_df, rr_group_cols


def calculate_relative_rates(df,
                             cause_to_reference_ages_map,
                             cause_meta_df,
                             br_group_cols,
                             value_cols,
                             pop_col):
    """Calculate relative rates for a value across age.
    """
    if isinstance(value_cols, str):
        value_cols = [value_cols]

    assert_pop_col(df, pop_col)

    id_cols = list(set(df.columns) - set(value_cols) - {pop_col})

    # calculate base rates (the reference age groups for each cause)
    base_df, baserate_cols = calculate_baserates(df,
                                                 cause_to_reference_ages_map,
                                                 cause_meta_df,
                                                 br_group_cols,
                                                 value_cols, pop_col)
    # add the baserate to the data
    df = df.merge(base_df, on=br_group_cols, how='left')
    relrate_cols = []
    for value_col in value_cols:
        relrate_col = 'relrate_{}'.format(value_col)
        baserate_col = 'baserate_{}'.format(value_col)
        df[relrate_col] = (df[value_col] / df[pop_col]) / df[baserate_col]
        df[relrate_col] = df[relrate_col].fillna(0)
        relrate_cols.append(relrate_col)

    keep_cols = id_cols + value_cols + baserate_cols + relrate_cols + [pop_col]
    return df[keep_cols]


def get_age_cause_groups(df, cause_meta_df, correct_garbage,
                         move_gc_age_limits):
    """Determine age/cause groups we are adjusting.
    """
    # make sure the flagged column exists
    assert_flagged_col(df)
    if not correct_garbage:
        max_age = 65
        tb_max_age = 60
        tb_cause_ids = get_all_related_causes(297, cause_meta_df)
        df.loc[df['age_group_years_start'] > max_age, 'flagged'] = 0
        df.loc[
            (df['cause_id'].isin(tb_cause_ids)) &
            (df['age_group_years_start'] > tb_max_age),
            'flagged'] = 0
    elif correct_garbage:
        locations_in_data = df.location_id.unique()
        for location in locations_in_data:
            try:
                df.loc[(df.location_id == location)
                       & (~df.age_group_id.isin(move_gc_age_limits[location])),
                       'flagged'] = 0
            except KeyError as e:
                raise type(e)("Location_id: {} in data is not in our"
                              " map for age limits"
                              " on moving garbage codes to hiv".format(e))
    return df


def identify_positive_excess(df,
                             rates_df,
                             cause_to_reference_ages_map,
                             loc_meta_df,
                             age_meta_df,
                             cause_meta_df,
                             value_cols,
                             pop_col,
                             correct_garbage,
                             pop_df=None,
                             pop_id_cols=['location_id', 'year_id',
                                          'sex_id', 'age_group_id']):
    """Identify positive excess for flagged rows in df.
    """
    # do some prep work first
    df = _prep_for_pe_correction(df, value_cols, pop_col,
                                 pop_df, pop_id_cols, correct_garbage)

    # calculate baserates in-data
    br_group_cols = ['nid', 'extract_type_id', 'location_id', 'year_id', 'sex_id',
                     'cause_id', 'site_id']
    base_df, baserate_cols = calculate_baserates(df,
                                                 cause_to_reference_ages_map,
                                                 cause_meta_df,
                                                 br_group_cols,
                                                 value_cols, pop_col)
    df = df.merge(base_df, on=br_group_cols, how='left')
    # bring on out of data relative rates (world relative rates)
    df = df.merge(rates_df, on=['cause_id', 'age_group_id', 'sex_id'],
                  how='left')
    df = calculate_positive_excess(df, pop_col, loc_meta_df, age_meta_df, cause_meta_df)
    # population is no longer needed
    df.drop(pop_col, axis=1, inplace=True)
    return df


def _prep_for_pe_correction(df, value_cols, pop_col, pop_df,
                            pop_id_cols, correct_garbage):
    """Prep data before positive excess calculation."""
    if isinstance(value_cols, str):
        value_cols = [value_cols]
    # make sure the 'flagged' column is set
    assert_flagged_col(df)

    # id cols will be original columns - val columns
    id_cols = list(
        set(df.columns) -
        set(value_cols) -
        {'flagged'} -
        {pop_col}
    )
    if correct_garbage:
        gc_dup_cols = id_cols + ['flagged']
        dup_cols = gc_dup_cols
    else:
        dup_cols = id_cols
    # these should not be duplicated
    dups = df[df[dup_cols].duplicated()]
    if len(dups) > 0:
        raise AssertionError(
            "Found duplicates in non-value columns "
            "{c}: \{df}".format(c=id_cols, df=dups)
        )

    # add population to the data
    if pop_col not in df.columns:
        if pop_df is None:
            raise AssertionError(
                "If pop_col ('{}') is not already in the dataframe, "
                "pop_df must not be None".format(pop_col)
            )
        df = df.merge(pop_df, on=pop_id_cols, how='left')
    # every row should have a population
    report_if_merge_fail(df, pop_col, pop_id_cols)
    return df


def calculate_baserates(df,
                        cause_to_reference_ages_map,
                        cause_meta_df,
                        br_group_cols,
                        value_cols, pop_col,
                        ref_age_id_col=['age_group_id']):
    """Calculate baserates for a value across age.
    """
    # ok for value_cols to be a string
    if isinstance(value_cols, str):
        value_cols = [value_cols]
    # make sure population data is there
    assert_pop_col(df, pop_col)

    # map from every cause id in the cause meta df to which
    # key in the reference_group_map to use
    reference_group_map = \
        prep_child_to_available_parent_map(
            cause_meta_df,
            list(cause_to_reference_ages_map.keys()),
            as_dict=True
        )
    # 999 is the pseudo cause id for injury garbage
    reference_group_map[9991] = 294
    reference_group_map[9992] = 294

    # add that mapping to the data
    df['ref_group_cause_id'] = df['cause_id'].map(reference_group_map)
    # restrict the dataframe to only the data that will be used to
    # calculate baserates
    base_df = df
    base_df['is_ref_age'] = base_df.apply(is_ref_age,
                                          axis=1,
                                          args=[cause_to_reference_ages_map,
                                                'age_group_id',
                                                'ref_group_cause_id'])
    base_df = base_df[base_df['is_ref_age']]
    base_df.drop('is_ref_age', inplace=True, axis=1)
    group_cols = br_group_cols + ref_age_id_col
    base_df = base_df.groupby(
        group_cols, as_index=False)[value_cols + [pop_col]].sum()
    # calculate the baserate for each value_col
    baserate_cols = []
    for value_col in value_cols:
        baserate_col = 'baserate_{}'.format(value_col)
        base_df[baserate_col] = base_df[value_col] / base_df[pop_col]
        baserate_cols.append(baserate_col)

    # collapse each reference age, averaging the rates to get a baserate
    base_df = base_df.groupby(
        br_group_cols, as_index=False)[baserate_cols].mean()
    return base_df, baserate_cols


def apply_ussr_tb_proportions(df, cause_meta_df):
    # These are the iso3s in the rates file, check against these/
    # They are derived from the iso3's in Russia sources and ICD10 source
    ussr_iso3s = ['ARM', 'AZE', 'BLR', 'EST', 'GEO', 'KAZ', 'KGZ', 'LTU', 'LVA',
                  'MDA', 'RUS', 'TJK', 'TKM', 'UKR', 'UZB']
    if df['iso3'].isin(ussr_iso3s).any():
        tb_props = (
            pd.read_csv(CONF.get_resource('ussr_tb_proportions'))
            # updates for GBD 2020: expand to tb_other children and U5 age detail
            .merge(
                pd.DataFrame({"cause_id": 934, "cause_id_child": get_all_related_causes(934, cause_meta_df)}),
                how='outer',
            )
            .assign(cause_id=lambda d: d['cause_id_child'].fillna(d['cause_id'], downcast='infer'))
            .drop(columns='cause_id_child')
            .pipe(misc.expand_to_u5_age_detail)
        )
        df = df.merge(tb_props, how='left',
                      on=['iso3', 'age_group_id', 'sex_id', 'cause_id'],
                      indicator=True)
        df.loc[df['_merge'] == 'both',
               'deaths_post'] = df['deaths'] * (1 - df['tb_hiv_prop'])
        df.loc[df['_merge'] == 'both',
               'positive_excess'] = df['deaths'] * df['tb_hiv_prop']
        df = df.drop(['_merge', 'tb_hiv_prop', 'iso3'], axis=1)
    else:
        pass
    return df


def calculate_positive_excess(df, pop_col, loc_meta_df, age_meta_df, cause_meta_df):
    """Calculate the positive excess for flagged observations."""
    # calculate the values after positive excess is removed
    df = add_location_metadata(df, 'iso3', location_meta_df=loc_meta_df,
                               cache_dir=CONF.get_directory('db_cache'))
    df.loc[
        df['flagged'] == 1,
        'deaths_post'
    ] = df['baserate_deaths'] * df[pop_col] * df['rrateWLD']
    # calculate the positive excess
    df.loc[
        df['flagged'] == 1,
        'positive_excess'
    ] = df['deaths'] - df['deaths_post']
    # where positive excess is negative, or flagged is 0, val_post = val
    no_pe = (df['positive_excess'] < 0) | (df['positive_excess'].isnull())
    df.loc[no_pe, 'deaths_post'] = df['deaths']
    df.loc[no_pe, 'positive_excess'] = 0
    df = apply_ussr_tb_proportions(df, cause_meta_df)

    # maintaining index order is very important here!
    over_60 = (
        df["age_group_id"].map(age_meta_df.set_index("age_group_id")["age_group_years_start"]) >= 60
    )
    tb = df["cause_id"].isin([297] + get_all_related_causes("tb_other", cause_meta_df))
    override = over_60 & tb
    df.loc[override, "deaths_post"] = df["deaths"]
    df.loc[override, "positive_excess"] = 0

    return df


def move_excess_to_target(df, value_cols, cause_to_targets_map,
                          correct_garbage):
    """Move excess to target causes.
    """
    # now positive excess has to be moved to the target causes
    id_cols = list(
        set(df.columns) -
        set(value_cols) -
        {'flagged'} -
        {'deaths_post'} -
        {'positive_excess'}
    )
    assert not df[id_cols].duplicated().any()

    pe_df = extract_positive_excess(df, id_cols, cause_to_targets_map)
    df = df.drop('positive_excess', axis=1)
    if correct_garbage:
        df = df.merge(pe_df, on=id_cols, how='outer', indicator=True)
    else:
        df = df.merge(pe_df, on=id_cols, how='outer')
    # fill in all the missing values where merge may have created missings
    fill_zero_cols = ['positive_excess'] + value_cols
    df[fill_zero_cols] = df[fill_zero_cols].fillna(0)
    df['deaths_post'] = df['deaths_post'].fillna(df['deaths'])
    # targeted causes should now have val_post = val + positive_excess
    target_causes = list(set(cause_to_targets_map.values()))
    df.loc[
        df['cause_id'].isin(target_causes),
        'deaths_post'] = df['deaths'] + df['positive_excess']

    # positive excess has fulfilled its duty and can go away now
    df = df.drop('positive_excess', axis=1)
    assert np.allclose(df['deaths'].sum(), df['deaths_post'].sum())

    # fix deaths_post confusion
    df['deaths_pre'] = df['deaths']
    df['deaths'] = df['deaths_post']
    df = df[df['deaths'] > 0]
    df.drop(['deaths_post'], axis=1, inplace=True)
    # done!
    return df


def extract_positive_excess(df, id_cols, cause_to_targets_map):
    """Extract positive excess and assign target causes."""
    df = df[id_cols + ['positive_excess']]
    df['target_cause_id'] = df['cause_id'].map(
        cause_to_targets_map
    )
    df.loc[df['cause_id'] == 606, 'target_cause_id'] = 294
    # every cause should have a target, even if it is not corrected
    report_if_merge_fail(df, 'target_cause_id', 'cause_id')
    df['cause_id'] = df['target_cause_id']
    df = df.groupby(
        id_cols, as_index=False)['positive_excess'].sum()
    return df
