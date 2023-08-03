"""
Code to format Race/Ethnicity specific USA Cause of death data.
"""
from pathlib import Path

import numpy as np
import pandas as pd
from db_tools import ezfuncs

# repo specific imports
from cod_prep.claude.configurator import Configurator
from cod_prep.claude.formatting import finalize_formatting, update_nid_metadata_status
from cod_prep.downloaders.ages import get_cod_ages, get_ages
from cod_prep.downloaders.engine_room import get_cause_map
from cod_prep.downloaders.locations import add_location_metadata
from cod_prep.utils import report_if_merge_fail, print_log_message

CONF = Configurator()

project_id = CONF.get_id('project')

WRITE = True
IS_ACTIVE = False
IS_MORT_ACTIVE = False

# base location of the incoming data
IN_DIR = Path(
    "FIELPATH",
)

# columns for our output dataframe
ID_COLS = [
    "nid",
    "location_id",
    "year_id",
    "age_group_id",
    "sex_id",
    "population_group_id",
    "data_type_id",
    "representative_id",
    "code_system_id",
    "code_id",
    "site",
]
VALUE_COL = ["deaths"]
FINAL_FORMATTED_COLS = ID_COLS + VALUE_COL

SOURCE = "SOURCE"
YEARS = list(range(1999, 2020))


def read_data():
    keep_cols = [
        "nid",
        "year_id",
        "location_id",
        "sex_id",
        "age_group_id",
        "value",
        "race_orig",
        "race_hisp_recode",
        "race_label_1977",
        "deaths"
    ]
    dfs = []
    for year in YEARS:
        df = pd.read_csv(str(IN_DIR) + "FILEPATH")
        assert (df.icd_version == "ICD10").all()
        dfs.append(df)
    df = pd.concat(dfs, ignore_index=True)
    df = df[keep_cols]

    # check for sex_ids
    assert df.sex_id.isin([1, 2, 9]).all()

    # do some location_id checks, and then get the state ids (data comes at county level)
    df["location_id"] = df["location_id"].astype(int)
    # have to pull this loc metadata with location_set 8 to include the county locs
    df = add_location_metadata(df, ["level"], location_set_id=8)
    assert (df.level == 5).all(), (
        "There are non-county level location in the data"
        " that need special handling first."
    )


    df.drop(["level"], axis=1, inplace=True)

    return df

def get_age_group_ids(df):
    """Create age variable based on GBD shared age groups."""
    # df comes in with an "age_group_id" column that has string codes for each age group
    # rename to make less confusing
    df = df.rename(columns={'age_group_id': 'age_code'})
    df.loc[df.age_code == '999','age_code'] = '-9'
    df['age'] = df.age_code.str.extract(r'([0-9]{1,2})([0-9mo\.\-]*)')[0]
    ages = get_cod_ages()
    ages['age'] = ages.age_group_name.str.extract(r'([0-9]{1,2})([ a-zA-Z0-9\-]*)')[0]
    ages = ages.loc[ages.age.notnull()]
    df = df.merge(ages[['age', 'age_group_id']], how='left', on='age', validate='many_to_one')
    # Final fixes
    df.age_group_id.update(df.age_code.map({'-9': 283, '0': 2, '0.01': 3}))
    assert df.age_group_id.notnull().all()
    return df


def get_population_group_id(df):
    df = (
        # There are three total deaths where race is unknown. These are cases where the decedent
        # died in Puerto Rico (which assigns "Other race"), but were US residents
        # For the sake of time, we just drop these for now
        df.query("race_label_1977 != 'None'")
        .assign(
            population_group_name=lambda d: d["race_label_1977"].replace(
                {
                    "Hispanic": "Hispanic, Any race",
                    "NH Black": "Non-Hispanic, Black",
                    "NH White": "Non-Hispanic, White",
                    "NH AIAN": "Non-Hispanic, American Indian, Alaskan Native",
                    "NH API": "Non-Hispanic, Asian, Pacific Islander",
                }
            )
        )
        .merge(
            ezfuncs.query(
                """
                SELECT population_group_id, population_group_name
                FROM shared.population_group
                """,
                conn_def="shared",
            ),
            how="left",
            validate="many_to_one",
        )
    )
    report_if_merge_fail(df, "population_group_id", "population_group_name")
    return df


def adjust_raw_causes(df):
    """Adjust causes in raw data due to special requests."""
    # ICD10 fixes
    icd10 = df['code_system_id'] == 1
    df.loc[
        (df['value'].str.startswith('U0')) & icd10,
        'value'] = df['value'].str[0:3]
    df.loc[(
        (df['value'] == 'A09.0') | (df['value'] == 'A09.9')
    ) & icd10, 'value'] = 'K52.9'
    df.loc[
        (df['value'].str.contains('^V8[7-9]')) & icd10, 'value'
    ] = 'acause_inj_trans_road_4wheel'
    # ICD9_detail fixes
    icd9 = df['code_system_id'] == 6
    df.loc[
        (df['value'].str.contains('^[89]')) & icd9,
        'value'] = 'E' + df['value']
    return df


def remap_code_id(df, code_ids, num_digits, value_col='value'):
    """Retry code id mapping at num_digits length."""
    missing_mappings = df[df['code_id'].isnull()].copy()
    missing_mappings = missing_mappings.drop('code_id', axis=1)
    is_icd9_ecode = (missing_mappings['code_system_id'] == 6) & \
        (missing_mappings[value_col].str.startswith("E"))
    missing_mappings.loc[
        is_icd9_ecode, value_col
    ] = missing_mappings[value_col].apply(lambda x: x[:(num_digits + 1)])
    missing_mappings.loc[
        ~is_icd9_ecode, value_col
    ] = missing_mappings[value_col].apply(lambda x: x[:num_digits])
    filled_mappings = missing_mappings.merge(
        code_ids, on=['code_system_id', value_col], how='left')
    return filled_mappings


def map_code_id(df, code_map, remove_decimal=True, value_col='value'):
    """Add code id to the df using the given code map."""
    # remove decimal if necessary
    if remove_decimal:
        df[value_col] = df[value_col].str.replace(".", "")
        code_map[value_col] = code_map[value_col].str.replace(".", "")
        code_map = code_map.drop_duplicates(
            subset=['code_system_id', value_col, 'cause_id'])
    code_ids = code_map[
        ['code_system_id', value_col, 'code_id']].drop_duplicates()
    df.loc[df[value_col] == 'acause_digest_gastrititis',
           value_col] = 'acause_digest_gastritis'
    # make sure there is just one cause code
    assert not code_ids[
        ['code_system_id', value_col]
    ].duplicated().values.any()
    df = pd.merge(df, code_ids, on=['code_system_id', value_col], how='left')
    for num_dig_retry in [4, 3]:
        if df.code_id.isnull().any():
            print(("Trying mapping again at {} digits...".format(
                num_dig_retry)))
            # try again at 4 digits
            filled_mappings = remap_code_id(df, code_ids, num_dig_retry)
            df = df.loc[df['code_id'].notnull()]
            df = df.append(filled_mappings, ignore_index=True)

    # finally, assert everything merged together
    report_if_merge_fail(df, 'code_id', ['code_system_id', value_col])

    return df


def create_nid(df):
    nids = pd.read_excel(
        "FILEPATH"
    )
    nid_dict = nids.set_index("Year")["Merged NID"].to_dict()
    df["nid"] = df["year_id"].map(nid_dict)
    assert df["nid"].notna().all()
    return df


def add_manual_cols(df):
    df["representative_id"] = 1
    df["site"] = ""
    df["data_type_id"] = 9
    df["code_system_id"] = 1
    return df


def format_source():
    df = read_data()
    start_deaths = df.deaths.sum()

    df = get_age_group_ids(df)

    df = get_population_group_id(df)

    df = add_manual_cols(df)

    df = adjust_raw_causes(df)

    # get code_id
    code_system_id = df['code_system_id'].unique()[0]
    cause_map = get_cause_map(code_system_id=code_system_id)
    df = map_code_id(df, cause_map)

    df = create_nid(df)

    df = df[FINAL_FORMATTED_COLS]

    assert df.notnull().values.all()
    df = df.groupby(ID_COLS, as_index=False)[VALUE_COL].sum()

    # run finalize formatting
    locals_present = finalize_formatting(
        df, SOURCE, project_id, write=WRITE, extract_type="US_NCHS_counties_race", check_ages=False
    )
    nid_meta_df = locals_present["nid_meta_df"]

    # update nid metadata status
    if WRITE:
        nid_extracts = (
            nid_meta_df[["nid", "extract_type_id"]]
            .drop_duplicates()
            .to_records(index=False)
        )
        for nid, extract_type_id in nid_extracts:
            nid = int(nid)
            extract_type_id = int(extract_type_id)
            update_nid_metadata_status(
                project_id, nid, extract_type_id, is_active=IS_ACTIVE, is_mort_active=IS_MORT_ACTIVE
            )
