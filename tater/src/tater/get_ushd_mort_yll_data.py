import pyreadr
import pandas as pd
from pathlib import Path

import gbd.constants as gbd

from dbload.config.dbconfig import DBConfig
from dbload.fatal_model import get_fatal_model_draws_file
from dbload.load_data import (
    get_translate_area_map,
    translate_age
)


from tater.utils.check_prod import using_prod_db
from tater.get_ushd_paf_data import remap_race_codes, check_for_duplicate_rows

def get_data_frame(model_version_id=None, rake_version_id=None, draw_type=None, year=None, extra_dim=None,
                   raked=True, location_id=None, acauses=None):
    """
    Get the draws data from the list of draws-files in ushd database for the arguments passed and
    transforms dataframe to match the dalynator-equivalent draws-schema.
    Args:
        model_version_id (int): id representing model_version for the model_draw_file.
        rake_version_id (int): id representing rake_version for the model_draw_file.
        draw_type (str): "mort" or "yll".
        year (str): year from user input.
        extra_dim (str): "race" or "edu".
        raked (boolean): Determines whether to return raked draws or unraked.
        location_id (int): id representing location for draws data.
        acauses (list): list of acause strings to extract
    Returns:
        DataFrame of fatal (mx or yll) draws data.
    """

    if draw_type is None or draw_type not in ["mort", "yll"]:
        raise NameError(f"draw_type is {draw_type}. Either 'mort' or 'yll' must be specified.")

    if extra_dim not in ['race', 'edu']:
        raise ValueError(f"Invalid extra_dim: {extra_dim}. One of 'race' or 'edu' should be specified.")

    # predeclare empty loc_maps
    loc_maps = {}
    # remove "all race" or "all edu" aggregates from results. for new codes in both cases the code is 1
    # for old codes remove 9.  Someday we will no longer use the old race codes

    if acauses:
        l_df = []
        for acause in acauses:
            l_df.append(get_fatal_model_draws_file(
            model_version_ids=[model_version_id], rake_version_ids=[rake_version_id], acause=acause, draw_type=draw_type, years=[year], raked=raked))
        draw_files_df = pd.concat(l_df, axis=0, ignore_index=True)
    else:
        draw_files_df = get_fatal_model_draws_file(model_version_ids=[model_version_id], rake_version_ids=[rake_version_id], draw_type=draw_type, years=[year], raked=raked)

    if (9 in draw_files_df[extra_dim].unique()):
        draw_files_df = draw_files_df.loc[draw_files_df[extra_dim] != 9]
    else:
        draw_files_df = draw_files_df.loc[draw_files_df[extra_dim] != 1]
    if len(draw_files_df) == 0:
        raise FileNotFoundError("No draw files were found for the given parameters.")
    # subset by mcnty, state or natl
    if 573 < location_id <= 94092:
        lvl = "mcnty"
    elif location_id == 102:
        lvl = "natl"
    elif 523 <= location_id <= 573:
        lvl = "state"
    else:
        raise ValueError(f"Location ID = {location_id} not processed by USHD taternator.")

    draw_files_df = draw_files_df.loc[draw_files_df['level'] == lvl]
    file_prefix = "mx" if draw_type == "mort" else "yll"
    # For mcnty work use optimized draws files if present.
    if Path(Path(draw_files_df['file_path'].iloc[0]).parent, "draws_mcnty").is_dir() and lvl == "mcnty":
        print("Using optimized draws_mcnty dir")
        if (len(loc_maps) == 0):
            loc_maps = get_translate_area_map()
        mcnty_id = [i for i in loc_maps['mcnty'] if loc_maps['mcnty'][i]==location_id][0]

        # select [file_path, cause_id], replace file name and path with draws_mcnty modified path
        draw_files = draw_files_df[["file_path", "cause_id"]].replace(to_replace=rf"/{file_prefix}_draws_mcnty", value=f"/draws_mcnty/{mcnty_id}/{file_prefix}_draws_{mcnty_id}", regex=True).reset_index()
    else:
        # select [file_path, cause_id]
        #pop_id = "pop_289"
        pop_id = None
        if pop_id:
            draw_files = draw_files_df[["file_path", "cause_id"]].replace(to_replace=rf"/{file_prefix}_draws", value=f"/draws_agg_{pop_id}/{file_prefix}_draws", regex=True).reset_index()
        else:
            draw_files = draw_files_df[["file_path", "cause_id"]].reset_index()
        if lvl == "mcnty":
                print("Not using optimized draws_mcnty dir, this will take a LONG TIME, or be OOM killed, or both")

    # predeclare lists of files
    df_files = []
    files_not_found = []
    for idx, draw_file in draw_files.iterrows():
        file = Path(draw_file.file_path)
        if (idx % 100 == 0):
            print(f"Reading idx = {idx} of {len(draw_files)} rows file = {draw_file.file_path}", flush=True)
        is_rds = file.suffix.lower() == '.rds'

        # read file
        if is_rds:
            try:
                df_file = pyreadr.read_r(file)[None]
                if 'index' in df_file:
                    del df_file['index']

            except pyreadr.custom_errors.PyreadrError:
                files_not_found.append(str(file))
                continue
        else:
            try:
                df_file = pd.read_csv(file)
            except FileNotFoundError:
                files_not_found.append(str(file))
                continue

        # This will be used to create n_draws column
        max_draws = max(df_file.sim)

        # add location_id to the dataframe and select records that match.
        df_file, loc_maps = add_and_filter_by_location_ids(df_file, location_id, loc_maps=loc_maps)

        # pivot (transform long to wide format) the sim column
        value_col = "mx" if draw_type == "mort" else "yll"  # The value cols are: 'mx' for death and 'yll' for yll.
        index_cols = [x for x in df_file.columns if x not in ["sim", value_col]]
        df_file = df_file.pivot_table(index=index_cols, columns="sim", values=value_col)
        # dalynator expects draws columns in range: [draw_0, draw_1, ... draw_999]
        # ushd has sim in range: [1, 2, ..., 1000]
        df_file.columns = [f"draw_{int(col-1)}" for col in df_file.columns]
        df_file = df_file.reset_index()

        # assign cause_id, measure_id, and n_draws which are required for burdenator data schema
        if is_rds:
            cause_id = draw_file.cause_id
        else:
            acauses = df_file['acause'].tolist()
            del df_file['acause']
            cause_id = acauses_to_cause_ids(acauses)

        measure_id = gbd.measures["DEATH"] if draw_type == "mort" else gbd.measures["YLL"]
        # add and drop remaining constant columns to match the dalynator draws schema
        df_file = df_file.assign(
            cause_id=cause_id,
            measure_id=measure_id,
            n_draws=max_draws,
        )

        # rename columns to match burdenator data schema
        df_file = df_file.rename(columns={'year': 'year_id', 'sex': 'sex_id'})

        # drop extra columns not part of schema
        df_file = df_file.drop(["level", "area"], axis=1)

        df_files.append(df_file)

    if len(files_not_found) > 0:
        raise FileNotFoundError(f"Draws files were not found: \n{files_not_found}")

    df = pd.concat(df_files, axis=0, ignore_index=True)

    # add age_group_id
    ages = list(df.age.unique())
    age_ids = translate_age(ages=ages)
    df_age_ids = pd.DataFrame({
         "age": ages,
         "age_group_id": age_ids
    })
    df = df.merge(df_age_ids, how="left", on="age")
    
    # drop extra columns not part of schema
    df = df.drop(["age"], axis=1)


    if set(df.race.unique()) == {1, 2, 3, 4, 7}:
        df = remap_race_codes(df)
    if 'acause' in df.columns:
        del df['acause']
    if 'edu' not in df.columns:
        print("missing edu, adding it now")
        df['edu'] = 1
    elif df['edu'].isnull().values.any():
        print("edu has NaN, overrwriting with 1 now")
        df['edu'] = 1

    if 'race' not in df.columns:
        print("missing race, adding it now")
        df['race'] = 1

    df = check_for_duplicate_rows(df)

    return df


def add_and_filter_by_location_ids(df, location_id=None, *,loc_maps):
    """Adds 'location_id' column to the dataframe by mapping from 'level','area' columns.
    Args:
        df (DataFrame): Draws data with 'level' and 'area' columns from which 'location_id' column is created.
        location_id (int): If specified, the rows not matching this 'location_id' are removed.
        loc_maps (dict): Required keyword argument.  skip data base hit if loc_maps already populated.
    Returns:
        Dataframe with 'location_id' column.
    """

    if (len(loc_maps) == 0):
        loc_maps = get_translate_area_map()
        print("getting get_translate_area_map()")
    df_loc_list = []

    for lvl in ["natl", "state", "mcnty"]:
        df_lvl = pd.DataFrame(list(loc_maps[lvl].items()), columns=["area", "location_id"])
        df_lvl["level"] = lvl
        df_loc_list.append(df_lvl)

    # Location mapping DataFrame with ['area', 'level', 'location_id'] columns
    df_locs = pd.concat(df_loc_list, axis=0, ignore_index=True)

    # To speed up, get level,area for matching location_id and filter out before merge.
    if location_id:
        df_locs = df_locs[df_locs.location_id == location_id]
        levels = df_locs["level"].unique()
        areas = df_locs["area"].unique()
        df = df[df["level"].isin(levels)]
        df = df[df["area"].isin(areas)]

    df_merged = df.merge(df_locs, how="left", on=["level", "area"])
    return df_merged, loc_maps


def acauses_to_cause_ids(acauses):
    "Returns a list of cause_ids corresponding to acauses argument."
    db_conf = DBConfig("fatal_database")
    with db_conf.get_connection_cursor() as cursor:
        in_ = ",".join(f"'{acause}'" for acause in acauses)
        sql = f"SELECT acause, cause_id FROM shared.cause WHERE acause IN ({in_})"
        cursor.execute(sql)
        res = cursor.fetchall()
    # be sure the results are explicitly ordered
    lookup = dict(res)
    return [lookup[acause] for acause in acauses]
