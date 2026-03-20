from pathlib import Path
import pandas

from dalynator.lib.utils import get_index_draw_columns

from dbload.paf_compile import get_paf_compile_draw_file

from tater.utils.check_prod import using_prod_db

def get_data_frame(paf_compile_run_id=None, location_id=None, year=None):
    """
    Return ushd paf data from paf_draw_file_compile path saved in ushd database.
    Args:
        paf_compile_run_id (int): id representing paf_run_compile saved in paf_draw_file_compile table.
        location_id (int): id representing location.
        year (int): year from user input.
    Returns:
        DataFrame of paf draws.
    """
    draw_file_df = get_paf_compile_draw_file(
        paf_compile_run_id=paf_compile_run_id, location_id=location_id, year_id=year
    )
    # Assumes that only one record is returned.
    assert len(draw_file_df) == 1, f"not 1 draw file for {paf_compile_run_id} / {location_id} / {year}"
    draw_file = Path(draw_file_df[0])

    try:
        df = pandas.read_csv(draw_file, index_col=0)
    except FileNotFoundError:
        raise FileNotFoundError(f"Draw file not found: {draw_file}")

    # add n_draws column
    _, draw_cols = get_index_draw_columns(df)
    max_draws = int(draw_cols[-1].split("_")[-1]) + 1  # 0-indexed
    df = df.assign(n_draws=max_draws)
    # 100 = dev 2,3 = prod
    if paf_compile_run_id == 100 or (paf_compile_run_id <= 3 and using_prod_db()):
        # HACK - fix race codes
        df = remap_race_codes(df)
    
    if 'race' not in df.columns:
        raise ValueError(f"Draw file {draw_file_df[0]} missing race column")
    if 'edu' not in df.columns:
        # raise ValueError(f"Draw file {draw_file_df[0]} missing edu column")
        msg = f"Draw file {draw_file_df[0]} missing edu column. Adding edu = 1"
        # warnings.warn(msg)
        df['edu'] = 1  # all levels of education

    df = check_for_duplicate_rows(df)

    return df


def remap_race_codes(df):
    """
    Re-maps race codes in df.

    Remaps Key

    Race        From    To
    Hispanic    7       2
    NH Black    2       4
    NH White    1       5
    NH AIAN     3       6
    NH API      4       7
    """
    assert set(df.race.unique()) == {1, 2, 3, 4, 7}, "Unexpected race codes."

    return df.replace({'race': {
        7: 2,  # Hispanic
        2: 4,  # NH Black
        1: 5,  # NH White
        3: 6,  # NH AIAN
        4: 7,  # NH API
    }})


def check_for_duplicate_rows(df):
    """
    Checks for duplicate rows in returned dataframe - database had some
    """
    df_deduped = df.drop_duplicates()
    n_dupe_rows = df.shape[0] - df_deduped.shape[0]
    if n_dupe_rows > 0:
        print(f"{n_dupe_rows} rows were duplicated and removed - check non-zero causes below:")
        print(df['cause_id'].value_counts() - df_deduped['cause_id'].value_counts())

    return df_deduped
