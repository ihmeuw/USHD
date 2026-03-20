from dalynator.data_container import DataContainer
from dalynator.lib.utils import get_index_draw_columns

from transforms.transforms import transform_metric, DEFAULT_MERGE_COLUMNS

from tater import (
    get_ushd_mort_yll_data,
    get_ushd_paf_data,
)
import gbd.constants

from dbload.config.dbconfig import DBConfig
from dbload.model import Model

from dalynator.aggregate_causes import AggregateCauses

class USHDDataContainer(DataContainer):
    """A DataContainer-subclass that loads USHD specific draws for mainly 5 measures:
    'yll', 'yld', 'death', 'paf', 'daly'

    Provide simple caching and key-based retrieval for the limited
    'input data' types [yll, yld, death, pop, paf] that are used in the
    Taternator.

    Makes DataFrame access look like this...
        my_data_container['yll']

    ... and helps eliminate redundant trips to the filesystem/db
    """

    def __init__(self, cache_granularity_dict, n_draws, gbd_release_id,
                 age_group_ids=None, age_group_set_id=None, cache_dir=None,
                 fatal_version_id=None, rake_version_id=None, nonfatal_run_id=None, 
                 paf_compile_run_id=None, extra_dim=None, pop_merge_columns=None):
        """Initialize the DataContainer with proper directories and
        scope as determined in the cache_granularity_dict. Usually these would
        be loc/year.

        Args:
            cache_granularity_dict (dict): dictionary outlining the scope of
                the container, usually in the form of {'location_id': loc_id,
                'year_id', year_id}
            n_draws (int): Number of draws to resample raw data to (for YLD,
                YLL, Deaths, DALYs, PAFs)
            gbd_release_id (int): The id of the GBD Release (see shared.gbd_release
                table in the db for mapping to the year)
            age_group_ids (List[int]): The aggregate age groups to calculate
                for summarization, other than all-age and age-standardized
            age_group_set_id (int): age_group_set_id is used to query age trees.
            daly_dir (str): Root directory where DALY data are stored
            cache_dir (str): Root directory where all caches are stored
            fatal_version_id (int): model_version_id of model_draw_file table in ushd_fatal database.
            rake_version_id (int): rake_version_id of model_draw_file table in ushd_fatal database.
            nonfatal_run_id (int): model_run_id of model_draw_file table in ushd_nonfatal database.
            paf_compile_run_id (int): paf_run_id of paf_draw_file table in ushd_risk database.
            extra_dim (str): 'race' or 'edu' that specifies whether to load mort or yll data
                by race or by edu respectively.
            pop_merge_columns: list of columns to use when merging population. This will default
                to transforms.transforms.DEFAULT_MERGE_COLUMNS + extra_dim. This is expected to be
                e.g., ['location_id', 'year_id', 'age_group_id', 'sex_id', 'race'] where extra_dim is 'race'

        """
        if pop_merge_columns is None:
            pop_merge_columns = DEFAULT_MERGE_COLUMNS + [extra_dim]

        super().__init__(
            cache_granularity_dict=cache_granularity_dict,
            n_draws=n_draws,
            release_id=gbd_release_id,
            age_group_ids=age_group_ids,
            age_group_set_id=age_group_set_id,
            cache_dir=cache_dir,
            pop_merge_columns=pop_merge_columns,
        )
        self.fatal_version_id = fatal_version_id
        self.rake_version_id = rake_version_id
        self.nonfatal_run_id = nonfatal_run_id
        self.paf_compile_run_id = paf_compile_run_id
        self.extra_dim = extra_dim

    def _get_df_by_key(self, key):
        """Return a DataFrame for 'key' type of data"""
        self.validate_cache_granularity_dict(key)
        if not self._is_valid_indata_type(key) and "location_hierarchy" not in key:
            raise KeyError(
                "key must be one of ['{}']".format("', '".join(self.valid_indata_types))
            )

        if key == "yll":
            if self.fatal_version_id is None:
                raise NameError(
                    "fatal_version_id must be specified on the "
                    "USHDDataContainer to retrieve YLL data"
                )
            # This list is set in run_taternator_cleanup.py
            if hasattr(self, 'l_acauses'):
                l_acauses = []
                for cause_id in self.l_acauses:
                    l_acauses.append(Model(DBConfig("fatal_database")).get_acause_from_cause_id(cause_id = cause_id))
            elif ('paf' in self.cached_values):
                # this is a complicated way to get the list of cause dependencies
                index_columns = ['age_group_id', 'cause_id', 'edu', 'location_id', 'measure_id', 'race', 'rei_id', 'sex_id', 'year_id']
                paf_df = self.cached_values['paf']
                cause_hierarchy = self.cached_values['cause_hierarchy'][0]
                ac = AggregateCauses(
                        cause_hierarchy,
                        paf_df,
                        index_columns=index_columns)
                paf_df = ac.get_data_frame()
                l_acauses = []
                for cause_id in paf_df['cause_id'].unique():
                    l_acauses.append(Model(DBConfig("fatal_database")).get_acause_from_cause_id(cause_id = cause_id))

            else:
                raise ValueError("paf must be loaded before ylls to get cause_ids")

            yll_df = get_ushd_mort_yll_data.get_data_frame(
                model_version_id=self.fatal_version_id,
                rake_version_id=self.rake_version_id,
                draw_type="yll",
                year=self.cache_granularity_dict["year_id"],
                extra_dim=self.extra_dim,
                raked=True,
                location_id=self.cache_granularity_dict["location_id"],
                acauses=l_acauses,
            )

            # ushd yll draws are stored in rate space. Convert draws to number space
            yll_df = self._convert_rate_to_num(yll_df)

            # reorder columns
            index_cols, draw_cols = get_index_draw_columns(yll_df)
            df = yll_df[index_cols + draw_cols]
        elif key == "death":
            if self.fatal_version_id is None:
                raise NameError(
                    "fatal_version_id must be specified on the "
                    "USHDDataContainer to retrieve death data."
                )
            mort_df = get_ushd_mort_yll_data.get_data_frame(
                model_version_id=self.fatal_version_id,
                rake_version_id=self.rake_version_id,
                draw_type="mort",
                year=self.cache_granularity_dict["year_id"],
                extra_dim=self.extra_dim,
                raked=True,
                location_id=self.cache_granularity_dict["location_id"],
            )

            # ushd mort/death/mx draws are stored in rate space. Convert draws to number space
            mort_df = self._convert_rate_to_num(mort_df)

            # reorder columns
            index_cols, draw_cols = get_index_draw_columns(mort_df)
            df = mort_df[index_cols + draw_cols]
        elif key == "paf":
            if self.paf_compile_run_id is None:
                raise NameError(
                    "paf_compile_id must be specified on the "
                    "USHDDatacontainer to retried paf data."
                )

            paf_df = get_ushd_paf_data.get_data_frame(
                paf_compile_run_id=self.paf_compile_run_id,
                location_id=self.cache_granularity_dict["location_id"],
                year=self.cache_granularity_dict["year_id"],
            )

            self._validate_pafs(paf_df)
            # reorder columns
            index_cols, draw_cols = get_index_draw_columns(paf_df)
            df = paf_df[index_cols + draw_cols]
        elif key == "daly":
            raise NotImplementedError(
                "USHD 'daly's not available. This will be implemented in future."
            )
        elif key == "yld":
            raise NotImplementedError(
                "USHD 'yld's data not available. This will be implemented in future."
            )
        else:
            # delegate to the base class DataContainer
            super()._get_df_by_key(key)
            return
        # Use local simple resample (first N draws) instead of default dalynator resample
        df = self._simple_resample(df, key)
        self.cached_values[key] = df

    def _convert_rate_to_num(self, rate_df):
        return transform_metric(
            df=rate_df,
            from_id=gbd.constants.metrics.RATE,
            to_id=gbd.constants.metrics.NUMBER,
            pop_df=self.__getitem__("pop"),
            merge_columns=self.pop_merge_columns,
        )

    def _simple_resample(self, df, key):
        '''Potentially resample raw data, if dataframe has different number
        of draws from self.n_draws.
        '''
        if key not in self.resample_types:
            return df

        # List to hold the columns we want to drop
        columns_to_drop = ["n_draws"]

        # Iterate through each column in the DataFrame
        for column in df.columns:
            # Check if the column name starts with 'draw_'
            if column.startswith('draw_'):
                # Extract the draw number from the column name
                draw_number = int(column.split('_')[1])
                # If the draw number is greater than self.n_draws, mark it for removal
                if draw_number >= self.n_draws:
                    columns_to_drop.append(column)

        # Drop the columns from the DataFrame
        df = df.drop(columns=columns_to_drop)

        return df
