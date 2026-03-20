import contextlib
import csv
import json
import logging
import pathlib
import pickle
import warnings
import os

from dalynator.makedirs_safely import makedirs_safely
from dbload.config.dbconfig import (
    DBConfig,
    is_connection_active,
)
from dbload.load_data import (
    get_translate_area_map,
    translate_age,
)
from dbload.save_covariate_population import (
    get_population_data,
)
from dbload.risk_exp_model import RiskExpModel
from dbload.fatal_model import get_fatal_model_draws_file
import db_queries
import db_tools
import gbd.constants
import gbd.release
import hierarchies.dbtrees
from tater.get_ushd_paf_data import remap_race_codes
from tater.utils.check_prod import using_prod_db

import pandas
import pyreadr


class USHDCache:
    """
    This populates a file-based cache with data to be used by tater pipeline processes.

    USHDDataContainer is expected to be responsible for loading from the file-based cache.
    """
    def __init__(self, cache_dir,
                 release_id,
                 age_group_set_id,
                 location_set_ids,
                 extra_dim, paf_compile_run_id,
                 cause_set_ids,
                 measure_ids,
                 fatal_version_id,
                 rake_version_id,
                 all_year_ids,
                 ):
        self.cache_dir = cache_dir        
        self.release_id = release_id
        self.age_group_set_id = age_group_set_id
        self.location_set_ids = location_set_ids
        self.extra_dim = extra_dim  # "race" or "edu"
        self.paf_compile_run_id = paf_compile_run_id
        self.cause_set_ids = cause_set_ids
        self.measure_ids = measure_ids
        self.fatal_version_id = fatal_version_id
        self.rake_version_id = rake_version_id
        self.all_year_ids = all_year_ids

    def load_caches(self) -> None:
        """
        Populate self.cache_dir with all pertinent files.
        """
        self.ensure_cache_dir_exists()
        res = {}
        # these keys are identical to USHDDataContainer
        res['cause_risk_metadata'] = self._cache_cause_risk_metadata()
        res['pop'] = self._cache_population()
        res['age_spans'] = self._cache_age_spans()
        res['location_hierarchy'] = self._cache_location_hierarchies()
        res['age_weights'] = self._cache_age_weights()
        res['cause_hierarchy'] = self._cache_cause_hierarchy()
        # the DataContainer does not provide this directly. it is included for convenience of testing
        res['all_reis'] = self._cache_all_reis()
        return res

    def ensure_cache_dir_exists(self):
        makedirs_safely(self.cache_dir)

    def _cache_population(self):
        """
        Load population data associated with PAF inputs and save to disk.
        """
        # Note: population is explicitly associated with PAF, fatal, and
        # non-fatal data but only PAF data is required to be used.
        pop_df = _get_population_from_paf_compile_run_id(self.paf_compile_run_id)

        # Replace zero population strata with 1e-12 *before* aggregation
        pop_df.loc[pop_df['pop']==0, 'pop'] = 1.0e-12

        # dev = 100, prod == 2,3
        if self.paf_compile_run_id == 100 or (self.paf_compile_run_id <= 3 and using_prod_db()):
            logging.debug(f"re-mapping race codes for paf_compile_run_id {self.paf_compile_run_id}")
            pop_df = remap_race_codes(pop_df)

        # Update schema
        # add location_id from dict of {mcnty: loc_id} pairs
        map = get_translate_area_map()
        location_id = pandas.Series(map['mcnty'], name='location_id')
        pop_df = pop_df.merge(location_id, left_on='mcnty', right_index=True)

        pop_df = _add_age_group_id(pop_df)

        pop_df.rename(columns={
            "year": "year_id",
            "sex": "sex_id",
            "pop": "pop_scaled",
        }, inplace=True)

        # sum state pop by demographics
        non_pop_data_cols = ['state', 'sex_id', 'age_group_id', 'year_id', self.extra_dim]
        state_df = pop_df.groupby(non_pop_data_cols)[['pop_scaled']].sum().reset_index()

        # add state location_id from dict of {state: loc_id} pairs
        location_id = pandas.Series(map['state'], name='location_id')
        state_df = state_df.merge(location_id, left_on='state', right_index=True)
        state_df = state_df.reset_index(drop=True)

        # sum natl pop by demographics
        non_pop_data_cols = ['sex_id', 'age_group_id', 'year_id', self.extra_dim]
        natl_df = pop_df.groupby(non_pop_data_cols)[['pop_scaled']].sum().reset_index()
        natl_df['location_id'] = 102

        # concat state and natl pop to mcnty dataframe
        pop_df = pandas.concat([pop_df, state_df, natl_df]).reset_index(drop=True)

        # Drop extra columns not in the expected set before assertion and saving
        expected_columns = {'location_id', 'year_id', 'age_group_id', 'sex_id', self.extra_dim, 'pop_scaled', 'race_set'}
        # this assertion is just to tell the programmer if columns are different than expected
        result = set(pop_df.columns).symmetric_difference(expected_columns)
        if (len(result)):
            print(f"Population columns have unexpected (or missing) values: {result}")
        # Drop any extra columns
        extra_columns = set(pop_df.columns) - expected_columns
        if extra_columns:
            pop_df.drop(columns=list(extra_columns), inplace=True)

        # compute "both sexes" population as it will be used
        non_sex_data_columns = ["location_id", "year_id", "age_group_id", self.extra_dim]
        all_sex_df = pop_df.groupby(non_sex_data_columns).sum(numeric_only=True)
        all_sex_df['sex_id'] = gbd.constants.sex.BOTH
        all_sex_df = all_sex_df.reset_index()

        pop_df = pandas.concat([pop_df, all_sex_df]).reset_index(drop=True)

        # compute "all `extra_dim`" population
        non_ed_columns = ["location_id", "year_id", "age_group_id", "sex_id"]
        all_ed_df = pop_df.groupby(non_ed_columns).sum(numeric_only=True)
        all_ed_df[self.extra_dim] = 1
        all_ed_df = all_ed_df.reset_index()

        pop_df = pandas.concat([pop_df, all_ed_df]).reset_index(drop=True)

        # compute "all ages" population
        non_ages_columns = ["location_id", "year_id", self.extra_dim, "sex_id"]
        all_ages_df = pop_df.groupby(non_ages_columns).sum(numeric_only=True)
        all_ages_df['age_group_id'] = gbd.constants.age.ALL_AGES
        all_ages_df = all_ages_df.reset_index()

        # compute "20+" population
        pop_20plus = pop_df[pop_df['age_group_id'].isin([9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 160])]
        age_20plus_df = pop_20plus.groupby(non_ages_columns).sum(numeric_only=True)
        age_20plus_df['age_group_id'] = 37 # gbd.constants.age does not have this
        age_20plus_df = age_20plus_df.reset_index()

        pop_df = pandas.concat([pop_df, all_ages_df, age_20plus_df]).reset_index(drop=True)

        assert (pop_df.loc[pop_df.pop_scaled==0].empty), "Cached population file would have strata with 0.0!"

        cache_file = f"{self.cache_dir}/pop.h5"

        data_columns = ["location_id", "year_id", "age_group_id", "sex_id", self.extra_dim]

        pop_df.to_hdf(cache_file, "pop", data_columns=data_columns, format="table")
        return pop_df

    def _cache_age_spans(self):
        age_spans = db_queries.get_age_spans(
            age_group_set_id=self.age_group_set_id,
            release_id=self.release_id,
        )
        cache_file = f"{self.cache_dir}/age_spans.h5"
        age_spans.to_hdf(cache_file, "age_spans", data_columns=['age_group_id'], format='table')
        return age_spans

    def _cache_location_hierarchies(self):
        results = []
        for location_set_id in self.location_set_ids:
            # NOTE: provide release_id (gbd_release_id + decomp_step no longer used for dalynator 1.0.1)
            loc_hierarchy = hierarchies.dbtrees.loctree(
                location_set_id=location_set_id,
                release_id=self.release_id,
            )

            cache_file = f"{self.cache_dir}/location_hierarchy_{location_set_id}.pickle"
            with open(cache_file, 'wb') as outf:
                pickle.dump(loc_hierarchy, outf)
            results.append(loc_hierarchy)
        return results

    def _cache_age_weights(self):
        age_weights = _get_age_weights_from_paf_compile_run_id(self.paf_compile_run_id)

        if age_weights is None:
            raise RuntimeError("Failed to load age_weights: _get_age_weights_from_paf_compile_run_id returned None. Check that 'age_std_file' is present in settings and the file exists.")

        age_weights = _add_age_group_id(age_weights)
        age_weights.drop(columns=['age'], inplace=True)

        age_weights.rename(columns={"wt": "age_group_weight_value"}, inplace=True)

        cache_file = f"{self.cache_dir}/age_weights.h5"
        age_weights.to_hdf(cache_file, "age_weights", data_columns=["age_group_id"], format="table")

        return age_weights

    def _cache_cause_hierarchy(self):
        trees = []
        for cause_set_id in self.cause_set_ids:
            cause_tree = hierarchies.dbtrees.causetree(
                cause_set_id=cause_set_id,
                release_id=self.release_id)
            trees.append(cause_tree)

        cache_file = pathlib.Path(f"{self.cache_dir}/cause_hierarchy.pickle")

        with cache_file.open("wb") as outf:
            pickle.dump(trees, outf)

        return trees

    def _cache_cause_risk_metadata(self):
        """
        This caches a DataFrame of 100% PAFs with columns rei_id and cause_id.

        These are a special case of cause/risk pairs and is a very small subset
        of the universe of cause/risk pairs because most causes are not 100%
        attributable to a single risk.
        """
        loader = CauseRiskMetadataLoader.from_cache(self)
        res = loader.load_cause_risk_metadata()
        cache_file = f"{self.cache_dir}/cause_risk_metadata.csv"
        res.to_csv(cache_file, columns=['cause_id', 'rei_id'], index=False)
        return res

    def _cache_all_reis(self):
        """
        Load all REIs to be processed as a list and save as a CSV file.
        """
        loader = CauseRiskMetadataLoader.from_cache(self)
        all_reis_by_sex = loader.get_all_reis_by_sex()

        # drop_duplicates() is like unique() but returns a Series
        all_reis = all_reis_by_sex['rei_id'].drop_duplicates()

        cache_file = f"{self.cache_dir}/all_reis.csv"
        all_reis.to_csv(cache_file, index=False)

        return all_reis


def _add_age_group_id(df):
    """
    Add age_group_id to a DataFrame with USHD ages.
    """
    if 'age' not in df.columns:
        raise ValueError("DataFrame does not contain 'age' column - cannot add age_group_id")

    ages = list(sorted(df.age.unique()))
    ids = translate_age(ages=ages)
    age_group_id = pandas.Series(ids, index=ages, name='age_group_id')
    return df.merge(age_group_id, left_on='age', right_index=True)


@contextlib.contextmanager
def temp_database(db_conf: DBConfig, database):
    """
    Temporarily modify a DBConfig object to use the specified database.

    This function will error if db_conf has an active cursor, which is only
    to be expected if this is used within another get_connection_cursor() call.

    Yields a working connection cursor.
    """
    # get_connection_cursor() supports nested connections but we explicitly cannot nest inside an active connection.
    # This is because in nesting the connections get_connection_cursor() re-uses the existing connection which may be
    # to the wrong database
    if db_conf.connection is not None and is_connection_active(db_conf.connection) and db_conf.cursor_count:
        msg = "temp_database can only be used on a DBConfig with an inactive connection"
        raise RuntimeError(msg)

    orig_db = db_conf.database
    db_conf.set_database(database)

    with db_conf.get_connection_cursor() as cursor:
        yield cursor

    db_conf.set_database(orig_db)


def _get_population_from_paf_compile_run_id(paf_compile_run_id):
    """
    Return the population data associated with a paf compile run.
    """
    db_conf = DBConfig()
    model_exp_metadata_id = _get_model_exp_metadata_id_from_paf_compile_run_id(paf_compile_run_id, db_conf)

    # get model exposure metadata from a paf_run_id, which is the settings used to run the exposure SD model
    risk_model = RiskExpModel.from_config(db_conf)

    if(model_exp_metadata_id is None):
        warnings.warn("model_exp_metadata_id is missing. Will load pop from hard-coded value.:\nFILEPATH", Warning)
        pop = pyreadr.read_r('FILEPATH')[None]
    else:
        settings = risk_model.get_model_metadata(model_exp_metadata_id=model_exp_metadata_id)
        pop: pandas.DataFrame = _get_population_from_risk_settings(settings)
    return pop


def _get_model_exp_metadata_id_from_paf_compile_run_id(paf_compile_run_id, db_conf):
    paf_run_ids = _get_paf_run_ids_from_paf_compile_run_id(db_conf, paf_compile_run_id)

    # Assume that as part of uploadling a PAFRunCompile that the related
    # PAFRun's all share a common population file.
    paf_run_id = paf_run_ids[0]

    # get model_exp_run_id AKA model_exp_run_id from paf_run_id
    model_exp_run_id = _get_model_exp_run_id_from_paf_run_id(db_conf, paf_run_id)

    model_exp_metadata_id = _get_model_exp_metadata_id_from_model_exp_run_id(db_conf, model_exp_run_id)
    return model_exp_metadata_id


def _get_paf_run_ids_from_paf_compile_run_id(db_conf, paf_compile_run_id):
    """
    Return the ids associated with the individual PAF calculations that were compiled together.
    """
    with temp_database(db_conf, "risk_database") as cursor:
        cursor.execute(
            """
               SELECT paf_run_id
                 FROM paf_run_paf_compile_run
                WHERE paf_compile_run_id = ?""", [paf_compile_run_id])
        res = cursor.fetchall()

    if res is None:
        msg = f"Unable to get paf_run_id_list for paf_compile_run_id {paf_compile_run_id}"
        raise RuntimeError(msg)
    else:
        return [paf_run_id for paf_run_id, in res]


def _get_model_exp_run_id_from_paf_run_id(db_conf, paf_run_id):
    """
    Returns the model_exp_run_id associated with a paf_run_id.

    This value is the model_exp_run_id which identifies an exposure SD model
    and can be used to retrieve the settings for said model.
    """
    with temp_database(db_conf, "risk_database") as cursor:
        cursor.execute("""
            SELECT model_exp_run_id
            FROM paf_run
            WHERE paf_run_id = ?""", [paf_run_id])
        res = cursor.fetchone()

    if res is None:
        msg = f"Failed get model_exp_run_id for paf_run_id {paf_run_id}"
        raise RuntimeError(msg)

    return res[0]


def _get_model_exp_metadata_id_from_model_exp_run_id(db_conf, model_exp_run_id):
    with temp_database(db_conf, "risk_database") as cursor:
        cursor.execute(
            """
            SELECT model_exp_metadata_id
              FROM model_exp_run
             WHERE model_exp_run_id = ?""", [model_exp_run_id])
        res = cursor.fetchone()

    if res is None:
        msg = f"Failed to get model_exp_metadata_id for model_exp_run_id {model_exp_run_id}"
        raise RuntimeError(msg)

    return res[0]


def _get_population_from_risk_settings(settings):
    """
    Retrieve population implied by risk exposure model settings.

    Args:
        settings: pandas.DataFrame with 2 unnammed columns
            0: field names e.g., "in_overweight_est"
            1: field values

    Mike plans on significantly reworking how settings are handled for risk
    exposure models so much of this function will need to be rewritten.
    """
    # Re moving this line b/c we can simply use the settings from the exp_run rather
    # than getting the settings from the upstream models.
    # input_model_settings = _get_input_settings(settings) 

    # Identify the element in settings "pop_covariate_dataset_id", and cast it to
    # an int
    if all(settings.columns == [0, 1]):
        key = 0
        value = 1
    elif all(settings.columns == ["key", "value"]):
        key = "key"
        value = "value"
    if "pop_covariate_dataset_id" in settings[key].values:
        pop_covariate_dataset_id = int(settings.loc[settings[key] == "pop_covariate_dataset_id", value].values[0])
        print(f"pop_covariate_dataset_id: {pop_covariate_dataset_id}")
        # Load from the database.
        pop = get_population_data(covariate_dataset_id=pop_covariate_dataset_id)
    elif "pop_file" in settings[key].values:
        pop_file = settings.loc[settings[key] == "pop_file", value].values[0]
        if os.path.exists(pop_file):
            warnings.warn(f"pop_covariate_dataset_id is missing from settings:\n{settings}.\nLoading from pop_file = {pop_file}", Warning)
            pop = pyreadr.read_r(pop_file)[None]
        else:
            msg = f"pop_covariate_dataset_id is missing from settings:\n{settings}.\nTried file {pop_file} but it is not a valid file, check that this is running on an archive node."
            raise RuntimeError(msg)
    else:
        msg = f"Neither pop_covariate_dataset_id or pop_file is present in settings: {settings}."
        raise RuntimeError(msg)

    return(pop)
    

def _get_age_weights_from_paf_compile_run_id(paf_compile_run_id):
    """
    Return the age weights data associated with a paf compile run.
    """
    db_conf = DBConfig()

    model_exp_metadata_id = _get_model_exp_metadata_id_from_paf_compile_run_id(paf_compile_run_id, db_conf)

    if(model_exp_metadata_id is None):
            warnings.warn("model_exp_metadata_id is missing. Will load pop weights from hard-coded path: \n FILEPATH", Warning)
            pop = pyreadr.read_r('FILEPATH')[None]
    else:
        # get model exposure metadata from a paf_run_id, which is the settings used to run the exposure SD model
        risk_model = RiskExpModel.from_config(db_conf)
        settings = risk_model.get_model_metadata(model_exp_metadata_id=model_exp_metadata_id)

        pop: pandas.DataFrame = _get_age_weights_from_risk_settings(settings)

    return pop


def _get_age_weights_from_risk_settings(settings):
    # Robustly check for 'age_std_file' in settings
    if all(settings.columns == [0, 1]):
        key = 0
        value = 1
    elif all(settings.columns == ["key", "value"]):
        key = "key"
        value = "value"

    mask = settings[key] == "age_std_file"
    if not mask.any():
        age_std_file = "FILEPATH"
        msg = f"age_std_file is missing from settings:\n{settings}\nusing hard coded {age_std_file}."
        #raise RuntimeError(msg)
    else:
        age_std_file = settings[mask][value].values[0]
    if not age_std_file or not isinstance(age_std_file, str):
        msg = f"age_std_file value is invalid: {age_std_file} in settings: {settings}"
        raise RuntimeError(msg)
    try:
        pop = pyreadr.read_r(age_std_file)[None]
    except Exception as e:
        raise RuntimeError(f"Failed to read age_std_file '{age_std_file}': {e}")
    return pop


class CauseRiskMetadataLoader:
    """
    Helper class for getting data to be cached.

    Methods here are closely copied from the dalynator.cache.Cache codebase in
    hopes of future work to refactor and reuse via import.
    """

    @classmethod
    def from_cache(cls, cache: USHDCache):
        return cls(release_id=cache.release_id,
                   paf_compile_run_id=cache.paf_compile_run_id,
                   measure_ids=cache.measure_ids,
                   fatal_version_id=cache.fatal_version_id,
                   rake_version_id=cache.rake_version_id,
                   all_year_ids=cache.all_year_ids,
                   )

    def __init__(self, release_id, paf_compile_run_id, measure_ids, fatal_version_id, rake_version_id, all_year_ids):
        self.release_id = release_id
        self.paf_compile_run_id = paf_compile_run_id
        self.measure_ids = measure_ids
        self.fatal_version_id = fatal_version_id
        self.rake_version_id = rake_version_id
        self.all_year_ids = all_year_ids

    def load_cause_risk_metadata(self):
        """Get 100 percent pafs metadata for cause-risk pairs"""
        # NOTE: we expect an empty result when our PAF rei's are just 370

        # get reis that exist in paf output
        paf_reis = self._get_rei_from_paf_output()
        existing_reis = set(
            paf_reis.rei_id.unique())

        # get cause-risk metadata from the database
        metadata = self._get_cause_risk_metadata_from_database()
        metadata_reis = set(
            metadata.rei_id.unique())

        # filter down to the risks that we have paf output for AND metadata for
        usable_reis = list(
            existing_reis & metadata_reis)
        result = metadata.loc[metadata.rei_id.isin(usable_reis)]

        return result[['cause_id', 'rei_id']]

    def _get_rei_from_paf_output(self):
        """
        Get rei_id values from PAF output.

        This differs from the dalynator Cache in it's use of the database
        instead of checking an equivalent datastructure to j['existing_reis']
        saved as existing_reis.csv.gz
        """
        db_conf = DBConfig()
        # select settings
        with temp_database(db_conf, "risk_database") as cursor:
            cursor.execute(
                """
                SELECT settings
                  FROM paf_compile_metadata AS META
                  JOIN paf_compile_run      AS RUN
                    ON META.paf_compile_metadata_id = RUN.paf_compile_metadata_id
                 WHERE RUN.paf_compile_run_id = ?""", [self.paf_compile_run_id])

            res = cursor.fetchone()
            if res is None:
                raise RuntimeError(f"No settings found for paf_compile_run_id {self.paf_compile_run_id}")
            else:
                settings, = res

        # load JSON as j
        j = json.loads(settings)

        # get data
        existing_reis_data = j['existing_reis']
        res = pandas.DataFrame(existing_reis_data)

        # it's easier when you know what is in the data
        assert set(res.columns) == {'rei_id', 'cause_id', 'measure_id', 'sex_id'}

        # perform replacement if necessary
        cond = (gbd.constants.measures.YLL not in self.measure_ids and
                gbd.constants.measures.DEATH not in res['measure_id'].unique())
        if cond:
            res = res.replace(to_replace={'measure_id': gbd.measures.YLL},
                              value={'measure_id': gbd.measures.DEATH})

        return res

    def _get_cause_risk_metadata_from_database(self, gbd_round_id=7):
        """
        Return DataFrame of 100% risk-outcome pairs.

        For example, drug use disorders are 100% attributable to drug use.

        Returns DataFrame with columns: cause_id, rei_id, cause_risk_metadata_type_id, metadata_val
        """
        # Unfortunately, until get_rei_metadata is updated to include as many
        # gbd_metadata_types as we want to include, we have to put a direct sql
        # query in here. Taken from db_queries.core.utils.metadata - this code was not found by CC
        q = """
            SELECT
                cm.cause_id,
                cm.rei_id,
                cm.cause_risk_metadata_type_id,
                cm.cause_risk_metadata_value as metadata_val
            FROM
                shared.cause_risk_metadata_history cm
            JOIN
                (SELECT
                    max(mh.cause_risk_metadata_version_id)
                    as cause_risk_metadata_version_id
                FROM
                    shared.cause_risk_metadata_version cmv
                JOIN
                    shared.cause_risk_metadata_history mh
                    ON cmv.cause_risk_metadata_version_id =
                    mh.cause_risk_metadata_version_id
                WHERE
                    cmv.gbd_round_id = {gbd_round_id}) cmv ON
                cmv.cause_risk_metadata_version_id=cm.cause_risk_metadata_version_id
            WHERE
                cm.cause_risk_metadata_type_id =1
                and cm.cause_risk_metadata_value = 1
                """.format(gbd_round_id=gbd_round_id)
        metadata = db_tools.ezfuncs.query(q, conn_def='cod')
        # cause_risk_metadata is required to correct rounding errors. Raise if there's no
        # cause_risk_metadata_version for this round or no metadata for PAFs of 1
        if metadata.empty:
            raise ValueError(
                f"No metadata for PAFs of 1 found for release_id {self.release_id}."
            )

        return metadata

    def get_all_reis_by_sex(self):
        """
        Return all REIs that can be computed given the PAF/fatal/non-fatal inputs.

        Returns a DataFrame with measure_id/sex_id/cause_id/rei_id columns
        """
        paf_rei_df = self._get_rei_from_paf_output()

        fatal_causes_df = self._get_fatal_cause_df()
        nonfatal_causes_df = self._get_nonfatal_cause_df()

        causes_df = pandas.concat([fatal_causes_df, nonfatal_causes_df]).reset_index(drop=True)

        reis_by_sex, paf_only_causes = self._get_intersection_and_difference_of_paf_and_cause_inputs(
            paf_rei_df, causes_df)
        self.log_mismatched_causes(paf_only_causes)
        return reis_by_sex

    def _get_fatal_cause_df(self):
        """
        Return a DataFrame with the fatal cause list by measure_id/sex_id/cause_id.

        If no fatal model is provided return an empty DataFrame with these columns.
        """

        by_measure = []
        for draw_type, measure_id in [["mort", gbd.constants.measures.DEATH], ["yll", gbd.constants.measures.YLL]]:
            files_df = get_fatal_model_draws_file(
                model_version_ids=[self.fatal_version_id],
                rake_version_ids=[self.rake_version_id],
                draw_type=draw_type,
                geo_level='mcnty',  # mcnty locations are the "most detailed" locations
                # trick: models are run for the same causes for all years, so we can just check the first year
                years=self.all_year_ids[:1],
            )
            assert 'cause_id' in files_df.columns, "You need to update dbload to return cause_id"
            df = files_df[['cause_id', 'sex']]
            df['measure_id'] = measure_id
            by_measure.append(df)

        result = pandas.concat(by_measure).rename(columns={'sex': 'sex_id'}).reset_index(drop=True)
        return result

    def _get_nonfatal_cause_df(self):
        """
        Return a DataFrame with the non-fatal cause list by measure_id/sex_id/cause_id.

        If no non-fatal model is provided return an empty DataFrame with these columns.
        """
        logging.warning("Not checking non-fatal cause list and assuming no non-fatal model is provided")
        return pandas.DataFrame(columns=['measure_id', 'sex_id', 'cause_id'])

    def _get_intersection_and_difference_of_paf_and_cause_inputs(self, paf_rei_df, causes_df):
        """
        Return the *inner join* and the causes from the paf_rei_df not present in causes_df.
        """
        # do an outer join
        outer = pandas.merge(paf_rei_df, causes_df,
                             how='outer',
                             on=['measure_id', 'sex_id', 'cause_id'],
                             # generate an extra column named _merge with
                             # values "both", "left_only", or "right_only" to
                             # provide details about each row's source data
                             indicator=True,
                             )

        # get the things that would removed by an inner join
        paf_only_mask = outer['_merge'] == 'left_only'
        paf_only = outer.loc[paf_only_mask, ['cause_id', 'measure_id']]

        # get the inner join
        inner_mask = outer['_merge'] == "both"
        inner = outer.loc[inner_mask, ['measure_id', 'sex_id', 'cause_id', 'rei_id']]

        # return
        return inner, paf_only

    def log_mismatched_causes(self, df):
        template = "Cause {cause} exists in PAF data but is missing from measures {measures}"
        for cause_id, idx in df.groupby('cause_id').groups.items():
            measures = df.loc[idx, 'measure_id'].unique()
            logging.info(template.format(cause=cause_id, measures=measures))
