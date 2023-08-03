import warnings

import numpy as np
import pandas as pd
from hierarchies import tree

from cod_prep.claude.cod_process import CodProcess
from cod_prep.claude.configurator import Configurator
from cod_prep.downloaders.ages import add_age_metadata
from cod_prep.downloaders.causes import (
    add_cause_metadata,
    get_all_related_causes,
    get_current_location_hierarchy
)
from cod_prep.utils import report_if_merge_fail, tree_utils, print_log_message, CodSchema


class Recoder(CodProcess):
    def __init__(self, cause_meta_df, loc_meta_df, source, code_system_id, data_type_id):
        self.source = source
        self.code_system_id = code_system_id
        self.data_type_id = data_type_id
        self.cause_meta_df = cause_meta_df
        self.loc_meta_df = loc_meta_df
        self.conf = Configurator("standard")
        self.vr_indicators_path = self.conf.get_resource('vr_indicators')
        self.cache_options = {
            'force_rerun': False,
            'block_rerun': True,
            'cache_results': False,
            'cache_dir': self.conf.get_directory('db_cache')
        }

    def get_computed_dataframe(self, df):
        if 'data_type_id' not in df.columns:
            df['data_type_id'] = self.data_type_id
        df = self.recode(df)
        df = self.conform_secret_causes(df)
        df = self.clean_up(df)

        return df

    def get_diagnostic_dataframe(self):
        """Return diagnostics."""
        pass

    def clean_up(self, df):
        """Group rogue duplicates."""
        df.drop(columns="data_type_id", inplace=True)
        schema = CodSchema.infer_from_data(df)
        df = df.groupby(schema.id_cols, as_index=False)[schema.value_cols].sum()
        return df

    def conform_secret_causes(self, df):
        """Remove secret causes and conform to reporting cause hierarchy."""
        df = add_cause_metadata(
            df, add_cols=['secret_cause', 'parent_id'],
            cause_meta_df=self.cause_meta_df,
            **self.cache_options
        )
        injuries_replace_parents = [722, 720, 719]
        replaced_injuries = df['cause_id'].isin(injuries_replace_parents)
        df.loc[replaced_injuries, 'parent_id'] = 723
        secret_causes = df['secret_cause'] == 1
        not_cc_code = df['cause_id'] != 919
        len_before = len(df)
        if df['parent_id'].isnull().values.any():
            raise AssertionError(
                'There are missing parent cause_ids'
            )
        df.loc[secret_causes & not_cc_code, 'cause_id'] = df['parent_id']
        len_after = len(df)
        if len_before != len_after:
            raise AssertionError(
                'The length of the dataframe has changed from {} to {}'.format(
                    len_before, len_after
                )
            )
        df.drop(['parent_id', 'secret_cause'], axis=1, inplace=True)
        return df

    def drop_leukemia_subtypes(self, df):
        """Remove leukemia subtypes deaths created by redistribution.

        Request made by Tina GBD 2017 and supported by Mohsen. Deaths
        that are created in redistribution for leukemia subtypes should
        be recoded to the parent leukemia.
        """
        leuk_subtypes = get_all_related_causes('neo_leukemia', self.cause_meta_df)
        location_metadata = get_current_location_hierarchy()

        df = df.merge(location_metadata[['location_id','parent_id']], on = 'location_id')
        df['parent_id'] = np.where(df['parent_id'] == 102, df['location_id'], df['parent_id'])
        leuk_keep_cols = ['nid', 'extract_type_id', 'parent_id', 
                                  'year_id', 'age_group_id', 'sex_id', 'cause_id', 
                                  'site_id']
        df_agg_leuk_drop =  df.groupby(leuk_keep_cols).agg({'deaths': 'sum', 
                                                                'deaths_rd': 'sum',
                                                                'deaths_corr': 'sum',
                                                                'deaths_raw': 'sum'}).reset_index()

        df_agg_leuk_drop = df_agg_leuk_drop[
            (df_agg_leuk_drop['cause_id'].isin(leuk_subtypes)) & 
            (df_agg_leuk_drop['deaths_rd'] > 0) & 
            (df_agg_leuk_drop['deaths_raw'] <= 0)][leuk_keep_cols]

        df_agg_leuk_drop['drop_leuk'] = 1
        df = df.merge(df_agg_leuk_drop, on = leuk_keep_cols, how = 'left')

        df.loc[df.drop_leuk == 1, 'cause_id'] = 487
        df = df.drop(['parent_id', 'drop_leuk'], axis=1)

        return df

    def recode(self, df):
        cause_metadata_df = self.cause_meta_df
        cause_metadata_df = cause_metadata_df[["cause_id",
                                               "path_to_top_parent",
                                               "acause"]]
        age_add_cols = [
            f'age_group_{unit}_{side}'
            for unit in ['days', 'years']
            for side in ['start', 'end']]
        df = add_age_metadata(df, age_add_cols, **self.cache_options)
        for col in age_add_cols:
            report_if_merge_fail(df, col, 'age_group_id')

        ckd_cause_ids = get_all_related_causes('ckd', cause_metadata_df)
        ckd_cause_ids.remove(593)
        ckd_less_other = df['cause_id'].isin(ckd_cause_ids)
        neonate = df.age_group_days_end <= 27
        vr = df['data_type_id'] == 9
        df.loc[ckd_less_other & neonate & vr, 'cause_id'] = 652

        resp_ids = [509, 515, 516, 520]
        is_cert_resp_causes = df['cause_id'].isin(resp_ids)
        # neonate already defined
        df.loc[is_cert_resp_causes & neonate, 'cause_id'] = 322

        is_asthma = df['cause_id'] == 515
        perinates = (df.age_group_days_start >= 28) & (df.age_group_days_end <= 364)
        df.loc[is_asthma & perinates, 'cause_id'] = 322

        maternal_cause_ids = get_all_related_causes(366, cause_metadata_df)
        maternal_cause_ids = df['cause_id'].isin(maternal_cause_ids)
        # ages not in the maternal age range
        non_maternal_ages = ~((df.age_group_years_end > 10) & (df.age_group_years_start < 55))
        df.loc[maternal_cause_ids & non_maternal_ages, 'cause_id'] = 919

        alzheimers = df['cause_id'] == 543
        under_40 = df['age_group_years_end'] <= 40
        df.loc[alzheimers & under_40, 'cause_id'] = 919

        cong_causes = get_all_related_causes('cong', cause_metadata_df)
        congenital = df['cause_id'].isin(cong_causes)
        over_70 = df['age_group_years_start'] >= 70
        df.loc[congenital & over_70, "cause_id"] = 919

        hepatitis = get_all_related_causes(400, cause_metadata_df)
        hepatitis = df['cause_id'].isin(hepatitis)
        if self.code_system_id in [7, 9]:
            df.loc[hepatitis & neonate, "cause_id"] = 380
        else:
            df.loc[hepatitis & neonate, "cause_id"] = 384

        inj_disaster_light = df['cause_id'] == 984
        df.loc[inj_disaster_light, 'cause_id'] = 716

        if self.code_system_id not in [1, 6]:
            ckd_diabetes = df['cause_id'].isin([997, 998])
            df.loc[ckd_diabetes, 'cause_id'] = 589

        diabetes_type_2 = df['cause_id'] == 976
        under_15 = df['age_group_years_end'] <= 15
        df.loc[diabetes_type_2 & under_15, 'cause_id'] = 975

        iron_or_iodine = df['cause_id'].isin([388, 390])
        df.loc[iron_or_iodine, 'cause_id'] = 919

        under_1 = df['age_group_years_end'] <= 1
        cvd_ihd = df['cause_id'] == 493
        df.loc[cvd_ihd & under_1, 'cause_id'] = 643

        df.loc[df.cause_id.isin([344, 409, 410,
                                 542, 558, 669,
                                 680, 961]), 'cause_id'] = 919
        if self.data_type_id not in [6, 7, 8]:
            df.loc[df['cause_id'] == 687, 'cause_id'] = 919

        one_to_14 = (df.age_group_years_start >= 1) & (df.age_group_years_end <= 15)
        cvd_ihd = df['cause_id'] == 493
        vr = df['data_type_id'] == 9
        df.loc[cvd_ihd & one_to_14 & vr, 'cause_id'] = 507

        cancer_recodes = get_all_related_causes([411, 414, 423, 426, 429, 432,
                                                 435, 438, 441, 444, 450, 453,
                                                 456, 459, 462, 465, 468, 474,
                                                 486, 483], cause_metadata_df)
        cancer_recodes = df['cause_id'].isin(cancer_recodes)
        cancer_ages = df.age_group_years_end <= 15
        df.loc[cancer_recodes & cancer_ages, "cause_id"] = 489

        not_icd10 = self.code_system_id != 1
        neo_meso = df['cause_id'] == 483
        df.loc[neo_meso & not_icd10, "cause_id"] = 489

        if self.source.endswith("AAMSP"):
            digest_hernia = df['cause_id'].isin([531])
            df.loc[digest_hernia, "cause_id"] = 919

        if self.source == "Iran_special_ICD10":
            homicide_and_suicide = df['cause_id'].isin([724, 725, 726, 727, 941,
                                                        718, 719, 720, 721, 722, 723])
            bad_years = df['year_id'].isin(list(range(2007, 2015)))
            # _unintent
            df.loc[bad_years & homicide_and_suicide, "cause_id"] = 919

        inj_war = get_all_related_causes(945, cause_metadata_df)
        is_inj_war = df['cause_id'].isin(inj_war)
        jamaica = df['location_id'] == 115
        year_2005 = df['year_id'] == 2005
        vr = df['data_type_id'] == 9
        df.loc[is_inj_war & jamaica & year_2005 & vr, 'cause_id'] = 724

        inj_mech_gun = df['cause_id'] == 705
        year_2006 = df['year_id'] == 2006
        df.loc[inj_mech_gun & year_2006 & jamaica & vr, 'cause_id'] = 724

        if self.source == "ICD10":
            digest_ibd = get_all_related_causes('digest_ibd', cause_metadata_df)
            is_ibd = df['cause_id'].isin(digest_ibd)
            suriname = df['location_id'] == 118
            year_1995_2012 = df['year_id'].isin(list(range(1995, 2013, 1)))
            df.loc[is_ibd & suriname & year_1995_2012, 'cause_id'] = 526

        hiv = get_all_related_causes(298, cause_metadata_df)
        hiv = df['cause_id'].isin(hiv)
        pre_1980 = df['year_id'] < 1980
        df.loc[hiv & pre_1980, 'cause_id'] = 919

        diabetes_causes = get_all_related_causes(587, cause_metadata_df)
        diabetes = df['cause_id'].isin(diabetes_causes)
        df.loc[neonate & diabetes, 'cause_id'] = 380

        under_20 = df['age_group_years_end'] <= 20
        stroke = get_all_related_causes(
            'cvd_stroke', cause_metadata_df
        )
        stroke_deaths = df['cause_id'].isin(stroke)
        va = df['data_type_id'] == 8
        df.loc[under_20 & stroke_deaths & va, 'cause_id'] = 491
        if self.source == "Russia_FMD_1999_2011":
            cvd_pvd = df['cause_id'] == 502
            df.loc[cvd_pvd, 'cause_id'] = 491

        if self.source == "Iran_special_ICD10":
            sui_homi_causes = [717, 718, 719, 720, 721, 722, 723,
                               724, 725, 726, 727, 941]
            sui_homi = df['cause_id'].isin(sui_homi_causes)
            bad_years = df['year_id'].isin(list(range(2007, 2015)))
            df.loc[sui_homi & bad_years, 'cause_id'] = 919

        if "India_MCCD" in self.source:
            non_neonates = ~(df.age_group_days_end <= 27)
            neonatal_sepsis = df['cause_id'].isin([])
            df.loc[non_neonates & neonatal_sepsis, 'cause_id'] = 380

        if self.source == "India_SCD_states_rural":
            warnings.warn("Implement SCD rd artifact recode")


        inj_war_execution = df['cause_id'] == 854

        if self.source == "ICD9_BTL":
            ecuador = df['location_id'] == 122
            year_1980_1990 = df['year_id'].isin(list(range(1980, 1991, 1)))
            df.loc[inj_war_execution & ecuador & year_1980_1990,
                   'cause_id'] = 855

            bih = df['location_id'] == 44
            year_1985_1991 = df['year_id'].isin([1985, 1986, 1987, 1988,
                                                 1989, 1990, 1991])
            df.loc[inj_war_execution & bih &
                   year_1985_1991, 'cause_id'] = 855
            warnings.warn("BTL cancer recode needed")

        if self.source == "ICD10":
            irq = df['location_id'] == 143
            year_2008 = df['year_id'] == 2008
            df.loc[inj_war_execution & year_2008 & irq, 'cause_id'] = 855

        if self.source == "India_SRS_states_report":
            cirrhosis_ids = [521, 522, 523, 524, 971, 525]
            hepatitis_id = 400

            under_15 = df['age_group_years_end'] <= 15
            cirrhosis = df['cause_id'].isin(cirrhosis_ids)
            df.loc[under_15 & cirrhosis, 'cause_id'] = hepatitis_id

            start_deaths = df[self.val_cols].sum(axis=0)
            # Create proportions to split
            split_df = pd.DataFrame()
            ages_to_split = df.query(
                "age_group_years_start >= 15 & age_group_years_end <= 25")\
                .age_group_id.unique().tolist()
            for age_group_id in ages_to_split:
                for cirrhosis_id in cirrhosis_ids:
                    small_df = pd.DataFrame({
                        'new_cause_id': [cirrhosis_id, hepatitis_id],
                        'pct': [0.70, 0.30]
                    })
                    small_df['cause_id'] = cirrhosis_id
                    small_df['age_group_id'] = age_group_id
                    split_df = split_df.append(small_df, sort=True)
            # Merge in the proportions and split
            df = df.merge(split_df, how='left', on=['age_group_id', 'cause_id'])
            matches = df.new_cause_id.notnull()
            df.loc[matches, 'cause_id'] = df['new_cause_id']
            df.loc[matches, 'deaths'] = df['deaths'] * df['pct']
            for col in ['deaths_raw', 'deaths_corr', 'deaths_rd']:
                df.loc[matches & (df['new_cause_id'] == hepatitis_id), col] = 0
            df.drop(["new_cause_id", "pct"], axis='columns', inplace=True)
            assert np.allclose(start_deaths, df[self.val_cols].sum(axis=0))
            assert df.notnull().values.all()

        malawi_va_study = df['nid'] == 413649
        congenital = df.cause_id.isin(get_all_related_causes('cong', cause_metadata_df))
        df.loc[malawi_va_study & congenital, 'cause_id'] = 919

        keep_cancer_ids = [411, 426, 429, 441, 444]
        all_cancer_ids = get_all_related_causes('_neo', cause_metadata_df)
        keep_cancer = df.cause_id.isin(keep_cancer_ids)
        all_cancer = df.cause_id.isin(all_cancer_ids)
        va = df.data_type_id == 8
        breast_cancer = df.cause_id == 429
        males = df.sex_id == 1

        df.loc[va & all_cancer & ~keep_cancer, 'cause_id'] = 919
        df.loc[va & breast_cancer & males, 'cause_id'] = 919

        if self.source == "ICD9_detail":
            if ((df['location_id'] == 43) & (df['year_id'] == 1997)).any():
                warnings.warn("Albania homicide recode needed")

        if self.source == "ICD9_USSR_Tabulated":
            warnings.warn("Missing some homicide fixes for TJK, ARM here.")

        df = self.drop_leukemia_subtypes(df)

        # mortuary, burial, self-reported COD, census/survey,
        # and tabulated hospital data should be reduced down to just
        # injuries, maternal, and cc_code
        if self.data_type_id in [1, 3, 5, 7]:
            maternal_causes = get_all_related_causes('maternal', cause_metadata_df)
            injury_causes = get_all_related_causes('_inj', cause_metadata_df)
            maternal = df['cause_id'].isin(maternal_causes)
            inj = df['cause_id'].isin(injury_causes)
            bolivia_dhs = df['nid'] == 18979
            neonatal_parent = df['cause_id'] == 380

            df.loc[~(maternal | inj | neonatal_parent) & bolivia_dhs, 'cause_id'] = 919

            df.loc[~(maternal | inj) & ~bolivia_dhs, 'cause_id'] = 919

            if self.data_type_id == 5:
                df.loc[~maternal, 'cause_id'] = 919

        non_icd9_or_10 = self.code_system_id not in {1, 6}
        male = df['sex_id'] == 1
        neo_breast = df['cause_id'].isin(
            get_all_related_causes("neo_breast", cause_metadata_df)
        )
        df.loc[non_icd9_or_10 & male & neo_breast, "cause_id"] = 919

        bulimia = df['cause_id'].isin(
            get_all_related_causes("mental_eating_bulimia", cause_metadata_df)
        )
        df.loc[bulimia, 'cause_id'] = 573

        dominican_republic = df['location_id'] == 111
        vr = df['data_type_id'] == 9
        meningitis = df['cause_id'].isin(
            get_all_related_causes("meningitis", cause_metadata_df)
        )
        infectious = df['cause_id'].isin(
            get_all_related_causes('infectious', cause_metadata_df)
        )
        df.loc[dominican_republic & df.eval('year_id == 1990') & vr & (meningitis | infectious), "cause_id"] = 919

        if self.source == "ICD9_USSR_Tabulation":
            armenia = df['location_id'] == 33
            injury_causes = df['cause_id'].isin(
                get_all_related_causes('_inj', cause_metadata_df)
            )
            df.loc[armenia & df.eval('year_id == 1988') & injury_causes, 'cause_id'] = 919

        under_10 = (df.age_group_years_start < 10)
        age_10_to_14 = (df.age_group_years_start >= 10) & (df.age_group_years_start < 15)
        all_liver_cancers = get_all_related_causes('neo_liver', cause_metadata_df)
        alcohol_nash_hbl = cause_metadata_df.loc[
            cause_metadata_df.acause.isin(['neo_liver_alcohol', 'neo_liver_nash','neo_liver_hbl'])
        ].cause_id.unique().tolist()
        df.loc[(under_10) & (df.cause_id.isin(all_liver_cancers)), 'cause_id'] = 1005
        df.loc[(age_10_to_14) & (df.cause_id.isin(alcohol_nash_hbl)), 'cause_id'] = 421

        age_10_to_95 = (df.age_group_years_start >= 10)
        df.loc[(age_10_to_95) & (df.cause_id == 1009), 'cause_id'] = 1010 # retinoblastoma -> neo_eye_other
        df.loc[(~age_10_to_95) & (df.cause_id == 1010), 'cause_id'] = 1009 # neo_eye_other -> retinoblastoma

        pn_to_15 = (df.age_group_days_start > 27) & (df.age_group_years_start < 15)
        df.loc[(pn_to_15) & (df.cause_id == 509), 'cause_id'] = 520

        ntd_chagas = df["cause_id"].isin(
            get_all_related_causes("ntd_chagas", cause_metadata_df)
        )
        loc_tree = tree.parent_child_to_tree(
            self.loc_meta_df, parent_col="parent_id", child_col="location_id",
        )
        # Get all Latin America and Caribbean regions
        regions = list(self.loc_meta_df.loc[(self.loc_meta_df['level'] == 2) &
            ((self.loc_meta_df['location_name'].str.contains('Latin')) | (self.loc_meta_df['location_name'] == 'Caribbean')),
            'location_id'].unique())
        loc_ids = []
        for region in regions:
            ids = [n.id for n in tree_utils.search(loc_tree.root, id=region)[0].all_descendants()]
            loc_ids += ids
        latin_america_or_caribbean = df["location_id"].isin(loc_ids)
        df.loc[ntd_chagas & ~latin_america_or_caribbean, "cause_id"] = 919

        corona = df['cause_id'].isin(
            get_all_related_causes('lri_corona', cause_metadata_df)
        )
        df.loc[(df.year_id < 2020) & (corona), 'cause_id'] = 919

        df = df.drop(age_add_cols, axis='columns')

        return df

class FinalRecoder(CodProcess):
    """
    For data that get squared during noisereduction, it is possible
    that some observations we recoded out in the above recoder can be
    reintroduced into the data. This happens to be the case when the recode
    is age or sex specific. 

    These recodes should be re applied in final clean. Different from above, these
    recodes will not recode to another cause, but will instead drop these observations
    from the data.
    """
    id_cols = ['nid', 'extract_type_id', 'location_id', 'year_id',
               'age_group_id', 'sex_id', 'cause_id',
               'site_id']
    val_cols = ['cf_final', 'cf_raw', 'cf_corr', 'cf_rd', 'cf_agg']

    def __init__(self, cause_meta_df, loc_meta_df, source, code_system_id, data_type_id):
        self.source = source
        self.code_system_id = code_system_id
        self.data_type_id = data_type_id
        self.cause_meta_df = cause_meta_df
        self.loc_meta_df = loc_meta_df
        self.conf = Configurator("standard")
        self.vr_indicators_path = self.conf.get_resource('vr_indicators')
        self.cache_options = {
            'force_rerun': False,
            'block_rerun': True,
            'cache_results': False,
            'cache_dir': self.conf.get_directory('db_cache')
        }

    def get_computed_dataframe(self, df):
        start_cols = df.columns.tolist()
        start_df = df.copy()
        if 'data_type_id' not in df.columns:
            df['data_type_id'] = self.data_type_id
        df = self.recode(df)
        df = df[start_cols]
        self.validate_drops(start_df, df)
        return df

    def validate_drops(self, start_df, end_df):
        """
        Print message for how many rows dropped in final recode. Also, validate
        that any dropped data was a product of noise reduction (has cf_agg = 0) 
        """
        start_df = start_df[self.id_cols + ['cf_agg']]
        end_df = end_df[self.id_cols]
        dropped_rows = len(start_df) - len(end_df)
        print_log_message("Dropped " + str(dropped_rows) + " rows of data during Final Recode")
        dropped_data = start_df.merge(end_df, on=self.id_cols, how='left', indicator=True)
        dropped_data = dropped_data.loc[dropped_data.cause_id != 1048]
        assert (dropped_data.loc[dropped_data._merge == 'left_only'].cf_agg == 0).all(), \
            "FinalRecoder dropped data that was non-zero before noisereduction"

    def recode(self, df):
        cause_metadata_df = self.cause_meta_df
        cause_metadata_df = cause_metadata_df[["cause_id",
                                               "path_to_top_parent",
                                               "acause"]]
        age_add_cols = [
            f'age_group_{unit}_{side}'
            for unit in ['days', 'years']
            for side in ['start', 'end']]
        df = add_age_metadata(df, age_add_cols, **self.cache_options)
        for col in age_add_cols:
            report_if_merge_fail(df, col, 'age_group_id')
        ckd_cause_ids = get_all_related_causes('ckd', cause_metadata_df)
        ckd_cause_ids.remove(593)
        ckd_cause_ids.remove(589)
        ckd_less_other = df['cause_id'].isin(ckd_cause_ids)
        neonate = df.age_group_days_end <= 27
        vr = df['data_type_id'] == 9
        df = df.loc[~(ckd_less_other & neonate & vr)]

        resp_ids = [509, 515, 516, 520]
        is_cert_resp_causes = df['cause_id'].isin(resp_ids)
        neonate = df.age_group_days_end <= 27
        df = df.loc[~(is_cert_resp_causes & neonate)]

        if self.source == "Iran_special_ICD10":
            homicide_and_suicide = df['cause_id'].isin([724, 725, 726, 727, 941,
                                                        718, 719, 720, 721, 722, 723])
            bad_years = df['year_id'].isin(list(range(2007, 2015)))
            # _unintent
            df = df.loc[~(bad_years & homicide_and_suicide)]

        under_20 = df['age_group_years_end'] <= 20
        stroke = get_all_related_causes(
            'cvd_stroke', cause_metadata_df
        )
        stroke_deaths = df['cause_id'].isin(stroke)
        va = df['data_type_id'] == 8
        df = df.loc[~(under_20 & stroke_deaths & va)]

        if self.source == "India_SRS_states_report":
            cirrhosis_ids = [521, 522, 523, 524, 971, 525]
            under_15 = df['age_group_years_end'] <= 15
            cirrhosis = df['cause_id'].isin(cirrhosis_ids)
            df = df.loc[~(under_15 & cirrhosis)]

        # Recode non-ICD10 and non-ICD9_detail male breast cancer
        non_icd9_or_10 = self.code_system_id not in {1, 6}
        male = df['sex_id'] == 1
        neo_breast = df['cause_id'].isin(
            get_all_related_causes("neo_breast", cause_metadata_df)
        )
        df = df.loc[~(non_icd9_or_10 & male & neo_breast)]

        under_10 = (df.age_group_years_start < 10)
        age_10_to_14 = (df.age_group_years_start >= 10) & (df.age_group_years_start < 15)
        all_liver_cancers = get_all_related_causes('neo_liver', cause_metadata_df)
        all_liver_cancers.remove(1005)
        all_liver_cancers.remove(417)
        alcohol_nash_hbl = cause_metadata_df.loc[
            cause_metadata_df.acause.isin(['neo_liver_alcohol', 'neo_liver_nash','neo_liver_hbl'])
        ].cause_id.unique().tolist()
        df = df.loc[~((under_10) & (df.cause_id.isin(all_liver_cancers)))]
        df = df.loc[~((age_10_to_14) & (df.cause_id.isin(alcohol_nash_hbl)))]

        corona = df['cause_id'].isin(
            get_all_related_causes('lri_corona', cause_metadata_df)
        )
        df = df.loc[~((df.year_id < 2020) & (corona))]

        return df
