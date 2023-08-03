"""
Description: this is the main script for data cleaning.
      parse_cod_mortality is for the most part straightforward -- it loads data, recodes variables
      to be consistent across years, and saves cleaned files.
"""

from __future__ import division
from datetime import datetime
import pandas as pd
import numpy as np
import os
import sys
import data_dicts
import recode
import missing
from filepaths import get_filepaths

def run_codeath(test=False):
    """Run data cleaning, using args passed from the shell script.

    Passed arguments:
    year (int): year being cleaned.
    us_indir: filepath of the us county level file.
    ps_indir: if present, filepath of the Puerto Rico + territories file
    parent_dir: root of filesystem to which you want to save results
        (see parallel_codeath.py for current parent dir)
    map_dir: filepath to dataset that, on the state level, maps nonfips code,
        FIPS code, postal code, and state name.
    """
    year = int(sys.argv[1])

    # 1994 has separate territories files, so indexing sys.argv is a little different
    if year == 1994:
        us_indir = sys.argv[2]
        ps_indir = [sys.argv[3][1:len(sys.argv[3]) - 1], sys.argv[4][:-1], sys.argv[5][:-1]]
        parent_dir, map_dir = sys.argv[6:8]
        archive_date = sys.argv[8]
    else:
        us_indir, ps_indir, parent_dir, map_dir = sys.argv[2:6]
        archive_date = sys.argv[6]

    # number of rows of file to read; make it small if you're just testing
    if test:
        nrows = 500
        # note this in name when saving
        nrow_str = '_TEST_{}_ROWS'.format(nrows)
    else:
        nrows = None
        nrow_str = ''

    # run code
    cod_data, cod_data_raw, cod_missingness = parse_cod_mortality(
        year, parent_dir, us_indir, ps_indir, map_dir, nrows
    )

    # save files:
    
    # 1. Save parsed-but-not-cleaned files:
    cod_data_raw.to_csv('FILEPATH', index=False)

    # 2. Save missingness
    cod_missingness.to_csv('FILEPATH', index=False)

    # 3. Save cleaned data

    # add NIDs here, which depend on year or year/territory (where applicable), for database tracking
    nids_us = pd.read_csv('FILEPATH')
    nids_terr = pd.read_csv('FILEPATH')
    cod_data.index.name = None  # reset index to avoid an ambiguity error
    cod_data.deaths.sum()
    cod_data = pd.merge(cod_data, nids_us, on='year', how='left')
    cod_data.index.name = None  # reset index to avoid an ambiguity error
    cod_data['state_res_numeric'] = cod_data['state_res_numeric'].fillna(0)  # Canadian provinces have NA state fips, so set to 0 for turning to int
    cod_data['state_res_numeric'] = cod_data['state_res_numeric'].astype(int)  # make sure numeric state code column is an int for merging with nids
    cod_data_territories = cod_data.loc[cod_data['state_res_numeric'] > 56]  # single out territories
    cod_data = cod_data.loc[cod_data['state_res_numeric'] <= 56]  # drop territories from main dataset before re-appending later
    cod_data_territories = cod_data_territories.drop(['nid'], axis=1)  # remove non-territory nid
    cod_data_territories = pd.merge(cod_data_territories, nids_terr[['year', 'state_res_numeric', 'nid']],
                                    on=['year', 'state_res_numeric'], how='left')  # join territory-specific nids
    cod_data = cod_data.append(cod_data_territories)  # re-combine territories and rest of US
    cod_data.index.name = None  # reset index to avoid accidentally dropping year column

    # save output
    cod_data.to_csv('FILEPATH', index=False)


def parse_cod_mortality(year, parent_dir, us_indir, ps_indir, map_dir, nrows=None):
    """Parse and clean NVSS data, ensuring consistency across years.

    Passed arguments:
    year (int): year being cleaned.
    us_indir: filepath of the us county level file.
    ps_indir: if present, filepath of the Puerto Rico + territories file
    map_dir: filepath to dataset that, on the state level, maps nonfips code,
        FIPS code, postal code, and state name.
    """
    # extract variables of interest using the data dictionary for that year
    cod_data_dict = data_dicts.give_data_dict(year)

    dictlen = len(cod_data_dict)
    convert = {}
    for idx in range(dictlen):
        convert[idx] = str

    cod_data_raw = pd.io.parsers.read_fwf(
        us_indir, colspecs=cod_data_dict.values(),
        header=None, nrows=nrows, converters=convert
    )
    cod_data_raw.columns = cod_data_dict.keys()

    # add deaths in territories if they exist
    if ps_indir != 'NONE':
        # 1994 has separate files for each territory
        if year == 1994:
            cod_data_ps_dfs = []
            for territory in ps_indir:
                cod_data_ps = pd.io.parsers.read_fwf(
                    territory, colspecs=cod_data_dict.values(),
                    header=None, nrows=nrows, converters=convert
                )
                cod_data_ps.columns = cod_data_dict.keys()
                cod_data_ps_dfs.append(cod_data_ps)
            cod_data_ps = pd.concat(cod_data_ps_dfs, ignore_index=True)
            cod_data_raw = cod_data_raw.append(cod_data_ps)
        else:
            cod_data_ps = pd.io.parsers.read_fwf(
                ps_indir, colspecs=cod_data_dict.values(),
                header=None, nrows=nrows, converters=convert
            )
            cod_data_ps.columns = cod_data_dict.keys()
            cod_data_raw = cod_data_raw.append(cod_data_ps)
    index = cod_data_raw.index

    # create a new df that will contain only data
    # that is consistent over years and coding.
    # it will get filled over the course of this function
    if year < 1980:
        new_columns = ['year', 'state_res_numeric', 'state_res_alpha', 'cause', 'icd_version']
    else:
        new_columns = [
            'year', 'state_res_numeric', 'state_res_alpha', 'county_res', 'full_fips_res_numeric', 'sex', 'age',
            'race', 'race_recode_40', 'race_bridged', 'race_imputation', 'hisp_desc', 'hisp_recode', 'race_hisp_recode',
            'race_label_1977', 'race_label_1997', 'education', 'education_orig', 'edu_flag', 'cause', 'icd_version',
            'industry', 'occupation'
        ]

    cod_data = pd.DataFrame(index=index, columns=new_columns)

    # set year (note: this isn't in our data dict because
    # it's coded inconsistently across years)
    cod_data_raw['year'] = year
    cod_data['year'] = year

    # icd version
    if year in range(1968, 1979):
        cod_data.icd_version = "ICD8a"
    elif year in range(1979, 1999):
        cod_data.icd_version = "ICD9"
    else:
        cod_data.icd_version = "ICD10"

    state_map = pd.read_csv(map_dir.strip(), dtype=str)

    # Years after 2003 only have the alphabetic state label, years before only have the numeric one.
    # we map each of these to the other for a full dataset of both.
    for county_type in ['res', 'occ']:  # repeat this process for state/county of residence and occurrence

        print 'recoding %s' % county_type

        if year < 1982:
            # merge onto original data
            cod_data_raw['pre_fips_county_%s' % county_type] = cod_data_raw['state_%s_NONFIPS' % county_type] + \
                cod_data_raw['county_%s_NONFIPS' % county_type]
            cod_data_raw['fips_%s' % county_type] = cod_data_raw['pre_fips_county_%s' %
                                                                 county_type].map(fips_dict)

            # NA values will be foreign residents; fill these spots with the appropriate
            # '00' code for state and '000' for county
            cod_data_raw['fips_%s' % county_type].fillna('00000', inplace=True)
            cod_data['county_%s' % county_type] = cod_data_raw['fips_%s' %
                                                               county_type].apply(lambda x: x[2:5])

            # map the non-FIPS state codes to fips, both alpha and numeric
            fips_map = dict(zip(state_map['nonfips'], state_map['fips']))
            alpha_map = dict(zip(state_map['nonfips'], state_map['alpha']))

            cod_data['state_%s_numeric' % county_type] = cod_data_raw['state_%s_NONFIPS' %
                                                                      county_type].map(fips_map)
            cod_data['state_%s_alpha' % county_type] = cod_data_raw['state_%s_NONFIPS' %
                                                                    county_type].map(alpha_map)

        elif year >= 2003:
            fips_map = dict(zip(state_map['alpha'], state_map['fips']))
            cod_data['state_%s_alpha' % county_type] = cod_data_raw['state_%s_alpha' % county_type]
            cod_data['state_%s_numeric' % county_type] = cod_data['state_%s_alpha' %
                                                                  county_type].map(fips_map)

            # This dataset includes US territories. American Samoa and the Northern Marianas have counties coded to
            # '000' for everybody (residents and otherwise). Here, we recode deaths to territory residents that occur in
            # that territory to the special code '998', so we can identify the true '0's later in the cleaning process.
            print "recoding marianas and samoa deaths"
            cod_data_raw['county_%s' % county_type] = np.vectorize(recode.recode_mp_as)(cod_data_raw['state_occ_alpha'],
                                                                                        cod_data_raw['state_res_alpha'],
                                                                                        cod_data_raw['county_%s' % county_type])

            # There are 32 deaths with null values for 'county_occ' and 'county_res' in 2004. Set these to 999.
            if year == 2004:
                cod_data_raw['county_%s' % county_type] = cod_data_raw['county_%s' %
                                                                       county_type].replace('nan', '999')
            cod_data['county_%s' % county_type] = cod_data_raw['county_%s' % county_type]

        else:
            alpha_map = dict(zip(state_map['fips'], state_map['alpha']))
            cod_data['state_%s_numeric' %
                     county_type] = cod_data_raw['state_%s_numeric' % county_type]
            cod_data['state_%s_alpha' % county_type] = cod_data['state_%s_numeric' %
                                                                county_type].map(alpha_map)
            cod_data['county_%s' % county_type] = cod_data_raw['county_%s' % county_type]

        cod_data['full_fips_%s_numeric' % county_type] = cod_data['state_%s_numeric' % county_type] + \
            cod_data['county_%s' % county_type]

    # AGE: recode age to be consistent across groups
    cod_data['age'] = np.vectorize(recode.recode_age)(cod_data['year'], cod_data_raw['age'])

    # SEX: after 2003, sex gets coded as M/F rather than 1/2. make this consistent.
    cod_data['sex'] = cod_data_raw['sex'].apply(recode.recode_sex)

    # HISPANIC ORIGIN: supersedes race if in data
    if year < 1984:  # placeholder empty hisp_desc and hisp_recode columns in years where not available
        cod_data_raw['hisp_desc'] = None
        cod_data['hisp_recode'] = 9
    elif year in range(1984, 1989):  # use hisp_desc to create hisp_recode
        cod_data['hisp_recode'] = np.vectorize(recode.recode_hisp_desc)(cod_data_raw['hisp_desc'])
    else:
        cod_data['hisp_recode'] = cod_data_raw['hisp_recode']

    cod_data['hisp_desc'] = cod_data_raw['hisp_desc']

    # RACE: add race variables and more generalized race group based on detailed race information in raw data
    if year < 1992:  # placeholder empty race_imputation column
        cod_data_raw['race_imputation'] = None
    if year < 2003:  # placeholder empty race_bridged column
        cod_data_raw['race_bridged'] = None
    if year < 2011:  # placeholder empty race_recode_40 column
        cod_data_raw['race_recode_40'] = None

    cod_data['race'] = cod_data_raw['race']
    cod_data['race_bridged'] = cod_data_raw['race_bridged']
    cod_data['race_imputation'] = cod_data_raw['race_imputation']  # not used to get race group but leave in anyway
    cod_data['race_recode_40'] = cod_data_raw['race_recode_40']
    cod_data['race_label_1997'] = np.vectorize(recode.add_1997_race_label)(year, cod_data['race'],
                                                                           cod_data['race_bridged'],
                                                                           cod_data['race_recode_40'],
                                                                           cod_data['hisp_recode'])
    cod_data['race_hisp_recode'] = np.vectorize(recode.recode_race_hisp)(cod_data['race_label_1997'])

    cod_data['race_label_1977'] = np.vectorize(recode.add_1977_race_label)(cod_data['state_res_alpha'],
                                                                           cod_data['race'],
                                                                           cod_data['race_label_1997'])

    # INDUSTRY AND OCCUPATION: we don't have enough years to (necessarily) make this worthwhile, but we can keep it
    # for now. (see recode.py for details on this) Short version: we only have data from 1985-1999, only the 1992-1999
    # data needs to be recoded. All other years, fill in the 'unknown' values
    if year in range(1985, 1992):
        cod_data['industry'] = cod_data_raw['industry_recode']
        cod_data['occupation'] = cod_data_raw['occupation_recode']
    elif year in range(1992, 2000):
        cod_data['industry'] = cod_data_raw['industry'].apply(recode.recode_industry)
        cod_data['occupation'] = cod_data_raw['occupation'].apply(recode.recode_occupation)
    else:
        cod_data['industry'] = '51'
        cod_data['occupation'] = '59'

    # CAUSE: causes are numeric or alphanumeric codes of length three (i.e. 243 for ICD9 or A14 for ICD10) or 4 with
    # a decimal (i.e. 243.8 or A14.3).  However, these codes are recorded in the data without the decimal point.
    # this means that if we don't change anything, and read them in as numbers later, the codes 042.0 and 420 will
    # look identical.  To clarify this, we add the decimal point and standardize everything to length 4, and make sure
    # to read things in as strings in the future.
    # We recode entity codes (the other codes on the certificate) the same way
    # range determined by number of entity codes in dictionary (fewer in earlier years)

    for entity_idx in range(0, 1 + len([value for key, value in cod_data_dict.items() if key.startswith("entity")])):
        print 'recoding cause number %s' % entity_idx

        if entity_idx == 0:
            new_colname = 'cause'
            old_colname = 'cause'
        else:
            new_colname = 'multiple_cause_%s' % entity_idx
            old_colname = 'entity_%s' % entity_idx

            # the code has a bunch of extraneous information; just keep the cause code
            cod_data_raw[new_colname] = cod_data_raw[old_colname].fillna('0000000')
            cod_data_raw[new_colname] = cod_data_raw[new_colname].apply(lambda x: x[2:6].strip())

        # add a decimal point if it's 4 elements long, otherwise keep it 3-digit
        cod_data[new_colname] = cod_data_raw[new_colname].apply(
            lambda x: x if len(x) == 3 else x[0:3] + '.' + x[3])

    # MISSINGNESS
    # count missingness/unknowns, get percentages
    cod_missingness = missing.calc_missingness(cod_data)
    cod_data.set_index('year', drop=False, inplace=True)

    # add deaths column because will need to include fractional deaths in later adjustments
    cod_data['deaths'] = 1

    return cod_data, cod_data_raw, cod_missingness


# SCRIPT STARTS ACTUALLY RUNNING FUNCTIONS HERE
pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

run_codeath(test=False)