"""
Description: this is the main script for data cleaning. It consists of three functions:
              1. run_local: run data cleaning code on a local computer
              2. run_on_cluster: run the data cleaning code within the current cluster session
              3. parse_linked_births: main function to clean data

              parse_linked_births is for the most part straightforward -- it loads data, recodes variables
              to be consistent across years, and saves cleaned files. The exceptions are 1980-81 and 1988-91.
              In these cases, inappropriate borough coding in Alaska and New York (1980-81), delayed recognition
              of Poquoson City, VA as an independent city (1980-81), and HIV censoring in Georgia (1988-91)
              forces us to make additional adjustments both here and in the 02_clean_pre_pipeline folder.
              These files are initially saved in an intermediate directory. At the end of the script, we
              run a function that tabulates and reports missingness for each variable in the dataset.

              Note that the script "begins" at the bottom, where functions actually get called.

input: raw NVSS text files by year
output: 1) parsed files - the raw data with county and territory files combined into one csv file per year
        2) missingness info - one csv per year with details on % missing of each variable
        3) clean files - data by year after variable values are recoded to be consistent across years and
           partially adjusted in the cases of Alaska, New York, Virginia, and Georgia. Additional adjustments
           are made to deaths in these states in the 02_clean_pre_pipeline stage.
"""

from __future__ import division
import pandas as pd
import numpy as np
import os
import sys
import data_dicts_linked_births
import recode
import missing
from filepaths import get_linked_birth_filepaths
from datetime import datetime

def run_on_cluster(test=False):
    """Run data cleaning on the cluster, using args passed from the shell script.

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
    us_indir, ps_indir, parent_dir, map_dir = sys.argv[2:7]

    # number of rows of file to read; make it small if you're just testing
    if test:
        nrows = 500
        # note this in name when saving
        nrow_str = 'FILEPATH'
    else:
        nrows = None
        nrow_str = ''

    # run code
    lb_data_raw, lb_data, lb_missingness = parse_linked_births(year, parent_dir, us_indir, ps_indir, map_dir, nrows)

    # save files:
    # 0. make archive folders if they don't already exist
    archive_date = datetime.today().strftime('%Y_%m_%d')
    for sub_dir in ['FILEPATH', 'FILEPATH', 'FILEPATH']:
        if not os.path.exists('FILEPATH'):
            os.makedirs('FILEPATH')
    
    # 1. Save parsed-but-not-cleaned files:
    lb_data_raw.to_csv('FILEPATH', index=False)  # archived version
    lb_data_raw.to_csv('FILEPATH'.format(parent_dir, year), index=False)

    # 2. Save missingness
    lb_missingness.to_csv('FILEPATH')

    # add NIDs here, which depend on year, for database tracking
    nids = pd.read_csv(parent_dir + FILEPATH)
    lb_data.index.name = None  # reset index to avoid an ambiguity error
    lb_data = pd.merge(lb_data, nids, on='year', how='left')
    lb_data.index.name = None  # reset index to avoid an ambiguity error
    lb_data['state_res_numeric'] = lb_data['state_res_numeric'].fillna(0)  # Canadian provinces have NA state fips, so set to 0 for turning to int
    lb_data['state_res_numeric'] = lb_data['state_res_numeric'].astype(int)  # make sure numeric state code column is an int for merging with nids
    lb_data.index.name = None  # reset index to avoid accidentally dropping year column

    # 3. Save cleaned data.
    lb_data.to_csv(.format(parent_dir, archive_date, year, nrow_str), index=False)  # archived version
    lb_data.to_csv(.format(parent_dir, year, nrow_str), index=False)

def parse_linked_births(year, parent_dir, us_indir, ps_indir, map_dir, nrows=None):
    """Parse and clean NVSS data, ensuring consistency across years.

    Passed arguments:
    year (int): years being cleaned.
    us_indir: filepath of the us county level file.
    ps_indir: if present, filepath of the Puerto Rico + territories file
    map_dir: filepath to dataset that, on the state level, maps nonfips code,
        FIPS code, postal code, and state name.
    """
    # extract variables of interest using the data dictionary for that year
    lb_data_dict = data_dicts_linked_births.give_data_dict(year)

    dictlen = len(lb_data_dict)
    convert = {}
    for idx in range(dictlen):
        convert[idx] = str

    lb_data_raw = pd.io.parsers.read_fwf(
        us_indir, colspecs=lb_data_dict.values(),
        header=None, nrows=nrows, converters=convert
    )
    lb_data_raw.columns = lb_data_dict.keys()

    # add deaths in territories
    # skip 2003, territory file is blank
    # skip 2018, don't have territories right now
    if year not in [2003, 2018]:
        lb_data_ps = pd.io.parsers.read_fwf(
            ps_indir, colspecs=lb_data_dict.values(),
            header=None, nrows=nrows, converters=convert
        )
        lb_data_ps.columns = lb_data_dict.keys()
        lb_data_raw = lb_data_raw.append(lb_data_ps)
    index = lb_data_raw.index

    # create a new df that will contain only data
    # that is consistent over years and coding.
    # it will get filled over the course of this function
    new_columns = [
        'year', 'state_res_numeric', 'state_res_birth', 'state_res_alpha', 'county_res',
        'county_res_birth', 'full_fips_res_numeric', 'state_occ_birth', 'sex', 'age', 'mother_age',
        'mother_education', 'mother_education_orig', 'mother_edu_flag',
        'cause', 'icd_version', 'deaths'
    ]

    lb_data = pd.DataFrame(index=index, columns=new_columns)
    lb_data['year'] = year  # assign data year

    # set icd version
    if year in range(1995, 1999):
        lb_data.icd_version = "ICD9"
    else:
        lb_data.icd_version = "ICD10"

    state_map = pd.read_csv(map_dir.strip(), dtype=str)

    # STATE AND COUNTY LABELS
    # Ensure that states have both a FIPS code and an alphabetic postal code.

    # Years after 2003 only have the alphabetic state label, years before only have the numeric one.
    # we map each of these to the other for a full dataset of both.
    for county_type in ['res', 'occ']:  # repeat this process for state/county of residence and occurrence

        print 'recoding %s' % county_type

        if year >= 2003:
            fips_map = dict(zip(state_map['alpha'], state_map['fips']))

            # In 2005-06 and 2014-17, state/county of residence at death is missing, so we use at birth instead
            if (year in range(2005, 2007) or year in range(2014, 2018)) and county_type == 'res':
                lb_data_raw['state_%s_alpha' % county_type] = lb_data_raw['state_%s_alpha_birth' % county_type]
                lb_data['state_%s_alpha' % county_type] = lb_data_raw['state_%s_alpha' % county_type]

            lb_data_raw['state_%s_numeric' % county_type] = lb_data_raw['state_%s_alpha' %
                                                                        county_type].map(fips_map)
            lb_data_raw['state_%s_birth' % county_type] = lb_data_raw['state_%s_alpha_birth' %
                                                                      county_type].map(fips_map)
            lb_data['state_%s_alpha_birth' % county_type] = lb_data_raw['state_%s_alpha_birth' % county_type]

            # This dataset includes US territories. American Samoa and the Northern Marianas have counties coded to
            # '000' for everybody (residents and otherwise). Here, we recode deaths to territory residents that occur in
            # that territory to the special code '998', so we can identify the true '0's later in the cleaning process.
            print "recoding marianas and samoa deaths"
            if year in range(2005, 2007) or year in range(2014, 2018):
                lb_data_raw['county_%s' % county_type] = np.vectorize(recode.recode_mp_as)(
                    lb_data_raw['state_occ_alpha'],
                    lb_data_raw['state_res_alpha_birth'],
                    lb_data_raw['county_%s' % county_type] if county_type == 'occ' else lb_data_raw['county_%s_birth' %
                                                                                                    county_type])
            else:
                lb_data_raw['county_%s' % county_type] = np.vectorize(recode.recode_mp_as)(
                    lb_data_raw['state_occ_alpha'],
                    lb_data_raw['state_res_alpha'],
                    lb_data_raw['county_%s' % county_type])

            # Assign final state and county values
            # Set any null state or county values to 99 or 999, respectively (currently affects 2005-06, 2014-17)
            if (year in range(2005, 2007) or year in range(2014, 2018)) and county_type == 'res':
                lb_data_raw['state_%s_birth' % county_type] = lb_data_raw['state_%s_birth' %
                                                                          county_type].replace(np.nan, '99')
                lb_data_raw['county_%s_birth' % county_type] = lb_data_raw['county_%s_birth' %
                                                                           county_type].replace(np.nan, '999')
                lb_data['state_%s_numeric' % county_type] = lb_data_raw['state_%s_birth' % county_type]
                lb_data['county_%s' % county_type] = lb_data_raw['county_%s_birth' % county_type]
                lb_data['state_%s_birth' % county_type] = lb_data_raw['state_%s_birth' % county_type]
                lb_data['county_%s_birth' % county_type] = lb_data_raw["county_%s_birth" % county_type]
            else:
                # preserve state/county of residence at birth and state of birth occurrence for vetting
                if county_type == 'res':
                    lb_data_raw['state_%s_birth' % county_type] = lb_data_raw['state_%s_birth' %
                                                                              county_type].replace(np.nan, '99')
                    lb_data_raw['county_%s_birth' % county_type] = lb_data_raw['county_%s_birth' %
                                                                               county_type].replace(np.nan, '999')
                    lb_data['county_%s_birth' % county_type] = lb_data_raw["county_%s_birth" % county_type]
                lb_data['state_%s_birth' % county_type] = lb_data_raw['state_%s_birth' %
                                                                      county_type].replace(np.nan, '99')

                # continue with assigning state/county of death
                lb_data_raw['state_%s_alpha' % county_type] = lb_data_raw['state_%s_alpha' %
                                                                          county_type].replace(np.nan, '99')
                lb_data_raw['state_%s_numeric' % county_type] = lb_data_raw['state_%s_numeric' %
                                                                            county_type].replace(np.nan, '99')
                lb_data_raw['county_%s' % county_type] = lb_data_raw['county_%s' %
                                                                     county_type].replace(np.nan, '999')
                lb_data['state_%s_alpha' % county_type] = lb_data_raw['state_%s_alpha' % county_type]
                lb_data['state_%s_numeric' % county_type] = lb_data_raw['state_%s_numeric' % county_type]
                lb_data['county_%s' % county_type] = lb_data_raw['county_%s' % county_type]


        # 1995-2002
        else:
            alpha_map = dict(zip(state_map['fips'], state_map['alpha']))
            lb_data['state_%s_numeric' %
                    county_type] = lb_data_raw['state_%s_numeric' % county_type]
            lb_data['state_%s_alpha' % county_type] = lb_data['state_%s_numeric' %
                                                              county_type].map(alpha_map)
            lb_data['state_%s_alpha_birth' % county_type] = lb_data['state_%s_birth' %
                                                                    county_type].map(alpha_map)
            lb_data['county_%s' % county_type] = lb_data_raw['county_%s' % county_type]
            if county_type == 'res':
                lb_data_raw['county_%s_birth' % county_type] = lb_data_raw['county_%s_birth' %
                                                                           county_type].replace(np.nan, '999')
            if county_type == 'occ':
                lb_data['state_%s_birth' % county_type] = lb_data_raw['state_%s_birth' %
                                                                      county_type].replace(np.nan, '99')

        lb_data['full_fips_%s_numeric' % county_type] = lb_data['state_%s_numeric' % county_type] + \
            lb_data['county_%s' % county_type]

    # AGE: recode both infant and mother age to be consistent across groups and align with GBD standards
    if year in range(1995, 2003):
        lb_data['age'] = np.vectorize(recode.recode_infant_age)(year, lb_data_raw['age_days'])
    else:
        lb_data['age'] = np.vectorize(recode.recode_infant_age)(year, lb_data_raw['age_recode_22'])

    lb_data['mother_age'] = np.vectorize(recode.recode_mother_age)(lb_data_raw['mother_age_recode_9'])


    # SEX: after 2003, sex gets coded as M/F rather than 1/2. make this consistent.
    lb_data['sex'] = lb_data_raw['sex'].apply(recode.recode_sex)

    # MOTHER'S EDUCATION:
    # see recode.py for details
    if year in range(1995, 2003):
        # years from 1989 to 2002 are flag 0 UNLESS the state didn't report education, then they're flag 2
        # first, pull list of states and when they started reporting education
        edu_flags_1989_2002 = pd.read_csv(
            .format(parent_dir))
        # next, assign flag based on only state and year. make sure relevant columns are integers before comparisons
        lb_data_raw['year'] = lb_data_raw['year'].astype(int)
        lb_data_raw['state_occ_numeric'] = lb_data_raw['state_occ_numeric'].astype(int)
        lb_data_raw['month_death'] = lb_data_raw['month_death'].astype(int)
        lb_data_raw = pd.merge(lb_data_raw, edu_flags_1989_2002[['state', 'year', 'edu_flag', 'month']],
                               left_on=['year', 'state_occ_numeric'], right_on=['year', 'state'], how='left')
        # finally, take month into account (some states began reporting education partway through a year)
        # 'month' = month of the given year in which the state began reporting education. values are:
        # -1: edu reporting began in a previous year
        # when month = -99, edu was not reported any time in the given year
        # when month = 1-12, edu reporting began in the indicated month of the given year
        lb_data_raw.loc[lb_data_raw['month_death'] < lb_data_raw['month'], 'edu_flag'] = 2
        lb_data_raw.loc[lb_data_raw['state_occ_numeric'] > 56, 'edu_flag'] = 0  # assign flag 0 to all territories for now
        lb_data_raw.drop(['state', 'month'], axis=1)  # no longer needed after edu_flags are assigned
        lb_data_raw['mother_edu_flag'] = lb_data_raw['edu_flag']
    elif year >= 2014:
        # in 2014+, only 2003 revision is used
        lb_data_raw['mother_edu_flag'] = 1
    else:
        # for 2003-2013, two mother's education columns are listed: mother_edu_revised, which is
        # populated if the data are based on the 2003 U.S. birth certificate revision, and mother_edu_unrevised,
        # which is populated if the data are based on the 1989 revision. We use the 2003 revision if applicable
        # and otherwise use the 1989 revision. (Different states adopted the 2003 revision at different times.)
        lb_data_raw['mother_education'] = lb_data_raw['mother_edu_revised'].fillna(lb_data_raw['mother_edu_unrevised'])

        # assign edu flags based on which education columns do/do not have values. If the revised column has a value,
        # flag 1 is used to mark 2003 revision. If the revised column has no value but the unrevised column does,
        # flag 0 is used to mark 1989 revision. If neither condition is met, flag 2 is used to denote unreported.
        lb_data_raw.loc[pd.notnull(lb_data_raw['mother_edu_revised']), 'mother_edu_flag'] = 1
        lb_data_raw.loc[pd.isnull(lb_data_raw['mother_edu_revised']) & pd.notnull(lb_data_raw['mother_edu_unrevised']),
                        'mother_edu_flag'] = 0
        lb_data_raw.loc[pd.isnull(lb_data_raw['mother_edu_revised']) & pd.isnull(lb_data_raw['mother_edu_unrevised']),
                        'mother_edu_flag'] = 2

    # get recoded mother's education values
    lb_data['mother_education'] = np.vectorize(recode.recode_education)(
        lb_data_raw['mother_education'], lb_data_raw['mother_edu_flag'])

    # keep original codes and flags for 1989 and 2003 versions
    lb_data['mother_education_orig'] = lb_data_raw['mother_education']
    lb_data['mother_edu_flag'] = lb_data_raw['mother_edu_flag']

    # CAUSE: causes are numeric or alphanumeric codes of length three (i.e. 243 for ICD9 or A14 for ICD10) or 4 with
    # a decimal (i.e. 243.8 or A14.3).  However, these codes are recorded in the data without the decimal point.
    # this means that if we don't change anything, and read them in as numbers later, the codes 042.0 and 420 will
    # look identical.  To clarify this, we add the decimal point and standardize everything to length 4, and make sure
    # to read things in as strings in the future.
    # We recode entity codes (the other codes on the certificate) the same way
    # range determined by number of entity codes in dictionary (fewer in earlier years)

    for entity_idx in range(0, 1 + len([value for key, value in lb_data_dict.items() if key.startswith("entity")])):
        print 'recoding cause number %s' % entity_idx

        if entity_idx == 0:
            new_colname = 'cause'
        else:
            new_colname = 'multiple_cause_%s' % entity_idx
            old_colname = 'entity_%s' % entity_idx

            # the code has a bunch of extraneous information; just keep the cause code
            lb_data_raw[new_colname] = lb_data_raw[old_colname].fillna('0000000')
            lb_data_raw[new_colname] = lb_data_raw[new_colname].apply(lambda x: x[2:6].strip())

            # some infant cause codes that should be 3 digits are missing leading 0s in 2006 data - add those here
            lb_data_raw[new_colname] = lb_data_raw[new_colname].apply(lambda x: x.zfill(3) if len(x) < 3 else x)

        # add a decimal point if it's 4 elements long, otherwise keep it 3-digit
        lb_data[new_colname] = lb_data_raw[new_colname].apply(
            lambda x: x if len(x) == 3 else x[0:3] + '.' + x[3])

    # MISSINGNESS
    # count missingness/unknowns, get percentages
    lb_missingness = missing.calc_missingness(lb_data)

    # add deaths column because will need to include fractional deaths in later adjustments
    lb_data['deaths'] = lb_data_raw['record_weight']  # record weight = deaths in this case
    lb_data.loc[lb_data['deaths'] == ".", 'deaths'] = 1  # a couple cases where deaths = "." - reassign to 1
    lb_data['deaths'] = lb_data['deaths'].fillna(1)  # territories are not weighted, so just assign 1 death per row

    return lb_data_raw, lb_data, lb_missingness


# SCRIPT STARTS ACTUALLY RUNNING FUNCTIONS HERE
pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

print "running remotely!"
run_on_cluster(test=False)
