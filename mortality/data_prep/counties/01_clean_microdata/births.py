##-----------------------------------------------------------------------------------------------------
## Description: this is the main script for data cleaning. It consists of three functions:
##				1. run_local: run data cleaning code on a local computer
##				2. run_on_cluster: run the data cleaning code within the current cluster session
##				3. parse_births: main function to clean data.
##
## 				parse_births is for the most part straightforward-- it loads data, recodes variables
##				to be consistent across years, and saves a cleaned files. At the end of the script, we
##				run a function that tabulates and reports missingness for each variable in the dataset.
##
##				Note that the script "begins" at the bottom, where functions actually get called.
##-----------------------------------------------------------------------------------------------------------

from __future__ import division
from datetime import datetime
import pandas as pd
from pandas import DataFrame
import platform
import numpy as np
import os
import sys

# import custom functions
import births_data_dicts
import recode


def run_local():
    """ Run data cleaning on a local, non-cluster machine or in an interactive cluster session.
    Custom modify all arguments.
    It is suggested that you set 'nrows' to a smaller value (in the low thousands)
    to avoid memory issues on local machines.
    """


    nrows = None  # number of rows of file to read; make it small for running local jobs
    map_dir = "FILEPATH"

    yearvals = [1989]

    for year in yearvals:
        us_indir = "FILEPATH"
        births_data = parse_births(year, us_indir, map_dir, nrows)
        return births_data


def run_on_cluster():
    """ Run data cleaning on the cluster, using arguments passed from the shell script.
		
    Passed arguments:
	year (int): year being cleaned.
        us_indir: filepath of the us county level file.
	parent_dir: root of filesystem to which you want to save results (see parallel_codeath.py for current parent dir)
	map_dir: filepath to dataset that, on the state level, maps nonfips code, FIPS code, postal code, and state name.
    """

    # set parameters
    year, us_indir, parent_dir, map_dir = sys.argv[1:6]

    # remove \r character that magically appears and I can't seem to remove with dos2unix or sed :(
    map_dir = map_dir.strip()

    year = int(year)
    nrows = None  # number of rows of file to read; make it small if you're just testing things out

    # run code
    births_data, births_data_raw = parse_births(year, parent_dir, us_indir, map_dir, nrows)

    # save files:
    # 0. make archive folders if they don't already exist
    archive_date = datetime.today().strftime('%Y_%m_%d')
    for sub_dir in ["FILEPATH", "FILEPATH"]:
        if not os.path.exists("FILEPATH"):
            os.makedirs("FILEPATH")
    
    # 1. Save parsed-but-not-cleaned files:
    births_data_raw.to_csv("FILEPATH", index=False)  # archived version
    births_data_raw.to_csv("FILEPATH", index=False)

    # add NIDs here, which depend on year, for database tracking
    nids = pd.read_csv("FILEPATH")
    births_data.index.name = None  # reset index to avoid an ambiguity error
    births_data = pd.merge(births_data, nids, on='year', how='left')
    births_data.index.name = None  # reset index to avoid an ambiguity error
    births_data['state_res_numeric'] = births_data['state_res_numeric'].fillna(0)  # Canadian provinces have NA state fips, so set to 0 for turning to int
    births_data['state_res_numeric'] = births_data['state_res_numeric'].astype(int)  # make sure numeric state code column is an int for merging with nids
    births_data.index.name = None  # reset index to avoid accidentally dropping year column

    # 2. Save cleaned data
    births_data.to_csv("FILEPATH", index=False)  # archived version
    births_data.to_csv("FILEPATH", index=False)


def parse_births(year, parent_dir, us_indir, map_dir, nrows=None):

    """ Parse and clean NVSS data, ensuring consistency across years.
    Passed arguments:
        year (int): year being cleaned.
        us_indir: filepath of the us county level file.
        ps_indir: if present, filepath of the Puerto Rico + territories file
        map_dir: filepath to dataset that, on the state level, maps nonfips code, FIPS code, postal code, and state name
    """

    births_data_dict = births_data_dicts.give_data_dict(year)

    dictlen = len(births_data_dict)
    convert = {}
    for idx in range(dictlen):
        convert[idx] = str

    births_data_raw = pd.io.parsers.read_fwf(us_indir, colspecs=births_data_dict.values(), header=None, nrows=nrows,
                                             converters=convert)
    births_data_raw.columns = births_data_dict.keys()
    print(births_data_raw.head())

    index = births_data_raw.index
    new_columns = ['year', 'state_res_numeric', 'state_res_alpha', 'county_res', 'full_fips_res_numeric',
                   'sex', 'mother_age', 'age', 'education', 'education_orig', 'edu_flag']
    births_data = DataFrame(index=index, columns=new_columns)

    # set year (note: this isn't in our data dict because it's coded inconsistently across years)
    births_data['year'] = year
    births_data_raw['year'] = year

    # set variables with easy or no recoding
    births_data['rec_weight'] = births_data_raw['rec_weight']
    births_data['mother_age'] = np.vectorize(recode.recode_mother_age)(age_var=births_data_raw['mother_age'])
    births_data['age'] = 0  # infants are age 0 at birth
    births_data['sex'] = births_data_raw['sex'].apply(recode.recode_sex) 

    # STATE AND COUNTY LABELS
    # Ensure that states have both a FIPS code and an alphabetic postal code.
    state_map = pd.read_csv(map_dir, dtype=str)
    alpha_map = dict(zip(state_map['fips'], state_map['alpha']))
    for county_type in ['res', 'occ']:  # repeat this process for state/county of residence and occurrence
        print 'recoding %s' % county_type

        if year >= 2003:
            fips_map = dict(zip(state_map['alpha'], state_map['fips']))
            births_data['state_%s_alpha' % county_type] = births_data_raw['state_%s_alpha' % county_type]
            births_data['state_%s_numeric' % county_type] = births_data['state_%s_alpha' % county_type].map(fips_map)
            # this dataset includes us territories. American Samoa and the Northern Marianas have counties coded to
            # '000' for everybody (residents and otherwise). Here, we recode deaths to territory residents that occur
            # in that territory to the special code '998', so we can identify the true '0's later
            # in the cleaning process.
            print "recoding marianas and samoa deaths"
            births_data_raw['county_%s' % county_type] =\
                np.vectorize(recode.recode_mp_as)(births_data_raw['state_occ_alpha'], births_data_raw['state_res_alpha'],
                                                  births_data_raw['county_%s' % county_type])
            # set any null 'county_occ' or 'county_res' values in 2004 to 999
            if year == 2004:
                births_data_raw['county_%s' % county_type] = births_data_raw['county_%s' % county_type].replace('nan', '999')
            births_data['county_%s' % county_type] = births_data_raw['county_%s' % county_type]
    
        else:
            births_data['state_%s_numeric' % county_type] = births_data_raw['state_%s_numeric' % county_type]
            births_data['state_%s_alpha' % county_type] = births_data['state_%s_numeric' % county_type].map(alpha_map)
            births_data['county_%s' % county_type] = births_data_raw['county_%s' % county_type]
    
        births_data['full_fips_%s_numeric' % county_type] = births_data['state_%s_numeric' % county_type] +\
            births_data['county_%s' % county_type]

    # EDUCATION: we had to make a new system for this in order to combine changes in all previous mapping methods;
    # see recode.py for details
    if year in range(1989, 2003):
        births_data_raw['education'] = births_data_raw['edu_1989']
        # years from 1989 to 2002 are flag 0 UNLESS the state didn't report education, then they're flag 2
        # first, pull list of states and when they started reporting education
        edu_flags_1989_2002 = pd.read_csv("FILEPATH")
        # next, assign flag based on only state and year. make sure relevant columns are integers before comparisons
        births_data_raw['year'] = births_data_raw['year'].astype(int)
        births_data_raw['state_occ_numeric'] = births_data_raw['state_occ_numeric'].astype(int)
        births_data_raw['month_birth'] = births_data_raw['month_birth'].astype(int)
        births_data_raw = pd.merge(births_data_raw, edu_flags_1989_2002[['state', 'year', 'edu_flag', 'month']],
                                   left_on=['year', 'state_occ_numeric'], right_on=['year', 'state'], how='left')
        # finally, take month into account (some states began reporting education partway through a year)
        # 'month' = month of the given year in which the state began reporting education. values are:
        # -1: edu reporting began in a previous year
        # when month = -99, edu was not reported any time in the given year
        # when month = 1-12, edu reporting began in the indicated month of the given year
        births_data_raw.loc[births_data_raw['month_birth'] < births_data_raw['month'], 'edu_flag'] = 2
        births_data_raw.loc[
            births_data_raw['state_occ_numeric'] > 56, 'edu_flag'] = 0  # assign flag 0 to all territories for now
        births_data_raw.drop(['state', 'month'], axis=1)  # no longer needed after edu_flags are assigned
    elif year in range(2003, 2014):
        # for years greater than 2003, two education columns are listed: edu_1989, for the states that report
        # using the 1989 system, and edu_2003, for the states that report using the 2003 system.  We have to create
        # one unified 'education' column that combines these two, to pass into the function (along with necessary flags)
        births_data_raw['education'] = births_data_raw['edu_1989'].fillna(births_data_raw['edu_2003'])
        births_data_raw.loc[births_data_raw['edu_flag'] == 'S', 'edu_flag'] = 0
        births_data_raw.loc[births_data_raw['edu_flag'] == 'A', 'edu_flag'] = 1
        births_data_raw.loc[births_data_raw['edu_report_flag'] == 0, 'edu_flag'] = 2
    else:
        births_data_raw['education'] = births_data_raw['edu_2003']
        births_data_raw['edu_flag'] = 1

    births_data['education'] = np.vectorize(recode.recode_education)(
        births_data_raw['education'], births_data_raw['edu_flag'])
    births_data['education_orig'] = births_data_raw['education']   # keep original codes and flags for 1989 and 2003 versions
    births_data['edu_flag'] = births_data_raw['edu_flag']

    births_data['births'] = births_data_raw['rec_weight'].fillna(1)  # if no record weight, assume 1 birth
    return births_data, births_data_raw


# SCRIPT STARTS ACTUALLY RUNNING FUNCTIONS HERE
pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

# uncomment to run on cluster via parallel_births.py
print "running remotely!"
run_on_cluster()

# uncomment to run locally as a test
# print "running locally!"
# births_data, births_data_raw = run_local()
