"""
There was previously existing code for cause of death work that reads in the cause-specific death
microdata in from the NVSS and converts it into csv format (from fixed-width files).
All we need for this life expectancy work is death counts by year, county, sex, age,
and cause. This script reads in each year's microdata file, compresses it to
get those death counts, and reads it out in the format it should be in for the life
expectancy regression.

There are a few wrinkles in this process, one dealing with misassigned deaths in
Alaska, New York, and Virginia, and the other with censored deaths in Georgia.  We address
these issues in 01_adjust_alaska_nyc_virginia.R and 02_adjust_georgia.R in this folder.

input: files in clean_data - parsed, CSV-form data with formatting modifications made in codeath.py
output: files in FILEPATH, FILEPATH, and FILEPATH - FILEPATH
        data compressed to the format of deaths by year, county, sex, age, and cause. Files with race
        and ethnicity information have deaths by year, county, sex, age, cause, race, and race_hisp_recode.
        Files with education information have deaths by year, county, sex, age, cause, and educational
        attainment.
"""

from __future__ import division
from datetime import datetime
import os
import pandas as pd
import numpy as np
import sys

pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

print "running remotely"
parent_in_dir, parent_out_dir, loc_dir, year, include_race_ethn, include_edu, archive_date = sys.argv[1:8]
include_race_ethn = include_race_ethn.strip()
include_edu = include_edu.strip()
include_race_ethn = include_race_ethn == "True"
include_edu = include_edu == "True"
year = int(year)  # convert from string
yearvals = [year]


def make_dtype():  # we want to import these columns as strings, even if they seem numeric
    key_list = ['cause']
    for idx in range(1, 21):
        key_list.append('multiple_cause_%s' % idx)
    dtype_dict = {key: str for key in key_list}

    dtype_dict['age'] = str
    dtype_dict['icd_version'] = str
    dtype_dict['education'] = str

    return dtype_dict, key_list


######################
# start aggregating
######################
for year in yearvals:
    print 'aggregating data for %d' % year

    # prep locations
    loc = pd.read_csv(loc_dir + "locations.csv")
    loc.drop(['location_parent_id', 'path_to_top_parent',
              'end_date', 'current'], axis=1, inplace=True)
    states, counties = loc.query('fips < 100'), loc.query('fips > 100')
    states.rename(columns={'fips': 'state_fips'}, inplace=True)

    dtype_dict, cause_list = make_dtype()

    in_dir = '%s/mortality/data_%s_cleaned.csv' % (parent_in_dir, year)

    # what column names in the dataset do you want to index on?
    index_cols = ['year_id', 'icd_version', 'state_fips', 'location_id', 'sex_id', 'age_group_id', 'value', 'nid']
    if include_race_ethn:
        index_cols.extend(['race_orig', 'race_1977', 'race_1997', 'race_label_1977', 'race_label_1997',
                           'race_hisp_recode', 'hisp_recode', 'hisp_label'])
    elif include_edu:
        index_cols.extend(['edu', 'edu_label'])

    # read in merged-county microdata
    microdata = pd.read_csv(in_dir, delimiter=',', dtype=dtype_dict)
    microdata.rename(columns={'education': 'edu'}, inplace=True)  # rename here so columns are consistent with those in linked births

    # for mortality by race/ethnicity, simplify and add labels for hisp_recode
    if include_race_ethn:
        microdata.hisp_recode = microdata.hisp_recode.replace([7, 8], 6)  # collapse non-Hispanic white, black, other to just non-Hispanic
        hisp_labels_dict = {1: 'Mexican', 2: 'Puerto Rican', 3: 'Cuban', 4: 'Central or South American',
                            5: 'Other or unspecified Hispanic', 6: 'Non-Hispanic', 9: 'Unknown'}
        microdata['hisp_label'] = microdata['hisp_recode'].map(hisp_labels_dict)

    # for mortality by education, replace infant deaths with linked birth/infant death info in avail. years
    if include_edu and year in range(1995, 2019):
        # get linked birth/infant death data
        in_dir_lb = '%s/linked_births/data_%s_cleaned.csv' % (parent_in_dir, year)
        lb_data = pd.read_csv(in_dir_lb, delimiter=',', dtype=dtype_dict)
        lb_data.rename(columns={'mother_edu_flag': 'edu_flag', 'mother_education': 'edu',
                                'mother_education_orig': 'edu_orig'}, inplace=True)
        nid = microdata[microdata.state_res_numeric < 57].nid.unique()[0]
        lb_data['nid'] = nid

        # drop infants from mortality data and combine with lb data
        microdata = microdata.drop(microdata[microdata.age.isin(["999i", "0d", "1-5mo", "7d", "6-11mo"])].index)
        microdata = lb_data.append(microdata)

    microdata.rename(columns={'full_fips_res_numeric': 'fips', 'state_res_numeric': 'state_fips'}, inplace=True)

    # drop deaths to 'foreign' residents (county_res==0)
    microdata = microdata.query('(county_res!=0) & (state_fips!=0)')

    # set unknown age values to -9
    microdata.age = microdata.age.replace(['999', '999i', '0d', '7d', '28d'], ['-9', '-9', '0', '0.01', '0.1'])

    # merge on location ids
    microdata = pd.merge(microdata, states[['state_fips', 'location_id']], on='state_fips', how='left')
    microdata.rename(columns={'location_id': 'state'}, inplace=True)

    # Fips 26124 never existed, reassigning to largest county in Michigan, Wayne County.
    microdata.loc[microdata.fips == 26124, 'fips'] = 26163

    # this will cause NaNs for the Northern Marianas and American Samoa, which have a made-up 'national' fips.
    microdata = pd.merge(microdata, counties[['fips', 'location_id']], on='fips', how='left')

    # replace null location ids in Northern Marianas and American Samoa with state-level location ids:
    # 376 for Marianas, 298 for Samoa
    def recode_mp_as(state_location_id, location_id):
        if pd.isnull(location_id):
            return state_location_id
        else:
            return location_id
    microdata['location_id'] = np.vectorize(recode_mp_as)(microdata['state'], microdata['location_id'])

    null = microdata[pd.isnull(microdata['location_id'])]
    if not null.empty:
        print 'NULL LOCATION IDS'

    microdata.rename(columns = {'fips': 'county', 'year': 'year_id', 'cause': 'value', 'age': 'age_group_id',
                                'sex': 'sex_id'}, inplace=True)

    # assign standard IDs for race_1977 & race_1997 based on values in shared database
    microdata['race_orig'] = microdata['race']  # preserve original race values
    race_ids_1977_dict = {'NH White': 5, 'NH Black': 4, 'NH AIAN': 6, 'NH API': 7, 'Hispanic': 2, 'NH Other Race': 3}
    race_ids_1997_dict = {'NH White alone': 10, 'NH Black alone': 11, 'NH AIAN alone': 12, 'NH Asian alone': 13,
                          'NH NHOPI alone': 14, 'NH API': 7, 'NH Multiracial': 15, 'Hispanic': 2, 'NH Other Race': 3}
    microdata['race_1977'] = microdata['race_label_1977'].map(race_ids_1977_dict)
    microdata['race_1997'] = microdata['race_label_1997'].map(race_ids_1997_dict)

    # add edu_label
    edu_labels_dict = {100: 'Unknown', 101: 'Less than HS', 102: 'HS graduate', 103: 'Some college', 104: 'College graduate'}
    microdata['edu_label'] = microdata['edu'].map(edu_labels_dict)
    
    # replace location ids with merged location ids
    # merged_location_id == location_id except for counties that are part of a merged county with multiple county FIPS, so we can do a simple replacement
    mcnty = pd.read_csv(loc_dir + "merged_counties.csv")
    microdata = pd.merge(microdata, mcnty[['location_id', 'merged_location_id']], on='location_id', how='left')
    microdata['location_id'] = microdata['merged_location_id']
    microdata = microdata.drop('merged_location_id', axis=1)

    # sum up number of deaths, stratified by sex, age, year, and county
    print "aggregating counts"
    aggregated = microdata.groupby(index_cols)[["deaths"]].sum()

    # we only need to keep one column of this since they're all the same, and rename that column to reflect what
    # the number actually means (a count of deaths)
    sort_cols = ['year_id', 'state_fips', 'location_id', 'sex_id', 'age_group_id', 'value']
    if include_race_ethn:
        sort_cols.extend(['race_orig', 'race_1977', 'race_1997', 'race_label_1977', 'race_label_1997'])
    elif include_edu:
        sort_cols.extend(['edu', 'edu_label'])

    aggregated = aggregated.reset_index().sort_values(by=sort_cols).set_index('year_id')

    # split into territories and us
    # remove 'state_fips' column per CoD pre-redistribution format for non-territory data
    us = aggregated.query('state_fips < 57')
    us = us.drop(['state_fips'], axis=1)

    territories = aggregated.query('state_fips > 56')

    print "saving and archiving files"
    if not os.path.exists('%s/FILEPATH/%s' % (parent_out_dir, archive_date)):
        os.makedirs('%s/FILEPATH/%s' % (parent_out_dir, archive_date))
    file_name = 'deaths_by_cause_%d.csv' % year
    terr_file_name = 'deaths_by_cause_territories_%d.csv' % year

    if include_race_ethn:
        file_name = 'deaths_by_cause_%d_race_ethn.csv' % year
        terr_file_name = 'deaths_by_cause_territories_%d_race_ethn.csv' % year
    elif include_edu:
        file_name = 'deaths_by_cause_%d_edu.csv' % year
        terr_file_name = 'deaths_by_cause_territories_%d_edu.csv' % year
    us.to_csv('%s/FILEPATH/%s/%s' % (parent_out_dir, archive_date, file_name))  # archived version
    us.to_csv('%s/%s' % (parent_out_dir, file_name))

    if not territories.empty:
        territories = territories.drop(['state_fips'], axis=1)
        territories.to_csv('%s/FILEPATH/%s/%s' % (parent_out_dir, archive_date, terr_file_name))  # archived version
        territories.to_csv('%s/%s' % (parent_out_dir, terr_file_name))
