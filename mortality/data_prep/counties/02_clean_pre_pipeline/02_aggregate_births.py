"""
input: files in clean_data - parsed, CSV-form data with formatting modifications made in births.py
output: files in FILEPATH - births data compressed to the format of births by year, county,
        sex, mother's age, and mother's educational attainment.
"""

from __future__ import division
from datetime import datetime
import os
import pandas as pd
import numpy as np
import itertools
import sys

pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

print "running remotely"
parent_in_dir, parent_out_dir, loc_dir, year, archive_date = sys.argv[1:6]
year = int(year)  # convert from string
yearvals = [year]

######################
# start aggregating
######################
for year in yearvals:
    print 'aggregating births for %d' % year

    # read in merged counties and location ids
    loc = pd.read_csv(loc_dir)
    loc.rename(columns={'cnty': 'fips', 'state': 'state_fips'}, inplace=True)  # rename columns for easier merging

    dtype_dict = {"mother_age": str}  # import mother's age column as strings

    # pull clean, imputed data
    in_dir = '%s/births/births_%s_cleaned.csv' % (parent_in_dir, year)

    # what column names in the dataset do you want to index on?
    index_cols = ['year', 'nid', 'state_fips', 'mcnty', 'sex', 'mother_age', 'age', 'edu', 'edu_label']

    # read in microdata for specified year
    microdata = pd.read_csv(in_dir, dtype=dtype_dict)

    microdata.rename(columns={'state_res_numeric': 'state_fips'}, inplace=True)
    microdata['fips'] = 1000*microdata['state_fips'] + microdata['county_res']  # add full FIPS code

    # drop territories and foreign locations
    microdata = microdata.query('(state_fips < 57) & (county_res!=0) & (state_fips!=0)')
    microdata = microdata[microdata.state_fips.notnull()]

    # set unknown mother's age values to -9
    microdata.mother_age = microdata.mother_age.replace(['999'], ['-9'])

    # merge on county fips to get mcnty and location ids
    microdata = pd.merge(microdata, loc[['fips', 'mcnty', 'location_id']], on='fips', how='left')

    # Fips 26124 never existed, reassigning to largest county in Michigan, Wayne County.
    microdata.loc[microdata.fips == 26124, 'fips'] = 26163

    null = microdata[pd.isnull(microdata['location_id'])]
    if not null.empty:
        print 'NULL LOCATION IDS'
        break

    # sum up number of births, stratified by sex, mother's age, year, and county
    print "aggregating counts"
    aggregated = microdata.groupby(index_cols)[["births"]].sum()

    # we only need to keep one column of this since they're all the same, and rename that column to reflect what
    # the number actually means (a count of births)
    sort_cols = ['year', 'nid', 'mcnty', 'state_fips', 'sex', 'mother_age', 'age', 'edu', 'edu_label']
    aggregated = aggregated.reset_index().sort_values(by=sort_cols).set_index('year')
    aggregated['mother_age'] = pd.to_numeric(aggregated['mother_age'])  # convert mother's age to int for merging later
    births_count_orig = aggregated.births.sum()  # compare with births count after squaring to make sure nothing changes

    # make births dataset square, since it is a type of population file
    # note: this may need to change in the future based on how we end up imputing values for unknown education
    sexes = [1, 2]
    mother_ages = range(10, 55, 5)
    edus = range(100, 105)
    edu_labels_dict = {100: 'Unknown', 101: 'Less than HS', 102: 'HS graduate', 103: 'Some college', 104: 'College graduate'}
    square_data = pd.DataFrame(itertools.product([year], aggregated.nid.unique(), loc.mcnty.unique(), sexes,
                                                 mother_ages, [0], edus))  # 0 refers to infant age at birth here
    square_data.columns = ['year', 'nid', 'mcnty', 'sex', 'mother_age', 'age', 'edu']  # re-assign column names
    square_data = pd.merge(square_data, loc[['mcnty', 'state_fips']].drop_duplicates(), on='mcnty', how='left')  # re-attach state FIPS
    square_data = pd.merge(square_data, aggregated, how='left')  # merge existing, non-zero birth counts
    square_data['edu_label'] = square_data['edu'].map(edu_labels_dict)  # add edu labels to rows with 0 births
    square_data['births'] = square_data['births'].fillna(0)  # fill NA birth counts with 0s
    square_data = square_data[sort_cols + ['births']]  # set column order
    square_data = square_data.sort_values(by=sort_cols)  # sort data by sort columns
    print 'finished squaring data'

    # check that births count remained the same after squaring data (allow for rounding error)
    if not np.isclose(square_data.births.sum(), births_count_orig, atol=1e-08):
        if square_data.births.sum() > births_count_orig.round():
            print 'ERROR: squaring data added births'
            break
        elif square_data.births.sum() < births_count_orig.round():
            print 'ERROR: squaring data removed births'
            break
    else:
        print 'squaring data did not change births count'

    print "saving and archiving files"
    file_name = 'births_%d.csv' % year
    if not os.path.exists('%s/_archive/%s' % (parent_out_dir, archive_date)):
        os.makedirs('%s/_archive/%s' % (parent_out_dir, archive_date))
    square_data.to_csv('%s/_archive/%s/%s' % (parent_out_dir, archive_date, file_name), index=False)  # archived version
    square_data.to_csv('%s/%s' % (parent_out_dir, file_name), index=False)
