"""
Description: 02_aggregate_microdata.py generates a new .csv of prepped mortality data for each year. 
             Here, we aggregate all years into one file.
"""

from __future__ import division
from datetime import datetime
import os
import pandas as pd
import sys

pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

# set up parameters and directories
parent_dir, agg_type, start_year, end_year, archive_date = sys.argv[1:6]
yearvals = range(int(start_year), int(end_year)+1)
race_ethn = True if agg_type == "county_race_ethn" else False
edu = True if agg_type == "county_edu" else False
if race_ethn:
    parent_dir += "_race_ethn"
elif edu:
    parent_dir += "_edu"

# aggregate all years and save
alldata = pd.DataFrame()
file_suffix = ""
if race_ethn:
    file_suffix += "_race_ethn"
elif edu:
    file_suffix += "_edu"
for year in yearvals:
    print year
    data = pd.read_csv("{}/deaths_by_cause_{}{}.csv".format(parent_dir, year, file_suffix))
    alldata = alldata.append(data)

alldata.set_index("year_id").to_csv("{}/deaths_by_cause_all{}.csv".format(parent_dir, file_suffix))
