"""
Description: 02_aggregate_microdata.py generates a new .csv of prepped mortality data for each year
             1980+. Here, we aggregate all years into one file.
"""

from __future__ import division
from datetime import datetime
import os
import pandas as pd
import sys

pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

# set up parameters and directories
if len(sys.argv) > 1:
    print "running remotely"
    parent_dir, agg_type, start_year, end_year, archive_date = sys.argv[1:6]
    yearvals = range(int(start_year), int(end_year)+1)
    race_ethn = True if agg_type == "county_race_ethn" else False
    edu = True if agg_type == "county_edu" else False
    if race_ethn:
        parent_dir += "_race_ethn"
    elif edu:
        parent_dir += "_edu"
else:
    print "running locally"

    j = 'FILEPATH'
    LU = 'FILEPATH'
    RELEASE = 'FILEPATH'

    # set up directories
    parent_dir = 'FILEPATH'.format(LU, RELEASE)
    yearvals = range(1999, 2004)
    race_ethn = False
    edu = False
    archive_date = datetime.today().strftime('%Y_%m_%d')

# aggregate all years and save
alldata = pd.DataFrame()
file_suffix = ""
if race_ethn:
    file_suffix += "_race_ethn"
elif edu:
    file_suffix += "_edu"
for year in yearvals:
    print year
    data = pd.read_csv("FILEPATH"
    alldata = alldata.append(data)

if not os.path.exists("FILEPATH"
    os.makedirs("FILEPATH"
alldata.set_index("FILEPATH"
alldata.set_index("FILEPATH"
