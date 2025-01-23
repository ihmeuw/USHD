"""
Description: 02_aggregate_births.py generates a new .csv of prepped births data for each year
             1989+. Here, we aggregate all years into one file.
"""

from __future__ import division
from datetime import datetime
import os
import pandas as pd
import sys

pd.set_option('display.max_rows', 10)
pd.set_option('display.max_columns', 10)

# set up parameters and directories
print "running remotely"
parent_dir, start_year, end_year, archive_date = sys.argv[1:5]
yearvals = range(int(start_year), int(end_year)+1)

# aggregate all years and save
alldata = pd.DataFrame()
for year in yearvals:
    print year
    data = pd.read_csv("FILEPATH"
    alldata = alldata.append(data)

if not os.path.exists("FILEPATH"
    os.makedirs("FILEPATH"
alldata.set_index("FILEPATH"
alldata.set_index("FILEPATH"
