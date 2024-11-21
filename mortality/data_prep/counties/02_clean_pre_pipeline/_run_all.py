"""
Description: This script submits jobs to prepare the mortality data in three versions:
              1. Without race and ethnicity (Hispanic) or education info,  (.../county/)
              2. With race and ethnicity but without education info, and   (.../county_race_ethn/)
              3. With education but without race and ethnicity info.   (.../county_edu/)
"""

import getpass
import sys
import os
from datetime import datetime
os.chdir("FILEPATH")
from sbatch import sbatch

# point to US counties mortality repo
code_dir = 'FILEPATH/data_prep/counties/02_clean_pre_pipeline'
log_dir = "FILEPATH"
archive_date = datetime.today().strftime('%Y_%m_%d')

# make adjustments in data
r_img = "FILEPATH"
worker_adjustments = code_dir + '/01_impute_data.R'
adjustments_jid = sbatch(code = worker_adjustments, name = 'impute_mortality',
       arguments = ['mortality', archive_date], 
       queue = "QUEUE", fthread = 1, m_mem_free = "16G", 
       h_rt = "02:00:00", archive = True, project = "PROJECT", 
       sgeoutput = log_dir, shell = "FILEPATH",
       sing_image = r_img,
       submit = True)

# aggregate microdata by year
in_dir = "FILEPATH"
out_dir = "FILEPATH"
loc_dir = "FILEPATH"
yearvals = range(1980, 2021)

aggregation_jids = []
aggregation_jids_race_ethn = []
aggregation_jids_edu = []

for year in yearvals:
    jobname = 'aggregate_{}'.format(year)
    worker = code_dir + '/02_aggregate_microdata.py'
    jid = sbatch(code = worker, name = jobname,
                 hold = adjustments_jid,
                 arguments = [in_dir, out_dir, loc_dir, year, False, False, archive_date],
                 queue = "QUEUE", fthread = 1, m_mem_free = "15G", 
                 h_rt = "3:00:00", archive = True, project = "PROJECT", 
                 sgeoutput = log_dir, 
                 submit = True)

    jid_race_ethn = sbatch(code = worker, name = jobname + '_race_ethn',
                           hold = adjustments_jid,
                           arguments = [in_dir, out_dir + '_race_ethn', loc_dir, year, True, False, archive_date],
                           queue = "QUEUE", fthread = 1, m_mem_free = "15G", 
                           h_rt = "3:00:00", archive = True, project = "PROJECT", 
                           sgeoutput = log_dir, 
                           submit = True)
    
    if year >= 1989:  # edu is always missing before 1989, so drop earlier years
        jid_edu = sbatch(code = worker, name = jobname + '_edu',
                           hold = adjustments_jid,
                           arguments = [in_dir, out_dir + '_edu', loc_dir, year, False, True, archive_date],
                           queue = "QUEUE", fthread = 1, m_mem_free = "15G", 
                           h_rt = "3:00:00", archive = True, project = "PROJECT", 
                           sgeoutput = log_dir, 
                           submit = True)
        aggregation_jids_edu.append(jid_edu)
    aggregation_jids.append(jid)
    aggregation_jids_race_ethn.append(jid_race_ethn)

# aggregate all years (one county-level, one county-race/ethnicity-level, one county-edu-level)
worker = code_dir + '/04_agg_all_years_mort.py'
sbatch(code = worker, name = 'aggregate_all_years',
       hold = aggregation_jids,
       arguments = [out_dir, "county", yearvals[0], yearvals[len(yearvals)-1], archive_date],
       queue = "QUEUE", fthread = 1, m_mem_free = "40G", 
       h_rt = "5:00:00", archive = True, project = "PROJECT", 
       sgeoutput = log_dir, 
       submit = True)

sbatch(code = worker, name = 'aggregate_all_years_race_ethn',
       hold = aggregation_jids_race_ethn,
       arguments = [out_dir, "county_race_ethn", yearvals[0], yearvals[len(yearvals)-1], archive_date],
       queue = "QUEUE", fthread = 1, m_mem_free = "40G", 
       h_rt = "5:00:00", archive = True, project = "PROJECT", 
       sgeoutput = log_dir, 
       submit = True)

sbatch(code = worker, name = 'aggregate_all_years_edu',
       hold = aggregation_jids_edu,
       arguments = [out_dir, "county_edu", 1989, yearvals[len(yearvals)-1], archive_date],
       queue = "QUEUE", fthread = 1, m_mem_free = "40G", 
       h_rt = "5:00:00", archive = True, project = "PROJECT", 
       sgeoutput = log_dir, 
       submit = True)
