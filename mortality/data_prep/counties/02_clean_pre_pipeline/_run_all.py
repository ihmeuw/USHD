"""
Description: This script submits jobs to prepare the mortality data for the CoD
             pipeline by race/ethn
"""

import getpass
import sys
import os
from datetime import datetime
from sbatch import sbatch


# point to US counties mortality repo
code_dir = 'FILEPATH'

# make adjustments in data
r_img = 'FILEPATH'
worker_adjustments = code_dir + '/01_impute_data.R'
adjustments_jid = sbatch(code = worker_adjustments, name = 'impute_mortality',
       arguments = ['mortality'], fthread = 1, m_mem_free = "16G", 
       h_rt = "02:00:00", archive = True,, shell = 'FILEPATH',
       sing_image = r_img,
       submit = True)


# aggregate microdata by year
in_dir = 'FILEPATH'
out_dir = 'FILEPATH'
loc_dir = 'FILEPATH'
yearvals = range(2000, 2020)

aggregation_jids_race_ethn = []
worker = code_dir + '/02_aggregate_microdata.py'

for year in yearvals:
    jobname = 'aggregate_{}'.format(year)

    jid_race_ethn = sbatch(code = worker, name = jobname,
                           hold = adjustments_jid,
                           arguments = [in_dir, out_dir, loc_dir, year, True, False, archive_date],
                           fthread = 1, m_mem_free = "10G", 
                           h_rt = "3:00:00", archive = True, 
                           submit = True)
    
    aggregation_jids_race_ethn.append(jid_race_ethn)

worker = code_dir + '/03_agg_all_years_mort.py'
sbatch(code = worker, name = 'aggregate_all_years',
       hold = aggregation_jids_race_ethn,
       arguments = [out_dir, "county_race_ethn", yearvals[0], yearvals[len(yearvals)-1], archive_date],
       fthread = 1, m_mem_free = "30G", 
       h_rt = "5:00:00", archive = True,
       submit = True)
