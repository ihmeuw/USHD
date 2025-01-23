"""
Description: This script submits jobs to aggregate births as a population denominator for infant mortality data split by
             mother's educational attainment.
"""

import getpass
import sys
import os
from datetime import datetime
USER = getpass.getuser()
h = 'FILEPATH'
os.chdir('%s/us_counties/mortality/sae_models/functions/' % h)
from sbatch import sbatch

# set the release date
release = 'FILEPATH'

# point to US counties mortality repo
code_dir = .format(h)
log_dir = 'FILEPATH'
archive_date = datetime.today().strftime('%Y_%m_%d')
r_img = 'FILEPATH'

# make adjustments in data
worker_adjustments = code_dir + '/01_impute_data.R'
adjustments_jid = sbatch(code = worker_adjustments, 
    name = 'impute_births', arguments = ['births', archive_date], 
    queue = "QUEUE", fthread = 1, m_mem_free = "15G", h_rt = "2:00:00", 
    archive = True, project = "PROJECT", sgeoutput = log_dir, 
    shell = 'FILEPATH',
    sing_image = r_img,
    submit = True)

# Upload cleaned mortality data
upload_worker = 'FILEPATH/upload_clean_data.R' % USER
upload_clean_births = sbatch(code = upload_worker, 
    name = 'upload_clean_births',
    hold = adjustments_jid, arguments = ['births', archive_date], 
    queue = "QUEUE", fthread = 1, m_mem_free = "150G", h_rt = "24:00:00", 
    archive = True, project = "PROJECT", sgeoutput = log_dir, 
    shell = 'FILEPATH',
    sing_image = r_img,
    submit = True)

# aggregate births by year
in_dir = 'FILEPATH'\
         'FILEPATH'.format(release)
out_dir = 'FILEPATH'\
    'FILEPATH'.format(release)
loc_dir = "FILEPATH/merged_counties.csv"
yearvals = range(2000, 2021)

aggregation_jids = []

for year in yearvals:
    jobname = 'FILEPATH'
    worker = code_dir + '/02_aggregate_births.py'
    jid = sbatch(code = worker, name = jobname,
        hold = adjustments_jid,
        arguments = [in_dir, out_dir, loc_dir, year, archive_date],
        queue = "QUEUE", fthread = 1, m_mem_free = "25G", 
        h_rt = "3:00:00", archive = True, project = "PROJECT", 
        sgeoutput = log_dir, 
        submit = True)
    aggregation_jids.append(jid)
    
# aggregate all births
worker = code_dir + '/04_agg_all_years_births.py'
sbatch(code = worker, name = 'aggregate_all_years',
    hold = aggregation_jids,
    arguments = [out_dir, yearvals[0], yearvals[len(yearvals)-1], archive_date],
    queue = "QUEUE", fthread = 1, m_mem_free = "35G", 
    h_rt = "5:00:00", archive = True, project = "PROJECT", 
    sgeoutput = log_dir, 
    submit = True)
