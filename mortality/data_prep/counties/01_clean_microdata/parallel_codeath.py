"""
Description: Script to submit a data cleaning job (i.e. codeath.py) for multiple years in parallel
NOTE: If just updating for a new year, simply change the 'years' variable in this script.
"""

import sys
import getpass
import os
from filepaths import get_filepaths
from datetime import datetime

repo_dir = "FILEPATH"
sys.path.append(repo_dir)
os.chdir("FILEPATH")
from sbatch import sbatch

def run_job(years):
    """Submit jobs."""
    worker = "FILEPATH/data_prep/counties/01_clean_microdata/codeath.py"
    parent_dir = "FILEPATH"
    map_dir = "FILEPATH"
    date = datetime.today().strftime('%Y_%m_%d')
    log_dir = "FILEPATH"

    jid_parse = []
    for year in years:
        us_indir, ps_indir = get_filepaths(year)
        jid = sbatch(code = worker, name = 'cod_parse_{}'.format(year), 
                     arguments = [year, us_indir, ps_indir, parent_dir, map_dir, date], 
                     queue = "QUEUE", fthread = 1, m_mem_free = "20G", 
                     h_rt = "01:00:00", archive = True, project = "PROJECT", 
                     sgeoutput = log_dir,
                     submit = True)
        jid_parse.append(jid)

if __name__ == "__main__":
    years = range(1980, 2021)
    run_job(years)
