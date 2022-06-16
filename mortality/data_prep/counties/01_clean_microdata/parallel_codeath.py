"""
Description: Script to submit a data cleaning job (i.e. codeath.py) for multiple years in parallel
"""

import sys
import getpass
import os
from filepaths import get_filepaths
from datetime import datetime
from sbatch import sbatch

def run_job(years):
    """Submit qsubs."""
    worker = "FILEPATH/data_prep/counties/01_clean_microdata/codeath.py"
    parent_dir = "FILEPATH"
    map_dir = "FILEPATH"

    jid_parse = []
    for year in years:
        us_indir, ps_indir = get_filepaths(year)
        jid = sbatch(code = worker, name = 'cod_parse_{}'.format(year), 
                     arguments = [year, us_indir, ps_indir, parent_dir, map_dir, date], 
                     fthread = 1, m_mem_free = "16G", 
                     h_rt = "01:00:00",
                     sgeoutput = log_dir,
                     submit = True)
        jid_parse.append(jid)

if __name__ == "__main__":
    years = range(2000, 2020)
    run_job(years)
