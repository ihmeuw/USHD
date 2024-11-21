"""
Description: Script to submit a linked birth data cleaning job for multiple years in parallel
NOTE: If just updating for a new year, simply change the 'years' variable in this script.
"""

import sys
import getpass
import os
os.chdir('FILEPATH/data_prep/counties/01_clean_microdata/')
from filepaths import get_linked_birth_filepaths
os.chdir("FILEPATH")
from sbatch import sbatch

def run_job(years):
    """Submit jobs."""
    worker = "FILEPATH/data_prep/counties/01_clean_microdata/linked_births.py"
    parent_dir = "FILEPATH"
    map_dir = "FILEPATH"
    log_dir = "FILEPATH"
    
    for year in years:
        us_indir, ps_indir = get_linked_birth_filepaths(year)
        sbatch(code = worker, name = 'cod_parse_{}'.format(year), 
            arguments = [year, us_indir, ps_indir, parent_dir, map_dir], 
            queue = "QUEUE", fthread = 1, m_mem_free = "16G", 
            h_rt = "01:00:00", archive = True, project = "PROJECT", 
            sgeoutput = log_dir,
            submit = True)


if __name__ == "__main__":
    years = range(1995, 2019)
    run_job(years)
