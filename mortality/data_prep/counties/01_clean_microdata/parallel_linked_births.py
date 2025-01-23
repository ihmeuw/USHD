"""
Description: Script to submit a linked birth data cleaning job for multiple years in parallel
NOTE: If just updating for a new year, simply change the 'years' variable in this script.
"""

import sys
import getpass
import os
os.chdir('FILEPATH')
from filepaths import get_linked_birth_filepaths
USER = getpass.getuser()
h = 'FILEPATH'
j = 'FILEPATH'
'FILEPATH'
release = 'FILEPATH'     # set release date
os.chdir('%s/us_counties/mortality/sae_models/functions/' % h)
from sbatch import sbatch

def run_job(years):
    """Submit qsubs."""
    worker = "%s/us_counties/mortality/data_prep/counties/"\
             "01_clean_microdata/linked_births.py" % h
    parent_dir = FILEPATH\
                 "FILEPATH".format(l, release)
    map_dir = "%s/FILEPATH/state_map.csv" % j
    log_dir = 'FILEPATH'
    
    for year in years:
        us_indir, ps_indir = get_linked_birth_filepaths(year)
        sbatch(code = worker, name = 'FILEPATH'
            arguments = [year, us_indir, ps_indir, parent_dir, map_dir], 
            queue = "QUEUE", fthread = 1, m_mem_free = "16G", 
            h_rt = "01:00:00", archive = True, project = "PROJECT", 
            sgeoutput = log_dir,
            submit = True)


if __name__ == "__main__":
    years = range(1995, 2019)
    run_job(years)
