"""Pull the filename based on the year of data.

Filenames were previously inconsistent and this dictionary was a way of keeping
track of each filename. In August of 2017 new NVSS mortality files were
received, and this method became much simpler.

NOTE: TO PROCESS A NEW YEAR OF DATA, ENSURE THE FILENAME FOLLOWS THIS PATTERN

input: --year (int), the year for which you want to retrieve filepaths

output: Two filepaths 'US' refers to the United States file,
'PS' to the Puerto Rico + territories file
"""
import platform

def get_filepaths(year):
    """Get file names for each year of data. Update as we get new data"""
    # temp fix until all data are moved to the 2019 directory (currently, only 2018 and 2019 data have been moved)
    if year in range(2018, 2020):
        parent_folder = 'FILEPATH'
    else:
        parent_folder = 'FILEPATH'
    public_dir = 'FILEPATH'

    if year in range(1968, 1989):
        us_indir = 'FILEPATH'
        ps_indir = 'NONE'
    else:
        us_indir = 'FILEPATH'
        # there are territories files for 1994, however, they are separate for each territory
        if year == 1994:
            ps_indir = [
                'FILEPATH',
                'FILEPATH',
                'FILEPATH'
            ]
        elif year == 1999:
            ps_indir = 'FILEPATH'
        elif year in range(1995, 2000):
            ps_indir = 'FILEPATH'
        elif year >= 2000:
            ps_indir = 'FILEPATH'
        else:
            ps_indir = 'NONE'

    return us_indir, ps_indir