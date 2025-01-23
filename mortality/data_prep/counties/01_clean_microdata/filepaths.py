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
j = "FILEPATH"
l = "FILEPATH"

def get_filepaths(year):
    """Get file names for each year of data. Update as we get new data"""
    # temp fix until all data are moved to the 2020 directory (currently, only 2018-2020 data have been moved)
    if year in range(2018, 2021):
        RELEASE = 'FILEPATH'
        parent_folder = 'FILEPATH'.format(l, RELEASE)
    else:
        RELEASE = 'FILEPATH'
        parent_folder = 'FILEPATH'.format(l, RELEASE)
    public_dir = 'FILEPATH'

    if year in range(1968, 1989):
        us_indir = .format(
            public_dir, year
        )
        ps_indir = 'NONE'
    else:
        us_indir = .format(parent_folder, year, RELEASE)
        # there are territories files for 1994, however, they are separate for each territory
        if year == 1994:
            ps_indir = [
                'FILEPATH'
                'GUAM.TXT'.format(public_dir, year),
                'FILEPATH'
                'PUERTO_RICO.TXT'.format(public_dir, year),
                'FILEPATH'
                'VIRGIN_ISLANDS.TXT'.format(public_dir, year)
            ]
        elif year == 1999:
            ps_indir = 'FILEPATH'\
                       'TERRITORIES.DAT'.format(public_dir, year)
        elif year in range(1995, 2000):
            ps_indir = 'FILEPATH'\
                       'TERRITORIES.TXT'.format(public_dir, year)
        elif year >= 2000:
            ps_indir = 'FILEPATH'.format(
                parent_folder, year, RELEASE)
        else:
            ps_indir = 'NONE'

    return us_indir, ps_indir

def get_births_filepaths(year):
    RELEASE = 'FILEPATH'
    year_range = '1989_2020'

    parent_folder = 'FILEPATH'\
                    'NATALITY'.format(l, year_range, RELEASE)

    if year in range(1980, 1989):
        us_indir = "FILEPATH"
    else:
        us_indir = .format(parent_folder, year, RELEASE)

    return us_indir

def get_linked_birth_filepaths(year):
    RELEASE = 'FILEPATH'
    year_range = '1989_2019'

    parent_folder = 'FILEPATH'\
                            'LINKED_BIRTH_INFANT_DEATHS/LINKED_ALL_COUNTY'.format(l, year_range, RELEASE, year_range)

    us_indir = .format(parent_folder, year)
    ps_indir = .format(parent_folder, year)

    return us_indir, ps_indir
