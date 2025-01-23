##-----------------------------------------------------------------------------------------------------
## Description: All NVSS data is stored in fixed-width file formats, but the encoding of those files
##				(i.e. which variables are stored where on each row) changes from year to year. These
##				dicts identify the variables of interest in each file, and map their positions for extraction.
##				At the bottom of the script is a function (give_data_dict()) that takes in a year value and
##				returns the appropriate dict.
##
## Full documentation can be found here:
## 			1959-1990 available at: http://www.nber.org/data/vital-statistics-mortality-data-multiple-cause-of-death.html
##			1991-2001 available at: http://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm
## 			2002-2006 available in the respective year's folder in FILEPATH
## 			2007 onward not available for specific years, but the documentation in the appropriate folders suggests that format is the same as 2005's.
##

##		  corresponds to the latest dict (currently labeled data_dict_2003_2018). If it does correspond, you can
##		  simply update that dict (e.g. to data_dict_2003_2019) and increment the ranges in the yeardict variable
##		  of the give_data_dict() function at the bottom of the script. If the data does not correspond to the latest
##		  dict, you must write a new dict and update give_data_dict() appropriately.
##-----------------------------------------------------------------------------------------------------------------

import BetweenDict as bd


data_dict_1980_1981 = {
    'rec_type': [10, 11],
    'res_status': [11, 12],
    'rec_weight': [207, 208],
    'state_occ_NONFIPS': [27, 29],
    'county_occ_NONFIPS': [29, 32],
    'state_res_NONFIPS': [12, 14],
    'county_res_NONFIPS': [14, 17],
    'sex': [34, 35],
    'mother_age': [40, 42]
}

data_dict_1982_1988 = {
    'rec_type': [10, 11],
    'res_status': [11, 12],
    'rec_weight': [207, 208],
    'state_occ_numeric': [193, 195],
    'county_occ': [195, 198],
    'state_res_numeric': [198, 200],
    'county_res': [200, 203],
    'full_fips_res_numeric':[198, 203],
    'sex': [34, 35],
    'mother_age': [40, 42]
}

data_dict_1989_2002 = {
    'rec_type': [4, 5],
    'res_status': [5, 6],
    'rec_weight': [6, 7],
    'state_occ_numeric': [20, 22],
    'county_occ': [22, 25],
    'state_res_numeric': [41, 43],
    'county_res': [43, 46],
    'full_fips_res_numeric': [41, 46],
    'month_birth': [171, 173],
    'sex': [188, 189],
    'mother_age': [75, 76],  # mother's age group recode (note: in 1997 age group changed from 10-49 to 10-54)
    'edu_1989': [82, 84]  # mother's education (automatically 1989 version)
}

data_dict_2003_2013 = {
    'rec_type': [136, 137],
    'res_status': [137, 138],
    'rec_weight': [7, 8],
    'state_occ_alpha': [29, 31],
    'county_occ': [36, 39],
    'state_res_alpha': [108, 110],  # explicitly mother's state of residence
    'county_res': [113, 116],  # also mother's
    'sex': [435, 436],
    'mother_age': [92, 93],  # mother's age group recode (single year ages are all NA)
    'edu_2003': [154, 155],  # mother's education, 2003 version
    'edu_1989': [155, 157],  # mother's education, 1989 version
    'edu_1989_recode': [157, 158],  # mother's education recode, 1989 version
    'edu_flag': [6, 7],  # version indicator - 1989 (S) or 2003 (A) version of US birth certificate
    'edu_report_flag': [570, 571],  # flag indicating whether or not mother's education was reported
}

data_dict_2014_2020 = {
    'rec_type': [102, 103],
    'res_status': [103, 104],
    'rec_weight': [6, 7],
    'state_occ_alpha': [23, 25],
    'county_occ': [27, 30],
    'state_res_alpha': [88, 90],  # explicitly mother's state of residence
    'county_res': [90, 93],  # also mother's
    'sex': [474, 475],
    'mother_age': [78, 79],  # mother's age group recode (single year ages are all NA)
    'edu_2003': [123, 124],  # mother's education, 2003 version (1989 version not used in 2014+ data)
}


def give_data_dict(year):
    
    yeardict = {
        (1980, 1982): (1980, 1981),
        (1982, 1989): (1982, 1988),
        (1989, 2003): (1989, 2002),
        (2003, 2014): (2003, 2013),
        (2014, 2021): (2014, 2020)
    }

    yearmap = bd.BetweenDict(yeardict)
    yearvals = yearmap[year]
    dict_str = 'data_dict_%d_%d' % (yearvals[0], yearvals[1])
    data_dict = eval(dict_str)
    return data_dict
