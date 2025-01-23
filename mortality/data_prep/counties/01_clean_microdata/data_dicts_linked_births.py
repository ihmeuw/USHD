##-----------------------------------------------------------------------------------------------------
## Description: All NVSS data is stored in fixed-width file formats, but the encoding of those files
##				(i.e. which variables are stored where on each row) changes from year to year. These
##				dictionaries identify the variables of interest in each file, and map their positions for extraction.
##				At the bottom of the script is a function (give_data_dict()) that takes in a year value and
##				returns the appropriate dict.
##
## Documentation can be found here:
##        FILEPATH\
##           DOCUMENTATION\
##

##		  corresponds to the latest dict. If it does correspond, you can simply update that dict and increment
##        the ranges in the yeardict variable of the give_data_dict() function at the bottom of the script. If the
##        data does not correspond to the latest dict, you must write a new dict and update give_data_dict()
##        appropriately.
##-----------------------------------------------------------------------------------------------------------------

import BetweenDict as bd

data_dict_1995_2002 = {
    'year': [523, 527],
    'res_status': [504, 505],
    'month_death': [527, 529],
    'full_fips_res_numeric': [512, 517],
    'state_res_numeric': [512, 514],
    'county_res': [514, 517],
    'full_fips_occ_numeric': [507, 512],
    'state_occ_numeric': [507, 509],
    'county_occ': [509, 512],
    'state_occ_birth': [13, 15],
    'state_res_birth': [18, 20],
    'county_res_birth': [20, 23],
    'sex': [78, 79],
    'age_days': [210, 213],
    'age_recode_5': [213, 214],
    'mother_age': [29, 31],
    'mother_age_recode_9': [31, 32],     # only codes 1-8 in 1995-6
    'mother_age_flag': [28, 29],
    'mother_education': [38, 40],
    'mother_edu_recode': [40, 41],
    'cause': [215, 219],
    'cause_recode': [219, 222],
    'place_accident': [214, 215],
    'number_entities': [260, 262],
    'entity_1': [262, 269],
    'entity_2': [269, 276],
    'entity_3': [276, 283],
    'entity_4': [283, 290],
    'entity_5': [290, 297],
    'entity_6': [297, 304],
    'entity_7': [304, 311],
    'entity_8': [311, 318],
    'entity_9': [318, 325],
    'entity_10': [325, 332],
    'entity_11': [332, 339],
    'entity_12': [339, 346],
    'entity_13': [346, 353],
    'entity_14': [353, 360],
    'entity_15': [360, 367],
    'entity_16': [367, 374],
    'entity_17': [374, 381],
    'entity_18': [381, 388],
    'entity_19': [388, 395],
    'entity_20': [395, 402],
    'record_weight': [222, 230]
}

data_dict_2003_2003 = {
    'year': [1070, 1074],
    'res_status': [1033, 1034],
    'month_death': [1140, 1142],
    'state_occ_alpha': [1034, 1036],
    'county_occ': [1036, 1039],
    'state_occ_alpha_birth': [29, 31],
    'state_res_alpha': [1042, 1044],
    'county_res': [1048, 1051],
    'state_res_alpha_birth': [108, 110],
    'county_res_birth': [113, 116],
    'pmsa_res': [1060, 1064],
    'cmsa_res': [1066, 1068],
    'sex': [435, 436],
    'age_days': [754, 757],
    'age_recode_5': [757, 758],
    'age_recode_22': [758, 760],
    'mother_age': [76, 78],
    'mother_age_recode_9': [92, 93],
    'mother_age_recode_14': [90, 92],
    'mother_age_recode_41': [88, 90],
    'mother_age_flag': [87, 88],
    'mother_age_imputation': [86, 87],
    'mother_edu_revised': [154, 155],
    'mother_edu_unrevised': [155, 157],
    'mother_edu_recode': [157, 158],
    'cause': [766, 770],
    'cause_recode': [771, 774],
    'manner_of_death': [760, 761],
    'place_death': [1068, 1069],
    'number_entities': [785, 787],
    'entity_1': [787, 794],
    'entity_2': [794, 801],
    'entity_3': [801, 808],
    'entity_4': [808, 815],
    'entity_5': [815, 822],
    'entity_6': [822, 829],
    'entity_7': [829, 836],
    'entity_8': [836, 843],
    'entity_9': [843, 850],
    'entity_10': [850, 857],
    'entity_11': [857, 864],
    'entity_12': [864, 871],
    'entity_13': [871, 878],
    'entity_14': [878, 885],
    'entity_15': [885, 892],
    'entity_16': [892, 899],
    'entity_17': [899, 906],
    'entity_18': [906, 913],
    'entity_19': [913, 920],
    'entity_20': [920, 927],
    'record_weight': [775, 783]
}

data_dict_2004_2013 = {
    'year': [1187, 1192],
    'res_status': [1150, 1151],
    'month_death': [1257, 1259],
    'state_occ_alpha': [1151, 1153],    # missing for 2005-06 (documentation says territories only)
    'county_occ': [1153, 1156],         # missing for 2005-06 (documentation says territories only)
    'state_occ_alpha_birth': [29, 31],        # missing for 2005-06 (documentation says territories only)
    'state_res_alpha': [1159, 1161],    # missing for 2005-06 (documentation says territories only)
    'county_res': [1165, 1168],         # missing for 2005-06 (documentation says territories only)
    'state_res_alpha_birth': [108, 110],      # used instead of state_res_alpha in 2005-06
    'county_res_birth': [113, 116],     # used instead of county_res in 2005-06
    'pmsa_res': [1177, 1181],           # 2004 only (filler space otherwise)
    'cmsa_res': [1183, 1185],           # 2004 only (filler space otherwise)
    'sex': [435, 436],
    'age_days': [871, 874],
    'age_recode_5': [874, 875],
    'age_recode_22': [875, 877],
    'mother_age_recode_9': [92, 93],
    'mother_age_recode_14': [90, 92],
    'mother_age_recode_41': [88, 90],
    'mother_age_flag': [87, 88],
    'mother_age_imputation': [86, 87],
    'mother_edu_revised': [154, 155],
    'mother_edu_unrevised': [155, 157],
    'mother_edu_recode': [157, 158],
    'mother_edu_report_flag': [570, 571],
    'cause': [883, 887],
    'cause_recode': [888, 891],
    'manner_of_death': [877, 878],
    'place_injury': [881, 882],
    'place_death': [1185, 1186],
    'number_entities': [902, 904],
    'entity_1': [904, 911],
    'entity_2': [911, 918],
    'entity_3': [918, 925],
    'entity_4': [925, 932],
    'entity_5': [932, 939],
    'entity_6': [939, 946],
    'entity_7': [946, 953],
    'entity_8': [953, 960],
    'entity_9': [960, 967],
    'entity_10': [967, 974],
    'entity_11': [974, 981],
    'entity_12': [981, 988],
    'entity_13': [988, 995],
    'entity_14': [995, 1002],
    'entity_15': [1002, 1009],
    'entity_16': [1009, 1016],
    'entity_17': [1016, 1023],
    'entity_18': [1023, 1030],
    'entity_19': [1030, 1037],
    'entity_20': [1037, 1044],
    'record_weight': [892, 900]    # marked incorrectly in 2004 documentation
}

data_dict_2014_2018 = {
    'year': [1671, 1675],
    'rec_type': [102, 103],
    'res_status': [1634, 1635],         # missing through 2017, despite documentation
    'month_death': [1741, 1743],
    'state_occ_alpha': [1635, 1637],    # missing through 2017 (documentation says territories only)
    'county_occ': [1637, 1640],         # missing through 2017 (documentation says territories only)
    'state_occ_alpha_birth': [23, 25],
    'state_res_alpha': [1643, 1645],    # missing through 2017 (documentation says territories only)
    'county_res': [1649, 1652],         # missing through 2017 (documentation says territories only)
    'state_res_alpha_birth': [88, 90],    # used instead of state_res_alpha through 2017
    'county_res_birth': [90, 93],         # used instead of county_res through 2017
    'sex': [474, 475],
    'age_days': [1355, 1358],
    'age_recode_5': [1358, 1359],
    'age_recode_22': [1359, 1361],
    'mother_age_recode_9': [78, 79],
    'mother_age_recode_14': [76, 78],
    'mother_age_recode_41': [74, 76],
    'mother_age_flag': [73, 74],
    'mother_age_imputation': [72, 73],
    'mother_education': [123, 124],
    'cause': [1367, 1371],
    'cause_recode': [1372, 1375],
    'manner_of_death': [1361, 1362],
    'place_injury': [1365, 1366],
    'place_death': [1669, 1670],
    'number_entities': [1386, 1388],
    'entity_1': [1388, 1395],
    'entity_2': [1395, 1402],
    'entity_3': [1402, 1409],
    'entity_4': [1409, 1416],
    'entity_5': [1416, 1423],
    'entity_6': [1423, 1430],
    'entity_7': [1430, 1437],
    'entity_8': [1437, 1444],
    'entity_9': [1444, 1451],
    'entity_10': [1451, 1458],
    'entity_11': [1458, 1465],
    'entity_12': [1465, 1472],
    'entity_13': [1472, 1479],
    'entity_14': [1479, 1486],
    'entity_15': [1486, 1493],
    'entity_16': [1493, 1500],
    'entity_17': [1500, 1507],
    'entity_18': [1507, 1514],
    'entity_19': [1514, 1521],
    'entity_20': [1521, 1528],
    'record_weight': [1376, 1384]
}


def give_data_dict(year):
	yeardict = {
        (1995, 2003): (1995, 2002),
        (2003, 2004): (2003, 2003),
        (2004, 2014): (2004, 2013),
        (2014, 2019): (2014, 2018)
	}

	yearmap = bd.BetweenDict(yeardict)
	yearvals = yearmap[year]

	dict_str = 'data_dict_%d_%d' %(yearvals[0], yearvals[1])
	data_dict = eval(dict_str)

	return data_dict
