# -----------------------------------------------------------------------------------------------------
# Description: Series of functions to specifically recode any given column of a raw US NVSS file.
# Input values change based on the year being cleaned, and the final output should yield a set of
# variables that are consistently coded across time.
# Full documentation can be found here:
# 1959-1990 available at: http://www.nber.org/data/vital-statistics-mortality-data-multiple-cause-of-death.html
# 1991-2001 available at: http://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm
# Additional documentation available in FILEPATH/PROJECT_FOLDERS/USA/FILEPATH/
#                                       CUSTOM_MORTALITY/DOCUMENTATION
# Input: See individual documentation for each function.
# Output: See individual documentation for each function.
# -----------------------------------------------------------------------------------------------------------

### TERRITORY COUNTY OF RESIDENCE/OCCURENCE ###
# In Northern Marianas(MP, 69) and American Samoa (AS, 60), all counties of residence are coded to 'zero',
# whether the decedent died in-territory or not.  To clear up this confusion, we recode county of residence/occurence in
# these two territories to '998' when the decedent both died and resided in-territory, and keep county of residence/occurence
# as-is if the decedent died elsewhere.

import math

def recode_mp_as(state_occ, state_res, county):
  if state_res in ['MP', 'AS'] and state_res == state_occ:
    return "998"
  else:
    return county


#### HISPANIC DESCENT ####

def recode_hisp_desc(hisp_code):

  # From 1984-1988, there is only an 'origin or descent' variable available (no Hispanic recode).
  # This function uses descent to create a value for Hispanic recode for those years.

  # 1984-1988 'descent' scheme:
  # 00: Non-Spanish
  # 01: Mexican
  # 02: Puerto Rican
  # 03: Cuban
  # 04: Central or South American
  # 05: Other or unknown Spanish
  # 06-24: Other European, African, and Asian ethnicities
  # 88: Not reported
  # 99: Not classifiable

  # Recode to the following Hispanic recode scheme:
  # 1 = Mexican
  # 2 = Puerto Rican
  # 3 = Cuban
  # 4 = Central or South American
  # 5 = Other or unknown Hispanic
  # 6 = Non-Hispanic white
  # 7 = Non-Hispanic black
  # 8 = Non-Hispanic other races
  # 9 = Hispanic origin unknown

  hisp_code = int(hisp_code)

  if hisp_code in list([0, range(6, 25)]):
    recode = 8
  elif hisp_code in list([88, 99]):
    recode = 9
  else:
    recode = hisp_code

  return recode


#### RACE ####

def add_1997_race_label(year, race_code, bridged_flag, recode_40, hisp_recode):
  # This function combines year, race, race bridging flag, race recode 40 scheme, and Hispanic recode
  # to arrive at a value for race group. All possible values for the input variables are listed below,
  # as well as all values of race group the inputs can map to.

  # race:
    # 1980-1988:
    # 00 = Other Asian or Pacific Islander
    # 01 = White
    # 02 = Black
    # 03 = American Indian incl. Aleuts & Eskimos
    # 04 = Chinese
    # 05 = Japanese
    # 06 = Hawaiian incl. Part-Hawaiian
    # 07 = All other races
    # 08 = Filipino

    # 1989-1991:
    # 01 = White
    # 02 = Black
    # 03 = American Indian incl. Aleuts & Eskimos
    # 04 = Chinese
    # 05 = Japanese
    # 06 = Hawaiian incl. Part-Hawaiian
    # 07 = Filipino
    # 08 = Other Asian or Pacific Islander
    # 09 = All other races

    # 1992 onward:
    # 00 = Other Races (Puerto Rico occurrence only)
    # 01 = White
    # 02 = Black
    # 03 = American Indian incl. Aleuts & Eskimos
    # 04 = Chinese
    # 05 = Japanese
    # 06 = Hawaiian incl. Part-Hawaiian
    # 07 = Filipino
    # 08 = Other Asian or Pacific Islander (Virgin Islands, Guam, American Samoa occurrence only)
    # 10 = Guamanian (1994-95 in Guam only)
    # 18 = Asian Indian
    # 28 = Korean
    # 38 = Samoan
    # 48 = Vietnamese
    # 58 = Guamanian
    # 68 = Other Asian or Pacific Islander in areas reporting codes 18-58
    # 78 = Combined other Asian or Pacific Islander, incl. codes 18-68 for areas that do not report them separately

  # hisp_recode:
  # 1 = Mexican
  # 2 = Puerto Rican
  # 3 = Cuban
  # 4 = Central or South American
  # 5 = Other or unknown Hispanic
  # 6 = Non-Hispanic white
  # 7 = Non-Hispanic black
  # 8 = Non-Hispanic other races
  # 9 = Hispanic origin unknown

  # race_bridged:
  # NA/None = Race is not bridged
  # 1 = Race is bridged

  # race_recode_40:
  # 01 = White
  # 02 = Black
  # 03 = American Indian or Alaskan Native (AIAN)
  # 04 = Asian Indian
  # 05 = Chinese
  # 06 = Filipino
  # 07 = Japanese
  # 08 = Korean
  # 09 = Vietnamese
  # 10 = Other or Multiple Asian
  # 11 = Hawaiian
  # 12 = Guamanian
  # 13 = Samoan
  # 14 = Other or Multiple Pacific Islander
  # 15-40 = Specific Multiple Race Combinations
  # 99 = Unknown and Other Race

  # map above variables to the following race groups, where 'NH' = Non-Hispanic:
  # NH White
  # NH Black
  # NH AIAN
  # NH Asian
  # NH NHOPI
  # NH API (pre-2011 only, where not possible to separate Other Asian and Other Pacific Islander)
  # NH Multiracial (where available)
  # NH Other Race (Puerto Rico occurrence only post-1992)
  # Hispanic (supersedes all races)

  race_code = int(race_code)

  # these codes are consistent across all years
  if race_code == 1:
    race_label = 'NH White alone'
  elif race_code == 2:
    race_label = 'NH Black alone'
  elif race_code == 3:
    race_label = 'NH AIAN alone'
  elif race_code in range(4, 6):
    race_label = 'NH Asian alone'
  elif race_code == 6:
    race_label = 'NH NHOPI alone'

  # specific to 1980-88
  if year in range(1980, 1989):
    if race_code == 0:
      race_label = 'NH API'
    elif race_code == 8:
      race_label = 'NH Asian alone'
    elif race_code == 7:
      race_label = 'NH Other Race'

  # specific to 1989-91
  elif year in range(1989, 1992):
    if race_code == 7:
      race_label = 'NH Asian alone'
    elif race_code == 8:
      race_label = 'NH API'
    elif race_code == 9:
      race_label = 'NH Other Race'

  # specific to 1992 onward
  elif year >= 1992:
    if race_code == 0:
      race_label = 'NH Other Race'
    elif race_code in list([7, 18, 28, 48]):
      race_label = 'NH Asian alone'
    elif race_code in list([10, 38, 58]):
      race_label = 'NH NHOPI alone'
    elif race_code in list([8, 68]):
      race_label = 'NH API'
    elif race_code == 78:
      race_label = 'NH Multiracial'

  # for 2003 onward (empty column otherwise)
  if bridged_flag is not None:
    bridged_flag = float(bridged_flag)
    if not math.isnan(bridged_flag):
      bridged_flag = int(bridged_flag)
      if bridged_flag == 1:
        race_label = 'NH Multiracial'

  # for 2011 onward (empty column otherwise)
  if recode_40 is not None:
    recode_40 = int(recode_40)
    if recode_40 == 10:
      race_label = 'NH Asian alone'
    elif recode_40 == 14:
      race_label = 'NH NHOPI alone'
    elif recode_40 in range(15, 41):
      race_label = 'NH Multiracial'

  # allow Hispanic ethnicity to supersede race
  if hisp_recode is not None:
    hisp_recode = int(hisp_recode)
    if hisp_recode <= 5:
      race_label = 'Hispanic'

  return race_label

def add_1977_race_label(state_res_code, race_code, race_label):
    # Used for simplicity to produce quicker estimates. Most of the time the more detailed groups coded
    # above can be coded directly to these condensed groups. In the case of NH Multiracial, we use
    # the value of the original race code to assign a group.

    # 1977 groupings:
    # NH (Non-Hispanic) White
    # NH Black
    # NH AIAN (American Indian or Alaskan Native)
    # NH API (Asian or Pacific Islander)
    # Hispanic
    # NH Other Race

    race_code = int(race_code)

    # Leave blank for territories
    if state_res_code in ['PR', 'VI', 'GU', 'AS', 'MP']:
        recode = None
    else:
        if race_label in ['NH White alone', 'NH Black alone', 'NH AIAN alone']:
            recode = race_label.replace(' alone', '')
        elif race_label in ['NH Other Race', 'Hispanic']:
            recode = race_label
        elif race_label in ['NH NHOPI alone', 'NH API', 'NH Asian alone']:
            recode = 'NH API'
        elif race_label == 'NH Multiracial':
            if race_code == 0:
                print 'Warning: race code of 0 detected, which indicates a Puerto Rico occurrence. Recoded to' \
                      'NH Other Race.'
                recode = 'NH Other Race'
            elif race_code == 1:
                recode = 'NH White'
            elif race_code == 2:
                recode = 'NH Black'
            elif race_code == 3:
                recode = 'NH AIAN'
            elif race_code in range(4, 9) + [10, 18, 28, 38, 48, 58, 68, 78]:
                recode = 'NH API'
            else:
                print 'FILEPATH'
        else:
            print 'FILEPATH'
            recode = "NH Other Race"

    return recode

def recode_race_hisp(race_label):
  # We need to retain four categories of race/hispanic variables. This will be
  # done using the race group variable created above, which contains all relevant race information.
  # The following four groups are a broader generalization of the groups used:
  # 1: White non-Hispanic
  # 2: Black non-Hispanic
  # 3: Other non-Hispanic
  # 4: Hispanic
  # 9: Unknown

  if race_label == 'Hispanic':
    recode = 4
  elif race_label in ['NH White', 'NH White alone']:
    recode = 1
  elif race_label in ['NH Black', 'NH Black alone']:
    recode = 2
  elif race_label in ['NH AIAN', 'NH AIAN alone', 'NH Asian', 'NH Asian alone', 'NH NHOPI', 'NH NHOPI alone',
                      'NH API', 'NH Multiracial', 'NH Other Race']:
    recode = 3
  else:
    print 'Unrecognized race group %s' % race_label
    recode = 9
  return recode


#### EDUCATION ####

def recode_education(education, flag):
  # From 1989 to 2002, education was coded based on number of years of
  # elementary school, high school, or college, then recoded into binned
  # numbers of years.  Starting in 2003, some states kept the 1989 (non-recoded)
  # system, while others switched to a coding method that specifically spelled
  # out what grade was completed or degree obtained.  Since neither of these
  # systems can be directly mapped onto each other, we make a new mapping scheme
  # that primarily preserves the 2003 format, but does not distinguish between
  # different graduate degrees. Maps of all three schema are below.

  # For the 2003-onward data, the schema used (1989, 2003, or none) is identified using
  # an education reporting flag:
  # 0: 1989 revision on certificate
  # 1: 2003 revision on certificate
  # 2: no education item on certificate

  # In the main code, we assign the education variable to '9' ('Unknown') if the flag equals 2.
  # Here, we use flag information to determine which mapping to perform.

  # 1989 Version (used exclusively 1989-2002):
  # 00: No formal education
  # 01-08: Years of elementary school
  # 09: 1 year of high school
  # 10: 2 years of high school
  # 11: 3 years of high school
  # 12: 4 years of high school
  # 13: 1 year of college
  # 14: 2 years of college
  # 15: 3 years of college
  # 16: 4 years of college
  # 17: 5 or more years of college
  # 99: not stated

  # 2003 Version (used in conjunction with the 1989 version, 2003-2018):
  # 1: 8th grade or less
  # 2: 9-12th grade, no diploma
  # 3: High school graduate or GED completed
  # 4: Some college credit, no degree
  # 5: Associate degree
  # 6: Bachelor's degree
  # 7: Master's
  # 8: Doctorate/Professional
  # 9: Unknown

  # Our hybrid system:
  # 1: 8th grade or less
  # 2: 9-12th grade, no diploma
  # 3: High school graduate or GED completed
  # 4: Some college credit, no bachelor's degree (includes associate's degree)
  # 5: Bachelor's degree and higher
  # 9: Unknown

  # sometimes education is NaN or None and won't convert to int, so we set missing values manually
  if isinstance(education, float) or education is None:
    if flag in [0, 2]:
      education = 99
    else:
      education = 9

  education = int(education)
  flag = int(flag)

  # 1989 version flag
  if flag == 0:
    if education < 9:
      recode = 1
    elif education in range(9, 12):
      recode = 2
    elif education == 12:
      recode = 3
    elif education in range(13, 16):
      recode = 4
    elif education in range(16, 18):
      recode = 5
    elif education == 99:
      recode = 9
    else:
      print 'Unrecognized education code %d' % education

  # 2003 version flag
  elif flag == 1:
    if education < 5 or education == 9:
      recode = education
    elif education == 5:
      recode = 4
    elif education in range(6, 9):
      recode = 5
    else:
      print 'Unrecognized education code %d' % education

  # no education information available
  elif flag == 2:
    recode = 9

  else:
    print 'Unrecognized education flag %s' % flag

  return recode


#### SEX ####

def recode_sex(sex_code):
  # in 2003, sex started being coded as an alphabetic variable (M/F) rather than a numeric one (1/2).
  # We recode these years back to the numeric variable.

  sex_dict = {'M': 1,
              '1': 1,
              'F': 2,
              '2': 2}

  return sex_dict[sex_code]


#### AGE ####

def recode_age(year, age_code):
  # From 1985 to 2002, detail age was coded with a three-digit variable, the first
  # of which specified broad time categories (years, months, days, etc.), and the
  # second of which specified specific times within that category.  A similar system,
  # with slightly different categories and using four digits rather than three, was
  # implemented beginning in 2003.  We map both of these schema to the GBD age list.

  # 1985-2002:
  # 0 01-99:  Years less than 100
  # 1 00-99:  Years 100+
  # 2 01-11,99:	 Months
  # 3 01-03, 99:  Weeks
  # 4 01-27, 99:  Days
  # 5 01-23, 99:  Hours
  # 6 01-59, 99: Minutes
  # 9 99  Age Not Stated

  # 2003-2018:
  # 1 001-135, 999:  Years
  # 2 001-011, 999:  Months
  # 4 001-027, 999:  Days
  # 5 001-023, 999:  Hours
  # 6 001-059, 999:  Minutes
  # 9 999: Age Not Stated

  # GBD bins:
  # 0d: 0-6 days
  # 7d: 7-27 days
  # 1-5mo : 1-5 months
  # 6-11mo : 6-11 months
  # 12-23mo : 12-23 months
  # 2-4 : 2-4 years
  # 5: 5-9 years
  # 10: 10-14 years
  # 15: 15-19 years
  # 20: 20-24 years
  # 25: 25-29 years
  # 30: 30-34 years
  # 35: 35-39 years
  # 40: 40-44 years
  # 45: 45-49 years
  # 50: 50-54 years
  # 55: 55-59 years
  # 60: 60-64 years
  # 65: 65-69 years
  # 70: 70-74 years
  # 75: 75-79 years
  # 80: 80-84 years
  # 85: 85-89 years
  # 90: 90-94 years
  # 95: 95+ years
  # 999: Unknown

  category = int(age_code[0])
  detail = int(age_code[1:])

  if category == 9:
    recode = '999'
  elif category in range(5, 7):
    recode = '0d'
  elif category == 4:
    if detail < 7:
      recode = '0d'
    elif detail <= 27:
      recode = '7d'
    else:
      recode = '999i'  # i for infant - need to track for replacing with linked birth infant death data
  elif category == 3:
    recode = '7d'
  elif category == 2:
    if detail < 6:
      recode = "1-5mo"
    elif detail <= 11:
      recode = "6-11mo"
    else:
      recode = "999i"  # i for infant - need to track for replacing with linked birth infant death data
  elif category == 1 and year <= 2002:
    recode = '95'
  else:  # will only get to this point if 'category' is 0 (for pre-2003) or 1 (for 2003 on).
    if detail == 1:
      recode = "12-23mo"
    elif detail <= 4:
      recode = "2-4"
    elif detail == 999:
      recode = 999
    elif detail >= 95:
      recode = '95'
    else:
      for binstart in range(5, 95, 5):
        if detail in range(binstart, binstart + 5):
          recode = str(binstart)
          break

  return recode

def recode_infant_age(year, age_var):
  # In the linked birth infant death files, we are given infant age in days. In 2003-2017, we use
  # age_recode_22, which contains an age breakdown into hours, days, and months. This function recodes
  # that age information to GBD standard infant ages. For infant ages in years without age_recode_22
  # (1995-2002), we estimate that 28-182 days = 1-5 months, and 183-365 days = 6-11 months.

  # 1995-2002:
  # 000-365: age in days

  # 2003-2017:
  # BlanFILEPATH Age 1 year and over or not stated
  # 01: Under 1 hour
  # 02: 1-23 hours
  # 03: 1 day
  # 04: 2 days
  # 05: 3 days
  # 06: 4 days
  # 07: 5 days
  # 08: 6 days
  # 09: 7 days
  # 10: 14-20 days
  # 11: 21-27 days
  # 12: 1 month
  # 13: 2 months
  # 14: 3 months
  # 15: 4 months
  # 16: 5 months
  # 17: 6 months
  # 18: 7 months
  # 19: 8 months
  # 20: 9 months
  # 21: 10 months
  # 22: 11 months

  # GBD bins:
  # 0d: 0-6 days
  # 7d: 7-27 days
  # 1-5mo : 1-5 months
  # 6-11mo : 6-11 months
  # 999: Unknown

  # sometimes age is NaN and won't convert to int, so we set missing values manually
  if isinstance(age_var, float):
    age_var = 99
  else:
    age_var = int(age_var)

  if year in range(1995, 2003):
    if age_var <= 6:
      recode = '0d'
    elif 7 <= age_var <= 27:
      recode = '7d'
    elif 28 <= age_var <= 182:
      recode = '1-5mo'
    elif 183 <= age_var <= 365:
      recode = '6-11mo'
    else:
      recode = '999i'
  else:
    if age_var <= 8:
      recode = '0d'
    elif 7 <= age_var <= 11:
      recode = '7d'
    elif 12 <= age_var <= 16:
      recode = '1-5mo'
    elif 17 <= age_var <= 22:
      recode = '6-11mo'
    else:
      recode = '999i'

  return recode

def recode_mother_age(age_var):
  # We use mother's age as an additional variable in determining a value for
  # infant 'education' based on mother's educational attainment. We need to
  # consider younger mothers who haven't necessarily completed their education.
  # This function recodes mother's age to be consistent across all years of data
  # and compliant with GBD age bins.

  # 1995-2017 (mother_age_recode_9 in births and linked births files)
  # 1: Under 15 years
  # 2: 15-19 years
  # 3: 20-24 years
  # 4: 25-29 years
  # 5: 30-34 years
  # 6: 35-39 years
  # 7: 40-44 years
  # 8: 45-49 years
  # 9: 50-54 years

  # GBD bins:
  # 10: 10-14 years
  # 15: 15-19 years
  # 20: 20-24 years
  # 25: 25-29 years
  # 30: 30-34 years
  # 35: 35-39 years
  # 40: 40-44 years
  # 45: 45-49 years
  # 50: 50-54 years
  # 55: 55-59 years
  # 999: Unknown

  # sometimes age is NaN and won't convert to int, so we set missing values manually
  if isinstance(age_var, float):
    age_var = 99
  else:
    age_var = int(age_var)

  if age_var in range(1, 10):
    recode = str(1 + age_var + 4*(age_var + 1))
  elif age_var < 1 or age_var > 10:
    print "FILEPATH"
    recode = '999'
  else:
    recode = '999'

  return recode

#### INDUSTRY ####

def recode_industry(ind_code):

  # Industry and Occupation were both coded only from 1985-1999.
  # Industry (defined as the 'type of activity at a person's place of work') and Occupation
  # (defined as 'kind of work a person does to earn a living) are given three-digit codes as
  # defined by the Census Bureau. The coding system changed slightly in 1992. From 1985 to 1991,
  # these three-digit codes were recoded to a condensed two-digit codee list (51 entries long
  # for Industry, 59 for Occupation), but this recoding was not done from 1992 to 1999. Here,
  # for 1992-1999, we recode the given Industry code to the two-digit recodes defined for 1985-1991.
  # Recoding was assigned using documentation available here:

  # http://www.bls.gov/nls/quex/r1/y97r1cbka1.pdf

  # in conjunction with the recode list (copied below, available in the mortality documentation for
  # the 80's at 'FILEPATH').  Additional verification was performed using the full 1980 code
  # list:

  # http://books.google.com/books?id=XCSomxzjVNIC&pg=PR3&lpg=PR3&dq=1980+census+of+population+classified+index+of+industries+and+occupations&source=bl&ots=uwYv4A7m2B&sig=gjQTtWwa1xoKXbeu8QfVHHWH_yI&hl=en&sa=X&ei=orLBUrWlLsaGoQSRnoGgCA&ved=0CGQQ6AEwBg#v=onepage&q=1980%20census%20of%20population%20classified%20index%20of%20industries%20and%20occupations&f=false

  # and the full 1990 code list:

  # http://books.google.com/books?id=2tFvFuJdBhwC&pg=PR5&dq=1990+census+of+population+classified+index+of+industries+and+occupations&hl=en&sa=X&ei=EBLDUp-tIcTtoATiy4CwCg&ved=0CC0Q6AEwAA#v=onepage&q=1990%20census%20of%20population%20classified%20index%20of%20industries%20and%20occupations&f=false

  # and finally, for Industry ONLY, a direct 1993-to-1985 code crosswalk is available at:

  # http://www.census.gov/people/io/files/1990%20Census_SIC%20codes.pdf

  # 'Business or Industry Recode 51'

  # 01 	Agriculture, Forestry, and Fisheries
  # 02 	Mining
  # 03 	Construction
  # 04 	Manufacturing*
  # 05 		Nondurable goods*
  # 06 			Food and kindred products
  # 07 			Textile mill and finished products
  # 08 			Paper and allied products
  # 09 			Printing, publishing and allied products
  # 10 			Chemicals and allied products
  # 11 			Petroleum and coal products
  # 12 			Rubber, Plastics, and leather products
  # 13 		Durable goods*
  # 14 			Lumber and other wood products, and furniture
  # 15 			Stone, clay, glass and concrete products
  # 16 			Primary metal industries
  # 17 			Fabricated metal industries
  # 18 			Machinery, except electrical
  # 19 			Electrical machinery, equipment, and supplies
  # 20 			Transportation equipment
  # 21 			Miscellaneous manufacturing industries
  # 22 	Transportation, communications, and other public utilities*
  # 23 		Transportation*
  # 24 			Railroads
  # 25 			Trucking and warehousing
  # 26 			Other transportation
  # 27 		Communications
  # 28 		Utilities and sanitary services
  # 29 	Wholesale trade
  # 30 	Retail trade*
  # 31 		Food, bakery, and dairy stores
  # 32 		Auto dealers and supply stores
  # 33 		Eating and drinking places
  # 34 		Other retail trade
  # 35 	Finance, insurance, and real estate
  # 36 	Business and repair services*
  # 37 		Automative services and repair
  # 38 		Other business and repair services
  # 39 	Personal services*
  # 40 		Private households
  # 41 		Beauty and barber shops
  # 42 		Other personal services
  # 43 	Entertainment and recreation services
  # 44 	Professional and related services*
  # 45 		Health services
  # 46 		Educational services
  # 47 		Social services
  # 48 		Legal, engineering, and other services
  # 49 	Public administration
  # 50 	Military
  # 51 	Industry unknown or not reported
  ind_code = int(ind_code)

  if ind_code in range(10, 33):
    recode = '01'
  elif ind_code in range(40, 51):
    recode = '02'
  elif ind_code == 60:
    recode = '03'
  elif ind_code in range(100, 131):
    recode = '06'
  elif ind_code in range(132, 153):
    recode = '07'
  elif ind_code in range(160, 163):
    recode = '08'
  elif ind_code in range(171, 173):
    recode = '09'
  elif ind_code in range(180, 193):
    recode = '10'
  elif ind_code in range(200, 202):
    recode = '11'
  elif ind_code in range(210, 223):
    recode = '12'
  elif ind_code in range(230, 243):
    recode = '14'
  elif ind_code in range(250, 263):
    recode = '15'
  elif ind_code in range(270, 281):
    recode = '16'
  elif ind_code in range(281, 302):
    recode = '17'
  elif ind_code in range(310, 333):
    recode = '18'
  elif ind_code in range(340, 351):
    recode = '19'
  elif ind_code in range(351, 371):
    recode = '20'
  elif ind_code in range(371, 393):
    recode = '21'
  elif ind_code == 400:
    recode = '24'
  elif ind_code in range(410, 412):
    recode = '25'
  elif ind_code in range(401, 433):
    recode = '26'
  elif ind_code in range(440, 443):
    recode = '27'
  elif ind_code in range(450, 473):
    recode = '28'
  elif ind_code in range(500, 572):
    recode = '29'
  elif ind_code in range(601, 612):
    recode = '31'
  elif ind_code in range(612, 620):
    recode = '32'
  elif ind_code == 641:
    recode = '33'
  elif ind_code in range(580, 692):
    recode = '34'
  elif ind_code in range(700, 713):
    recode = '35'
  elif ind_code in range(742, 752):
    recode = '37'
  elif ind_code in range(721, 761):
    recode = '38'
  elif ind_code == 761:
    recode = '40'
  elif ind_code in range(772, 781):
    recode = '41'
  elif ind_code in range(762, 792):
    recode = '42'
  elif ind_code in range(800, 811):
    recode = '43'
  elif ind_code in range(812, 841):
    recode = '45'
  elif ind_code in range(842, 861):
    recode = '46'
  elif ind_code in range(861, 882):
    recode = '47'
  elif ind_code in range(841, 894):
    recode = '48'
  elif ind_code in range(900, 933):
    recode = '49'
  elif ind_code in range(940, 961) or ind_code == 991:
    recode = '50'
  elif ind_code in range(961, 993):
    recode = '51'
  else:
    #recode unknowns to 999 to fit database schema
    recode = '999'
    print 'Unknown industry code %d!!' % ind_code

  return recode


#### OCCUPATION ####

def recode_occupation(occ_code):

  # see 'recode_industry' for full explanation and documentation sources.  List of
  # occupation recodes is reproduced below.

  # 'Usual Occupation Recode 59'

  # 01 	Executive, administrative, and managerial occupations*
  # 02 		Executive and administrative occupations
  # 03 		Management related occupations
  # 04 	Professional specialty occupations*
  # 05 		Architects, engineers, and scientists
  # 06 		Health diagnosis and treatment occupations
  # 07 		Teachers
  # 08 		Other professional specialty occupations
  # 09 	Technicians and related support
  # 10 	Sales occupations
  # 11 	Administrative support occupations, including clerical*
  # 12 		Secretaries, stenographers, and typists
  # 13 		Records processing occupations
  # 14 		Mail and message distributing occupations
  # 15 		Other administrative support occupations
  # 16 	Service occupations*
  # 17 		Private household occupations
  # 18 		Protective service occupations
  # 19 		Food preparation and service occupations
  # 20 		Health service occupations
  # 21 		Cleaning and building service occupations
  # 22 		Personal service occupations
  # 23 	Farming, forestry, and fishing occupations*
  # 24 		Farm and other agricultural occupations
  # 25 		Forestry, fishing, and hunting occupations
  # 26 	Precision production, craft and repair occupations*
  # 27 		Mechanics and repairers*
  # 28 			Vehicle and mobile equipment mechanics and repairers
  # 29 			Other mechanics and repairers
  # 30 		Construction trades*
  # 31 			Carpenters and apprentices
  # 32 			Electricians, apprentices, and electrical power installers and repairers
  # 33 			Painters, construction and maintenance
  # 34 			Other construction trades
  # 35 		Extractive occupations
  # 36 		Precision production occupations*
  # 37 			Supervisors, production occupations
  # 38  		Precision metal and wood working operations
  # 39 			Precision textile, apparel, and furnishings machine workers
  # 40 			Precision food production occupations
  # 41 			Other precision production occupations
  # 42 	Machine operators, assemblers, and inspectors*
  # 43 		Machine operators and tenders, except precision*
  # 44 			Metal, plastic, and woodworking machine operators
  # 45 			Printing machine operators
  # 46 			Textile, apparel, and furnishing machine operators
  # 47 			Machine operators, assorted materials
  # 48 		Fabricators, assemblers, and hand working occupations
  # 49 		Production inspectors, testers, samplers, and weighers
  # 50 	Transportation and material moving occupations*
  # 51 		Motor vehicle operators
  # 52 		Other transportation occupations
  # 53 	Handlers, equipment cleaners, helpers, and laborers*
  # 54 		Construction laborers
  # 55 		Laborers, except construction
  # 56 		Other handlers, cleaners, and laborers
  # 57 	Military
  # 58 	Homemaker
  # 59 	Occupation unknown or not reported

  occ_code = int(occ_code)

  if occ_code in range(3, 23):
    recode = '02'
  elif occ_code in range(23, 38):
    recode = '03'
  elif occ_code in range(43, 84):
    recode = '05'
  elif occ_code in range(84, 107):
    recode = '06'
  elif occ_code in range(113, 164):
    recode = '07'
  elif occ_code in range(164, 200):
    recode = '08'
  elif occ_code in range(203, 236):
    recode = '09'
  elif occ_code in range(243, 286):
    recode = '10'
  elif occ_code in range(313, 316):
    recode = '12'
  elif occ_code in range(325, 345):
    recode = '13'
  elif occ_code in range(354, 358):
    recode = '14'
  elif occ_code in range(303, 390):
    recode = '15'
  elif occ_code in range(403, 408):
    recode = '17'
  elif occ_code in range(413, 428):
    recode = '18'
  elif occ_code in range(433, 445):
    recode = '19'
  elif occ_code in range(445, 448):
    recode = '20'
  elif occ_code in range(448, 456):
    recode = '21'
  elif occ_code in range(456, 470):
    recode = '22'
  elif occ_code in range(473, 490):
    recode = '24'
  elif occ_code in range(494, 500):
    recode = '25'
  elif occ_code in range(505, 520):
    recode = '28'
  elif occ_code in range(503, 550):
    recode = '29'
  elif occ_code in range(567, 570):
    recode = '31'
  elif occ_code in range(575, 578):
    recode = '32'
  elif occ_code == 579:
    recode = '33'
  elif occ_code in range(553, 600):
    recode = '34'
  elif occ_code in range(613, 618):
    recode = '35'
  elif occ_code == 628:
    recode = '37'
  elif occ_code in range(634, 660):
    recode = '38'
  elif occ_code in range(666, 675):
    recode = '39'
  elif occ_code in range(686, 689):
    recode = '40'
  elif occ_code in range(675, 700):
    recode = '41'
  elif occ_code in range(703, 734):
    recode = '44'
  elif occ_code in range(734, 738):
    recode = '45'
  elif occ_code in range(738, 750):
    recode = '46'
  elif occ_code in range(753, 780):
    recode = '47'
  elif occ_code in range(783, 796):
    recode = '48'
  elif occ_code in range(796, 800):
    recode = '49'
  elif occ_code in range(803, 815):
    recode = '51'
  elif occ_code in range(823, 860):
    recode = '52'
  elif occ_code == 869:
    recode = '54'
  elif occ_code == 889:
    recode = '55'
  elif occ_code in range(864, 889):
    recode = '56'
  elif occ_code in range(903, 906):
    recode = '57'
  elif occ_code == 914:
    recode = '58'
  elif occ_code in range(913, 918) or occ_code == 999:
    recode = '59'
  else:
    #recode unknowns to 999 to fit database schema
    recode = '999'
    print 'Unknown occupation code %d!!' % occ_code

  return recode
