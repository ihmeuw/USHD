rm(list = ls())

library(data.table)
library(openxlsx)
locs <- fread('FILEPATH')

# Define Lower Mississippi Delta as defined here
# https://www.nps.gov/locations/lowermsdeltaregion/counties-within-lmdi.htm
Arkansas <- unique(locs[state_name == 'Arkansas']$mcnty)
Louisiana <- unique(locs[state_name == 'Louisiana']$mcnty)
Mississippi <- unique(locs[state_name == 'Mississippi']$mcnty)

Illinois_strings <- c('Alexander County', 'Franklin County', 
                      'Gallatin County', 'Hamilton County', 'Hardin County', 
                      'Jackson County', 'Johnson County', 'Massac County', 
                      'Perry County', 'Pope County', 'Pulaski County', 
                      'Randolph County', 'Saline County', 'Union County', 
                      'White County', 'Williamson County')

Illinois <- unique(locs[state_name == 'Illinois' & cnty_name %in% Illinois_strings]$mcnty)
stopifnot(length(Illinois) == length(Illinois_strings))

Kentucky_strings <- c('Ballard County', 'Caldwell County', 'Calloway County', 
                      'Carlisle County', 'Christian County', 'Crittenden County', 
                      'Fulton County', 'Graves County', 'Henderson County', 
                      'Hickman County', 'Hopkins County', 'Livingston County', 
                      'Lyon County', 'Marshall County', 'McCracken County', 
                      'McLean County', 'Muhlenberg County', 'Todd County', 
                      'Trigg County', 'Union County', 'Webster County')

Kentucky <- unique(locs[state_name == 'Kentucky' & cnty_name %in% Kentucky_strings]$mcnty)
stopifnot(length(Kentucky) == length(Kentucky_strings))

Missouri_strings <- c('Bollinger County', 'Butler County', 'Cape Girardeau County', #combined Cape and Girardeau counties from nps
                      'Carter County', 'Crawford County', 'Dent County', 'Douglas County', 
                      'Dunklin County', 'Howell County', 'Iron County', 'Madison County', 
                      'Mississippi County', 'New Madrid County', 'Oregon County', 
                      'Ozark County', 'Pemiscot County', 'Perry County', 'Phelps County', 
                      'Reynolds County', 'Ripley County', 'Scott County', 'Shannon County', 
                      'Sainte Genevieve County', 'Saint Francois County', 'Stoddard County', 
                      'Texas County', 'Washington County', 'Wayne County', 'Wright County')

Missouri <- unique(locs[state_name == 'Missouri' & cnty_name %in% Missouri_strings]$mcnty)
stopifnot(length(Missouri) == length(Missouri_strings))

Tennessee_strings <- c('Benton County', 'Carroll County', 'Chester County', 'Crockett County', 
                       'Decatur County', 'Dyer County', 'Fayette County', 'Gibson County', 
                       'Hardeman County', 'Hardin County', 'Haywood County', 'Henderson County', 
                       'Henry County', 'Lake County', 'Lauderdale County', 'Madison County', 
                       'McNairy County', 'Obion County', 'Shelby County', 'Tipton County', 
                       'Weakley County')

Tennessee <- unique(locs[state_name == 'Tennessee' & cnty_name %in% Tennessee_strings]$mcnty)
stopifnot(length(Tennessee) == length(Tennessee_strings))

lmd <- c(Arkansas, Louisiana, Mississippi, Illinois, Kentucky, Missouri, Tennessee)
lmd_locs <- copy(locs)
lmd_locs[,lmd := ifelse(mcnty %in% lmd, 1, 0)]
lmd_locs <- unique(lmd_locs[,c('mcnty','lmd')])

write.csv(lmd_locs, 'FILEPATH/lower_mississippi_delta.csv')

# Define Appalachia as defined here
# https://www.arc.gov/appalachian-counties-served-by-arc/
appalachia <- data.table(read.xlsx('FILEPATH'))
appalachia <- appalachia[4:nrow(appalachia)]
setnames(appalachia, c('fips','state_name','county_name'))
appalachia[,fips := as.numeric(fips)]

appalachia_locs <- copy(locs)
appalachia_locs[,appalachia := ifelse(cnty %in% appalachia$fips, 1, 0)]
appalachia_locs <- appalachia_locs[,c('mcnty','appalachia')]

write.csv(appalachia_locs, 'FILEPATH/appalachia.csv')
