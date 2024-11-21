# Script to explore 10 Americas definitions

(list = ls())

library(lbd.loader, lib.loc = sprintf("FILEPATH/lbd.loader-%s.%s", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))
library(data.table)
library(reshape)

OUT_DIR <- 'FILEPATH'

locs <- fread('FILEPATH')

income <- get_covariate_data(covariate_name = 'income_pc_by_race_ethn', year = as.list(2020))
pop_density <- get_covariate_data(covariate_name = 'pop_density', year = as.list(2020))
pop_race_1977 <- get_population_data(population_name = 'pop_by_race_ethn_1977', year = as.list(2020))
pct_PI <- data.table(readRDS('FILEPATH/pct_PI.rds'))
urban_rural <- data.table(readRDS('FILEPATH/urban_rural.rds'))
segragetion_index <- data.table(readRDS('FILEPATH/segragetion_index.rds'))

income <- merge(income,
                pop_race_1977[,.(pop = sum(pop)),.(year,mcnty,race)],
                by = c('year','mcnty','race'),
                all = T)

natl_income <- median(income[,.(income_pc = weighted.mean(income_pc, pop)), .(mcnty)]$income_pc)

lmd <- fread('FILEPATH/lower_mississippi_delta.csv')
appalachia <- fread('FILEPATH/appalachia.csv')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AMERICA 1
# America 1 consists of Asians, excluding those living in counties where Pacific
# Islanders make up more than 40% of the total Asian population. 
# Asians in the latter group of counties were included in America 3.

america_1 <- merge(pop_race_1977[race == 7, c('year','mcnty','sex','age','race')],
                   pct_PI[,c('mcnty','pct_PI')],
                   by = c('mcnty'),
                   all = T)

stopifnot(nrow(america_1[!complete.cases(america_1)]) == 0)

america_1[,america_1 := fcase((pct_PI > .3), 'Excluded',
                              (pct_PI <= .3), 'Included')]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AMERICA 2
# America 2 consists of northland
# (Minnesota, the Dakotas, Iowa, Montana, and Nebraska)
# low-income rural white populations, with income and
# education below the national average.

white_income <- income[race == 5, c('mcnty','income_pc')]

america_2 <- merge(white_income, 
                   unique(locs[,c('mcnty','state_name')]),
                   by = 'mcnty',
                   all.x = T)

america_2 <- merge(america_2,
                   urban_rural,
                   by = 'mcnty',
                   all.x = T)
america_2[,race := 5]

northlands <- c('Minnesota', 'North Dakota', 'South Dakota', 'Iowa', 'Montana', 'Nebraska')
america_2[,america_2 := fcase((!state_name %in% northlands | income_pc > natl_income | urban != 0), 'Excluded',
                              ((state_name %in% northlands) & income_pc < natl_income & urban == 0), 'Included')]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AMERICA 4
# America 4 America 4 consists of low-income white
# populations in Appalachia and the Mississippi Valley

white_income <- income[race == 5, c('mcnty','income_pc')]

america_4 <- merge(white_income, 
                   unique(locs[,c('mcnty','state_name')]),
                   by = 'mcnty',
                   all.x = T)

america_4 <- merge(america_4,
                   urban_rural,
                   by = 'mcnty',
                   all.x = T)

america_4[,race := 5]

delta_appalachia <- c(lmd[lmd == 1]$mcnty, appalachia[appalachia == 1]$mcnty)

america_4[,america_4 := fcase((!mcnty %in% delta_appalachia | income_pc > natl_income), 'Excluded',
                              ((mcnty %in% delta_appalachia) & income_pc < natl_income), 'Included')]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AMERICA 5
# America 5 is made up of Native Americans living 
# in the West, excluding the West Coast.

west_not_wc <- c('Arizona', 'Colorado', 'Idaho', 'Kansas', 
                 'Minnesota', 'Montana', 'Nebraska',
                 'Nevada', 'New Mexico', 'North Dakota', 
                 'Oklahoma', 'South Dakota', 'Utah', 'Wyoming')

america_5 <- unique(locs[,c('mcnty','state_name')])
america_5[,america_5 := ifelse(state_name %in% west_not_wc, 
                               'Included',
                               'Excluded')]

america_5[,race := 6]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# America 7
# America 7 comprises low-income rural black population
# in the Mississippi Valley and the Deep South.

black_income <- income[race == 4, c('mcnty','income_pc')]

america_7 <- merge(black_income, 
                   unique(locs[,c('mcnty','state_name')]),
                   by = 'mcnty',
                   all.x = T)

america_7 <- merge(america_7,
                   urban_rural,
                   by = 'mcnty',
                   all.x = T)


america_7[,race := 4]

deep_south <- c(lmd[lmd == 1]$mcnty, locs[state_name %in% c('Louisiana', 'Mississippi', 'Alabama',
                                                            'Georgia', 'South Carolina')]$mcnty)

america_7[,america_7 := fcase((!mcnty %in% deep_south | income_pc > natl_income | urban != 0), 'Excluded',
                              ((mcnty %in% deep_south) & income_pc < natl_income & urban == 0), 'Included')]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# America 8
# America 8 includes black population living in highly segregated, urban areas

america_8 <- merge(urban_rural, 
                   unique(locs[,c('mcnty','state_name')]),
                   by = 'mcnty',
                   all.x = T)

america_8 <- merge(america_8,
                   segragetion_index,
                   by = 'mcnty',
                   all = T)

america_8[,america_8 := fcase((urban != 1 | segragetion_index < 60), 'Excluded',
                              (urban == 1 & segragetion_index >= 60), 'Included')]

america_8[,race := 4]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# America 9
# America 9 Latino population living in the SW

america_9 <- unique(locs[,c('mcnty','state_name')])

southwest <- c('Arizona', 'New Mexico', 'Colorado', 'Texas')

america_9[,america_9 := fcase((state_name %in% southwest), 'Included',
                              (!state_name %in% southwest), 'Excluded')]

america_9[,race := 2]

america_9$america_9 <- factor(america_9$america_9, levels = c('Included','Excluded'))

# America 10
america_10 <- copy(america_9)
america_10[,america_10 := fcase((america_9 == 'Included'), 'Excluded',
                                (america_9 == 'Excluded'), 'Included')]

america_10[,race := 2]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# America 3
# Leftover counties from Americas 1,2,4,5

america_3_white <- merge(america_2[,c('mcnty','america_2')],
                   america_4[,c('mcnty','america_4')],
                   by = 'mcnty')

america_3_asian <- america_1[,c('mcnty','america_1')]
america_3_aian <- america_5[,c('mcnty','america_5')]

america_3_white[,race := 'White']
america_3_asian[,race := 'Asian']
america_3_aian[,race := 'AIAN']

america_3_white[,america_3 := fcase((america_2 == 'Included' | america_4 == 'Included'), 'Excluded',
                                    (america_2 == 'Excluded' & america_4 == 'Excluded'), 'Included')]
america_3_asian[,america_3 := fcase((america_1 == 'Included'), 'Excluded',
                                    (america_1 == 'Excluded'), 'Included')]
america_3_aian[,america_3 := fcase((america_5 == 'Included'), 'Excluded',
                                    (america_5 == 'Excluded'), 'Included')]

america_3 <- rbindlist(list(america_3_white[,c('mcnty','race','america_3')], 
                            america_3_asian[,c('mcnty','race','america_3')], 
                            america_3_aian[,c('mcnty','race','america_3')]),
                       fill = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# America 6
# The rest of black america

america_6 <- merge(america_7[,c('mcnty','america_7')],
                   america_8[,c('mcnty','america_8')],
                   by = 'mcnty')

america_6[,america_6 := fcase((america_7 == 'Included' | america_8 == 'Included'), 'Excluded',
                              (america_7 == 'Excluded' & america_8 == 'Excluded'), 'Included')]

america_6[,race := 4]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine Americas into one definition CSV

america_3[,race := fcase(race == 'White', 5,
                         race == 'Asian', 7,
                         race == 'AIAN', 6)]
america_1[,america := ifelse(america_1 == 'Included', 1, 0)]
america_2[,america := ifelse(america_2 == 'Included', 2, 0)]
america_3[,america := ifelse(america_3 == 'Included', 3, 0)]
america_4[,america := ifelse(america_4 == 'Included', 4, 0)]
america_5[,america := ifelse(america_5 == 'Included', 5, 0)]
america_6[,america := ifelse(america_6 == 'Included', 6, 0)]
america_7[,america := ifelse(america_7 == 'Included', 7, 0)]
america_8[,america := ifelse(america_8 == 'Included', 8, 0)]
america_9[,america := ifelse(america_9 == 'Included', 9, 0)]
america_10[,america := ifelse(america_10 == 'Included', 10, 0)]

# MAKE AMERICAS CSV
americas <- rbindlist(list(america_1[america != 0,c('mcnty','race','america')], 
                           america_2[america != 0,c('mcnty','race','america')], 
                           america_3[america != 0,c('mcnty','race','america')], 
                           america_4[america != 0,c('mcnty','race','america')], 
                           america_5[america != 0,c('mcnty','race','america')], 
                           america_6[america != 0,c('mcnty','race','america')],
                           america_7[america != 0,c('mcnty','race','america')], 
                           america_8[america != 0,c('mcnty','race','america')], 
                           america_9[america != 0,c('mcnty','race','america')],
                           america_10[america != 0,c('mcnty','race','america')]))

stopifnot(unique(americas$america) == 1:10)
stopifnot(nrow(americas) / 5 == 3110)

write.csv(americas, paste0(OUT_DIR, 'americas_definitions.csv'),
          row.names = F)
