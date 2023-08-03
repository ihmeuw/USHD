#################################################################
# Description: Launcher script to impute data by individual year
#              impute_single_year.R
#################################################################

rm(list = ls())

library(data.table)
library(fs)

ushd_dir <- 'FILEPATH'
repo_dir <- 'FILEPATH'

data_out_dir <- 'FILEPATH'

year_list <- 2000:2019

for(year in year_list){
  worker = code_dir + '/impute_single_year.R'
  sbatch(code = worker, name = 'impute_' + str(year),
  	arguments = [year],
    fthread = 1, m_mem_free = "30G", 
    h_rt = "5:00:00", archive = True,
    submit = True)

}