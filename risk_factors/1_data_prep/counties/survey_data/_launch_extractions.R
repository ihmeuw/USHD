####################################################################################################
## Description: Launch risk factor survey data extractions
##
####################################################################################################

# Set up ------------------------------------------------------------------

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  code_dir <- paste0('FILEPATH', Sys.info()['user'])
} else {
  code_dir <- paste0('FILEPATH')
}

source(paste0(code_dir, "/risk_factors/0_functions/sbatch.R"))

# run this line to loop through all the surveys we extract, or save `svy` as 
#   the name of one of the below surveys to just run the extraction for that specific survey.
surveys <- c("brfss", "nhis", "nhanes", "gallup", "cps")
# Launch extraction scripts ------------------------------------

for(svy in surveys){
  sbatch(code = paste0(code_dir, "FILEPATH"),
         queue = "QUEUE",
         name = paste0("extract", svy),
         fthread = ifelse(svy == "brfss", "6", "2"),
         m_mem_free = ifelse(svy == "brfss", "125G", "25G"),
         h_rt = ifelse(svy == "brfss", "02:30:00", "00:45:00"),
         archive = T,
         sgeoutput = paste0("FILEPATH"),
         sing_image = "FILEPATH")
  print(paste(svy, "job submitted"))
}
