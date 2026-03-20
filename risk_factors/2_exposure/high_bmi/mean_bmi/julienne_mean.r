####################################################################################################
## Description: Create julienned draws in directoris specified by location (mcnties) 
##              to feed into the PAF calculator
##              
##              Uses settings code from launch_risks.R
####################################################################################################

if (!interactive()) {
  (yr <- commandArgs(TRUE)[[1]])
  (r <- commandArgs(TRUE)[[2]])
  (s <- commandArgs(TRUE)[[3]])
  (output_dir_draws_est <- commandArgs(TRUE)[[4]])
  (race_code_set <- commandArgs(TRUE)[[5]])
  (raked <- commandArgs(TRUE)[[6]])
} else {
  yr <- 2009
  r <- 7
  s <- 1
  raked <- TRUE
}

library(data.table)

e <- 1
raked <- ifelse(raked == TRUE, "_raked", "")
message(yr,r,s,raked)
name_mcnty <- paste0("_", yr, "_", s, "_", r, "_", e, raked, ".rds")
draws_dt <- readRDS(paste0(output_dir_draws_est, "/draws/draws_mcnty", name_mcnty))

lapply(0:3109, function(m) {
  message(m)
  draws_sub <- draws_dt[area==m]
  draws_sub[, edu := e]
  dir.create(paste0(output_dir_draws_est, "/draws_mcnty/", m, "/"), recursive = T)
  saveRDS(draws_sub, paste0(output_dir_draws_est, "/draws_mcnty/", m, "/", paste0("draws_", m, name_mcnty)))
})
