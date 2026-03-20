####################################################################################################
## Description: Compile NHANES measured BMI microdata to fit ensemble distribution weights
## 
## Input: FILEPATH
## 
## Output: Cleaned microdata with columsn:
##        - data (measured BMI)
##        - nid_loc_yr_index
##        - any other columns (age, sex, year, location, nid, race, edu, survey weights, etc.)
##
####################################################################################################

# set up ------------------------------------------------------------------

library(data.table)
nf_repo <- paste0("FILEPATH")
funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

out_root <- "FILEPATH"
run_date <- make_time_stamp()
out_dir <- sprintf("%s%s/", out_root, run_date)
if(!dir.exists(out_dir)) dir.create(out_dir)

# load data ---------------------------------------------------------------

in_root <- "FILEPATH"

original <- readRDS(paste0(in_root, "nhanes_microdata.rds"))

# save the versions of the extracted data used
cat(system(paste0("readlink -f ", in_root), intern = TRUE), " \n", file = paste0(out_dir, "input_data.txt"))

data <- original[, .(NID, age, sex, svyyear, race, race2, edu, mec_wt, bmi)]
setnames(data, c("NID", "bmi", "mec_wt"), c("nid", "data", "svy_wt"))

# make grouped age variable
data[, age2 := ifelse(age >= 50, "50_or_older", "under_50")]
data[, age70 := ifelse(age >= 70, "70_or_older", "under_70")]
data[, age_5 := 5*floor(age/5)]
data[, age_20 := 20*floor(age/20)] # note that this is a different age 20 than that used in the BMI CW
data <- data[age >= 20]

# add US location ID
data[, loc := 102]
data[, source := "nhanes"]

# drop svyyear 2017_2018 because we use 2017_2020prp instead
data <- data[svyyear != "2017_2018"]

data <- data[svyyear %in% c("2011_2012", "2013_2014", "2015_2016", "2017_2020prp")] # subset to year modeling in paper

# drop missing rows missing BMI
data <- data[!is.na(data)]

# drop extreme values of BMI (using GBD rules)
data <- data[data > 10 & data < 70]

# create the index var
data[, nid_loc_yr_index := .GRP, by = .(loc, svyyear,  nid)]
data[, nid_index := .GRP, by = nid] 

# testing making microdata grouped by 4 instead of two years

cols <- c("nid_loc_yr_index", "nid_index")
data[, (cols) := ceiling(.SD/2), .SDcols = cols]

# add database IDs

# save --------------------------------------------------------------------

saveRDS(data, paste0(out_dir, "bmi_microdata.rds"))
