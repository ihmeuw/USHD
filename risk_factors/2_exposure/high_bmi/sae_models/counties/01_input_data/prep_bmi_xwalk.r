####################################################################################################
## Description: Prep NHANES, BRFSS, NHIS, and Gallup data for BMI crosswalk + SAE models
##
##              Create three main outputs:
##              1) Microdata standardized for BMI crosswalk
##              2) Paired measures of mean BMI across studies (incorporating survey weights)
##              3) Plots of BMI data
##              
##              **NOTE** We extract variables for use in the FPG, LDL-c, and SBP 
##                crosswalks, in addition to the BMI cw. We extract those variables
##                before applying BMI cw in order to keep respondents responses
##                together on a single line. This script removes rows with missing
##                BMI values, which may be problematic (but probably not)
##
## Inputs:  Extracted survey data 
##            FILEPATH
##            FILEPATH
##            FILEPATH
##            FILEPATH
##
## Output:  FILEPATH
##            extraction_versions.txt (versions of survey data used)
##            git_info.txt
##            nhanes_mec_survey_object.rds (object of class survey.design containing
##              NHANES data that specifies complex survey design)
##            prepped_bmi_xwalk_micro.rds (BMI microdata to be used in crosswalk models.
##              Respondents with missing data removed)
##            prepped_bmi_xwalk_micro_incl_missing.rds (BMI microdata including missing data)
##            prepped_bmi_xwalk_edu_summary.rds (weighted mean BMI by age/sex/svyyear/edu)
##            prepped_bmi_xwalk_race_summary.rds (weighted mean BMI by age/sex/svyyear/race)
##            unadjusted_bmi_dat.pdf (plots of BMI data)
##            
##            (note that the prepped data in 01_input_data comes from the final BMI cw model in the correction_models sub directory)
##          
####################################################################################################

###########################################################################
# Set up ------------------------------------------------------------------
###########################################################################

library(data.table)
library(forcats)
library(survey)

if (dir.exists(paste0('FILEPATH', Sys.info()['user']))) {
  repo_dir <- paste0('FILEPATH', Sys.info()['user'], '/risk_factors/')
} else {
  repo_dir <- paste0('FILEPATH', Sys.info()['user'], '/us_counties/risk_factors/')
}

source(paste0(repo_dir, "0_functions/load_ushd_db.R"))  # load USHD database
source(paste0(repo_dir, '0_functions/helper/_versioning_functions.R')) # includes make_time_stamp() + get_git_status()
source(paste0(repo_dir, '2_exposure/high_bmi/correction_models/viz_bmi.r'))

root_J <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
root_LU <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
out_dir_main <- paste0(root_LU, "FILEPATH")

# retrieve metadata for best versions of survey extractions
# if a different version is desired, you can change to get_best = F to see available version ids,
# then specify a version id with survey_extraction_version_id = <id>
nhanes_meta <- get_survey_extraction(input_type = "nhanes", get_best = T)
brfss_meta <- get_survey_extraction(input_type = "brfss", get_best = T)
gallup_meta <- get_survey_extraction(input_type = "gallup", get_best = T)

nhanes_path <- as.character(unique(nhanes_meta[, output_file_path]))
brfss_path <- as.character(unique(brfss_meta[, output_file_path]))
gallup_path <- as.character(unique(gallup_meta[, output_file_path]))

# Generate run date and create model output folder
run_date <- make_time_stamp()
message(run_date)
output_dir <- paste0(out_dir_main, run_date, "/")
message(output_dir)
dir.create(output_dir)
# Save git history to output_dir
cat(get_git_status(repo = repo_dir, "Risk Factor Repo", show_diff = T), file = paste0(output_dir, "git_info.txt"), sep = "\n")

# save the versions of the extracted data used
cat("Prepped crosswalk data based on the following extraction versions: ", nhanes_path, brfss_path, gallup_path,
    sep = " \n ", file = paste0(output_dir, "extraction_versions.txt"))

################################################################################
# Load input data and pare down to relevant variables --------------------------
################################################################################

# NHANES ------------------------------------------------------------------

nhanes <- readRDS(paste0(nhanes_path, "nhanes_microdata.rds"))
nhanes <- nhanes[, .(svyyear, sex, age, race, race2, edu, preg, marital,  # demographics
                     mec_wt, int_wt, psu_pseudo, stra_pseudo, NID,# survey design
                     bmi, weight, height, weight_report, height_report, bmi_report, # bmi-related
                     diq, predb, riskdb, db_insulin, db_pill, # diabetes-related variables
                     lbxglu, mec_fast_wt, lbxgh, # FPG/A1c biomarker data
                     smoke_any, smoke, anydrnk, hlthplan, # covariates (probably won't use all of these)
                     lbxtc, lbxtr, lbdldl, cholcheck, cholmeds, lastcholcheck, highchol, # cholesterol
                     highbp, bpmeds, bpsys, bpdias, bposys, bpodias   # hypertension
)] 
setnames(nhanes, c("age", "diq"), c("age_continuous", "diabetes"))

# create a common race variable we can use across years
nhanes[!is.na(race), bridged_race := race]
nhanes[!is.na(race2), bridged_race := race2]
nhanes[bridged_race == "other" | bridged_race == "NH Asian Only", 
       bridged_race := "residual"] # create a residual category for NHANES; that is Asian, NHOPI, AIAN, MR, and other
nhanes[, bridged_race := droplevels(bridged_race)]
nhanes[, survey := "nhanes"]

# there are a couple of cases where self-reported weight and measured weight are extremely 
#   different -- to the extent that I believe there might have been confusions b/w
#   pounds and kilograms (these instances are rare enough that I believe it is on
#   the NHANES side, and not our extractions). I will remove cases where self-reported
#   and measured weight differ by more than 50%:
nhanes <- nhanes[is.na(weight_report) | is.na(weight) | !(weight/weight_report > 1.5 | weight_report/weight > 1.5)]


# BRFSS -------------------------------------------------------------------

brfss <- readRDS(paste0(brfss_path, "brfss_microdata.rds"))
brfss <- brfss[, .(year, month, id, age_continuous, sex, race77, race97, edu, preg, marital, state, cnty, mcnty, cbsa,
                   wt, design_wt, ststr, phone, seqno, nid, 
                   diabetes, predb, db_insulin, db_pill,
                   smoke_any, smoke, anydrnk, hlthplan, # covariates
                   evercheckchol, lastcholcheck, highchol, # cholesterold
                   highbp, bpmeds, # hypertension
                   height, weight)]
setnames(brfss, c("height", "weight", "evercheckchol", "id"), c("height_report", "weight_report", "cholcheck", "brfss_index"))
brfss[, bmi_report := weight_report/height_report^2]
# create race variables that match NHANES

brfss[year < 2001, bridged_race := race77] # switched to 1997 categories in 2001; 1997 categories match NHANES better
brfss[year >= 2001, bridged_race := race97]
brfss[(!bridged_race %in% c("NH Black", "NH White", "Hispanic", "NH White Only", "NH Black Only")) & !is.na(bridged_race), 
      bridged_race := "residual"] # collapse races that don't match NHANES categories into the residual race -- years before NHANES had option for Asian
brfss[, survey := "brfss"]
brfss[, bridged_race := droplevels(bridged_race)]

# Gallup ------------------------------------------------------------------

gallup <- readRDS(paste0(gallup_path, "/gallup_microdata_race97.rds")) # use 1997 OMB standards data b/c it matches the NHANES data better

gallup <- gallup[year <= 2017] # DECIDED 4/7/22 to crosswalk all Gallup years (2008-2017) to NHANES, even though there isn't a perfect map with svyyears. Still not using 2018 and 2019 b/c of issues with their sample
gallup <- gallup[, .(year, race97, educ, sex, age, marital,
                     wt_nalt_wb, wt_state_wb, sample, mcnty, state, census_region, time_region_phone, msacode, idate, nid, # extract national wb (well-being weights)
                     height, weight,
                     diabetes, 
                     uid,
                     smoker, hlthplan,  # covariates
                     highchol, treated_chol, # cholesterold
                     highbp, treated_bp # hypertension
                     )] 
setnames(gallup, c("height", "weight", "age", "educ", "wt_nalt_wb", "sample", "uid"), c("height_report", "weight_report", "age_continuous", "edu", "wt", "phone", "gallup_uid"))
setnames(gallup, c("smoker", "treated_bp"), c("smoke_any", "bpmeds"))
gallup[, height_report := height_report / 100] # convert from cm to meters
gallup[, bmi_report := weight_report/height_report^2]

# create race vars that match NHANES

gallup[, bridged_race := race97]
gallup[(!bridged_race %in% c("NH Black", "NH White", "Hispanic", "NH White Only", "NH Black Only")) & !is.na(bridged_race), 
      bridged_race := "residual"] # collapse races that don't match NHANES categoires into the residual race -- years before NHANES had option for Asian
gallup[, bridged_race := droplevels(bridged_race)]
gallup[, survey := "gallup"]


###########################################################################
# Common adjustments/standardization --------------------------------------
###########################################################################

# Combine data
prep_cw_dat <- rbind(nhanes, brfss, gallup, fill = T, use.names = TRUE)

# restrict to NHANES years
prep_cw_dat <- prep_cw_dat[year %in% 1999:2020 | survey == "nhanes"]

# create age bins
prep_cw_dat[, age := 5*floor(age_continuous/5)]
prep_cw_dat[age > 80, age := 80] # nhanes age only goes up to 80, so we need to topcode all for prediction purposes
prep_cw_dat <- prep_cw_dat[age >= 20] # drop respondents with age less than 20
prep_cw_dat[, age := as.factor(age)]
# make 10 year age bins
prep_cw_dat[, age10 := fct_collapse(age, `20_29` = c("20", "25"), 
                                    `30_39` = c("30", "35"),
                                    `40_49` = c("40", "45"),
                                    `50_59` = c("50", "55"),
                                    `60_69` = c("60", "65"),
                                    `70_79` = c("70", "75"),
                                    `80+` = c("80"))]
prep_cw_dat[, age20 := fct_collapse(age, `20_39` = c("20", "25", "30", "35"), # Note that the age group that combines 20-24 with other groups shouldn't be stratified by all Edu categories b/c education in not considered complete until 25
                                    `40_59` = c("40", "45", "50", "55"),
                                    `60+` = c("60", "65", "70", "75", "80"))]


# create year bins
# match up years with NHANES (necessary for crosswalking so we can apply self-report correction with time-effects)
prep_cw_dat[survey != "nhanes", svyyear := 2*floor((year-1)/2)+1]
prep_cw_dat[survey != "nhanes", svyyear := paste(svyyear, as.numeric(svyyear)+1, sep="_")]

# NHANES has a 2017-March 2020 (prepandemic) survey that in representative of that period of time; it's the only way to 
#   match 2019 BRFSS as of now, so we will use 2017_2020prp instead
prep_cw_dat[survey != "nhanes" & year %in% 2017:2020, svyyear := "2017_2020prp"]
prep_cw_dat[survey == "gallup" & year == 2017, svyyear := "2017_2018"] # align Gallup 2017 data with NHANES 2017_2018 instead of 2015_2016 or 2017_2020prp to minimize distortion because of partial wave matching

warning("We don't currently have data for 2020 outside of NHANES")
prep_cw_dat[svyyear == "2017_2020prp", .N, .(year, survey)]

# OLD: drop rows corresponding to the 2017_2018 wave to avoid double-counting
# 8/11/22 -- we don't need to drop these years b/c we subset to specific years in the
#   CW data anyway, and we'll need 2017_2018 for the moving time band approach.
#   This harmonized microdata isn't used outside of the crosswalk model, but 
#   we will be careful if we do use it self where else where we don't subset svyyear.
# prep_cw_dat <- prep_cw_dat[svyyear != "2017_2018"]

# Use the common race/ethnicity values from the database
race_codes <- setDT(list(race_eth = c("Hispanic","NH Black Only", "NH White Only","residual", "NH Black", "NH White"),
                         race_eth_code = c(2, 11, 10, 97, 11, 10),
                         category_mismatch = c(0, 0, 0, 0, 1, 1))) # indicator of a category mismatch (e.g., it's not completely accurate to map "NH Black" to "NH Black Alone", but it's the best we can do in 1999 , 2000)
prep_cw_dat <- merge(prep_cw_dat, race_codes, by.x = "bridged_race", by.y = "race_eth", all = T)


# make collapsed educational categories
prep_cw_dat[, edu2 := fct_collapse(edu, `less than HS` = c("less than HS"),
                                   `HS or higher` = c("HS grad", "some college", "college grad"))]


# In GBD 2023, GBD updated the values of BMI included: BMI > 8 & BMI < 80 (exclusive of bounds).
# Previously, bounds were BMI > 10 & BMI < 70. Updated based on feedback from external
#   sources (source: Slack with Rebecca on 3/14/24)
# Additionally, we are no longer applying additional outliering rules based on
#   what is included/excluded in other surveys. From now on, we'll only use the
#   GBD bounds for BMI. This is to make our inputs more consistent.
# Exception is excluding a few cases where self-reported and measured weight
#   were extremely different (see notes above)

# Apply outliering
prep_cw_dat[!(bmi > 8 & bmi < 80), c("bmi", "height", "weight") := NA]
prep_cw_dat[!(bmi_report > 8 & bmi_report < 80), c("bmi_report", "height_report", "weight_report") := NA]
# update names
setnames(prep_cw_dat, c("bmi", "height", "weight"), c("bmi_measure", "height_measure", "weight_measure"))

# make sure sex, race are all factors
cols <- c("sex", "race_eth_code")
prep_cw_dat[, (cols) := lapply(.SD, as.factor), 
            .SDcols = cols]

# save microdata before dropping data for vetting/experimentation
saveRDS(prep_cw_dat, paste0(output_dir, "prepped_bmi_xwalk_micro_incl_missing.rds"))

# Drop missing vals -------------------------------------------------------

# drop rows with missing values in the crosswalk covariates
n_1 <- prep_cw_dat[, .N, .(survey, svyyear)]
print("% missing in the required vars; Missing % includes values that were outliered.")
missing <- prep_cw_dat[, (lapply(.SD, function(v) round(mean(is.na(v)), digits = 3))), 
            by = .(svyyear, survey), 
            .SDcols = c("edu", "svyyear", "sex", "age", "race_eth_code", "height_report", "weight_report", "bmi_report", "height_measure", "weight_measure", "bmi_measure")]
print("% respondents with missing report_bmi in NHANES who have not-missing measured BMI")
prep_cw_dat[survey == "nhanes" & (is.na(bmi_report)), mean(!is.na(bmi_measure))]

print(missing)
prep_cw_dat <- na.omit(prep_cw_dat, cols = c("svyyear", "sex", "age", "race_eth_code", "edu", "survey")) # don't drop rows just because they're missing height and weight, because we ultimately only need BMI "height_report", "weight_report",
prep_cw_dat <- prep_cw_dat[!(survey != "nhanes" & is.na(bmi_report))] # drop rows if NOT NHANES and bmi_report is missing (but do NOT drop rows of NHANES where bmi measure is available but not self-report, otherwise the measured data will be subject to non-random missingness)
n_2 <- prep_cw_dat[, .N, .(survey, svyyear)]

n <- merge(n_1, n_2, all = T, by = c("survey", "svyyear"), suffixes = c("_full", "_na_omit"))
n[, perc_dropped := (N_full - N_na_omit)/N_full]

print(paste("dropped rows due to missing values:"))
print(n) # some of the surveys have high missingness (as much as 10% in some years; I spot checked BRFSS 2001, NHIS 2001_2002 -- unfortunately, this appears consistent with high dk/rf responses to height and weight variables)

# Save microdata, upload metadata to USHD database -----------------------------

saveRDS(prep_cw_dat, paste0(output_dir, "prepped_bmi_xwalk_micro.rds"))

extraction_ids <- list(unique(nhanes_meta$survey_extraction_version_id), unique(brfss_meta$survey_extraction_version_id),
                       unique(gallup_meta$survey_extraction_version_id))

survey_compile_microdata_version_id <-
  save_compile_microdata(survey_extraction_version_ids = extraction_ids,
                         output_file_path = output_dir,
                         modeler = Sys.info()[["user"]],
                         description = "Add NHANES 2021-2023 to extraction; add BRFSS 2020",
                         is_best = TRUE,
                         prev_issues = "Didn't include latest NHANES round; excluding BRFSS 2020 made creating pairs of years with NHANES challenging",
                         discussion = "not much yet")

################################################################################
# Collapse data by survey/age/sex/race/edu/year --------------------------------
################################################################################

# Calculate mean BMI for each age/sex/race-eth/edu survey

not_nhanes_summary_race <- prep_cw_dat[survey != "nhanes", list(bmi_report = weighted.mean(bmi_report, wt, na.rm = T)), by='svyyear,sex,age,race_eth_code,category_mismatch,survey']

not_nhanes_summary_edu <- prep_cw_dat[survey != "nhanes" & as.numeric(levels(age))[age] >= 25, list(bmi_report = weighted.mean(bmi_report, wt, na.rm = T)), by='svyyear,sex,age,edu,survey']

nhanes_report_race <- prep_cw_dat[survey == "nhanes", list(bmi_report = weighted.mean(bmi_report, int_wt, na.rm = T)), by='svyyear,sex,age,race_eth_code,survey']
nhanes_report_race[, category_mismatch := 0] # by definition, NHANES race/eth categories are not mismatched from NHANES
nhanes_measure_race <- prep_cw_dat[survey == "nhanes", list(bmi_measure = weighted.mean(bmi_measure, mec_wt, na.rm = T)), by='svyyear,sex,age,race_eth_code,survey']

nhanes_report_edu <- prep_cw_dat[survey == "nhanes" & as.numeric(levels(age))[age] >= 25, list(bmi_report = weighted.mean(bmi_report, int_wt, na.rm = T)), by='svyyear,sex,age,edu,survey']
nhanes_measure_edu <- prep_cw_dat[survey == "nhanes" & as.numeric(levels(age))[age] >= 25, list(bmi_measure = weighted.mean(bmi_measure, mec_wt, na.rm = T)), by='svyyear,sex,age,edu,survey']

# combine self-report into long format, and merge column of bmi_measure for a given age, sex, svyyear (i.e., attach NHANES measured bmi to each group)
summary_bmi_race <- rbind(not_nhanes_summary_race, nhanes_report_race, fill = T, use.names = TRUE)
summary_bmi_race <- merge(summary_bmi_race, nhanes_measure_race[, .(age, sex, svyyear, race_eth_code, bmi_measure)], by = c("svyyear", "sex", "age", "race_eth_code"), all.x = T)

summary_bmi_edu <- rbind(not_nhanes_summary_edu, nhanes_report_edu, fill = T, use.names = TRUE)
summary_bmi_edu <- merge(summary_bmi_edu, nhanes_measure_edu[, .(age, sex, svyyear, edu, bmi_measure)], by = c("svyyear", "sex", "age", "edu"), all.x = T)

# make sure that NAs in edu/race are not included
summary_bmi_race <- summary_bmi_race[!is.na(race_eth_code)]
summary_bmi_edu <- summary_bmi_edu[!is.na(edu)]

# save collapsed BMI data -------------------------------------------------

saveRDS(summary_bmi_race, paste0(output_dir, "prepped_bmi_xwalk_race_summary.rds"))
saveRDS(summary_bmi_edu, paste0(output_dir, "prepped_bmi_xwalk_edu_summary.rds"))


###########################################################################
# Make plots of data ------------------------------------------------------
###########################################################################

plot_bmi(prep_cw_dat, output_dir)

# update symbolic link so that output dir is "latest"
system(paste0("ln -sfn ", output_dir, " ", output_dir, "../_LATEST"))