####################################################################################################
## Description: Check that all of the articles that used cohorts deemed to be
##                essentially one race are included in extraction sheet. When screening,
##                I was a bit ad hoc about what studies I flagged to check for being essentially
##                one race b/c it required me to remember the study demographics of
##                cohorts. 
##              Now, we need to be systematic. Load the filled-out extraction sheet
##                and compare that to all of the studies used in GBD's extractions.
##                Check if there are any articles that I did NOT extract that used
##                the same cohort as studies I did extract (and therefore should
##                be considered essentially one race)
##                
## Input:   Extraction sheet; GBD's bundle
## Output:  Saves list of NIDs that we should add to the extraction sheet.
## 
####################################################################################################

library(data.table)
library(openxlsx)
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")
source("FILEPATH")

time_stamp <- make_time_stamp()

gbd_extraction_root <- "FILEPATH"
ushd_extraction_root <- "FILEPATH"
ushd_version <- "BMI_USHD_RR_extraction_2022_03_16_19_11.xlsx"

documentation_message <- list(memo = "Figure out NIDs that are missing from original USHD extraction\naccording to study cohort\n",
                              date_run = paste(time_stamp, "\n"),
                              ushd_extraction_version = paste0(ushd_extraction_root, ushd_version,"\n"))

# Compile GBD extractions -------------------------------------------------
ff <- list.files(gbd_extraction_root,
                 pattern = "^cc",
                 full.names = T)

my_read <- function(f){
  if(grepl(".csv", f)) fread(f) else as.data.table(read.xlsx(f))
}
all_extractions <- lapply(ff, my_read)

all_extractions <- rbindlist(all_extractions, fill =T )

# get all NIDs in GBD's bundle --------------------------------------------

bundle_version_id = 40010 # old 37763
bundle_version <- get_bundle_version(bundle_version_id = bundle_version_id, export = FALSE)


# load the USHD extraction ------------------------------------------------
ushd_extract <- read.xlsx(paste0(ushd_extraction_root, ushd_version))
setDT(ushd_extract)

# filter to articles that are actually included in USHD extraction

ushd_extract <- ushd_extract[in_bundle_current_40010 == 1]

# Check -------------------------------------------------------------------

# are there NIDs that are assocaited with studies we extracted from, but the NIDs
#   were not extracted for USHD?

ushd_study_name <- ushd_extract[, study_name]
ushd_nid <- ushd_extract[, nid]

check <- all_extractions[(!nid %in% ushd_nid) & study_name %in% ushd_study_name & keep == 0] #keep = 0 filters to articles GBD actually used

check[, unique(nid)]
check[, unique(study_name), nid] # see what study it is

# manually exclude some NIDs associated with studies not included for being effectively
#   single race (b/c those NIDs have already been excluded)
check <- check[nid != 340297]

# make sure that all of the NIDs are in the latest bundle
stopifnot(check[, .N] == 0 | mean(check[, unique(nid)] %in% unique(bundle_version$nid)) == 1)

# Save NIDs to add --------------------------------------------------------

out_dir <- paste0(ushd_extraction_root, "nids_to_add/", time_stamp)
dir.create(out_dir)
saveRDS(file = paste0(out_dir, "/nids_to_add.rds"), check[, unique(study_name), nid])

# save info
documentation_message <- append(documentation_message, check[, .(study_name = unique(study_name)), nid])
lapply(documentation_message, cat, file = paste0(out_dir, "/memo.txt"), append = T)
