####################################################################################################
## Description: This script is based on **refesh_bmi_sr_extraction.R**. Use that
##                script to create an extraction sheet based on USHD screening sheet
##                and GBD extractions. Use THIS script instead if you just want to add
##                a few additional NIDs
##                
##              1) Run refesh_bmi_sr_extraction.R to get an entirely new extraction sheet
##              2) put the filepath below as ushd_version_new (the version that includes all of the NIDs, but is not filled out)
##              3) update the filepath for ushd_previous (the version that was extracted, but you want to add to)
##                
## Input:   GBD's extractions
##          USHD extraction sheet
##          New and old version names for extaction shseets
##          Screening sheet
##          
## Output:  A new extraction sheet that combines: pre-existing USHD extraction sheet, information from 
##            USHD screening sheet about inclusion criteria, filepaths, etc., and the GBD extractions
## 
####################################################################################################

rm(list = ls())

# set up ------------------------------------------------------------------

library(data.table)
library(openxlsx)
source("FILEPATH")

gbd_extraction_root <- "FILEPATH"
ushd_extraction_root <- "FILEPATH"
ushd_version_new <- "BMI_USHD_RR_extraction_2022_03_16_18_34.xlsx"  # (the version that includes all of the NIDs, but is not filled out)
ushd_previous <- "BMI_USHD_RR_extraction_2022_02_28_09_01.xlsx"  # (the version that was extracted, but you want to add to)
ushd_screening_sheet <- read.xlsx("FILEPATH")
setDT(ushd_screening_sheet)


# Compile GBD extractions -------------------------------------------------

ff <- list.files(gbd_extraction_root,
                 pattern = "^cc",
                 full.names = T)

my_read <- function(f){
  if(grepl(".csv", f)) fread(f) else as.data.table(read.xlsx(f))
}
all_extractions <- lapply(ff, my_read)

all_extractions <- rbindlist(all_extractions, fill =T )


# Identify rows to add ----------------------------------------------------

#  load old and new extraction sheets
USHD_new <- readWorkbook(paste0(ushd_extraction_root, ushd_version_new), sheet = "extraction", skipEmptyCols = F)
USHD_old <- readWorkbook(paste0(ushd_extraction_root, ushd_previous), sheet = "extraction", skipEmptyCols = F)
setDT(USHD_new)
setDT(USHD_old)

# what NIDs are in new but not old

add_nids <- USHD_new[!nid %in% USHD_old$nid, as.numeric(unique(nid))]
message("Adding ", uniqueN(add_nids), " nids")


# Filter extractions to just the articles we want to add ----------------------------


# filter to USHD NIDs
extractions <- all_extractions[nid %in% add_nids | underlying_nid %in% add_nids]

# check if any NIDs we wanted to include are not in extraction sheets
stopifnot(sum(!add_nids %in% extractions$nid) == 0) # all NIDs we screened should be inextraction sheet
# check if any NIDs we wanted to include are excluded by GBD (after extraction)
extractions[keep == 1 & nid %in% add_nids] # keep = 1 means that excluded for GBD

# Merge on some of the USHD screening criteria
screening_info <- copy(ushd_screening_sheet)

# clean up names
setnames(screening_info,
         c("Determination", 
           "Reason.for.Exclude", 
           "SR.Notes", 
           "in_bundle_current_40010", 
           'Almost.entirely.one.race.(reported)?.(Mark.as."incl?".if.>90%)', 
           'Percent.of.study.population.that.is.NH.White..Put."!".in.front.of.number.if.article.does.not.specify.NH.White.or.specifically.includes.Hispanic.&.white.people.in.NH.white.Put."~".in.front.of.number.if.the.%.can.be.calculated.but.not.directly.reported.(include.rough.estimate)', 
           "How.race.incorporated", 
           "Extraction.tips", 
           "Model.specification",
           "GHDx_NID"),
         c("USHD_determination",
           "USHD_exclusion_reason",
           "USHD_SR_notes",
           "in_bundle_current_40010",
           "almost_all_one_race",
           "Percent_white",
           "How_race_incorported",
           "USHD_extraction_tips",
           "USHD_model_specification",
           "nid"))

screening_info <- screening_info[, c("USHD_determination",
                   "USHD_exclusion_reason",
                   "USHD_SR_notes",
                   "in_bundle_current_40010",
                   "almost_all_one_race",
                   "Percent_white",
                   "How_race_incorported",
                   "USHD_extraction_tips",
                   "Full.file.path",
                   "nid"
                   ), 
               with = F][USHD_determination != "Excl"]

extractions <- merge(screening_info, extractions, by = "nid", all.y = T) # only add the screening info from extraction sheet to avoid duplicating rows
# write.xlsx(extractions, file = paste0(ushd_extraction_root, "extraction_reference_", make_time_stamp(), ".xlsx"))
setnames(extractions, "keep", "gbd_keep") # update column name to match our extraction template



# load the full, old extraction workbook
USHD_extract_sheet <- loadWorkbook(paste0(ushd_extraction_root, ushd_previous))
original <- readWorkbook(USHD_extract_sheet, sheet = "extraction", skipEmptyCols = F) # skipEmptyCols = F must be specified, otherwise you'll get an off-by-one error for data validation :'(
setDT(original)

# clean up column names to merge
check <- ls(extractions)[!ls(extractions) %in% ls(original)] # names in GBD's extraction that don't match extraction sheet

invisible(sapply(check, function(s){
  temp <- grep(pattern = paste0("\\b", s, "\\b"), x = ls(original), ignore.case = T, value = T) # see if the mismatched column name has different capitalization than the words in original
  if(length(temp) > 0 ){
    setnames(extractions, old = s, new = temp)
  } 
}))

check <- ls(extractions)[!ls(extractions) %in% ls(original)] # update mismatched names
# sapply(check, function(s) {cat(s, " missing, check: \n\n", paste(agrep(s, x = ls(original), ignore.case = T, value = T), collapse = ", "), "\n ---- \n")}) # fuzzy match to see if there are typos

update_extraction <- rbind(original, extractions, fill =T)

writeData(wb=USHD_extract_sheet, sheet = "extraction", x = update_extraction)

# add worksheet that describes the data input files used
addWorksheet(wb=USHD_extract_sheet, sheetName = "extraction_provenance2")
ff <- as.data.table(ff)
ff[, last_updated := file.info(ff)$ctime]
writeData(wb=USHD_extract_sheet, sheet = "extraction_provenance", x = ff)

saveWorkbook(wb=USHD_extract_sheet, paste0(ushd_extraction_root, "BMI_USHD_RR_extraction_", make_time_stamp(), ".xlsx"))
