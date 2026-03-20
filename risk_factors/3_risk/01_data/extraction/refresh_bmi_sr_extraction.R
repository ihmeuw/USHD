####################################################################################################
## Description: Create an extraction sheet that includes the most-recent template
##                with the previously-extracted results from GBD's systematic review(s).
##                1) Load the extraction template and previous extractions from GBD
##                2) Filter to just the NIDs that we've included for screening
##                3) Add USHD-specific columns + information from screening sheet
##                4) Add the old extractions to the extraction template and save
##              NOTE: to add rows from GBD's extraction sheets to an existing USHD
##                extraction sheet (e.g., because I accidentally left out an NID),
##                use **add_nid_sr_extraction.R** instead.                
## Input:   
##       GBD's extractions: "FILEPATH"
##       USHD current screening sheet: "FILEPATH")
##       USHD extraction sheet template: <- "FILEPATH"
##
## Output:
##       FILEPATH
## 
####################################################################################################

# set up ------------------------------------------------------------------

library(data.table)
library(openxlsx)
source("FILEPATH")

gbd_extraction_root <- "FILEPATH"
ushd_screening_sheet <- read.xlsx("FILEPATH")
ushd_extraction_sheet_template <- "FILEPATH"
ushd_extraction_root <- "FILEPATH"
setDT(ushd_screening_sheet)
# Compile GBD extractions -------------------------------------------------

ff <- list.files(gbd_extraction_root,
                 pattern = "^cc",
                 full.names = T)

my_read <- function(f){
  if(grepl(".csv", f)) fread(f) else as.data.table(read.xlsx(f))
}
all_extractions <- lapply(ff, my_read)

all_extractions <- rbindlist(all_extractions, fill = T)

# Filter extractions to USHD-screened articles ----------------------------

setDT(ushd_screening_sheet)
include_nids <- ushd_screening_sheet[Determination != "Excl", GHDx_NID]
stopifnot(sum(is.na(include_nids)) == 0)

# filter to USHD NIDs
extractions <- all_extractions[nid %in% include_nids | underlying_nid %in% include_nids]

# check if any NIDs we wanted to include are not in extraction sheets
stopifnot(sum(!include_nids %in% extractions$nid) == 0) # all NIDs we screened should be in extraction sheet
# check if any NIDs we wanted to include are excluded by GBD (after extraction)
extractions[keep == 1 & nid %in% include_nids] # keep = 1 means that excluded for GBD

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

extractions <- merge(screening_info, extractions, by = "nid", all = T)
# write.xlsx(extractions, file = paste0(ushd_extraction_root, "extraction_reference_", make_time_stamp(), ".xlsx"))
setnames(extractions, "keep", "gbd_keep") # update column name to match our extraction template



# load extraction sheet template
USHD_extract_sheet <- loadWorkbook(ushd_extraction_sheet_template)
template <- readWorkbook(USHD_extract_sheet, sheet = "extraction", skipEmptyCols = F) # skipEmptyCols = F must be specified, otherwise you'll get an off-by-one error for data validation :'(
setDT(template)

# clean up column names to merge
check <- ls(extractions)[!ls(extractions) %in% ls(template)] # names in GBD's extraction that don't match extraction sheet

invisible(sapply(check, function(s){
  temp <- grep(pattern = paste0("\\b", s, "\\b"), x = ls(template), ignore.case = T, value = T) # see if the mismatched column name has different capitalization than the words in template
  if(length(temp) > 0 ){
    setnames(extractions, old = s, new = temp)
  } 
}))

check <- ls(extractions)[!ls(extractions) %in% ls(template)] # update mismatched names
if(length(check) > 0){
  sapply(check, function(s) {cat(s, " missing, check: \n\n", paste(agrep(s, x = ls(template), ignore.case = T, value = T), collapse = ", "), "\n ---- \n")}) # fuzzy match to see if there are typos
  stop("Check typos in column names")
}


update_extraction <- rbind(template, extractions, fill =T)

writeData(wb=USHD_extract_sheet, sheet = "extraction", x = update_extraction)

# add worksheet that describes the data input files used
addWorksheet(wb=USHD_extract_sheet, sheetName = "extraction_provenance")
ff <- as.data.table(ff)
ff[, last_updated := file.info(ff)$ctime]
writeData(wb=USHD_extract_sheet, sheet = "extraction_provenance", x = ff)

saveWorkbook(wb=USHD_extract_sheet, paste0(ushd_extraction_root, "BMI_USHD_RR_extraction_", make_time_stamp(), ".xlsx"))
