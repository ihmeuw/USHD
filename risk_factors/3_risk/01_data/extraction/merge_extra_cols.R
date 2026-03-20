####################################################################################################
## Description: When cleaning the extractions, I removed extra columns I assumed
##                we didn't need. This was a mistake b/c some of those are used
##                in clean_extractions or clean_mr_brt
##              This script reads in the cleaned and original extraction sheets,
##                creates a hash based on some columns that should be unique per
##                row (because there isn't a real unique ID column at this point).
##                This hash is used as a merge key, and add the additional columns
##                from original onto the cleaned data.
##                
##                
##              Overwrites the file BMI_USHD_RR_extraction_2022_03_16_19_11_EXTRACTION_CLEANED.xlsx
##              
####################################################################################################

stop("This script doesn't need to be rerun -- it updates the columns in EXTRACTION_CLEANED,
      and the updated version is in the directory now")

library(openxlsx)
library(data.table)

orig <- read.xlsx("")
clean <- read.xlsx("")
clean_wb <- loadWorkbook("")
setDT(orig)
setDT(clean)

# create unique Key based on nid, sex, cohort_exposed_def, and effect size
keys <- c("nid", "cohort_sample_size_total", "control_for_confounders_score", "stroke_type", "location_id", "cohort_exposed_def", "effect_size", "lower", "outcome_def", "subgroup_analysis_free_text")

for( i in keys){
  clean[is.na(get(i)), (i) := "was_na"]
  orig[is.na(get(i)), (i) := "was_na"]
}
library(rlang)
orig[, my_hash :=  paste0(lapply(.SD, hash), collapse = ""), by = 1:nrow(orig),
     .SDcols = keys]

clean[, my_hash := paste0(lapply(.SD, hash), collapse = ""), by = 1:nrow(clean),
      .SDcols = keys]

# make sure all Hashes are unique
stopifnot(orig[, uniqueN(my_hash)] == orig[, .N])
stopifnot(clean[, uniqueN(my_hash)] == clean[, .N])

# get list of columns in original extraction that I mistakenly removed from the cleaned version

extra_cols <- names(orig)[which(names(orig) == "issues_note"):ncol(orig)] # all of the column names to theh righ tof "issues_notes"
extra_cols <- setdiff(extra_cols, c(names(clean), "risk_mapping.1", "outcome_mapping.1", "rei_id", "cause_id",# don't create duplicate columns
                                    grep("Score", ignore.case = F, extra_cols, value = T))) # don't copy the duplicate score column 

setkey(clean, my_hash)
setkey(orig, my_hash)

# all hashes in clean should also be in orig, but not the other way around
tt <- which(sapply(clean$my_hash, function(c) !c %in% orig$my_hash))
if(length(tt) > 0){
  # investigate any cases where not all hashes from cleaned version in the original
  for(a_nid in clean[!orig][, unique(nid)]){
    cat("\n#########", a_nid, "##################\n")
    for(i in keys){
      if(!identical(orig[nid == a_nid, ..i],clean[nid == a_nid, ..i])) {
        print(i)
        print(orig[nid == a_nid, ..i])
        print(clean[nid == a_nid, ..i])
      }
    }
  }
  stop("hash key didn't work")
}

# merge on the new columns
new_clean <- merge(clean, orig[, c(extra_cols, "my_hash"), with = F], by = "my_hash", all.x = T)

stopifnot(clean[,.N] == new_clean[,.N])

# make sure no values in the manually cleaned version have been changed
stopifnot(all.equal(new_clean[, names(clean), with = F],
          clean))

# remove the NAs I added

for( i in keys){
  new_clean[get(i) == "was_na", (i) := NA]
}

new_clean[, my_hash:=NULL]
setkey(new_clean, seq)

setkey(clean, seq)

# overwrite the extraction sheet with validated data
writeData(wb = clean_wb, sheet = "extraction", x = new_clean, colNames = T, startRow = 1, startCol = 3)
saveWorkbook(clean_wb, "FILEPATH", overwrite = T) # this version preserves formatting and has a second header, which cannot be uploaded to DB

