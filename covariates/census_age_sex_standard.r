####################################################################################################
## Description: Format the single-year age-sex distribution from the 2010 census to use for age and
##              sex standardizing.
##
## Output:      'census_age_sex_standard.rds' -- by single age and sex, to be used for the risk
##                  factor analyses. Note that this gives actual population counts.
##              'census_age_weights.csv' -- by 5-year age groups only, to be used in mortality
##                  analyses. Note that this gives normalized weights.
####################################################################################################

library(data.table)
library(stringr)

rm(list = ls())
root <- ifelse(Sys.info()[1] == "Windows", "[FILEPATH]", "[FILEPATH]")

indir <- paste0(root, "[FILEPATH]")
outdir <- paste0(root, "[FILEPATH]")

## Load and formate the population counts ----------------------------------------------------------
# load the metadata and extract age and sex information from the necessary variables
meta <- fread(paste0(indir, "[FILEPATH]"), header = F)
setnames(meta, c("variable", "label"))
meta <- meta[grepl("Number", label) & grepl("Male|Female", label),]
meta <- meta[sapply(gregexpr("-", label), length) %in% c(1,3),] # 1 = lines with totals; 3 = lines with single-age estimates
meta[, age := substr(label, sapply(gregexpr("-", label), function(x) x[3]) + 2, 100)]
meta[, age := as.numeric(str_trim(gsub("Under", "", gsub("years|year", "", age))))] # age will be missing for the totals
meta[grepl("Under 1 year", label), age := 0]
meta[, sex := ifelse(grepl("Male", label), 1, 2)]
meta <- meta[, list(variable, age, sex)]

# load the data and keep only necessary variables
stdpop <- fread(paste0(indir, "[FILEPATH]"))
stdpop <- melt(stdpop, id.vars = c("GEO.id", "GEO.id2", "GEO.display-label"))
stdpop <- merge(stdpop, meta, by = "variable")
stdpop <- stdpop[, list(age, sex, stdpop = as.numeric(value))]
setkeyv(stdpop, c("sex", "age"))

# single-year estimates are available only to age 99 -- use the difference between the grand total
# and the sum age 0-99 as the stdpopulation for age 100 (100+)
remainder <- (stdpop[is.na(age), stdpop, sex] - stdpop[!is.na(age), sum(stdpop), sex])$stdpop
stdpop[is.na(age), stdpop := remainder]
stdpop[is.na(age), age := 100]
setkeyv(stdpop, c("sex", "age"))

## Save the by sex and single-year age version -----------------------------------------------------
saveRDS(stdpop, file = paste0(outdir, "census_age_sex_standard.rds"))

## Collapse and save the by 5-year age group versions ----------------------------------------------
stdpop[between(age, 1, 4), age := 1]
for (ii in seq(5, 80, 5)) stdpop[between(age, ii, ii + 4), age := ii]
stdpop[age >= 85, age := 85]
stdpop <- stdpop[, list(wt = sum(stdpop)), keyby = 'age']
stdpop[, wt := wt/sum(wt)]
write.csv(stdpop, file = paste0(outdir, "census_age_weights.csv"), row.names = F)
