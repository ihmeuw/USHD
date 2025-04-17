####################################################################################################
## Description: Prep ACS 5-year estimates of educational attainment for adults aged 25+
####################################################################################################

# Load settings file and directories ---------------------------------------------------------------
user <- Sys.info()[["user"]]
if (!grepl("covariates$", getwd())) setwd("FILEPATH")
library(tidycensus)

in_dir <- "FILEPATH"
out_dir <- "FILEPATH"
if (!dir.exists(out_dir)) dir.create(out_dir)  # create new folder for outputs if one doesn't exist already
temp_dir <- "FILEPATH"
nids <- data.table(year = 2007:2020, nid = c(111405:111406, 111410, 132413, 236697, 236698, 280080,
                                             399019, 399014, 448152, 476546, 503429, 520867, 548126))

# Load and format data -----------------------------------------------------------------------------
extract_data <- function(year) {
  cat(paste0(year, "\n")); flush.console()
  
  # load metadata, format, and find variables of interest
  metadata_pattern <- "FILEPATH"
  split_string <- "; "
  if(year >= 2018) {id_variables <- c("GEO_ID", "NAME")} else {id_variables <- c("GEO.id", "GEO.id2", "GEO.display-label")}
  
  files <- list.files(paste0(in_dir, ifelse(year == 2015, "FILEPATH", "FILEPATH")),
                      pattern = "S1501", full.names = T)
  if (year >= 2019) {  # temp before indexing
    files <- list.files(temp_dir, pattern = "FILEPATH", full.names = T)
    metadata_pattern <- "Metadata"
  }
  meta <- fread(grep(metadata_pattern, files, value = T))[, 1:2, with = F]  # blank third column shows up in 2018 so specify first two
  setnames(meta,  c("var", "label"))
  meta[, label := gsub(" - |!!", "; ", label)]  # make separators consistent
  meta[, label := gsub("RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT; ", "", label)]  # unnecessary spec
  meta[, label := gsub("Population 25 years and over; Population", "Population", label)]  # extra formatting for 2018+
  meta <- meta[grepl("Estimate", label) & grepl("Total|Percent;", label) & !grepl("POVERTY RATE|MEDIAN EARNINGS|IMPUTED", label)]  # only both sexes estimates, no poverty rate or median income
  if (year >= 2019) {  # account for formatting changes beginning in 2019
    meta <- meta[grepl("AGE BY EDUCATIONAL ATTAINMENT", label) & grepl("Population 25 years and over", label) &
                   !grepl("Annotation", label)]
    meta[, label := gsub("AGE BY EDUCATIONAL ATTAINMENT; ", "", label)]  # remove once used to filter
  }
  meta[, label := mgsub(label, c("Percent; ", "Estimate; "), c("Percent ", ""))]  # restructure label before split
  meta[, label := gsub("Percent Percent", "Percent", label)]  # in some years "Percent" gets duplicated
  if (year >= 2015) {
    meta[, label := gsub("Total; ", "Total ", label)]
  } else {
    meta[, label := mgsub(label, c("Total; Population", "Total; Percent"), c("Total Population", "Percent"))]
  }
  meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = split_string)))
  setnames(meta, c("variable", "description", "education"))
  meta[grepl("Percent (high school graduate|bachelor's degree) or higher", education), description := education]
  meta[, education := ifelse(grepl("Total|Estimate|Percent", education), "All", education)]  # more accurate description of education
  
  # merge data with metadata
  file_paths <- grep("FILEPATH", files, invert = T, value = T)  # isolate data (i.e., not metadata) files from all files
  if (year > 2017) {
    file_path <- grep(paste0("5YR_EST_", year, "_S1501"), file_paths, value = T)  # specify pattern for 5Y files newer than 2017
  } else {
    file_path <- grep("AFF", file_paths, value = T)  # 2009-2017 5Y files have "AFF" (American FactFinder) in the name
  }
  if (year >= 2019) {  # temp before indexing
    file_path <- grep("Metadata", files, invert = T, value = T)
  }
  data <- fread(file_path, header = T)
  if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1,]  # remove row of variable descriptions
  data <- melt(data, id.vars = id_variables)
  data <- merge(data, meta, all.y = T, by = "variable")
  if (year >= 2018) data[, GEO.id2 := substr(GEO_ID, nchar(GEO_ID) - 4, nchar(GEO_ID))]  # last 5 digits of GEO_ID = county fips
  data <- data[, list(GEO.id2, variable, value, description, education)]
  
  # filter down to variables of interest
  setnames(data, "GEO.id2", "fips")
  data[, c("fips", "value", "year") := list(as.integer(as.character(fips)), as.numeric(as.character(value)), year - 2)]
  data <- data[(description == "Total Population 25 years and over" & education == "All") |  # total population
               (description == "Percent high school graduate or higher") |  # proportion with HS+ edu
               (description == "Percent bachelor's degree or higher") |  # proportion with BA+ edu
               (grepl("Percent Population 25 years", description) & grepl("or higher", education))]  # different format in 2018+
  data[, description := str_replace(description, "Percent Population 25 years and over", paste("Percent", tolower(education)))]  # make 2018+ consistent with earlier years
  data <- dcast(data, fips + year ~ description, value.var = "value")
  setnames(data, old = c("Total Population 25 years and over", "Percent high school graduate or higher", 
                         "Percent bachelor's degree or higher"), new = c("pop", "edu_hs", "edu_ba"))
  
  return(list("data" = data, "file_path" = file_path))
}

# Load and format data -----------------------------------------------------------------------------
extract_data_CT <- function(year) {
  cat(paste0(year, "\n")); flush.console()
  
  # load metadata, format, and find variables of interest
  metadata_pattern <- "FILEPATH"
  split_string <- "; "
  files <- list.files(paste0(temp_dir, 'CT/'), pattern = "FILEPATH", full.names = T)
  metadata_pattern <- "Metadata"
  
  meta <- fread(grep(metadata_pattern, files, value = T))[, 1:2, with = F]  # blank third column shows up in 2018 so specify first two
  setnames(meta,  c("var", "label"))
  meta[, label := gsub(" - |!!", "; ", label)]  # make separators consistent
  meta[, label := gsub("RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT; ", "", label)]  # unnecessary spec
  meta[, label := gsub("Population 25 years and over; Population", "Population", label)]  # extra formatting for 2018+
  meta <- meta[grepl("Estimate", label) & grepl("Total|Percent;", label) & !grepl("POVERTY RATE|MEDIAN EARNINGS|IMPUTED", label)]  # only both sexes estimates, no poverty rate or median income
  meta <- meta[grepl("AGE BY EDUCATIONAL ATTAINMENT", label) & grepl("Population 25 years and over", label) &
                 !grepl("Annotation", label)]
  meta[, label := gsub("AGE BY EDUCATIONAL ATTAINMENT; ", "", label)]  # remove once used to filter
  meta[, label := mgsub(label, c("Percent; ", "Estimate; "), c("Percent ", ""))]  # restructure label before split
  meta[, label := gsub("Percent Percent", "Percent", label)]  # in some years "Percent" gets duplicated
  meta[, label := gsub("Total; ", "Total ", label)]
  meta <- data.table(meta$var, do.call("rbind", strsplit(meta$label, split = split_string)))
  setnames(meta, c("variable", "description", "education"))
  meta[grepl("Percent (high school graduate|bachelor's degree) or higher", education), description := education]
  meta[, education := ifelse(grepl("Total|Estimate|Percent", education), "All", education)]  # more accurate description of education
  
  # merge data with metadata
  file_paths <- grep("FILEPATH", files, invert = T, value = T)  # isolate data (i.e., not metadata) files from all files
  file_path <- grep(paste0("5YR_EST_", year, "_S1501"), file_paths, value = T)  # specify pattern for 5Y files newer than 2017 (aka post-American FactFinder, or AFF, era)
  file_path <- grep("Metadata", files, invert = T, value = T)
  data <- fread(file_path, header = T)
  if (names(which.max(table(sapply(data, class)))) == "character") data <- data[-1,]  # remove row of variable descriptions
  data <- melt(data, id.vars = id_variables)
  data <- merge(data, meta, all.y = T, by = "variable")
  data[, GEO.id2 := as.numeric(substr(GEO_ID, (nchar(GEO_ID) - 5), nchar(GEO_ID)))]  # last 5 digits of GEO_ID = county fips
  data <- data[, list(GEO.id2, variable, value, description, education)]
  
  # filter down to variables of interest
  setnames(data, "GEO.id2", "fips")
  data[, c("fips", "value", "year") := list(as.integer(as.character(fips)), as.numeric(as.character(value)), year - 2)]
  data <- data[(description == "Total Population 25 years and over" & education == "All") |  # total population
                 (description == "Percent high school graduate or higher") |  # proportion with HS+ edu
                 (description == "Percent bachelor's degree or higher") |  # proportion with BA+ edu
                 (grepl("Percent Population 25 years", description) & grepl("or higher", education))]  # different format in 2018+
  data[, description := str_replace(description, "Percent Population 25 years and over", paste("Percent", tolower(education)))]  # make 2018+ consistent with earlier years
  data <- data[!fips %in% c(990000,990100)]
  data <- dcast(data, fips + year ~ description, value.var = "value")
  setnames(data, old = c("Total Population 25 years and over", "Percent high school graduate or higher", 
                         "Percent bachelor's degree or higher"), new = c("pop", "edu_hs", "edu_ba"))
  data <- data[!is.na(edu_ba)]
  
  return(list("data" = data, "file_path" = file_path))
}

extractions <- lapply(2009:2022, extract_data)
data <- rbindlist(lapply(1:length(extractions), function(i) {extractions[[i]]$data}))  # rbind all data objects from extractions
data <- data[fips < 60000, ]  # filter out territories
data <- merge(data, nids, by = "year")  # add nids
data_ct <- extract_data_CT(2022)
data <- rbind(data[!(year == 2020 & (fips >= 9000 & fips <= 9999))],
              data_ct$data, fill = T)
nid_2020 <- unique(data[year == 2020]$nid)[1]
data[year == 2020, nid := nid_2020]


# map CT census tracts to old county FIPS by loading a CT tract-level dataset and isolating tract
# and county fips from GEOID
ct_2020_tract <- as.data.table(get_acs(geography = "tract", variables = c("B17001_001"),
                                       year = 2020, state = "CT", survey = "acs5"))
ct_2020_tract[, c("tract_fips", "fips") := list(as.numeric(substr(GEOID, 6, 11)), as.numeric(substr(GEOID, 1, 5)))]
ct_2020_tract[tract_fips == 990000, fips := 9001]  # manually assign FIPS to single county for water tracts (estimates are 0)
ct_2020_tract[tract_fips == 990100, fips := 9007]
tract_fips_map <- unique(ct_2020_tract[, list(tract_fips, fips)]); rm(ct_2020_tract)
data <- merge(data, tract_fips_map,
              by.x = 'fips', by.y = 'tract_fips', all.x = T)
data[!is.na(fips.y), fips := fips.y]
data <- data[,c('fips','year','edu_ba','edu_hs','pop','nid')]

## Format and save -------------------------------------------------------------
# name file for date/time stamp, separated by underscores (YYYY_MM_DD_HH_MM_SS)
date_time_stamp <- mgsub(as.character(Sys.time()), c("-", " ", ":"), rep("_", 3))

# make list of file paths named for corresponding NID to pass as extraction metadata
nid_path_list <- lapply(1:length(extractions), function(i) {list(extractions[[i]]$file_path)})
names(nid_path_list) = nids[, nid]  # map each NID to its respective list of file paths

# save output
saveRDS(data, file = paste0(out_dir, date_time_stamp, ".rds"))