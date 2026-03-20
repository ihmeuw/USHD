## Description:  Generic functions for extractions (other than BRFSS);  brfss_extraction_prep/_functions.R has functions specific for extracting BRFSS 
##                 
#################################################################################


# Load vars ---------------------------------------------------------------


# read file (of variety of types)
read_file <- function(filepath, # full file path
                      rows = Inf # number of rows to read in; 0 reads just header, Inf reads all rows
){
  require(haven)
  filetype <- sub('.*\\.', '', filepath)
  if (filetype == "csv" | filetype == "CSV") {
    file <- fread(filepath, nrows = rows)
  } else if (filetype == "sas7bdat" | filetype == "SAS7BDAT") {
    file <- as.data.table(read_sas(filepath, n_max = rows))
  } else if (filetype == "DTA" | filetype == "dta") {
    file <- as.data.table(read_dta(filepath, n_max = rows))
  } else if (filetype == "XPT" | filetype == "xpt"){
    file <- as.data.table(read_xpt(filepath, n_max = rows))
  } else{
    stop(paste0("filetype '", filetype, "' not found"))
  }  
  return(file)
}


# Search for partial string matches ---------------------------------------

# make %like% case insensitive https://stackoverflow.com/questions/41425699/how-to-get-the-like-operator-to-be-case-insensitive
`%like%` <- function (x, pattern) { 
  grepl(pattern, x, ignore.case=TRUE)
}

# Check files exist -------------------------------------------------------

# make function to check that all files exist
# provide a tibble or dataframe with filenames in a column called "title"
check_files_exist <- function(tibble, dir, file_name){
  require(dplyr)
  missing_files <- tibble %>% 
    dplyr::mutate(file_exists = file.exists(paste0(dir, get(file_name)))) %>%
    dplyr::filter(!file_exists)
  
  if(nrow(missing_files) > 0){
    return(missing_files)
  } else{
    print("All files found!")
  }
}

