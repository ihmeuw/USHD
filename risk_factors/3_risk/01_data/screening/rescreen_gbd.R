####################################################################################################
## Description: Create a screening sheet based on GBD's systematic review(s)
##                1) Pull the RR bundle used for GBD's BMI risk curves
##                2) Filter to sources that have US locations
##                3) Find relevant NIDs
##                4) Find PMIDs from NIDs
##                5) Conduct PubMed search
##                6) Create a fresh screening sheet
## 
## Input: GBD's latest bundle (bundle ID 40010 as of 2/22/2022)
##        Scripts to create screening template (ghdx_match.R)
##
## Output: FILEPATH
##          Misc. intermediate outputs
##
####################################################################################################

# set up ------------------------------------------------------------------

out_path <- "FILEPATH"

source("FILEPATH/get_bundle_data.R")
source("FILEPATH/get_bundle_version.R")
source("FILEPATH/get_ids.R")
source("FILEPATH/_versioning_functions.R")
source("FILEPATH/ghdx_match.R")
source(sprintf("FILEPATH/update_locations_nids.R", Sys.info()["user"])) # load run_update_location_nids()

library(data.table)
library(stringr)
library(stringi)
library(xml2)

# get GBD 2020 bundle for BMI RR ------------------------------------------

# get bundle information for BMI RR
bundle_version_id = 40010 # old 37763
bundle_version <- get_bundle_version(bundle_version_id = bundle_version_id, export = FALSE)
# filter to US locations --------------------------------------------------

all_locs <- get_ids("location", return_all_columns = T)
loc_ids = unique(c(102, # USA
                   all_locs[location_parent_id == 102, location_id], # US Admin 1
                   all_locs[location_parent_id %in% all_locs[location_parent_id == 102, location_id], location_id]) # US Admin 2
                 )

locs <- all_locs[location_id %in% loc_ids]

additional_usa_nids <- run_update_location_nids() # outputs list of NIDs that are in extractions as USA but not in bundle

US_bundle <- bundle_version[location_id %in% locs$location_id | nid %in% additional_usa_nids] # note that not all nids in additional_usa_nids will be in bundle_version if those NIDs were excluded from latest bundle

# check if there are NIDs in new bundle that are not in old bundle:


# CONFIRM that did not accidentally exclude any US locations. 1/28/2020 -- this looks good
table(bundle_version[!US_bundle, on = "nid", location_name])

# Get PubMed IDs ----------------------------------------------------------

# Based on bundle, get the NIDs

if(US_bundle[, uniqueN(nid)] >=500) stop("Don't enter more than 500 NIDs into the tool")
search <- paste0("FILEPATH", paste(US_bundle[, unique(nid)], collapse = "+"))
time_stamp <- make_time_stamp()
dir.create(paste0(out_path, time_stamp))
cat(search, file = paste0(out_path, time_stamp, "/search_nids_", "bundle_version_id_", bundle_version_id, ".txt"))

citations_path <-  paste0(out_path, time_stamp, "/search_by_multiple_nids_", bundle_version_id, ".csv")

print(paste0("Enter the URL based on NIDs into your web browser (may need to log into GHDx)
      Download the CSV of results and save to\n", citations_path))
readline(prompt="Press [enter] to continue AFTER completing the above")

# read the citations:
cit <- fread(citations_path)

# use ECitMatch, which is the API to PubMed Batch Citation Matcher
# https://www.ncbi.nlm.nih.gov/books/NBK25499/
# https://www.ncbi.nlm.nih.gov/books/NBK25499/#!po=89.2857
# https://pubmed.ncbi.nlm.nih.gov/batchcitmatch/

# base = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi?"

# create search string to match  ECitMatch format:
# journal_title|year|volume|first_page|author_name|your_key|
# use NID as your_key
term <- cit[, .(Journal, 
                Year = `Publication year`,
                Volume, 
                first_page = ifelse(grepl("e", Pages), NA, str_extract(Pages, "[:digit:]+(?=-||[\U{2013}])")), # get the number before the hyphen, or the EN DASH which is not recognized as "-") 
                author =  gsub(",", "",stri_trans_general(str = Authors,id = "Latin-ASCII")),
                  # gsub(",", "", Authors),
                # first_author = apply(sapply(strsplit(Authors,","), "[", 1:5),2,  paste, collapse = " "),#sapply(strsplit(Authors,","), "[", 1),
                NID)]

# address ad-hoc issues that created issues finding PMID
term[NID %in% c("115797", "365224"), Journal := NA]
term[NID %in% c("340295", "409935", "409954"), first_page := NA]

term <- as.data.table(lapply(term, function(c) tolower(gsub(" ", "+", c))))
term[, search := paste(.SD, collapse = "|"), by = 1:nrow(term)][, search := paste0(search, "|")][, search := gsub("NA", "", search)]

chunks <- split(term$search, ceiling(seq_along(term$search)/20)) # split queries into chunks of 20 # do not parallelize
citations <- vector(mode= "list", length = length(chunks))
i = 1
for(cc in chunks){
  all_terms <- paste(cc, collapse = "%0D")
  # browseURL(sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi?db=pubmed&retmode=xml&bdata=%s", all_terms))
  url <- sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi?db=pubmed&retmode=xml&bdata=%s", all_terms)
  
  xx <- read_xml(x = url, as_html = T)
  xx <- xml_text(xx)
  citations[[i]] <- fread(text = xx, col.names = c(colnames(term)[1:6], "PMID"))
  i = i+1
  Sys.sleep(3) # Don't query too fast. https://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.Usage_Guidelines_and_Requiremen
}

citations <- rbindlist(citations)

# check which citations didn't have PMIDs
if(citations[str_detect(PMID, "[:alpha:]"), .N] > 0){
  warning("Not all citations were matched to PMIDs:")
  citations[str_detect(PMID, "[:alpha:]")][]  
}

# add PMID to the citations csv:
cit <- cit[citations[, .(NID, PMID)], on = "NID"]
fwrite(cit, citations_path)

print(paste0("Enter the PMIDs from the csv at ", citations_path,
             " directly into PubMed. Download results according to screening sheet 
             directions on the HUB. Save the search results to ",
             paste0(out_path, time_stamp, "/pubmed_results_bundle", bundle_version_id,".txt"),
             " and then run the next line"))
readline(prompt="Press [enter] to continue AFTER completing the above")

# Create screening sheet --------------------------------------------------
ghdx_match(input_file = paste0(out_path, time_stamp, "/pubmed_results_bundle", bundle_version_id,".txt"),
           output_file = paste0("FILEPATH"),
           bundle_list = c(0),
           freeze_header = F)
