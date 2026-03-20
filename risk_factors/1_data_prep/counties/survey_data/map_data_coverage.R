###################################################################################################
## Description: 1) Map which counties have identifiers in BRFSS, and for which locations the data
##                 are only available as a state remainder. Show metro areas with identifiers.
##              2) Identify any counties that fall in multiple metropolitan areas in the same year.
##
## Inputs: BRFSS microdata which contains information about the file for each observation:
##         FILEPATH 
##         Note: This function is sourced from prep_brfss; use the output_dir variable generated in
##               prep_brfss.R to obtain the correct time stamp.
##               
##         mcnty, cbsa, and state shapefiles in FILEPATH
##
## Output: FILEPATH
##          
###################################################################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sf)
library(RColorBrewer)
library(ggforce) 
tibble.print_max = Inf

# Load data/shapefiles ----------------------------------------------------------------------------
mcnty_shape <- readRDS("FILEPATH")
mcnty_shape <- as(mcnty_shape, "sf")
mcnty_shape <- mcnty_shape %>% rename(mcnty_geo = geometry)
state_shape <- readRDS("FILEPATH")
state_shape <- as(state_shape, "sf")
state_shape <- state_shape %>% dplyr::rename(state_geo = geometry)

if(sys.nframe() == 0) {  # only read in data if this function is not sourced; otherwise, use "data" in the global environment
  data <- readRDS("FILEPATH")
}

data <- data[, cbsa := as.integer(cbsa)]

# For each year, prepare data for mapping ---------------------------------------------------------
years <- min(data$year):max(data$year)
plot_list <- vector(mode = "list", length = length(years))

# load mcnty/cbsa crosswalk
cw <- readRDS("FILEPATH") # produced in FILEPATH

for (i in 1:length(years)) {
  a_year = years[[i]]
  print(a_year)
  
  if (a_year >= 2013) {
    # load CBSA shapefile for a_year (since we don't have a geographically-stable CBSA shape, like we do for cnty/mcnty)
    shape_yr <- case_when(
      a_year %in% 2013:2014 ~ "2013_2014",
      a_year %in% 2015:2016 ~ "2015_2016",
      a_year %in% 2017 ~ "2017",
      a_year %in% 2018:2019 ~ "2018_2019",
      a_year %in% 2020:2022 ~ "2020_2021_2022",
      a_year %in% 2023 ~ "2023"
    )
    cbsa_shape <- readRDS(paste0("FILEPATH")) %>% rename(cbsa_geo = geometry)
    
    # subset to a_year and only the relevant vars
    dat <- data[year == a_year, .(year, state, mcnty, cbsa)]
  } else {
    dat <- data[year == a_year, .(year, state, mcnty)]
  }
  
  # remove individual rows for respondents
  if (a_year >= 2013) {
    dat <- dat[!is.na(mcnty) | !is.na(cbsa)]  # rows in data should have non-missing mcnty or cbsa codes (CHECK THIS LINE)
  } else {
    dat <- dat[!is.na(mcnty)]
  }
  dat <- unique(dat)
  
  # merge data with mcnty, cbsa, and state shapefiles
  dat <- as.data.table(dplyr::right_join(mcnty_shape[, c("mcnty_geo", "mcnty")], dat, by = "mcnty"))
  if (a_year >= 2013) { 
    dat <- as.data.table(dplyr::right_join(cbsa_shape[, c("cbsa_geo", "cbsa")], dat, by = "cbsa")) # just right join since we only want places where CBSAs have data (since it will be a layer on top of counties)
  }
  dat <- as.data.table(dplyr::full_join(state_shape[, c("state_geo", "state")], dat, by = "state"))  # full join to see which states are missing
  
  # create indicator of mcnty and cbsa suppression and state missingness
  dat[, mcnty_dat := ifelse(is.na(mcnty), "0", "1")] # use cnty as an indicator of if the mcnty is suppressed or not because cnty will only be not-NA if not suppressed, whereas mcnty is merged with the shapefile so won't be NA
  if (a_year >= 2013) {
    dat[, cbsa_dat := ifelse(is.na(cbsa), "0", "1")]
  }
  dat[, state_dat := ifelse(is.na(year), "0", "1")]  # year will be NA from full join with state shapefile if state is not present in data
  
  # map where county data available
  num_mcnty_not_suppressed <- sum(unique(dat[, .(mcnty, mcnty_dat)])$mcnty_dat == "1")
  num_mcnty <- uniqueN(mcnty_shape$mcnty)
  
  if (a_year >= 2013) {
    num_cbsa_not_suppressed <- sum(unique(dat[, .(cbsa, cbsa_dat)])$cbsa_dat == "1")
    num_cbsa <- uniqueN(cbsa_shape$cbsa)
    
    # look at CBSA in data that did not merge with shapefile
    dat[!is.na(cbsa) & is.na(st_dimension(cbsa_geo))]
    t <- unique(dat[!is.na(cbsa) & is.na(st_dimension(cbsa_geo)), cbsa])

    # make sure there is a shapefile corresponding to each CBSA
    if(length(t) > 0){
      print("NOT ALL CBSAs MERGED CORRECTLY:")
      print(t)
      print(unique(data[cbsa %in% t, cbsa_name]))  
    }
    
    # CHECK: are there mcntys in multiple CBSAs/MDs (for which we have SMART data)?
    # method: count how many unique combinations of CBSAs and mcntys exist; if there are more
    #         unique combos than unique mcntys, we know that some mcntys are in multiple CBSAs,
    #         which is an issue for our analysis. Do this separately for CBSAs and metro division.
    
    # get CBSAs for which we have data
    cbsas <- unique(dat[!is.na(cbsa), cbsa])
    
    # get unique combos of mcnty and relevant CBSAs; see if any duplicated mcntys
    dups <- cw[!is.na(cbsa) & !is.na(mcnty) & year == a_year & cbsa %in% cbsas] %>% 
      as.data.frame() %>%
      select(c(mcnty, cbsa)) %>%
      unique() %>%
      select(mcnty) %>%
      duplicated()
    
    if (!a_year %in% unique(cw[, year])) {
      print("See previous year's crosswalk; CBSAs did not change this year.")
    } else if (sum(dups) > 0) {
        dup_mcnty <- cw[!is.na(cbsa) & !is.na(mcnty) & year == a_year & cbsa %in% cbsas] %>% 
        as.data.frame() %>%
        dplyr::select(c(mcnty, cbsa)) %>%
        unique() %>%
        dplyr::filter(dups) %>%
        dplyr::select(mcnty) %>%
        unlist()
      print(paste("There is/are", sum(dups), "mcnty(s) that fall into multiple CBSAS:"))
      temp <- cw %>%
        as.data.frame() %>%
        dplyr::filter(mcnty %in% dup_mcnty & year == a_year & cbsa %in% cbsas) %>%
        dplyr::select(year, cnty, cnty_name, mcnty, cbsa, CBSA.Title, is_metropolitan_division, Parent.CBSA.Code) %>% 
        dplyr::rename(is_md = is_metropolitan_division) %>% tidyr::as_tibble() %>%
        print(n = 50, tibble.width = Inf)
    } else {
      print("There are NO mcntys that fall into multiple CBSAs")
    }
  }
  
  # look at mcnty in data that did not merge with shapefile
  dat[!is.na(mcnty) & is.na(st_dimension(mcnty_geo))]
  t <- unique(dat[!is.na(mcnty) & is.na(st_dimension(dat$mcnty_geo)), mcnty])
  if (length(t) > 0) {
    print("NOT ALL MCNTYs MERGED CORRECTLY:")
    print(t)
  }
  
  # report on states not included in the given year
  missing_states <- sort(unique(dat[is.na(year), state]))
  if (length(missing_states) > 0) {
    print(paste0("The following state FIPS are not included in the ", a_year, " survey:"))
    print(missing_states)
  }
  
  # create basemap of blank states, no outlines, and default plot title/subtitle
  map <- ggplot() +
    geom_sf(data = state_shape, mapping = aes(geometry = state_geo),
            fill = "white", color = "transparent", lwd = 0.05, alpha = 0.8) +
    coord_sf(expand = F) +
    theme_bw(base_size = 10) +
    labs(title = paste("Level of geographic data", a_year),
         subtitle = paste(num_mcnty_not_suppressed, "/", num_mcnty, "MCNTYs unsuppressed (at least partially)")) +
    theme(legend.title=element_blank())
  
  # some mcntys are duplicated for both cbsa_dat = 0 and cbsa_dat = 1; make this temp dataset to get the unique mcntys only
  mcnty_tmp <- unique(dat[mcnty_dat == "1", .(mcnty)])
  mcnty_tmp <- as.data.table(dplyr::right_join(mcnty_shape[, c("mcnty_geo", "mcnty")], mcnty_tmp, by = "mcnty"))
  
  # some CBSAs are duplicated when they contain multiple mcntys; make this temp dataset to get the unique CBSAs only
  if (a_year >= 2013) {
    cbsa_tmp <- unique(dat[cbsa_dat == "1", .(cbsa)])
    cbsa_tmp <- as.data.table(dplyr::right_join(cbsa_shape[, c("cbsa_geo", "cbsa")], cbsa_tmp, by = "cbsa"))
  }
  
  # add geographic detail and outlines in layers
  if (a_year < 2013) {
    map <- map + geom_sf(data = dat[state_dat == "1"], aes(geometry = state_geo, fill = "#ffb59e"),  # first layer = state-level detail, then
                         color = "gray12", lwd = 0.05) +
                 geom_sf(data = mcnty_shape, aes(geometry = mcnty_geo), fill = "transparent", lwd = 0.01, color = "gray85") +  # mcnty outlines, then
                 geom_sf(data = mcnty_tmp, aes(geometry = mcnty_geo, fill = "#de2d26"),  # mcnty-level detail, then
                         color = "transparent", lwd = 0.0125, alpha = 0.6) +
                 geom_sf(data = state_shape, mapping = aes(geometry = state_geo),  # state outlines
                         fill = "transparent", color = "gray12", lwd = 0.05, alpha = 0) +
                 scale_fill_identity(guide = "legend", labels = c("mcnty", "state"), breaks = c("#de2d26", "#ffb59e"))
  } else {
    map <- map + geom_sf(data = dat[state_dat == "1"], aes(geometry = state_geo, fill = "#ffb59e"),  # first layer = state-level detail, then
                         color = "transparent", lwd = 0.05) +
                 geom_sf(data = mcnty_shape, aes(geometry = mcnty_geo), fill = "transparent", lwd = 0.01, color = "gray85") +  # mcnty outlines, then
                 geom_sf(data = cbsa_tmp, aes(geometry = cbsa_geo, fill = "#2b8cbe"),  # CBSA-level detail, then
                         color = "transparent", lwd = 0.025, alpha = 0.8) +
                 geom_sf(data = mcnty_tmp, aes(geometry = mcnty_geo, fill = "#de2d26"),  # mcnty-level detail, then
                         color = "transparent", lwd = 0.0125, alpha = 0.6) +
                 geom_sf(data = state_shape, mapping = aes(geometry = state_geo),  # state outlines
                         fill = "transparent", color = "gray12", lwd = 0.05, alpha = 0) +
                 scale_fill_identity(guide = "legend", labels = c("mcnty", "CBSA", "state"),
                                     breaks = c("#de2d26", "#2b8cbe", "#ffb59e")) +
                 labs(title = paste("Level of geographic data", a_year),
                      subtitle = paste(num_mcnty_not_suppressed, "/", num_mcnty, "MCNTYs unsuppressed (at least partially) \n",
                                       num_cbsa_not_suppressed, "/", num_cbsa, "CBSAs + Metropolitan Divisions unsuppressed"))
      
  }
  
  plot_list[[i]] <- map
}

# Save output into PDF ----------------------------------------------------------------------------
# see https://stackoverflow.com/questions/20500706/saving-multiple-ggplots-from-ls-into-one-and-separate-files-in-r
pdf(paste0(output_dir, "brfss_data_coverage.pdf"))
invisible(lapply(plot_list, print))
dev.off()
