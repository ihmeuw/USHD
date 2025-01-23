####################################################################################################
## Description: Prep draws of misclassification ratios from X. Reads in the ratios
##              and standard errors that have been manually extracted
##
## Requires:    Extracted ratios/standard errors from the paper: table 4 https://www.cdc.gov/nchs/data/series/sr_02/sr02_151.pdf
##
## Outputs:     prepped draws for each mcnty/edu/sex/age/year (combined_misclassification_ALL_[n.sims]_draws)
##
####################################################################################################

setwd(file.path("FILEPATH", Sys.info()['user'], "FILEPATH"))
stopifnot(grepl("mortality/sae_models$", getwd()))

library(crosswalk002, lib.loc = "FILEPATH")

# standard libraries
library(R.utils)
library(data.table)
library(ggplot2)
library(mgsub)
library(glue)



# sourcing the USHD database library, instead of sourcing the entire functions
# directory, you must source only the functions that are need.
source("FILEPATH/settings.r")
source("FILEPATH/constants.r")

# ggplot theme
theme_set(theme_minimal())
theme_update(plot.caption = element_text(hjust = 0)) # left-justify any captions for all plots

# just use an example directory to get the ages we want
dir <- "FILEPATH"
get_settings(dir)
n.sims <- 1000
plot <- T # if you want to produce a few plots along the way to make sure everything looks okay
log_transform <- T # if you want to log_transform transform

loc <- fread('FILEPATH/merged_counties.csv')
date_time_stamp <- format(Sys.time(), "%Y_%m_%d")


# metadata
edu_map <- data.table(
  edu = c(101, 102, 103, 104),
  edu_label = c("Less than HS",
                "HS graduate",
                "Some college",
                "College graduate")
) 
sex_map <-
  data.table(sex_name = c("Male", "Female", "both"),
             sex = c(1, 2, 3))


### --- misclassification by age/sex
ratios <- fread("FILEPATH/edu_misclassification_ratios_rostron.csv")

# apply our education groups to ratios
ratios[education == "Less than high school graduate", edu_label := "Less than HS"]
ratios[education == "High school graduate", edu_label := "HS graduate"]

ratios_some_college <- data.table::copy(ratios[education == "More than high school graduate", ])
ratios_college <- data.table::copy(ratios[education == "More than high school graduate", ])
ratios <- ratios[education != "More than high school graduate", ]
ratios_some_college[, edu_label := "Some college"]
ratios_college[, edu_label := "College graduate"]
ratios <- rbindlist(
  list(
    ratios,
    ratios_some_college,
    ratios_college
  ), use.names = TRUE
)
rm(ratios_some_college, ratios_college)

if (any(is.na(ratios$edu_label))) stop("ratios$edu_label has NA values.")
ratios[, education := NULL]
ratios <- merge(ratios, edu_map, by = "edu_label", all.x = T)

stopifnot(setequal(unique(ratios$edu), unique(edu_map$edu)))

# add sex column
ratios <- merge(ratios, sex_map, by = "sex_name", all.x = T)
if (any(is.na(ratios))) stop("NA values in ratios")

if (log_transform) {
  message("Log transforming the ratios...")
  ratios[, c("ratio", "se")] <-
    as.data.table(
      crosswalk002::delta_transform(
        mean = ratios$ratio,
        sd = ratios$se,
        transformation = 'linear_to_log'
      )
    )
  message("Done")
}


draws <- data.table()
# generate draws
for (line in 1:nrow(ratios)) {
  r <- ratios[line, ratio]
  se <- ratios[line, se]
  e <- ratios[line, edu]
  s <- ratios[line, sex]

  if (log_transform) {
    message("Getting draws from Log Normal Distribution...")
    sample.draws <- rlnorm(n.sims, r, se) 
  } else {
    message("Getting draws from Normal Distribution...")
    sample.draws <- rnorm(n.sims, r, se)
  }

  temp <- data.table(
    edu = e,
    sex = s,
    sim = seq(1, n.sims),
    ratio = sample.draws
    )

  stopifnot(nrow(temp) == n.sims)

  if (any(is.na(temp))) {
    stop(glue::glue("temp has NA values. line = {line}, sex = {s}, edu = {e}"))
  }

  # append to draws
  draws <- rbind(draws, temp)
  rm(temp)
}
if (any(is.na(draws))) stop("draws has NA values")
stopifnot(setequal(unique(draws$edu), unique(edu_map$edu)))


# format draws
draws <- merge(draws, sex_map, by = "sex", all.x = T)
draws <- merge(draws, edu_map, by = "edu", all.x = T)
draws <- draws[, race := all_pop_id]
if (any(is.na(draws))) stop("draws has NA values")

if (plot) {

  pdf(glue("FILEPATH"), width = 17, height = 8)


  if (log_transform) {
    log_transform_label <- "Log Normal"
  } else {
    log_transform_label <- "Normal"
  }


  p <- ggplot(draws, aes(sex_name, ratio, fill = sex_name)) +
    facet_wrap( ~ edu_label, ncol = 2, scales = "free_y") +
    geom_violin(trim = F) +
    labs(
      title = "Violin plot of education misclassification ratio draws",
      subtitle = glue::glue("Draws are from {log_transform_label} distribution"),
      caption = "Made by generate_misclassification_draws_edu.r"
    )
  plot(p)

  p <- ggplot(draws, aes(ratio)) +
    geom_density() +
    geom_vline(
      data = ratios,
      aes(xintercept = exp(ratio)),
      alpha = 0.5,
      linetype = "dashed"
    ) +
    facet_grid(sex_name ~ edu_label, scales = "free") +
    labs(
      title = "Density plots of education misclassification ratio draws",
      subtitle = glue::glue("Draws are from {log_transform_label} distribution"),
      caption = "Made by generate_misclassification_draws_edu.r"
    )
  plot(p)

  dev.off()
}

# remove formatting
draws[, sex_name := NULL]
draws[, edu_label := NULL]

setcolorder(draws, neworder = c("race", "edu", "sex", "sim", "ratio"))

# save
saveRDS(draws, "FILEPATH")

