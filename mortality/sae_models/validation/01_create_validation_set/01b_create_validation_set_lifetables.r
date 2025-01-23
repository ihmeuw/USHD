####################################################################################################
## Description: This is a worker script that is ran in parallel, for each sex,
## race, edu, and year. It is the second script in a set of three
## (start_validation_set.r, create_validation_set_lifetables.r,
## finish_valiation_set.r). See start_validation_set.r for details.
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))
suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  library(glue)
  R.utils::sourceDirectory('functions', modifiedOnly = FALSE)
}))


# Script arguments ----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- args[1]
output_dirname <- args[2]
sex <- args[3]
race <- args[4]
edu <- args[5]
year <- args[6]
growth_rate_file <- args[7]
allow_mx_0 <- as.logical(args[8])

message(
  glue(
    "Arguments:",
    "base_dir: {base_dir}",
    "output_dirname: {output_dirname}",
    "sex: {sex}",
    "race: {race}",
    "edu: {edu}",
    "year: {year}",
    .sep = "\n"
  )
)


# Get settings --------------------------------------------------------------------------------


message(glue("Reading settings from {base_dir}"))
get_settings(base_dir)

if (!lt_hc) {
  stop("lt_hc is FALSE")
}

base_dir = file.path(
  "FILEPATH",
  "FILEPATH"
)

final_output_dir <- file.path(base_dir, output_dirname)


# get data ------------------------------------------------------------------------------------


ex <- readRDS(file.path(
  final_output_dir,
  "intermediate_validation_set",
  "ex_input",
  glue("ex_input_data_{year}_{sex}_{race}_{edu}.rds")
))



# Run lifetables ------------------------------------------------------------------------------

all_gr <-
  readRDS(
    growth_rate_file
  )

all_gr <- all_gr[level == "mcnty"]
setnames(all_gr, "area", "mcnty")

# merge growth rates onto the data
ex <-
  merge(ex,
        all_gr,
        by = c("mcnty", "year", "sex", "race", "edu"),
        all.x = T)
stopifnot(nrow(ex[is.na(mean_gr)]) == 0)
gc()

message("Running first call to lifetable()...")
if (by_edu) {
  ex <-
    ex[, c(list(pop = sum(pop)), lifetable( 
      mx = mx,
      sex = sex[1],
      use_graduation = T, 
      extrap = F,
      lt_hc = lt_hc,
      gr = mean_gr,
      adult_ages_only = TRUE
    )[, list(age, mx, ax, qx, ex)]),
    by = "mcnty,race,edu,year,sex,sim"]
} else {
  ex <-
    ex[, c(list(pop = sum(pop)), 
           lifetable(
             mx = mx,
             sex = sex[1],
             use_graduation = T, 
             extrap = F,
             lt_hc = lt_hc,
             gr = mean_gr
           )[, list(age, mx, ax, qx, ex)]),
       by = "mcnty,race,edu,year,sex,sim"]
}
message("Done with first call to lifetable().")

if ("mean_gr" %in% names(ex)) {
  ex[, mean_gr := NULL]
}


# Clean lifetables ----------------------------------------------------------------------------



ex[, drop := any(is.na(ex)), by = "mcnty,race,edu,year,sex"]
ex[, drop := any(is.infinite(ex)), by = "mcnty,race,edu,year,sex"]
# drop will have value of TRUE if it should be dropped, so keep the rows where drop is not TRUE
ex <- ex[drop != TRUE,]
ex[, drop := NULL]


# Collapse draws ------------------------------------------------------------------------------

# collapse life table draws to get point estimates and CI

ex <- ex[, list(
  total_pop = pop[1],
  mx = mean(mx),
  ax = mean(ax),
  ex_se = sd(ex),
  ex_var = var(ex),
  ex_lb = quantile(ex, 0.025),
  ex_ub = quantile(ex, 0.975)
), keyby = "mcnty,race,edu,year,sex,age"]

# merge on the growth rates again
ex <-
  merge(ex,
        all_gr,
        by = c("mcnty", "year", "sex", "race", "edu"),
        all.x = T)
stopifnot(nrow(ex[is.na(mean_gr)]) == 0)


# isn't necessarily correct - not all the values necessarily align. To get them to align we must
# re-run lifetables(). Graduation is set to False because it's already be ran in the first
# lifetable() call. "We use draws of age-standardized mortality rates to generate draws of life
# tables (and thus life expectancy). We then calculate the mean of the mx draws and ax draws, and
# generate another life table based on these mean mx and ax vectors. Point estimates come from this
# final life table, and UIs come from quantiles across the draws."
message("Running second call to lifetable()...")
if (by_edu) {
  ex <-
    ex[, c("ex_mean") := lifetable(
      mx = mx,
      ax = ax,
      use_graduation = F, 
      extrap = F,
      gr = mean_gr,
      lt_hc = lt_hc,
      adult_ages_only = TRUE,
      skip_mx_0_check = allow_mx_0
    )[, list(ex)], by = "mcnty,year,sex,race,edu"]
} else {
  ex <-
    ex[, c("ex_mean") := lifetable(
      mx = mx,
      ax = ax,
      use_graduation = F, 
      extrap = F,
      gr = mean_gr,
      lt_hc = lt_hc,
      skip_mx_0_check = allow_mx_0
    )[, list(ex)], by = "mcnty,year,sex,race,edu"]
}
gc()

ex <-
  ex[, list(mcnty,
            year,
            sex,
            age,
            race,
            edu,
            ex_mean,
            ex_se,
            ex_var,
            ex_lb,
            ex_ub)]
gc()
setkeyv(ex, c("mcnty", "year", "sex", "race", "edu", "age"))


# Save ----------------------------------------------------------------------------------------

saveRDS(ex,
        file.path(
          final_output_dir,
          "intermediate_validation_set",
          "ex_output",
          glue("ex_output_{year}_{sex}_{race}_{edu}.rds")
        ))

message("DONE.")
