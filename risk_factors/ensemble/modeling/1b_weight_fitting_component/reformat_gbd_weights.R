#### Make GBD high BMI ensemble weights into format expected for USHD

library(lbd.loader, lib.loc = sprintf("FILEPATH", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))

library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

# upload to USHD db
stop("I think this script is no longer needed. USHD produces its own ensemble weights now.")
verson <- save_ensemble_weights(
  "FILEPATH",
  version_name = "GBD2020_weights",
  version_type = "neither",
  gbd_round_id = 7,
  prev_issues = "Uploading so that can call GBD weights using USHD approach",
  description = "Ensemble weights used for GBD 2020 metab_bmi_adult"
)

wt_dbr <- get_ensemble_weights(
  model_run_name = NULL,
  version_type = NULL,
  ensemble_weight_version_ids = as.list(verson),
  get_best = F
)

# check that the weight sets are the same
wt_disk <- fread("FILEPATH")

stopifnot(all.equal(wt_dbr[, names(wt_disk), with = F], wt_disk))