.datatable.aware = TRUE
#' Return USHD model results for a model.
#'
#' @details
#' Returns columns:
#'  modelable_entity_id [integer]
#'  model_version_id [integer]
#'  bundle_id [integer]
#'  crosswalk_version_id [integer]
#'  location_id [integer]
#'  year_id [integer]
#'  age_group_id [integer]
#'  sex_id [integer]
#'  measure_id [integer]
#'  measure [character]
#'  mean [double]
#'  lower [double]
#'  upper [double]
#'
#'
ushd.get_model_results <- function(model_exp_run_id, age_group_id, location_id, year_id, sex_id, modelable_entity_id, extra_dim, extra_dim_values, rei_id) {
  
  # helper function for optional arguments
  list.or.NULL <- function(val) if (is.null(val)) NULL else as.list(val)

  res = get_risk_exp_model_summary_data(
    years = list.or.NULL(year_id),
    sex = list.or.NULL(sex_id),
    edu = if (extra_dim == 'edu') as.list(extra_dim_values) else NULL,
    race = if (extra_dim == 'race') as.list(extra_dim_values) else NULL,
    location_id = as.list(location_id),
    # database column age_group_id actually contains USHD age groups e.g., 20, 25
    model_exp_run_id = model_exp_run_id
  )
  res <- res[age < 98] # remove all-age and age-standardized, since this messes up the age group translation step below
  # 2. Remove excess data not necessary for later operations
  res[, `:=`(pred_se = NULL, summary_type = NULL)]
  
  # 3. Transform data identifiers
  res <- ushd.add_age_group_id(res)
  
  warning("ushd.get_model_results: WARNING! not translating location_id values or filtering them")
  res[, location_id := area]
  
  setnames(
    res,
    c("sex",    "year",    "pred_mean", "pred_lb", "pred_ub"),
    c("sex_id", "year_id", "mean",      "lower",   "upper"))
  
  # 4. Remove data
  res[, `:=`(age = NULL, level = NULL, area = NULL)]
  
  # 5. Add additional columns necessary for the schema to align
  res[, measure_id := 19]
  res[, measure := "continuous"]
  
  res[, modelable_entity_id := modelable_entity_id]
  res[, model_version_id := model_exp_run_id]
  res[, bundle_id := 0xFACADE]
  res[, crosswalk_version_id := 0xFACADE]
  
  return(res)
}

