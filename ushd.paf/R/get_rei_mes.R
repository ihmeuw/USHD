#' Return USHD modelable entities
#'
#' These entities do not necessarily represent a complete set of entities, only
#' those specific to USHD. They are expected to be combined with a regular
#' get_rei_mes() result by replacing the rows in the get_rei_mes() call which
#' have the same draw_type
#'
#' @details
#' This is a function internal to the paf codebase we need to carefully alter.
#' It is used in several places:
#' - exp_max_min.R depends on getting rows for draw_type "exposure" + "exposure_sd"
#' - launch_paf.R errors if there are rows with an NA model_version_id excepting draw_type of paf/paf_unmediated
#'
#' @return data.table with columns rei_id, modelable_entity_id, modelable_entity_name, draw_type, exp_categ,
#'   and model_version_id for the three draw types exposure, exposure_sd, and paf_unmediated
ushd.get_rei_mes <- function(rei_id) {
  # helper function to get the value passed as model_version_id in our result
  get.exposure.mvid <- function(modelable_entity_id) {

    if (!rei_id %in% c(370, 86, 105)) { # REIs for BMI, ambient PM2.5, and FPG, respectively
      stop(glue::glue("rei_id = {rei_id} not currently supported by get_rei_mes.R"))
    }
    tryCatch(
      # on success return the model_exp_run_id
      ushd.dbr::get_risk_exp_model_run(modelable_entity_id = modelable_entity_id
                                       )$model_exp_run_id,
      # on error just return NA_integer_
      error = function(e) NA_integer_)
  }

  # these are in the epi.modelable_entity table
  # their modelable_entity_type_id is 23 which is a generic
  # "other"/"Modelable entities used for non-GBD estimation"
  #
  # We could use a SQL LIKE e.g., WHERE modelable_Entity_name LIKE "% [USHD]"
  if (rei_id == 370) { # BMI
    # Adult mean BMI [USHD] - 27160
    # Adult BMI standard deviation [USHD] - 27161
    tmp_mei <- data.table(
      rei_id = 370,
      modelable_entity_id = c(27160, 27161, 27162),
      modelable_entity_name = c("Adult mean BMI [USHD]", "Adult BMI standard deviation [USHD]", "Adult high body-mass index unmediated PAF [USHD]"),
      draw_type = c("exposure", "exposure_sd", "paf_unmediated"),
      exp_categ = NA_character_
    )
    # pull the model version IDs
    # Do this separately because there may be multiple model versions for the same MEI (in the case
    # where we have raked and unraked mean BMI)
    tmp_model_version_ids <- rbindlist(lapply(tmp_mei$modelable_entity_id, function(mei) data.table(modelable_entity_id = mei, model_version_id = as.integer(get.exposure.mvid(mei)))))
    # merge with the other MEI info
    return(merge(tmp_mei, tmp_model_version_ids, by = "modelable_entity_id"))

  } else if (rei_id == 105 ) { # FPG
    # 32615	Prevalence of diabetes (FPG >= 7 mmol/L or treated) [USHD]
    tmp_mei <- data.table(
      rei_id = 105,
      modelable_entity_id = c(32615),
      modelable_entity_name = c("Prevalence of diabetes (FPG >= 7 mmol/L or treated) [USHD]"),
      draw_type = c("exposure"),
      exp_categ = NA_character_
    )
    tmp_model_version_ids <- rbindlist(lapply(tmp_mei$modelable_entity_id, function(mei) data.table(modelable_entity_id = mei, model_version_id = as.integer(get.exposure.mvid(mei)))))
    # merge with the other MEI info
    return(merge(tmp_mei, tmp_model_version_ids, by = "modelable_entity_id"))
  } else if (rei_id == 86) { # Ambient PM2.5 pollution
    tmp_mei <- data.table(
      rei_id = 86,
      modelable_entity_id = c(27657, 27660),
      modelable_entity_name = c("Ambient particulate matter (PM2.5) pollution population-weighted (micrograms/cubic meter) [USHD]", "Ambient particulate matter (PM2.5) pollution unmediated PAF [USHD]"),
      draw_type = c("exposure", "paf_unmediated"),
      exp_categ = NA_character_
    )
    # pull the model version IDs
    # Do this separately because there may be multiple model versions for the same MEI (in the case
    # where we have raked and unraked mean exp)
    tmp_model_version_ids <- rbindlist(lapply(tmp_mei$modelable_entity_id, function(mei) data.table(modelable_entity_id = mei, model_version_id = as.integer(get.exposure.mvid(mei)))))
    # merge with the other MEI info
    return(merge(tmp_mei, tmp_model_version_ids, by = "modelable_entity_id"))
  }
  else {
    stop(sprintf("ushd.get_rei_mes does not support rei_id %s", rei_id))
  }

}
