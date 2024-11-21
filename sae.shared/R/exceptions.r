#' @title looser_confidence_intervals_exception
#'
#' @description Exception for compile_estimates check: point estimates within confidence intervals
#' Sometimes this does not occur for more unstable estimates, but that does not mean that there large issues with the estimates
#'
#' @param dir [character] Cause directory
#' @param rake_to_gbd_version [list] GBD metadata
#' @param var [character] One of "mx", "yll"
#' @param est [data.table] Data table of draws to validate
#' @param issues [vector] List of issues.
#'
#' @return list of issues.
#'
#' @rdname looser_confidence_intervals_exception
#' @export
looser_confidence_intervals_exception <- function(dir, rake_to_gbd_version, var, est, issues) {
  dir_path <- strsplit(dir, "/")[[1]]
  acause <- dir_path[length(dir_path)]

  SPECIAL_CASE_CAUSES <- c("inj_homicide", "inj_homicide_knife", "inj_homicide_other", "inj_homicide_gun", "resp_pneum_coal", "mental_drug_amphet", "mental_drug_cocaine")
  SPECIAL_CASE_CAUSES_2021 <- c("_subs","mental_drug","mental_alcohol","mental_drug_opioids", "mental_drug_cocaine", "mental_drug_amphet", "mental_drug_other")
  
  gbd_7_test <- F
  if(is.null(rake_to_gbd_version$gbd_round_id)) {
    gbd_7_test <- T
  } else if(rake_to_gbd_version$gbd_round_id == 7) {
    gbd_7_test <- T
  }
  
  if(gbd_7_test) {
    
    # If we are dealing with particular causes and the number of rows where there are issues in < 300, let it pass
    if(acause %in% SPECIAL_CASE_CAUSES_2021 & est[, sum(!data.table::between(value_mean, value_lb, value_ub))] < 300) {
      return(issues)
    } else if(acause %in% c("hiv","hiv_other")) {
      # In Alaska, there are some unstable estimates in the oldest ages and some sexes and races
      
      tmp <- est[!data.table::between(value_mean, value_lb, value_ub)]
      if(nrow(tmp) > 1000) issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))

      if(nrow(tmp[level == "state"]) > 0) {
        if(length(unique(tmp[level == "state", area])) != 1) issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))
        if(!unique(tmp[level == "state", area]) == 2) issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))
      }

      if(nrow(tmp[age < 75]) != 0) issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))

      return(issues)
      
    } else if (est[, sum(!data.table::between(value_mean, value_lb, value_ub))] > 0) {
      issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))
    }
    
    return(issues)
  }
  
  if (acause %in% SPECIAL_CASE_CAUSES & rake_to_gbd_version$gbd_round_id == 6) {
    message("Skipping test of ", var, " > ub for ", paste(as.character(acause)))
    dir.create(file.path(dir, "../reports/agg_errors/"), recursive = T)
    saveRDS(est[value_mean > value_ub, ], file = file.path(dir, paste0("../reports/agg_errors/", var, "_agg_compile_", acause, ".rds")))
  } else if (acause %in% SPECIAL_CASE_CAUSES_2021  & rake_to_gbd_version$gbd_round_id == 7) {
    message("Skipping test of ", var, " > ub for ", paste(as.character(acause)))
    dir.create(file.path(dir, "../reports/agg_errors/"), recursive = T)
    saveRDS(est[value_mean > value_ub, ], file = file.path(dir, paste0("../reports/agg_errors/", var, "_agg_compile_", acause, ".rds")))
  } else if (est[, sum(!data.table::between(value_mean, value_lb, value_ub))] > 0) {
    issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))
  }
  return(issues)
}

