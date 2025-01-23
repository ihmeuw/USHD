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
    dir.create(file.path(dir, "FILEPATH"), recursive = T)
    saveRDS(est[value_mean > value_ub, ], file = file.path(dir, paste0("FILEPATH", var, "_agg_compile_", acause, ".rds")))
  } else if (acause %in% SPECIAL_CASE_CAUSES_2021  & rake_to_gbd_version$gbd_round_id == 7) {
    message("Skipping test of ", var, " > ub for ", paste(as.character(acause)))
    dir.create(file.path(dir, "FILEPATH"), recursive = T)
    saveRDS(est[value_mean > value_ub, ], file = file.path(dir, paste0("FILEPATH", var, "_agg_compile_", acause, ".rds")))
  } else if (est[, sum(!data.table::between(value_mean, value_lb, value_ub))] > 0) {
    issues <- c(issues, paste("Point estimates of", var, "are outside confidence bounds"))
  }
  return(issues)
}

#' @title
#'
#' @description Differences between by-education model population and
#'   county-model population leads to differences in weights used in agg_sex.r,
#'   which leads to differences in raked results. We understand that when a
#'   county, year, age, has zero population across both sexes and every
#'   education group, raked all-education both sex estimates from the
#'   by-education model (the things that were raked) will not match raked both
#'   sex estimates from the county model (the raking target).
#'   So, this exception allows these estimates not to match under the same
#'   conditions that the difference arises: when population is zero across both
#'   sexes and every education group within a county, year, and age. The zero
#'   population causes a fallback weight to be used in sex aggregation that is
#'   formed across all ages, and the by-edu model uses ages 25+, while the
#'   county model uses every age, so the sex aggregation weights are different,
#'   so the sex-aggregated and raked estimates are different. This function can
#'   be run on the whole data.table of draws, or just the rows that have been
#'   put in the failures data.table.
#'
#' @param population
#' @param draws
#'
#' @return data.table of estimates with column zero_pop_sex_agg_exception added
#' @export
#'
#' @example failures <- exception_zero_pop_sex_agg_raking_diff(failures)
exception_zero_pop_sex_agg_raking_diff <- function(est) {
  # save for testing
  start_cols <- names(est)
  start_nrows <- nrow(est)

  pop <- load_population(pop_file)
  setnames(pop, "mcnty", "area")
  pop[, level := "mcnty"]

  # sum across edu and sex, because our condition for this exception is based on
  # all-edu both-sex pop totals
  pop <- pop[, .(pop = sum(pop)), by = "level,area,year,age"]

  # make an all-ages pop. Not really necessary but doing this makes any
  # unexpected NA values more obvious.
  pop <- rbindlist(list(pop,
                        pop[, .(age = 99, pop = sum(pop)), by = "level,area,year"],
                        pop[, .(age = 98, pop = sum(pop)), by = "level,area,year"]),
                   use.names = T)


  # merge on pop
  est <-
    merge(est,
          pop,
          by = c('level', 'area', 'year', 'age'),
          all.x = T) # LEFT merge filters population for us
  if (any(is.na(est))) {
    stop("Unexpected NAs in est")
  }

  est[, zero_pop_sex_agg_exception := ifelse(pop == 0 & sex == 3, yes = TRUE, no = FALSE)]

  # we believe that this should only happen when all-edu both-sex pop is zero.
  # The all-edu part is implicit, every row of est is all-edu. But we can test
  # the both-sex part
  if (nrow(est[zero_pop_sex_agg_exception == T & sex != 3]) > 0) {
    stop(
      "Unexpected values of TRUE in zero_pop_sex_agg_exception outside of both-sexes combined (sex 3)."
    )
  }

  est[, pop := NULL]

  # no difference in columns except for the new column
  stop.if.not.expected.cols(est, expected = start_cols, extra.ok = TRUE)
  stopifnot(nrow(est) == start_nrows)
  if (any(is.na(est))) {
    stop("Unexpected NAs in est")
  }
  if ("pop" %in% names(est)) {
    stop("'pop' column is still present in est.")
  }

  rm(pop)

  return(est)
}
