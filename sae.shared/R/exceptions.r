
#' @title substance_abuse_males_gbd6_cause
#'
#' @description Manual pass for children of _subs.
#' _subs (substance abuse) is modeled in GBD round 6 for males ages 1, 5, and 10, but USHD does not model it for these ages.
#' For _subs itself, we got around this by running a separate model to cover these ages. However, as we continued down the
#' cause tree we realized this was unsustainable. So, just for these causes, we want to set their values to "0" for this check,
#' with the understanding that the sum of mental_alcohol + mental_drug will NOT match the value of _subs for these results.
#' (_subs is level 2; mental_alcohol and mental_drug are level 3 causes.)
#'
#' @param rake_to_gbd_version [list] GBD metadata
#' @param this_parent [character] Cause you are checking.
#' @param est [data.table] Combined upper and lower geography draws.
#'
#' @return est data.table, with value_mean_upper set to 0 if applicable.
#'
#' @rdname substance_abuse_males_gbd6_cause
#' @export
substance_abuse_males_gbd6_cause <- function(rake_to_gbd_version, this_parent, est) {
  subs_subtree_mx <- c("_subs", "mental_alcohol", "mental_drug")
  subs_subtree_yll <- c("_subs")
  if(is.null(rake_to_gbd_version$gbd_round_id)) {
    return(est)
  }
  if (rake_to_gbd_version$gbd_round_id != 6) {
    return(est)
  }
  if (this_parent %in% subs_subtree_mx) {
    est[age %in% c(1, 5, 10) & sex == 1 & measure == "mx", parent_value := 0]
    stopifnot(nrow(est[measure == "mx" & child_value == 0 & parent_value != 0]) == 0) # Make sure you covered all upper-values of 0 for these causes.
  }
  if (this_parent %in% subs_subtree_yll) {
    est[age %in% c(1, 5, 10) & sex %in% c(1, 3) & measure == "yll", parent_value := 0]
    stopifnot(nrow(est[measure == "yll", child_value == 0 & parent_value != 0]) == 0) # Make sure you covered all upper-values of 0 for these causes.
  }


  return(est)
}

#' @title substance_abuse_males_gbd6_geo
#'
#' @description Manual pass for children of _subs.
#' _subs (substance abuse) is modeled in GBD round 6 for males ages 1, 5, and 10, but USHD does not model it for these ages.
#' For _subs itself, we got around this by running a separate model to cover these ages. However, as we continued down the
#' cause tree we realized this was unsustainable. So, just for these causes, we want to set their values to "0" for this check,
#' with the understanding that the sum of mental_alcohol + mental_drug will NOT match the value of _subs for these results.
#' (_subs is level 2; mental_alcohol and mental_drug are level 3 causes.)
#'
#' @param rake_to_gbd_version [list] GBD metadata
#' @param this_cause [character] Cause you are checking.
#' @param est [data.table] Combined upper and lower geography draws.
#'
#' @return est data.table, with value_mean_upper set to 0 if applicable.
#'
#' @rdname substance_abuse_males_gbd6_geo
#' @export
substance_abuse_males_gbd6_geo <- function(rake_to_gbd_version, this_cause, est) {
  subs_subtree_mx <- c("mental_alcohol", "mental_drug", "mental_drug_opioids", "mental_drug_cocaine", "mental_drug_amphet", "mental_drug_other")
  subs_subtree_yll <- c("mental_drug", "mental_drug_opioids")
  if(is.null(rake_to_gbd_version$gbd_round_id)) {
    return(est)
  }
  if (rake_to_gbd_version$gbd_round_id != 6) {
    return(est)
  }
  if (this_cause %in% subs_subtree_mx) {
    est[age %in% c(1, 5, 10) & sex == 1 & measure == "mx", value_mean_upper := 0]
    stopifnot(nrow(est[measure == "mx" & value_mean_lower == 0 & value_mean_upper != 0]) == 0) # Make sure you covered all upper-values of 0 for these causes.
  }
  if (this_cause %in% subs_subtree_yll) {
    est[age %in% c(1, 5, 10) & sex == 1 & measure == "yll", value_mean_upper := 0]
    stopifnot(nrow(est[measure == "yll", value_mean_lower == 0 & value_mean_upper != 0]) == 0) # Make sure you covered all upper-values of 0 for these causes.
  }
  return(est)
}

#' @title inj_homicide_yll_gbd6
#'
#' @description Exception for inj_homicide
#' For inj_homicide, several mortality-rate draws are zero in GBD, while the YLL draws
#' are positive. This creates issues, because USHD's raked mortality rates are used to generate YLLs,
#' and we end up with lower YLLs that are zero (generated from raked USHD data)
#' and upper draws that are positive (drawn from GBD). The solution is, when we see
#' this specific situation, to jitter the USHD values slightly so they are non-zero.
#'
#' @param rake_to_gbd_version [list] GBD metadata
#' @param raking_val [data.table] Data table of draws to rake.
#' @param measure [character] One of 'mx', 'yll'.
#' @param causes [data.table] Data.table of cause hierarchy.
#'
#' @return Data.table of draws, with YLL inj_homicide draws jittered to slightly more than 0.
#'
#' @rdname inj_homicide_yll_gbd6
#' @export
inj_homicide_yll_gbd6 <- function(rake_to_gbd_version, raking_val, measure, causes) {

  if(is.null(rake_to_gbd_version$gbd_round_id)) {
    return(raking_val)
  }
  if (rake_to_gbd_version$gbd_round_id != 6) {
    return(raking_val)
  }
  prefix <- causes[acause == "inj_homicide", path_to_top_parent]
  if (length(prefix) == 0) {
    # "inj_homicide" not in causes
    return(raking_val)
  }
  inj_homicide_subtree <- causes[grepl(prefix, path_to_top_parent), acause]
  if (nrow(raking_val[acause %in% inj_homicide_subtree & value == 0]) > 0 & measure == "yll") {
    print("Found rows where lower values were zero in the inj_homicide subtree. These numbers will be changed to slightly larger than 0 for raking (1e-20).")
    print(raking_val[acause %in% inj_homicide_subtree & value == 0])

    raking_val[acause %in% inj_homicide_subtree & value == 0, value := 1e-20]
  }
  return(raking_val)
}


#' @title looser_confidence_intervals_exception
#'
#' @description Exception for compile_estimates check: point estimates within confidence intervals
#' There are some causes in GBD round 6 where we need to perform a looser check here.
#' Specifically, in inj_homicide there are ~100 draws for men aged 60 in 2014
#' that are practically zero. When calculating the upper and lower bounds, we get zero
#' for both, but this causes an error with this check.
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

    # If we are dealing with particular causes and the number of rows where there are issues in < 100, let it pass
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

#' @title gbd7_version257_digest_upper_children
#'
#' @description  An exception for CodCorrect version 257 where age_group_id 28 returns NA values for children of digest_upper
#' But, age_group_id 389 can capture the mortality rates (which are 0 anyway)
#' Later versions of CodCorrect appear to not have this issue
#'
#' @param draws [data.table] GBD draws data.
#' @param draw_settings [list] named list specifying GBD settings.
#' @param cause_id [integer] cause_id/s of acause/s to check exception for.
#' @param measure [character] One of "mx", "yll" or "yld"
#'
#' @return data.table of draws.
#'
#' @rdname gbd7_version257_digest_upper_children
#' @export
gbd7_version257_digest_upper_children <- function(draws, draw_settings, cause_id, measure) {
  if(is.null(draw_settings)) {
    return(draws)
  }
  if(length(setdiff(cause_id, c(528, 527))) == 0 &&
     draw_settings$version == 257 &&
     draw_settings$gbd_round_id == 7 &&
     draw_settings$decomp_step == "step3" &&
     measure == "mx") {

    message(paste0("Applying correction to causes ", paste(cause_id,collapse=",")," to fill in 0s for age_group_id 28"))

    # add on 0 values for age_group_id 28
    non_draws_cols <- names(draws)[!(names(draws) %like% "draw_")]
    non_draws_cols <- non_draws_cols[non_draws_cols != "age_group_id"]
    draw_cols <- names(draws)[names(draws) %like% "draw_"]
    age_group_id_28_sub <- unique(draws[,(non_draws_cols), with=F])
    age_group_id_28_sub[,age_group_id := 28]
    draws <- rbind(draws, age_group_id_28_sub, fill=T)
    draws[age_group_id == 28, (draw_cols) := 0, with=T]

    stopifnot(nrow(draws[is.na(draw_1)]) == 0)
  }
  return(draws)
}

#' @title  inj_reporting_exception_cause_list
#'
#' @description An exception for particular CodCorrect runs that do not have the aggregated values for cause_id 1056 available.
#'
#' @param draw_settings [list] named list specifying GBD settings.
#' @param cause_id [character] cause_id/s of acause/s to check exception for.
#'
#' @return vector of cause_id/s.
#'
#' @rdname inj_reporting_exception_cause_list
#' @export
inj_reporting_exception_cause_list <- function(draw_settings, cause_id) {
  if(is.null(draw_settings)) {
    return(cause_id)
  }
  if(1056 %in% cause_id &&
     draw_settings$version == 257 &&
     draw_settings$gbd_round_id == 7 &&
     draw_settings$decomp_step == "step3") {

    message("Replacing cause_id 1056 with the internal causes 940 and 716")

    cause_id <- cause_id[cause_id != 1056]
    # Use the component internal causes instead
    # 940 = inj_electrocution
    # 716 = inj_othunintent
    cause_id <- c(cause_id, 940, 716)
  }
  return(cause_id)
}

#' @title inj_reporting_exception_aggregation
#'
#' @description Another exception related to inj_reporting_exception_cause_list where we now have to aggregate the draws
#' to get cause_id 1056
#'
#' @param draw_settings [list] named list specifying GBD settings.
#' @param cause_id [character] cause_id/s of acause/s to check exception for.
#' @param draws [data.table] GBD draws data.
#'
#' @return data.table of draws.
#'
#' @rdname inj_reporting_exception_aggregation
#' @export
inj_reporting_exception_aggregation <- function(draw_settings, cause_id, draws) {
  if(is.null(draw_settings)) {
    return(draws)
  }
  if(all(c(940, 716) %in% cause_id) &&
     draw_settings$version == 257 &&
     draw_settings$gbd_round_id == 7 &&
     draw_settings$decomp_step == "step3") {

    message("Aggregating the internal injuries causes (940, 716) to the reporting cause (1056)")

    draws[cause_id %in% c(940, 716), cause_id := 1056]
    draw_cols <- names(draws)[names(draws) %like% "draw_"]
    non_draws_cols <- names(draws)[!(names(draws) %like% "draw_")]
    draws<-draws[, (draw_cols) := lapply(.SD, function(x) sum(x)), .SDcols=draw_cols, by=non_draws_cols]
    draws <- unique(draws)
  }
  return(draws)
}

#' @title
#'
#' @description Differences between by-education model population and
#'   county-model population leads to differences in weights used in agg_sex.r,
#'   which leads to differences in raked results. We understand that when a
#'   county, year, age, has zero population across both sexes and every
#'   education group, raked all-education both sex estimates from the
#'   by-education model (the things that were raked) will not match raked both
#'   sex estimates from the county model (the raking target). We're okay with
#'   this. So, this exception allows these estimates not to match under the same
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
