#' Various functions that support cause raking 
#'
#' Confirm that the margins across the upper draws (parent cause and upper geography)
#' add up, ensuring a successful raking operation. If they do not, that means our rake
#'
#' @param data dataframe with all draws
#' @param geography_value_col name of the column with the upper geography mortality rate. Ex: 'natl_value'
#' @param parent_value_col name of the column with the parent cause mortality rate. Ex: 'parent_value'
#' @param geography_demographics columns detailing the demographics of the upper geography draws
#' @param parent_cause_demographics columns detailing the demographics of the parent cause draws
#'
#' @export
confirm_equal_margins <- function(data, geography_value_col, parent_value_col,
                                  geography_demographics, parent_cause_demographics) {
  data <- copy(data)
  data[, weighted_parent := sum(get(parent_value_col) * pop_weight), by = geography_demographics]

  # Sum over the different causes for the same county
  data[, summed_geog := sum(get(geography_value_col)), by = parent_cause_demographics]
  data[, diff := abs(weighted_parent - summed_geog)]
  data[, percent_diff := abs((weighted_parent - summed_geog) / ((weighted_parent + summed_geog) / 2)) * 100]

  # Tolerance is in percentage point space (percent difference)
  discrepancy <- max(abs(data$diff))
  tolerence <- 1e-10
  percent_diff <- max(data$percent_diff)

  if (is.nan(discrepancy)) {
    stop_or_quit(
      "Something went wrong... NaN found in marginals.",
      status = 13
    )
  }

  if (discrepancy > tolerence) {
    msg <- paste0("Marginals do not add up prior to raking! \nDifference of ",
                  discrepancy, " found (", percent_diff, "%). Tolerence: ", tolerence)
    stop_or_quit(msg, status = 17)
  } else {
    message(paste0("Marginals have difference of ", discrepancy, " ( ", percent_diff, "%)."))
  }
}

#' Calculate the cause fraction represented by the modeled child causes
#' and then remove the excluded child causes from upper draws in the cause that some child causes are not modeled.
#'
#' @param data dataframe with all draws
#' @param cause_df dataframe with cause metadata for the child causes
#' @param raking_area_var column that defines the most detailed location(s) for the upper geography draws
#' @param shared_demographics demographics shared by all draws
#'
#' @return list with two named items: 'data', a revised version of data, with the draws from the excluded
#'         cause(s) dropped; 'removed_draws', a dataframe with draws from the removed cause(s) with cause
#'         fractions calculated across all excluded causes
#'
#' @export
remove_excluded_cause_draws <- function(data, causes_df, raking_area_var, shared_demographics) {
  data <- copy(data)

  # Calculate cause fraction
  val_col <- paste0(raking_area_var, "_value")
  # First, determine if the mortality rates are 0 for every single cause, in which cause the cause fraction
  # should just be 0 instead of NaN
  data[, total_val := sum(get(val_col)), by = c(raking_area_var, shared_demographics)]
  data[total_val == 0, cf := 0]
  data[total_val != 0, cf := get(val_col) / total_val]
  
  data[, total_val := NULL]
  data <- merge(data, causes_df[, list(acause, run_model)], by = "acause")

  removed_draws <- data[run_model == 0, c(raking_area_var, shared_demographics, "acause", "cf"), with = F]

  # Clean up
  data <- data[run_model == 1, ]
  data[, c("run_model", "cf") := NULL]

  if (nrow(data) == 0) {
    stop("Upper geographic draws has no rows after removing excluded causes. ",
         "Probable cause: run_model is set to 0 for all child causes.")
  }

  return(list("data" = data, "removed_draws" = removed_draws))
}

#' If some child causes are excluded from raking,
#' remove the cause fraction represented by these causes from the parent cause (mortality/yll) rate.
#' Namely, multiply the parent value (mx/yll) by the cause fraction of the remaining causes calculated for the upper geography
#'
#' @return data with column 'parent_value' scaled by remaining cause fractions and
#'         column 'parent_value_total' with the original parent mx values
#'
#' @export
scale_down_parent_val <- function(data, removed_draws, raking_area_var, shared_demographics) {
  data <- copy(data)

  # Collapse removed (excluded) upper draws by acause,
  # inverting cause fraction to CF represented by included draws
  collapsed_removed_draws <- removed_draws[, list(cf = 1 - sum(cf)), by = c(raking_area_var, shared_demographics)]
  data <- merge(data, collapsed_removed_draws, by = c(raking_area_var, shared_demographics), all.x = TRUE)

  data[, parent_value_total := parent_value]
  data[, parent_value := cf * parent_value]
  data[, cf := NULL]

  return(data)
}

#' Calculate excluded child causes from their cause fractions post-raking
#'
#' @param data dataframe with all draws
#' @param removed_draws draws of excluded causes from upper geography draws. Exactly 'removed_draws'
#'        from `remove_excluded_cause_draws`
#' @param causes_df dataframe with cause metadata for the child causes
#' @param lower_demographics colunms deatiling the most detailed demographics
#' @param geography_demographics columns detailing the demographics of the upper geography draws
#' @param geoagg_file File that includes geography scaling information.
#'
#' @export
add_excluded_cause_draws <- function(data, removed_draws, causes_df, lower_demographics, geography_demographics, geoagg_file) {
  original_rows <- nrow(data) 
  
  # Any acause works to get square demographics (not including acause)
  first_acause <- causes_df[run_model == 1, acause[1]]
  square <- data[acause == first_acause, c(lower_demographics[lower_demographics != "acause"], "pop", "parent_value_total"), with = F]
  if (nrow(square)==0){
    stop_or_quit(msg="You're not merging non-modeled child draws correctly! Review.")
  }
  
  # Merge on cause fractions for excluded upper draws and create lower draws to add on
  # square demographics are most detailed (lower demographics) minus acause
  by_cols <- intersect(lower_demographics, geography_demographics)
  by_cols <- by_cols[by_cols != "acause"]
  
  # 1.) Merge county-to-state mapping onto 'square'
  # 2.) Validate schema for both data tables 
  if ('state' %in% geography_demographics){
    by_cols <- c(by_cols, 'state')
    
    county_to_state_mapping = readRDS(geoagg_file)
    square = merge(square, county_to_state_mapping, by='mcnty', all.x=T)
    square$wt <- NULL
    
    stop.if.not.expected.cols(square, expected=c('state', 'mcnty', 'year', 'sex', 'age', 'sim', 'pop', 'parent_value_total'), extra.ok = TRUE)
    stop.if.not.expected.cols(removed_draws, expected=c('state', 'year', 'sex', 'age', 'sim', 'acause', 'cf'), extra.ok = TRUE)
  
    # Also drop extra age categories from removed_draws 
    unique_ages <- unique(square$age)
    removed_draws <- removed_draws[age %in% unique_ages]
  } 
  lower_draws_to_add <- merge(square, removed_draws, by = by_cols, all.y=T, allow.cartesian=T)
  lower_draws_to_add[, value := cf * parent_value_total]
  lower_draws_to_add <- lower_draws_to_add[, c(lower_demographics, "pop", "value"), with = F]

  stopifnot(!any(is.na(lower_draws_to_add)))

  data <- rbind(data, lower_draws_to_add, use.names = TRUE, fill = TRUE)
  
  final_rows <- nrow(data) 
  
  if (original_rows == final_rows){
    stop_or_quit("You haven't added removed non-modeled draws back in! Review.", status=6)
  }
  
  return(data)
}
