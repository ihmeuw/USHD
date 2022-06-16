# Functions to help with merging/joining data tables

#' @title cross
#'
#' @description Returns the cross product of two data.tables without mutating either
#'
#' @export
cross <- function(df1, df2) {
  # Set up arbitrary merge key
  df1[, key := 1]
  df2[, key := 1]

  product <- merge(df1, df2, by = "key", allow.cartesian = TRUE)

  # Clean up
  df1[, key := NULL]
  df2[, key := NULL]
  product[, key := NULL]

  return(product[])
}

#' @title merge.flexible
#'
#' @description Helper function for merges within flexible raking code.
#'
#' @param lower Lower data.table
#' @param upper Upper data.table
#'
#' @return Merged result of lower and upper (outer join, or all=TRUE)
#'
#' @export
merge.flexible <- function(lower, upper) {
  lower <- copy(lower)
  upper <- copy(upper)

  always_merge_cols <- c("age", "year", "sex", "sim")
  merge_cols <- copy(always_merge_cols)

  merge.validate(lower, upper, always_merge_cols)

  # Determine whether race, education, and cause agree between data tables.
  result <- determine_dimension_equal(lower, upper, merge_cols)
  lower <- result[[1]]
  upper <- result[[2]]
  merge_cols <- result[[3]]

  # Rename any potential value columns in upper with the suffix 'target'.
  value_cols <- c("mx", "yll", "value")
  found_value_cols <- names(upper)[names(upper) %in% value_cols]
  if (length(found_value_cols) > 0) setnames(upper, found_value_cols, paste0(found_value_cols, "_target"))

  # Include any other columns with the same dimensions in 'merge_cols'.
  # This catches things like 'level/area' or 'mcnty'.
  shared_cols <- intersect(names(lower), names(upper))
  unhandled_cols <- setdiff(shared_cols, c(merge_cols, "race", "edu"))
  allowed_unhandled_cols <- c("level", "area", "mcnty", "state", "natl")

  error_unhandled_cols <- setdiff(unhandled_cols, allowed_unhandled_cols)
  if (length(error_unhandled_cols) > 0) {
    stop(sprintf("Unhandled columns found in datasets: %s. Cannot complete merge.", paste0(error_unhandled_cols, ", ")))
  }

  merge_cols <- unique(c(merge_cols, unhandled_cols))
  result <- merge(lower, upper, by = merge_cols, all = TRUE)

  # Make sure you haven't inadvertently renamed any columns
  if (any(grepl("\\.x", names(result)))) {
    dup_merge_cols <- names(result)[grepl("\\.x|\\.y", names(result))]
    stop(sprintf("Merge was not specified correctly - duplicate columns found were %s.", paste0(dup_merge_cols, collapse = ", ")))
  }

  return(result)
}

#' @title determine_dimension_equal
#'
#' @description Helper function for merge.flexible to determine if common dimensions have equivalent values.
#'
#' @param lower Lower data.table
#' @param upper Upper data.table
#' @param merge_cols Vector of 'by' columns for merge.
#'
#' @return List with 3 elements: lower, upper, and merge_cols
#'
#' @export
determine_dimension_equal <- function(lower, upper, merge_cols) {
  potentially_equal_dimensions <- c("race", "edu", "acause")

  for (col in potentially_equal_dimensions) {
    if (!setequal(lower[[col]], upper[[col]])) {
      setnames(upper, col, paste0(col, "_target"))
    } else {
      merge_cols <- c(merge_cols, col)
    }
  }

  return(list(lower, upper, merge_cols))
}

#' @title merge.validate
#'
#' @description Helper function for merge.flexible, to make sure that a merge can happen
#'
#' @param lower Lower data.table
#' @param upper Upper data.table
#' @param always_merge_cols Vector of columns to merge on, used for validation
#'
#' @export
merge.validate <- function(lower, upper, always_merge_cols) {
  errors <- c()

  # Make sure required columns are present
  if (!all(always_merge_cols %in% names(lower))) {
    missing_cols <- always_merge_cols[!always_merge_cols %in% names(lower)]
    errors <- c(errors, sprintf("The following required columns were missing from lower: %s", paste0(missing_cols, collapse = ", ")))
  }
  if (!all(always_merge_cols %in% names(upper))) {
    missing_cols <- always_merge_cols[!always_merge_cols %in% names(upper)]
    errors <- c(errors, sprintf("The following required columns were missing from upper: %s", paste0(missing_cols, collapse = ", ")))
  }

  # Check that race dimension is present.
  if (!"race" %in% names(lower)) errors <- c("lower dataframe missing race column.")
  if (!"race" %in% names(upper)) errors <- c("upper dataframe missing race column.")

  # Check that education is present
  if (!"edu" %in% names(lower)) errors <- c("lower dataframe missing edu column.")
  if (!"edu" %in% names(upper)) errors <- c("upper dataframe missing edu column.")

  # Flag errors.
  if (length(errors) > 0) {
    for (error in errors) {
      message(error)
    }
    stop("Unable to complete merge. Review errors.")
  }
}
