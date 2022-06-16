# Set global constants here

#' @export
geo_choices <- c("mcnty", "state", "natl")

#' @export
cause_choices <- c(0, 1, 2, 3, 4)

#' @export
race_default <- 1

#' @export
by_race_default <- c(2, 4, 5, 6, 7)

#' @export
edu_default <- 1

#' @export
by_edu_default <- c(100, 101, 102, 103, 104)

# These cause_ids were added with GBD 2019.  They are level 1 totals associated with particular cause trees
# (e.g. 1029 total_cancer_reporting) and as such should not be raked to _all
# 1059, 1060 added
#' @export
cause_id_exclusion_list <- c(1026:1029, 1059:1060)

#' @export
expected.cause.cols <- c(
  "level", # numeric: level in cause hierarchy. 0-4 inclusive
  "cause_id", # numeric: cause id
  "cause_outline", # character: identifier for path to cause in cause tree e.g., "A.6.1.2". _all has outline "Total"
  "acause", # character: "analytical code for cause"; character human-readable cause identifier
  "parent_id", # numeric: id of this causes parent
  "path_to_top_parent", # character: CSV of path to root of tree e.g., "294,295,344,353". 294 is the root
  "sexes", # character: of R vector literal for sex_id's pertaining to this cause e.g., "c(1,2)". Parse with eval(parse(text = sexes))
  "ages", # like sexes, but with ages e.g., "c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)"
  "run_model" # numeric: indicates whether a model should be run for this cause
)


# used by check_raked_results.r
#' @export
abs_tol <- 1e-10

#' @export
rel_tol <- 1e-2
