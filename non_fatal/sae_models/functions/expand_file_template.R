# Expand a file template into all desired permutations
#
# @param template a function to apply e.g., `paste0(dir, "/", var, "_est_", level, "_", year, "_", sex, "_", race, ifelse(raked, "_raked", ""), ".rds")`
# @param ... named arguments for all values that need cross-join expansion
#
# @return vector of results from template
# @example
# \dontrun{
# dir <- "THE_DIR"
# raked <- FALSE
# ages <- c(1, 5, 10, 20)
# sexes <- c(1, 2)
# expand_file_template(
#  paste0(dir, "/my_file_", age, "_", sex, ifelse(raked, "_raked", ""), ".rds"),
#  age = ages,
#  sex = sexes)
#
# [1] "THE_DIR/my_file_1_1.rds"  "THE_DIR/my_file_1_2.rds"  "THE_DIR/my_file_5_1.rds"  "THE_DIR/my_file_5_2.rds" "THE_DIR/my_file_10_1.rds"
# [6] "THE_DIR/my_file_10_2.rds" "THE_DIR/my_file_20_1.rds" "THE_DIR/my_file_20_2.rds"
# }

expand_file_template <- function(template, ...) {
  # substitute the code block so that we may evaluate it in a different context
  template <- substitute(template)
  # get remaining args passed by name
  args <- list(...)
  # create a cross-join of these
  cj <- do.call(data.table::CJ, args)
  # evaluate the template
  # use our cross-join data.table as the environment
  # look for any values not in the cj in the calling parent environment
  eval(template, envir = cj, enclos = parent.frame())
}