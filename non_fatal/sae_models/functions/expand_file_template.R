# Expand a file template into all desired permutations
#
# @param template a function to apply 
# @param ... named arguments for all values that need cross-join expansion
#
# @return vector of results from template

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