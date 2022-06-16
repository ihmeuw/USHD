#' List of command line skips
#' @export
CLI.Flags <- list(
  skip_mx_aggregation = "--skip_mx_aggregation",
  skip_yll_aggregation = "--skip_yll_aggregation",
  skip_yld_aggregation = "--skip_yld_aggregation"
)

#' Add flags to an argparse::ArgumentParser for skipping MX and/or YLL aggregation
#' @export
add_aggregation_skip_flags <- function(parser) {
  parser$add_argument(CLI.Flags$skip_mx_aggregation, action = "store_true", default = FALSE, help = "Add this flag to skip MX aggregation")
  parser$add_argument(CLI.Flags$skip_yll_aggregation, action = "store_true", default = FALSE, help = "Add this flag to skip YLL aggregation")
  parser$add_argument(CLI.Flags$skip_yld_aggregation, action = "store_true", default = FALSE, help = "Add this flag to skip YLD aggregation")
}

#' Add dir arg
#' @export
add_dir_argument <- function(parser) {
  parser$add_argument("dir", help = "Model run dir")
}

#' Add geography level arg
#' @export
add_level_argument <- function(parser) {
  parser$add_argument("level", help = "The geography level to aggregate data to e.g., mcnty, state, natl")
}

#' Add lifetable Boolean
#' @export
add_lifetable_argument <- function(parser) {
  parser$add_argument("run_lt", choices = c("TRUE", "FALSE"), help = "TRUE or FALSE to set whether to create life tables")
}

#' Add race integer
#' @export
add_race_argument <- function(parser) {
  parser$add_argument("race", type = "integer", help = "Numeric race identifier to aggregate data for")
}

#' Add raked Boolean
#' @export
add_raked_argument <- function(parser) {
  parser$add_argument("raked", choices = c("TRUE", "FALSE"), help = "Work on raked data?")
}

#' Add sex integer
#' @export
add_sex_argument <- function(parser) {
  parser$add_argument("sex", type = "integer", choices = c(1, 2, 3))
}

#' Add edu integer
#' @export
add_edu_argument <- function(parser) {
  parser$add_argument("edu", type = "integer", help = "Numeric edu identifier to aggregate data for")
}


#' Add year integer
#' @export
add_year_argument <- function(parser) {
  parser$add_argument("year", type = "integer", help = "Year to process")
}

#' Add cause level integer
#' @export
add_raking_initial_terminal_level <- function(parser, max_initial_level = 4) {
  parser$add_argument("--initial_level", type = "integer", choices = 0:max_initial_level)
  parser$add_argument("--terminal_level", type = "integer", choices = 0:4)
}

#' Add acause name of parent
#' @export
add_children_of_argument <- function(parser) {
  parser$add_argument("children_of",
    type = "character", default = NULL,
    help = "Parent cause to subset to. Can be helpful for rakes over many levels of the cause hierarchy."
  )
}

#' validate cause and level arguments
#' @export
validate_cause_subset_args <- function(children_of, initial_level, terminal_level) {
  if (!is.null(children_of) & (!is.null(initial_level) | !is.null(terminal_level))) {
    stop_or_quit("Parent cause subset and initial level/terminal level cannot be specified at the same time.", status = 2)
  }
}

#' Get arguments for argparse
#' @export
get_args <- function() {
  args <- commandArgs(TRUE)

  if (is.null(args) || length(args) == 0) {

    args <- commandArgs(FALSE)
    idx <- match("--no-save", args)
    if (is.na(idx)) {
      message(sprintf("--no-save not detected in commandArgs() - cannot determine CLI args from '%s'", paste(commandArgs(), collapse = " ")))
      args <- character(0)
    } else if (idx == length(args)) {
      # indicates no CLI args were passed. Return empty vector of args
      args <- character(0)
    } else {
      args <- args[-seq_len(idx + 1)]
    }
  }
  return(args)
}
