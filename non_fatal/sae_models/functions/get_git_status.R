#' @title Get Git Status
#'
#' @description Given a path to a directory, determine if it is a git repository, and if so,
#' return a string of git hash, branch, date of last commit, and status plus the
#' diff if requested
#'
#' @param repo The git repository directory
#' @param repo_name The name of the repo to print in the header
#' @param show_diff Print the diff
#'
#' @return character vector of a report intended to be printed with cat
#'
#' @export
get_git_status <- function(repo, repo_name, show_diff = FALSE) {
  # start the report with the name of the repo in a header
  report <- paste("\n\n**********", repo_name, "**********\n", repo,
                  collapse = "\n "
  )
  # first check and see if this is a git repository and give a warning if it isn't
  if (!dir.exists(paste0(repo, "/.git"))) {
    report <- paste0(c(
      report,
      "\n WARNING: 'repo' does not appear to be a git repository",
      "\n          Cannot print git hash\n"
    ))
  } else {
    # Collect some git commands for the report
    repo_git_cmd <- paste0("cd ", repo, "; git")
    branch <- system(paste(repo_git_cmd, "branch | grep \'*\' | cut -d ' ' -f2-"),
                     intern = TRUE
    )
    commit <- system(paste(repo_git_cmd, "log -1 --pretty=oneline"),
                     intern = TRUE
    )
    commit_date <- system(paste(repo_git_cmd, "log -1 --format=%cd"),
                          intern = TRUE
    )
    status <- system(paste(repo_git_cmd, "status --long"), intern = TRUE)
    if (show_diff) diff <- system(paste(repo_git_cmd, "diff HEAD"), intern = TRUE)
    # finish constructing the report
    report <- paste(c(
      report,
      "\n******* Branch *******", branch,
      "\n******* Commit *******", commit,
      "\n** Commit Date/Time **", commit_date,
      "\n******* Status *******", status, "\n"
    ),
    collapse = "\n "
    )
    if (show_diff) {
      report <- paste(c(
        report,
        "********Diff********", diff, "\n"
      ),
      collapse = "\n "
      )
    }
  }
  return(report)
}