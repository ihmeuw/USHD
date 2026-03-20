#' @title Get unique jobmon tasks
#'
#' @description Given that jobmon tasks cannot be filtered with the base unique(), this
#' function takes a list of jobmon tasks subsets them down to unique task based on their
#' name field. This assumes that all tasks have unique names.
#' This function also removes the placeholder value (1), and returns NULL
#' the list of jobmon tasks empty or all placeholder values
#'
#' @param jobmon_tasks a list of jobmon tasks
#'
#' @return Returns a list of unique jobmon tasks, or NULL if an empty list or placeholder
#' only list was passed in.
#'
#' @rdname get_unique_jobmon_tasks
#' @export
get_unique_jobmon_tasks <- function(jobmon_tasks) {
  hold <- unlist(jobmon_tasks)
  if (!is.null(hold)) {
    # an empty hold due to e.g., na.omit(upstream_jobs) is effectively NULL
    # or an all placeholder column (all 1s)
    if (length(hold) == 0 | class(hold) == "numeric") {
      hold <- NULL
    } else {
      unique_holder <- data.table(holder = hold, identifier = "remove")
      for (i in 1:nrow(unique_holder)) {
        if (!"numeric" %in% class(unique_holder$holder[[i]])) {
          unique_holder[i, identifier := unique_holder$holder[[i]]$name]
        }
      }
      unique_holder <- unique_holder[identifier != "remove"]

      # check again, in case all holds were placeholders
      if (nrow(unique_holder) == 0) {
        hold <- NULL
      } else {
        hold <- unique_holder[!duplicated(identifier), ]$holder
      }
    }
  }
  return(hold)
}
