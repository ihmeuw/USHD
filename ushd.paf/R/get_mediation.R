#' Return mediation table for a given rei_id
ushd.get_mediation <- function(rei_id) {
  if (rei_id %in% c(370, 105, as.integer(modeled_airpollution_reis))) { # 370 = adult bmi
    out <- data.table::data.table(
      cause_id = numeric(),
      draw = numeric(),
      mediation = numeric()
    )
    message("Returning empty mediation table for rei_id ", rei_id, ". Mediation may occur in GBD.")
  } else if (rei_id %in% as.integer(unmodeled_airpollution_reis)) {
    stop("get_mediation for rei_id = ", rei_id, " not handled yet")
  } else {
    message(sprintf("No mediation defined for rei_id %s...erroring", rei_id)) # also print as message (the error is not printed b/c of with.retries wrapper)
    stop(sprintf("No mediation defined for rei_id %s", rei_id))
  }
  return(out)
}
