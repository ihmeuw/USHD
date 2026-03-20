#' Return directory string for a given rei_id
ushd.get_dir_string <- function(rei_id) {
  return(names(modeled_airpollution_reis)[[which(as.integer(modeled_airpollution_reis) == rei_id)]])
}

modeled_airpollution_reis = list(
  'pm_archive_2023_08_03' = '86', # Ambient particulate matter pollution
  'no2' = '404', # Nitrogen dioxide pollution
  'ozone' = '88' # ambient ozone pollution
)
unmodeled_airpollution_reis = list(
  'air_hap' = '87' # Household air pollution from solid fuels
)

rei_tmrel_constants = list(
  '86' = list(
    tmrel.start = 2.4,
    tmrel.end = 5.9
  ),
  '404' = list(
    tmrel.start = 4.545,
    tmrel.end = 6.190
  ),
  '88' = list(
    tmrel.start = 29.1,
    tmrel.end = 35.7
  )
)