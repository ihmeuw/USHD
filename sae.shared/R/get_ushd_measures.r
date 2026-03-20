#' @title get_ushd_measures
#'
#' @description
#'
#' @param draws [data.table] Raked draws in count space
#' @param measure_id [integer] -- 1 for death (mx), 2 for daly, 3 for yld, 4 for yll.
#' @param fatal_model_id [integer] -- USHD DB model id to get file paths from
#'
#' @return data.table of measures (currently mx or YLL) corresponding to demographic values in draws
#'
#' @export
get_ushd_measures = function(draws, measure_id, fatal_model_id) {
  l_measure_str <- c("mx", "daly", "yld", "yll")
  if (measure_id %in% c(1,4)) {
    measure_id_str = l_measure_str[[measure_id]]
    l_causes <- unique(draws$acause)
    if ("mcnty" %in% colnames(draws)) {
      l_mcntys <- unique(draws$mcnty)
    } else {
      l_mcntys <- unique(draws$area)
    }
    l_sexes <- unique(draws$sex)
    l_races <- unique(draws$race)
    l_ages <- unique(draws$age)
    l_measures <- c()
    for (acause in l_causes) {
      l_files <- c()
      for (sex in l_sexes) {
        # USHD YLLs have values for neo_breast, AB does not
        if (sex == 1 & acause == "neo_breast") next
        file_path <- ushd.dbr::get_fatal_model_draws_file(model_run_ids = list(fatal_model_id),
                                                          draw_type = measure_id_str,
                                                          acause = acause,
                                                          geo_level = 'mcnty',
                                                          years = list(year),
                                                          sex = sex,
                                                          race = l_races,
                                                          raked=TRUE)$file_path
        # remove duplicate entries in DB with //
        l_files <- c(l_files, unique(gsub("//", "/", file_path)))
      }
      if (dir.exists(paste0(dirname(l_files[[1]]), "/draws_mcnty"))) {
        # Create all combinations of file paths and county numbers
        combinations <- expand.grid(l_files = l_files, l_mcnty = l_mcntys, stringsAsFactors = FALSE)

        # Define a function to insert the county number into the file path
        insert_county <- function(file_path, county) {
          # Construct the new subdirectory path with the county number
          new_subdir <- sprintf("/draws_mcnty/%s/", county)

          # Insert the new subdirectory path into the original file path
          parts <- strsplit(file_path, glue::glue("/{measure_id_str}_draws_mcnty"))[[1]]
          new_path <- paste0(parts[1], new_subdir, glue::glue("{measure_id_str}_draws_"), county, parts[2])
          return(new_path)
        }

        # Apply the function to each combination
        l_juliened_files <- mapply(insert_county, combinations$l_files, combinations$l_mcnty)

        print(glue::glue("Reading {length(l_juliened_files)} juliened files for {acause}"))
        measure_draws <- lapply(l_juliened_files, readRDS)
        measure_draws <- lapply(measure_draws, function(dt) {
          dt <- subset(dt, sim <= max(unique(draws$sim)))
          dt <- subset(dt, age %in% l_ages)
          return(dt)
        })
        l_measures <- c(l_measures, measure_draws)
      } else {
        stop(glue::glue("Juliened file directory {paste0(dirname(l_files[[1]]), '/draws_mcnty')} not available."))
      }
    }
    measure_draws <- rbindlist(l_measures, use.names = TRUE)
    # force measures_draws to use same column name as draws
    if (("mcnty" %in% colnames(measure_draws)) & ("area" %in% colnames(draws))) {
      setnames(measure_draws, "mcnty", "area")
    } else if (("area" %in% colnames(measure_draws)) & ("mcnty" %in% colnames(draws))) {
      setnames(measure_draws, "area", "mcnty")
    } 
  } else {
    stop(glue::glue("measure_id = {measure_id} ({l_measure_str[[measure_id]]}) not currently supported"))
  }
  measure_draws[, measure_value := get(measure_id_str)]
  # remove measure_id_str and level columns
  measure_draws[, c(measure_id_str, "level") := NULL]

  return(measure_draws)
}
