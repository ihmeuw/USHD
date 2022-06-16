make_maps_and_plots <- function(plot_data,  # data table containing output (data.table)
                          var,  # variable name of covariate in plot_data (character)
                          geo_var,  # geographic unit to map (character)
                          parent_dir,  # parent directory of where to save maps (character)
                          choropleth_map,  # base map shapefile (SpatialPolygonsDataFrame)
                          outline_map,  # outline map shapefile (SpatialPolygonsDataFrame)
                          title,  # map title (character)
                          ylim = c(0, 1),  # y axis limits (numeric vector)
                          yformat = "digit3",  # y axis label format (character) (see y_labels() function for options)
                          by_race = F,  # is the covariate stratified by race/ethnicity? (boolean)
                          by_edu = F,  # is the covariate stratified by educational attainment? (boolean)
                          make_maps = T,  # produce maps? (boolean)
                          make_line_plots = T)  # produce line plots? (boolean)
{

  # make maps
  if (make_maps) {
    message("Making maps...")
    series_map(choropleth_map = choropleth_map,
               outline_map = outline_map,
               data = plot_data,
               geog_id = geo_var,
               by_race = by_race,
               by_edu = by_edu,
               variable = var,
               map_title = title,
               series_dimension = "year",
               destination_folder = paste0(parent_dir, "get_cov_ids.r"),
               additional_variable_name_string = ifelse(by_race, "_by_race_ethn", ifelse(by_edu, "_by_edu", "")),
               color_ramp = woodson_pallettes("easter_to_earth"),
               histogram = TRUE)
  }

  # make plots
  if (make_line_plots & geo_var == "mcnty") {
    message("Making line plots...")
    temp <- merge(plot_data, unique(loc[, list(mcnty, state_name)]), by = "mcnty")
    plot_county_trends_series(data = temp,
                              year_var = "year",
                              covar = var,
                              county_var = "mcnty",
                              state_var = "state_name",
                              by_race = by_race,
                              by_edu = by_edu,
                              if (by_race) race_var = "race_1977_label",
                              if (by_edu) edu_var = "edu",
                              plot_title = title,
                              start_year = min(temp$year),
                              end_year = max(temp$year),
                              covar_lab = title,
                              ylims = ylim,
                              n_breaks = 5,
                              y_format = yformat,
                              pdf_dest = paste0(parent_dir, "get_cov_ids.r", var, if (by_race) "_by_race_ethn", if (by_edu) "_by_edu", ".pdf"))
  }

  return("Done!")
}
