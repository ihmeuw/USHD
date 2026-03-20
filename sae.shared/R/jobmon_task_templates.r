#' @title load_jobmon_task_templates
#'
#' @description Compiles the jobmon task templates.
#'
#' @param jobmon_tool [jobmonr::tool] Instance of jobmonr::tool.
#'
#' @return [list] Jobmon task templates used in raking and aggregation
#'
#' @rdname load_jobmon_task_templates
#'
#' @export
load_jobmon_task_templates <- function(jobmon_tool) {
  templates <- list()
  ### Raking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Command and template for 1d - attributable burden raking.
  attrib_burden_raking_command <- paste0(
    "PYTHONPATH= {rshell} {scriptname} --from_dir {from_dir} --to_dir {to_dir} ",
    "--measure {measure} --from_geo {from_geo} --to_geo {to_geo} ",
    "--location_id {location_id} --measure_id {measure_id} ",
    "--common_raking_vars {common_raking_vars} --lower_raking_vars {lower_raking_vars} ",
    "--upper_raking_vars {upper_raking_vars} --year {year} --rei_id {rei_id}"
  )
  templates$attrib_burden_raking_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "attrib_burden_raking_template",
    command_template = attrib_burden_raking_command,
    node_args = list("location_id", "year"),
    task_args = list(
      "from_dir", "to_dir", "measure",
      "from_geo", "to_geo", "measure_id",
      "common_raking_vars", "lower_raking_vars",
      "upper_raking_vars", "rei_id"
    ),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for 1d raking (used in raking/_run_all_raking_flexible)
  flexible_raking_command <- paste0(
    "PYTHONPATH= {rshell} {scriptname} --from_dir {from_dir} --to_dir {to_dir} ",
    "--measure {measure} --from_geo {from_geo} --to_geo {to_geo} ",
    "--common_raking_vars {common_raking_vars} --lower_raking_vars ",
    "{lower_raking_vars} --upper_raking_vars {upper_raking_vars} ",
    "--year {year} --sex {sex}"
  )
  templates$flexible_raking_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "flexible_raking_template",
    command_template = flexible_raking_command,
    node_args = list("year", "sex"),
    task_args = list(
      "from_dir", "to_dir", "measure",
      "from_geo", "to_geo",
      "common_raking_vars", "lower_raking_vars",
      "upper_raking_vars"
    ),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for 2d raking (used in raking/_run_all_flexible_raking)
  flexible_2d_raking_command <- paste0("PYTHONPATH= {rshell} {scriptname} --from_dir {from_dir} --to_dir {to_dir} ",
                                       "--measure {measure} --to_geo {to_geo} --target_dims {target_dims} --draws {draws} ",
                                       "--common_raking_vars {common_raking_vars} ",
                                       "--use_verbose {use_verbose} --parent_acause {parent_acause} --year {year} ",
                                       "--sex {sex} --age {age}")
  templates$flexible_2d_raking_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "flexible_2d_raking_template",
    command_template = flexible_2d_raking_command,
    node_args = list("parent_acause", "year", 
                     "sex", "age"),
    task_args = list("from_dir", "to_dir", "measure", "to_geo",
                     "target_dims", "common_raking_vars",
                     "draws", "use_verbose"),
    op_args = list("rshell", "scriptname")
  )
  
  # PK test: need to rake within a county-level model across causes
  flexible_raking_command_exp <- paste0(
    "PYTHONPATH= {rshell} {scriptname} --from_dir {from_dir} --to_dir {to_dir} ",
    "--measure {measure} --from_geo {from_geo} --to_geo {to_geo} ",
    "--common_raking_vars {common_raking_vars} --lower_raking_vars ",
    "{lower_raking_vars} --upper_raking_vars {upper_raking_vars} ",
    "--year {year} --sex {sex} --parent_acause {parent_acause}"
  )
  templates$flexible_raking_template_exp <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "flexible_raking_template_exp",
    command_template = flexible_raking_command_exp,
    node_args = list("parent_acause", "year", "sex"),
    task_args = list(
      "from_dir", "to_dir", "measure",
      "from_geo", "to_geo",
      "common_raking_vars", "lower_raking_vars",
      "upper_raking_vars"
    ),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for raking compile (used in raking/_run_all_flexible_raking.r)
  raking_compile_command <- "PYTHONPATH= {rshell} {scriptname} {dir} {year} {sex} {acause} {measure}"
  templates$raking_compile_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "raking_compile_template",
    command_template = raking_compile_command,
    node_args = list("dir", "year", "sex", "acause", "measure"),
    task_args = list(),
    op_args = list("rshell", "scriptname")
  )
  ### Aggregation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Command and template for pred_lt (used in post_estimation/launch_aggregation.r)
  pred_lt_command <- "PYTHONPATH= {rshell} {scriptname} --dir {dir} --year {year} --sex {sex} --race {race} --raked {raked} --edu {edu}"
  templates$pred_lt_template <- jobmonr::task_template(tool = jobmon_tool,
                                                       template_name = "pred_lt_template",
                                                       command_template = pred_lt_command,
                                                       node_args = list("dir", "year", "sex", "race", "raked", "edu"),
                                                       task_args = list(),
                                                       op_args = list("rshell", "scriptname"))
  
  # Command and template for pred_lt (used in post_estimation/launch_aggregation.r)
  pred_yll_command <- "PYTHONPATH= {rshell} {scriptname} --dir {dir} --year {year} --sex {sex} --race {race} --raked {raked} --edu {edu}"
  templates$pred_yll_template <- jobmonr::task_template(tool = jobmon_tool,
                                                        template_name = "pred_yll_template",
                                                        command_template = pred_yll_command,
                                                        node_args = list("dir", "year", "sex", "race", "raked", "edu"),
                                                        task_args = list(),
                                                        op_args = list("rshell", "scriptname"))
  
  # Command and template for agg_races (used in post_estimation/launch_aggregation.r)
  agg_races_command <- "PYTHONPATH= {rshell} {scriptname} {dir} {year} {sex} {edu} {raked} {lt} {agg_skip_flags}"
  templates$agg_races_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_races_template",
    command_template = agg_races_command,
    node_args = list("dir", "year", "sex", "edu", "raked", "lt", "agg_skip_flags"),
    task_args = list(),
    op_args = list("rshell", "scriptname"))

  # Command and template for agg_edus (used in post_estimation/launch_aggregation.r) 
  agg_edus_command <- "PYTHONPATH= {rshell} {scriptname} {dir} {year} {sex} {raked} {lt} {agg_skip_flags}"
  templates$agg_edus_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_edus_template",
    command_template = agg_edus_command,
    node_args = list("dir", "year", "sex", "raked", "lt", "agg_skip_flags"),
    task_args = list(),
    op_args = list("rshell", "scriptname"))
  
  # Command and template for agg_geos (used in post_estimation/launch_aggregation.r)
  agg_geos_command <- "PYTHONPATH= {rshell} {scriptname} {dir} {level} {year} {sex} {race} {edu} {raked} {lt} {agg_skip_flags}"
  templates$agg_geos_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_geos_template",
    command_template = agg_geos_command,
    node_args = list("dir", "level", "year", "sex", "race", "edu", "raked", "lt", "agg_skip_flags"),
    task_args = list(),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for agg_sex (used in post_estimation/launch_aggregation.r)
  agg_sex_command <- "PYTHONPATH= {rshell} {scriptname} {dir} {level} {year} {race} {edu} {raked} {lt} {agg_skip_flags}"
  templates$agg_sex_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_sex_template",
    command_template = agg_sex_command,
    node_args = list("dir", "level", "year", "race", "edu", "raked", "lt", "agg_skip_flags"),
    task_args = list(),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for aggregation compile (used in post_estimation/launch_aggregation.r)
  agg_compile_command <- "PYTHONPATH= {rshell} {scriptname} {dir} {raked} {lt} {validate} {agg_skip_flags}"
  templates$agg_compile_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_compile_template",
    command_template = agg_compile_command,
    node_args = list("dir", "raked", "lt", "validate", "agg_skip_flags"),
    task_args = list(),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for agg_geos_ab (used in scripts/attrib_burden_launch_aggregation.r)
  agg_geos_ab_command <- paste0("PYTHONPATH= {rshell} {scriptname} --dir {dir} --measure_id {measure_id} ",
                                "--location_id {location_id} --year {year} --rei_id {rei_id} --raked {raked}")
  templates$agg_geos_ab_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_geos_ab_template",
    command_template = agg_geos_ab_command,
    node_args = list("dir", "measure_id", "location_id", "year", "raked"),
    task_args = list("rei_id"),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for agg_races_sexes_ab (used in scripts/attrib_burden_launch_aggregation.r)
  agg_races_sexes_ab_command <- paste0("PYTHONPATH= {rshell} {scriptname} --dir {dir} --measure_id {measure_id} ",
                                "--location_id {location_id} --year {year} --rei_id {rei_id} --raked {raked}")
  templates$agg_races_sexes_ab_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_races_sexes_ab_template",
    command_template = agg_races_sexes_ab_command,
    node_args = list("dir", "measure_id", "location_id", "year", "raked"),
    task_args = list("rei_id"),
    op_args = list("rshell", "scriptname")
  )
  
  # Command and template for aggregation compile of ab (used in scripts/attrib_burden_launch_aggregation.r)
  agg_compile_ab_command <- paste0("PYTHONPATH= {rshell} {scriptname} --dir {dir} --year {year} ",
                                   "--measure_id {measure_id} --raked {raked} --rei_id {rei_id} --validate {validate}")
  templates$agg_compile_ab_template <- jobmonr::task_template(
    tool = jobmon_tool,
    template_name = "agg_compile_ab_template",
    command_template = agg_compile_ab_command,
    node_args = list("dir", "year", "measure_id", "raked", "validate"),
    task_args = list("rei_id"),
    op_args = list("rshell", "scriptname")
  )
  
  return(templates)
}
