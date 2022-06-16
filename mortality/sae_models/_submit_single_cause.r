####################################################################################################
## Description: Submit jobs to run model and mx/yll prediction code and apply life table functions
##              based on the specifications given in the "settings" file in the specified directory
##
## Passed args: dir [character] -- home directory for settings and final output
##              run_lt [logical] -- should life tables be calculated?
##              type [character] -- what type of run is this? options:
##                "models_only" -- just fit models and generate initial area-year-sex-race ASMRs
##                "post_models" -- just run aggregation, collapse, and compile steps required after
##                    area-year-sex-race ASMRs are available (i.e., after running with type =
##                    "models_only")
##                "post_raking" -- the same as "post_models", but using raked area-year-sex-race
##                    ASMRs instead of directly estimated ASMRs.
##                "all" -- run all steps, assuming raking == F where relevant (equivalent to
##                    running with type = "models_only" and then type = "post_models")
##                "validation" -- run all steps required to get estimates needed for validation
##                    models (i.e., most aggregation steps are skipped). This also assumes that
##                    raking == F where relevant.
##                "agg_raked_yll" -- After raking YLLs specifically, run just the aggregation
##                    steps in post-processing on these raked YLLs
##              resub [logical] -- should jobs where the relevant output file already exists be
##                skipped?
##              testing [logical] -- is this a testing job? if T, logs all output.
##              priority [integer] -- Defines the priority of the job relative to other jobs.
##                Priority is an integer in the range -1023 to 1024. The default priority value
##                for jobs is 0.
##
## Requires:    a fully specified settings file ([dir]/settings.csv)
##
## Outputs:     submitted jobs for all processes required to estimate mx, ylls, and (if lt == T)
##                life tables from data prep, to models, to aggregation, to compiled results.
##              "runtime_info.txt" which includes information about provided settings, system
##                settings, and job IDs for submitted jobs.
##
####################################################################################################

suppressMessages(suppressWarnings({
  library(R.utils)
  library(data.table)
  R.utils::sourceDirectory('functions', modifiedOnly = FALSE) 
}))

## Get and check settings --------------------------------------------------------------------------
parser <- argparse::ArgumentParser()
add_dir_argument(parser)
add_lifetable_argument(parser)

parser$add_argument("type", choices = c("models_only", "post_models", "post_raking", "validation", "all", "agg_raked_yll"), help = "Determine type of run")

parser$add_argument("resub", choices = c("TRUE", "FALSE"), help = "Use this flag if this is a resubmission to skip completed jobs")
parser$add_argument("testing", choices = c("TRUE", "FALSE"), help = "Set as testing job. Log all output")
parser$add_argument("all_cause_dependency", choices = c("TRUE", "FALSE"), help = "Defaults to TRUE. If set to FALSE, the models will run without depending on an all-cause run to be in the folder and YLLs will not be predicted.")
parser$add_argument("--project", default = "PROJECT")
parser$add_argument("--queue", default = "QUEUE")
parser$add_argument("--priority", default = "0")
add_aggregation_skip_flags(parser)

args <- parser$parse_args(get_args())
message(as.logical(args$run_lt))

dir <- args$dir
run_lt <- as.logical(args$run_lt)
type <- args$type
resub <- as.logical(args$resub)
testing <- as.logical(args$testing)
project <- args$project
queue <- args$queue
priority <- args$priority
all_cause_dependency <- as.logical(args$all_cause_dependency)
skip_yll_aggregation <- args$skip_yll_aggregation
skip_mx_aggregation <- args$skip_mx_aggregation
skip_yld_aggregation <- args$skip_yld_aggregation


if(!all_cause_dependency) skip_yll_aggregation <- T

if (is.na(testing)) testing <- F

if (type %in% c("post_raking", "agg_raked_yll")) raked <- T else raked <- F
if (type == "validation") validate <- T else validate <- F

print("Settings passed are:")
for (setting in names(args)) {
  print(paste0(setting, ": ", get(setting)))
}

# check settings (skip for resubmits since this will have already happened)
if (resub | type %in% c("post_models", "post_raking", "agg_raked_yll")) {
  get_settings(dir)
} else {
  check_settings(dir)
  if (run_lt) {
    if (!setequal(ages, c(0, 1, seq(5, 85, 5)))) stop("the age groups are not correct for life tables")
  }
}

# assume that the model is in the following directory:
root <- "FILEPATH"
current_dir <- gsub("FILEPATH", "", dir)
lu_root <- paste0("FILEPATH", LU_folder, "FILEPATH")
lu_modeldir <- paste0(lu_root, "/", current_dir)

# create a table for holding job ids
jids <- CJ(level = c(area_var, names(geoagg_files)), year = years, sex = c(sexes, 3),
           race = unique(c(races, all_pop_id)), edu = unique(c(edu_groups, all_pop_id)))

if(race_together) {
  jids[, race_fit := 99]
} else {
  jids[, race_fit := race]
}

if(edu_together) {
  jids[, edu_fit := 99]
} else {
  jids[, edu_fit := edu]
}


if (!is.null(geoagg_files)) { # we may not be able to aggregate in all years, so year-level combinations with no crosswalk need to be removed.
  for (this_level in names(geoagg_files)) {
    weights <- readRDS(geoagg_files[this_level])
    if ("year" %in% names(weights)) {
      jids <- jids[level != this_level | year %in% unique(weights$year), ]
    }
    rm(weights)
  }
}

# in validation models we don't always make predictions for all years, so drop the jobs for years we don't need
if (validate) {
  load(gs_file)
  jids <- jids[year %in% unique(gs_mx$year), ]
  rm(gs_mx, gs_ex); gc()
}

if (resub & !skip_mx_aggregation) {
  delete_final_output(dir, type)
}

if (by_edu & by_race) {
  stop(
    glue::glue("Unhandled case:",
               "by_race = {by_race}",
               "race_together = {race_together}",
               "by_edu = {by_edu}",
               "edu_together = {edu_together}",
               .sep = "\n")
  )
}


## Submit stage 1 jobs (data prep, models, pred ASMRs) ---------------------------------------------
if (type %in% c("models_only", "all", "validation")) {

  # Prep input data --------------------------------------------------------------------------------
  jids[, prep_inputs := sbatch(code = paste0(model_class, "/prep_inputs.r"),
                             arguments = c(dir),
                             fthread = 1, m_mem_free = "64G", h_rt = "00:30:00", archive = T,
                             skip_if_exists = if (resub) paste0(lu_modeldir, "/data.rds"),
                             project = project, queue = queue, priority = priority,
                             sgeoutput = if (testing) dir)]


  # fit models -------------------------------------------------------------------------------------

  if(race_together|edu_together) {
    mem_fit <- "150G"; q_fit <- "QUEUE"; time_fit <- "336:00:00"
  } else {
    mem_fit <- "50G"; q_fit <- queue; time_fit <- "72:00:00"
  }

  jids[sex != 3 & (!by_race | race != all_pop_id) & (!by_edu | edu != all_pop_id),
       fit_mod := sbatch(code = paste0(model_class, "/fit_mod_", model, ".r"),
                       arguments = c(dir, sex, race_fit, edu_fit),
                       hold = unique(prep_inputs),
                       fthread = 15,
                       m_mem_free = mem_fit, queue = q_fit, h_rt = time_fit,
                       archive = T,
                       skip_if_exists = if (resub) paste0(dir, "/model_fit_", sex, "_", race_fit, "_" , edu_fit, ".rds"),
                       project = "PROJECT",
                       type = "TMB",
                       mkl_threads = 15, omp_threads = 1,
                       sgeoutput = if (testing) dir),
       by = "sex,race_fit,edu_fit"]

  ## Prediction ------------------------------------------------------------------------------------

  ## Arguments for the prediction array job
  m_mem_free_pred <- 50
  if (by_race | by_edu) m_mem_free_pred <- m_mem_free_pred * 3

  m_mem_free_save <- 15
  if (by_race | by_edu) m_mem_free_save <- m_mem_free_save * 3
  fthread_array <- 4
  h_rt_array <- "04:00:00"
  draw_width <- 50

  # generate the required files
  gen_files_mx <- function(sex, race, edu){
    required_files <- c()
    for(year in years) {
      required_files <- c(required_files,  paste0(dir, "/mx_est_", area_var, "_", year, "_", sex, "_", race, "_", edu, ".rds"))
    }
    return(required_files)
  }

  # Predict area-sex-level ASMRs (by sex, race)
  mem_pred_mx <- 20
  if (by_race|by_race) mem_pred_mx <- mem_pred_mx * 5

  # Submit pred_mx
  jids[sex != 3 & (!by_race | race != all_pop_id) & (!by_edu | edu != all_pop_id),
       pred_mx := sbatch(code = paste0(model_class, "/pred_mx.r"),
                       arguments = c(dir, sex, race_fit, edu_fit, validate, resub),
                       name = paste0("pred_mx_", sex, "_", race_fit, "_", edu_fit),
                       hold = unique(na.omit(fit_mod)),
                       fthread = 4, m_mem_free = paste0(mem_pred_mx, "G"),
                       h_rt = "4:00:00", archive = T,
                       skip_if_exists = if (resub) gen_files_mx(sex, race, edu),
                       project = project, queue = queue, priority = priority,
                       sgeoutput = if (testing) dir),
       by = "sex,race_fit,edu_fit"]

  array_sub <- T

  if(all(jids[,is.na(pred_mx) | pred_mx == 1])) {
    array_sub <- F
  }


  if (array_sub) {
    for(s in sexes) {
      for(r in unique(jids$race_fit)) {
        for (e in unique(jids$edu_fit)) {

          p_mx_job <- jids[!is.na(pred_mx) & pred_mx != 1 & sex == s, ]
          p_mx_job <- unique(p_mx_job[race_fit == r & edu_fit == e, .(pred_mx)])

          cat(
            stringr::str_glue(
              "sex s is {s}",
              "race r is {r}",
              "edu e is {e}",
              "race_together is {race_together}",
              "edu_together is {edu_together}",
              .sep = "\n")
          )
          cat("\n")
          print(unique(jids[pred_mx %in% p_mx_job, list(sex, race_fit, edu_fit, pred_mx)]))

          # This function submits the array jobs
          jids <- prep_mx_array_jobs(dir, root, sex = s,
                             race = r,
                             edu = e,
                             validate, resub, queue = queue,
                             paste0(m_mem_free_pred, "G"),
                             paste0(m_mem_free_save, "G"), fthread = fthread_array,
                             h_rt = h_rt_array, draw_width,
                             p_mx_job, jids)
        }
      } 
    } 
  }  else {
      jids[, c("pred_mx", "pred_sub", "save_sub") := list(1,1,1)] # 1 here is just a placeholder job id
  }


} else {
    jids[, c("pred_mx", "pred_sub", "save_sub") := list(1,1,1)] # 1 here is just a placeholder job id
}


## Submit stage 2 jobs (pred lts, aggregate by geography and sex, collapse and compile) ------------
if (type %in% c("post_models", "post_raking", "all", "validation", "agg_raked_yll")) {

  cause <- basename(dir)
  parent_dir <- dirname(dir)

  if (type != "agg_raked_yll") {

    # Life tables (by year, sex, race)
    if (run_lt) {

      mem_lt <- 15

      if((by_race|by_edu) & !raked & n.sims == 1000){
        mem_lt <- mem_lt * 4
      } else if (n.sims == 1000) {
        mem_lt <- mem_lt * 4
      }

      run_time_lt <- "10:00:00"

      jids[sex != 3 & (!by_race | race != all_pop_id),
           pred_lt := sbatch(code = fs::path_package("sae.shared", "scripts", "pred_lt.r"),
                           name = paste("pred_lt", year, sex, race, raked, edu, sep = "_"),
                           arguments = c("--dir", dir, "--year", year, "--sex", sex, "--race", race, "--raked", raked, "--edu", edu),
                           hold = c(unique(pred_mx),
                                    unique(pred_sub),
                                    unique(save_sub)),
                           fthread = 8, m_mem_free = paste0(mem_lt, "G"),
                           h_rt = run_time_lt, archive = T,
                           skip_if_exists = if (resub) cause_mx_draws_path(root = parent_dir, acause = cause, measure = "lt", type = "est", area_var = "mcnty",
                                                                           year = year, sex = sex, race = race, edu = edu, raked = ifelse(raked, "raked", "unraked")),
                           project = project, queue = queue, priority = priority,
                           sgeoutput = if (testing) dir),
           by = "year,sex,race,edu"]

      saveRDS(jids[,.(level,year,sex,race,edu,race_fit,edu_fit,pred_lt)], paste0(parent_dir,"/lt_jids.rds"))

    } 

    # Predict area-sex-level YLL Rates (by year, sex, race)
    # Read in the life table JIDs
    if(file.exists(paste0(parent_dir, "/lt_jids.rds"))) {
      relevant_jids <- readRDS(paste0(parent_dir, "/lt_jids.rds"))
      setnames(relevant_jids,"pred_lt", "lt_hold_jid")
      jids <- merge(jids, relevant_jids, by=c("level","year","sex","race","edu","race_fit","edu_fit"))
    } else {
      jids[,lt_hold_jid := 1]
    }

    if (!validate & all_cause_dependency) {
      mem_yll <- 15
      if((by_race|by_edu) & !raked & n.sims == 1000) {
        mem_yll <- mem_yll * 2
      }


      jids[sex != 3 & (!by_race | race != all_pop_id),
           pred_yll := sbatch(code = fs::path_package("sae.shared", "scripts", "pred_yll.r"),
                            arguments = c("--dir", dir, "--year", year, "--sex", sex, "--race", race, "--raked", raked, "--edu", edu),
                            name = paste("pred_yll", year, sex, race, raked, edu, sep = "_"),
                            hold = c(unique(lt_hold_jid),
                                     unique(pred_mx),
                                     unique(pred_sub),
                                     unique(save_sub)),
                            fthread = 8, m_mem_free = paste0(mem_yll, "G"),
                            h_rt = "01:00:00", archive = T,
                            skip_if_exists = if (resub) cause_mx_draws_path(root = parent_dir, acause = cause, measure = "yll", type = "est", area_var = "mcnty",
                                                                            year = year, sex = sex, race = race, edu = edu, raked = ifelse(raked, "prelim_raked", "unraked")),
                            project = project, queue = queue,
                            sgeoutput = if (testing) dir),
           by = "year,sex,race,edu"]

    } else {  
      jids[, pred_yll := NA]
    }

  } else { 
    jids[, pred_yll := NA] 
  }

  # allow skipping of MX and/or YLL aggregation based off of flags
  agg_skip_flags <- c(
    if (skip_yll_aggregation) CLI.Flags$skip_yll_aggregation, 
    if (skip_mx_aggregation) CLI.Flags$skip_mx_aggregation, 
    CLI.Flags$skip_yld_aggregation
  )

  # for testing purposes
  print(agg_skip_flags)


  generate_skip_files = function(cause, area_var, year, sex, race, edu, raked){
    
    if (skip_yll_aggregation && skip_mx_aggregation) return(NULL)
    skip_files = c(
      if (!skip_mx_aggregation) cause_mx_draws_path(root = parent_dir, acause = cause, measure = "mx",
                                                    type = "est", area_var = area_var, year = year,
                                                    sex = sex, race = race, edu = edu, raked = raked),
      if (!skip_yll_aggregation) cause_mx_draws_path(root = parent_dir, acause = cause,
                                                     measure = "yll", type = "est",
                                                     area_var = area_var, year = year, sex = sex,
                                                     race = race, edu = edu, raked = raked),
      if (run_lt) cause_mx_draws_path(root = parent_dir, acause = cause, measure = "lt", type = "est",
                                  area_var = area_var, year = year, sex = sex, race = race,
                                  edu = edu, raked = raked)
    )
    return(skip_files)
  }

  # Aggregate races (by year, sex
  if (by_race) { 

    mem_race <- 75
    if (n.sims == 1000 & !raked) {
      mem_race <- mem_race * 2
    }

    h_race <- "06:00:00"

    if (!validate) {
      jids[sex != 3,
           agg_races := sbatch(code = fs::path_package("sae.shared", "scripts", "agg_races.r"),
                             arguments = c(dir, year, sex, edu, raked, run_lt, agg_skip_flags),
                             name = paste("agg_race", year, sex, race, raked, edu, sep = "_"),
                             hold = na.omit(c(unique(lt_hold_jid), 
                                              unique(pred_yll),
                                              unique(pred_mx),
                                              unique(pred_sub),
                                              unique(save_sub))),
                             fthread = 8, m_mem_free = paste0(mem_race, "G"), h_rt = h_race, archive = T,
                             skip_if_exists = if (resub) generate_skip_files(cause, area_var, year, sex, all_pop_id, edu, ifelse(raked, "raked", "unraked")),
                             project = project, queue = queue,
                             sgeoutput = if (testing) dir),
           by = "year,sex,edu"] 
    } else {
      jids[, agg_races := NA]
    }
  } else {
    jids[, agg_races := NA]
  }
  
  # placeholder for education aggregation
  jids[, agg_edus := NA]

  # Aggregate geographies (by level, year, sex, race)
  if (!is.null(geoagg_files)) {

    mem_geo <- 20
    if ((by_race|by_edu) & !raked & n.sims == 1000) mem_geo <- mem_geo * 2
    h_geos <- "06:00:00"

    if (!validate) {
      jids[sex != 3 & level != area_var,
           agg_geos := sbatch(code = fs::path_package("sae.shared", "scripts", "agg_geos.r"),
                            arguments = c(dir, level, year, sex, race, edu, raked, run_lt, agg_skip_flags),
                            name = paste("agg_geos", year, level, sex, race, raked, edu, sep = "_"),
                            hold = na.omit(unique(c(pred_yll, lt_hold_jid, agg_races, agg_edus,pred_sub,save_sub))),
                            fthread = 8, m_mem_free = paste0(mem_geo, "G"), h_rt = h_geos, archive = T,
                            skip_if_exists = if (resub) generate_skip_files(cause, level, year, sex, race, edu, ifelse(raked, "raked", "unraked")),
                            project = project, queue = queue, priority = priority,
                            sgeoutput = if (testing) dir),
           by = "level,year,sex,race,edu"]

    } else { 
      jids[, agg_geos := NA]
    }

  } else { 
    jids[, agg_geos := NA]
  }

  # Aggregate sexes (by level, year, race)
  mem_sex_mcnty <- 30
  if ((by_race|by_edu) & n.sims == 1000 & !raked){
    mem_sex_mcnty <- mem_sex_mcnty * 2
  }
  mem_sex_oth <- 10
  h_sex_mcnty <- "07:00:00"
  h_sex_oth <- "05:00:00"

  if (!validate) {
    jids[, agg_sex := sbatch(code = fs::path_package("sae.shared", "scripts", "agg_sex.r"),
                           arguments = c(dir, level, year, race, edu, raked, run_lt, agg_skip_flags),
                           name = paste("agg_sex", year, level, race, raked, edu, sep = "_"),
                           hold = na.omit(unique(c(pred_yll, agg_races, agg_geos, agg_edus, lt_hold_jid))),
                           fthread = 8, m_mem_free = ifelse(level[1] == "mcnty", paste0(mem_sex_mcnty, "G"),
                                                            paste0(mem_sex_oth, "G")),
                           h_rt = ifelse(level[1] == "mcnty", h_sex_mcnty, h_sex_oth), archive = T,
                           skip_if_exists = if (resub) generate_skip_files(cause, level, year, '3', race, edu, ifelse(raked, "raked", "unraked")),
                           project = project, queue = queue, priority = priority,
                           sgeoutput = if (testing) dir),
         by = "level,year,race,edu"]

  } else {
    jids[, agg_sex := NA]
  }

  # Compile estimates
  mem_comp <- 15
  if ((by_race|by_edu)) {
    mem_comp <- mem_comp * 3
  }

  jids[, compile := sbatch(code = fs::path_package("sae.shared", "scripts", "compile_estimates.r"),
                         arguments = c(dir, raked, run_lt, validate, agg_skip_flags),
                         name = paste("compile_estimates", year, sep = "_"),
                         hold = if (run_lt) na.omit(unique(c(agg_sex, pred_mx, pred_lt))) else na.omit(unique(c(agg_sex, pred_mx))),
                         fthread = 2, m_mem_free = paste0(mem_comp, "G"), h_rt = "04:00:00", archive = T,
                         project = project, queue = queue,
                         sgeoutput = if (testing) dir)]
} 

job.ids <- vector("numeric")

jid.cols <- c(
  "prep_inputs",
  "fid_mod",
  "plot_mod",
  "pred_mx",
  "pred_lt",
  "pred_yll",
  "agg_races",
  "agg_edus",
  "agg_geos",
  "agg_sex",
  "agg_mater_neonat",
  "compile"
)

for (jid.col in jid.cols) {
  if (jid.col %in% colnames(jids)) {
    vals <- na.omit(jids[[jid.col]])
    vals <- vals[vals != 1]
    job.ids <- c(job.ids, unique(vals))
  }
}

if (length(job.ids) == 0) {
  stop_or_quit("Queued 0 jobs - all work skipped", status = 0)
}

fwrite(list(jobs = job.ids), file = file.path(dir, paste0("runtime_info.latest_jobs.", type, ".txt")))

## Save runtime info to dir -----------------------------------------------------------------------
info <- c("", "", "",
          paste("Time Submitted:", Sys.time()),
          paste("Runtime arguments:", dir, run_lt, type, resub),
          "", "")

options(width = 500, max.print = 50000)
sink(file = paste0(dir, "/runtime_info.txt"), append = T)
cat(paste(info, collapse = "\n"))
data.frame(jids)
sink()

saveRDS(jids, paste0(dir, '/submit_single_cause_jids_', type, '.rds'))
