####################################################################################################
## Description: Launches aggregate_injury_child_causes.r as an array job because otherwise it runs very slowly
##              for a race/ethnicity model
##
##
## Inputs:      dir [character] -- the directory for all models
##
####################################################################################################

stopifnot(grepl("mortality/sae_models$", getwd()))

suppressPackageStartupMessages({
  library(R.utils)
  library(data.table)
  sourceDirectory("functions/")
})

user <- Sys.getenv("USER")

dir <- commandArgs()[4]
resub <- as.logical(commandArgs()[5])

get_settings(dir)

# settings
script <- paste0("/post_estimation/aggregate_injury_child_causes.r")
queue <- "QUEUE"
fthread <- 2
m_mem_free <- "80G"
h_rt <- "05:00:00"
archive <- T
project <- "PROJ"

raked <- F
validate <- F
lt <- F 

## Build combos that we need to run
combos <- as.data.table(expand.grid(geo_level = c(area_var, names(geoagg_files)),
                                    sex = c(sexes, 3),
                                    year = years,
                                    race = unique(c(races, all_pop_id)),
                                    var = c("mx"), # yll
                                    edu = unique(c(edu_groups, all_pop_id))))

write.csv(combos, paste0(dir, "/_inj/", "/model_args.csv"))

run_at_once <- 500

id <- sbatch(code = script,
             arguments = c(dir, resub),
             name = paste0("agg_injuries"),
             fthread = fthread, 
             m_mem_free = m_mem_free,
             h_rt = h_rt, 
             archive = archive,
             project = project, 
             queue = queue,
             sgeoutput = dir, 
             array = paste0("1-", nrow(combos)), 
             array_throttle = run_at_once)


mem_comp <- 15
if (by_race) {
  mem_comp <- mem_comp*3
}

skip_mx_aggregation <- ""
skip_yll_aggregation <- ""

if(!("mx" %in% combos$var)) skip_mx_aggregation <- "--skip_mx_aggregation"
if(!("yll" %in% combos$var)) skip_yll_aggregation <- "--skip_yll_aggregation"

agg_skip_flags <- c(skip_mx_aggregation, skip_yll_aggregation, "--skip_yld_aggregation",
                    "--skip_pred_aggregation")

sbatch(code = fs::path_package("sae.shared", "scripts", "compile_estimates.r"),
       arguments = c(paste0(dir,"/_inj/"), raked, lt, validate, agg_skip_flags),
       hold = id, 
       fthread = 2, m_mem_free = paste0(mem_comp,"G"), h_rt = "04:00:00",
       archive = archive,
       project = project, queue = queue,
       sgeoutput = paste0(dir, "/_inj/"))









