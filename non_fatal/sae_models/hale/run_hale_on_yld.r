####################################################################################################
## Inputs:      yld_dir [character] -- the directory for Years Lost to Disease (yld) input model data
##              lt_dir [character] -- the directory for Life Table (lt) input model data
##              year [int] -- year to process
##              sex [int] -- sex to process
##              race [int] -- race to process
##              draws [int] -- draws in input datasets
##              hale_version [int] -- Central Comp version ID
##              save_loc [character] -- the directory to save output data to
##
##
####################################################################################################
cat(sprintf("JOB_ID is %s\n", Sys.getenv("JOB_ID", "<no JOB_ID env var>")))

repo <- paste0("FILEPATH")

library(data.table)
library(reticulate)
library(plyr)

## Get settings ------------------------------------------------------------------------------------

get_args <- function () 
{
  args <- commandArgs(TRUE)
  if (is.null(args) || length(args) == 0) {
    args <- commandArgs(FALSE)
    idx <- match("--no-save", args)
    if (is.na(idx)) {
      message(sprintf("--no-save not detected in commandArgs() - cannot determine CLI args from '%s'", 
                      paste(commandArgs(), collapse = " ")))
      args <- character(0)
    }
    else if (idx == length(args)) {
      args <- character(0)
    }
    else {
      args <- args[-seq_len(idx + 1)]
    }
  }
  return(args)
}

stop_or_quit <- function (msg, status = 1, override_quit = F) 
{
  if (rlang::is_interactive() | override_quit) {
    stop(msg)
  }
  else {
    message(msg)
    quit(save = "no", status = status)
  }
}

if (interactive()) {
} else {
  
  parser <- argparse::ArgumentParser()
  parser$add_argument("--yld_dir", default = "FILEPATH", type = "character", help = "Years Lost to Disease (YLD) directory")
  parser$add_argument("--lt_dir", default = "FILEPATH", type = "character", help = "Life Table (LT) directory")
  parser$add_argument("--year", default = 2010L, type="integer", help="Year to use for input/output files.", choices=c(1950:2200))
  parser$add_argument("--sex", default = 2L, type = "integer", help="Sex to use for input/output files.", choices=c(1, 2, 3))
  parser$add_argument("--race", default = 1L, type = "integer", help="Race to use for input/output files.")
  parser$add_argument("--edu", default = 1L, type = "integer", help="Edu to use for input/output files.")
  parser$add_argument("--draws", default = 100L, type = "integer", help="Number of draws in input data.")
  parser$add_argument("--save_loc", default = "yld_dir", type = "character", help="Location to save final outputs to.")
  
  args <- parser$parse_args(get_args())
  for (key in names(args)) {
    assign(key, args[[key]])
  }
  if (save_loc == "yld_dir") save_loc = yld_dir
  
  print("Arguments passed were:")
  for (arg in names(args)){
    print(paste0(arg, ": ", get(arg)))
  }
}

# The default location for the python virtual env.
if (Sys.getenv("RETICULATE_PYTHON") == "") {
  Sys.setenv("RETICULATE_PYTHON"='FILEPATH')
}

levels <- c("mcnty", "state", "natl")

print(paste0("Using Python interpreter from ", Sys.getenv("RETICULATE_PYTHON")))
hale <- reticulate::import_from_path("hale")

for (current_level in levels) {
  print(current_level)
  # read input files (reading unraked to get age_id mapping)
  yld_file = paste0(yld_dir, sprintf(paste0("/yld_draws_", current_level, "_%d_%d_%d_%d.rds"), year, sex, race, edu))
  if (file.exists(yld_file)) {
    unraked_yld <- readRDS(yld_file)
  } else {
    stop_or_quit(sprintf("File %s not present", yld_file), status=51)
  }
  
  yld_file = paste0(yld_dir, sprintf(paste0("/yld_draws_", current_level, "_%d_%d_%d_%d_raked.rds"), year, sex, race, edu))
  if (file.exists(yld_file)) {
    yld <- readRDS(yld_file)
  } else {
    stop_or_quit(sprintf("File %s not present", yld_file), status=51)
  }
  
  lt_file = paste0(lt_dir, sprintf(paste0("/FULL_lt_draws_", current_level, "_%d_%d_%d_%d_raked.rds"), year, sex, race, edu))
  if (file.exists(lt_file)) {
    lt <- readRDS(lt_file)
  } else {
    stop_or_quit(sprintf("File %s not present", lt_file), status=51)
  }
  
  # HALE expects draws from 0:999 instead of 1:1000
  yld[,sim := sim - 1]
  lt[,sim := sim - 1]
  
  # check that both datasets have the same ages for processing
  if (!identical(unique(lt$age), unique(yld$age))) {
    print("Ages don't match in life tables and ylds, only using ages present in both")
    l_ages <- intersect(unique(lt$age), unique(yld$age))
    lt <- lt[age %in% l_ages,]
    yld <- yld[age %in% l_ages,]
    unraked_yld <- unraked_yld[age %in% l_ages,]
  }
  
  # yld data has age_group_id's but lt does not.  This maps the ages to age_group_ids for LT 
  # (assumes that ages and age_group_ids are sorted the same way)
  lt$age_group_id <- mapvalues(lt$age, from=unique(unraked_yld$age), to=unique(unraked_yld$age_group_id))
  yld$age_group_id <- mapvalues(yld$age, from=unique(unraked_yld$age), to=unique(unraked_yld$age_group_id))
  
  # HALE expects nLx
  lt <- setnames(lt, c("Lx"), c("nLx"))
  
  # HALE expects wide format inputs
  # removing age, level and race from yld so that duplicate columns are not created in merge in python code
  yld_w <- dcast(data.table(subset(yld, select=-c(race, level, age))), year+area+sex+age_group_id~sim, value.var = list("yld"))
  
  # subsetting to adjusted == 1 because I had to choose one or the other
  lt_w <- dcast(lt, year+area+level+age+sex+race+age_group_id~sim, value.var = list("mx", "ax", "qx", "lx", "dx", "nLx", "Tx", "ex"))
  
  # HALE expects sex_id, year_id, location_id
  yld_w <- setnames(yld_w, c("sex", "year", "area"), c("sex_id", "year_id", "location_id"))
  lt_w <- setnames(lt_w, c("sex", "year", "area"), c("sex_id", "year_id", "location_id"))
  
  # fix yld_data draw names
  yld_w <- setnames(yld_w, paste0(c(seq(0,draws-1))), paste0("yld_", c(seq(0, draws-1))))
  
  # call HALE code
  hale_df <- hale$calculate_hale(lt_w, yld_w, unique(yld$age_group_id), draws)
  
  # might want to output hale_df at this point to look at all input and output datasets for debugging
  
  # drop extra data
  subset_vars_hale <- c("year_id", "location_id", "level", "sex_id", "race", "age", "age_group_id", paste0("draw_", c(seq(0, draws-1))))
  subset_vars_ex <- c("year_id", "location_id", "level", "sex_id", "race", "age", "age_group_id", paste0("ex_", c(seq(0, draws-1))))
  hale_sub <- subset(hale_df, select=subset_vars_hale)
  ex_sub <- subset(hale_df, select=subset_vars_ex)
  
  # Convert back to long format
  hale_sub_l <- melt.data.table(data.table(hale_sub), id.vars = c("age_group_id", "year_id", "location_id", "level", "sex_id", "race", "age"), variable.name = "sim")
  ex_sub_l <- melt.data.table(data.table(ex_sub), id.vars = c("age_group_id", "year_id", "location_id", "level", "sex_id", "race", "age"), variable.name = "sim")
  
  if (!is.factor(hale_sub_l$sim)) {
    stop_or_quit("variable sim coming out of melt.data.table is not a factor")
  }
  # converts "draw_0" to 1. THIS ONLY WORKS BECAUSE sim IS A FACTOR!
  hale_sub_l <- hale_sub_l[,sim := as.integer(sim)]
  ex_sub_l <- ex_sub_l[,sim := as.integer(sim)]
  
  # Convert names
  hale_sub_l <- setnames(hale_sub_l, c("sex_id", "year_id", "location_id", "value"), c("sex", "year", "area", "HALE"))
  ex_sub_l <- setnames(ex_sub_l, c("sex_id", "year_id", "location_id", "value"), c("sex", "year", "area", "ex"))
  
  # Merge HALE and ex
  merged <- merge(hale_sub_l, ex_sub_l, by = c("age_group_id", "year", "area", "level", "sex", "race", "age", "sim"), all = TRUE)
  merged[, diff := ex - HALE]
  
  # create output directory if not present
  dest_dir <- file.path(save_loc)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # output file
  ofile = paste0(save_loc, sprintf(paste0("/hale_draws_", current_level, "_%d_%d_%d_%d.rds"), year, sex, race, edu))
  saveRDS(merged, ofile, compress = TRUE)
}
