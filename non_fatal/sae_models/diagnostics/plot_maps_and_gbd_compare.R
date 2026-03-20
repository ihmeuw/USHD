####################################################################################################
## Description: Make maps of estimate mean, upper and lower bounds and se. Code comparing to GBD
##                estimates is not currently functional.
##
## Passed args: output_dir [character] -- file path to look for settings and estimates for model 1
##              dir2 [character list] -- file path(s) to look for settings and estimates for comparison
##                models()   
##              v1 [character] -- label for model 1; it does not matter what this is
##              v2 [character list] -- label(s) for comparison model(s); it does not matter what this is
##              plot_types [character] -- either standard, compare, or standardxxcompare;
##                the last option gets split into the individual terms, and it indicates
##                that the user wants to plot both types of plots (xx is just there for nice
#                 script submission)
##              adjusted [integer] -- 0 if plotting unadjusted estimates; 1 if plotting adjusted estimates
##                This gets transformed to "unadjusted" and "adjusted" to be used in the plot file names
##              plot_years [integer list] -- years that you want to plot for (all years will take
##                a long time and the PDFs will be very large)
##              age_plot [integer list] -- ages that you want to plot
##
## Requires:    Cases and population input data (data_file and pop_file)
##               ("[dir]/est_all.rds")
##
## Outputs:     If running just standard plots: ("[dir]/model_results.pdf")
##
##              If running comparison plots (if more than one comparison model, each of these is created
##                for each comparison model): "[dir]/model_result_compare_[basename(dir2)].pdf")
##
##              If running both sets of plots, then all of the above plots are created
####################################################################################################

###### Load required libraries
pacman::p_load(R.utils, data.table, TMB, ggplot2, parallel, gridExtra, RColorBrewer, dplyr, stringr, sf, forcats, cowplot)

###### Set up objects from command args
if (!interactive()) {
  (repo <- commandArgs(TRUE)[[1]])
  (output_dir <- commandArgs(TRUE)[[2]])
  (settings_loc  <- commandArgs(TRUE)[[3]])
  (output_dir_draws_est <- commandArgs(TRUE)[[4]])
}

###### Source functions
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  source(paste0(repo, "/functions/", func))
}

###### Assign settings from settings file
get_settings(settings_loc)

# has to be loaded after settings loaded if interactive
if (interactive()) {
  if (outcome[1] %like% 'obese'){
    output_dir_draws_est <- paste0(draws_est_output_dir,'/obese/', run_date)
  } else if (outcome[1] %like% 'overweight'){
    output_dir_draws_est <- paste0(draws_est_output_dir,'/overweight/', run_date)
  } else if (outcome[1] %like% 'underweight'){
    output_dir_draws_est <- paste0(draws_est_output_dir,'/underweight/', run_date)
  } else {
    output_dir_draws_est <- paste0(draws_est_output_dir, run_date)
  }
}

load("FILEPATH")


v1 <- "new_results"
v2 <- NULL

plot_types <- "standard"

n_models <- length(c(v1, v2))
combos <- data.table(model1 = v1, model2 = v2)

colors <- rev(brewer.pal(9, "Spectral"))

read_dt <- function(dir, version) {
  dir_dt <- readRDS(paste0(dir, "/est/pred_est_all.rds"))
  dir_dt$version <- version
  
  settings <- as.data.table(read.csv(settings_loc, header = F))
  ages <- eval(parse(text=as.character(settings[V1 == "ages", V2])))
  years <- eval(parse(text=as.character(settings[V1 == "years", V2])))
  
  dir_dt <- dir_dt[year %in% years]
  
  return(dir_dt)
}

all_dirs <- list()
dir0_dt <- read_dt(output_dir_draws_est, v1)
all_dirs[[length(all_dirs) + 1]] <- dir0_dt

all_preds <- rbindlist(all_dirs, use.names = T)

message("Done combining data")

for (i in 0:(length(all_dirs) - 1)) {
  rm(list = paste0("dir", i, "_dt"))
}
rm(all_dirs)
gc()


#### Convert area to mcnty
## Load recode mapping
recode <- readRDS(paste0(output_dir, "/recode.rds"))$area

## maps
shp_mcnty <- readRDS("FILEPATH")
shp_state <- readRDS("FILEPATH")

###### Check states object
if (!(is.null(states) | states[[1]] == "all")) { # Specific states were requested
  ### Restrict states shapefile to only those states that were requested
  shp_state <- shp_state[shp_state@data$state_name %in% states,]
  
  ### Restrict mcnty shapefile to only those states that were requested
  shp_mcnty <- shp_mcnty[shp_mcnty@data$state_name %in% states,]
}

shp_state <- as(shp_state, "sf")
shp_mcnty <- as(shp_mcnty, "sf")

# merge on metadata
if (by_race) {
  races_together <- data.table(race_group = c(race_labels, 'All Races'), race = c(races, 1))[, race := as.numeric(race)]
  all_preds <- merge(all_preds, races_together, by = "race", all.x = T)
  all_preds[, race_group := fct_reorder(race_group, race)]
} else {
  all_preds[, race_group := "All"]
}

all_preds[, sex_name := ifelse(sex == 1, "Male", ifelse(sex == 2, "Female", "Both"))]
s_labels <- unique(all_preds$sex_name)

# Determine if source or source_v2 is used, and rename it to source if source_v2 is used
if(by_source){
  if(any(grepl("source_v2", names(all_preds)))){
    setnames(all_preds, "source_v2", "source")
  } 
}

if (by_source) {
  all_compare  <- melt.data.table(all_preds, id.vars = c("level", "area", "year", "sex", "sex_name", "race", "race_group", "age", "version", "source"))
} else {
  all_compare  <- melt.data.table(all_preds, id.vars = c("level", "area", "year", "sex", "sex_name", "race", "race_group", "age", "version"))
}
setnames(all_compare , "variable", "measure")

if (by_source) {
  all_compare <- dcast.data.table(all_compare, level + area + year + sex + sex_name + race + race_group + age + source + measure ~ version, value.var = "value")
} else {
  all_compare <- dcast.data.table(all_compare, level + area + year + sex + sex_name + race + race_group + age + measure ~ version, value.var = "value")
}

### function to plot maps
plot_compare <- function(m, comp, measure_name, data, vals_num, vals_trans, compare, v1, single = FALSE) {
  if (compare) {
    fill_val <- paste0(comp, "_", mod)
    ti <- paste0(comp, " ", measure_name, " vs ", v1 , ", ", y, ", ", sx, ", Age ", a) # in this case, v1 is actually the directory name of the comparison model
  } else {
    fill_val <- v1
    ti <- paste0(measure_name, ", ", y, ", ", sx, ", Age ", a)
  }
  
  if (single) {
    ti <- paste0(ti, ", ", unique(eval(parse(text = data))$race_group))
    if (by_source) {
      p1 <- ggplot(data = eval(parse(text = data))) + 
        geom_sf(aes(fill = get(fill_val), geometry = geometry), color = "transparent", lwd = 0) +
        geom_sf(data = shp_state, fill = NA, color = "gray80", lwd = 0.1) +
        scale_fill_gradientn(colors = colors, name = paste0(measure_name), labels = function(x) {round(x, 2)}, limits = c(eval(parse(text = vals_num))[1], eval(parse(text = vals_num))[length(eval(parse(text = vals_num)))]), trans = "identity") +
        coord_sf(expand = F, datum = NA) +
        labs(title = ti) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"), plot.title = element_text(hjust = 0.5, face = "bold"), panel.border = element_blank()) +
        guides(color = "none") +
        theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
              axis.title = element_blank(), panel.grid.major = element_line(colour = "transparent"),
              panel.border = element_blank(),  panel.spacing = unit(0.1, "lines"),
              legend.position = "bottom", legend.margin = margin(0, 0, 0, 0, unit = "lines"),
              legend.box = "vertical", strip.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5, nbin = 100, direction = "horizontal", title = measure_name), title.position = "top", order = 1)
    } else {
      p1 <- ggplot(data = eval(parse(text = data))) + 
        geom_sf(aes(fill = get(fill_val), geometry = geometry), color = "transparent", lwd = 0) +
        geom_sf(data = shp_state, fill = NA, color = "gray80", lwd = 0.1) +
        scale_fill_gradientn(colors = colors, name = paste0(measure_name), labels = function(x) {round(x, 2)}, limits = c(eval(parse(text = vals_num))[1], eval(parse(text = vals_num))[length(eval(parse(text = vals_num)))]), trans = "identity") +
        coord_sf(expand = F, datum = NA) +
        labs(title = ti) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"), plot.title = element_text(hjust = 0.5, face = "bold"), panel.border = element_blank()) +
        guides(color = "none") +
        theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
              axis.title = element_blank(), panel.grid.major = element_line(colour = "transparent"),
              panel.border = element_blank(),  panel.spacing = unit(0.1, "lines"),
              legend.position = "bottom", legend.margin = margin(0, 0, 0, 0, unit = "lines"),
              legend.box = "vertical", strip.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5, nbin = 100, direction = "horizontal", title = measure_name), title.position = "top", order = 1)
    }
    
  } else {
    if (by_source) {
      p1 <- ggplot(data = eval(parse(text = data))) + facet_wrap( ~ race_group, ncol = 3) +
        geom_sf(aes(fill = get(fill_val), geometry = geometry), color = "transparent", lwd = 0) +
        geom_sf(data = shp_state, fill = NA, color = "gray80", lwd = 0.1) +
        scale_fill_gradientn(colors = colors, name = paste0(measure_name), labels = function(x) {round(x, 2)}, limits = c(eval(parse(text = vals_num))[1], eval(parse(text = vals_num))[length(eval(parse(text = vals_num)))]), trans = "identity") +
        coord_sf(expand = F, datum = NA) +
        labs(title = ti) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"), plot.title = element_text(hjust = 0.5, face = "bold")) +
        guides(color = "none") +
        theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
              axis.title = element_blank(), panel.grid.major = element_line(colour = "transparent"),
              panel.border = element_blank(),  panel.spacing = unit(0.1, "lines"),
              legend.position = "bottom", legend.margin = margin(0, 0, 0, 0, unit = "lines"),
              legend.box = "vertical", strip.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5, nbin = 100, direction = "horizontal", title = measure_name), title.position = "top", order = 1)
    } else {
      p1 <- ggplot(data = eval(parse(text = data))) + facet_wrap(~ race_group, ncol = 3) +
        geom_sf(aes(fill = get(fill_val), geometry = geometry), color = "transparent", lwd = 0) +
        geom_sf(data = shp_state, fill = NA, color = "gray80", lwd = 0.1) +
        scale_fill_gradientn(colors = colors, name = paste0(measure_name), labels = function(x) {round(x, 2)}, limits = c(eval(parse(text = vals_num))[1], eval(parse(text = vals_num))[length(eval(parse(text = vals_num)))]), trans = "identity") +
        coord_sf(expand = F, datum = NA)+
        labs(title = ti) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"), plot.title = element_text(hjust = 0.5, face = "bold")) +
        guides(color = "none") +
        theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
              axis.title = element_blank(), panel.grid.major = element_line(colour = "transparent"),
              panel.border = element_blank(),  panel.spacing = unit(0.1, "lines"),
              legend.position = "bottom", legend.margin = margin(0, 0, 0, 0, unit = "lines"),
              legend.box = "vertical", strip.background = element_blank(), plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5, nbin = 100, direction = "horizontal", title = measure_name), title.position = "top", order = 1)
    }
  }
  
  return(p1)
}

if ("standard" %in% plot_types) {
  pdf(paste0(output_dir, "/mapped_results.pdf"), width = 11, height = 8.5)
  message("Plotting new values")
  for (a in unique(c(98, 99))) {
    message(a)
    for (sx in s_labels[s_labels != "Both"]) {
      message(sx)
      for (y in years) {
        message(y)
        for (m in "pred_mean") {
          message(m)
          measure_name <- m
          comp <- "New"
          
          # values plots
          min_break_val <- min(all_compare[measure == m & age == a, ..v1], na.rm = T)
          max_break_val <- max(all_compare[measure == m & age == a, ..v1], na.rm = T)
          assign(paste0("vals_break_val_", m), seq(min_break_val, max_break_val, length.out = 10))
          assign(paste0("vals_break_trans_val_", m), (eval(parse(text = paste0("vals_break_val_", m))) - eval(parse(text = paste0("vals_break_val_", m)))[1]) / max(eval(parse(text = paste0("vals_break_val_", m))) - eval(parse(text = paste0("vals_break_val_", m)))[1]))
          
          shapes_to_plot_mcnty <- merge(shp_mcnty, all_compare[level == "mcnty" & year == y & age == a & sex_name == sx & measure == m], by.x = "mcnty", by.y = "area", allow.cartesian = T)
          assign(paste0("shapes_to_plot_mcnty_", m), shapes_to_plot_mcnty)
          
          print(plot_compare(m, comp, measure_name, data = paste0("shapes_to_plot_mcnty_", m),
                             vals_num = paste0("vals_break_val_", m),
                             vals_trans = paste0("vals_break_trans_val_", m), compare = FALSE,
                             v1 = v1))
        }
      }
    }
    
    for (sx in s_labels[s_labels != "Both"]) {
      message(sx)
      for (y in years) {
        message(y)
        for (r in c(1)) {
          # values plots
          min_break_val <- min(all_compare[measure == m & age == a, ..v1], na.rm = T)
          max_break_val <- max(all_compare[measure == m & age == a, ..v1], na.rm = T)
          assign(paste0("vals_break_val_", m), seq(min_break_val, max_break_val, length.out = 10))
          assign(paste0("vals_break_trans_val_", m), (eval(parse(text = paste0("vals_break_val_", m))) - eval(parse(text = paste0("vals_break_val_", m)))[1]) / max(eval(parse(text = paste0("vals_break_val_", m))) - eval(parse(text = paste0("vals_break_val_", m)))[1]))
          
          shapes_to_plot_mcnty <- merge(shp_mcnty, all_compare[level == "mcnty" & year == y & age == a & sex_name == sx & measure == m & race == r], by.x = "mcnty", by.y = "area", allow.cartesian = T)
          assign(paste0("shapes_to_plot_mcnty_", m), shapes_to_plot_mcnty)
          
          print(plot_compare(m, comp, measure_name, data = paste0("shapes_to_plot_mcnty_", m),
                             vals_num = paste0("vals_break_val_", m),
                             vals_trans = paste0("vals_break_trans_val_", m), compare = FALSE,
                             v1 = v1, single = TRUE))
        }
      }
    }
  }
  dev.off()
}
