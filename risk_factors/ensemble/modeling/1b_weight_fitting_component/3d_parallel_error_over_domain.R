# set up ------------------------------------------------------------------

# Load libraries and helper
library(spatstat.geom)
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(scales)
library(stringr)
library(ggplot2)
library(RColorBrewer)
'%notlike%' <- Negate('%like%')
'%ni%' <- Negate('%in%')

invisible(sapply(list.files("FILEPATH", full.names = T), source))
source(paste0("FILEPATH")) # updated functions to solve for dist parameters

source(paste0("FILEPATH/1_parallel_helper_functions.R"))
source(paste0("FILEPATH/curve_creation_functions.R"))
source(paste0("FILEPATH/plot_edensity_components.R"))
source(paste0("FILEPATH/plot_weights_score.R"))

## Get info from launch script

if(interactive()){
  task_id = 1
  args = c("FILEPATH", "TRUE")
} else{
  task_id <- ifelse(Sys.getenv('SLURM_ARRAY_TASK_ID') != '', as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')), NA)
  args <- commandArgs(trailingOnly = TRUE)
}
param_map_filepath <- args[1]
by_demo <- as.logical(args[2])
param_map <- fread(param_map_filepath)

me_type <- "high_bmi"
all.combos <- fread(paste0("FILEPATH"))
offset <- 0
# add a row for the GBD 2020 high BMI weights
all.combos <- rbind(all.combos,
      as.data.table(list(strategy = "GBD2020_metab_bmi", option = NULL, launch.date = "GBD2020_metab_bmi", microdata.filepath = NULL, me_type = "high_bmi", index = nrow(all.combos)+1)),
      fill = T)

# prepping output filepath
# Now need to plan PDF output filepath

get_recent<-function(folder, pattern=NULL, sheet=NULL, path=F){
  require(data.table)
  
  files<-list.files(folder, full.names=T, pattern=pattern)
  files<-files[!grepl("\\~\\$", files)]
  infoo<-file.info(files)
  most_recent_path<-row.names(infoo[infoo$mtime==max(infoo$mtime),])
  
  return(most_recent_path)
  
}

output.fp <- get_recent(folder = paste0("FILEPATH"))


pdf_filepath = paste0(output.fp, "/", me_type, "_weight_comparisons.pdf")


#read in dataset (this must have a column 'data' with the values and column 'nid_loc_yr_index')
data_filepath = unique(all.combos[strategy != "GBD2020_metab_bmi"]$microdata.filepath)
data <- readRDS(data_filepath)



# process settings associated with weight sets ----------------------------

# load the settings used for all of the runs we're comparing, read in the thresholds, and
#   check that they are the same
offset = 0
optim_type = "sbplx"
settings <- rbindlist(lapply(all.combos[strategy != "GBD2020_metab_bmi"]$launch.date, function(v) {
  temp <- fread(list.files(paste0("FILEPATH", v), pattern = "param*", full.names = T))
}), fill = T)
settings[, launch_paths_expanded := ifelse(is.na(launch_paths_expanded ), launch.date, launch_paths_expanded )]
thresholds <- unique(settings[, .(low_threshold, medium_threshold, high_threshold)])
if(thresholds[, .N] != 1) stop("There should be exactly 1 set of thresholds used across all models comparing. Found ", thresholds[, .N])
low_threshold <-thresholds$low_threshold
medium_threshold <- thresholds$medium_threshold
high_threshold <- thresholds$high_threshold

#get list of distributions included in the ensemble
all.combos[, index := .I]
all.combos[, c("me_type", "strategy", "option") := lapply(.SD, as.character),
           .SDcols = c("me_type", "strategy", "option")]

all.combos

# get the weights placed on each threshold
setting_weights <- unique(settings[, .(setting_weight = paste(paste(c(low_threshold, medium_threshold, high_threshold), 
                  paste0(round(c(low_weight, medium_weight, high_weight), digits = 3)*100, "%"), sep = "="), collapse = " ")),
         .(1:nrow(settings), launch_paths_expanded)][, nrow := NULL][, launch.date := launch_paths_expanded])

setting_weights[, launch.date := gsub("/$", "", launch.date)] # remove trailing "/" so that can merge with the launch.date in all.combos (trailing / added when extra_dim_path_name is "")
all.combos <- setting_weights[all.combos, on = c(launch.date = "launch.date")]

weight_list <- vector(mode = "list", length = uniqueN(all.combos$index))


# generate fits associated with each weight set ---------------------------

all.ests.combined <- lapply(all.combos$index, function(index) {
  
  var <- all.combos[index]$me_type
  strat <- all.combos[index]$strategy
  opt <- all.combos[index]$option
  launch.date <- as.character(all.combos[index]$launch.date)
  setting_weight <- all.combos[index]$setting_weight
  
  if (strat == "m2") {
    
    weights = fread(paste0("FILEPATH"))
    weight_list[[index]] <<- weights
    
    output <- lapply(unique(data$nid_loc_yr_index), function(nid_use){
      
      rows <- data[nid_loc_yr_index == get("nid_use")]
    
      output <- data_to_curves(Data = rows$data, svy_wt = rows$svy_wt, distlist = distlist, 
                               weights = as.list(weights),
                               nid_loc_yr_i = get("nid_use"))
      print(paste0("NID Index " ,nid_use, " out of ", max(data$nid_loc_yr_index)))
      output[(which.min(abs((offset+low_threshold)-xpoints))), category := "underweight"]
      output[(which.min(abs((offset+medium_threshold)-xpoints))), category := "overweight"]
      output[(which.min(abs((offset+high_threshold)-xpoints))), category := "obese"]
      output[is.na(category), category := "other.point"]

      return(output)
      
    })
    
    all.ests <- rbindlist(output)
    all.ests[, variable := var]
    all.ests[, strategy := strat]
    all.ests[, option := opt]
    all.ests[, optim_type := optim_type]
    all.ests[, launch.date := launch.date]
    all.ests[, local.global := "m2global"]
    all.ests[, setting_weight := ifelse(!is.na(setting_weight), setting_weight, "")]
    
    all.ests[, index := index]
  }
  
  if(strat == "GBD2020_metab_bmi"){

    output <- lapply(unique(data$nid_loc_yr_index), function(nid_use){

      rows <- data[nid_loc_yr_index == get("nid_use")]

      weights = fread(paste0("FILEPATH"))[1][, c("location_id", "year_id", "sex_id", "age_group_id") := NULL]
      weight_list[[index]] <<- weights
      
      output <- data_to_curves(Data = rows$data, svy_wt = rows$svy_wt, distlist = distlist, 
                               weights = as.list(weights),
                               nid_loc_yr_i = get("nid_use"),
                               use_gbd_edensity = T)
      print(paste0("NID Index " ,nid_use, " out of ", max(data$nid_loc_yr_index)))

      output[(which.min(abs((offset+low_threshold)-xpoints))), category := "underweight"]
      output[(which.min(abs((offset+medium_threshold)-xpoints))), category := "overweight"]
      output[(which.min(abs((offset+high_threshold)-xpoints))), category := "obese"]
      output[is.na(category), category := "other.point"]

      return(output)

    })

    all.ests <- rbindlist(output)
    all.ests[, variable := var]
    all.ests[, strategy := strat]
    all.ests[, option := opt]
    all.ests[, optim_type := NA]
    all.ests[, launch.date := launch.date]
    all.ests[, local.global := "customglobal"]
    

    all.ests[, index := index]

  }

  return(all.ests)
  
}) %>% rbindlist(fill = T)


# Adding in some columns as labels 
all.ests.combined$index.local.global <- paste0(all.ests.combined$index, "_", all.ests.combined$local.global)
all.ests.combined[local.global %like% "local", twosplit := "local"]
all.ests.combined[local.global %like% "global", twosplit := "global"]
all.ests.combined[, index.label := paste0(me_type, "_", strategy, "_", option)]

# creating error metrics --------------------------------------------------

# Calculating error at points
all.ests.combined[, squared.error := (cdf.model.points-cdf.data.points)^2]

# Smaller dataframe with error just at mild/moderate/severe cutoffs
just.thresholds.combined <- all.ests.combined[category %in% c("underweight", "overweight", "obese")]

# Getting the sum of the squared error by each of the categories
all.ests.combined[,sum.squared.error := sum(squared.error), by = index.local.global]

#getting the sum of the squared error at just the thresholds
just.thresholds.combined[, sum.squared.error.category := sum(squared.error), by = c("index.local.global", "category")]

# Adding in an additional column label
just.thresholds.combined$index.label_plus_index <- paste0(just.thresholds.combined$index, "_", just.thresholds.combined$index.label)


# plotting labels ---------------------------------------------------------

# Creating Label Dataframe for plots
max.val <- max(just.thresholds.combined$cdf.data.points)
label.df <- distinct(just.thresholds.combined, twosplit, index.label, sum.squared.error.category, index.label_plus_index, setting_weight, launch.date)
label.df$maxval <- rep(max.val, times = nrow(label.df))
label.df$label.index <- rep(c(1:(nrow(label.df)/3)), each = 3)
stopifnot(unique(label.df[, uniqueN(label.index), launch.date]$V1) == 1) # make sure USHD changes don't mess with how index assigned
label.df[, total.sum.error := sum(sum.squared.error.category), by = "label.index"]
label.df <- label.df[!duplicated(label.index)]

# Adding in Better Labels to the Just Thresholds Data frame
label_fit_type <- function(DT){
  DT[index.label_plus_index %like% "thresholds_1", fit.type := "Entire Dist"]
  DT[index.label_plus_index %like% "thresholds_2", fit.type := "Left Half of Dist"]
  DT[index.label_plus_index %like% "thresholds_3", fit.type := "Worst Threshold"]
  DT[index.label_plus_index %like% "thresholds_4", fit.type := "Weighted SE at Pts"]
  DT[index.label_plus_index %like% "thresholds_5", fit.type := "Weighted SSE at Pts"]
  DT[index.label_plus_index %like% "custom", fit.type := "Custom Weights"]
  DT[index.label_plus_index %like% "GBD2020", fit.type := "GBD 2020"]
  
  DT[index.label_plus_index %like% "m1", how.fit := "Separately -"]
  DT[index.label_plus_index %like% "m2", how.fit := "Simultaneously -"]
  DT[index.label_plus_index %like% "custom", how.fit := "Prespecified -"]
  DT[index.label_plus_index %like% "GBD2020", how.fit := "GBD ensemble method -"]
  
  if(by_demo){
    # if fitting my demographic, more useful to specify the demographic than the model settings (because the model settings were all the same)
    DT[, final.label := basename(launch.date)]
  } else{
    DT[, final.label := paste0(how.fit, " ", fit.type, ifelse(!is.na(setting_weight), paste0("\nWt on pt: ", setting_weight), ""))]  
  }
  
  return(DT)
}

just.thresholds.combined <- label_fit_type(just.thresholds.combined)
label.df <- label_fit_type(label.df)

# Adding in final labels so we can know how things were weighted
all.combos[, index.label_plus_index := paste0(index, "_",me_type, "_", strategy, "_", option)]
just.thresholds.combined <- merge(all.combos[, c("index.label_plus_index"), with = FALSE], just.thresholds.combined, by = "index.label_plus_index")
label.df <- merge(all.combos, label.df, by = "index.label_plus_index")


# facet labels
label.df[, weight.set.label := paste0("Weight Set ", label.index)]
just.thresholds.combined[, weight.set.label := paste0("Weight Set ", index)]
label.df[, strip.label := paste(weight.set.label, final.label, sep = "\n")]
just.thresholds.combined[, strip.label := paste(weight.set.label, final.label, sep = "\n")]

label_category <- function(DT){
  DT[category == "obese", threshold := paste0("High Priority Point: ",high_threshold)]
  DT[category == "overweight", threshold := paste0("Middle Priority Point: ", medium_threshold)]
  DT[category == "underweight", threshold := paste0("Low Priority Point: ", low_threshold)]
  
  DT$threshold <- factor(DT$threshold, levels = c(paste0("High Priority Point: ",high_threshold), paste0("Middle Priority Point: ", medium_threshold), paste0("Low Priority Point: ", low_threshold))) 
  return(DT)
}
just.thresholds.combined <- label_category(just.thresholds.combined)


# plots of error at thresholds --------------------------------------------

first.overall.plot <- ggplot(just.thresholds.combined) +
  geom_point(aes(x = cdf.data.points, y = cdf.model.points, color = threshold), alpha = 0.5)+#,  color = "dodgerblue2", alpha = .2) + 
  facet_wrap(~ strip.label) +
  geom_abline() +
  labs(x = paste0("Prevalence Data at Priority Points: ", low_threshold, ", ", medium_threshold, ", ", high_threshold), 
       y = paste0("Modeled Prevalence at Priority Points: ", low_threshold, ", ", medium_threshold, ", ", high_threshold), 
       title = "Data vs. Model Fit at Priority Points using Different Weight Sets") +
  geom_text(
    data    = label.df,
    mapping = aes(x = max.val*.5, y = -Inf, label = paste0("SSE: ", round(total.sum.error, 4))),
    hjust   = .5,
    vjust   = -2, 
    size = 6
  ) +
  geom_text(data = label.df[ , .SD[which.min(total.sum.error)]],
            mapping = aes(x = max.val*.5, y = -Inf, label = paste0("SSE: ", round(total.sum.error, 4))),
            hjust   = .5,
            vjust   = -2, 
            size = 6,
            fontface = "bold")+
  theme_bw()+
  theme(axis.title = element_text(size = 18), 
        plot.title = element_text(size = 20), 
        strip.text = element_text(size = 16, margin = margin(.1, 0, .1, 0, "cm"))) +
  guides(color = guide_legend(override.aes = list(alpha=1, size = 4)))


# Creating the Label Data frame for the facet plot

facet.max.val <- max(just.thresholds.combined[index.local.global %like% "global"]$cdf.data.points)
facet.label.df <- distinct(just.thresholds.combined[index.local.global %like% "global"][, c("min.prev", "max.prev") := list(min(cdf.data.points), max(cdf.data.points)), by = .(category, index)], category, index.label, sum.squared.error.category, index.label_plus_index, min.prev, max.prev)
facet.label.df$maxval <- rep(facet.max.val, times = nrow(facet.label.df))


facet.label.df <- merge(all.combos, facet.label.df, by = "index.label_plus_index")

facet.label.df <- label_fit_type(facet.label.df)
facet.label.df <- label_category(facet.label.df)

facet.label.df[, weight.set.label := paste0("Weight Set ", index)]
facet.label.df[, strip.label := paste(weight.set.label, final.label, sep = "\n")]



facet.by.category <- ggplot(just.thresholds.combined[index.local.global %like% "global"]) +
  geom_point(aes(x = cdf.data.points, y = cdf.model.points, color = threshold), alpha = .3) +
  facet_wrap(~threshold + strip.label, scales = "free", nrow = length(thresholds))+ # using wrap instead of grid to get free scales
  geom_abline() +
  labs(x = "Prevalence Data at Priority Points", 
       y = "Modeled Prevalence at Priority Points", 
       title = "Data vs. Model at Each Priority Point using Different Weight Sets", 
       color = "Priority Point",
       caption = "Bolded version has lower error") +
  geom_text(
    data    = facet.label.df,
    mapping = aes(x = max.prev-(max.prev-min.prev)/2, y = -Inf, label = paste0("SSE: ", round(sum.squared.error.category, 4))),
    hjust   = .5,
    vjust   = -2,
    size = 6)  + 
  geom_text(data = facet.label.df[ , .SD[which.min(sum.squared.error.category)], by = threshold],
    mapping = aes(x = max.prev-(max.prev-min.prev)/2, y = -Inf, label = paste0("SSE: ", round(sum.squared.error.category, 4))),
    hjust   = .5,
    vjust   = -2, 
    size = 6,
    fontface = "bold")+
  scale_color_manual(values = c("blue3","green4",  "firebrick3")) +
theme_bw()+
  theme(axis.title = element_text(size = 18), 
        plot.title = element_text(size = 20), 
        strip.text.x = element_text(size = 13, margin = margin(.1, 0, .1, 0, "cm")),
        strip.text.y = element_text(size = 13, margin = margin(0, .1, 0, .1, "cm")), 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14)) +
  guides(color = "none")


# Plotting continuous error -----------------------------------------------

all.ests.combined[, xpoint.index := rep(c(1:1000), times = (length(unique(all.ests.combined$index.local.global))*length(unique(data$nid_loc_yr_index))))]
all.ests.subset <- all.ests.combined[xpoint.index %% 5 == 0]



all.ests.subset$xpoints <- all.ests.subset$xpoints - offset
all.ests.subset <- all.ests.subset[index.local.global %like% "global"]

cut.points <- seq(min(all.ests.subset$xpoints), max(all.ests.subset$xpoints), length = 200)
all.ests.subset$bin <- .bincode(all.ests.subset$xpoints, cut.points, right = T)

all.ests.subset$index.label_plus_index <- paste0(all.ests.subset$index, "_", all.ests.subset$index.label)


cut.points <- data.table(bin.points = cut.points, 
                         bin = c(1:200))

all.ests.subset <- merge(all.ests.subset, cut.points, by = "bin", all = T)

all.ests.subset <- all.ests.subset[!is.na(all.ests.subset$squared.error)]

all.ests.subset[, mean.squared.error.by.x := mean(squared.error), by = c("index.local.global", "bin.points")]
all.ests.subset[, median.squared.error.by.x := median(squared.error), by = c("index.local.global", "bin")]


all.ests.subset[, percent.error := abs((cdf.model.points - cdf.data.points)/cdf.data.points)*100]
all.ests.subset[, mean.percent.error.by.x := mean(percent.error), by = c("index.local.global", "bin")]
all.ests.subset[, median.percent.error.by.x := median(percent.error), by = c("index.local.global", "bin")]

plot.df <- all.ests.subset[!duplicated(all.ests.subset, by = c("index.label_plus_index", "bin.points"))]

left <- c(min(plot.df$xpoints), medium_threshold, high_threshold)
right <- c(low_threshold, high_threshold, max(plot.df$xpoints))
top <- c(max(plot.df$mean.squared.error.by.x) * 1.02, max(plot.df$mean.squared.error.by.x) * 1.02, max(plot.df$mean.squared.error.by.x) * 1.02)
bottom <- c(0, 0, 0)
color <- c('orangered4', 'darkorange2', 'goldenrod2')
rectangles <- data.table(x1 = left, x2 = right, y1 = bottom, y2 = top, col = color)

plot.df <- label_fit_type(plot.df)
plot.df <- merge(all.combos, plot.df, by = "index.label_plus_index")
plot.df[, fill.label := paste0("Weight Set ", index.y, ": ", final.label)]

mean.squared.error.over.dist <- ggplot(plot.df[!is.na(index.local.global) & !is.na(bin)]) +
  geom_rect(data = rectangles, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col), alpha = .3, show.legend = F) +
  geom_line(aes(x = bin.points, y = mean.squared.error.by.x, color = fill.label, group = fill.label), size = 1.5) +
  labs(x = "Domain of Data", y = "Mean Squared Error across Input Data Sources", title = "Mean Squared Error Across Domain for Different Weight Sets", color = "Weight Set") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 24)) +
  scale_fill_manual(values = c("darkorange2","goldenrod2",  "orangered4"))



rectangles$y2 <- c(max(plot.df$median.squared.error.by.x) * 1.02, max(plot.df$median.squared.error.by.x) * 1.02, max(plot.df$median.squared.error.by.x) * 1.02)


median.squared.error.over.dist <- ggplot(plot.df[!is.na(index.local.global) & !is.na(bin)]) +
  geom_rect(data = rectangles, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = col), alpha = .3, show.legend = F) +
  geom_line(aes(x = bin.points, y = median.squared.error.by.x, color = fill.label, group = fill.label), size = 1.5) +
  labs(x = "Domain of Data", y = "Median Squared Error across Input Data Sources", title = "Median Squared Error Across Domain for Different Weight Sets", color = "Weight Set") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 24)) +
  scale_fill_manual(values = c("darkorange2","goldenrod2",  "orangered4"))



rectangles$y2 <- c(max(plot.df$mean.percent.error.by.x) * 1.02, max(plot.df$mean.percent.error.by.x) * 1.02, max(plot.df$mean.percent.error.by.x) * 1.02)



mean.percent.error.over.dist <- ggplot(plot.df[!is.na(index.local.global) & !is.na(bin)]) +
  geom_rect(data = rectangles, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2/100, fill = col), alpha = .3, show.legend = F) +
  geom_line(aes(x = bin.points, y = mean.percent.error.by.x/100, color = fill.label, group = fill.label), size = 1.5) +
  scale_y_continuous(labels = percent) +
  labs(x = "Domain of Data", y = "Mean Percent Error across Input Data Sources", title = "Mean Percent Error Across Domain for Different Weight Sets", color = "Weight Set") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 24)) +
  scale_fill_manual(values = c("darkorange2","goldenrod2",  "orangered4"))





rectangles$y2 <- c(max(plot.df$median.percent.error.by.x) * 1.02, max(plot.df$median.percent.error.by.x) * 1.02, max(plot.df$median.percent.error.by.x) * 1.02)



median.percent.error.over.dist <- ggplot(plot.df[!is.na(index.local.global) & !is.na(bin)]) +
  geom_rect(data = rectangles, mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2/100, fill = col), alpha = .3, show.legend = F) +
  geom_line(aes(x = bin.points, y = median.percent.error.by.x/100, color = fill.label, group = fill.label), size = 1.5) +
  scale_y_continuous(labels = percent) +
  labs(x = "Domain of Data", y = "Median Percent Error across Input Data Sources", title = "Median Percent Error Across Domain for Different Weight Sets", color = "Weight Set") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 24)) +
  scale_fill_manual(values = c("darkorange2","goldenrod2",  "orangered4"))




median.percent.error.over.dist.zoomed <- median.percent.error.over.dist + xlim(min(left) - .001, max(right) + .001) + coord_cartesian(ylim=c(0, 25))

mean.percent.error.over.dist.zoomed <- mean.percent.error.over.dist + xlim(min(left) - .001, max(right) + .001) + coord_cartesian(ylim=c(0, 25))

mean.squared.error.over.dist.zoomed <- mean.squared.error.over.dist + xlim(min(left) - .001, max(right) + .001) + coord_cartesian(ylim=c(0, 3e-4))

median.squared.error.over.dist.zoomed <- median.squared.error.over.dist + xlim(min(left) - .001, max(right) + .001)  + coord_cartesian(ylim=c(0, 3e-4))


# Plot the implied distribution of each weight set (using same mean/SD --------
calculate_curve <- function(nid_loc_yr_i, mean_val, sd_val, bottom_cutoff, top_cutoff, weights, offset, use_gbd_edensity = F){
  
  CDF_Data_Params <- list()
  CDF_Data_Params$mn <- mean_val
  CDF_Data_Params$variance <- sd_val ^2
  CDF_Data_Params$XMIN <- bottom_cutoff
  CDF_Data_Params$XMAX <- top_cutoff
  CDF_Data_Params$x <- seq(CDF_Data_Params$XMIN, CDF_Data_Params$XMAX, length = 5000)
  CDF_Data_Params$fx <- 0*CDF_Data_Params$x
  CDF_Data_Params$cdfDATAYout <- sort(CDF_Data_Params$x)
  CDF_Data_Params$nid <- nid_loc_yr_i
  
  if(use_gbd_edensity){
    dlist <- append(distlist,  list(invweibull = invweibullOBJ, betasr = betasrOBJ, glnorm = glnormOBJ)) 
    dlist <- dlist[!duplicated(names(dlist))]
    names(dlist)
    names(weights)
    dlist <- dlist[names(weights)]
    den <- create_pdf(distlist = dlist, weights = weights, CDF_Data_Params = CDF_Data_Params)  
    rm(dlist) # avoid confusion
  } else{
    den <- create_pdf(distlist = distlist, weights = weights, CDF_Data_Params = CDF_Data_Params)  
  }
  
  
  CDF_Data_Params <- integrate_pdf(den, CDF_Data_Params = CDF_Data_Params, scale = T)
  
  storing <- copy(CDF_Data_Params)
  
  new <- create_pdf_points(den, CDF_Data_Params = CDF_Data_Params)
  
  output <- data.table(xpoints = storing$x, pdf.points = new$cdfFITYout, cdf.points = storing$cdfFITYout, nid_loc_yr_i = nid_loc_yr_i)
  
  return(output)
}


global_mean_val <- data[, weighted.mean(data, svy_wt)]
global_sd_val <- sqrt(c(data[, spatstat.geom::weighted.var(data, svy_wt)]))
curve.output <- lapply(all.combos$index, function(loop.index){
  
  if(loop.index == 1){print("Calculating Curves...")}
  
  ld = all.combos[index == loop.index]$launch.date
  op = all.combos[index == loop.index]$option
  
  #reading in the weights
  if(ld == "GBD2020_metab_bmi"){
    weights = fread(paste0("FILEPATH"))[1][, c("location_id", "year_id", "sex_id", "age_group_id") := NULL]
  } else{
    weights = fread(paste0("FILEPATH"))  
  }
  
  weights = as.list(weights)
  
  # note that this passes nid_loc_yr_i == loop.index, which is the weight set index - it gets renamed later
  if(ld == "GBD2020_metab_bmi"){
    ensemble.dist.output <- calculate_curve(nid_loc_yr_i = loop.index, mean_val = global_mean_val, sd_val = global_sd_val, bottom_cutoff = 10, top_cutoff = 70, weights = weights, offset = offset, use_gbd_edensity = T)  
  } else{
    ensemble.dist.output <- calculate_curve(nid_loc_yr_i = loop.index, mean_val = global_mean_val, sd_val = global_sd_val, bottom_cutoff = 10, top_cutoff = 70, weights = weights, offset = offset)
  }
  
  
  return(ensemble.dist.output)
  
}) %>% rbindlist()

all.combos <- label.df[, .(index = index, final.label = paste0("Weight Set ", index, ": ", final.label))][all.combos, on = "index"][, ]


# read in arbitrary weights to get the function below to work (we will delete the edensity after)
weights <- fread(paste0("FILEPATH"))  

# create weighted CDF/PDF of overall data in USHD (this corresponds to the global_mean_val and global_sd_val used above)
data_curve <- data_to_curves(Data = data$data, svy_wt = data$svy_wt, distlist = distlist,  
               weights = as.list(weights), # transpose(weights),  # make some tweaks because I decided to use the same function as in 1_paralleL_helper_functions.R rather than in curve_creation_functions.R, but they expect slightly different format for weights
               nid_loc_yr_i = "all")
data_curve <- data_curve[, names(data_curve) %notlike% "model", with = F]

names(curve.output)[names(curve.output) == 'nid_loc_yr_i'] <- 'index'

curve.plot.df <- merge(curve.output, all.combos, by = "index")
curve.plot.df[, xpoints := xpoints - offset]


curve.compare.plot.pdf <- ggplot() +
  geom_line(data = curve.plot.df, aes(x = xpoints, y = pdf.points, color = final.label), size = 1) +
  geom_line(data = data_curve, aes(x = xpoints, y = pdf.data.points), lty = "dotted")+ # plot the real data
  geom_vline(xintercept =unlist(thresholds))+
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(x = "Domain", y = "Probability Density", color = "Weight Set", title = "Curve Shape Comparison (PDF)", subtitle = sprintf("Using overall mean (%#.2f) and std. dev (%#.2f)", global_mean_val, global_sd_val)) +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 24)) +
  facet_wrap(~final.label)

curve.compare.plot.cdf <- ggplot() +
  geom_line(data = curve.plot.df, aes(x = xpoints, y = cdf.points, color = final.label), size = 1) +
  geom_line(data = data_curve, aes(x = xpoints, y = cdf.data.points), lty = "dotted")+ # plot the real data
  geom_vline(xintercept =unlist(thresholds))+
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(x = "Domain", y = "Cumulative Density", color = "Weight Set", title = "Curve Shape Comparison (CDF)", subtitle = sprintf("Using overall mean (%#.2f) and std. dev (%#.2f)", global_mean_val, global_sd_val)) +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20), 
        plot.title = element_text(size = 24)) +
  facet_wrap(~final.label)

temp_weight_list <- lapply(weight_list[!all.combos$launch.date %like% "GBD2020"], unlist) # don't include the GBD plots here
temp_names <- all.combos[!launch.date %like% "GBD2020", final.label]

density.construction.plots <- plot_edensity(weight_list = temp_weight_list, # a list of weights (at least one set) 
                          distlist = distlist, # object containing the pdf_families
                          plot_components = T,  # logical: if true, plots the component distributions and ens dist
                          scale = T, # logical: rescale each CDF so that it is in the range [0,1]
                          version_titles = temp_names # must be the same length as weight_list
)


# plot the various weights list ----------------------------------------------------
fams <- unique(unlist(sapply(weight_list, names)))
weights <- rbindlist(weight_list, fill = T)
weights[, index := .I]
weights <- merge(weights, all.combos, by = "index")
weights <- melt(weights, id.vars = "final.label", measure.vars = fams, variable.name = "distribution", value.name = "wt")

weight.set.plot <- ggplot(weights, aes(x = distribution, y = final.label, fill = wt), color = "white")+
  geom_tile()+
  geom_text(aes(label = ifelse(!is.na(wt), round(wt, digits = 3), "-")))+
  scale_fill_distiller(palette = "Purples", direction = 1)+
  theme_bw()+
  labs(y = "Weight set",
       title = "Comparing weights in each weight set")

# make plots showing how each weight set changed over the optimization process
dates <- setdiff(all.combos$launch.date, "GBD2020_metab_bmi")
weight.plots <- lapply(dates, plot_weights_score)


pdf(pdf_filepath, width=18, height=10)
weight.set.plot
curve.compare.plot.pdf
curve.compare.plot.cdf
first.overall.plot
facet.by.category
mean.squared.error.over.dist
mean.squared.error.over.dist.zoomed
median.squared.error.over.dist
median.squared.error.over.dist.zoomed
mean.percent.error.over.dist
mean.percent.error.over.dist.zoomed
mean.percent.error.over.dist
mean.percent.error.over.dist.zoomed
density.construction.plots
weight.plots
dev.off()
