####################################################################################################
## Description: Exploration of the BMI direct estimates in BRFSS to inform the 
##                mean BMI models
##                
## Input: Input is specified in settings.csv
## 
##        Direct calculations of mean BMI and prevalence of overweight/obese 
##          (i.e., calculated with sample weights), for fitting the model (aggregated
##          to states)
##
####################################################################################################

# set up ------------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(ggplot2)
library(mgcv)
library(png)
library(lme4)
library(mvtnorm)
library(boot)

###### Source functions
repo <- "FILPEATH"
funcs <- list.files(paste0(repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(repo, "/functions/", func)))
}

##### Set data location and settings locations, and read in settings
settings_path <- "FILEPATH"
settings_file <- "mean_bmi_model_48_v1"
settings_loc <- paste0(settings_path, settings_file, ".csv")
get_settings(settings_loc)

direct_state <- readRDS(direct_estimates_file)
direct_state <- direct_state[level == "state" & !(age %in% 98:99) & sex != 3 & race != 1 & year != -1]

cols <- c("pred_bmi", "underwt", "overwt", "obese")
direct_ests <- lapply(1:n.imp, function(i){ # n.imp is loaded from settings.csv
  temp <- copy(direct_state)
  temp[, (cols) := lapply(cols, function(c) get(paste(c, i, sep = "_")))]
  temp[, .(state, state_name, sex, race, age, year, level, weights, sample_size, pred_bmi, underwt, overwt, obese)]
})

plot_dir <- paste(dirname(direct_estimates_file), "prevalence_exploration", sep = "/")
dir.create(plot_dir)

# delete existing plots
ps <- list.files(path = plot_dir, full.names = T)
sapply(ps, file.remove)
png(paste(plot_dir, "plot%o3d.png", sep = "/"), width = 600, height = 800)
# plot distribution of mean BMI's -----------------------------------------

warning("Only using imputation 1 at the moment")
DT <- direct_ests[[1]]
DT[, c("sex", "state", "race") := lapply(.SD, as.factor), .SDcols = c("sex", "state", "race")]
ggplot(DT, aes(pred_bmi, weight = weights))+
  geom_density()+
  facet_grid(age~sex)+
  labs(x = "mean BMI", title = "Weighted distribution of mean BMI across strata, faceted by sex & age")


ggplot(DT[sample_size >= 30], aes(pred_bmi, weight = weights))+
  geom_density()+
  facet_grid(age~sex)+
  labs(x = "mean BMI", 
       title = "Weighted distribution of mean BMI across strata, faceted by sex & age",
       caption = "restricted to strata with N >= 30")


ggplot(DT[sample_size >= 30], aes(log(pred_bmi), weight = weights))+
  geom_density()+
  facet_grid(age~sex)+
  labs(x = "log(mean BMI)",
       title = "Weighted distribution of log(mean BMI) across strata, faceted by sex & age",
       caption = "restricted to strata with N >= 30")


ggplot(DT, aes(pred_bmi, fill = race, weight = weights))+
  geom_density(alpha = 0.5)+
  facet_grid(age~sex)+
  labs(x = "mean BMI",
       title = "Weighted distribution of mean BMI across strata, faceted by sex & age, colored by race")


ggplot(DT[sample_size >= 30], aes(pred_bmi, fill = race, weight = weights))+
  geom_density(alpha = 0.5)+
  facet_grid(age~sex)+
  labs(x = "mean BMI",
       title = "Weighted distribution of mean BMI across strata, faceted by sex & age, colored by race",
       caption = "restricted to strata with N >= 30")


ggplot(DT, aes(pred_bmi, weight = weights))+
  geom_density(alpha = 0.5)+
  facet_grid(race~sex)+
  labs(x = "mean BMI",
       title = "Weighted distribution of mean BMI across strata, faceted by sex & race")


ggplot(DT[sample_size >= 30], aes(pred_bmi, weight = weights))+
  geom_density(alpha = 0.5)+
  facet_grid(race~sex)+
  labs(x = "mean BMI",
       title = "Weighted distribution of mean BMI across strata, faceted by sex & race",
       caption = "restricted to strata with N >= 30")

# plot relationship b/w mean and prevalence ------------------------------

DT_long <- melt(DT, measure.vars = c("underwt", "overwt", "obese"), variable.name = "prev_type", value.name = "Prevalance")

ggplot(DT_long[sample_size >= 30], aes(pred_bmi, Prevalance, weight = weights, color = prev_type)) + theme_bw() +
  geom_point(aes(alpha = weights), shape = 1)+
  geom_smooth()+
  labs(caption = "restricted to strata with N >= 30", x = "Mean BMI", title = "Mean BMI vs Prevalences of underweight, overweight, and obese")

ggplot(DT_long[sample_size >= 30], aes(log(pred_bmi), Prevalance, weight = weights, color = prev_type)) + theme_bw() +
  geom_point(aes(alpha = weights), shape = 1)+
  geom_smooth()+
  labs(caption = "restricted to strata with N >= 30", x = "Log Mean BMI", title = "Log Mean BMI vs Prevalences of underweight, overweight, and obese")

ggplot(DT_long[sample_size >= 30], aes(pred_bmi, logit(Prevalance), weight = weights, color = prev_type)) + theme_bw() +
  geom_point(aes(alpha = weights), shape = 1)+
  geom_smooth()+
  labs(caption = "restricted to strata with N >= 30", x = "Mean BMI", title = "Mean BMI vs Logit Prevalences of underweight, overweight, and obese")

ggplot(DT_long[sample_size >= 30], aes(log(pred_bmi), logit(Prevalance), weight = weights, color = prev_type)) + theme_bw() +
  geom_point(aes(alpha = weights), shape = 1)+
  geom_smooth()+
  labs(caption = "restricted to strata with N >= 30", x = "Log Mean BMI", title = "Log Mean BMI vs Logit Prevalences of underweight, overweight, and obese")

ggplot(DT_long[sample_size >= 30], aes(pred_bmi, Prevalance, weight = weights, color = prev_type)) + theme_bw() +
  geom_point(aes(alpha = weights), shape = 1)+
  facet_wrap(~race)+
  geom_smooth()+
  ylim(-0.1,1.01)+
  labs(caption = "restricted to strata with N >= 30", x = "Mean BMI", title = "Mean BMI vs Prevalences of underweight, overweight, and obese")

dev.off()