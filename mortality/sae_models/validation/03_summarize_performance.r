####################################################################################################
## Description: Merge predictions from validation models with the gold standard and calculate error.
##              Then calculate and plot summary validation metrics.
##
## Passed Args: dir [character] -- the directory where "lt_est_validation.rds" and
##                "mx_est_validation.rds" can be found (i.e., files created by
##                "compile_val_preds.r" or equivalent)
##
## Outputs:     lt_est_validation_prepped.rds -- validation metrics for life expectancy at birth by
##                validation type, sample size, and sex.
##              mx_est_validation_prepped.rds  -- validation metrics for all age and age-specific
##                mortality rates by validation type, sample size, sex, and age.
##              validation_results.pdf -- plots of the validation results for life expectancy and
##                for all age and age-specific mortality rates.
####################################################################################################

library(data.table)
library(ggplot2)
library(R.utils)
library(scales)
library(glue)
stopifnot(grepl("mortality/sae_models$", getwd()))
R.utils::sourceDirectory('functions', modifiedOnly = FALSE)

# set theme for all plots
theme_set(theme_bw(base_size = 12))

if(interactive()) {
  dir <- "FILEPATH"
  through_2019_only <- F
} else {
  args <- commandArgs(trailingOnly = TRUE)
  dir <- args[1]
  through_2019_only <- as.logical(args[2])
}

message(dir)

get_settings(dir)

val_types <- "sampled_down_val_set_only"
vv <- "sampled_down_val_set_only"

lu_root <- paste0("FILEPATH",LU_folder,"FILEPATH")
lu_model_dir <- file.path(lu_root, "", gsub("FILEPATH", "", dir))

## Load and merge predictions with the gold standard -----------------------------------------------
gs_mx <- readRDS(gs_file)
gs_ex <- readRDS(file.path(dirname(gs_file), "validation_set_gs_ex.rds"))

gs_ex <- gs_ex[, list(level = "mcnty", area = mcnty, year, sex, age, race, edu, gs_ex = ex_mean)]
gs_mx <- gs_mx[, list(level = "mcnty", area = mcnty, year, sex, age, race, edu, gs_mx = mx_mean)]


if (!file.exists(file.path(lu_model_dir, "validation_visualizations"))) {
  dir.create(file.path(lu_model_dir, "validation_visualizations"), recursive = T)
}

# population
pop <- readRDS(pop_file)
setnames(pop, old = c("mcnty"), new = c("area"))
pop[, state := NULL]
if ("edu_label" %in% names(pop)) {
  pop[, edu_label := NULL]
}

if (!by_race) {
  if(all_pop_id %in% unique(pop$race) & length(unique(pop$race)) > 1) {
    stop(paste0("Pop already has race ",all_pop_id,", this could lead to duplication of data."))
  }
  pop[, race := all_pop_id]
  pop <- pop[, list(pop = sum(pop)), by = "area,year,sex,age,race,edu"]
}

if (!by_edu) {
  if(all_pop_id %in% unique(pop$edu) & length(unique(pop$edu)) > 1) {
    stop(paste0("Pop already has edu ",all_pop_id,", this could lead to duplication of data."))
  }
  pop[, edu := all_pop_id]
  pop <- pop[, list(pop = sum(pop)), by = "area,year,sex,age,race,edu"]
}

if (by_edu) {
  pop <- pop[age >= 25]
}

pop_99 <- copy(pop)
pop_99 <- pop_99[, list(pop = sum(pop)), by = "area,year,sex,race,edu"]
pop_99[, age := 99]
pop_98 <- copy(pop_99)
pop_98[, age := 98]
pop <- rbindlist(
  list(pop, pop_99, pop_98),
  use.names = T
)
rm(pop_99, pop_98); gc()

# life expectancy
ex <- readRDS(file.path(dir, "lt_est_validation.rds"))


ex <- merge(ex, gs_ex, by = c("level", "area", "year", "sex", "age", "race", "edu"))

# for ex_age, we actually want all ages total pop
ex <- merge(ex, pop[age == 98, ], by = c("area", "year", "sex", "race", "edu"), all.x = T, suffixes = c("", "_pop"))
ex[, age_pop := NULL]
stopifnot(!any(is.na(ex)))

# mortality rates
mx <- readRDS(file.path(dir, "mx_est_validation.rds"))

# Note inner merge: this filters
mx <- merge(mx, gs_mx, by = c("level", "area", "year", "sex", "age", "race", "edu"))
mx <- merge(mx, pop, by = c("area", "year", "sex", "age", "race", "edu"), all.x = T)
stopifnot(!any(is.na(mx)))


if(through_2019_only) {
  mx <- mx[year <= 2019]
  ex <- ex[year <= 2019]
}

# remove all-age mortality estimates. These are not very useful to use because the age pattern of the input population
# varies by size. As size increases, the population proportions become more similar to those of the GS. Thus, the all-age estimates
# generally improve with size just because the population proportions are more similar.

mx <- mx[age != 98]

# save out merged and prepared files
message("Saving out formatted files...")
saveRDS(mx, file.path(dir, paste0("mx_est_validation_prepped",if(through_2019_only) "_pre_2020", ".rds")))
saveRDS(ex, file.path(dir, paste0("ex_est_validation_prepped",if(through_2019_only) "_pre_2020", ".rds")))

## plot estimates, lower bound, upper bound, and gold standard ------------------------------------
# plots each iter and size on same graph. ex_age or mx is on y axis. year is on x axis.
# runs on the counties in the validation set. Skips if there is no data for a county, sex, race combo
# each page is a county, race, sex, area.

# before saving anything, i want to try dev.off() to make sure that there are no issues with saving.
tryCatch({
  dev.off()
}, warning = function(warning_condition) {
  print(paste("Warning:", conditionMessage(warning_condition)))
}, error = function(error_condition) {
  print(paste("Error:", conditionMessage(error_condition)))
}, finally = {
  # print("dev.off() attempted.")
})


# start PDF
x <- file.path(lu_model_dir, paste0("FILEPATH.pdf"))
message(glue("working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)


plot_le <- function(plot_data, plotting_age, by_edu) {
  plot_data <- data.table::copy(plot_data[age == plotting_age,])
  p <- ggplot(data = plot_data,
              aes(x = year, y = ex_mean + plotting_age)) +
    geom_line() +
    geom_ribbon(aes(ymin = ex_lb + plotting_age, ymax = ex_ub + plotting_age), alpha = 0.3) +
    facet_grid("iter ~ size") +
    labs(title = glue("Average age at death for age {plotting_age}: area {area}, sex {sex}, race {race}, and edu {edu}"),
         subtitle = glue("{vv}"),
         caption = glue("From {basename(dir)}. Dashed line is gold standard.")) +
    geom_line(aes(y = gs_ex + plotting_age), linetype = "dashed") + # add gold standard
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0))
  return(p)
}


for (area in unique(ex$area)) {
  for (sex in c(1, 2)) {
    for (race in unique(ex$race)) {
      for (edu in unique(ex$edu)) {
        message(glue("race {race}, edu {edu}, area {area}, sex {sex}"))
        stopifnot("area" %in% names(ex))

        plot_data <-
          data.table::copy(ex[val_type == "sampled_down_val_set_only" &
                                area == get("area", .GlobalEnv) &
                                sex == get("sex", .GlobalEnv) &
                                race == get("race", .GlobalEnv) &
                                edu == get("edu", .GlobalEnv),])

        # nrow can be zero if a specific race/edu isn't in the validation set
        if (nrow(plot_data) > 0) {
          if (by_edu) {
            # first do age 25
            p <- plot_le(plot_data,
                         plotting_age = 25,
                         by_edu = by_edu)
            plot(p)
            rm(p)
            # then do 85
            p <-
              plot_le(plot_data, plotting_age = 85, by_edu = by_edu)
            plot(p)
            rm(p)
          } else {
            # use age == 0
            p <- plot_le(plot_data, plotting_age = 0, by_edu)
            plot(p)
            rm(p)
          } # end if / else by_edu
        } # end if nrow > 0
      } # end edu loop
    } # end race loop
  } # end sex loop
} # end area loop
dev.off()
invisible(gc())

x = file.path(lu_model_dir, paste0("FILEPATH.pdf"))
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

for (area in unique(mx$area)) {
  for (sex in c(1, 2)) {
    for (race in unique(mx$race)) {
      for (edu in unique(mx$edu)) {
        message(paste0("race ", race, ", edu", edu, ", area ", area, ", sex ", sex))
        stopifnot("area" %in% names(mx))
        plot_data <-
          mx[val_type == "sampled_down_val_set_only" &
               age == 99 &
               area == get("area", .GlobalEnv) &
               sex == get("sex", .GlobalEnv) &
               race == get("race", .GlobalEnv) &
               edu == get("edu", .GlobalEnv), ]
        if (nrow(plot_data) == 0) {
          next
        }

        p <- ggplot(data = plot_data,
                    aes(x = year, y = mx_mean)) +
          geom_line() +
          geom_ribbon(aes(ymin = mx_lb, ymax = mx_ub), alpha = 0.3) +
          facet_grid("iter ~ size") +
          labs(
            title = paste0(
              "Age Stardandized Mortality Rate for area ",
              area,
              " and sex ",
              sex,
              ", race ",
              race,
              ", edu ",
              edu
            ),
            caption = paste0("From ", basename(dir), ". Dashed line is gold standard.")
          ) +
          geom_line(aes(y = gs_mx), linetype = "dashed") + # add gold standard
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0)
          )
        plot(p)
        rm(p)
      }
    }
  }

}

dev.off()

## Calculate error and validation metrics for life expectancy at birth -----------------------------
if (by_edu) {
  start_age <- 25
} else {
  start_age <- 0
}

ex_age <- ex[age == start_age, list(iter, val_type, size, sex, race, edu, year, pop, ex_mean, ex_lb, ex_ub, gs_ex, error = ex_mean - gs_ex, coverage = data.table::between(gs_ex, ex_lb, ex_ub))] # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error

quintiles <- quantile(ex_age$pop, probs = c(0, .2, .4, .6, .8, 1))

ex_age[pop >= quintiles[1] & pop < quintiles[2], quintile := 1]
ex_age[pop >= quintiles[2] & pop < quintiles[3], quintile := 2]
ex_age[pop >= quintiles[3] & pop < quintiles[4], quintile := 3]
ex_age[pop >= quintiles[4] & pop < quintiles[5], quintile := 4]
ex_age[pop >= quintiles[5] & pop <= quintiles[6], quintile := 5]

ex_age[, .N, by = quintile]

stopifnot(!any(is.na(ex_age$quintile)))

## scatters of life expectancy at birth estimates vs gold standard ------------------------------------

# start PDF
x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}"))
pdf(x, width = 14, height = 8)
rm(x)

# scatter of life expectancy at age standard vs estimates FIRST ITERATION
ggplot(data = ex_age[iter == 1, ], aes(x = gs_ex + start_age, y = ex_mean + start_age, color = coverage)) +
  facet_grid("val_type ~ size") +
  geom_point(size = 0.3, alpha = 0.15) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = "Life Expectancy at Birth from gold standard and estimates, first iteration",
       caption = "Each point is a race, edu, sex, year, and county") +
  theme(plot.caption = element_text(hjust = 0))

# scatter of life expectancy at birth gold standard vs estimates
p <- ggplot(data = ex_age, aes(x = gs_ex + start_age, y = ex_mean + start_age, color = coverage)) +
  facet_grid("val_type ~ size") +
  geom_point(size = 0.15, alpha = 0.075) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = "Life Expectancy at Birth from gold standard and estimates, all iterations",
       caption = "Each point is a race, edu, sex, year, county, and iteration") +
  theme(plot.caption = element_text(hjust = 0))

# save as a png as well
ggsave(file.path(lu_model_dir, "FILEPATH.png"), plot = p)
plot(p)  # show on PDF
rm(p)
dev.off()  # end main plots

# new PDF for pop-sized plots
x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

# scatter of life expectancy at birth gold standard vs estimates FIRST ITERATION WITH POP
for (i in c(1:5)) {
  p <- ggplot(data = ex_age[iter == 1 & quintile == i, ],
              aes(x = gs_ex + start_age, y = ex_mean + start_age, size = pop, color = coverage)) +
    facet_grid("val_type ~ size") +
    geom_point(alpha = 0.25) +
    scale_size(range = c(0.05, 3)) +
    geom_abline(intercept = 0, slope = 1) +
    coord_equal() +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    labs(x = "gold standard", y = "estimates",
         title = glue("Average age at death for age {start_age} from gold standard and estimates, first iteration, with population quintile {i}"),
         caption = "Each point is a race, edu, sex, year, and county.\nPoints are sized by population, and scale is set for each plot inidividually",
         size = "Population") +
    theme(legend.position="bottom",
          plot.caption = element_text(hjust = 0))
  plot(p)
  rm(p)
}
dev.off()  # end pop pdf

# new PDF for sex plots
x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

# scatter of life expectancy at birth gold standard vs estimates males, 1st iter
ggplot(data = ex_age[sex == 1 & iter == 1, ], aes(x = gs_ex + start_age, y = ex_mean + start_age, color = coverage)) +
  facet_grid("val_type ~ size") +
  geom_point(size = 0.3, alpha = 0.15) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = glue("Average age at death for age {start_age} from gold standard and estimates, Males, first iteration"),
       caption = "Each point is a race, edu, year, and county") +
  theme(plot.caption = element_text(hjust = 0))

# scatter of life expectancy at birth gold standard vs estimates females, 1st iter
ggplot(data = ex_age[sex == 2 & iter == 1, ], aes(x = gs_ex + start_age, y = ex_mean + start_age, color = coverage)) +
  facet_grid("val_type ~ size") +
  geom_point(size = 0.3, alpha = 0.15) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = glue("Average age at death for age {start_age} from gold standard and estimates, Males, first iteration"),
       caption = "Each point is a race, year, and county") +
  theme(plot.caption = element_text(hjust = 0))

dev.off()  # end sex lt plots

# begin lt year plots
x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

# scatter of life expectancy at birth gold standard vs estimates by year
for (year in sort(unique(ex_age$year))) {
  y <- ggplot(data = ex_age[year == get("year", .GlobalEnv), ], aes(x = gs_ex + start_age, y = ex_mean + start_age, color = coverage)) +
    facet_grid("val_type ~ size") +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_abline(intercept = 0, slope = 1) +
    coord_equal() +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    labs(x = "gold standard", y = "estimates",
         title = glue("Average age at death for age {start_age} from gold standard and estimates, year {year}"),
         caption = "Each point is a race, sex, iteration, and county") +
    theme(plot.caption = element_text(hjust = 0))
  plot(y)
}
rm(y)

dev.off()  # end year lt plots

# scatter of life expectancy at birth gold standard vs estimates by race
if ((length(unique(ex_age$race)) > 1) || (length(unique(ex_age$edu)) > 1)) {
  x = file.path(lu_model_dir, "FILEPATH.pdf")
  message(glue("Working on {x}..."))
  pdf(x, width = 14, height = 8)
  rm(x)
  for (race in sort(unique(ex_age$race))) {
    for (edu in sort(unique(ex_age$edu))) {
    r <-
      ggplot(data = ex_age[race == get("race", .GlobalEnv) &
                         edu == get("edu", .GlobalEnv) &
                         iter == 1,], aes(x = gs_ex + start_age, y = ex_mean + start_age, color = coverage)) +
      facet_grid("val_type ~ size") +
      geom_point(size = 0.15, alpha = 0.2) +
      geom_abline(intercept = 0, slope = 1) +
      coord_equal() +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
      labs(x = "gold standard", y = "estimates",
           title = glue("Average age at death for age {start_age} from gold standard and estimates, race/ethnicity {race}, education {edu}"),
           caption = "Each point is a sex, year, iteration, and county") +
      theme(plot.caption = element_text(hjust = 0))
    plot(r)
  } # end edu loop
  } # end race loop
  rm(r)
  dev.off()  # end race plots
} # end if statement

# done with ex_age scatters

## Calculate error and validation metrics for age specific mortality rates -------------------------
mx <- mx[, list(iter, val_type, size, sex, age, race, edu, year, pop, mx_mean, mx_lb, mx_ub, gs_mx, error = (mx_mean - gs_mx), coverage = data.table::between(gs_mx, mx_lb, mx_ub))] # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error

quintiles <- quantile(mx[age == 99, pop], probs = c(0, .2, .4, .6, .8, 1))

mx[pop >= quintiles[1] & pop < quintiles[2], quintile := 1]
mx[pop >= quintiles[2] & pop < quintiles[3], quintile := 2]
mx[pop >= quintiles[3] & pop < quintiles[4], quintile := 3]
mx[pop >= quintiles[4] & pop < quintiles[5], quintile := 4]
mx[pop >= quintiles[5] & pop <= quintiles[6], quintile := 5]

stopifnot(!any(is.na(mx[age == 99, quintile])))

## scatters of mortality rate vs gold standard ------------------------------------
x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)
# scatter of mx gold standard vs mx estimates FIRST ITERATION
ggplot(data = mx[iter == 1 & age == 99, ], aes(x = gs_mx, y = mx_mean, color = coverage)) +
  geom_point(size = 0.3, alpha = 0.15) +
  facet_grid("val_type ~ size") +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = "Mortality rates from gold standard and estimates, first iteration",
       caption = "Each point is a race, edu, sex, year, and county") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))


# scatter of mx gold standard vs mx estimates all iterations
p <- ggplot(data = mx[age == 99, ], aes(x = gs_mx, y = mx_mean, color = coverage)) +
  geom_point(size = 0.15, alpha = 0.075) +
  facet_grid("val_type ~ size") +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = "Mortality rates from gold standard and estimates",
       caption = "Each point is a race, sex, year, county, and iteration") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))

# scatter as a png as well
ggsave(file.path(lu_model_dir, "FILEPATH.png"), plot = p)

plot(p)
rm(p)

dev.off()  # end mx main plots


# new PDF for pop-sized plots
x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

# scatter of mx gold standard vs mx estimates FIRST ITERATION WITH POP
for (i in c(1:5)) {
  print(i)
  p <- ggplot(data = mx[age == 99 & iter == 1 & quintile == i, ],
              aes(x = gs_mx, y = mx_mean, size = pop, color = coverage)) +
    facet_grid("val_type ~ size") +
    geom_point(alpha = 0.3) +
    scale_size(range = c(0.05, 3)) +
    geom_abline(intercept = 0, slope = 1) +
    coord_equal() +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    labs(x = "gold standard", y = "estimates",
         title = paste0("Mortality rates from gold standard and estimates, first iteration, population quintile ", i),
         caption = "Each point is a race, sex, year, and county.\nPoints are sized by population, and scale is set for each plot inidividually",
         size = "Population") +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0))
  plot(p)
  rm(p)
}
dev.off()  # end pop pdf

x = file.path(lu_model_dir, "FILEPATH.pdf")
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

# scatter of mx gold standard vs estimates males, 1st iter
ggplot(data = mx[age == 99 & sex == 1 & iter == 1, ], aes(x = gs_mx, y = mx_mean, color = coverage)) +
  facet_grid("val_type ~ size") +
  geom_point(size = 0.3, alpha = 0.15) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = "Mortality rates from gold standard and estimates, Males, first iteration",
       caption = "Each point is a race, year, and county") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))

# scatter of mx gold standard vs estimates females, 1st iter
ggplot(data = mx[age == 99 & sex == 2 & iter == 1, ], aes(x = gs_mx, y = mx_mean, color = coverage)) +
  facet_grid("val_type ~ size") +
  geom_point(size = 0.3, alpha = 0.15) +
  geom_abline(intercept = 0, slope = 1) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  labs(x = "gold standard", y = "estimates",
       title = "Mortality rates from gold standard and estimates, Females, first iteration",
       caption = "Each point is a race, year, and county") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))

dev.off()  # end mx sex plots

x = file.path(lu_model_dir, "FILEPATH.pdf")
pdf(x, width = 14, height = 8)
message(glue("Working on {x}..."))
rm(x)

# scatter of mx gold standard vs estimates by year
for (year in sort(unique(mx$year))) {
  y <- ggplot(data = mx[age == 99 & year == get("year", .GlobalEnv), ], aes(x = gs_mx, y = mx_mean, color = coverage)) +
    facet_grid("val_type ~ size") +
    geom_point(size = 0.5, alpha = 0.2) +
    geom_abline(intercept = 0, slope = 1) +
    coord_equal() +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    labs(x = "gold standard", y = "estimates",
         title = paste0("Mortality rates from gold standard and estimates, ", year),
         caption = "Each point is a race, sex, iteration, and county") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0))
  plot(y)
}
rm(y)
dev.off()  # end mx year plots

# scatter of mx gold standard vs estimates by race
if (length(unique(mx$race)) > 1) {
  x = file.path(lu_model_dir, "FILEPATH.pdf")
  pdf(x, width = 14, height = 8)
  message(glue("Working on {x}..."))
  rm(x)
  for (race in sort(unique(mx$race))) {
    r <- ggplot(data = mx[age == 99 & race == get("race", .GlobalEnv), ], aes(x = gs_mx, y = mx_mean, color = coverage)) +
      facet_grid("val_type ~ size") +
      geom_point(size = 0.15, alpha = 0.1) +
      geom_abline(intercept = 0, slope = 1) +
      coord_equal() +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
      labs(x = "gold standard", y = "estimates",
           title = paste0("Mortality rates from gold standard and estimates, race/ethnicity ", race),
           caption = "Each point is a sex, year, iteration, and county") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0))
    plot(r)
  }
  rm(r)
  dev.off()  # end mx race plots
}

# done with scatters

## calculate errors, coverage, and correlation ----------------------------------------------------

ex_age_all <- data.table::copy(ex_age) # copy early, use later
ex_age <- ex_age[, list(rmse = sqrt(mean(error^2)),
                mean_error = mean(error),
                mean_abs_error = mean(abs(error)),
                mean_rel_error = mean(error / gs_ex),
                mean_abs_rel_error = mean(abs(error) / gs_ex),
                corr = cor(ex_mean, gs_ex),
                coverage = mean(data.table::between(gs_ex, ex_lb, ex_ub))),  # explicitly using data.table is necessary because if it isn't, and if dplyr is loaded, this between will cause an error
         keyby = "val_type,size,race,edu"]

if (by_race) {

  # across all race/ethnicities
  ex_age_all <- ex_age_all[, list(rmse = sqrt(mean(error^2)),
                          mean_error = mean(error),
                          mean_abs_error = mean(abs(error)),
                          mean_rel_error = mean(error / gs_ex),
                          mean_abs_rel_error = mean(abs(error) / gs_ex),
                          corr = cor(ex_mean, gs_ex),
                          coverage = mean(data.table::between(gs_ex, ex_lb, ex_ub))),  # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error
                   keyby = "val_type,size,edu"]  # note race is not in the by
  ex_age_all[, race := "across all race/ethnicities"]

  ex_age <- rbindlist(
    list(
      ex_age,
      ex_age_all
    ),
    use.names = TRUE
  )
}

if (by_edu) {
  # across all educations
  ex_age_all <- ex_age_all[, list(rmse = sqrt(mean(error^2)),
                          mean_error = mean(error),
                          mean_abs_error = mean(abs(error)),
                          mean_rel_error = mean(error / gs_ex),
                          mean_abs_rel_error = mean(abs(error) / gs_ex),
                          corr = cor(ex_mean, gs_ex),
                          coverage = mean(data.table::between(gs_ex, ex_lb, ex_ub))),  # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error
                   keyby = "val_type,size,race"]  # note edu is not in the by
  ex_age_all[, edu := "across all education"]

  ex_age <- rbindlist(
    list(
      ex_age,
      ex_age_all
    ),
    use.names = TRUE
  )
}



mx_all <- data.table::copy(mx)
mx <- mx[, list(rmse = 100000 * sqrt(mean(error^2)),
                mean_error = 100000 * mean(error),
                mean_abs_error = 100000 * mean(abs(error)),
                mean_rel_error = mean(error / gs_mx),
                mean_abs_rel_error = mean(abs(error) / gs_mx),
                corr = cor(mx_mean, gs_mx),
                coverage = mean(data.table::between(gs_mx, mx_lb, mx_ub))),  # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error
         keyby = "val_type,size,age,race,edu"]


if (by_race) {
  mx_all <- mx_all[, list(rmse = 100000 * sqrt(mean(error^2)),
                          mean_error = 100000 * mean(error),
                          mean_abs_error = 100000 * mean(abs(error)),
                          mean_rel_error = mean(error / gs_mx),
                          mean_abs_rel_error = mean(abs(error) / gs_mx),
                          corr = cor(mx_mean, gs_mx),
                          coverage = mean(data.table::between(gs_mx, mx_lb, mx_ub))),  # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error
                   keyby = "val_type,size,age,edu"]  # note that race is not in the by
  mx_all[, race := "across all race/ethnicities"]

  mx <- rbindlist(
    list(
      mx,
      mx_all
    ),
    use.names = TRUE
  )
}

if (by_edu) {
  mx_all <- mx_all[, list(rmse = 100000 * sqrt(mean(error^2)),
                          mean_error = 100000 * mean(error),
                          mean_abs_error = 100000 * mean(abs(error)),
                          mean_rel_error = mean(error / gs_mx),
                          mean_abs_rel_error = mean(abs(error) / gs_mx),
                          corr = cor(mx_mean, gs_mx),
                          coverage = mean(data.table::between(gs_mx, mx_lb, mx_ub))),  # explicitly using data.table is necessary because if it isn't, if dplyr is loaded, this between will cause an error
                   keyby = "val_type,size,age,race"]  # note that race is not in the by
  mx_all[, edu := "across all education"]

  mx <- rbindlist(
    list(
      mx,
      mx_all
    ),
    use.names = TRUE
  )

}

rm(mx_all, ex_age_all); gc()

## Save and plot results ---------------------------------------------------------------------------
# save results
message("Saving results...")
write.csv(ex_age, file = paste0(dir, "/ex_validation_results",if(through_2019_only) "_pre_2020",".csv"), row.names = F)


if(nrow(mx[is.infinite(mean_rel_error)]) == 0) {
  write.csv(mx, file = paste0(dir, "/mx_validation_results",if(through_2019_only) "_pre_2020",".csv"), row.names = F)
}


# set metric names
e0_metrics = c(mean_error = "Mean Error [(pred - true)]", mean_abs_error = "Mean Absolute Error [abs(pred - true)]",
            mean_rel_error = "Mean Relative Error [(pred - true)/true]", mean_abs_rel_error = "Mean Absolute Relative Error [abs(pred - true)/true]",
            rmse = "RMSE [sqrt(mean(pred - true))]", corr = "Correlation", coverage = "Coverage")

mx_metrics = c(mean_error = "Mean Error [(pred - true) * 100,000]", mean_abs_error = "Mean Absolute Error [abs(pred - true) * 100,000] ",
               mean_rel_error = "Mean Relative Error [(pred - true)/true]", mean_abs_rel_error = "Mean Absolute Relative Error [abs(pred - true)/true]",
               rmse = "RMSE [sqrt(mean(pred - true)) * 100,000]", corr = "Correlation", coverage = "Coverage")


if (by_race & F) {
  message("\nAdding all-race validation...")
  # read in the direct all race validation results (made by this script)
  ex_age_all <- fread(file = "FILEPATH/e0_validation_results.csv")
  ex_age_all[, race := NULL]
  ex_age_all[, race := "all-race with covariates validation"]
  mx_all <- fread(file = "FILEPATH/mx_validation_results.csv")
  mx_all[, race := NULL]
  mx_all[, race := "all-race with covariates validation"]

  ex_age <- rbindlist(
    list(ex_age, ex_age_all),
    use.names = T
  )

  mx <- rbindlist(
    list(mx, mx_all),
    use.names = T
  )

  rm(ex_age_all, mx_all); gc()
}






# plots of age-specific mortality rates performance -----------------------

# reshape results data
ex_age <- melt(ex_age, id.vars = c("val_type", "size", "race", "edu"))
ex_age[, variable := factor(variable, levels = names(e0_metrics), labels = e0_metrics)]
ex_age[, val_type := gsub("_", " ", val_type)]

mx <- melt(mx, id.vars = c("val_type", "size", "age", "race", "edu"))
# stop("error here")
mx[, variable := factor(variable, levels = names(mx_metrics), labels = mx_metrics)]
mx[, val_type := gsub("_", " ", val_type)]


x = file.path(lu_model_dir, file.path(glue("FILEPATH.pdf")))
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)

for (current_race in sort(unique(ex_age$race))) {
  for (current_edu in sort(unique(ex_age$edu))) {
    # plots of life expectancy predictive performance
    r <- ggplot(
      ex_age[race == current_race & edu == current_edu],
      aes(
        x = factor(size),
        y = value,
        colour = val_type,
        linetype = val_type,
        shape = val_type,
        group = val_type
      )
    ) +
      facet_wrap(~ variable, scales = "free") +
      geom_point(size = 2) + geom_line() +
      scale_colour_discrete(name = "Validation Type") +
      scale_linetype_discrete(name = "Validation Type") +
      scale_shape_discrete(name = "Validation Type") +
      labs(
        x = "Sample Size",
        y = "",
        title = glue(
          "Validation Results: Life Expectancy at age {start_age} (race {current_race} and edu {current_edu})"
        )
      ) +
      theme(
        legend.position = c(0.8, 0.2),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    plot(r)
  } # end edu loop
} # end race loop

if (!by_edu) {
  ages <- c("All ages, crude" = 98, "All ages, standardized" = 99, "< 1 yr" = 0, "1-4 yrs" = 1,
            "5-9 yrs" = 5, "10-14 yrs" = 10, "15-19 yrs" = 15, "20-24 yrs" = 20, "25-29 yrs" = 25,
            "30-34 yrs" = 30, "35-39 yrs" = 35, "40-44 yrs" = 40, "45-49 yrs" = 45, "50-54 yrs" = 50,
            "55-59 yrs" = 55, "60-64 yrs" = 60, "65-69 yrs" = 65, "70-74 yrs" = 70, "75-79 yrs" = 75,
            "80-84 yrs" = 80, "85+ yrs" = 85)
} else {
  ages <- c("All ages, crude" = 98, "All ages, standardized" = 99, "25-29 yrs" = 25,
            "30-34 yrs" = 30, "35-39 yrs" = 35, "40-44 yrs" = 40, "45-49 yrs" = 45, "50-54 yrs" = 50,
            "55-59 yrs" = 55, "60-64 yrs" = 60, "65-69 yrs" = 65, "70-74 yrs" = 70, "75-79 yrs" = 75,
            "80-84 yrs" = 80, "85+ yrs" = 85)
}


for (this_age in names(ages)) {
  # message(this_age)
  for (current_race in sort(unique(mx$race))) {
    for (current_edu in sort(unique(mx$edu))) {
      p <-
        ggplot(
          mx[race == current_race &
               age == ages[this_age] & edu == current_edu,],
          aes(
            x = factor(size),
            y = value,
            colour = val_type,
            linetype = val_type,
            group = val_type
          )
        ) +
        facet_wrap( ~ variable, scales = "free") +
        geom_point(size = 2) + geom_line() +
        scale_colour_discrete(name = "Validation Type") +
        scale_linetype_discrete(name = "Validation Type") +
        scale_shape_discrete(name = "Validation Type") +
        labs(
          x = "Sample Size",
          y = "",
          title = paste0(
            "Validation Results: Mortality Rate, ",
            this_age,
            " (race ",
            current_race,
            ")"
          ),
          caption = "RMSE, Mean Error, and Mean Absolute Error metrics are multiplied by 100,000. No other metrics are scaled."
        ) +
        theme(
          legend.position = c(0.8, 0.2),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0)
        )
      plot(p)
    } # end edu loop
  } # end race loop
} # end age loop
dev.off()


x = file.path(lu_model_dir, paste0("FILEPATH.pdf"))
message(glue("Working on {x}..."))
pdf(x, width = 14, height = 8)
rm(x)


# plots of life expectancy predictive performance -------------------------

r <-
  ggplot(ex_age,
         aes(
           x = factor(size),
           y = value,
           colour = factor(interaction(race, edu)),
           group = factor(interaction(race, edu)),
           shape = factor(interaction(race, edu))
         )) +
  facet_wrap( ~ variable, scales = "free") +
  geom_point(alpha = 0.5, size = 2) + geom_line(alpha = 0.5) +
  scale_colour_discrete(name = "Race/ethnicity or Education") +
  scale_linetype_discrete(name = "Race/ethnicity or Education") +
  scale_shape_discrete(name = "Race/ethnicity or Education") +
  labs(
    x = "Sample Size",
    y = "",
    title = glue("Validation Results: Life Expectancy at age {start_age}")
  ) +
  theme(legend.position = c(0.8, 0.1),
        axis.text.x = element_text(angle = 45, hjust = 1))
plot(r)


# plots of age-specific mortality rates performance -----------------------


if (!by_edu) {
  ages <- c("All ages, crude" = 98, "All ages, standardized" = 99, "< 1 yr" = 0, "1-4 yrs" = 1,
            "5-9 yrs" = 5, "10-14 yrs" = 10, "15-19 yrs" = 15, "20-24 yrs" = 20, "25-29 yrs" = 25,
            "30-34 yrs" = 30, "35-39 yrs" = 35, "40-44 yrs" = 40, "45-49 yrs" = 45, "50-54 yrs" = 50,
            "55-59 yrs" = 55, "60-64 yrs" = 60, "65-69 yrs" = 65, "70-74 yrs" = 70, "75-79 yrs" = 75,
            "80-84 yrs" = 80, "85+ yrs" = 85)
} else {
  ages <- c("All ages, crude" = 98, "All ages, standardized" = 99, "25-29 yrs" = 25,
            "30-34 yrs" = 30, "35-39 yrs" = 35, "40-44 yrs" = 40, "45-49 yrs" = 45, "50-54 yrs" = 50,
            "55-59 yrs" = 55, "60-64 yrs" = 60, "65-69 yrs" = 65, "70-74 yrs" = 70, "75-79 yrs" = 75,
            "80-84 yrs" = 80, "85+ yrs" = 85)
}

mx[, race := as.factor(race)]
mx[, edu := as.factor(edu)]
for (this_age in names(ages)) {
  p <- ggplot(mx[age == ages[this_age],],
              aes(
                x = factor(size),
                y = value,
                colour = factor(interaction(race, edu)),
                group = factor(interaction(race, edu)),
                shape = factor(interaction(race, edu))
              )) +
    facet_wrap( ~ variable, scales = "free") +
    geom_point(alpha = 0.5, size = 2) + geom_line(alpha = 0.5) +
    scale_colour_discrete(name = "Race/ethnicity or Education") +
    scale_linetype_discrete(name = "Race/ethnicity or Education") +
    scale_shape_discrete(name = "Race/ethnicity or Education") +
    labs(
      x = "Sample Size",
      y = "",
      title = paste0("Validation Results: Mortality Rate, ", this_age),
      caption = "RMSE, Mean Error, and Mean Absolute Error metrics are multiplied by 100,000. No other metrics are scaled."
    ) +
    theme(
      legend.position = c(0.8, 0.1),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(hjust = 0)
    )
  plot(p)
}
dev.off()

message("DONE!")

