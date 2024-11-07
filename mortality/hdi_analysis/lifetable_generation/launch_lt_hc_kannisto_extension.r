# load common functions used by USHD
empir_funcs <- list.files("functions/")
for(func in empir_funcs) {
  source(paste0("functions/", func))
}

proj_dir <- "[PROJECT_DIRECTORY]"

races <- c(2,4,5,6,7)
race_labels <- c("Hispanic","NH Black", "NH White", "NH AIAN","NH API")
sexes <- c(1,2)
years <- 2008:2021 # hdi analysis years

resub_fits <- F
by_puma <- T # if you want to crosswalk from mcnty to PUMA

model_dir <- "[MODEL_DIRECTORY]"
# used to deterine if the script ran all the way through
lt_plotting_dir <- file.path(proj_dir,"full_lifetables", basename(dirname(model_dir)), "plots") 

## Make a list of the dimensions
lt_calc_info <- data.table()

for(geo in c("natl","mcnty")) {

  for(rr in races) {

    for(sx in c(1,2)) {

      for(yr in years) {

        if((resub_fits & !file.exists(file.path(lt_plotting_dir, paste0(geo,"_",yr,"_sex_",sx,"_race_",rr,".pdf")))) | !resub_fits) {

          lt_calc_info <- rbind(lt_calc_info,
                                data.table(geography = geo,
                                           race = rr,
                                           sex = sx,
                                           year = yr))
        }
      }
    }
  }
}

lt_calc_info[,ushd_dir := model_dir]
lt_calc_info[,by_puma := by_puma]

if(by_puma) {
  lt_calc_info <- lt_calc_info[geography == "mcnty"]
}

fwrite(lt_calc_info, file.path(proj_dir, "full_lt_calc_list.csv"))

compile_reg_id <- sbatch(code = "[PATH_TO_CODE]/hc_extension_ex.r",
                         name = "extend_lts",
                         queue = "QUEUE",
                         fthread = 3,
                         m_mem_free = "20G",
                         h_rt = "01:00:00",
                         archive = T,
                         project = "PROJECT",
                         sgeoutput = proj_dir,
                         array = paste0("1-", nrow(lt_calc_info)),
                         array_throttle = 400)

# Check that all of them finished
lt_calc_info[,full_lt_filename := file.path(dirname(lt_plotting_dir), "kannisto_hc_method", paste0("full_lt_",ifelse(by_puma, "puma", geography),"_",year,"_sex_",sex,"_race_",race,".rds"))]
lt_calc_info[,comparison_filename := file.path(dirname(lt_plotting_dir), "kannisto_hc_method","comparison_data", paste0("comparison_lt_",ifelse(by_puma, "puma", geography),"_",year,"_sex_",sex,"_race_",race,".rds"))]
lt_calc_info[,diffs_filename := file.path(dirname(lt_plotting_dir), "kannisto_hc_method","ex85_differences", paste0("diffs_",ifelse(by_puma, "puma", geography),"_",year,"_sex_",sex,"_race_",race,".rds"))]

missing <- lt_calc_info$full_lt_filename[!file.exists(lt_calc_info$full_lt_filename)]
stopifnot(length(missing) == 0)

### Comparison
comparison_files <- lt_calc_info[, comparison_filename]
comparison_files <- comparison_files[(file.exists(comparison_files))]
comparison_lt <- rbindlist(lapply(comparison_files, readRDS), fill=T)
comparison_lt <- comparison_lt[!(version %like% "3 - ")]

comparison_lt[,race := factor(race, c(2,4,5,6,7), c("Latino", "Black", "White", "AIAN", "API"))]

### Save the extended 5-year age group life tables to be used in the HDI analysis (before switching to using 1-year qx)
saveRDS(comparison_lt[version %like% "2 - " & age >= 25, .(puma = area, year, sex, race, age, ex)],
        file.path(dirname(lt_plotting_dir), "kannisto_hc_method","abridged_lt_for_hdi", "lt_puma_extended.rds"))

comparison_lt[,sex := factor(sex, c(1:2), c("Males","Females"))]

combined_0 <- copy(comparison_lt[age %in% c(25,85)])[,.(level, area, year, sex, race, age, ex, qx, mx, ax, lx, version)]
combined_0[version %like% "1 - ", version := "original"]
combined_0[version %like% "2 - ", version := "extension_ex_scaled"]

# make sure that ax, qx, and values did not change in the younger ages
younger_age_compare <- dcast.data.table(combined_0[age <= 80],
                                        level + area + year + sex + race + age ~ version,
                                        value.var=c("mx","qx","ax","lx"))

stopifnot(nrow(younger_age_compare[abs(mx_original - mx_extension_ex_scaled) > 0.00001]) == 0)
stopifnot(nrow(younger_age_compare[abs(qx_original - qx_extension_ex_scaled) > 0.00001]) == 0)
stopifnot(nrow(younger_age_compare[abs(ax_original - ax_extension_ex_scaled) > 0.00001]) == 0)
stopifnot(nrow(younger_age_compare[abs(lx_original - lx_extension_ex_scaled) > 0.00001]) == 0)

combined_0 <- dcast.data.table(combined_0, level + area + year + sex + race + age ~ version, value.var=c("ex"))

## Get data on the differences
diff_files <- lt_calc_info[, diffs_filename]
diff_files <- diff_files[(file.exists(diff_files))]
diffs_dt <- rbindlist(lapply(diff_files, readRDS), fill=T)

diffs_dt[,race := factor(race, c(2,4,5,6,7), c("Latino", "Black", "White", "AIAN", "API"))]
diffs_dt[,sex := factor(sex, c(1:2), c("Males","Females"))]

combined_0 <- merge(combined_0, diffs_dt[,.(change_85 = abs_diff, area, year, sex, race, level)], by=c("area","year","sex","race","level"))
# check that the change equals what is in the data
combined_0[,diff_recalc := extension_ex_scaled - original]
stopifnot(nrow(combined_0[age == 85 & abs(change_85 - abs(diff_recalc)) > 0.0001]) == 0)

# Look at some of the places with large differences
plot_large_diff <- function(comparison_lt, combined_0, rr, sx, yr, lvl, loc, print_plot = T) {

  tmp <- comparison_lt[race == rr & sex == sx & year == yr & level == lvl & area == loc]
  ex_label_compare <- combined_0[race == rr & sex == sx & year == yr & level == lvl & area == loc]

  ex_new <- ex_label_compare[age == 85]$extension_ex_scaled
  ex_old <- ex_label_compare[age == 85]$original
  label_ex <- paste0("New e85: ",round(ex_new,2),", original e85: ",round(ex_old,2))

  if(print_plot) {
    pdf(file.path(dirname(lt_plotting_dir),"kannisto_hc_method","plots","post_modeling",paste0(lvl,"_area_",loc,"_",rr,"_",sx,"_",yr,".pdf")), height = 10, width=12)
    plot_comparison(fdata = tmp, title = paste0(lvl,", area ",loc,", ",rr,", ",sx,", ",yr," -- ",label_ex))
    dev.off()
  } else {
    return(plot_comparison(fdata = tmp, title = paste0(lvl,", area ",loc,", ",rr,", ",sx,", ",yr," -- ",label_ex)))
  }
}

# plot comparison_lt
plot_lt_results <- function(fdata, var) {
  gg <- ggplot(fdata, aes(age, get(var), color = version, linetype=version)) +
    geom_line(alpha=0.7) +
    scale_linetype_manual(values = c(`1 - original` = "dashed",
                                     `2 - post extension and scaling` = "solid")) +
    theme_bw() +
    scale_color_manual(values = c(`1 - original` = "#1b9e77",
                                  `2 - post extension and scaling` = "#d95f02")) +
    theme(legend.position = "bottom")

  if(var == "qx") {
    gg <- gg +
      scale_y_log10() +
      labs(title = paste0(var," (log10)"),
           y = var,
           color = "",
           linetype = "")
  } else {
    gg <- gg +
      labs(title = var,
           y = var,
           color = "",
           linetype = "")
  }
  return(gg)
}

plot_comparison <- function(fdata, title) {
  gg1 <- plot_lt_results(fdata = fdata, var = "ex")
  gg2 <- plot_lt_results(fdata = fdata, var = "qx")
  gg3 <- plot_lt_results(fdata = fdata, var = "mx")
  gg4 <- plot_lt_results(fdata = fdata, var = "lx")
  gg5 <- plot_lt_results(fdata = fdata, var = "ax")

  gridExtra::grid.arrange(gg1, gg2, gg3, gg4, gg5, ncol=2, top = paste0(title))
}

pdf(file.path(dirname(lt_plotting_dir),"kannisto_hc_method","plots","post_modeling",paste0("scatter_e0",if(by_puma)"_puma",".pdf")), height=8,width=12)

for(ag in c(25,85)) {

  for(rr in unique(combined_0$race)) {
    gg <- ggplot(combined_0[race == rr & level != "natl" & age == ag], aes(original, extension_ex_scaled, color = sex)) +
      geom_point(shape=1,alpha=0.5) +
      theme_bw() +
      geom_abline(intercept = 0, slope = 1) +
      scale_color_manual(values = c(Females = "#1b9e77",
                                    Males = "#d95f02"))+
      facet_wrap(~level+year) +
      labs(title = paste0("Life expectancy, age ",ag,", ",rr),
           subtitle = paste0("Original versus extended and scaled to terminal ex"),
           color = element_blank())

    print(gg)
  }
}

dev.off()

### NOTE: you can look at any places with large changes in e85 using plot_large_diff()
