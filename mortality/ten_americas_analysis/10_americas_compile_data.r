
# Load required libraries
pacman::p_load(data.table, ggrepel) 
source(paste0("/mortality/sae_models/functions/load_sae_shared.r"))
source(paste0("/mortality/ten_americas_analysis/functions.r"))
set.seed(12345)

###################################################
# Arguments
resub <- F # resub = T if we want to regenerate all results
years <- 2000:2021
n_draws <- 1000
n_pre_y <- 10 # use previous 10 years to calculate growth rates

indir <- "[PROJECT_DIRECTORY]"
outdir <- "[PROJECT_DIRECTORY]"

america_def_file <- paste0(dirname(outdir), "/americas_definitions.csv")
growth_rates_lbl <- "final"

adjust_mis_code <- paste0("/mortality/ten_americas_analysis/draws_adjust_misclassification.r")

# Ten Americas labels
america_labels <- c("America 1: Asian", 
                    "America 2: Northlands, Rural, Low-income, White", 
                    "America 3: Middle America", 
                    "America 4: Appalachia and Mississippi Valley, Low-income, White", 
                    "America 5: West, AIAN",
                    "America 6: Middle Black", 
                    "America 7: South, Rural, Low-income, Black", 
                    "America 8: High-risk, Urban, Black", 
                    "America 9: Southwest, Latino", 
                    "America 10: Middle Latino") # be revised later


####################################################
# Adjust deaths with misclassification 
# Submit jobs
if (resub | !file.exists(paste0(outdir, "/10_americas_adjusted_deaths_draws_2021.rds"))) {
  
  for (this_year in years) {
    
    message(this_year)
    
    sbatch(name = paste0("adjust_mis_", this_year),
           code = adjust_mis_code,
           arguments = c(indir, outdir, america_def_file, this_year, resub),
           archive = T, 
           fthread = 1, m_mem_free = "100G", h_rt = "5:00:00",
           sgeoutput = dirname(outdir))
    
  }
} else {
  message("Data has been prepped")
}


##################################################
# Load data
# Load pop with 10 America groups
pop_files <- list.files(path = indir, 
                        pattern = "pop_\\d{4}\\.RDS", full.names = T)
pop <- rbindlist(lapply(pop_files, readRDS))
pop_gr <- pop[year %in% (min(years)-(n_pre_y-1)):max(years)]# save pop with the previous 10 years to calculate growth rates later
pop <- pop[year %in% years]

# Calculate growth rates
if (resub | !file.exists(paste0(dirname(outdir), "/growth_rates_", growth_rates_lbl, ".rds"))) {
  calc_growth_rates(pop_gr, outdir = dirname(outdir), lbl = growth_rates_lbl, n_years = n_pre_y)
}
# Load growth rates
gr <- readRDS(paste0(dirname(outdir), "/growth_rates_", growth_rates_lbl, ".rds"))

# Load death draws with 10 america groups
death_draws_files <- list.files(path = outdir, 
                          pattern = "10_americas_adjusted_deaths_draws_\\d{4}\\.rds", full.names = T)
death_draws <- rbindlist(lapply(death_draws_files, readRDS))
stopifnot(length(unique(death_draws$year)) == length(years))


###################################################
# Simulate deaths -----------------------------------------------------
death_draws[, simulated_deaths := rpois(1, deaths), keyby = 'america,year,sex,age,draw']

# Merge with pop
draws <- merge(death_draws, pop, by = c("america", "year", "sex", "age"), all.x= T)
stopifnot(nrow(draws[is.na(simulated_deaths) | is.na(pop)]) == 0)

# Calculate mortality rates -----------------------------------------------------
draws <- draws[, mx := simulated_deaths / pop] # zero row has pop = 0
stopifnot(nrow(draws[is.na(mx)]) == 0)
draws <- draws[,.(america, year, sex, age, draw, pop, mx)]

# Aggregate sexes
message("Aggregating sexes")
both_sexes <- draws[, list(sex = 3,
                           pop = sum(pop),
                           mx = ifelse(sum(pop) == 0, 0, weighted.mean(mx, pop))),
                    by = 'america,year,age,draw']
draws <- rbind(draws, both_sexes, use.names = T, fill = T)

# Save
saveRDS(draws, paste0(outdir, "/deaths_all_", n_draws, "_draws_adjusted.rds"))


# Generate life table -----------------------------------------
calc_sum_lifetable_draws(draws, gr, outdir = outdir, lbl = "adjusted")


##################################
# Partial life expectancy by age group
lt <- readRDS(paste0(outdir, "/lt_all_", n_draws, "_draws_adjusted.rds"))

# Aggregate ages
age_groups <- data.table(age = c(0,1,5,10,15,20,25,30,35,40,
                                 45,50,55,60,65,70,75,80,85),
                         age_g = c(0,0,5,5,5,5,25,25,25,25,
                                   45,45,45,45,65,65,65,65,85))
lt <- merge(lt, age_groups, by = "age", all.x = T)

# calculate Lx in each age group
lt_age <- lt[, Lx_g := sum(Lx), keyby = "america,year,sex,age_g,draw"]
lt_age[, lx_g := lx[1], keyby = "america,year,sex,age_g,draw"]
lt_age[,pex := Lx_g/lx_g]
lt_age[age == 85, pex := ex]
lt_age <- unique(lt_age[,.(america,year,sex,age_g,draw,pex)])
setnames(lt_age, "age_g", "age")

# Save draws
saveRDS(lt_age, paste0(outdir, "/pex_all_1000_draws_age.rds"))

# Summarize
lt_est <- lt_age[,list(pex_mean = mean(pex),
                       pex_lower = quantile(pex, 0.025, type = 5),
                       pex_upper = quantile(pex, 0.975, type = 5)),
                 keyby = 'america,year,sex,age']

# Formatting ----------------------
lt_est[, sex_lbl := factor(sex, levels = 1:3, labels = c("Males", "Females", "Both Sexes"))]
lt_est[, america_lbl := factor(america, levels = 1:10, labels = america_labels)]
lt_est[, age_lbl := factor(age, level = c(0,5,25,45,65,85), labels = c("0-4","5-24","25-44","45-64","65-84","85+"))]

saveRDS(lt_est, paste0(outdir, "/pex_all_age.rds"))



