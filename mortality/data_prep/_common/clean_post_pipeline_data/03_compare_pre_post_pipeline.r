######################################################################################################
## Description: Check the post-pipeline deaths dataset:
##            do deaths before and after running through the CoD pipeline match:
##            1. by year?
##            2. by year-sex?
##            3. by year-sex-age?
##            4. by year-sex-location_id?
##            Also, are the discrepancies in age made up for by the values in the 'missing' age group?
## Input:
##      -post_pipeline: data.table, cleaned post-pipeline dataset with location_id as an identifier
##      -pre_pipeline_dir: string, path to .csv of pre-CoD pipeline deaths
##      -cc_dir: string, path to .csv of corrections-phase deaths that include cc code deaths
##      -epsilon: limit to which you want death counts to add up (they won't match perfectly
##        due to rounding. Default is 0.05)
##      -missing: demographic category in which you expect missingness. For counties, will be "age".
##               for tracts, "sex".
######################################################################################################

compare_pre_post_pipeline <- function(post_pipeline, pre_pipeline_dir, cc_dir, by_race = F, 
                                      epsilon = 0.05, missing = NULL){

  library(data.table)

  # Read and format pre-pipeline data to contain same age variable as post-pipeline data
  pre_pipeline <- fread(pre_pipeline_dir)
  if (by_race) setnames(pre_pipeline, "race_hisp_recode", "race")
  pre_pipeline[, age := as.numeric(age_group_id)]
  pre_pipeline[age_group_id == -9, age := NA]    # missing ages
  pre_pipeline[age_group_id %in% c(0.01, "1-5mo", "6-11mo"), age := 0]
  pre_pipeline[age_group_id %in% c("12-23mo", "2-4"), age := 1]
  
  # Get number of cc code deaths, which should be the only discrepancy between pre- and post-pipeline data
  cc_deaths <- fread(cc_dir)
  num_cc_deaths <- sum(cc_deaths[cause_id == 919, deaths]); rm(cc_deaths)

  # Pre-/post-pipeline total death count check:
  tot_pre <- sum(pre_pipeline[, deaths])
  tot_post <- sum(post_pipeline[level == 1, deaths])    # these are aggregated up so deaths are duplicated
  whole_diff <- tot_post - tot_pre
  message(paste0("Total death discrepancy of ", round(abs(whole_diff), 2), " deaths, which ", 
                 if(isTRUE(all.equal(num_cc_deaths, abs(whole_diff), epsilon))) "matches the number of cc code deaths :)" 
                 else ("does NOT match the number of cc code deaths :(")))
  
  # Sum across years, year-sexes, year-sex-ages, and year-sex-location_ids, make sure numbers add
  sum_levels <- list(
                  c("year_id"),
                  c("year_id", "sex_id"),
                  c("year_id", "sex_id", "age"),
                  c("year_id", "sex_id", "location_id"),
                  if (by_race) c("year_id", "sex_id", "race")
    )

  for (level in sum_levels){
    if(!is.null(level)) {

      idx <- 1
      for (word in level) {
        if (idx == 1) {
          bystring <- word
        } else {
          bystring <- paste(bystring, word, sep = ",")
        }
        idx <- idx + 1
      }
      
      message(paste("Comparing deaths by", bystring))
      pre <- pre_pipeline[, list(deaths_pre = sum(deaths)), by = bystring]
      post <- post_pipeline[level == 1, list(deaths_post = sum(deaths)), by = bystring]
      merged <- merge(pre, post, by = level, all = T)
      merged[, difference := deaths_post - deaths_pre]

      bad <- merged[abs(difference) > epsilon]
      if (nrow(bad) != 0){
        # see if issue is due to missing data
        if (!is.null(missing) & missing %in% level){
          if (missing == "age"){
            message("Checking missing age")
            pre_count <- sum(pre_pipeline[is.na(age), deaths]) # total number of 'missing' deaths in pre-rdp dataset
            post_count <- tot_post - sum(pre_pipeline[!is.na(age), deaths]) - whole_diff  # total post-rdp deaths, minus unknown ages, minus total discrepancy in deaths
          } else if (missing == "sex"){
            message("Checking missing sex")
            pre_count <- sum(pre_pipeline[sex_id == 9, deaths]) # total number of 'missing' deaths in pre-rdp dataset
            post_count <- tot_post - sum(pre_pipeline[sex_id != 9, deaths]) - whole_diff  # total post-rdp deaths, minus unknown ages, minus total discrepancy in deaths
          }
          diff <- post_count - pre_count
          if(abs(diff) > epsilon){
            message(paste("Death count mismatch in", missing, "of", round(diff, 2), "deaths"))
          } else {
            message("Discrepancy accounted for")
          }
        } else {
          discrepancy <- sum(abs(bad$difference))
          if (isTRUE(all.equal(num_cc_deaths, abs(discrepancy), epsilon))) {
            message("Discrepancy accounted for")
          } else {
            message(paste("Death count mismatch in", bystring, "by", round(discrepancy, 2), "deaths"))
          }
        }
      }
    }
  }
  message("Checks complete!")
}
