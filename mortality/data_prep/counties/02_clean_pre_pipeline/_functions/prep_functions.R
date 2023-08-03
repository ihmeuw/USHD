#################################################################
# Description: This is the place where any formatting functions
#              for imputation is going to live.
#################################################################

# function to create the race_group_1977 variable:
recode_race <- function(year, race_code, bridged_flag, recode_40, hisp_recode){
  
  race_group = 'Unknown'
  
  if (race_code == 1){
    race_group <- 'NH White'
  }else if (race_code == 2){
    race_group <- 'NH Black'
  }else if (race_code == 3){
    race_group <- 'NH AIAN'
  }else if (race_code %in% 4:5){
    race_group <- 'NH Asian'
  }else if (race_code == 6){
    race_group <- 'NH NHOPI'
  }
  
  # specific to 1989-91
  if (year %in% 1989:1991){
    if (race_code == 7){
      race_group <- 'NH Asian'
    }else if (race_code == 8){
      race_group <- 'NH Other API'
    }else if (race_code == 9){
      race_group <- 'NH Other Race'
    }
  }
  
  # specific to 1992 onward
  else if (year >= 1992){
    if (race_code == 0){
      race_group <- 'NH Other Race'
    }else if (race_code %in% c(7, 18, 28, 48)){
      race_group <- 'NH Asian'
    }else if (race_code %in% c(10, 38, 58)){
      race_group <- 'NH NHOPI'
    }else if (race_code %in% c(8, 68)){
      race_group <- 'NH Other API'
    }else if (race_code == 78){
      race_group <- 'NH Multiracial'
    }
  }
  
  # for 2003 onward (empty column otherwise)
  if(!is.na(bridged_flag)){
    if (bridged_flag == 1){
      race_group <- 'NH Multiracial'
    }
  }
  
  # for 2011 onward (empty column otherwise)
  if (!is.na(recode_40)){
    if (recode_40 == 10){
      race_group <- 'NH Asian'
    }else if (recode_40 == 14){
      race_group <- 'NH NHOPI'
    }else if (recode_40 %in% 15:40){
      race_group <- 'NH Multiracial'
    }
  }
  
  # allow Hispanic ethnicity to supersede race
  if (!is.na(hisp_recode)){
    if (hisp_recode <= 5){
      race_group <- 'Hispanic'
    }
  }
  
  if (race_group %in% c('NH NHOPI', 'NH Other API', 'NH Asian')){
    race_group = 'NH API'
  }else if (race_group == 'NH Multiracial'){
    if (race_code == 0){
      race_group = 'NH Other Race'
    }else if (race_code == 1){
      race_group = 'NH White'
    }else if (race_code == 2){
      race_group = 'NH Black'
    }else if (race_code == 3){
      race_group = 'NH AIAN'
    }else if (race_code %in% c(4:9, 10, 18, 28, 38, 48, 58, 68, 78)){
      race_group = 'NH API'
    }
  }
  
  return(race_group)
}