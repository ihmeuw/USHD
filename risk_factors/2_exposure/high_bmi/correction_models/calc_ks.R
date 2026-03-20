####################################################################################################
## Description: Perform Kolmogorov-Smirnov test to compare NHANES measured
##                BMI distribution to the quantile-normalized description. Do separately
##                by survey and by sex. 
##                
##              If provide a path to all_models object, will calculate the KS statistic 
##                for each quantile normalization crosswalk. If provide a datatable
##                for pred, will calculate KS statistics for just the provided model.
##                
##                
## Inputs: Path to the all_models_with_fits.rds list in the candidate model folders.
##           -- OR --
##        A single DT containing predictions from QN cw
## 
## Outputs: Returns KS score for model(s)
##         
####################################################################################################

source("FILEPATH")

my_ks <- function(data){
  require(data.table)
  require(stringr)
  
  results = setDT(expand.grid(sex = 1:2, survey = c("overall", unique(data$survey)), D_stat = as.numeric(NA), KS_p_val = as.numeric(NA)))
  
  for(s in 1:2){
    nhanes_x <- data[survey == "nhanes" & sex == s, bmi_measure]
    nhanes_wt <- data[survey == "nhanes"  & sex == s, mec_wt]
    for(svy in unique(data$survey)){
      svy_x <- data[survey == svy & sex == s, bmi_matched_val]
      if(svy == "nhanes"){
        svy_wt <- data[survey == svy & sex == s, mec_wt]
      } else{
        svy_wt <- data[survey == svy & sex == s, wt]  
      }
      
      t <- ks_test(x = nhanes_x, y = svy_x, w_x = nhanes_wt, w_y = svy_wt, thresh = 0) # use a very conservative threshold of 0
      results[sex == s & survey == svy, D_stat := t$statistic]
      results[sex == s & survey == svy, KS_p_val := t$p.value]
    }
    
    # overall, excluding NHANES predicted
    svy_x <- data[survey != "nhanes" & sex == s, bmi_matched_val]
    svy_wt <- data[survey != "nhanes" & sex == s, wt]  
    t <- ks_test(x = nhanes_x, y = svy_x, w_x = nhanes_wt, w_y = svy_wt, thresh = 0) # use a very conservative threshold of 0
    results[sex == s & survey == "overall", D_stat := t$statistic]
    results[sex == s & survey == "overall", KS_p_val := t$p.value]
  }
  return(results)
}

calc_ks <- function(all_models_path = NULL, data = NULL){
  if(!is.null(all_models_path) & !is.null(data)){
    stop("Only `all_models_path` OR `data` can be not-NULL")
  }
  if(!is.null(all_models_path)){
    all_models <- readRDS(all_models_path)
    names <- names(all_models)
    names <- grep("quant", names, value = T) # get just quantile normalization models
    
    all_results <- NULL
    
    for(model in names){
      print(paste("calculating statistics for", model))
      model_list <- all_models[[model]]
      if(identical(character(0), as.character(model_list$adjusted_data)) || model_list$adjusted_data== "did not sucessfully complete"){
        print(paste(model, "did not sucessfully complete...skipping"))
      } else{
        data <- readRDS(model_list$adjusted_data)
        
        # calculate KS statistics
        results <- my_ks(data)
        # assign model name
        results[, model := model]
        
        # create a label with the stratification variables used in the model
        strat3 <- as.character(gsub("'", "", str_match(model_list$quant_norm_function, "(?<=strat3 = )\'[:graph:]+'"))) # get the 1st optional stratification 
        strat4 <- as.character(gsub("'", "", str_match(model_list$quant_norm_function, "(?<=strat4 = )\'[:graph:]+'"))) # get the 2nd optional stratification 
        if(!is.na(strat3)) {strat3 <- strat3} else {strat3 <- NULL}
        if(!is.na(strat4)) {strat4 <- strat4} else {strat4 <- NULL}
        strata <- paste("age, sex", strat3, strat4, sep = " ")
        
        results[, strat_by := strata]
        
        
        # combine with results from other models
        all_results <- rbind(all_results, results, use.name = T, fill = T)
      }
      
    }
  } else{ 
    all_results <- my_ks(data)
  }
  
  setDT(all_results)
  all_results <- all_results[!is.na(sex)]
  all_results[, KS_p_val := round(KS_p_val, digits = 4)]
  all_results[, significant_diff := as.numeric(KS_p_val < 0.05)] # if p< 0.05, we consider the predicted and measured distribution significantly different than each other (undesirable)
  return(all_results)
}
