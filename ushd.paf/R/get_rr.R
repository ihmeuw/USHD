#' Get RR data from air PM exposure files
#'
#' @param cause_list_file CSV file with list of causes and MRBRT lookup tables
#' @param rei_id Risk ID
#' @param sex_id vector of sex_ids to return data for. Probably 1:2, occasionally just 1 OR 2
#' @param location_id location_id value to return data for. 
#' @param year_id vector of years to return data for.
#' @param n_draws number of draws to return. `draw` values above this will be removed from the data.
#' @param extra_dim character name of extra dimension of data. This should be "race" or "edu"
#' @param extra_dim_values identifier to load relative to extra_dim. E.g., if
#'
#' @details Ported from 03_calc_rr_paf.r
#'
#' @return Returns data.table with columns age_group_id, sex_id, location_id, year_id, draw, and pred, and extra_dim
#' @export
ushd.get_pm_rr <- function(cause_list_file, rei_id, sex_id, location_id, year_id, n_draws, extra_dim, extra_dim_values) {
  # tmrel generated locally, could be read from GBD
  tmrel.start <- rei_tmrel_constants[[as.character(rei_id)]]$tmrel.start
  tmrel.end <- rei_tmrel_constants[[as.character(rei_id)]]$tmrel.end

  cause_list_file = gsub("EXP", ushd.get_dir_string(rei_id), cause_list_file)

  #Read exposure data in to calculate RR
  dt_exp <- ushd.get_exposure(model_exp_run_id = ushd.dbr::get_risk_exp_model_run(rei_id = rei_id)$model_exp_run_id,
                              year_id, sex_id, location_id, n_draws, extra_dim, extra_dim_values, level = "mcnty")
  
  # The exposure estimates include an age group "0" that includes <1 and 1-4.
  # ushd.get_exposure assigns this age_group_id 236. To align PAFs with the 
  # age groups used for YLLs, we want to split age_group_id 236 into age_group_ids 
  # 28 and 5 (ages <1 and 1-4, respectively).

  # If age_group_id 236 is in the DT and
  # age_group_id 28 and 5 are *not* in the DT
  if((236 %in% dt_exp[, unique(age_group_id)]) &
     !(28 %in% dt_exp[, unique(age_group_id)]) &
     !(5 %in% dt_exp[, unique(age_group_id)])) {
    # split age group 236
    dt_exp <- rbindlist(list(
      copy(dt_exp[age_group_id == 236])[, age_group_id := 28],
      copy(dt_exp[age_group_id == 236])[, age_group_id := 5],
      dt_exp[age_group_id != 236]
    ))  
  }
  
  # Add in tmrel
  dt_exp[,tmrel:=runif(.N, tmrel.start, tmrel.end)]

  mr.brt.cols <- c("exposure",paste0("mrbrt_",1:n_draws))
  setnames(dt_exp,"pred", "ambient")

  dt_exp = dt_exp[,draw:=draw+1]
  outcomes <- fread(cause_list_file)
  out <- vector("list", length(outcomes$cause_id)) %>%
    setNames(., outcomes$cause)
  
  for (cid in outcomes$cause_id) {

    row <- outcomes[cause_id == cid,]
    message(sprintf("Calculating Air %s RR for cause = %s", ushd.get_dir_string(rei_id), row$cause))

    mrbrt <- fread(row$filepath)
    exposure_vals <- mrbrt$risk
    mrbrt <- mrbrt[,1:(n_draws+1)]
    
    setnames(mrbrt,mr.brt.cols)
    mrbrt <- exp(mrbrt)
    mrbrt[,exposure:=NULL]
    mrbrt[,exposure_spline:=exposure_vals] # predicted out new exposures
    
    # make long by draw
    mrbrt <- melt.data.table(mrbrt,id.vars="exposure_spline", measure.vars=patterns("mrbrt_"), variable.name="draw", variable.factor=T, value.name="mrbrt")
    mrbrt[,draw:=as.numeric(draw)]
    mrbrt_exposures <- unique(mrbrt$exposure_spline)
    
    setkeyv(mrbrt,c("draw","exposure_spline"))
    
    # Calculate RR
    dt_cats <- c("tmrel","ambient")
    temp_dt <- copy(dt_exp)
    stopifnot(setequal(sort(unique(temp_dt$draw)), sort(unique(mrbrt$draw))))
    for(dt_cat in dt_cats){
      
      # merge on mrbrt predictions based on closest available exposure datapoint predicted for mr_brt & draw
      # make a join column because dt loses its exposure column (& we want to keep no2 and tmrel)
      mrbrt[,join_exp:=exposure_spline]
      temp_dt[,join_exp:=get(dt_cat)]
      
      # set the key variables on which to join (the roll is performed on the last one)
      setkeyv(mrbrt,c("draw","join_exp"))
      setkeyv(temp_dt,c("draw","join_exp"))
      
      # rolling join by nearest exposure
      temp_dt <- mrbrt[temp_dt, roll = "nearest"]
      
      # change name
      setnames(temp_dt,"mrbrt",paste0("rr_",dt_cat,"_",row$cause))
      
      # remove the unnecessary columns
      temp_dt[,exposure_spline:=NULL]
      temp_dt[,join_exp:=NULL]
    }
    temp_dt[,cause_id:=row$cause_id]
    rm(mrbrt)
    
    out[[row$cause]] = temp_dt
  }
  dt_exp <- rbindlist(out, fill=TRUE, use.names=TRUE)

  # added for air pollution exposure merge
  dt_exp[,parameter:="Air PM categorical"]

  # remove exposure columns to prevent merge issues
  dt_exp = dt_exp[,draw:=draw-1]

  return(dt_exp)
}
