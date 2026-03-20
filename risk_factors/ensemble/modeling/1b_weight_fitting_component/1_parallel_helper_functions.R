select_initial_weights <- function(distlist, initial_condition){
  # Select weights based on initial condition. Given n = the number of distributions in distlist, 
  # the first to nth sets of weights should have a different distribution given 90% of weight, 
  # and the n + 1 set of weights will be all distributions given equal weight
  
  if(initial_condition == (length(distlist)+1) ){
    weights = rep(1/length(distlist), length(distlist))
  } else if(initial_condition %in% 1:length(distlist)){
    weights = rep((1 - 0.9)/(length(distlist)-1), length(distlist))
    weights[[initial_condition]] <- 0.9
    
  } else {
    weights = runif(length(distlist),0,1)
    weights = weights/sum(weights)
  }
  
  return(weights)
}



create_CDF <- function(Data, nid_loc_yr_i, svy_wt) {
  cdfDATA <- spatstat.geom::ewcdf(Data, weights = svy_wt, normalise = 1)
  CDF_Data_Params <- list()
  CDF_Data_Params$mn <- weighted.mean(Data, w = svy_wt, na.rm = T)
  CDF_Data_Params$variance <- spatstat.geom::weighted.var(Data, w= svy_wt, na.rm=T)
  CDF_Data_Params$XMIN <- 10
  CDF_Data_Params$XMAX <- 50
  CDF_Data_Params$x <- seq(CDF_Data_Params$XMIN, CDF_Data_Params$XMAX, length = 1000)
  CDF_Data_Params$fx <- 0*CDF_Data_Params$x
  CDF_Data_Params$cdfDATAYout <- cdfDATA(sort(CDF_Data_Params$x))
  CDF_Data_Params$nid <- nid_loc_yr_i
  print(paste(CDF_Data_Params$mn, CDF_Data_Params$variance, CDF_Data_Params$XMIN, CDF_Data_Params$XMAX))
  
  
  return(CDF_Data_Params)
}

# I think this function finds distributions for which the parameters can't be solved
#   for the provided mean/var, so it sets the weight on this class to zero
find_bad_fits <- function(weights, distlist, CDF_Data_Params) {
  for(z in length(weights):1){
    LENGTH <- length(formals(unlist(distlist[[z]]$mv2par)))
    if (LENGTH==4) {
      est <- try((unlist(distlist[[z]]$mv2par(CDF_Data_Params$mn,CDF_Data_Params$variance,XMIN=CDF_Data_Params$XMIN,XMAX=CDF_Data_Params$XMAX))),silent=T)
    } else {
      est <- try((unlist(distlist[[z]]$mv2par(CDF_Data_Params$mn,CDF_Data_Params$variance))),silent=T)
    }
    if(class(est)=="try-error" | (class(est) == "character" && grepl("Error in int", est[1]))){
      weights[z] <- 0
    }
  }
  return(weights)
}

rescale_to_one <- function(weights){
  
  weights = weights/sum(weights)
}

create_pdf <- function(distlist, weights, CDF_Data_Params) {
  for(z in 1:length(weights)){
    dist_z <- names(weights)[z]
    LENGTH <- length(formals(unlist(distlist[[dist_z]]$mv2par)))
    if (LENGTH==4) {
      est <- try((unlist(distlist[[dist_z]]$mv2par(CDF_Data_Params$mn,CDF_Data_Params$variance,XMIN=CDF_Data_Params$XMIN,XMAX=CDF_Data_Params$XMAX))),silent=T)
    } else {
      est <- suppressMessages(try((unlist(distlist[[dist_z]]$mv2par(CDF_Data_Params$mn,CDF_Data_Params$variance))),silent=T))
    }
    fxj <- try(distlist[[dist_z]]$dF(CDF_Data_Params$x,est),silent=T)
    if(class(est)=="try-error" | (class(est) == "character" && grepl("Error in int", est[1]))) {
      fxj <- rep(0,length(CDF_Data_Params$fx))
      if(weights[[z]] > 0){
        message("Could not generate density with ", names(weights)[z])  
      }
      
    }
    fxj[!is.finite(fxj)] <- 0
    CDF_Data_Params$fx <- (CDF_Data_Params$fx + fxj*weights[[z]])
  }
  CDF_Data_Params$fx[!is.finite(CDF_Data_Params$fx)] <- 0
  CDF_Data_Params$fx[length(CDF_Data_Params$fx)] <- 0
  CDF_Data_Params$fx[1] <- 0
  den <- approxfun(CDF_Data_Params$x, CDF_Data_Params$fx, yleft=0, yright=0) #density function
  
  return(den)
}


integrate_pdf <- function(den, CDF_Data_Params, scale = F) {
  CDF_Data_Params$cdfFITYout <- c()
  for(val in CDF_Data_Params$x) {
    v<-try(integrate(den, min(CDF_Data_Params$x), val)$value)
    if (class(v)=="try-error") {
      v <- NA
    }
    CDF_Data_Params$cdfFITYout <- append(CDF_Data_Params$cdfFITYout,v)
  }
  if(scale){
    CDF_Data_Params$cdfFITYout <- CDF_Data_Params$cdfFITYout/max(CDF_Data_Params$cdfFITYout)
  }
  if(abs(max(CDF_Data_Params$cdfFITYout) -1) > 0.01 & !scale){
      warning("estimated density is improper (integrates to ",max(CDF_Data_Params$cdfFITYout), ". Check create_pdf() and integrate_pdf()" )  
  }
  
  return(CDF_Data_Params)
} 



invert_cdfs <- function(CDF_Data_Params, binwidth = 0.001){
  stop("Not yet adapted for USHD")
  #' @description approximates the inverse functions of the data_CDF and fit_CDF, 
  #' and calculates the x's at equally spaced y's to allow for tail method1 and method2 goodness-of-fit calculations.
  #' The equally spaced y-s and corresponding xs for the data_cdf and fit_cdf are stored in the object CDF_Data_Params    
  #' @param CDF_Data_Params 
  #' @param binwidth the size of each equally spaced bin between 0-1. Must be between 0-1. E.g. binwidth of 0.001 leads to 1000 equally spaced bins between 0-1 
  #' @return CDF_Data_Params with three additional list objects: 
  #' 1) CDF_Data_Params$cdfYout_tails - equally spaced Y's between 0-1 (e.g. 0.001, 0.002, 0.003...0.999)
  #' 2) CDF_Data_Params$cdfFITX_tails - corresponding X's of the fit_CDF at the values of cdfY_tails 
  #' 3) CDF_Data_Params$cdfDATAX_tails - corresponding X's of the data_cdf at the values of cdfY_tails
  
  # Must start the equally spaced Y-values after 0 and end at the maximum where the original cdFIT ends  
  min_y = binwidth
  max_y = max(CDF_Data_Params$cdfFITYout)
  CDF_Data_Params$cdfYout_tails <- seq(min_y,max_y,by=binwidth)
  
  # yleft needs to be very small and non-zero
  # yright needs to be almost but not one

  # ----- Fit CDF
  # Approximate cdf function 
  cdf_fit_func <- approxfun(x = CDF_Data_Params$x,
                            y = CDF_Data_Params$cdfFITYout, 
                            yleft = 0.00000001, yright = 0.99999999,
                            rule = 2)
  
  # Approximate the inverse of the cdf function
  inv_cdf_fit_func <- inverse(cdf_fit_func, lower = min(CDF_Data_Params$x)-1, upper = max(CDF_Data_Params$x)+1)
  
  # Find the corresponding X's for the cdfFIT, given the equally spaced Y's
  CDF_Data_Params$cdfFITX_tails <- unlist(lapply(CDF_Data_Params$cdfYout_tails, inv_cdf_fit_func))
  
  # ----- Data CDF
  # Approximate cdf functions
  cdf_data_func <- approxfun(x = CDF_Data_Params$x, 
                             y = CDF_Data_Params$cdfDATAYout, yleft = 0.00000001, yright = 0.99999999, rule = 2)
  
  # Approximate the inverse of the cdf function
  inv_cdf_data_func <- inverse(cdf_data_func, lower = min(CDF_Data_Params$x)-1, upper = max(CDF_Data_Params$x)+1)
  
  # Find the corresponding X's for the cdfDATA, given the equally spaced Y's
  CDF_Data_Params$cdfDATAX_tails <- unlist(lapply(CDF_Data_Params$cdfYout_tails, inv_cdf_data_func))
  
  return(CDF_Data_Params)
  
}

goodness_of_fit_calc <- function(Data, allData, CDF_Data_Params, weights, option, low_threshold, medium_threshold, high_threshold, low_tail, middle_tail, high_tail, low_weight, medium_weight, high_weight) {
  if (option == "thresholds_5") { # adding a weighted sum of the SQUARED differences at the three thresholds
    
    x_minus_3 <- which.min(abs(CDF_Data_Params$x - (offset + low_threshold))) # index closest to low threshold
    x_minus_2 <- which.min(abs(CDF_Data_Params$x - (offset + medium_threshold))) # index closet to medium threshold
    x_minus_1 <- which.min(abs(CDF_Data_Params$x - (offset + high_threshold))) # index closest to high threshold
    
    if ((CDF_Data_Params$x[x_minus_3] > min(Data)) & (CDF_Data_Params$x[x_minus_3] < max(Data))) {
      
      # calc absolute difference in the CDF at the threshold in real data vs estimated dist
      diff_min_x_minus_3 <- abs(CDF_Data_Params$cdfDATAYout[x_minus_3]-CDF_Data_Params$cdfFITYout[x_minus_3])
      
    } else {
      diff_min_x_minus_3 <- 0
    }
    
    
    
    if ((CDF_Data_Params$x[x_minus_2] > min(Data)) & (CDF_Data_Params$x[x_minus_2] < max(Data))) {
      
      diff_min_x_minus_2 <- abs(CDF_Data_Params$cdfDATAYout[x_minus_2]-CDF_Data_Params$cdfFITYout[x_minus_2])
      
    } else {
      diff_min_x_minus_2 <- 0
    }
    
    
    if ((CDF_Data_Params$x[x_minus_1] > min(Data)) & (CDF_Data_Params$x[x_minus_1] < max(Data))) {
      
      diff_min_x_minus_1 <- abs(CDF_Data_Params$cdfDATAYout[x_minus_1]-CDF_Data_Params$cdfFITYout[x_minus_1])
      
    } else {
      diff_min_x_minus_1 <- 0
    }

    
    weighted_ts_sum <- sum(
      c(diff_min_x_minus_3, diff_min_x_minus_2, diff_min_x_minus_1)^2 * 
        c(low_weight, medium_weight , high_weight) #these weights are for low, medium, and high threshold
      ) 
    
    if (weighted_ts_sum > 0) {
    
      weightfit <- list()
      weightfit$ks <- weighted_ts_sum
      weightfit$weights <- weights
      weightfit$distnames <- names(distlist)
      
      return(weightfit)
    
    } else if (weighted_ts_sum == 0) {
      
      xloc <- which.max(abs(CDF_Data_Params$cdfDATAYout-CDF_Data_Params$cdfFITYout))
      ks <- abs(CDF_Data_Params$cdfDATAYout[xloc]-CDF_Data_Params$cdfFITYout[xloc])
      y0 <- CDF_Data_Params$cdfDATAYout[xloc]
      y1 <- CDF_Data_Params$cdfFITYout[xloc]
      xPOSITION <- CDF_Data_Params$x[which(CDF_Data_Params$cdfDATAYout == y0)[1]]
      weightfit <- list()
      weightfit$ks <- ks
      weightfit$weights <- weights
      weightfit$distnames <- names(distlist)
      stop("check if this is ever used")
      return(weightfit)
      
    }
    
  } else{
    message("An invalid option: ", option, " was entered")
  }
}

iteration_weights_ks_tracking <- function(out_dir, optim_type, initial_condition, weightfit, nid_loc_yr_i) {
  
  files <- list.files(paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition))
  iteration = 1 + max(as.integer(unlist(lapply(files, function(f) { gsub(x = f, pattern = ".csv", replacement = "") } ))))
  write.csv(data.table(distname = weightfit$distnames, weights = weightfit$weights, ks = weightfit$ks, it.time = gsub("-", "_", Sys.time())), paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition, "/", iteration, ".csv"), row.names = F)
  
}  

global_ks_tracking <- function(allData, sum_KSs) {   
  for(nid_loc_yr_i in unique(allData$nid_loc_yr_index)) {
    files <- list.files(paste0(out_dir, "/", nid_loc_yr_i,"/", initial_condition))
    iteration = max(as.integer(unlist(lapply(files, function(f) { gsub(x = f, pattern = ".csv", replacement = "") } ))))
    it.data <- fread(paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition, "/", iteration, ".csv"))
    it.data[, globalks := sum_KSs]
    write.csv(it.data,paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition, "/", iteration, ".csv"), row.names = F)
  }
}

write_zero_csv <- function(out_dir, optim_type, initial_condition) {
  if(strategy == "m1"){
    dir.create(paste0(out_dir, "/", nid_loc_yr_i), showWarnings = F)
    unlink(x = paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition), recursive = T)
    dir.create(paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition), recursive = T, showWarnings = F)
    write.csv(data.table(), paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition, "/0.csv"))
  }
  if(strategy == "m2"){
    for(nid_loc_yr_i in unique(allData$nid_loc_yr_index)) {
      dir.create(paste0(out_dir, "/", nid_loc_yr_i), showWarnings = F)
      unlink(x = paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition), recursive = T)
      dir.create(paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition), recursive = T, , showWarnings = F)
      write.csv(data.table(), paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition, "/0.csv"))
    }
  }
}



combine_fit <- function(KSs) {
  sum_KSs<- sum(KSs)
  return(sum_KSs)
}


aggregate_iterations <- function(out_dir, nid_loc_yr_i, optim_type, initial_condition, option, launch.date){
  
  files <- list.files(paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition))
  max_iteration = max(as.integer(unlist(lapply(files, function(f) { gsub(x = f, pattern = ".csv", replacement = "") } ))))
  
  dat <- lapply(1:max_iteration, function(iteration){
    
    weights.i <- fread(paste0(out_dir, "/", nid_loc_yr_i, "/", initial_condition, "/", iteration, ".csv")) 
    weights.i[, i := iteration]
    weights.i[, nid_loc_yr_i := nid_loc_yr_i]
    weights.i[, initial_condition := initial_condition]
    weights.i[, option := option]
    weights.i[, iteration_time := it.time]
    
    return(weights.i)
    
  })  %>% rbindlist()
  
  dat[, initial_condition := initial_condition]
  dir.create(paste0(out_dir,"/", nid_loc_yr_i, "/aggregates/"), showWarnings = F)
  dir.create(paste0(out_dir,"/", nid_loc_yr_i, "/aggregates/"), showWarnings = F)
  saveRDS(dat, paste0(out_dir, "/", nid_loc_yr_i, "/aggregates/", initial_condition, ".rds"))
  
}


process_best_weights <- function(me_type, strategy, option, launch.date, test_additional_dist){
  
  source(paste0("FILEPATH")) # updated functions to solve for dist parameters
  
  run_dir <- paste0("FILEPATH")
  
  distlist <- c(classA, classM) 
  if(exists("test_additional_dist") && test_additional_dist){
    if(!("invweibull" %in% names(distlist))){ # skip if invweibull and beta are already in distlist --- this will happen if we load the USHD PDF families version instead of the one in the ensemble repo
      distlist <- append(distlist,  list(invweibull = invweibullOBJ)) #, glnorm = glnormOBJ))   glnorm does not have mv2par defined  
    }
    if(!"betasr" %in% names(distlist)){
      distlist <- append(distlist,  list(betasr = betasrOBJ))
    }
    
  } else{
    if("invweibull" %in% names(distlist)) {
      distlist$invweibull <- NULL # if we load distribution classes from the USHD pdf_families script, we automatically get invweibull
      distlist$betasr <- NULL
    }
  }
  
  # remove exp distribution b/c we cannot optimize variance (sd = mean)
  distlist$exp <- NULL
  # remove glnorm
  distlist$glnorm <- NULL
  
  if(me_type %in% c("bw", "ga")){
    distlist$invgamma <- NULL
    distlist$llogis <- NULL
  }
  
  # note that it's okay that this only reads in the files corresponding the first nid_loc_yr b/c the globalks value is updated across all nids, etc (b/c it's the sum across all sources)
  all.ic.fp <- paste0(run_dir, "1/aggregates/")
  all.ic.files <- list.files(all.ic.fp)
  
  all.ic.weights <- lapply(all.ic.files, function(file){
    single.ic <- readRDS(file.path(all.ic.fp, file))
    return(single.ic)
  }) %>% rbindlist(fill = T, use.names = T)
  
  all.ic.weights[ ,c("it.time", "nid_loc_yr_i") := NULL]
  
  best.ic.it <- all.ic.weights[globalks == min(globalks), ]
  best.ic.it <- best.ic.it[1:length(names(distlist))]
  stopifnot(uniqueN(best.ic.it$initial_condition) == 1)
  stopifnot(length(distlist) == uniqueN(all.ic.weights$distname)) # make sure that the distlist loaded matches the lists of dist optimized over
  
  best.weights = transpose(data.table(best.ic.it$weights))
  names(best.weights) <- names(distlist)
  
  dir.create(paste0(run_dir, "best_weights/"), showWarnings = F)
  
  write.csv(best.weights, paste0(run_dir, "best_weights/bestweights.csv"), row.names = F)
}



select_best_weights <- function(me_type, launch.date, option, new.name){
  run_dir <- paste0("FILEPATH")
  weights_dir <- paste0("FILEPATH")
  
  today.date = gsub("-", "_", Sys.Date())
  
  weight.set <- fread(paste0(run_dir, "best_weights/bestweights.csv"))
  
  existing.files <- file.exists(paste0(weights_dir, "selected_weights/"), recursive = TRUE)
  
  if(!existing.file){
    dir.create(paste0(weights_dir, "/selected_weights"), recursive = T)
  }
  
  write.csv(weight.set, paste0(weights_dir, new.name, ".csv"), row.names = F)
  
}










