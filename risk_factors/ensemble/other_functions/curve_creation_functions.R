# Curve Creation Functions

source(paste0("FILEPATH/ushd_pdf_families.R")) # updated functions to solve for dist parameters
source(paste0("FILEPATH/1_parallel_helper_functions.R"))
library(stats)
distlist <- c(classA, classM) 
distlist <- append(distlist,  list(invweibull = invweibullOBJ, betasr = betasrOBJ))


create_pdf_points <- function(den, CDF_Data_Params) {
  CDF_Data_Params$cdfFITYout <- c()
  for(val in CDF_Data_Params$x) {
    v<-den(val)
    CDF_Data_Params$cdfFITYout <- append(CDF_Data_Params$cdfFITYout,v)
  }
  
  
  return(CDF_Data_Params)
} 






# THIS CREATES AN ENSEMBLE CDF AND PDF FROM MICRODATA


data_to_curves <- function(Data, svy_wt, distlist, weights, nid_loc_yr_i, use_gbd_edensity = F) {
  stopifnot(is.list(weights))
  cdf_data_params <- create_CDF(Data,nid_loc_yr_i,svy_wt)
  XMAX <<- cdf_data_params$XMAX
  XMIN <<- cdf_data_params$XMIN
  
  if(use_gbd_edensity){
    # add additional distributions defined in the PAF code (and used in GBD weights), but not used in the code that lives in ensemble repo
    dlist <- append(distlist,  list(invweibull = invweibullOBJ, betasr = betasrOBJ, glnorm = glnormOBJ)) 
    dlist <- dlist[!duplicated(names(dlist))]
    names(dlist)
    names(weights)
    dlist <- dlist[names(weights)]
    den <- create_pdf(dlist, weights, CDF_Data_Params = cdf_data_params)
    rm(dlist) # avoid confusion
  } else{
    den <- create_pdf(distlist, weights, CDF_Data_Params = cdf_data_params)  
  }
  
  pdf.output <- create_pdf_points(den, CDF_Data_Params = cdf_data_params)
  
  pdf.data = data.table(x = density(Data,  n = 1024,  from = XMIN, to = XMAX, weights = svy_wt/sum(svy_wt))$x, y = density(Data,  n = 1024,  from = XMIN, to = XMAX)$y)
  pdf.data = pdf.data[sample(nrow(pdf.data), 1000),]
  pdf.data = pdf.data[order(x),]
  
  
  cdf <- integrate_pdf(den, cdf_data_params, scale = T)
  
  ensemble.info <- data.table(xpoints = cdf$x, cdf.model.points = cdf$cdfFITYout, cdf.data.points = cdf$cdfDATAYout, pdf.model.points = pdf.output$cdfFITYout, pdf.data.points = pdf.data$y, nid_loc_yr_i = cdf$nid)
  return(ensemble.info)
}


# THIS CREATES AN ENSEMBLE CDF AND PDF WHEN YOU HAVE AN INPUT MEAN/SD


# THE MEAN THAT YOU INPUT TO THIS FUNCTION NEEDS TO BE THE MEAN IN CORRECT SPACE, MEANING VALUES OF -6 TO 6

calculate_curve_know_mean_sd <- function(nid_loc_yr_i, mean_val, sd_val, bottom_cutoff, top_cutoff, weights, offset) {
  CDF_Data_Params <- list()
  CDF_Data_Params$mn <- mean_val + offset
  CDF_Data_Params$variance <- sd_val^2
  CDF_Data_Params$XMIN <- bottom_cutoff + offset
  CDF_Data_Params$XMAX <- top_cutoff + offset
  CDF_Data_Params$x <- seq(CDF_Data_Params$XMIN, CDF_Data_Params$XMAX, length = 1000)
  CDF_Data_Params$fx <- 0*CDF_Data_Params$x
  CDF_Data_Params$nid <- nid_loc_yr_i

  
  den <- create_pdf(distlist, weights, CDF_Data_Params)
  
  pdf <- create_pdf_points(den, CDF_Data_Params)
  
  cdf <- integrate_pdf(den, CDF_Data_Params, scale = T)
  
  
  
  ensemble.pdf.info <- data.table(xpoints = pdf$x - offset, pdf.model.points = pdf$cdfFITYout, cdf.model.points = cdf$cdfFITYout, nid_loc_yr_i = pdf$nid)
  
  
  
  return(ensemble.pdf.info)
}

















