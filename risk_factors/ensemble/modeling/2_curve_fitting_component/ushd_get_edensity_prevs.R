####################################################################################################
## Description: Solve for the prevalence of overweight and obesity implied by the
##                ensemble distribution. 
##        
## Inputs:  weights (ensemble weights)
##          mean = population mean
##          sd = population standard deviation
##         .min defines user set minimum x value
##         .max defines user maximum x value
##          scale=TRUE rescales the density so that it integrates to 1 across the defined support
##         .length -- deprecated 
##
##
## Output:  Returns a named list where over, obese, and under are the prevalence of 
##            overweight, obesity, and underweight in the population, respectively.
####################################################################################################

# set up ------------------------------------------------------------------

get_edensity_prevs <- function(weights, mean, sd, .min, .max, scale = TRUE) {
  library(data.table)
  # dlist is the universe of distribution families as defined in the above code
  #  classA is the majority of families
  #  classB is scaled beta
  #  classM is the mirror family of distributions
  
  W_ <- as.data.table(weights)
  M_ <- mean
  S_ <- sd
  
  mu <- log(M_/sqrt(1 + (S_^2/(M_^2))))
  sdlog <- sqrt(log(1 + (S_^2/M_^2)))

  XMIN <- qlnorm(0.001, mu, sdlog)
  XMAX <- qlnorm(0.999, mu, sdlog)

  if (missing(.min) | missing(.max)) {
    .min <- XMIN
    .max <- XMAX
  }
  
  # remove 0s or weights that are approximately zero
  W_ <- data.table(W_)
  W_ <- W_[1, W_ > 1e-6, with=FALSE]
  
  
  buildDENlist <- function(jjj) {
    distn = names(W_)[[jjj]]
    
    DIST_FUN <- dlist[paste0(distn)][[1]]
    
    EST <- NULL
    LENGTH <- length(formals(unlist(DIST_FUN$mv2par)))
    if (LENGTH == 4) {
      EST <- try(unlist(DIST_FUN$mv2par(M_, (S_^2),
                                        XMIN = XMIN,
                                        XMAX = XMAX)),
                 silent = T)
    } else {
      EST <- try(unlist(DIST_FUN$mv2par(M_, (S_^2))),
                 silent = T)
    }
    
    if(class(EST) != "numeric"){
      return(list(dPREV = c(0,0,0), weight = 0, cdf_over_support = 1))
      # note that the prevs will be NaN if all dist have issue & scale is TRUE b/c 
      # rescaling (dividing) by weights of zero is undefined
    }
    
    prevs <- list(uw = NULL, over = NULL, obese = NULL)
    cdf_min <- DIST_FUN$tailF(EST, .min, lt = T)
    cdf_max <- DIST_FUN$tailF(EST, .max, lt = T)
    prevs$uw <- DIST_FUN$tailF(EST, 18.5, lt = T)
    prevs$over <- DIST_FUN$tailF(EST, 25, lt = T)
    prevs$obese <- DIST_FUN$tailF(EST, 30, lt = T)
    prevs <- unlist(prevs) - c(cdf_min,
                               cdf_max,
                               cdf_max)
    prevs <- prevs * c(1, -1, -1)    
    cdf_over_support <- cdf_max - cdf_min
    
    if (class(EST) == "numeric" & all(is.finite(prevs))) {
      dEST <- EST
      dPREV <- unlist(prevs)
      weight <- W_[[jjj]]
    } else {
      dEST <- 0
      dPREV <- c(0,0,0)
      weight <- 0
      cdf_over_support <- 1
    }
    return(list(dPREV = dPREV, weight = weight, cdf_over_support = cdf_over_support))
  }
  prevOUT <- lapply(1:length(W_), buildDENlist)
  
  ## re-adjust weights in case any distributions dropped
  TW = unlist(lapply(prevOUT, `[[`, "weight"))
  if(sum(TW, na.rm = T) != 0){ # skip scaling weights if all are zero (happens when all dists fail to fit) to avoid NaN
    TW = TW/sum(TW, na.rm = T)  
  }
  
  
  ## retrieve the vector of re-scaling factors for each component distribution
  ##  i.e., CDF(upper bound) - CDF(lower bound)
  if(scale){
    SC = unlist(lapply(prevOUT, `[[`, "cdf_over_support"))  
  } else{
    SC = rep(1, length(prevOUT))
  }
  
  prevOUT <- lapply(prevOUT, `[[`, "dPREV")
  
  ## build ensemble prevs, weight, and rescale
  prevs <- as.list(Reduce("+", lapply(1:length(TW), function(jjj) unlist(prevOUT[[jjj]]) * TW[jjj] / SC[jjj])))
  names(prevs) <- c("under", "over", "obese")
  return(prevs)
}

get_edensity_prevs <- compiler::cmpfun(get_edensity_prevs)
