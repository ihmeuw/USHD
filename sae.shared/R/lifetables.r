####################################################################################################
## Description: Define functions for generating life tables. Only lifetable() is meant to be called
##              directly.
##
## Inputs:      mx [required] - a vector of mx values, corresponding to age groups 0, 1-4, and then
##                5-year age groups until the terminal age group.
##              ax [optional]- a vector of ax values, corresponding to the same age groups as the
##                mx vector
##              sex [optional] - a numeric, specifying sex (1 = male, 2 = female, 3 = both combined).
##                this is required if ax is not provided or if the Wang extrapolation will be used.
##              graduation [optional; default = T] - apply graduation to refine ax estimates.
##              extrap [optional; default = T] - apply Wang extrapolation (T) or calculate
##                life tables with the given terminal age group (F). In order to run the Wang
##                extrapolation, the terminal age group must be 85+
##              return_lt [optional; default = F] - return the entire life table (T), or just e0 (F)
##
## Outputs:     if return_lt = T, a data.table with the full life table.
##              if return_lt = F, a numeric scalar giving life expectancy at birth.
####################################################################################################

require(data.table)
require(boot)
require(zeallot)

## get_child_ax(): calculate ax for the infant (age 0) and childhood (ages 1-4) age groups using
## the Coale-Demeny values from the Preston demography book (table 3.3; page 48).
get_child_ax <- function(mx, sex) {
  if (sex == 1) {
    if (mx[1] < 0.107) ax <- c(0.045 + 2.684*mx[1], 1.651 - 2.816*mx[1])
    else ax <- c(0.330, 1.352)
    
  } else if (sex == 2) {
    if (mx[1] < 0.107) ax <- c(0.053 + 2.800*mx[1], 1.522 - 1.518*mx[1])
    else ax <- c(0.350, 1.361)
    
  } else if (sex == 3) {
    if (mx[1] < 0.107) ax <- c(0.049 + 2.742*mx[1], 1.587 - 2.167*mx[1])
    else ax <- c(0.340, 1.357)
  }
  return(ax)
}

#' Create a column called mean_gr (Mean growth rate)
#' If the settings specify that we use the Horuichi-Coale method, then we merge on growth rates. Otherwise, this column is NA
#' 
#' @param data data for which we need a mean_gr column
#' @param dir model directory
#' @param idvars variables used to merge on growth rates
#' @param lt_hc boolean that determines if we are using the H-C method
#' @return data with a mean_gr columns
add_growth_rates <- function(dir, data, idvars, lt_hc) {
  
  if(lt_hc) {
    growth_rate_data <- readRDS(paste0(dir,"/growth_rates.rds"))
    data <- merge(data, growth_rate_data, by=idvars, all.x=T)
    stopifnot(nrow(data[is.na(mean_gr)]) == 0)
  } else {
    data[,mean_gr := NA]
  }
  
  return(data)
  
}

## set_starting_ax(): set starting ax values, using get_child_ax() for ages < 5; using 1/mx for the
## terminal age group; and using 2.5 (midpoint) for all other age groups.
set_starting_ax <- function(mx, sex, x, lt_hc, alpha = NULL, beta = NULL, gr) {
  ax <- rep(2.5, x)
  ax[1:2] <- get_child_ax(mx, sex)
  
  if(lt_hc) {
    ax[x] <- (1/mx[x])*exp(-1*beta*unique(gr)*(mx[x]^(-1*alpha)))
  } else {
    ax[x] <- 1/mx[x]
  }
  
  return(ax)
}


## calc_qx(): calculate qx using the mx-to-qx conversion formula. optionally, in cases where this
## pushes qx over 1, adjust ax downward in an attempt to get qx back in a reasonable range.
calc_qx <- function(mx, ax, n, x, adjust_ax = F) {
  
  # mx-to-qx conversion
  qx <- (n * mx) / (1 + (n - ax) * mx)
  
  # reset terminal age group qx to 1
  qx[x] <- 1
  
  # check that the implied qx is not >= 1, and adjust ax and qx if necessary
  if (adjust_ax) {
    tobig <- which(qx > 1)
    
    while (length(tobig) > 0) {
      if (min(ax[tobig]) <= 0.1) {
        message("qx > 1 and ax is already close to 0")
        ax <- NA
        qx <- NA
      }
      ax[tobig] <- ax[tobig] - 0.1
      qx <- (n * mx) / (1 + (n - ax) * mx)
      tobig <- which(qx > 1)
    }
  }
  
  return(list(qx, ax))
}

## graduation(): apply graduate to generate refined estimates of ax.
graduation <- function(mx, ax, qx, n, x) {
  
  # calculate lx and dx
  lx <- c(1, cumprod(1 - qx)[-x])
  dx <- lx * qx
  
  # store original ax and qx
  original_ax <- ax
  original_qx <- qx
  
  # identify the age groups graduation will be applied to.
  # graduation only works for age groups where (n-1), n, and (n+1) all equal 5, so we have to skip
  # the first three age groups (because the first two are 0 and 1-4) and the last two age groups
  # (because the last age group is always the terminal age group).
  ii <- 4:(x - 2)
  
  # check that mx > 0. graduation fails if mx is 0, in which case we stick to the initial values
  if (sum(mx[ii] == 0) > 0) {
    message("mx is 0 in age groups, using original ax")
    return(list(ax, qx))
  }
  
  # iterate until graduation converges
  iter <- diff <- 1
  while (diff > 0.0001) {
    start_ax <- ax
    ax[ii] <- ((-5/24) * dx[ii - 1] + 2.5 * dx[ii] + (5/24) * dx[ii + 1])/dx[ii]
    ax[ii] <- pmin(n[ii], pmax(0.1, ax[ii])) # guarantee that ax stays in a reasonable range, i.e., (0 + 0.1, n - 0.1)
    qx <- (n * mx) / (1 + (n - ax) * mx)
    lx <- c(1, cumprod(1 - qx)[-x])
    dx <- lx*qx
    diff <- max(abs(start_ax[ii] - ax[ii]))
    iter <- iter + 1
    if (iter > 1000) { # if graduation isn't working, just use the original ax values
      message("graduation failed to converge, using original ax")
      ax <- original_ax
      qx <- original_qx
      break
    }
  }
  
  # check this hasn't done anything untoward to qx, and if so, revert to the original ax values.
  if (sum(qx[ii] > 1) > 0) {
    message("graduation causes qx > 1, using original ax")
    ax <- original_ax
    qx <- original_qx
  }
  
  # if graduation worked properly, return the modified ax and update qx as well
  return(list(ax, qx))
}

## wang_extrapolation(): apply the Wang extrapolation to extend the mx, ax, and qx vectors from
## terminal age group 85 to terminal age group 110+
wang_extrapolation <- function(mx, ax, qx, sex) {
  
  # extrapolate qx
  if (sex == 1) {
    par1 <- 0.61074
    par2 <- -0.1888774
    par3 <- c(0, 0.0854506, 0.1073827, 0.1362565, 0.0873758)
    
  } else if (sex == 2) {
    par1 <- 0.6261078
    par2 <- -0.1566213
    par3 <- c(0, 0.0457685, 0.128427, 0.1686179, 0.1229152)
    
  } else if (sex == 3) {
    par1 <- 0.6184239
    par2 <- -0.1727493
    par3 <- c(0, 0.06560955, 0.11790485, 0.15243720, 0.10514550)
  }
  
  diff <- par1 + par2 * logit(qx[18]) + par3
  qx <- c(qx[1:18], inv.logit(logit(qx[18]) + cumsum(diff)))
  
  # extrapolate ax
  if (sex == 1) {
    par1 <- c(2.876584, 1.000098, -0.5470469, -1.829395, -3.308708)
    par2 <- c(-0.7356333, 4.505065, 8.325985, 11.73745, 15.85796)
    par3 <- c(-0.3508558, -4.020929, -6.39603, -8.624996, -11.36632)
    
  } else if (sex == 2) {
    par1 <- c(3.020808, 1.61786, 0.1998074, -0.5838204, -1.593815)
    par2 <- c(-1.249447, 3.073897, 6.652864, 8.829251, 11.80925)
    par3 <- c(0.1314265, -3.187247, -5.435377, -6.917761, -9.000666)
    
  } else if (sex == 3) { # these are just the average of the male and female values.
    par1 <- c(2.9486960, 1.3089790, -0.1736198, -1.2066077, -2.4512615)
    par2 <- c(-0.9925401, 3.7894810, 7.4894245, 10.2833505, 13.8336050)
    par3 <- c(-0.1097146, -3.6040880, -5.9157035, -7.7713785, -10.1834930)
  }
  ax <- c(ax[1:18], par1 + par2 * qx[19:23] + par3 * qx[19:23]^2)
  
  # back-calculate mx from extrapolated qx and ax for age groups 80-84 through 100-105
  n <- c(1, 4, rep(5, 21))
  mx <- c(mx[1:17], qx[18:23] / (n[18:23] - (n[18:23] - ax[18:23]) * qx[18:23]))
  
  # add mx, ax, and qx for new 110+ terminal age group
  if (sex == 1) par1 <- 0.6865299 else par1 <- 0.6471901
  mx[24] <- exp(log(mx[23]) * par1)
  ax[24] <- 1/mx[24]
  qx[24] <- 1
  n[24] <- NA
  x <- 24
  
  # return the relevant life table quantities
  return(list(mx, ax, qx, n, x))
}


## lifetable(): calculate life tables from mx or mx and ax. Optionally, apply graduation and/or the
## Wang extrapolation and/or the Horiuchi-Coale method.
lifetable <- function(mx, ax = NULL, sex = NULL, graduation = T, extrap = T, lt_hc, gr = NULL, alpha=1.4, beta=0.095) {
  
  if (sum(is.na(mx)) > 0) return(as.numeric(NA))
  
  # get n
  x <- length(mx)
  n <- c(1, 4, rep(5, x - 2))
  
  # if not provided, set initial ax values for age groups 0 to 80-84
  if (is.null(ax)) ax <- set_starting_ax(mx, sex, x, lt_hc, alpha, beta, gr)
  
  # calculate qx
  c(qx, ax) %<-% calc_qx(mx, ax, n, x, adjust_ax = T)
  
  # use graduation to get improved ax estimates
  if(!(NA %in% ax)) {
    if (graduation) c(ax, qx) %<-% graduation(mx, ax, qx, n, x)
  }
  
  
  # extrapolate to move terminal age group from 85+ to 110+
  if (extrap == 1 & x == 19) {
    c(mx, ax, qx, n, x) %<-% wang_extrapolation(mx, ax, qx, sex)
  }
  
  # calculate the remainder of the life table
  qx[x] <- 1
  lx <- c(1, cumprod(1 - qx)[-x])
  dx <- lx * qx
  
  if(lt_hc) {
    Lx <- c(lx[-1] * n[-x] + (ax * dx)[-x], (1/mx[x])*exp(-1*beta*gr[x]*(mx[x]^(-1*alpha)))*dx[x])
  } else {
    Lx <- c(lx[-1] * n[-x] + (ax * dx)[-x], lx[x] / mx[x])
  }
  
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  n[x] <- NA
  ax[x] <- ex[x]
  
  # return results
  return(data.table(age = c(0, cumsum(n)[-x]), n, mx, ax, qx, lx, dx, Lx, Tx, ex))
}