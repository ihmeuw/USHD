# Function for KS test with weighted data
# Copy and pasted from https://github.com/cran/Ecume/blob/master/R/ks_test.R
#   One modification is changed import from spatstat.geom to spatstat.


.pKS2 <- function(x, n = length(x), tol) {
  # x[1:n] is input and output
  #
  #  Compute
  #    \sum_{k=-\infty}^\infty (-1)^k e^{-2 k^2 x^2}
  #    = 1 + 2 \sum_{k=1}^\infty (-1)^k e^{-2 k^2 x^2}
  #    = \frac{\sqrt{2\pi}}{x} \sum_{k=1}^\infty \exp(-(2k-1)^2\pi^2/(8x^2))
  #
  #    See e.g. J. Durbin (1973), Distribution Theory for Tests Based on the
  #  Sample Distribution Function.  SIAM.
  #
  #    The 'standard' series expansion obviously cannot be used close to 0;
  #  we use the alternative series for x < 1, and a rather crude estimate
  #  of the series remainder term in this case, in particular using that
  #  ue^(-lu^2) \le e^(-lu^2 + u) \le e^(-(l-1)u^2 - u^2+u) \le e^(-(l-1))
  #  provided that u and l are >= 1.
  #
  #    (But note that for reasonable tolerances, one could simply take 0 as
  #       the value for x < 0.2, and use the standard expansion otherwise.)
  #
  #   /
  #   double New, old, s, w, z;
  # int i, k, k_max;
  if (x == 0) {
    return(1)
  }
  k_max <- sqrt(2 - log(tol))
  if (x < 1) {
    z <- - pi^2/ (8 * x^2)
    w <- log(x)
    s <- seq(1, k_max, 2)
    s <- sum(exp(s^2 * z - w))
    return(s * sqrt(2 * pi))
  } else {
    z <- -2 * x^2
    s = -1; k = 1; old = 0; New = 1
    while(abs(old - New) > tol) {
      old <- New
      New <- New + 2 * s * exp(z * k * k);
      s <- -s
      k <- k + 1
    }
    return(New)
  }
}


#' Weighted KS Test
#'
#' @description Weighted Kolmogorov-Smirnov Two-Sample Test with threshold
#'
#' @param x Vector of values sampled from the first distribution
#' @param y Vector of values sampled from the second distribution
#' @param thresh The threshold needed to clear between the two cumulative distributions
#' @param w_x The observation weights for x
#' @param w_y The observation weights for y
#' @importFrom spatstat ewcdf
#' @examples
#'  x <- runif(100)
#'  y <- runif(100, min = .5, max = .5)
#'  ks_test(x, y, thresh = .001)
#' @details
#' The usual Kolmogorov-Smirnov test for two vectors **X** and **Y**, of size m
#' and n rely on the empirical cdfs \eqn{E_x} and \eqn{E_y} and the test statistic
#'  \deqn{D = sup_{t\in (X, Y)} |E_x(x) - E_y(x))}.
#' This modified Kolmogorov-Smirnov test relies on two modifications.
#' \itemize{
#'   \item Using observation weights for both vectors **X** and **Y**: Those
#' weights are used in two places, while modifying the usual KS test. First, the
#' empirical cdfs are updates to account for the weights. Secondly, the effective
#' sample sizes are also modified. This is inspired from
#' \url{https://stackoverflow.com/a/55664242/13768995}, using Monahan (2011).
#'
#'   \item Testing against a threshold: the test statistic is thresholded such
#'   that \eqn{D = max(D - thresh, 0)}. Since \eqn{0\le D\le 1}, the value of
#'   the threshold is also between 0 and 1, representing an effect size for the
#'   difference.
#' }
#' @md
#' @references
#' Monahan, J. (2011). _Numerical Methods of Statistics_ (2nd ed.,
#' Cambridge Series in Statistical and Probabilistic Mathematics). Cambridge:
#'  Cambridge University Press. doi:10.1017/CBO9780511977176
#' @return
#' A list with class \code{"htest"} containing the following components:
#' \itemize{
#'   \item *statistic* the value of the test statistic.
#'   \item *p.value* the p-value of the test.
#'   \item *alternative* a character string describing the alternative hypothesis.
#'   \item *method* a character string indicating what type of test was performed.
#'   \item *data.name* a character string giving the name(s) of the data.
#' }
#' @export
ks_test <-  function (x, y, thresh = .05, w_x = rep(1, length(x)),
                      w_y = rep(1, length(y))) {
  require(spatstat, quietly = T)
  # Inspired by https://stackoverflow.com/a/55664242/13768995
  DNAME <- base::deparse(substitute(x))
  DNAME <- paste(DNAME, "and", base::deparse(substitute(y)))
  w_x <- w_x[!is.na(x)]
  x <- x[!is.na(x)]
  x <- x[w_x > 0]
  w_x <- w_x[w_x > 0]
  w_x <- w_x / sum(w_x)
  n.x <- length(x)
  n.x <- as.double(n.x)
  if (n.x < 1L)  stop("not enough 'x' data")
  w_y <- w_y[!is.na(y)]
  y <- y[!is.na(y)]
  y <- y[w_y > 0]
  w_y <- w_y[w_y > 0]
  w_y <- w_y / sum(w_y)
  n.y <- length(y)
  if (n.y < 1L) stop("not enough 'y' data")
  n.x <- sum(w_x)^2/sum(w_x^2)
  n.y <- sum(w_y)^2/sum(w_y^2)
  w <- n.x * n.y / (n.x + n.y)
  x_ewcdf <- spatstat::ewcdf(x, weights = w_x)
  y_ewcdf <- spatstat::ewcdf(y, weights = w_y)
  xy <- c(x, y)
  STATISTIC <- max(abs(x_ewcdf(xy) - y_ewcdf(xy)) - thresh, 0)
  pkstwo <- function(x, tol = 1e-06) {
    if (is.numeric(x)) {
      x <- as.double(x)
    }
    else {
      stop("argument 'x' must be numeric")
    }
    if (is.na(x)) {
      return(NA)
    } else {
      return(.pKS2(x = x, tol = tol))
    }
  }
  if (STATISTIC == 0) {
    PVAL <- 1
  } else {
    PVAL <- min(1, max(2.2e-16, 1 - pkstwo(sqrt(w) * STATISTIC)))
  }
  
  METHOD <- paste0("Two-sample Weighted Kolmogorov-Smirnov test with threshold ", thresh)
  RVAL <- list(statistic = STATISTIC, p.value = PVAL, alternative = "two-sided",
               method = METHOD, data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}