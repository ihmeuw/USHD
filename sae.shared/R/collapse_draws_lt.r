#' @title collapse_draws_lt
#'
#' @description Define functions for collapsing mx, yll and lt draws:
#'              "collapse_draws_lt": Specific collapse function for lifetables
#'
#' @author      INDIVIDUAL_NAME
#'
#' @param draws - a data.table of draws of some variable.
#' @param dir [character] Root filepath for 'growth_rates.rds'
#' @param id_vars - id variables to collapse over, these also become the data.table keys.
#' @param education - if true, then calculate lifetables for adult ages only
#'
#' @return data.table with point estimates, confidence intervals, and standard errors for life tables
#'         for all specified id variables.
#'
#' @rdname collapse_draws
#' @export
collapse_draws_lt <- function(draws, dir, id_vars = c("level", "area", "year", "sex", "race", "edu", "age"), education = FALSE) {

  # collapse to get point estimates, CI, and standard errors
  est <- draws[, list(mx_mean = mean(mx), mx_lb = quantile(mx, 0.025, type = 5), mx_ub = quantile(mx, 0.975, type = 5), mx_se = sd(mx),
                      ax_mean = mean(ax), ax_lb = quantile(ax, 0.025, type = 5), ax_ub = quantile(ax, 0.975, type = 5), ax_se = sd(ax),
                      qx_lb = quantile(qx, 0.025, type = 5), qx_ub = quantile(qx, 0.975, type = 5), qx_se = sd(qx),
                      ex_lb = quantile(ex, 0.025, type = 5), ex_ub = quantile(ex, 0.975, type = 5), ex_se = sd(ex)),
               keyby = id_vars]
  rm(draws); gc()

  # for self-consistency, recalculate point estimates for qx and ex by applying life table methods
  # to point estimates of mx and ax
  id_vars2 <- setdiff(id_vars, c("age")) # remove age from id vars

  
  change_schema <- F
  if(settings$area_var %in% names(est)) {
    change_schema <- T
    setnames(est, settings$area_var, "area")
    est[, level := settings$area_var]
  }

  if(!("race" %in% names(est))) est[,race := race_default]

  est <- add_growth_rates(dir = dir, data = est,
                          idvars = c("year", "sex", "area", "level", "race", "edu"),
                          lt_hc = settings$lt_hc)

  if(change_schema) {
    # now set the schema back
    setnames(est, "area", settings$area_var)
    est[, level := NULL]
  }


  est[, c("qx_mean", "ex_mean", "ax_mean") := lifetable(mx = mx_mean, ax = ax_mean, use_graduation = F,
                                                        extrap = F, lt_hc = settings$lt_hc, gr = mean_gr, adult_ages_only = education)[, list(qx, ex, ax)],
      by = id_vars2]



  est[,mean_gr := NULL] # get rid of the growth rate because we no longer need it


  # fix a problem: if mx is so high that mx -> qx leads to qx > 1, ax is adjusted downward. Often
  # this happens on just one or two draws, leading to a lower and upper bound which are equal and a
  # mean estimate which is lower. In this case, update the mean estimate to equal the lower and
  # upper bounds
  est[abs(ax_ub - ax_lb) < 1e-10 & !data.table::between(ax_mean, ax_lb, ax_ub), ax_mean := (ax_lb + ax_ub)/2]

  # fix another problem: ex_mean can sometimes be outside the UI bounds when it is recalculated
  # this became an issue when switching to the Horiuchi-Coale method
  # so just set ex_mean to either ex_lb or ex_ub (whichever is relevant)
  est[ex_mean < ex_lb & abs(ex_mean - ex_lb) < 0.1, ex_lb := ex_mean]
  est[ex_mean > ex_ub & abs(ex_mean - ex_ub) < 0.1, ex_ub := ex_mean]
  est[ax_mean < ax_lb & abs(ax_mean - ax_lb) < 0.1, ax_lb := ax_mean]
  est[ax_mean > ax_ub & abs(ax_mean - ax_ub) < 0.1, ax_ub := ax_mean]

  # format and return
  setcolorder(est, c(id_vars, "mx_mean", "mx_lb", "mx_ub", "mx_se",
                     "ax_mean", "ax_lb", "ax_ub", "ax_se",
                     "qx_mean", "qx_lb", "qx_ub", "qx_se",
                     "ex_mean", "ex_lb", "ex_ub", "ex_se"))
  setkeyv(est, id_vars)
  est
}
