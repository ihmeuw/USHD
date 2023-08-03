## Purpose: modeling functions used across scripts


#' Encodes / indexes race and edu columns, starting from zero
#'
#' @param data
#'
#' @return data
#' @export
#'
#' @examples data <- index_edu_race(data)
index_edu_race <- function(data) {
  
  if("sex_fit" %in% names(data)) {
    data <- data[sex_fit == get("sex", .GlobalEnv), ]
  } else {
    data <- data[sex == get("sex", .GlobalEnv), ]
  }


  if(!race_together) {
    data <- data[race == get("race", .GlobalEnv)]
  }

  data[, race := as.integer(as.factor(race)) - 1]

  if(!edu_together) {
    data <- data[edu == get("edu", .GlobalEnv)]
  }
  # same for education
  data[, edu := as.integer(as.factor(edu)) - 1]

  data[, int := 1L]

  if (!nrow(data) > 0) {
    stop("data now has zero rows!")
  }

  return(data)
}


#' Messages / prints out all the settings arguments relevant to a specific model
#'
#' @return Nothing
#' @export
#'
#' @examples message_fit_args() # print the arguments
message_fit_args <- function() {
  cat(
    glue::glue(
      "Settings are:",
      "dir = '{dir}'",
      "sex = {sex}",
      "race = {race}",
      "edu = {edu}",
      "\n",
      .sep = "\n"
    )
  )
}


#' Counts number of unique values columns of data: mcnty, year, age, edu, race
#'
#' @param data
#'
#' @return list(num_j, num_t, num_a, num_e, num_r)
#' @export
#'
#' @examples counts <- count_variables(data)
#' for(i in unique(1:length(counts))) {
#'   assign(names(counts[i]), counts[[i]])
#' }
count_variables <- function(data) {

  num_j <- max(data$area) + 1
  num_t <- max(data$year) + 1
  num_a <- max(data$age) + 1
  # the number of races and educations should work since we are indexing race and education first
  num_e <- max(data$edu) + 1
  num_r <- max(data$race) + 1

  # above function always makes both num_r and num_e
  if (by_race) {
    num_subpop <- num_r
  } else if (by_edu) {
    num_subpop <- num_e
  } else {
    num_subpop <- 1
  }

  if (num_subpop != max(num_r, num_e)) {
    stop(glue::glue("setting num_subpop didn't work. num_subpop = {num_subpop}, num_r = {num_r}, num_e = {num_e}"))
  }
  return(list(num_j = num_j, num_t = num_t, num_a = num_a, num_e = num_e, num_r = num_r, num_subpop = num_subpop))

}


#' Create prior_list
#'
#' @param x
#'
#' @return prior_list
#' @export
#'
#' @examples # read in the priors
#' if(is.null(prior_type)){
#'   prior_type <- "loggamma"
#'   prior_list <- default_priors(3)
#' }
default_priors <- function(x) {
  prior_list <- list()
  for(i in unique(1:x)) {
    prior_list[[i]] <- c(1,1000,-3)
  }

  return(prior_list)
}


#' Generates variables related to time spline
#'
#' @param years
#' @param year_knots_num
#'
#' @return list(s_year, graph_ts, num_year_spline)
#' @export
#'
#' @examples   time_spline_info <- build_time_spline(years, year_knots_num)
#' s_year <- time_spline_info[[1]]
#' graph_ts <- time_spline_info[[2]]
#' num_year_spline <- time_spline_info[[3]]``
build_time_spline <- function(years, year_knots_num) {

  year_knots <- seq(1, length(years), length.out = year_knots_num) - 1
  s_year <- as.matrix(as.data.table(bs((1:num_t) - 1, knots = year_knots[1:(length(year_knots) - 1)], degree = 1, intercept = F)))
  num_year_spline <- ncol(s_year)
  graph_ts <- matrix(rep(0, num_year_spline^2), nrow = num_year_spline, ncol = num_year_spline)
  graph_ts[abs(row(graph_ts) - col(graph_ts)) == 1] <- 1
  graph_ts <- diag(apply(graph_ts, 1, sum)) - graph_ts
  graph_ts <- as(graph_ts, "dgTMatrix")

  return(list(s_year, graph_ts, num_year_spline))

}


#' Generates variables related to age spline
#'
#' @param ages
#' @param age_knots_spec
#'
#' @return list(s_age, graph_as, num_age_spline)
#' @export
#'
#' @examples
#' age_spline_info <- build_age_spline(ages, age_knots_spec)
#' s_age <- age_spline_info[[1]]
#' graph_as <- age_spline_info[[2]]
#' num_age_spline <- age_spline_info[[3]]
build_age_spline <- function(ages, age_knots_spec) {

  age_knots <- which(ages %in% age_knots_spec) - 1
  s_age <- as.matrix(as.data.table(bs((1:num_a) - 1, knots = age_knots[1:(length(age_knots) - 1)],degree = 1, intercept = F)))
  num_age_spline <- ncol(s_age)
  graph_as <- matrix(rep(0, num_age_spline^2), nrow = num_age_spline, ncol = num_age_spline)
  graph_as[abs(row(graph_as) - col(graph_as)) == 1] <- 1
  graph_as <- diag(apply(graph_as, 1, sum)) - graph_as
  graph_as <- as(graph_as, "dgTMatrix")

  return(list(s_age, graph_as, num_age_spline))

}


#' Takes values from a list of hyperprior parameters and puts them into named variables, e.g.,
#' re1_par1, re1_par2, re1_log_sigma. Creates three things: reX_par1, reX_par2, and an
#' reX_log_sigma, for each item in the list. Can be used on settings prior_list. Does not return
#' anything, instead, it assigns (creates) new variables in the parent scope. That is, the
#' variables appear in the scope above this function.
#'
#' @param hyperpriors_parameters_list: list of vectors, where each vector is the set of parameters
#'
#' @return None
#' @examples
#'
#' gen_hyperpriors_parameters(hyperpriors_parameters_list = prior_list)
#'
#' gen_hyperpriors_parameters(hyperpriors_parameters_list = list(c(5,0.05,-5), c(5,0.05,-5),c(5,0.05,-5)))
#'
gen_hyperpriors_parameters <- function(hyperpriors_parameters_list) {
  prior_types <- c("pc","loggamma", "half_normal")
  if (!(prior_type %in% prior_types)) {
    stop(glue::glue("prior_type must be one of {prior_types}, but it is {prior_type}."))
  }
  for (j in 1:length(hyperpriors_parameters_list)) {
    assign(paste0("re",j,"_par1"), hyperpriors_parameters_list[[j]][1], pos = parent.frame()) # parent.frame() gets the scope above this function
    assign(paste0("re",j,"_par2"), hyperpriors_parameters_list[[j]][2], pos = parent.frame())
    assign(paste0("re",j,"_log_sigma"), hyperpriors_parameters_list[[j]][3], pos = parent.frame())
  }
}

#' Create prior params, part of tmb_data, to be used with each covariate related random effect.
#'
#' @param prior_type from settings.csv, a Character that determines the prior type to be used on the standard deviation terms
#' @param covar_subpop_hyperpriors_settings from settings.csv a list containing re_cov_par1, re_cov_par2, re_cov_log_sigma
#'
#' @return list containing type, par1, and par2
#' @examples my_list = gen_covar_subpop_hyperpriors_parameters()
gen_covar_subpop_hyperpriors_parameters <- function(prior_type, covar_subpop_hyperpriors_settings) {
  return(list(type = prior_type, par1 = covar_subpop_hyperpriors_settings$re_cov_par1, par2 = covar_subpop_hyperpriors_settings$re_cov_par2))
}


#' Create a matrix with length(covars_subpop) columns and num_subpop rows of zeros to be used as a
#' container of Random Effects for covariates. Part of tmb_par. Here, 're' means 'Random Effect'
#'
#' Replaces this code:
#' tmb_par <- list(re4 = rep(0, num_subpop), re5 = rep(0, num_subpop), ...)
#'
#' @param num_subpop
#'
#' @return Matrix containing a column for each covariate related random effect.
gen_covar_subpop_re_matrix <- function(num_subpop) {
  temp = matrix(0, nrow = num_subpop, ncol = length(covars_subpop))
  return(temp)
}


#' Create the log sigmas for covariates for tmb_par. Function assumes that they are already exist
#' in parent scope.
#'
#' Replaces this code:
#' tmb_par <- list(
#'   re4_log_sigma = re4_log_sigma,
#'   re5_log_sigma = re5_log_sigma,
#'   re6_log_sigma = re6_log_sigma
#' )
#'
#' @param covar_subpop_hyperpriors_settings from settings.csv a list containing re_cov_par1, re_cov_par2, re_cov_log_sigma
#'
#' @return vector
gen_covar_subpop_log_sigmas <- function(covar_subpop_hyperpriors_settings) {

  temp_vect <- c()

  for (j in 1:length(covars_subpop)) {
    temp_vect <- c(temp_vect, covar_subpop_hyperpriors_settings$re_cov_log_sigma)
  } # end loop

  if (length(temp_vect) != length(covars_subpop)) {
    stop(glue::glue("temp_vect is wrong length. Should be {length(covars_subpop)} but it's {length(temp_vect)}."))
  }
  if (any(is.na(temp_vect))) {
    stop("temp_vect has NA values.")
  }
  return(temp_vect)
}

