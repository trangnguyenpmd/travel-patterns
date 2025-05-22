#============================================#
#==== COVID-19 in EU/EEA 2020 - 2022 ========#
#============================================#


### MANUSCRIPT
### GLOBAL MOBILITY FLOWS AND COVID-19 SPREAD IN EUROPE DURING THE EMERGENCY PHASE: INSIGHTS FROM FACEBOOK DATA

### Authors:
### Thi Huyen Trang Nguyen (1), Niel Hens (1,2), Christel Faes (1)

### Affiliation:
### (1) Data Science Institute, I-BioStat, Hasselt University, BE-3500 Hasselt, Belgium
### (2) Centre for Health Economic Research and Modelling Infectious Diseases, Vaccine and Infectious Disease Institute, University of Antwerp, BE-2000 Antwerpen, Belgium


# ====== GENERAL INFO ==========
### Document: FUNCTIONS
### Author: trangngpmd
### Date: 2024-10-08


# Functions in this file are copied from cran library and sourced codes from the following papers:
# 
# 1/Endemic-epidemic models with discrete-time serial interval distributions for infectious disease prediction
# PDF: https://doi.org/10.1016/j.ijforecast.2020.07.002
# Codes: https://github.com/jbracher/dengue_noro_rota
# 
# 2/ Multivariate endemic-epidemic models with higher-order lags and an application to outbreak detection
# PDF: https://arxiv.org/abs/1901.03090
# 
# 3/ Periodically stationary multivariate autoregressive models
# PDF: https://arxiv.org/abs/1707.04635
# Codes: https://github.com/jbracher/hhh4addon
# 
# 4/ Spatio-Temporal Analysis of Epidemic Phenomena Using the R Package surveillance
# PDF: https://www.jstatsoft.org/article/view/v077i11
# Codes: https://github.com/cran/surveillance/tree/master/R


# ------------------------------------------------------------------------------------#
# Check coefs -----
# ------------------------------------------------------------------------------------#
# Functions for check compatibility of a user-specified coefficient vector with model

checkCoefs <- function (object, coefs, reparamPsi=TRUE)
{
  theta <- coef(object, reparamPsi=reparamPsi)  #-> computes 1/exp(logpsi)
  if (length(coefs) != length(theta))
    stop(sQuote("coefs"), " must be of length ", length(theta))
  names(coefs) <- names(theta)
  coefs
}


# ----------------------------------------#
# Functions discrete Weibull weights -----
# ----------------------------------------#

# #' This function generates (shifted) discrete Weibull weights which are subsequently used inside of \code{get_weighted_lags}. To be passed
# #' to \code{hhh4_lag} or \code{profile_par_lag} as the \code{control$funct_lag} argument.
# #' @param par_lag a parameter vector of length 2 to steer the lag structure, here \eqn{logit(q)} and \eqn{log(beta)},
# #' where \eqn{q} and \eqn{beta} are the parameters of the discrete Weibull distribution as implemented in the `extraDistr` package.
# #' @param min_lag smallest lag to include; the support of the Poisson form starts only at \code{min_lag}. Defaults to 1.
# #' @param max_lag highest lag to include; higher lags are cut off and he remaining weights standardized. Defaults to 5.
# #' @author Maria Dunbar, Johannes Bracher
# #' @export
discrete_weibull_lag <- function(par_lag, min_lag, max_lag){
  if(length(par_lag) != 2){
    stop("The starting value for par_lag needs to be 2")
  }
  # ensure parameters are in the right domains:
  # first parameter (q) is a probability. Note: this is not the standard parameterization
  # described on Wikipedia.
  p_lag <- numeric(2)
  p_lag[1] <- exp(par_lag[1])/(1 + exp(par_lag[1]))
  # second parameter (beta) is a positive real number
  p_lag[2] <- exp(par_lag[2])
  
  # compute weights:
  weights0 <- c(rep(0, min_lag - 1),
                extraDistr::ddweibull(x = (min_lag : max_lag) - 1,
                                      shape1 = p_lag[1],
                                      shape2 = p_lag[2]))
  
  # standardize weights:
  weights <- weights0 / sum(weights0)
  return(weights)
}


# ----------------------------------------#
# Functions discrete gamma lag -----
# ----------------------------------------#
#' #' This function generates (shifted) discrete gamma weights which are subsequently used inside of \code{get_weighted_lags}. To be passed
#' #' to \code{hhh4_lag} or \code{profile_par_lag} as the \code{control$funct_lag} argument.
#' #' @param par_lag a parameter vector of length 2 to steer the lag structure, here \eqn{log(shape)} and \eqn{log(rate)},
#' #' where \eqn{shape} and \eqn{rate} are the parameters of the discrete gamma distribution as implemented in the \code{extraDistr} package.
#' #' @param min_lag smallest lag to include; the support of the Poisson form starts only at \code{min_lag}. Defaults to 1.
#' #' @param max_lag highest lag to include; higher lags are cut off and he remaining weights standardized. Defaults to 5.
#' #' @author Maria Dunbar, Johannes Bracher
#' #' @export
discrete_gamma_lag <- function(par_lag, min_lag, max_lag){
  # ensure parameters are in the right domains. Both are strictly positive
  p_lag <- exp(par_lag)
  # compute weights:
  # NB this implementation uses the inverse scale (rate) parameter
  weights0 <- c(rep(0, min_lag - 1),
                extraDistr::ddgamma((min_lag : max_lag) - 1,
                                    p_lag[1], p_lag[2],
                                    log = FALSE))
  # standardize weights:
  weights <- weights0 / sum(weights0)
  return(weights)
}

# ----------------------------------------#
# Functions discrete log normal weights -----
# ----------------------------------------#

#' This function generates discretized log-normal weights which are subsequently used inside of \code{get_weighted_lags}. To be passed
#' to \code{hhh4_lag} or \code{profile_par_lag} as the \code{control$funct_lag} argument.
#' @param par_lag a parameter vector of length 2 to steer the lag structure, here \eqn{meanlog} and \eqn{log(sdlog)},
#' where \eqn{meanlog} and \eqn{sdlog} are the parameters of the log-normal distribution.
#' @param min_lag smallest lag to include; the support of the Poisson form starts only at \code{min_lag}. Defaults to 1.
#' @param max_lag highest lag to include; higher lags are cut off and he remaining weights standardized. Defaults to 5.
#' @author Maria Dunbar, Johannes Bracher
#' @export
log_normal_lag <- function(par_lag, min_lag, max_lag){
  # ensure parameters are in the right domains. First is from R, second is strictly positive
  p_lag <- par_lag
  p_lag[2] <- exp(par_lag[2])
  # compute weights from increments of CDF:
  x_temp <- (min_lag - 1):max_lag
  weights0 <- c(rep(0, min_lag - 1),
                diff(plnorm(x_temp, p_lag[1], p_lag[2])))
  weights <- weights0 / sum(weights0)
  return(weights)
}
