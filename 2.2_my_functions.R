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


# ====== Function for waning immunity of vaccination ==========
# These two functions are adapted from the paper:
# Bekker-Nielsen Dunbar M, Held L.
# The COVID-19 vaccination campaign in Switzerland and its impact on disease spread. Epidemics. 2024;47:100745.
# Paper link: https://www.sciencedirect.com/science/article/pii/S1755436524000069
# The codes: https://github.com/mariabnd/ee-vax/blob/main/lib/function-waning.R


## Waning function -----
waning <- function(t, val){
  
  # Calculate downwards slope for waning time
  slope <- coef(lm(c(1, 0) ~ c(25, 38)))[[2]]
  adj <- ifelse(t <= 0, 0,
                ifelse(t == 1, 0.5,
                       ifelse(t > 1 & t <= 25, 1,
                              ifelse(t > 1 & t > 25 & t < 39, - 1 * (38 - t) * slope,
                                     ifelse(t > 1 & t >= 39, 0, 0)))))
  adj * val
}


## Apply waning to data -----
apply_waning <- function(data_set, FUN,
                         name = c("csum_waned", "cov_waned")){
  
  # Create empty variable for populating
  data_set$tmp_csum_waned <- NA
  
  # Calculate the waned sum for each country
  for (c in unique(data_set$country)){
    
    # Extract the data for the group
    df <- data_set[data_set$country == c, ]
    dm <- dim(df)
    
    # Create an empty (triangle) matrix to be populated with the waned values of entries
    mat <- matrix(rep(NA, times = dm[1] * dm[1]), ncol = dm[1])
    colnames(mat) <- df$date_week
    
    for (i in 1 : dm[1]){
      # extract the values at each date, calculate the waning at that date
      mat[i, ] <- FUN(1 : dm[1] - i, df$newdose[i])
    }
    
    # sum across the date to obtain the cumulative sum of waned
    data_set[data_set$country == c, ]$tmp_csum_waned <- colSums(mat)
  }
  
  data_set$tmp_cov_waned <- data_set$tmp_csum_waned / data_set$population
  names(data_set)[which(names(data_set) == "tmp_csum_waned")] <- name[1]
  names(data_set)[which(names(data_set) == "tmp_cov_waned")] <- name[2]
  
  return(data_set)
}






# ====== Function for decompose epidemic component ==========

# Functions in this file are copied from 2 sources of "auxiliary_functions" codes in 3 papers:

#   1/Endemi-epidemic models with discrete-time serial interval distributions for infectious disease prediction
# PDF: https://doi.org/10.1016/j.ijforecast.2020.07.002
# Codes: https://github.com/jbracher/dengue_noro_rota

# 2/ Multivariate endemic-epidemic models with higher-order lags and an application to outbreak detection
# PDF: https://arxiv.org/abs/1901.03090

# 3/ Periodically stationary multivariate autoregressive models
# PDF: https://arxiv.org/abs/1707.04635
# Codes: https://github.com/jbracher/hhh4addon


# Functions for decomposing the fitted values from hhh4 model fitted
## Decomposing epidemic component

decompose_epidemic_component <- function(fit){
  # extract info:
  sts <- fit$stsObj
  max_lag <- if(class(fit)[1] == "hhh4lag") fit$max_lag else 1
  subset <- fit$control$subset
  n_units <- ncol(sts@observed)
  param <- hhh4addon:::lambda_tilde_complex_neighbourhood(fit, periodic = FALSE,
                                                          subset = 1:max(subset))
  
  # initialize:
  contributions <- array(dim = c(max(subset),
                                 n_units,
                                 n_units,
                                 max_lag),
                         dimnames = list(1:max(subset),
                                         paste0("from.", colnames(sts@observed)),
                                         paste0("to.", colnames(sts@observed)),
                                         paste0("lag", 1:max_lag)))
  # fill:
  for(t in subset){
    phi_temp <- param$lambda[,,t]
    obs_preceding <- sts@observed[t - max_lag:1, , drop = FALSE]
    
    for(lag in 1:max_lag){
      inds <- seq(to = n_units*(max_lag - lag + 1), 
                  length.out = n_units)
      phi_this_lag <- phi_temp[, inds]
      contributions[t, , , lag] <- t(phi_this_lag)*matrix(obs_preceding[max_lag - lag + 1, ],
                                                          ncol = n_units, nrow = n_units)
    }
  }
  return(contributions)
}


## Decomposing 3 components

decompose_coarse <- function(fit){
  
  sts <- fit$stsObj
  max_lag <- if(class(fit)[1] == "hhh4lag") fit$max_lag else 1
  subset <- fit$control$subset
  n_units <- ncol(sts@observed)
  param <- hhh4addon:::lambda_tilde_complex_neighbourhood(fit, periodic = FALSE,
                                                          subset = 1:max(subset))
  
  decomposition_epidemic <- decompose_epidemic_component(fit)
  
  contributions_coarse <- array(dim = c(max(subset),5, n_units),
                                dimnames = list(1:max(subset),
                                                c("endemic",
                                                  "epidemic.self.lag1",
                                                  "epidemic.self.higher_lags",
                                                  "epidemic.other.lag1",
                                                  "epidemic.other.higher_lags"),
                                                paste0("to.", colnames(sts@observed))))
  
  for(t in subset){
    contributions_coarse[t, "endemic", ] <- param$nu[t, ]
    contributions_coarse[t, "epidemic.self.lag1", ] <- diag(decomposition_epidemic[t, , , 1])
    contributions_coarse[t, "epidemic.other.lag1", ] <- 
      colSums(decomposition_epidemic[t, , , 1]) - contributions_coarse[t, "epidemic.self.lag1", ]
    
    if(max_lag > 1){
      contributions_higher_lags_temp <- apply(decomposition_epidemic[t, , , 2:max_lag], 1:2, sum)
      contributions_coarse[t, "epidemic.self.higher_lags", ] <- diag(contributions_higher_lags_temp)
      contributions_coarse[t, "epidemic.other.higher_lags", ] <-
        colSums(contributions_higher_lags_temp) - contributions_coarse[t, "epidemic.self.higher_lags", ]
      
    }else{
      contributions_coarse[t, "epidemic.self.higher_lags", ] <- contributions_coarse[t, "epidemic.other.higher_lags", ] <- 0
    }
  }
  return(contributions_coarse)
}

