##############################################
###### COVID-19 in EU/EEA 2020 - 2022 ########
##############################################


### MANUSCRIPT
### GLOBAL MOBILITY FLOWS AND COVID-19 SPREAD IN EUROPE DURING THE EMERGENCY PHASE: INSIGHTS FROM FACEBOOK DATA

### Authors:
### Thi Huyen Trang Nguyen (1), Niel Hens (1,2), Christel Faes (1)

### Affiliation:
### (1) Data Science Institute, I-BioStat, Hasselt University, BE-3500 Hasselt, Belgium
### (2) Centre for Health Economic Research and Modelling Infectious Diseases, Vaccine and Infectious Disease Institute, University of Antwerp, BE-2000 Antwerpen, Belgium


# ====== GENERAL INFO ==========
### Document: HHH4 MODEL FIT (from the original observation) WITH RANDOM INTERCEPTS
### Author: trangngpmd
### Date: 2024-04-29


# ---------------------------------------------------------#
# STS OBJECT (surveillance-time-series STS class) -----

sts_covid <- sts(observed = cases_sts, #from week 13-2020 to week 50-2022 (143 weeks)
                 start = c(2020, 13), frequency = 52,
                 neighbourhood = neighbor_adjusted,  #use the adjusuted neighbourhood matrix
                 map = map_sts, population = pop_country_mat)

# plot(sts_covid, type = observed ~ time)


# ---------------------------------------------------------#
# DEFINE COVARIATES -----

offset <- population(sts_covid)         # in END component
log_pop <- log(population(sts_covid))   # in AR, NE component

log_susceptible <- log(1 - vaccine_mat)

stringency <- str_index_lag2

variant_alpha <- variant_alpha_mat
variant_delta <- variant_delta_mat
variant_omicron <- variant_omicron_mat

# ---------------------------------------------------------#
# MODEL FORMULA -----
## (component-specific fixed intercepts)
f_end <- ~ 1  +  log_susceptible + variant_alpha + variant_delta + variant_omicron
f_ar <- ~ 1 + log_pop + stringency + log_susceptible + variant_alpha + variant_delta + variant_omicron
f_ne <- ~ 1 + log_pop + stringency + variant_alpha + variant_delta + variant_omicron


## hhh4 data list
hhh4_datalist <- list(log_pop = log_pop,
                      log_susceptible = log_susceptible,
                      stringency = stringency, 
                      variant_alpha = variant_alpha,
                      variant_delta = variant_delta,
                      variant_omicron = variant_omicron
)


## Model set up & control list
lag_optimal <- 2
fit_start <- 3   
fit_end <- 143 

# -------------------------------------------------------------------#
# MODEL FIT WITH COMPONENT-SPECIFIC FIXED INTERCEPTS -----
# -------------------------------------------------------------------#

# Control list
control_fit <- list(
  end = list(f= f_end, offset = offset),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 10,log = TRUE, normalize = FALSE, initial = c("logd" = log(2)))),
  optimizer = list(stop = list(iter.max = 1000,tol=1e-5)),
  # start= list(fixed=c("ar.1"=-0.038, "ar.log_pop"=0.002,"ar.stringency"=0.001,
  #                     "ar.log_susceptible"=-0.053,"ar.variant_alpha"=-0.129,
  #                     "ar.variant_delta"=0.077, "ar.variant_omicron"=-0.180,
  #                     "ne.1"=-13, "ne.log_pop"=0.707, "ne.stringency"=-0.087,
  #                     "ne.variant_alpha"=-3.492, "ne.variant_delta"=-1.968,
  #                     "ne.variant_omicron"=-8.539, "end.1"=-19.107, "end.log_susceptible"=-0.172,
  #                     "end.variant_alpha"=-2.307, "end.variant_delta"=8.444,
  #                     "end.variant_omicron"=10.882, "neweights.logd"=0.018,'overdisp'=0.2)),
  family = "NegBin1",
  data = hhh4_datalist,
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, #continue finding the time-lag weights, which will be estimated from simulated datasets
  max_lag = lag_optimal)

fit <- profile_par_lag(sts_covid, control = control_fit)
fit$convergence #check convergence
summary(fit)

save(fit, file = paste0(path_model_fit,"fit_original.rda"))


# ---------------------------------------------------------------------#
# MODEL FIT WITH WITH RANDOM INTERCEPTS -----

# Random intercepts are included in all 3 components
fit <- update(fit, # fit original
              end = list(f = update(formula(fit)$end, ~. + ri() - 1)),
              ar = list(f = update(formula(fit)$ar, ~. + ri() - 1)),
              ne = list(f = update(formula(fit)$ne, ~. + ri() - 1)),
              use.estimates = FALSE,
              start = list(fixed=c("ar.ri(iid)"=-0.1657, "ar.log_pop"=0.0011,"ar.stringency"=0.0013,
                                   "ar.log_susceptible"=-0.0598,"ar.variant_alpha"=-0.0133,
                                   "ar.variant_delta"= 0.1462, "ar.variant_omicron"=-0.0831,
                                   "ne.ri(iid)"=-18.2257, "ne.log_pop"=1.0733, "ne.stringency"=-0.0897,
                                   "ne.variant_alpha"=-4.5731, "ne.variant_delta"=-2.3926,
                                   "ne.variant_omicron"=-7.4414, "end.ri(iid)"=-21.3839, "end.log_susceptible"=0.1603,
                                   "end.variant_alpha"=5.5582, "end.variant_delta"=10.6406,
                                   "end.variant_omicron"=13.2520, "neweights.logd"=-0.6271,'overdisp'=0.2)))


fit$convergence #check convergence
summary(fit)

save(fit, file = paste0(path_model_fit,"fit_original_ri.rda"))

rm(fit)

# ------------------------------------------------------------------------------#
# MODEL FIT WITH NEW SPATIAL WEIGHTS FROM TRAVEL PATTERNS DATA (TRAVEL_ARRAY) -----

## Retrieved the original fitted model with power law -----
fit_ri <- get(load(paste0(path_model_fit,"fit_original_ri.rda"))) 
summary(fit_ri)

## Update the original model with time-varying spatial weights -----
list_files <-list.files(path = path_weights, pattern = "\\.rds$", full.names = T)
list_files <- list_files[1:5]

for (list in list_files){
  
  name <- gsub(list, 
               pattern = "G:/My Drive/Projects/Facebook_Travel Patterns/R/GitHub codes/weights/", 
               replacement = "")
  name <- gsub(name, pattern = "prop_travel", replacement = "")
  name <- gsub(name, pattern = ".rds", replacement="")
  
  travel_array <- readRDS(list)
  fit <- update(fit_ri, ne = list(weights = travel_array))
  save(fit, file = paste0(path_model_fit,"fit_",name,".rda"))
  
  print(name)
  print(fit$convergence)
  
}



# Check convergence => All models are converged
list_files <-list.files(path = paste0(path_model_fit), pattern = "\\.rda$", full.names = T)
list_files 

for (list in list_files){
  fit <- get(load(list))
  if (fit$convergence == TRUE){
    print(list)
  }
  
}

# fit <- get(load(list_files[5]))
# summary(fit)

# ---------------------------------------------------------#
# MODEL FIT WITH INCOMING TRAVELERS -----
list_files[5] # check file fit if this model is fitted with the weight W
fit <- get(load(list_files[5]))

fit <- update(fit,
                end = list(f = update(formula(fit)$end, ~. + inbound_travelers)),
                use.estimates = FALSE,
                data = list(inbound_travelers = inbound_travelers))
summary(fit)
save(fit, file = paste0(path_model_fit,"fit_W_inbound.rda"))




# ---------------------------------------------------------#
# MODEL COMPARISON -----

## Calculate the scores -----
timepoint <- c(3, 142) ## year 2022
models_compare <- c("fit") # Models to compare
SCORES <- c("rps", "logs", "dss", "ses") #List score to compare between models

list_files <-list.files(path = path_model_fit, pattern = "\\.rda$", full.names = T)
list_files 

for (list in list_files){
  name <- gsub(list, pattern = "G:/My Drive/Projects/Facebook_Travel Patterns/R/GitHub codes/model_fit/", replacement = "")
  name <- gsub(name, pattern = ".rda", replacement="")
  
  fit <- get(load(list))
  predictions <- lapply(mget(models_compare), oneStepAhead_hhh4lag,
                        tp = timepoint, type = "final")
  Scores <- lapply(predictions, scores, which = SCORES, individual = TRUE)
  
  saveRDS(Scores, paste0(path_model_fit,"scores/",name, ".rds"))
}


## Extract the scores -----
data_score <- NULL
list_files <-list.files(path = paste0(path_model_fit,"scores/"), pattern = "\\.rds$", full.names = T)
list_files 

for (list in list_files){
  name <- gsub(list, pattern = "G:/My Drive/Projects/Facebook_Travel Patterns/R/GitHub codes/model_fit/scores/", replacement = "")
  name <- gsub(name, pattern = ".rds", replacement="")
  
  Scores <- readRDS(list)
  tmp <- data.frame(score = t(sapply(Scores, colMeans, dims = 2)),
                    model = name)
  
  data_score <- rbind(data_score, tmp)
  
}

View(data_score)

data_score <- data_score %>%
  mutate(model_name = c("fit_original","fit_powerlaw", 
                        "sensitivity fit_W",
                        "sensitivity fit_W_2023",
                        "fit_W","fit_W_decay",
                        "fit_W_inbound",
                         "fit_W_order"))
data_score$model_name <- factor(data_score$model_name, levels = c("fit_powerlaw", "fit_W_inbound",
                                                                  "fit_W", "fit_W_order", "fit_W_decay", "sensitivity fit_W", "sensitivity fit_W_2023"))



## Combine all Scores to a list
Scores_list <- readRDS(list_files[1])
names(Scores_list)[length(Scores_list)] <- data_score$model[1]

for (i in 2:nrow(data_score)){
 
  name <- data_score$model[i]
  
  Scores <- readRDS(list_files[i])
  names(Scores)[length(Scores)] <- name
  
  Scores_list <- modifyList(Scores_list, Scores)
  
}


## Permutation test -----
set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores_list$fit_W_inbound[, , score],
  score2 = Scores_list$fit_original_ri[, , score])) # powerlaw model
#             rps       logs        dss         ses      
# diffObs     8.544567  0.001599109 -0.01028306 2976447  
# pVal.permut 0.1664    0.49        0.6017      0.4677   
# pVal.t      0.1668302 0.4665433   0.5371882   0.4542845


set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores_list$fit_W_inbound[, , score],
  score2 = Scores_list$fit_W[, , score]))
#              rps         logs          dss          ses      
# diffObs     -7.501347   -0.0004047327 -0.006409344 -68028.44
# pVal.permut 0.0079      0.4544        0.5179       0.9826   
# pVal.t      0.008434346 0.3882312     0.3497923    0.9784023


set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores_list$fit_W_inbound[, , score],
  score2 = Scores_list$fit_W_decay[, , score]))
#             rps         logs         dss         ses      
# diffObs     -11.52278   -0.002126338 -0.01707038 1543779  
# pVal.permut 0.0059      0.0032       0.0031      0.7715   
# pVal.t      0.006353493 0.004639334  0.03034337  0.7541977


set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores_list$fit_W_inbound[, , score],
  score2 = Scores_list$fit_W_order[, , score]))
#             rps         logs         dss         ses      
# diffObs     -15.19469   -0.003777623 -0.02801254 2597821  
# pVal.permut 0.0091      0.001        3e-04       0.7382   
# pVal.t      0.008899162 0.001273796  0.004068107 0.7195748


set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores_list$fit_W[, , score],
  score2 = Scores_list$fit_W_decay[, , score]))
#             rps        logs         dss         ses      
# diffObs     -4.021436  -0.001721606 -0.01066103 1611808  
# pVal.permut 0.0777     0.0019       4e-04       0.5698   
# pVal.t      0.07717211 0.002517131  0.001467297 0.5467697


set.seed(1)
sapply(SCORES, function (score) permutationTest(
  score1 = Scores_list$fit_W[, , score],
  score2 = Scores_list$fit_W_order[, , score]))
#             rps        logs        dss          ses      
# diffObs     -7.693348  -0.00337289 -0.02160319  2665849  
# pVal.permut 0.0631     0.0014      2e-04        0.6179   
# pVal.t      0.06060676 0.001411832 0.0007310566 0.5937754

