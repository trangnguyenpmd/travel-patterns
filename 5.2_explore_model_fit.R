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
### Document: EXPLORE MODEL FIT
### Author: trangngpmd
### Date: 2024-11-21




# ---------------------------------------------------#
# PARAMETERS -----
# ---------------------------------------------------#

param_est <- NULL

list_files <-list.files(path = path_model_fit, pattern = "\\.rda$", full.names = T)
list_files 

for (list in list_files){
  name <- gsub(list, pattern = "G:/My Drive/Projects/Facebook_Travel Patterns/R/GitHub codes/model_fit/", replacement = "")
  name <- gsub(name, pattern = ".rda", replacement="")
  
  # Import fit
  fit <- get(load(list))
  tmp <- summary(fit)
  
  # Extract parameters
  nterms <- terms(fit)$nGroups + 2 #add overdisp parameters
  coefs <- coef(fit)[1:nterms]
  CIs <- confint(fit)[1:nterms, ]
  tab <- cbind(coefs, CIs)
  
  #add random effect variance
  tab <- rbind(tab, c(tmp$REmat[1,1],NA,NA))
  tab <- rbind(tab, c(tmp$REmat[2,2],NA,NA))
  tab <- rbind(tab, c(tmp$REmat[3,3],NA,NA))
  
  # Distributed lag
  tab <- rbind(tab, c(tmp$distr_lag[1],NA,NA))
  tab <- rbind(tab, c(tmp$distr_lag[2],NA,NA))
  
  # Final dataframe
  tab <- as.data.frame(tab) %>% 
    mutate(param = c(rownames(tab)[1:nterms], "ar.var", "ne.var", "end.var", "lag1", "lag2"), 
                     model = name)
  
  param_est <- rbind(param_est, tab)
}

# View(param_est)

param_est <- merge(param_est, data_score[,c("score.rps", "model", "model_name")], by="model", all.x=TRUE) 

write_xlsx(param_est, paste0(path_model_fit,"parameters/parameter_estimates.xlsx"))



tmp <- param_est %>%
  filter(!model == "fit_original")
param_char <- as.character(unique(tmp$param))
param_char

## Parameter plots -----
tmp <- param_est %>% 
  # filter(param %in% param_char[c(1:7)])    # AR component
  # filter(param %in% param_char[c(8:13)])     # NE component
  filter(param %in% param_char[c(14:18)]) # END component

ggplot(data = tmp) +
  geom_point(aes(x=model_name, y=coefs)) + 
  geom_errorbar(aes(x=model_name, ymin = `2.5 %`, ymax = `97.5 %`), width=0.2, position=position_dodge(0.05)) +
  facet_wrap(~param, ncol = 3, scales = "free_x") + 
  xlab("") + 
  ylab("Parameter estimate") +
  theme_bw() + 
  coord_flip()

ggsave(filename = paste0("Parameter estimates - END component.pdf"),
       path = paste0(path_plot,"parameters/"), units = "in",
       width = 7.2,   height = 5, scale = 1, limitsize = FALSE,   #1 rows
       device="pdf", dpi=600)




## Extract variance ----- 
tmp <- param_est %>% filter(param == "end.var")

data_score$end.var <- tmp$coefs
data_score$ne.var <- tmp$coefs
data_score$ar.var <- tmp$coefs

write_xlsx(data_score, paste0(path_model_fit,"parameters/scores.xlsx"))




# ---------------------------------------------------#
# COMPONENT CONTRIBUTION -----
# ---------------------------------------------------#

list_files <-list.files(path = path_model_fit, pattern = "\\.rda$", full.names = T)
list_files


## Calculate component contribution from each fitte model -----
# Total countries only

# Create a data frame to store the results
component_extracted <- data.frame(name=NA, obs=NA, date_week=NA, 
                                  fitted_mean=NA, ar=NA, ne=NA, end=NA, 
                                  ar.exppred.mean=NA, ne.exppred.mean=NA, end.exppred.mean=NA)
component_extracted$date_week <- as.Date(component_extracted$date_week)

# Extract fitted data for each component
for (list in list_files){
  
  name <- gsub(list, pattern = "G:/My Drive/Projects/Facebook_Travel Patterns/R/GitHub codes/model_fit/", replacement = "")
  name <- gsub(name, pattern = ".rda", replacement="")
  
  fit <- get(load(list))
  
  # Calculate the component contribution
  # extract fitted mean by component
  model <- hhh4addon:::terms.hhh4lag(fit) #extract the list of control & hhh4_datalist
  meanhhh <- surveillance:::meanHHH(fit$coefficients, model, total.only = FALSE)
      
  fitted_by_component <- data.frame(name = name,
                                    obs = rowSums(fit$stsObj@observed),
                                    date_week = seq(dateweek_start, dateweek_end, by=7),
                                    fitted_mean = c(rep(NA,2),rowSums(meanhhh$mean)), # adding 2 weeks with NA fitted values => total 143 weeks
                                    #fitted_values_check = c(rep(NA,2),rowSums(fitted.values(fit))),
                                    ar = c(rep(NA,2),rowSums(meanhhh$epi.own)),
                                    ne = c(rep(NA,2),rowSums(meanhhh$epi.neighbours)),
                                    end = c(rep(NA,2),rowSums(meanhhh$endemic)),
                                    ar.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$ar.exppred)),
                                    ne.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$ne.exppred)),
                                    end.exppred.mean = c(rep(NA,2),rowMeans(meanhhh$end.exppred))
      )
      
  component_extracted <- rbind(component_extracted, fitted_by_component)
}

component_extracted <- merge(component_extracted, data_score[,c("model", "model_name")], 
                             by.x="name", by.y="model", all.x=TRUE)

saveRDS(component_extracted[1:1145,], 
        paste0(path_model_fit,"parameters/component_extracted.rds"))


## Calculate the contribution of each component (percentage) -----
# component_extracted <- readRDS(paste0(path_model_fit,"parameters/component_extracted.rds"))

component_contribution <- component_extracted %>%
  filter(!is.na(fitted_mean)) %>%
  group_by(name) %>%
  summarise(total_obs = sum(obs, na.rm = T),
            total_fitted_mean = sum(fitted_mean, na.rm = T), 
            endemic = sum(end, na.rm = T),
            autoregressive = sum(ar, na.rm = T),
            neighbourhood = sum(ne, na.rm = T)) %>%
  mutate(percent_fit = total_fitted_mean/total_obs*100,
         percent_end = endemic/total_fitted_mean*100,
         percent_ar = autoregressive/total_fitted_mean*100,
         percent_ne = neighbourhood/total_fitted_mean*100) %>%
  left_join(data_score, by=c("name" = "model"))
  

write_xlsx(component_contribution, paste0(path_model_fit,"parameters/component_contribution.xlsx"))


#-----------------------------------------------------------------------------#
## Calculate component contribution from each fitted model for each country
#-----------------------------------------------------------------------------#

list_files <-list.files(path = path_model_fit, pattern = "\\.rda$", full.names = T)
list_files 

# Extract fitted data for each component (in each country)
model_name <- unique(data_score$model)

for (i in 1:length(list_files)){
  
  fit <- get(load(list_files[i]))
  
  #create array to store the results
  component_bycountry_array <- array(data = NA, dim = c(143,30,8), 
                                     dimnames = list(as.character(seq(dateweek_start, dateweek_end, by=7)), 
                                                     EU30_countries, 
                                                     c("obs","fitted_mean","ar", "ne", "end","ar.exppred", "ne.exppred", "end.exppred")))
  
  # extract fitted mean by component
  model <- hhh4addon:::terms.hhh4lag(fit) #extract the list of control & hhh4_datalist
  meanhhh <- surveillance:::meanHHH(fit$coefficients, model, total.only = FALSE)
      
  # fill the array
  component_bycountry_array[,,"obs"] <- fit$stsObj@observed
  component_bycountry_array[,,"fitted_mean"] <- rbind(NA, NA, meanhhh$mean)
  component_bycountry_array[,,"ar"] <- rbind(NA, NA, meanhhh$epi.own)
  component_bycountry_array[,,"ne"] <- rbind(NA, NA, meanhhh$epi.neighbours)
  component_bycountry_array[,,"end"] <- rbind(NA, NA, meanhhh$endemic)
      
  component_bycountry_array[,,"ar.exppred"] <- rbind(NA, NA, meanhhh$ar.exppred)
  component_bycountry_array[,,"ne.exppred"] <- rbind(NA, NA, meanhhh$ne.exppred)
  component_bycountry_array[,,"end.exppred"] <- rbind(NA, NA, meanhhh$end.exppred)


  saveRDS(component_bycountry_array, 
          paste0(path_model_fit,"parameters/component_extracted_bycountry_",model_name[i],".rds"))
  
}




#====== RANDOM EFFECTS =====================

ranef <- NULL

list_files <-list.files(path = path_model_fit, pattern = "\\.rda$", full.names = T)
list_files <- list_files[2:8]

model_name <- unique(data_score$model)[2:8]

for (i in 1:length(list_files)){
  
  fit <- get(load(list_files[i]))
  
  tmp <- as.data.frame(ranef(fit))
  tmp$random_intercept <- rownames(tmp)
  tmp$model <- model_name[i]
  tmp$component <- c(rep("ar",30), rep("ne",30), rep("end",30))
  tmp$country <- rep(EU30_countries,3)
  
  ranef <- rbind(ranef, tmp)
  
}

write_xlsx(ranef, paste0(path_model_fit,"parameters/random_intercept.xlsx"))





