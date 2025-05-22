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
### Document: TRAVEL PATTERNS (sensitivity analysis)
### Author: trangngpmd
### Date: 2024-11-20


#======================================#
# Import data fitted GAM -----
#======================================#

travel_patterns <- readRDS(paste0(path_data_clean,"travel_patterns_GAMLSS_predict.rds"))

#==========================================#
# Sensitivity Analysis -  Weight/2 -----
#==========================================#

#------------------------------------------------------------#
## Translate to n x n x day matrices from predicted values -----
tmp1 <- travel_patterns %>%
  select(date, origin, destination, predict_pop)


travel_array <- array(data = NA, dim = c(n_countries,n_countries,n_days),
                      dimnames = list(EU30_countries, EU30_countries, list_date_char))


for (i_day in list_date_char){
  
  # Change data predict to matrix
  na_matrix <- tmp1 %>%
    filter(date == i_day) %>%
    spread(destination, predict_pop) %>%
    arrange(origin) %>%
    select(!c("date","origin")) %>%
    select(sort(names(.)))
  
  na_matrix <- as.matrix(na_matrix)
  row.names(na_matrix) <- EU30_countries
  
  # Add min values to the NAs in the na_matrix
  day_matrix <- NULL
  for (j in 1:n_countries){
    tmp2 <- as.matrix(t(na_matrix[j,]))
    tmp2[is.na(tmp2)] <- min(tmp2, na.rm = TRUE)/2     ### NEW !!!!!!!!!!!!!!! add min(value)/2
    day_matrix <- rbind(day_matrix, tmp2)
  }
  
  # Iceland
  day_matrix[14,] <- min(na_matrix, na.rm = TRUE)/2    ### NEW !!!!!!!!!!!!!!! add min(value)/2
  
  # Final matrix
  diag(day_matrix) <- 0
  travel_array[,,i_day] <- day_matrix
} 

# Check array
# View(travel_array[,,100])

rm(tmp1, tmp2, i_day, j, na_matrix, day_matrix)


#------------------------------------------------------------#
## Translate to n x n x WEEK matrices
travel_array_week <- array(data = NA, dim = c(n_countries,n_countries,n_weeks),
                           dimnames = list(EU30_countries, EU30_countries, list_week_char))

for (i_wk in 1:n_weeks){
  tmp <- travel_array[,,c(i_wk*7-6)] + travel_array[,,c(i_wk*7-5)] + travel_array[,,c(i_wk*7-4)] + travel_array[,,c(i_wk*7-3)] + travel_array[,,c(i_wk*7-2)] + travel_array[,,c(i_wk*7-1)] +travel_array[,,c(i_wk*7)]
  travel_array_week[,,i_wk] <- tmp
}

saveRDS(travel_array_week, paste0(path_weights,"sens_travelW.rds"))

rm(tmp, i_wk)

#------------------------------------------------------------#
## Translate to row normalized version -----
normalized_array <- array(data = NA, dim = c(n_countries,n_countries,n_weeks),
                          dimnames = list(EU30_countries, EU30_countries, list_week_char))

for (i_wk in 1:n_weeks){
  tmp <- travel_array_week[,,i_wk]
  tmp <- tmp/rowSums(tmp)
  normalized_array[,,i_wk] <- tmp
}

rowSums(normalized_array[,,143])
saveRDS(normalized_array, paste0(path_weights, "sens_prop_travelW.rds"))

rm(tmp, i_wk, normalized_array)

rm(travel_array, travel_array_week)


#======================================%
# Array from predicted values in 2023 -----
#======================================%
#------------------------------------------------------------#
## Translate to n x n x day matrices from predicted values -----

tmp1 <- travel_patterns %>%
  select(date, origin, destination, predict_pop) %>%
  filter(between(date, as.Date("2023-01-02"), as.Date("2023-12-31")))

dum_date <- as.character(seq(as.Date("2023-01-02"), as.Date("2023-12-31"),1))
dum_week <- as.character(seq(as.Date("2023-01-02"), as.Date("2023-12-31"),7))

travel_array <- array(data = NA, dim = c(n_countries,n_countries,length(dum_date)),
                      dimnames = list(EU30_countries, EU30_countries, dum_date))


for (i_day in dum_date){
  
  # Change data predict to matrix
  na_matrix <- tmp1 %>%
    filter(date == i_day) %>%
    spread(destination, predict_pop) %>%
    arrange(origin) %>%
    select(!c("date","origin")) %>%
    select(sort(names(.)))
  
  na_matrix <- as.matrix(na_matrix)
  row.names(na_matrix) <- EU30_countries
  
  # Add min values to the NAs in the na_matrix
  day_matrix <- NULL
  for (j in 1:n_countries){
    tmp2 <- as.matrix(t(na_matrix[j,]))
    tmp2[is.na(tmp2)] <- min(tmp2, na.rm = TRUE)
    day_matrix <- rbind(day_matrix, tmp2)
  }
  
  # Iceland
  day_matrix[14,] <- min(na_matrix, na.rm = TRUE)
  
  # Final matrix
  diag(day_matrix) <- 0
  travel_array[,,i_day] <- day_matrix
} 

# Check array
# View(travel_array[,,1])

rm(i_day, j, na_matrix, day_matrix)

#------------------------------------------------------------#
## Translate to n x n x WEEK matrices (52 weeks) -----
travel_array_week <- array(data = NA, dim = c(n_countries,n_countries, length(dum_date)/7),
                        dimnames = list(EU30_countries, EU30_countries, dum_week))
                        
for (i_wk in 1:52){
  tmp <- travel_array[,,c(i_wk*7-6)] + travel_array[,,c(i_wk*7-5)] +travel_array[,,c(i_wk*7-4)] +travel_array[,,c(i_wk*7-3)] +travel_array[,,c(i_wk*7-2)] +travel_array[,,c(i_wk*7-1)] +travel_array[,,c(i_wk*7)]
  travel_array_week[,,i_wk] <- tmp
}

saveRDS(travel_array_week, paste0(path_weights, "sens_travelW_52wk2023.rds"))

# View(travel_array_week[,,1])

#------------------------------------------------------------#
## Translate to n x n x WEEK matrices (143 weeks) -----
tmp1 <- array(data = NA, dim = c(n_countries,n_countries,n_weeks),
                           dimnames = list(EU30_countries, EU30_countries, list_week_char))

tmp2 <- list_dateweek %>%
  mutate(week_num_2023 = c(13:52,52,1:52,1:50))


for (i in 1:n_weeks){
  week_num_2023 <- tmp2$week_num_2023[i]
  tmp1[,,i] <- travel_array_week[,,week_num_2023]
}


saveRDS(tmp1, paste0(path_weights, "sens_travelW_2023.rds"))

rm(tmp2, week_num_2023)


#------------------------------------------------------------#
## Translate to row normalized version -----
normalized_array <- array(data = NA, dim = c(n_countries,n_countries,n_weeks),
                          dimnames = list(EU30_countries, EU30_countries, list_week_char))

for (i_wk in 1:n_weeks){
  tmp <- tmp1[,,i_wk]
  tmp <- tmp/rowSums(tmp)
  normalized_array[,,i_wk] <- tmp
}

rowSums(normalized_array[,,143])
saveRDS(normalized_array, paste0(path_weights, "sens_prop_travelW_2023.rds"))

rm(tmp, tmp1, tmp2, i_wk, normalized_array)


