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
### Document: DATA VACCINATION - PROCESSING
### Author: trangngpmd
### Date: 2024-10-08

# -----------------------------------------------------------------------------------------------#
# ====== IMPORT DATA ==========

readcsv_func <- function(filename) {
  read_csv(filename, col_names = TRUE) %>% 
    mutate(country = filename)
}

vaccination_raw <-list.files(path = paste0(path_data_raw,"vaccination/"), pattern = "\\.csv$", full.names = T) %>%
  map_df(~readcsv_func(.))

vaccination_raw <- vaccination_raw %>%
  mutate(country = gsub(country, pattern = paste0(path_data_raw,"vaccination/"), replacement = "")) %>%
  mutate(country = gsub(country, pattern = ".csv", replacement = ""))

# saveRDS(vaccination_raw, paste0(path_data_raw,"vaccination/vaccination_raw_combine.rds"))



# -----------------------------------------------------------------------------------------------#
# ====== FORMATTING DATA ==========
vaccination_raw <- readRDS(paste0(path_data_raw,"vaccination/vaccination_raw_combine.rds"))

vaccination_clean <- vaccination_raw %>%
  rename(first_doses = `First doses`,
         primary_course = `Primary courses`,
         first_booster = `First boosters`,
         second_booster = `Second boosters`,
         third_booster = `Third boosters`) %>%
  mutate(year_by_week = as.numeric(substr(YearWeek,1,4)),
         week_num = as.numeric(substr(YearWeek,7,8))) %>%
  full_join(list_dateweek, by=c("year_by_week", "week_num")) %>%
  filter(between(date_week, date_start, date_end),
         !is.na(country)) %>%
  group_by(country) %>%
  arrange(date_week) %>%
  mutate(cumdose1st = cumsum(first_doses), 
         cumdose2nd = cumsum(primary_course), 
         cumbooster1 = cumsum(first_booster),
         cumbooster2 = cumsum(second_booster),
         cumbooster3 = cumsum(third_booster)) %>%
  arrange(country, date_week) %>%
  left_join(pop_country_year, by = c("country", "year_by_week" = "year")) %>%
  mutate(percent_dose1st = cumdose1st/population,
         percent_dose2nd = cumdose2nd/population,
         percent_booster1 = cumbooster1/population,
         percent_booster2 = cumbooster2/population,
         percent_booster3 = cumbooster3/population)

summary(vaccination_clean)

# saveRDS(vaccination_clean, paste0(path_data_clean,"vaccination_clean.rds"))


# ------------------------------------------------------------#
# ===== Extract data by dose & apply waning function =====
# ------------------------------------------------------------#

# ------------------------------------------------------------#
## THE PRIMARY COURSE (the first 2 doses) -----
tmp <- vaccination_clean %>%
  select(date_week, country, newdose = primary_course, cumdose = cumdose2nd, population) 

vaccine_2nd <- apply_waning(data_set = tmp, FUN = waning,
                            name = c("csum_waned_2nd", "cov_waned_2nd")) %>%
  select(date_week, country, cov_waned_2nd)

rm(tmp)

# ------------------------------------------------------------#
## 3RD DOSE VACCINATION DATA (the first booster) -----
tmp <- vaccination_clean %>%
  select(date_week, country, newdose = first_booster, cumdose = cumbooster1, population) 

vaccine_3rd <- apply_waning(data_set = tmp, FUN = waning,
                            name = c("csum_waned_3rd", "cov_waned_3rd")) %>%
  select(date_week, country, cov_waned_3rd)


rm(tmp)

# ------------------------------------------------------------#
## 4TH DOSE VACCINATION DATA (The second booster) -----
tmp <- vaccination_clean %>%
  select(date_week, country, newdose = second_booster, cumdose = cumbooster2, population) 

vaccine_4th <- apply_waning(data_set = tmp, FUN = waning,
                            name = c("csum_waned_4th", "cov_waned_4th")) %>%
  select(date_week, country, cov_waned_4th)

rm(tmp)


# ------------------------------------------------------------#
## 5TH DOSE VACCINATION DATA (The third booster) -----
tmp <- vaccination_clean %>%
  select(date_week, country, newdose = third_booster, cumdose = cumbooster3, population)

vaccine_5th <- apply_waning(data_set = tmp, FUN = waning,
                            name = c("csum_waned_5th", "cov_waned_5th")) %>%
  select(date_week, country, cov_waned_5th)


rm(tmp)

# ------------------------------------------------------------# 
## Merge all vaccine doses
vaccination_waned <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                            list(vaccine_2nd, vaccine_3rd, vaccine_4th, vaccine_5th))


vaccination_waned <- vaccination_waned %>%
  mutate(cov_waned = cov_waned_2nd + cov_waned_3rd + cov_waned_4th + cov_waned_5th)

tmp <- vaccination_waned %>%
  # filter(cov_waned > 1) %>%
  mutate(cov_waned = if_else(cov_waned >1,0.9999,cov_waned)) 


# saveRDS(vaccination_waned, paste0(path_data_clean,"vaccination_waned.rds"))


