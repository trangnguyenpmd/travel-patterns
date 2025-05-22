#============================================#
#==== COVID-19 in EU/EEA 2020 - 2022 ========#
#============================================#


### MANUSCRIPT
### CAPTURING THE SPATIOTEMPORAL SPREAD OF COVID-19 IN 30 EUROPEAN COUNTRIES DURING 2020 - 2022

### Authors:
### Thi Huyen Trang Nguyen (1), Niel Hens (1,2), Christel Faes (1)

### Affiliation:
### (1) Data Science Institute, I-BioStat, Hasselt University, BE-3500 Hasselt, Belgium
### (2) Centre for Health Economic Research and Modelling Infectious Diseases, Vaccine and Infectious Disease Institute, University of Antwerp, BE-2000 Antwerpen, Belgium



# ====== GENERAL INFO ==========
### Document: DATA VARIANTS PROCESSING
### Author: trangngpmd
### Date: 2024-10-09


# --------------------------------------------------------------------#
# ===== VARIANTS LIST ==========
# --------------------------------------------------------------------#
# The list is based on the table here: https://www.ecdc.europa.eu/en/covid-19/variants-concern

Alpha <- c("B.1.1.7")
Delta <- c("B.1.617.2")
Omicron <- c("BA.1",                       # Omicron_BA.1
             "BA.2","BA.2+L452X",          # Omicron_BA.2
             "BA.2.75",                    # Omicron_BA.2.75
             "BA.3",                       # Omicron_BA.3
             "BA.4", "BA.5","BA.4/BA.5",   # Omicron_BA.4 and BA.5
             "BQ.1",                       # Omicron_BQ.1
             "XBB", "XBB.1.5","XBB.1.5-like", "XBB.1.5-like+F456L",    # Omicron_XBB
             "BA.2.86", "B.1.1.529")             
Others <- c("B.1.351",                     # Beta
            "B.1.427/B.1.429",             # Epsilon
            "B.1.525",                     # Eta
            "B.1.617.1",                   # Kappa 
            "B.1.621",                     # Mu
            "C.37",                        # Lamda
            "P.1",                         # Gamma
            "P.3",                         # Theta
            "AY.4.2", "B.1.1.7+E484K",      # Others
            "B.1.620", "SGTF", "B.1.617.3", "B.1.616",   # Others
            "Other")                                     # Others
Unknown <- c("UNK")



# --------------------------------------------------------------------#
# ===== DATA IMPORT ==========
# --------------------------------------------------------------------#
variants_raw <- read.csv(paste0(path_data_raw,"variants/data_variants.csv"))

# unique(variants_raw$variant)


# --------------------------------------------------------------------#
# =====  DATA CLEANING ==========
# --------------------------------------------------------------------#
variants_clean <- variants_raw %>%
  filter(country %in% EU30_countries, source == "GISAID") %>%
  mutate(variants_group = case_when(variant %in% Alpha ~ "Alpha",
                                    variant %in% Delta ~ "Delta",
                                    variant %in% Omicron ~ "Omicron",
                                    variant %in% Others ~ "Others",
                                    variant %in% Unknown ~ "Unknown")) %>%
  mutate(year_by_week = as.numeric(substr(year_week,1,4)),
         week_num = as.numeric(substr(year_week,6,8))) %>%
  select(year_by_week, week_num, country, variant, variants_group,
         number_sequenced, number_detections_variant, number_sequenced_known_variant) %>%
  left_join(list_dateweek, by=c("year_by_week", "week_num")) %>%
  filter(between(date_week, date_start, date_end), !variants_group == "Unknown") %>%
  arrange(country, year_by_week, week_num, variants_group) 

# saveRDS(variants_clean, paste0(path_data_clean,"variants_clean.rds"))



# --------------------------------------------------------------------#
# ===== DATA EXTRACTION ==========
# --------------------------------------------------------------------#

# Extract the total samples sequenced
variants_total_sequenced <- variants_clean %>%
  select(country, date_week, number_sequenced ,number_sequenced_known_variant) %>% 
  distinct()

# Extract the total detected variants among total sample sequenced
variants_total_detected <- variants_clean %>% 
  group_by(country, date_week, variants_group) %>% 
  summarise(number_detections_variant = sum(number_detections_variant),.groups = 'drop') %>%
  distinct()

# LONG FORMAT
variants_long <- merge(variants_total_detected, variants_total_sequenced, 
                       by = c("country","date_week")) %>%
  mutate(percent_variant = number_detections_variant/number_sequenced,
         percent_variant_known = number_detections_variant/number_sequenced_known_variant) 

# saveRDS(variants_long, paste0(path_data_clean,"variants_long.rds"))


# WIDE FORMAT
variants_wide <- variants_long %>%
  select(date_week, country, variants_group, number_detections_variant) %>%
  group_by(country) %>%
  pivot_wider(names_from = variants_group,
              values_from = number_detections_variant) %>%
  left_join(variants_total_sequenced, by=c("country", "date_week")) %>%
  # Expand all date_week and country
  right_join(data.frame(date_week = rep(seq(date_start, date_end, by=7),30), 
                        country = rep(EU30_countries, each=143)),
             by=c("date_week", "country"))



# --------------------------------------------------------------------#
# ===== CALCULATION PERCENTAGE ==========
# --------------------------------------------------------------------#
## Calculate the number of sequences of all countries
numbervariants_allcountries <- variants_wide %>%
  group_by(date_week) %>%
  summarise(allcountries_allsequences = sum(number_sequenced_known_variant, na.rm = TRUE),
            allcountries_alpha = sum(Alpha, na.rm = TRUE), 
            allcountries_delta = sum(Delta, na.rm = TRUE), 
            allcountries_omicron = sum(Omicron, na.rm = TRUE))

## Calculate the percentage per country
variants_wide <- variants_wide %>%
  left_join(numbervariants_allcountries, by="date_week") %>%
  mutate(percent_alpha = if_else(number_sequenced_known_variant == 0, allcountries_alpha/allcountries_allsequences,
                                 Alpha/number_sequenced_known_variant),
         percent_delta = if_else(number_sequenced_known_variant == 0, allcountries_delta/allcountries_allsequences,
                                 Delta/number_sequenced_known_variant),
         percent_omicron = if_else(number_sequenced_known_variant == 0, allcountries_omicron/allcountries_allsequences,
                                   Omicron/number_sequenced_known_variant)) %>%
  mutate(percent_others = 1 - percent_alpha - percent_delta - percent_omicron)

# saveRDS(variants_wide, paste0(path_data_clean,"variants_wide.rds"))



