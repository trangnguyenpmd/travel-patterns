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
### Document: DATA IMPORT
### Author: trangngpmd
### Date: 2024-11-19


# ====== DATE, DATE_WEEK FORMAT ==========
## Start-end dates for setting up all datasets
date_start <- as.Date("2020-03-23") # start at week 13-2020
date_end <- as.Date("2022-12-18")   # end at week 50-2022

## Start-end weeks for model fitting and data extraction
dateweek_start <- as.Date("2020-03-23") # start at week 13-2020
dateweek_end <- as.Date("2022-12-12")   # end at week 50-2022

## 1001 days from date_start to date_end
list_date <- data.frame(
  date = seq(from=date_start, to=date_end, by="day"),
  week_num = c(rep(13:53, each=7), rep(1:52, each=7), rep(1:50, each=7)),
  year_by_week = c(rep(2020,(53-13+1)*7), rep(2021, 52*7), rep(2022,50*7))) %>%
  mutate(month = month(date), 
         year = year(date), 
         day_of_week = format(date, "%a"),
         weekend = if_else(day_of_week %in% c("Sat", "Sun"), 1,0),
         day_t = 1:as.numeric(date_end-date_start+1)) %>%
  arrange(date)

## 143 weeks from week start to week end
list_dateweek <- list_date %>%
  select(date, week_num, year_by_week) %>%
  distinct(week_num, year_by_week, .keep_all = TRUE) %>%
  rename("date_week" = "date") %>%
  mutate(week_t = 1:(53-13+1+52+50))

## Finalize the list_date
list_date <- merge(list_date, list_dateweek, by=c("week_num","year_by_week"), all.x=TRUE) %>%
  arrange(date)

## Extract date and week in character form
list_date_char <- as.character(list_date$date)
list_week_char <- as.character(list_dateweek$date_week)

n_days <- as.numeric(date_end - date_start + 1)
n_weeks <- as.numeric(nrow(list_dateweek))
n_countries <- as.numeric(length(EU30_countries))



# -----------------------------------------------------------------------------------------------#
# ====== POPULATION DATA ==========
# Data extracted from World Bank on 04/7/2024 (population by country by year 2020-2022)
pop_country_year <- read_xlsx(paste0(path_data_raw,"population/pop_country_year.xlsx")) %>%
  arrange(year, country)

## Transform population to matrix of 143 x 30
pop_country_mat <- matrix(data=c(rep(pop_country_year$population[1:30],53-13+1),
                                 rep(pop_country_year$population[31:60],52),
                                 rep(pop_country_year$population[61:90],50)), 
                          nrow=143, ncol=30, byrow = TRUE,
                          dimnames = list(list_week_char, EU30_countries))


# -----------------------------------------------------------------------------------------------#
# ====== CASE DATA FROM OUR WORLD IN DATA WEBSITE ==========
# Data was obtained on 20/2/2023. Note that this data has been updated with the data from
# the WHO since 8 March 2023. In this work, we use the data collected before WHO's date

## Import data as long format
cases_owid_date_long <- read.csv(paste0(path_data_raw,"covid19_cases/owid-covid-data.csv")) #data before 8/3/2023

## Clean data
cases_owid_date_long <- cases_owid_date_long %>%
  select(date, country = location, new_cases) %>%
  mutate(date = as.Date(date)) %>%
  left_join(list_date, by="date") %>%
  filter(country %in% EU30_countries,
         between(date, date_start, date_end)) %>%
  arrange(country, date)

## Aggregate by country by week
cases_owid_week_long <- cases_owid_date_long %>%
  group_by(date_week, country) %>%
  summarise(cases_wk = sum(new_cases, na.rm = T))

## Wide format
cases_owid_week_wide <- reshape(as.data.frame(cases_owid_week_long), direction = "wide", 
                                idvar = "date_week", timevar = "country")


## Total cases by country by year
cases_owid_total <- cases_owid_date_long %>%
  group_by(country, year) %>%
  summarise(total_cases = sum(new_cases, na.rm = T)) %>%
  left_join(pop_country_year, by=c("country", "year")) %>% 
  arrange(year, country)

## Create STS data, preparing for the formation of sts object in hhh4 model
cases_sts <- cases_owid_week_wide[,-1]
colnames(cases_sts) <- EU30_countries
rownames(cases_sts) <- seq(from = date_start, to=date_end, by=7)
# table(is.na(cases_sts))


# -----------------------------------------------------------------------------------------------#
# ====== STRINGENCY INDEX ==========
# Data was obtained on 2023-03-14, from the GitHub database Oxford Covid-19 Government Response Tracker (OxCGRT)
# https://github.com/OxCGRT/covid-policy-dataset

## Import data
str_index_date_long <- read.csv(paste0(path_data_raw,"stringency_index/OxCGRT_nat_latest_combined.csv"))

## Clean data
str_index_date_long <- str_index_date_long %>%
  select(date = Date, country = CountryName, StringencyIndex_Average) %>%
  filter(country %in% c(EU30_countries,"Czech Republic", "Slovak Republic")) %>%
  mutate(country = if_else(country == "Czech Republic", "Czechia", 
                           if_else(country == "Slovak Republic", "Slovakia", country)),
         date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  filter(between(date, date_start-14, date_end)) %>%
  left_join(rbind(data.frame(date = seq(date_start-14,date_start-1,"day"),
                             date_week = c(rep(date_start-14,7),rep(date_start-7,7))),
                  list_date[,c("date", "date_week")]),
            by="date")


## Aggregate by country by week
str_index_week_long <- str_index_date_long %>%
  group_by(country, date_week) %>%
  summarise(mean_stringency = mean(StringencyIndex_Average, na.rm = TRUE))


## Create matrix 143 x 30
str_index_mat <- reshape(as.data.frame(str_index_week_long), direction = "wide", 
                         idvar = "date_week", timevar = "country")

## 2-week lag of the stringency index
tmp <- str_index_mat %>% 
  filter(between(date_week, date_start-14,date_end-14))
tmp <- tmp[,-1]
colnames(tmp) <- EU30_countries
rownames(tmp) <- seq(from=date_start-14, to=date_end-14, by=7)

str_index_lag2 <- as.matrix(tmp)

rm(tmp)

# -----------------------------------------------------------------------------------------------#
# ====== VACCINATION DATA ==========
# Data was obtained on 09/10/2024. Specifically, we extracted the number of vaccine doses administered 
# to the total population in each EU/EEA countries as of 05/10/2023, 
# including the primary courses and three booster doses

vaccination_clean <- readRDS(paste0(path_data_clean, "vaccination_clean.rds"))
vaccination_waned <- readRDS(paste0(path_data_clean, "vaccination_waned.rds"))


## Create vaccination matrix
vaccine_mat <- spread(vaccination_waned %>%
                        select(date_week, country, cov_waned), 
                      key="country", value="cov_waned")

tmp <- vaccine_mat[rep(1, 37),] %>%
  mutate(date_week = seq(date_start, as.Date("2020-11-30"), by=7))

vaccine_mat <- rbind(tmp, vaccine_mat)
rownames(vaccine_mat) <- as.character(vaccine_mat$date_week)
vaccine_mat <- as.matrix(vaccine_mat[1:143,-1])




# -----------------------------------------------------------------------------------------------#
# ====== DATA COVID-19 VARIANTS ==========
# Data of variants were obtained from the ECDC website (https://www.ecdc.europa.eu/en/publications-data/data-virus-variants-covid-19-eueea) on 10/9/2024.
# The data from GISAID database is used for analysis.

variants_wide <- readRDS(paste0(path_data_clean, "variants_wide.rds"))


## Create variants matrices
tmp <- variants_wide %>%
  select(date_week, country, percent_alpha, percent_delta, percent_omicron, percent_others)

mat <- tmp %>%
  select(date_week, country, percent_alpha) %>%
  pivot_wider(names_from = country,
              values_from = percent_alpha)
variant_alpha_mat <- as.matrix(mat[,-1])

mat <- tmp %>%
  select(date_week, country, percent_delta) %>%
  pivot_wider(names_from = country,
              values_from = percent_delta)
variant_delta_mat <- as.matrix(mat[,-1])

mat <- tmp %>%
  select(date_week, country, percent_omicron) %>%
  pivot_wider(names_from = country,
              values_from = percent_omicron)
variant_omicron_mat <- as.matrix(mat[,-1])

rm(tmp)

# -----------------------------------------------------------------------------------------------#
# ====== MAP DATA ==========
# create data for world coordinates using map_data() function
tmp1 <- map_data("world") %>%
  rename(country = region) %>%
  filter(country %in% c(EU30_countries, "Czech Republic")) %>%
  mutate(country = if_else(country == "Czech Republic","Czechia", country)) 

tmp2 <- tmp1 %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  left_join(tmp1 %>% select(group, country) %>% distinct(), by = "group")

map_europe <- tmp2 %>%
  mutate(area_meters = as.numeric(st_area(tmp2))) %>%
  group_by(country) %>%
  mutate(main_land_area = max(area_meters)) %>%
  mutate(country_name = ifelse(main_land_area == area_meters, country, NA))

# plot check
# ggplot(data = map_europe) +
#   geom_sf() +
#   geom_sf_text(data = map_europe, aes(label = country_name),size = 3, color = "blue")

rm(tmp1, tmp2)

# (for plotting) Compute centroid values 
map_centroid <- as.data.frame(st_coordinates(st_centroid(map_europe$geometry)))
map_centroid$group <- map_europe$group
map_centroid <- merge(map_centroid, map_europe, by="group", all.x=TRUE)
map_centroid <- map_centroid %>% filter(country_name %in% EU30_countries)
st_geometry(map_centroid) <- "geometry"
# saveRDS(map_centroid, paste0(path_data_clean,"map_centroid.rds"))


## Prepare for sts object for hhh4 fit
map_europe <- cbind(country = EU30_countries,
                    rbind(
                      map_europe %>% filter(country == EU30_countries[1]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[2]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[3]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[4]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[5]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[6]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[7]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[8]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[9]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[10]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[11]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[12]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[13]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[14]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[15]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[16]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[17]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[18]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[19]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[20]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[21]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[22]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[23]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[24]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[25]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[26]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[27]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[28]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[29]) %>% st_combine() %>% st_as_sf(),
                      map_europe %>% filter(country == EU30_countries[30]) %>% st_combine() %>% st_as_sf()))

st_geometry(map_europe) <- "geometry"

## Merge map_europe with data COVID cases and population
map_europe <- map_europe %>%
  mutate(cases2020 = cases_owid_total$total_cases[1:30],
         cases2021 = cases_owid_total$total_cases[31:60],
         cases2022 = cases_owid_total$total_cases[61:90],
         pop2020 = cases_owid_total$population[1:30],
         pop2021 = cases_owid_total$population[31:60],
         pop2022 = cases_owid_total$population[61:90])

# saveRDS(map_europe, paste0(path_data_clean,"map_europe.rds"))

## Prepare for sts object for hhh4 fit
map_sts <- sf:::as_Spatial(map_europe) 
row.names(map_sts) <- EU30_countries


# -----------------------------------------------------------------------------------------------#
# ====== NEIGHBOURHOOD MATRIX ==========

neighbor_original <- readRDS(paste0(path_data_clean,"neighbor_original.rds"))
neighbor_adjusted <- readRDS(paste0(path_data_clean,"neighbor_adjusted.rds"))

neighbor_adjusted_long  <- melt(neighbor_adjusted , na.rm = TRUE)
colnames(neighbor_adjusted_long) <- c("origin", "destination", "ne_order")

# -----------------------------------------------------------------------------------------------#
# ====== INBOUND TRAVELERS ==========

inbound_travelers <- readRDS(paste0(path_data_clean,"inbound_travelers.rds"))


