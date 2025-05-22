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
### Document: LIBRARY - DATA SET UP
### Author: trangngpmd
### Date: 2024-11-19


# ====== LIBRARY ==========
library(surveillance)
library(hhh4addon)
library(hhh4contacts)

library(tidyverse)
library(zoo)
library(reshape2)
library(ggplot2)
library(sf)
library(readxl)
library(writexl)
library(stringr)
library(geosphere)
library(gridExtra)
library(ggrepel)

library(gamlss)
library(gamlss.cens)
library(gamlss.add)


# install.packages("hhh4addon")
# library(devtools)
# install_github("jbracher/hhh4addon", build_vignettes = TRUE)



# ====== PATH ==========
path_data_raw <- "G:/My Drive/Projects/Facebook_Travel Patterns/data_raw/"
path_data_clean <- "G:/My Drive/Projects/Facebook_Travel Patterns/data_clean/"
path_model_fit <- "G:/My Drive/Projects/Facebook_Travel Patterns/model_fit/"
path_plot <- "G:/My Drive/Projects/Facebook_Travel Patterns/plot_output/"


# ====== COUNTRY LIST ==========
EU30_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus" , "Czechia", 
                    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                    "Hungary", "Iceland", "Ireland",  "Italy", "Latvia","Liechtenstein", 
                    "Lithuania", "Luxembourg", "Malta", "Netherlands","Norway",  "Poland", 
                    "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

