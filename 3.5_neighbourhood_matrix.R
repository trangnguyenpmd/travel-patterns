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
### Document: NEIGHBOURHOOD MATRIX
### Author: trangngpmd
### Date: 2024-10-09


# --------------------------------------------------------------------#
# ====== ORIGINAL NEIGHBOURHOOD MATRIX ==========
# --------------------------------------------------------------------#
tmp <- st_make_valid(map_europe) 
row.names(tmp) <- EU30_countries
neighbor_original <- nbOrder(poly2adjmat(tmp), maxlag = Inf)

# saveRDS(neighbor_original, paste0(path_data_clean,"neighbor_original.rds"))


# Plot check
# mat_melt <- melt(neighbor_order_original) %>%
#   mutate_at(c("value"), as.factor)
# ggplot(data = mat_melt, aes_string(x = names(mat_melt)[1],
#                                    y = names(mat_melt)[2], fill = names(mat_melt)[3])) +
#   geom_tile() +
#   labs(x = "", y = "", title = "Neighborhood matrix (original)",
#        fill = "Order") +
#   geom_text(aes(x=Var2, y=Var1), label=mat_melt$value, size=3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_fill_viridis_d(direction = - 1) +
#   coord_fixed()


# (for plotting) connection: origin-destination
mat_melt <- melt(neighbor_original, na.rm = TRUE)
colnames(mat_melt) <- c("origin", "destination", "order")

map_connection <- mat_melt %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("origin" = "country")) %>%
  rename("X_origin" = "X", "Y_origin" = "Y") %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("destination" = "country")) %>%
  rename("X_destination" = "X", "Y_destination" = "Y") %>%
  select(origin, destination, order, X_origin, Y_origin, X_destination, Y_destination)

# saveRDS(map_connection, paste0(path_data_clean,"map_connection_original.rds"))




# --------------------------------------------------------------------#
# ====== ADJUSTED NEIGHBOURHOOD MATRIX ========== 
# (for powerlaw model)
# --------------------------------------------------------------------#
aa <- neighbor_original

# 1st order
aa[5,12] <- aa[12,5] <- 1    # Cyprus vs. Greece (1st order)
aa[7,30] <- aa[30,7] <- 1    # Denmark vs. Sweden (1st order)
aa[8,9] <- aa[9,8] <- 1      # Estonia vs. Finland
aa[8,30] <- aa[30,8] <- 1    # Estonia vs. Sweden
aa[14,23] <- aa[23,14] <- 1  # Iceland vs. Norway
aa[21,16] <- aa[16,21] <- 1  # Italy vs. Malta

#2nd order
aa[2,15] <- aa[15,2] <- 2    # Ireland vs. Belgium 
aa[3,5] <- aa[5,3] <- 2      # Cyprus vs. Bulgaria
aa[7,23] <- aa[23,7] <- 2    # Denmark vs. Norway
aa[7,9] <- aa[9,7] <- 2      # Denmark vs. Finland
aa[7,8] <- aa[8,7] <- 2      # Denmark vs. Estonia
aa[8,23] <- aa[23,8] <- 2    # Estonia vs. Norway (2nd order)
aa[9,17] <- aa[17,9] <- 2    # Finland vs. Latvia
aa[10,21] <- aa[21,10] <- 2  # France vs. Malta
aa[11,30] <- aa[30,11] <- 2  # Germany vs. Sweden
aa[14,9] <- aa[9,14] <- 2    # Iceland vs. Finland
aa[14,30] <- aa[30,14] <- 2  # Iceland vs. Sweden
aa[15,22] <- aa[22,15] <- 2  # Ireland vs. Netherlands
aa[15,10] <- aa[10,15] <- 2  # Ireland vs. France
aa[17,30] <- aa[30,17] <- 2  # Latvia vs. Sweden
aa[21,1] <- aa[1,21] <- 2    # Malta vs. Austria
aa[21,28] <- aa[28,21] <- 2  # Malta vs. Slovenia

#3rd order
aa[1,30] <- aa[30,1] <- 3   # Austria vs. Sweden
aa[2,21] <- aa[21,2] <- 3   # Belgium vs. Malta
aa[2,30] <- aa[30,2] <- 3   # Belgium vs. Sweden 
aa[4,21] <- aa[21,4] <- 3   # Croatia vs. Malta
aa[5,26] <- aa[26,5] <- 3   # Cyprus vs. Romania
aa[6,21] <- aa[21,6] <- 3   # Czechia vs. Malta
aa[6,30] <- aa[30,6] <- 3   # Czechia vs. Sweden
aa[7,17] <- aa[17,7] <- 3   # Denmark vs. Latvia
aa[7,14] <- aa[14,7] <- 3   # Denmark vs. Iceland
aa[8,14] <- aa[14,8] <- 3   # Estonia vs. Iceland
aa[8,11] <- aa[11,8] <- 3   # Estonia vs. Germany
aa[9,11] <- aa[11,9] <- 3   # Finland vs. Germany
aa[9,19] <- aa[19,9] <- 3   # Finland vs. Lithuania
aa[10,30] <- aa[30,10] <- 3 # France vs. Sweden
aa[11,21] <- aa[21,11] <- 3 # Germany vs. Malta
aa[11,23] <- aa[23,11] <- 3 # Germany vs. Norway
aa[13,21] <- aa[21,13] <- 3 # Hungary vs. Malta
aa[15,11] <- aa[11,15] <- 3 # Ireland vs. Germany
aa[15,20] <- aa[20,15] <- 3 # Ireland vs. Luxembourg
aa[15,16] <- aa[16,15] <- 3 # Ireland vs. Italy
aa[15,29] <- aa[29,15] <- 3 # Ireland vs. Spain
aa[17,23] <- aa[23,17] <- 3 # Latvia vs. Norway
aa[18,21] <- aa[21,18] <- 3 # Liechtenstein vs. Malta
aa[20,30] <- aa[30,20] <- 3 # Luxembourg vs. Sweden
aa[20,21] <- aa[21,20] <- 3 # Luxembourg vs. Malta
aa[22,30] <- aa[30,22] <- 3 # Netherlands vs. Sweden
aa[24,30] <- aa[30,24] <- 3 # Poland vs. Sweden
aa[27,21] <- aa[21,27] <- 3 # Slovakia vs. Malta
aa[29,21] <- aa[21,29] <- 3 # Spain vs. Malta
aa[30,19] <- aa[19,30] <- 3 # Sweden vs. Lithuania

#4th order
aa[1,23] <- aa[23,1] <- 4   # Austria vs. Norway
aa[1,9] <- aa[9,1] <- 4     # Austria vs. Finland
aa[1,8] <- aa[8,1] <- 4     # Austria vs. Estonia
aa[2,23] <- aa[23,2] <- 4   # Belgium vs. Norway
aa[2,9] <- aa[9,2] <- 4     # Belgium vs. Finland
aa[5,13] <- aa[13,5] <- 4   # Cyprus vs. Hungary
aa[6,23] <- aa[23,6] <- 4   # Czechia vs. Norway
aa[6,9] <- aa[9,6] <- 4     # Czechia vs. Finland
aa[7,21] <- aa[21,7] <- 4   # Denmark vs. Malta
aa[8,2] <- aa[2,8] <- 4     # Estonia vs. Belgium
aa[9,24] <- aa[24,9] <- 4   # Finland vs. Poland
aa[9,22] <- aa[22,9] <- 4   # Finland vs. Netherlands
aa[9,10] <- aa[10,9] <- 4   # Finland vs. France
aa[9,20] <- aa[20,9] <- 4   # Finland vs. Luxembourg
aa[10,23] <- aa[23,10] <- 4 # France vs. Norway
aa[11,14] <- aa[14,11] <- 4 # Germany vs. Iceland
aa[13,30] <- aa[30,13] <- 4 # Hungary vs. Sweden
aa[14,17] <- aa[17,14] <- 4 # Iceland vs. Latvia
aa[15,7] <- aa[7,15] <- 4   # Ireland vs. Denmark
aa[15,24] <- aa[24,15] <- 4 # Ireland vs. Poland
aa[15,6] <- aa[6,15] <- 4   # Ireland vs. Czechia
aa[15,1] <- aa[1,15] <- 4   # Ireland vs. Austria
aa[15,25] <- aa[25,15] <- 4 # Ireland vs. Portugal
aa[15,21] <- aa[21,15] <- 4 # Ireland vs. Malta
aa[15,28] <- aa[28,15] <- 4 # Ireland vs. Slovenia
aa[16,30] <- aa[30,16] <- 4 # Italy vs. Sweden
aa[18,30] <- aa[30,18] <- 4 # Liechtenstein vs. Sweden
aa[19,23] <- aa[23,19] <- 4 # Lithuania vs. Norway
aa[21,25] <- aa[25,21] <- 4 # Malta vs. Portugal
aa[21,24] <- aa[24,21] <- 4 # Malta vs. Poland
aa[21,26] <- aa[26,21] <- 4 # Malta vs. Romania
aa[22,21] <- aa[21,22] <- 4 # Netherlands vs. Malta
aa[22,8] <- aa[8,22] <- 4   # Netherlands vs. Estonia
aa[23,2] <- aa[2,23] <- 4   # Norway vs. Belgium
aa[23,22] <- aa[22,23] <- 4 # Norway vs. Netherlands
aa[23,20] <- aa[20,23] <- 4 # Norway vs. Luxembourg
aa[23,24] <- aa[24,23] <- 4 # Norway vs. Poland
aa[27,30] <- aa[30,27] <- 4 # Slovakia vs. Sweden
aa[28,30] <- aa[30,28] <- 4 # Slovenia vs. Sweden
aa[29,30] <- aa[30,29] <- 4 # Spain vs. Sweden

# 5th order
aa[1,14] <- aa[14,1] <- 5   # Austria vs. Iceland
aa[1,5] <- aa[5,1] <- 5     # Austria vs. Cyprus
aa[2,14] <- aa[14,2] <- 5   # Belgium vs. Iceland
aa[3,21] <- aa[21,3] <- 5   # Bulgaria vs. Malta
aa[4,30] <- aa[30,4] <- 5   # Croatia vs. Sweden
aa[4,5] <- aa[5,4] <- 5     # Croatia vs. Cyprus
aa[5,27] <- aa[27,5] <- 5   # Cyprus vs. Slovakia
aa[5,28] <- aa[28,5] <- 5   # Cyprus vs. Slovenia
aa[6,14] <- aa[14,6] <- 5   # Cyprus vs. Icelandy
aa[9,29] <- aa[29,9] <- 5   # Finland vs. Spain
aa[9,27] <- aa[27,9] <- 5   # Finland vs. Slovakia
aa[9,18] <- aa[18,9] <- 5   # Finland vs. Liechtenstein
aa[9,13] <- aa[13,9] <- 5   # Finland vs. Hungary
aa[9,28] <- aa[28,9] <- 5   # Finland vs. Slovenia
aa[9,16] <- aa[16,9] <- 5   # Finland vs. Italy
aa[10,14] <- aa[14,10] <- 5 # France vs. Iceland
aa[13,23] <- aa[23,13] <- 5 # Hungary vs. Norway
aa[14,20] <- aa[20,14] <- 5 # Iceland vs. Luxembourg
aa[14,19] <- aa[19,14] <- 5 # Iceland vs. Lithuania
aa[15,18] <- aa[18,15] <- 5 # Ireland vs. Liechtenstein
aa[15,13] <- aa[13,15] <- 5 # Ireland vs. Hungary
aa[15,27] <- aa[27,15] <- 5 # Ireland vs. Slovakia
aa[15,19] <- aa[19,15] <- 5 # Ireland vs. Lithuania
aa[15,30] <- aa[30,15] <- 5 # Ireland vs. Sweden
aa[15,4] <- aa[4,15] <- 5   # Ireland vs. Croatia
aa[16,23] <- aa[23,16] <- 5 # Italy vs. Norway
aa[19,21] <- aa[21,19] <- 5 # Lithuania vs. Malta
aa[21,30] <- aa[30,21] <- 5 # Malta vs. Sweden
aa[22,14] <- aa[14,22] <- 5 # Netherlands vs. Iceland
aa[23,29] <- aa[29,23] <- 5 # Norway vs. Spain
aa[23,18] <- aa[18,23] <- 5 # Norway vs. Liechtenstein
aa[23,27] <- aa[27,23] <- 5 # Norway vs. Slovakia
aa[23,28] <- aa[28,23] <- 5 # Norway vs. Slovenia
aa[24,14] <- aa[14,24] <- 5 # Poland vs. Iceland
aa[25,30] <- aa[30,25] <- 5 # Portugal vs. Sweden
aa[26,30] <- aa[30,26] <- 5 # Romania vs. Sweden

# 6th order
aa[3,30] <- aa[30,3] <- 6  # Bulgaria vs. Sweden
aa[4,23] <- aa[23,4] <- 6  # Croatia vs. Norway
aa[4,9] <- aa[9,4] <- 6    # Croatia vs. Finland
aa[5,18] <- aa[18,5] <- 6  # Cyprus vs. Liechtenstein
aa[5,11] <- aa[11,5] <- 6  # Cyprus vs. Germany
aa[5,6] <- aa[6,5] <- 6    # Cyprus vs. Czechia
aa[5,24] <- aa[24,5] <- 6  # Cyprus vs. Poland
aa[8,15] <- aa[15,8] <- 6  # Estonia vs. Ireland
aa[9,25] <- aa[25,9] <- 6  # Finland vs. Portugal
aa[9,21] <- aa[21,9] <- 6  # Finland vs. Malta
aa[9,26] <- aa[26,9] <- 6  # Finland vs. Romania
aa[9,15] <- aa[15,9] <- 6  # Finland vs. Ireland
aa[12,21] <- aa[21,12] <- 6 # Greece vs. Malta
aa[13,14] <- aa[14,13] <- 6 # Hungary vs. Iceland
aa[14,29] <- aa[29,14] <- 6 # Iceland vs. Spain
aa[14,18] <- aa[18,14] <- 6 # Iceland vs. Liechtenstein
aa[14,28] <- aa[28,14] <- 6 # Iceland vs. Slovenia
aa[14,27] <- aa[27,14] <- 6 # Iceland vs. Slovakia
aa[14,16] <- aa[16,14] <- 6 # Iceland vs. Italy
aa[15,26] <- aa[26,15] <- 6 # Ireland vs. Romania
aa[15,23] <- aa[23,15] <- 6 # Ireland vs. Norway
aa[15,17] <- aa[17,15] <- 6 # Ireland vs. Latvia
aa[16,5] <- aa[5,16] <- 6   # Italy vs. Cyprus
aa[17,21] <- aa[21,17] <- 6 # Latvia vs. Malta
aa[21,23] <- aa[23,21] <- 6 # Malta vs. Norway
aa[23,25] <- aa[25,23] <- 6 # Norway vs. Portugal
aa[23,26] <- aa[26,23] <- 6 # Norway vs. Romania

# 7th order
aa[2,5] <- aa[5,2] <- 7    # Belgium vs. Cyprus
aa[3,23] <- aa[23,3] <- 7  # Bulgaria vs. Norway
aa[3,9] <- aa[9,3] <- 7    # Bulgaria vs. Finland
aa[4,14] <- aa[14,4] <- 7  # Croatia vs. Iceland
aa[5,22] <- aa[22,5] <- 7  # Cyprus vs. Netherlands
aa[5,7] <- aa[7,5] <- 7    # Cyprus vs. Denmark
aa[5,19] <- aa[19,5] <- 7  # Cyprus vs. Lithuania
aa[5,21] <- aa[21,5] <- 7  # Cyprus vs. Malta
aa[8,21] <- aa[21,8] <- 7  # Estonia vs. Malta
aa[10,5] <- aa[5,10] <- 7  # France vs. Cyprus
aa[12,30] <- aa[30,12] <- 7 # Greece vs. Sweden
aa[14,25] <- aa[25,14] <- 7 # Iceland vs. Portugal
aa[14,15] <- aa[15,14] <- 7 # Iceland vs. Ireland
aa[14,26] <- aa[26,14] <- 7 # Iceland vs. Romania
aa[15,3] <- aa[3,15] <- 7   # Ireland vs. Bulgaria
aa[20,5] <- aa[5,20] <- 7   # Luxembourg vs. Cyprus
aa[21,14] <- aa[14,21] <- 7 # Malta vs. Iceland


# 8th order
aa[3,14] <- aa[14,3] <- 8   # Bulgaria vs. Iceland
aa[5,29] <- aa[29,5] <- 8   # Cyprus vs. Spain
aa[5,30] <- aa[30,5] <- 8   # Cyprus vs. Sweden
aa[5,17] <- aa[17,5] <- 8   # Cyprus vs. Latvia
aa[9,12] <- aa[12,9] <- 8   # Finland vs. Greece
aa[12,23] <- aa[23,12] <- 8 # Greence vs. Norway
aa[15,12] <- aa[12,15] <- 8 # Ireland vs. Greece

# 9th order
aa[5,25] <- aa[25,5] <- 9   # Cyprus vs. Portugal
aa[5,23] <- aa[23,5] <- 9   # Cyprus vs. Norway
aa[5,9] <- aa[9,5] <- 9     # Cyprus vs. Finland
aa[5,8] <- aa[8,5] <- 9     # Cyprus vs. Estonia
aa[14,12] <- aa[12,14] <- 9 # Iceland vs. Greece
aa[5,15] <- aa[15,5] <- 9   # Cyprus vs. Ireland

#10th order
aa[5,14] <- aa[14,5] <- 10   # Cyprus vs. Iceland


neighbor_adjusted <- aa

# saveRDS(neighbor_adjusted, paste0(path_data_clean,"neighbor_adjusted.rds"))

# Plot check
# mat_melt <- melt(neighbor_adjusted) %>%
#   mutate_at(c("value"), as.factor)
# ggplot(data = mat_melt, aes_string(x = names(mat_melt)[1],
#                                    y = names(mat_melt)[2], fill = names(mat_melt)[3])) +
#   geom_tile() +
#   labs(x = "", y = "", title = "Neighborhood matrix (adjusted)",
#        fill = "Order") +
#   geom_text(aes(x=Var2, y=Var1), label=mat_melt$value, size=3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_fill_viridis_d(direction = - 1) +
#   coord_fixed()


rm(aa)




