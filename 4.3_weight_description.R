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
### Document: SPATIAL WEIGHT DESCRIPTION
### Author: trangngpmd
### Date: 2024-11-20



# -----------------------------------------------------------------------------------------------#
# ====== Data Import ==========
# -----------------------------------------------------------------------------------------------#
travel_patterns <- readRDS(paste0(path_data_clean,"travel_patterns_GAMLSS_predict.rds"))


# -----------------------------------------------------------------------------------------------#
# ====== Weight Matrices ==========
# -----------------------------------------------------------------------------------------------#

# ---------------------------------------------------#
## Plot GAM-predict with blank cells (Week 143) -----
# ---------------------------------------------------#
tmp <- travel_patterns %>%
  filter(date_week == "2022-12-12") %>%
  group_by(origin, destination) %>%
  summarise(no_travelers = sum(predict_pop, na.rm = TRUE))

max(tmp$no_travelers) # 252921.6
min(tmp$no_travelers)

plot1 <- ggplot(tmp, aes(destination, origin, fill = no_travelers))+
  geom_tile(color = "white")+
  theme_bw()+
  scale_fill_gradient2(high="red",low="#FFFFCC", na.value="lightgrey",
                       limits = c(1, 400000),
                       breaks = c(1, 100000, 200000, 300000, 400000),
                       labels =   c(1, 100000, 200000, 300000, 400000)) + 
  xlab("")+ #Country of destination
  ylab("")+ #Country of origin
  ggtitle(expression(bold((A)~w[ji]^{(pred)}~("Week 50/2022")))) +
  labs(fill = "") +
  theme(legend.justification = c(1, 0),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1, size=6),
        axis.text.y=element_text(size=6),
        plot.title = element_text(face="bold")) +
  coord_fixed() +
  guides(fill = guide_colorbar(barwidth = 0.5))

plot1
rm(tmp)


# ---------------------------------------------------#
## Plot GAM-predict with min values (Week 143) -----
# ---------------------------------------------------#
tmp <- readRDS(paste0(path_weights, "travelW.rds"))
tmp <- tmp[,,143]
tmp <- melt(tmp, na.rm = TRUE)
colnames(tmp) <- c("origin", "destination", "no_travelers")

max(tmp$no_travelers)

plot2 <- ggplot(tmp, aes(destination, origin, fill = no_travelers))+
  geom_tile(color = "white")+
  theme_bw()+
  scale_fill_gradient2(high="red",low="#FFFFCC", na.value="lightgrey",
                       limits = c(1, 400000),
                       breaks = c(1, 100000, 200000, 300000, 400000),
                       labels =   c(1, 100000, 200000, 300000, 400000)) + 
  xlab("")+ #Country of destination
  ylab("")+ #Country of origin
  ggtitle(expression(bold((B)~w[ji]^{(pred)}~("Week 50/2022, adding min values")))) +
  labs(fill = "") +
  theme(legend.justification = c(1, 0),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1, size=6),
        axis.text.y=element_text(size=6),
        plot.title = element_text(face="bold")) +
  coord_fixed() +
  guides(fill = guide_colorbar(barwidth = 0.5))

plot2
rm(tmp)



# ---------------------------------------------------#
## Normalized version of GAM-predict with min values (Week 143) -----
# ---------------------------------------------------#
tmp <- readRDS(paste0(path_weights, "prop_travelW.rds"))
tmp <- tmp[,,143]
tmp <- melt(tmp , na.rm = TRUE)
colnames(tmp) <- c("origin", "destination", "prop_travelers")

max(tmp$prop_travelers) #0.4637971

plot3 <- ggplot(tmp, aes(destination, origin, fill = prop_travelers))+
  geom_tile(color = "white")+
  theme_bw()+
  scale_fill_gradient2(high="red",low="#FFFFCC", na.value="lightgrey",
                       limits = c(0, 1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  xlab("")+ #Country of destination
  ylab("")+ #Country of origin
  ggtitle(expression(bold((C)~w[ji]^{(pred)}~("Week 50/2022, normalized")))) +
  labs(fill = "") +
  theme(legend.justification = c(1, 0),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1, size=6),
        axis.text.y=element_text(size=6),
        plot.title = element_text(face="bold")) +
  coord_fixed() +
  guides(fill = guide_colorbar(barwidth = 0.5))

plot3

# ---------------------------------------------------#
## Normalized version of GAM-predict with neighbour inverse (Week 143) -----
# ---------------------------------------------------#
tmp <- readRDS(paste0(path_weights, "prop_travelW_order.rds"))
tmp <- tmp[,,143]
tmp <- melt(tmp , na.rm = TRUE)
colnames(tmp) <- c("origin", "destination", "prop_travelers")

max(tmp$prop_travelers) #0.6856232

plot4 <- ggplot(tmp, aes(destination, origin, fill = prop_travelers))+
  geom_tile(color = "white")+
  theme_bw()+
  scale_fill_gradient2(high="red",low="#FFFFCC", na.value="lightgrey",
                       limits = c(0, 1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  xlab("")+ #Country of destination
  ylab("")+ #Country of origin
  ggtitle(expression(bold((D)~w[ji]^{(pred)}~x~o[ji]^{-1}~("Week 50/2022, normalized")))) +
  labs(fill = "") +
  theme(legend.justification = c(1, 0),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1, size=6),
        axis.text.y=element_text(size=6),
        plot.title = element_text(face="bold")) +
  coord_fixed() +
  guides(fill = guide_colorbar(barwidth = 0.5))
plot4
rm(tmp)


# ---------------------------------------------------#
## Normalized version of GAM-predict with decay (Week 143) -----
# ---------------------------------------------------#
tmp <- readRDS(paste0(path_weights, "prop_travelW_decay.rds"))
tmp <- tmp[,,143]
tmp <- melt(tmp , na.rm = TRUE)
colnames(tmp) <- c("origin", "destination", "prop_travelers")

max(tmp$prop_travelers) #0.5711206

plot5 <- ggplot(tmp, aes(destination, origin, fill = prop_travelers))+
  geom_tile(color = "white")+
  theme_bw()+
  scale_fill_gradient2(high="red",low="#FFFFCC", na.value="lightgrey",
                       limits = c(0, 1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  xlab("")+ #Country of destination
  ylab("")+ #Country of origin
  ggtitle(expression(bold((E)~w[ji]^{(pred)}~x~o[ji]^{-d}~("Week 50/2022, normalized")))) +
  labs(fill = "") +
  theme(legend.justification = c(1, 0),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1, size=6),
        axis.text.y=element_text(size=6),
        plot.title = element_text(face="bold")) +
  coord_fixed() +
  guides(fill = guide_colorbar(barwidth = 0.5))
plot5
rm(tmp)


#--------------------
# Combined plot
# library("gridExtra")
plotcombine <- arrangeGrob(plot1,plot2,plot3, plot4, plot5, ncol=2, nrow=3)

ggsave(file="weights combined.pdf", plotcombine,
       path = paste0(path_plot,"data_description/"),
       height=14, width=10, units="in", dpi=600)






# -----------------------------------------------------------------------------------------------#
# ====== Weight boxplot ==========
# -----------------------------------------------------------------------------------------------#

# ---------------------------------------------------#
## Prepare data -----
# ---------------------------------------------------#
list_files <-list.files(path = path_weights, pattern = "\\.rds$", full.names = T)
list_files <- list_files[1:5]

weights_rownormalized <- NULL


for (list in list_files){
  
  name <- gsub(list, pattern = "G:/My Drive/Projects/Facebook_Travel Patterns/R/GitHub codes/weights/", replacement = "")
  name <- gsub(name, pattern = ".rds", replacement="")
  
  tmp <- readRDS(list)
  tmp <- as.data.frame.table(tmp) %>%
    select(date_week = Var3, origin = Var1, destination = Var2, prop_travelers = Freq) %>%
    left_join(neighbor_adjusted_long, by=c("origin", "destination")) %>%
    mutate(weight_name = name, 
           ne_order = as.factor(ne_order))
  
  weights_rownormalized <- rbind(weights_rownormalized, tmp)
}


## Extract Power law weights
fit_ri <- get(load(paste0(path_model_fit,"fit_original_ri.rda"))) 
decay_d <- exp(coef(fit_ri)[19])
decay_d

tmp <- neighbor_adjusted^(-decay_d)
diag(tmp) <- 0
tmp <- tmp/rowSums(tmp)

mat_melt <- melt(tmp) %>%
  rename(origin = Var1, destination = Var2, prop_travelers = value) %>%
  full_join(neighbor_adjusted_long, by=c("origin", "destination")) %>%
  mutate_at(c("ne_order"), as.factor) %>%
  mutate(weight_name = "Power law", date_week = NA) %>%
  select(weight_name,date_week, origin, destination, prop_travelers, ne_order)

## Combine weights
weights_rownormalized <- rbind(weights_rownormalized, mat_melt)


# ---------------------------------------------------#
## All weights all neighbour orders boxplot
# ---------------------------------------------------#

## Main manuscript
weights_rownormalized %>% 
  filter(!ne_order == 0) %>%
  filter(weight_name %in% c("prop_travelW", "prop_travelW_decay","prop_travelW_order", "Power law")) %>%
ggplot() + 
  geom_boxplot(aes(x=ne_order, y=prop_travelers, fill=weight_name),lwd=0.5, outlier.size=0.5) +  
  xlab("Neighborhood order") + 
  ylab("Row-normalized weight") +
  theme_bw() +
  theme(legend.position = c(0.9,0.7)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(name = "",
                     guide = "legend",
                     values = c("Power law" = "#ef7b45",
                                "prop_travelW" = "#4f772d" ,
                                "prop_travelW_order" = "#9b5de5",
                                "prop_travelW_decay" = "#f15bb5"), 
                     labels = c("Power law", expression(w[ji]^{(pred)}), 
                                expression(w[ji]^{(pred)}~x~o[ji]^{-1}),
                                expression(w[ji]^{(pred)}~x~o[ji]^{-d})),
                     aesthetics = c("fill", "colour"))

ggsave(filename = "weight_boxplot_rownormalized.pdf",
       path = paste0(path_plot,"data_description/"), units = "in",
       width = 8,   height = 4.5, scale = 1, limitsize = FALSE,   #1 rows
       device="pdf", dpi=600)


## Appendix
weights_rownormalized %>% 
  filter(!ne_order == 0) %>%
  # filter(weight_name %in% c("prop_travelW", "prop_travelW_decay","prop_travelW_order", "Power law")) %>%
  ggplot() + 
  geom_boxplot(aes(x=ne_order, y=prop_travelers, fill=weight_name),lwd=0.5, outlier.size=0.5) +  
  xlab("Neighborhood order") + 
  ylab("Row-normalized weight") +
  theme_bw() +
  theme(legend.position = c(0.9,0.7)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(name = "",
                     guide = "legend",
                     values = c("Power law" = "#ef7b45",
                                "prop_travelW" = "#4f772d" ,
                                "prop_travelW_order" = "#9b5de5",
                                "prop_travelW_decay" = "#f15bb5",
                                "sens_prop_travelW" = "#005f73",
                                "sens_prop_travelW_2023" = "#2ec4b6"
                                ), 
                     labels = c("Power law", expression(w[ji]^{(pred)}), 
                                expression(w[ji]^{(pred)}~x~o[ji]^{-1}),
                                expression(w[ji]^{(pred)}~x~o[ji]^{-d}),
                                expression(w[ji]^{(pred)}~("min value/2")),
                                expression(w[ji]^{(pred)}~("data 2023"))),
                     aesthetics = c("fill", "colour"))



ggsave(filename = "weight_boxplot_rownormalized (appendix).pdf",
       path = paste0(path_plot,"data_description/"), units = "in",
       width = 10,   height = 4.5, scale = 1, limitsize = FALSE,   #1 rows
       device="pdf", dpi=600)


# ---------------------------------------------------#
## Boxplot weights by time by neighbourhood order
# ---------------------------------------------------#
weight <- tibble(weight_name = unique(weights_rownormalized$weight_name)[-6],
                 new_name = c("w_jit^(pred)",
                                "w_jit^(pred) x o_ji^(-d)",
                                "w_jit^(pred) x o_ji^(-1)",
                                "w_jit^(pred) (with min value/2) - sensitivity analysis",
                                "w_jit^(pred) with data 2023 - sensitivity analysis"))
weight$new_name <- factor(weight$new_name, levels = c("w_jit^(pred)",
                                                      "w_jit^(pred) x o_ji^(-1)",
                                                      "w_jit^(pred) x o_ji^(-d)",
                                                      "w_jit^(pred) (with min value/2) - sensitivity analysis",
                                                      "w_jit^(pred) with data 2023 - sensitivity analysis"))


i <- 10
tmp <- weights_rownormalized %>%
  left_join(list_dateweek %>% mutate(date_week = as.factor(date_week)), by="date_week") %>%
  left_join(weight, by = "weight_name") %>%
  filter(!weight_name == "Power law") %>%
  filter(ne_order == i) %>%
  arrange(date_week)

max(tmp$prop_travelers)

ggplot(data=tmp) + 
  geom_boxplot(aes(x=as.factor(week_t), y=prop_travelers),lwd=0.5, outlier.size=0.5) +  
  facet_wrap(~new_name, ncol = 1, scales = "free") +
  xlab("Week of report") + 
  ylab("Row-normalized weight") +
  theme_bw() +
  theme(axis.text.x = element_text(size=5.5, angle = 90, vjust = 0.5, hjust = 0, color="black")) +
  scale_y_continuous(limits = c(0,0.04)) 

ggsave(filename = paste0("weight_order_",i,".pdf"),
       path = paste0(path_plot,"data_description/"), units = "in",
       width = 12,   height = 12, scale = 1, limitsize = FALSE,   #1 rows
       device="pdf", dpi=600)

