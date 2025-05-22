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
### Document: DATA DESCRIPTION & PLOTTING
### Author: trangngpmd
### Date: 2024-10-09




# --------------------------------------------------------------------#
# ======= EPI CURVES - STRINGENCY - VACCINATION -  VARIANTS ==========
# --------------------------------------------------------------------#

# -------------------------------------------#
## Epicurve -----
# -------------------------------------------#
plot1 <- cases_owid_week_long %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  group_by(date_week) %>%
  summarise(cases_wk = sum(cases_wk)) %>%
  mutate(caseswk_per_million = cases_wk/455993375*1000000) %>%
  ggplot() +
  geom_area(aes(x = date_week, y = cases_wk), fill="#003049") +
  xlab("Week of report") +
  ylab("New cases") +
  ggtitle("(A) Number of reported cases, aggregated over all 30 EU/EEA countries, W13/2020 - W50/2022") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0))
plot1

# -------------------------------------------#
## Stringency Index -----
# -------------------------------------------#
tmp <- str_index_week_long %>%
  group_by(date_week) %>%
  mutate(median = median(mean_stringency, na.rm = T),
         upper = quantile(mean_stringency,probs = 0.975, na.rm = T),
         lower = quantile(mean_stringency,probs = 0.025, na.rm = T))
plot2 <- ggplot(data = tmp) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=date_week), fill = "blue", alpha = 0.3) +
  geom_line(aes(x=date_week, y=median), color="blue", linewidth=0.6) + 
  xlab("Week of report") +
  ylab("Mean of the Stringency Index") +
  ggtitle("(B) Weekly mean of the Stringency Index, across 30 EU/EEA countries, W11/2020 - W50/2022",
          subtitle = "The solid line is the median of weekly means, the ribbon is the 95% quantiles of the weekly means accross EU/EEA countries") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold")) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,100))
plot2
rm(tmp)

# -------------------------------------------#
## Vaccination raw -----
# -------------------------------------------#
tmp <- vaccination_clean %>%
  select(date_week, Country = country, 
         primarycourse = percent_dose2nd,
         booster1 = percent_booster1, 
         booster2 = percent_booster2, 
         booster3 = percent_booster3) %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  melt(id.vars=c("date_week", "Country")) %>%
  group_by(date_week, variable) %>%
  mutate(median = median(value, na.rm = T),
         upper = quantile(value,probs = 0.975, na.rm = T),
         lower = quantile(value,probs = 0.025, na.rm = T))

plot3 <- ggplot() +
  geom_ribbon(data = tmp, aes(x=date_week, ymin=lower, ymax=upper, group = variable, fill = variable), alpha = 0.3) +
  geom_line(data = tmp, aes(y=median, x=date_week, group=variable, colour=variable),linewidth=0.6) + 
  #geom_line(data = tmp %>% filter(Country == "Belgium"), aes(y=value, x=date_week, colour=variable),linewidth=0.6, linetype="dashed") +
  xlab("Week of report") +
  ylab("Vaccination coverage") +
  ggtitle("(C) Raw vaccination coverage, across 30 EU/EEA countries, W50/2020 - W50/2022",
          subtitle = "The solid lines are the median of weekly percentages, the ribbons are the 95% quantiles of the weekly percentages accross EU/EEA countries") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = c(0.1,0.5)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               #limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 1, by = 0.2),
                     limits=c(0,1), labels = scales::percent)+
  scale_color_manual(name = "Dose",
                     guide = "legend",
                     values = c("primarycourse" = "#ef7b45",
                                "booster1" = "#0077b6" ,
                                "booster2" = "#f15bb5",
                                "booster3" = "#00f5d4"), 
                     labels = c('Primary course',
                                "Booster 1"  ,
                                "Booster 2", 
                                "Booster 3" ),
                     aesthetics = c("fill", "colour"))
plot3
rm(tmp)


# -------------------------------------------#
## Variant -----
# -------------------------------------------#
tmp <- variants_wide %>%
  filter(between(date_week, dateweek_start, dateweek_end)) %>%
  group_by(date_week) %>%
  summarise(total_allsequences = sum(number_sequenced_known_variant, na.rm = TRUE),
            total_alpha = sum(Alpha, na.rm = TRUE), 
            total_delta = sum(Delta, na.rm = TRUE), 
            total_omicron = sum(Omicron, na.rm = TRUE)) %>%
  mutate(Alpha = if_else(total_allsequences==0,0,
                         total_alpha/total_allsequences),
         Delta = if_else(total_allsequences==0,0,
                         total_delta/total_allsequences),
         Omicron = if_else(total_allsequences==0,0,
                           total_omicron/total_allsequences)) %>%
  mutate(Others = 1 - (Alpha + Delta + Omicron)) %>%
  select(date_week, Alpha, Delta, Omicron, Others) %>%
  pivot_longer(cols=c('Alpha', 'Delta', "Omicron", "Others"),
               names_to='Variants', values_to='Percentage')


plot4 <- ggplot(data = tmp) +
  #geom_col(aes(x = date_week, y = Percentage, fill = Variants)) +
  geom_line(aes(x = date_week, y = Percentage, color = Variants, linetype=Variants), linewidth=1) +
  ylab("Percentage") +
  xlab("Week of report") +
  ggtitle("(D) Variants circulation, aggregated over all 30 EU/EEA countries, W13/2020 - W50/2022") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text=element_text(colour="black"), 
        plot.title = element_text(face = "bold"),
        legend.position= c(0.1,0.5)) +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") 
plot4
rm(tmp)

## Combine plots -----
plotcombine <- plot_grid(plot1, plot2, plot3, plot4, ncol = 1, align = "v", axis = "b")
ggsave(file="data combine.pdf", plotcombine, 
       path = paste0(path_plot,"data_description/"), 
       height=12.5, width=10.5, units="in")




# --------------------------------------------------------------------#
# ==================== DATA VACCINATION ==============================
# --------------------------------------------------------------------#

# -------------------------------------------#
## Vaccination waning by time -----
# -------------------------------------------#
tmp <- vaccination_waned %>%
  select(date_week, Country = country, 
         waned_primarycourse = cov_waned_2nd, 
         waned_booster1 = cov_waned_3rd,
         waned_booster2 = cov_waned_4th,
         waned_booster3 = cov_waned_5th,
         waned_overall = cov_waned) %>%
  melt(id.vars=c("date_week", "Country")) %>%
  group_by(date_week, variable) %>%
  mutate(median = median(value, na.rm = T),
         upper = quantile(value,probs = 0.975, na.rm = T),
         lower = quantile(value,probs = 0.025, na.rm = T))

plot1 <- ggplot(data = tmp) +
  geom_ribbon(aes(x=date_week, ymin=lower, ymax=upper, group = variable, fill = variable), alpha = 0.3) +
  geom_line(aes(y=median, x=date_week, group=variable, colour=variable),linewidth=0.6) + 
  xlab("Week of report") +
  ylab("Vaccination coverage") +
  ggtitle("(A) Vaccination coverage with immunity waning across countries") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top") +
  scale_x_date(limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 1, by = 0.2),
                     limits=c(0,1), labels = scales::percent)+
  scale_color_manual(name = "",
                     guide = "legend",
                     values = c("waned_primarycourse" = "#ef7b45",
                                "waned_booster1" = "#0077b6" ,
                                "waned_booster2" = "#f15bb5",
                                "waned_booster3" = "#00f5d4",
                                "waned_overall" = "#283618" ), 
                     labels = c('Primary course (waned)',
                                "Booster 1 (waned)"  ,
                                "Booster 2 (waned)", 
                                "Booster 3 (waned)", 
                                "Overall coveraged (waned)"),
                     aesthetics = c("fill", "colour"))
plot1
rm(tmp)


# -------------------------------------------#
## Vaccination waning by country -----
# -------------------------------------------#
tmp <- vaccination_waned %>%
  select(date_week, Country = country, 
         waned_primarycourse = cov_waned_2nd, 
         waned_booster1 = cov_waned_3rd,
         waned_booster2 = cov_waned_4th,
         waned_booster3 = cov_waned_5th,
         waned_overall = cov_waned) %>%
  melt(id.vars=c("date_week", "Country")) %>%
  filter(variable == "waned_overall")


plot2 <- ggplot(data = tmp, aes(date_week, y = Country, fill = value)) + 
  geom_tile() +
  scale_fill_viridis(discrete = FALSE, limit = c(0,1), name="")+
  labs(title = "(B) The overall vaccination coverage with immunity waning by country",
       x = "Week of report", y = "") +
  theme_bw() + 
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top") +
  scale_x_date(limits = c(as.Date("2020-12-01"), as.Date("2022-12-31")),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5))

plot2
rm(tmp)


## Combine plots -----
#library("cowplot")
plotcombine <- plot_grid(plot1, plot2, ncol = 1, align = "v", axis = "b")
ggsave(file="vaccine combined.pdf", plotcombine, 
       path = paste0(path_plot,"data_description/"), 
       height=10, width=10, units="in")

# -------------------------------------------#
## Waning function -----
# -------------------------------------------#

tmp <- data.frame(waning_rate = waning(0 : 52, val = 1),
                  week = c(0:52))

ggplot(data = tmp, aes(x=week,y = waning_rate)) +
  geom_line() + 
  geom_point() +
  theme_bw() + 
  xlab("Weeks passed since vaccine given") + 
  ylab("Waning rate") +
  scale_x_continuous(breaks=c(0:52), limits = c(0, 52))

ggsave(filename = "waning function.pdf",
       path = paste0(path_plot,"data_description/"), 
       units = "in",
       width = 10, height = 3.5,
       device="pdf", dpi=600)


rm(tmp)


# --------------------------------------------------------------------#
# ================= NEIGHBOURHOOD MATRICES ===========================
# --------------------------------------------------------------------#

# -------------------------#
## Neighbour original -----
# -------------------------#
# New way to plot the matrix 
# The COVID-19 vaccination campaign in Switzerland and its impact on disease spread
# https://www.sciencedirect.com/science/article/pii/S1755436524000069

tmp <- neighbor_original %>%
  melt() %>%
  mutate_at(c("value"), as.factor)

plot1 <- ggplot(data = tmp, aes_string(x = names(tmp)[1], 
                                   y = names(tmp)[2], fill = names(tmp)[3])) + 
  geom_tile() +
  labs(x = "", y = "", title = "(A) Neighbourhood matrix (original)", fill = "Order") +
  geom_text(aes(x=Var2, y=Var1), label=tmp$value, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        title = element_text(face = "bold")) +
  scale_fill_viridis_d(direction = - 1) +
  coord_fixed()

plot1

rm(tmp)


# -------------------------#
## Neighbour adjusted -----
# -------------------------#
tmp <- neighbor_adjusted %>%
  melt() %>%
  mutate_at(c("value"), as.factor)

plot2 <- ggplot(data = tmp, aes_string(x = names(tmp)[1], 
                                       y = names(tmp)[2], fill = names(tmp)[3])) + 
  geom_tile() +
  labs(x = "", y = "", title = "(B) Neighbourhood matrix (adjusted)", fill = "Order") +
  geom_text(aes(x=Var2, y=Var1), label=tmp$value, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        title = element_text(face = "bold")) +
  scale_fill_viridis_d(direction = - 1) +
  coord_fixed()

plot2

rm(tmp)


# -------------------------#
## 1st order connection -----
# -------------------------#
mat_melt <- neighbor_original %>%
  melt() %>%
  select(origin = Var1, destination = Var2, order = value)
  
map_connection_original <- mat_melt %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("origin" = "country")) %>%
  rename("X_origin" = "X", "Y_origin" = "Y") %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("destination" = "country")) %>%
  rename("X_destination" = "X", "Y_destination" = "Y") %>%
  select(origin, destination, order, X_origin, Y_origin, X_destination, Y_destination)


mat_melt <- neighbor_adjusted %>%
  melt() %>%
  select(origin = Var1, destination = Var2, order = value)

map_connection <- mat_melt %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("origin" = "country")) %>%
  rename("X_origin" = "X", "Y_origin" = "Y") %>%
  left_join(map_centroid[,c("country", "X", "Y")], by=c("destination" = "country")) %>%
  rename("X_destination" = "X", "Y_destination" = "Y") %>%
  select(origin, destination, order, X_origin, Y_origin, X_destination, Y_destination)


i <- 1 # ranging from 1 to 10 (adjusted neighbourhood matrix) or from 1 to 8 (original neighbourhood matrix)
plot3 <- ggplot() +
  geom_sf(data = map_europe, size = 0.15, color = "black", fill = "#eef0f2") +
  geom_link(data = map_connection %>% filter(order == i), 
            aes(x=X_origin, y = Y_origin,
                xend = X_destination, yend = Y_destination), color="red") +
  geom_link(data = map_connection_original %>% filter(order == i),
            aes(x=X_origin, y = Y_origin,
                xend = X_destination, yend = Y_destination)) +
  geom_point(data = map_centroid, aes(x=X, y= Y), color = "red", size=1) +
  geom_sf_text(data = map_centroid, aes(label = country), size = 3, color="black") +
  labs(x="", y="") +
  ggtitle(paste0("(C) The ",i,"st-order neighbor"))+
  #xlim(-20,35) + ylim(34,66)+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold"))

plot3


## Combine plots -----
#library("cowplot")
plotcombine <- plot_grid(plot1, plot2, ncol = 1)
plotcombine <- plot_grid(plotcombine, plot3, ncol = 2)
ggsave(file="neighbour combined.pdf", plotcombine, 
       path = paste0(path_plot,"data_description/"), 
       height=10, width=10, units="in")



