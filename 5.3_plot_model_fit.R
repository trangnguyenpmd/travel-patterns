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
### Document: PLOT MODEL FIT
### Author: trangngpmd
### Date: 2024-11-21



## Set up dates
date_start_plot <- as.Date("2020-02-01")
date_end_plot <- as.Date("2023-02-01")

component_extracted <- readRDS(paste0(path_model_fit, "parameters/component_extracted.rds"))

# ----------------------------------------------------------------#
# COMPONENT CONTRIBUTION FROM THE ORIGINAL FIT-----
# ----------------------------------------------------------------#


# -------------------------------------#
##  All countries -----
# -------------------------------------#

# Overall fit
tmp <- component_extracted %>%
  select(date_week, ar, ne, end, model_name) %>%
  filter(!model_name %in% c("sensitivity fit_W", "sensitivity fit_W_2023"), !is.na(model_name)) %>%
  pivot_longer(cols= c("ar", "ne", "end"),
               names_to='Component',
               values_to='estimated')

plot1 <- ggplot() +
  geom_area(data = tmp, aes(x=date_week, y=estimated, fill=Component)) +
  geom_point(data = component_extracted %>% filter(!model_name %in% c("sensitivity fit_W", "sensitivity fit_W_2023"), !is.na(model_name)), 
             aes(x=date_week, y=obs), 
             stat = "identity", color="black", fill="black", size=0.5) + 
  facet_wrap(~ model_name, scales = "fixed", ncol = 2) + 
  theme_bw() +
  xlab("Week of report") +
  ylab("No.cases") +
  ggtitle("(A) The overall fitted model") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold'),
        legend.background = element_rect(fill='transparent')) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "1 month",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,10000000)) +
  scale_fill_manual(name = "",   guide = "legend",
                    values = c("ar" = "#0077b6" ,
                               "end" = "#00f5d4",
                               "ne" = "#f15bb5"), 
                    labels = c("Autoregressive"  ,
                               "Endemic", 
                               "Neighbourhood"))

plot1



# Percentage contribution
tmp <- component_extracted %>%
  filter(!model_name %in% c("sensitivity fit_W", "sensitivity fit_W_2023"), !is.na(model_name)) %>%
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100) %>%
  select(date_week, percent_ar, percent_ne, percent_end, model_name) %>%
  pivot_longer(cols= c("percent_ar", "percent_ne", "percent_end"), 
               names_to='Component', values_to='percent') %>%
  group_by(date_week, Component) %>%
  mutate(#mean = mean(percent, na.rm = T),
    median = median(percent, na.rm = T),
    upper = quantile(percent,probs = 0.975, na.rm = T),
    lower = quantile(percent,probs = 0.025, na.rm = T)) %>% ungroup() 

plot2 <- ggplot() + 
  geom_ribbon(data = tmp %>% filter(Component == "percent_ar"),
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#0077b6", alpha = 0.3) +
  geom_line(data = tmp %>% filter(Component == "percent_ar"), 
            aes(x=date_week, y=median, group=Component, color="percent_ar"), linewidth=0.4) +
  geom_ribbon(data = tmp %>% filter(Component == "percent_ne"),
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#f15bb5", alpha = 0.3) +
  geom_line(data = tmp %>% filter(Component == "percent_ne"), 
            aes(x=date_week, y=median, group=Component,color="percent_ne"), linewidth=0.4)  +
  geom_ribbon(data = tmp %>% filter(Component == "percent_end"),
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#00f5d4", alpha = 0.3) +
  geom_line(data = tmp %>% filter(Component == "percent_end"),
            aes(x=date_week, y=median, group=Component, color="percent_end"), linewidth=0.4) +
  # facet_wrap(~ model, scales = "free", ncol = 3) +
  theme_bw() +
  xlab("Week of report") +
  ylab("Percentage") +
  ggtitle("(B) Overall component contribution (%)",
          subtitle = "The solid lines are the medians, the ribbons are the 95% quantiles of the weekly percentages") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title=element_text(face='bold'),
        legend.background = element_rect(fill='transparent')) +
  theme(legend.position="top") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="1 year",
               date_minor_breaks = "3 month",
               date_labels = "%Y") +
  scale_color_manual(name = "Component",   guide = "legend",
                     values = c("percent_ar" = "#0077b6" ,
                                "percent_end" = "#00f5d4",
                                "percent_ne" = "#f15bb5"), 
                     labels = c("Autoregressive"  ,
                                "Endemic", 
                                "Neighbourhood" ))

plot2

## combine plots
# library(ggpubr)
plotcombine <- ggarrange(plot1, plot2, heights = c(2, 1), ncol = 1, nrow = 2, align = "v")

ggsave(file="fitted overall.pdf", plotcombine, 
       path = paste0(path_plot,"model_fit_plot/"), device="pdf",
       height=10, width=8, units="in")

# -------------------------------------------------------------#
## Calculate the contribution percentage by country -----
# -------------------------------------------------------------#
model_name <- unique(data_score$model)[2:8]

component_bycountry <- NULL

for(i in model_name){

component_bycountry_array <- readRDS(paste0(path_model_fit, 
                                            "parameters/component_extracted_bycountry_",i,".rds"))

tmp <- component_bycountry_array
obs <- as.data.frame(tmp[,,"obs"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
fitted_mean <- as.data.frame(tmp[,,"fitted_mean"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ar <- as.data.frame(tmp[,,"ar"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ne <- as.data.frame(tmp[,,"ne"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
end <- as.data.frame(tmp[,,"end"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))

ar_exppred <- as.data.frame(tmp[,,"ar.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ne_exppred <- as.data.frame(tmp[,,"ne.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
end_exppred <- as.data.frame(tmp[,,"end.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))


tmp <- lst(ar, ne, end, fitted_mean) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable) %>%
  group_by(country) %>%
  summarise(total_ar = sum(ar, na.rm = T),
            total_ne =sum(ne, na.rm = T), 
            total_end = sum(end, na.rm = T), 
            total_fitted = sum(fitted_mean, na.rm = T)) %>%
  mutate(percent_ar = total_ar/total_fitted,
         percent_ne = total_ne/total_fitted,
         percent_end = total_end/total_fitted, 
         model = i)

component_bycountry <- rbind(component_bycountry, tmp)

}

write_xlsx(tmp, paste0(path_model_fit,"parameters/percent_bycountry.xlsx" ))


# Boxplot percentage
tmp <- component_bycountry %>%
  filter(model=="fit_W_inbound")

# Autoregressive component
plot1 <- ggplot() +
  geom_boxplot(data = component_bycountry, 
               aes(x = country, y=percent_ar*100), lwd=0.5, outlier.size=0.5) + 
  geom_point(data = tmp, aes(x = country, y = percent_ar*100), color = "red", size = 0.5) +
  geom_text_repel(data = tmp, aes(x = country, y = percent_ar*100,label = round(percent_ar*100,1)), 
                  max.overlaps=30, size =2.5) + 
  labs(x = "", y = "Autoregressive component (%)") +
  theme_bw() +
  coord_flip()
plot1


# Neighborhood component

plot2 <- ggplot() +
  geom_boxplot(data = component_bycountry, 
               aes(x = country, y=percent_ne*100), lwd=0.5, outlier.size=0.5) + 
  geom_point(data = tmp, aes(x = country, y = percent_ne*100), color = "red", size = 0.5) +
  geom_text_repel(data = tmp, aes(x = country, y = percent_ne*100,label = round(percent_ne*100,1)), 
                  max.overlaps=30, size =2.5) + 
  labs(x = "", y = "Neighbourhood component (%)") +
  theme_bw() +
  coord_flip()
plot2

# Endemic component

plot3 <- ggplot() +
  geom_boxplot(data = component_bycountry, 
               aes(x = country, y=percent_end*100), lwd=0.5, outlier.size=0.5) + 
  geom_point(data = tmp, aes(x = country, y = percent_end*100), color = "red", size = 0.5) +
  geom_text_repel(data = tmp, aes(x = country, y = percent_end*100,label = round(percent_end*100,1)), 
                  max.overlaps=30, size =2.5) + 
  labs(x = "", y = "Endemic component (%)") +
  theme_bw() +
  coord_flip()

plot3



## combine plots
# library("gridExtra")
plotcombine <- arrangeGrob(plot1, plot2, plot3, ncol=1, nrow=3)

ggsave(file="component_comtribution_bycountry.pdf", plotcombine, 
       path = paste0(path_plot,"model_fit_plot/"), device="pdf",
       height=12, width=8, units="in")

