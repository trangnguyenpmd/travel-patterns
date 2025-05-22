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


# ---------------------------------------------------------#
# CALCULATE THE COMPONENT CONTRIBUTION -----

## Extract fitted mean by component
fit <- get(load(paste0(path_model_fit,"fit_W_inbound.rda"))) 
summary(fit)

model <- hhh4addon:::terms.hhh4lag(fit) #extract the list of control & hhh4_datalist
mu_coefs <- surveillance:::meanHHH(fit$coefficients, model, total.only = FALSE)

# Component extract by time and country
component_bycountry_array <- array(data = NA, dim = c(143,30,8), 
                                   dimnames = list(as.character(seq(dateweek_start, dateweek_end, by=7)), 
                                                   EU30_countries, 
                                                   c("obs","fitted_mean","ar", "ne", "end","ar.exppred", "ne.exppred", "end.exppred")))

#fill the array
component_bycountry_array[,,"obs"] <- fit$stsObj@observed
component_bycountry_array[,,"fitted_mean"] <- rbind(NA, NA, mu_coefs$mean)
component_bycountry_array[,,"ar"] <- rbind(NA, NA, mu_coefs$epi.own)
component_bycountry_array[,,"ne"] <- rbind(NA, NA, mu_coefs$epi.neighbours)
component_bycountry_array[,,"end"] <- rbind(NA, NA, mu_coefs$endemic)

component_bycountry_array[,,"ar.exppred"] <- rbind(NA, NA, mu_coefs$ar.exppred)
component_bycountry_array[,,"ne.exppred"] <- rbind(NA, NA, mu_coefs$ne.exppred)
component_bycountry_array[,,"end.exppred"] <- rbind(NA, NA, mu_coefs$end.exppred)

## Extract data for plotting
aa <- component_bycountry_array
obs <- as.data.frame(aa[,,"obs"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
fitted_mean <- as.data.frame(aa[,,"fitted_mean"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ar <- as.data.frame(aa[,,"ar"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ne <- as.data.frame(aa[,,"ne"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
end <- as.data.frame(aa[,,"end"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))

ar_exppred <- as.data.frame(aa[,,"ar.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
ne_exppred <- as.data.frame(aa[,,"ne.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))
end_exppred <- as.data.frame(aa[,,"end.exppred"]) %>% mutate(date_week = seq(dateweek_start, dateweek_end, by=7))


# Each component
aa <- lst(fitted_mean, ar, ne, end) %>%
  bind_rows(.id = "comp") %>%
  reshape2::recast(date_week + variable ~ comp, id.var = c('comp', 'date_week')) %>%
  rename(country = variable) %>% 
  mutate(percent_end = end/fitted_mean*100,
         percent_ar = ar/fitted_mean*100,
         percent_ne = ne/fitted_mean*100) %>%
  select(date_week, country, percent_ar, percent_ne, percent_end) %>%
  pivot_longer(cols= c("percent_ar", "percent_ne", "percent_end"), 
               names_to='Component', values_to='percent') %>%
  group_by(date_week, Component) %>%
  mutate(#mean = mean(percent, na.rm = T),
    median = median(percent, na.rm = T),
    upper = quantile(percent,probs = 0.975, na.rm = T),
    lower = quantile(percent,probs = 0.025, na.rm = T)) %>% ungroup() 

plot1 <- ggplot() + 
  #geom_line(data=aa, aes(x=date_week, y=percent, group=Component), color="#0077b6", linewidth=0.4, alpha=0.5) +
  #geom_line(data=aa, aes(x=date_week, y=upper, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  #geom_line(data=aa, aes(x=date_week, y=lower, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  geom_ribbon(data = aa %>% filter(Component == "percent_ar"), 
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#0077b6", alpha = 0.3) +
  geom_line(data = aa %>% filter(Component == "percent_ar"), 
            aes(x=date_week, y=median, group=Component), color="#0077b6", linewidth=0.6) + 
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("(A) The autoregressive component (%) across 30 EU/EEA countries") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold")) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,100))
plot1

plot2 <- ggplot() + 
  #geom_line(data=aa, aes(x=date_week, y=percent, group=Component), color="#0077b6", linewidth=0.4, alpha=0.5) +
  #geom_line(data=aa, aes(x=date_week, y=upper, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  #geom_line(data=aa, aes(x=date_week, y=lower, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  geom_ribbon(data = aa %>% filter(Component == "percent_ne"), 
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#f15bb5", alpha = 0.3) +
  geom_line(data = aa %>% filter(Component == "percent_ne"), 
            aes(x=date_week, y=median, group=Component), color="#f15bb5", linewidth=0.6) + 
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("(B) The neighbourhood component (%) across 30 EU/EEA countries") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold")) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,100))
plot2

plot3 <- ggplot() + 
  #geom_line(data=aa, aes(x=date_week, y=percent, group=Component), color="#0077b6", linewidth=0.4, alpha=0.5) +
  #geom_line(data=aa, aes(x=date_week, y=upper, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  #geom_line(data=aa, aes(x=date_week, y=lower, group=Component),color="#0077b6", linewidth=0.6, linetype = "dashed") +
  geom_ribbon(data = aa %>% filter(Component == "percent_end"), 
              aes(ymin=lower, ymax=upper, x=date_week), fill = "#00f5d4", alpha = 0.3) +
  geom_line(data = aa %>% filter(Component == "percent_end"), 
            aes(x=date_week, y=median, group=Component), color="#00f5d4", linewidth=0.6) + 
  xlab("Week of report") + 
  ylab("Percentage") +
  ggtitle("(C) The endemic component (%) across 30 EU/EEA countries") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        axis.text.x=element_text(angle = 0, vjust = 0.5, hjust=0.5),
        plot.title = element_text(face="bold")) +
  theme(legend.position="none") + 
  scale_x_date(limits = c(date_start_plot, date_end_plot),
               date_breaks  ="3 month",
               date_minor_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,100))

plot3


## combine plots
library("cowplot")
plotcombine <- plot_grid(plot1,plot2,plot3, ncol = 1, align = "v", axis = "b")
ggsave(file="fit_W_inbound_country median.pdf", plotcombine, 
       path = paste0(path_plot,"model_fit_plot/"), 
       height=9, width=7.5, units="in")



# ---------------------------------------------------------#
# CALCULATE FROM-TO CASES -----

## Loading the model
fit <- get(load(paste0(path_model_fit,"fit_W_inbound.rda"))) 
summary(fit)

## Calculate the From - To
tmp <- setDT(data.frame(decompose_epidemic_component(fit)))

tmp$date_week <- list_dateweek$date_week

tmp_melt <- melt(tmp,id.var = "date_week")[,':=' (origin=str_extract(variable, "(?<=from\\.)[^.]+"),
                                                  destination=str_extract(variable, "(?<=to\\.)[^.]+"))]
tmp_sum <- tmp_melt[,
                    .(value=sum(value)),
                    .(date_week, origin, destination)]

tmp_total <- tmp_sum %>%
  left_join(neighbor_adjusted_long, by=c("origin"="origin", "destination"="destination")) %>%
  group_by(ne_order, origin, destination) %>%
  summarise(total = sum(value, na.rm = TRUE))

tmp_ne <- tmp_total %>%
  filter(!ne_order == 0) %>%
  group_by(destination, ne_order) %>%
  summarise(total = sum(total)) %>%
  group_by(destination) %>%
  mutate(percent = total/sum(total)*100)

plot2 <- ggplot(data = tmp_ne, aes(x = as.factor(ne_order), y = destination, fill = log(percent))) + 
  geom_tile() +
  scale_fill_viridis(discrete = FALSE, limit = c(-5,5), name="") +
  # labs(title = "The log proportion of the estimated number of cases from the neighbourhood component \n per destination country stratified by the neighbourhood order of its origin country", 
  #      # subtitle = "Percentage are shown in log scale", 
  #      x = "Neighbourhood order of origin country", y = "Destination country") + 
  labs(title = "The log proportion of the estimated number of cases in destination country \n imported from origin countries stratified by neighbourhood order",
       x = "Neighbourhood order of origin country", y = "Destination country") + 
  theme_bw() +
  theme(axis.text=element_text(colour="black"),
        plot.title = element_text(face = "bold"),
        legend.position = "top") +
  guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5))
plot2

ggsave(file="fit_W_inbound_NE percentage.pdf", plot2, 
       path = paste0(path_plot,"model_fit_plot/"), 
       height=7.5, width=8.5, units="in")



# aa <- tmp_total %>%
#   filter(!ne_order == 0) %>%
#   group_by(origin) %>%
#   mutate(percent = total/sum(total)*100)
# 
# plot1 <- ggplot(data = aa) + 
#   geom_tile(aes(x = destination, y = origin, fill = log(percent))) +
#   scale_fill_viridis(discrete = FALSE,limit = c(-5.8,4.6), name="") +
#   labs(x = "Destination country", y = "Origin country",
#        title = "(A) Contribution (%) of neighbourhood component",
#        subtitle = "Percentages are shown in log scale") + 
#   theme_bw() +
#   theme(axis.text=element_text(colour="black"),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(face = "bold"),
#         legend.position = "top") +
#   guides(fill = guide_colorbar(barwidth = 15, barheight = 0.5))


#----------------------------------------------------#
# Map of random intercepts -----
#----------------------------------------------------#

aa <- ranef %>%
  filter(model %in% c("fit_W_inbound")) %>%
  mutate(component = ifelse(component == "ar", "Autoregressive random intercepts",
                            ifelse(component == "ne", "Neighbourhood random intercepts",
                                   "Endemic random intercepts" ))) %>%
  left_join(map_europe, by="country") %>%
  st_as_sf()

ggplot(data = aa %>% filter(model == "fit_W_inbound")) +
  geom_sf(aes(fill = `ranef(fit)`), size = 0.15, color = "black") +
  facet_wrap(.~component, ncol=3) +
  geom_sf_text(data = map_centroid, aes(label = country), size = 3, color="black") +
  scale_fill_gradient2(low = "#0081a7", mid="#ffffff", high = "#fb5607", limits = c(-3,3)) +
  labs(x="", y="", fill = "value") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face="bold"))

ggsave(filename = "map_random intercepts.pdf",
       path = paste0(path_plot, "model_fit_plot/"),
       units = "in",  width = 15, height = 7, 
       device="pdf", dpi=600)

