
library(RPresence)
library(jagsUI)
library(rjags)
library(dplyr)

# load data ----

## only data collected in Doubtful or Dusky complexes
## seasonal ch
everyone_ch_SA<-readRDS("./data/everyone_SA.RDS")
long_samp_ch_SA<-readRDS("./data/long_samp_SA.RDS") #skip 2007.67
ID_per_day_SA<-readRDS("./data/ID_per_day_SA.RDS") 

# run model ----

source('./scripts/model_run.R', local = TRUE)$value

# save results ----
saveRDS(out1_df, file = paste0("./data/survival&cap_SA",Sys.Date(),".rds"))

# Load results ----
date = "2025-01-29"
date = "2026-03-31"
results_in_SA<-readRDS(paste0("./data/survival&cap_SA",date,".rds"))

results_SA<-as.data.frame(summary(results_in_SA))
results_SA
min(results_SA$ess_bulk, na.rm = T)
max(results_SA$rhat, na.rm = T)

bayesplot::mcmc_trace(results_in_SA, pars = c("beta[1]", "beta[2]","alpha[1]","alpha[2]","sigma[1]","sigma[2]")) 

results_SA%>%
  filter(grepl("sigma", variable))%>%
  mutate(sigma2 = median^2)

results_SA%>%
  filter(grepl("alpha", variable))%>%
  mutate(sigma2 = median^2)

beta_med<-results_SA%>%
  filter(grepl("beta", variable))%>%
  mutate(inv_logit_beta_med = 1/(1+exp(-median)),
         pod = c("DOUBTFUL","DUSKY"))

results_SA%>%
  filter(grepl("phi.est", variable))

results_SA%>%
  filter(grepl("epsilon", variable))

## surival prob # not identifiable at last occasion
occasions_SA<-names(long_samp_ch_SA)

ID_per_day_SA$year_season_code<-as.character(ID_per_day_SA$year_season_code)

results_phi_SA<-results_SA%>%
  filter(grepl("phi.est", variable))%>%
  mutate(calfyr_season = (rep(names(long_samp_ch_SA)[2:(n_occ)], each = 2)), # skip 2006.07
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")

summary(results_phi_SA)
  
library(ggplot2)

## capture prob # not identifiable at first occasion

results_p_SA<-results_SA%>%
  filter(grepl("p.est", variable))%>%
  mutate(calfyr_season = rep(names(long_samp_ch_SA)[3:(n_occ+1)], each = 2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))%>%
  mutate(area = "Complexes only")

###

N_SA<-results_SA%>%
  filter(grepl("Doubtful_N", variable) | grepl("Dusky_N", variable))%>%
  mutate(calfyr_season = rep(rep(names(long_samp_ch_SA)[2:(n_occ+1)], each = 1),2),
         Pod = c(rep("DOUBTFUL",56),rep("DUSKY",56)))%>%
  mutate(Season = case_when(
    grepl(".33", calfyr_season) ~ "Summer",
    grepl(".67", calfyr_season) ~ "Winter",
    TRUE ~ "Spring"
  ))%>%
  left_join(ID_per_day_SA, by = c("calfyr_season" = "year_season_code", "Pod" = "POD","Season" = "season"))%>%
  mutate(eff = case_when(
    IDperDay != 0 ~ "effort",
    TRUE ~ "no effort"))

sumstats_n_SA<-N_SA%>%
  filter(median > 0)%>%
  group_by(Pod)%>%
  dplyr::summarise(q5_N = quantile(median, 0.05), med_N = median(median), q95_N = quantile(median, 0.95), min_census = min(n.y), med_census = median(n.y), max_census = max(n.y), min_q5 = min(q5), max_q95 = max(q95))

N_SA%>%
  filter(calfyr_season == 2023.67)

ggplot(N_SA%>%filter(median > 0))+
  geom_hline(data = sumstats_n_SA, mapping = aes(yintercept = med_N, color = Pod), linetype = "dashed")+
  geom_rect(data = sumstats_n_SA, mapping = aes(ymin = q5_N, ymax = q95_N, xmin = 2005.33, xmax = 2023.67, fill = Pod), alpha = 0.2)+
  geom_ribbon(aes(ymin = q5, ymax = q95, x = as.numeric(calfyr_season), color = Pod, fill = Pod), alpha = 0.8)+
  geom_path(aes(x = as.numeric(calfyr_season), y = median, group = Pod), color = "black", alpha = 0.8)+
  theme_bw()+
  xlab("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")")+
  ylab("Abundance")+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))

ggsave(paste0("./figures/abund_SA_ribbon_",date,".png"), dpi = 300, width = 300, height = 150, units = "mm")

results_phi<-results_phi_SA

head(results_phi)

results_p<-results_p_SA%>%
  filter(!(pod == "DUSKY" & calfyr_season == 2007.00))

head(results_p)

results_phi$season<-as.factor(results_phi$season)

phi_together<-ggplot(results_phi%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), alpha = 0.4)+
  geom_point(aes(fill = Season), shape = 21, alpha = 0.8)+
  geom_hline(beta_med, mapping = aes(yintercept = inv_logit_beta_med), linetype = "dashed", color = "red", alpha = 0.8)+
  facet_wrap(~pod)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.box.background = element_rect(fill='white'),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(color = "white"))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Survival probability,' *phi))+
  scale_fill_manual(values = c("Spring" = "orange", "Summer" = "lightgrey", "Winter" = "blue"))

phi_together

ggsave(paste0('./figures/phi_together_',date,'.png'), phi_together, dpi = 300, width = 200, height = 100, units = "mm")

p_together<-ggplot(results_p%>%filter(eff != "no effort"), aes(x = as.numeric(calfyr_season), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), alpha = 0.4)+
  geom_point(aes(fill = Season), shape = 21, alpha = 0.8)+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom",
        legend.box.background = element_rect(fill='white'),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(color = "white"))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))+
  scale_fill_manual(values = c("Spring" = "orange", "Summer" = "lightgrey", "Winter" = "blue"))

p_together

ggsave(paste0('./figures/p_together_',date,'.png'), p_together, dpi = 300, width = 200, height = 100, units = "mm")

phi_p_together<-ggpubr::ggarrange(p_together,phi_together, common.legend = T, labels = "auto", ncol = 1, legend = "bottom", align = "v")

ggsave(paste0('./figures/phi_p_together2_',date,'.png'), phi_p_together, dpi = 300, width = 250, height = 200, units = "mm")

####
p_box<-ggplot(results_p%>%filter(eff != "no effort"), aes(x = as.factor(Season), y = median))+
  geom_boxplot(aes(fill = Season), alpha = 0.5)+
  #geom_jitter(aes(color = Season))+
  facet_wrap(~pod)+
  ylab(expression('Capture probability, p'))+
  xlab("Season")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("Spring" = "orange", "Summer" = "lightgrey", "Winter" = "blue"))

ggsave(paste0('./figures/p_box_',date,'.png'), p_box, dpi = 300, width = 200, height = 100, units = "mm")

####
phi_box<-ggplot(results_phi%>%filter(eff != "no effort"), aes(x = as.factor(Season), y = median))+
  geom_boxplot(aes(fill = Season), alpha = 0.5)+
  #geom_jitter(aes(color = Season))+
  facet_wrap(~pod)+
  ylab(expression('Survival probability,' *phi))+
  xlab("Season")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("Spring" = "orange", "Summer" = "lightgrey", "Winter" = "blue"))

ggsave(paste0('./figures/phi_box_',date,'.png'), phi_box, dpi = 300, width = 200, height = 100, units = "mm")

###
p_SA<-results_p_SA%>%filter(eff != "no effort")

# complexes only for capture days
library(ggridges)
season_point<-ggplot(results_p_SA%>%filter(eff != "no effort"), aes(y = median, x = n.x, fill = Season))+
  geom_point(shape = 21, size = 3, alpha = 0.5)+
  theme_bw()+
  theme(legend.position = "none")+
  xlab("# capture days")+
  ylab(expression('Capture probability, p'))+
  scale_fill_manual(values = c("Spring" = "orange", "Summer" = "lightgrey", "Winter" = "blue"))+
  facet_wrap(~pod+Season)

ggsave(paste0('./figures/season_point_',date,'.png'), season_point, dpi = 300, width = 200, height = 100, units = "mm")

season<-ggpubr::ggarrange(p_box, season_point, phi_box, labels = "auto", ncol = 1)
ggsave(paste0('./figures/season',date,'.png'), season, dpi = 300, width = 150, height = 250, units = "mm")

##
nrow(results_p_SA)

results_p_SA%>%
  group_by(pod)%>%
  dplyr::summarise(quantile(median, c(0.10)))

