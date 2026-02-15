# Load results ----

date = "2026-02-14" #CALFYR
results_in_SA<-readRDS(paste0("./data/survival&cap_SA_calfyr",date,".rds"))

#everyone_ch_SA<-readRDS("./data/everyone_SA.RDS")
#long_samp_ch_SA<-readRDS("./data/long_samp_SA.RDS") #skip 2007.67
#ID_per_day_SA<-readRDS("./data/ID_per_day_SA.RDS") 

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
#occasions_SA<-names(long_samp_ch_SA)

I#D_per_day_SA$year_season_code<-as.character(ID_per_day_SA$year_season_code)

results_phi_SA<-results_SA%>%
  filter(grepl("phi.est", variable))%>%
  mutate(CALF_YR = rep(c(2005:2022), each = 2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))

summary(results_phi_SA)

library(ggplot2)

## capture prob # not identifiable at first occasion

results_p_SA<-results_SA%>%
  filter(grepl("p.est", variable))%>%
  mutate(CALF_YR = rep(c(2006:2023), each = 2),
         pod = rep(rep(c("DOUBTFUL","DUSKY"), each = 1), (n_occ-1)))

### CALFYR

N_SA<-results_SA%>%
  filter(grepl("Doubtful_N", variable) | grepl("Dusky_N", variable))%>%
  mutate(CALF_YR = c(2005:2023,2005:2023),
         pod = c(rep("DOUBTFUL",19),rep("DUSKY",19)))

ggplot(N_SA%>%filter(median > 0))+
  #geom_hline(data = sumstats_n_SA, mapping = aes(yintercept = med_N, color = Pod), linetype = "dashed")+
  #geom_rect(data = sumstats_n_SA, mapping = aes(ymin = q5_N, ymax = q95_N, xmin = 2005.33, xmax = 2023.67, fill = Pod), alpha = 0.2)+
  geom_ribbon(aes(ymin = q5, ymax = q95, x = as.numeric(CALF_YR), color = Pod, fill = Pod), alpha = 0.8)+
  geom_path(aes(x = as.numeric(CALF_YR), y = median, group = Pod), color = "black", alpha = 0.8)+
  theme_bw()+
  xlab("Year")+
  ylab("Abundance")+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2005:2024))

ggsave("./figures/abund_SA_ribbon_CALFYR.png", dpi = 300, width = 300, height = 150, units = "mm")

###

phi_together<-ggplot(results_phi_SA, aes(x = as.numeric(CALF_YR), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), alpha = 0.8)+
  geom_point()+
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
  ylab(expression('Survival probability,' *phi))

phi_together

ggsave('./figures/phi_together_CALFYR.png', phi_together, dpi = 300, width = 200, height = 100, units = "mm")

p_together<-ggplot(results_p_SA, aes(x = as.numeric(CALF_YR), y = median))+
  geom_errorbar(aes(ymin = q5, ymax = q95), alpha = 0.8)+
  geom_point()+
  facet_wrap(~pod)+
  theme_bw()+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  theme(legend.position = "bottom",
        legend.box.background = element_rect(fill='white'),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(color = "white"))+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab(expression('Capture probability, p'))

p_together

ggsave('./figures/p_together_CALFYR.png', p_together, dpi = 300, width = 200, height = 100, units = "mm")

phi_p_together<-ggpubr::ggarrange(p_together,phi_together, common.legend = T, labels = "auto", ncol = 1, legend = "bottom", align = "v")

ggsave('./figures/phi_p_together2_CALFYR.png', phi_p_together, dpi = 300, width = 250, height = 200, units = "mm")

###

nrow(results_p_SA)

results_p_SA%>%
  #filter(median >= 0.975)%>%
  group_by(pod)%>%
  dplyr::summarise(med_med = median(median))
40/56 

