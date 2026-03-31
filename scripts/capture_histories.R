library(odbc);library(dplyr);library(DBI);library(ggplot2);library(lubridate)

# query and format capture histories ----

## query data from photo database ----

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist
nrow(photo_analysis_calfyear_sql)

ind_lh<-lifehist%>%
  filter(LAST_YEAR != "<NA>")%>%
  tidyr::pivot_longer(cols = c(14:48), names_to = "CALFYEAR", values_to = "Ageclass")%>%
  filter(CALFYEAR > 2004)%>% 
  dplyr::select(POD, NAME, SEX, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, LAST_YEAR, CALFYEAR, Ageclass)

ind_lh$CALFYEAR<-as.numeric(ind_lh$CALFYEAR)
ind_lh$FIRST_CALF = as.numeric(ind_lh$FIRST_CALF)

# this is used later for the timeline so keep year filter here
PA_filter<-photo_analysis_calfyear_sql%>%
  filter(CALFYEAR > 2004 & CALFYEAR < 2024)
unique(PA_filter$SURVEY_AREA)

### only DOUBTFUL and DUSKY complex survey area (SA) ----
PA_filter_SA<-photo_analysis_calfyear_sql%>%
  filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY")%>% #only looking at doubtful and dusky complexes
  filter(CALFYEAR > 2004 & CALFYEAR < 2024)
unique(PA_filter_SA$SURVEY_AREA)

### assign season code ----
# spring is first third (+ 0), summer second third (+ 0.33), and winter final third (+ 0.67) of a year

yr_season_code<-data.frame(CALFYEAR = c(rep(1990:2024, each = 3)),
                         season_code = c(rep(c(0,0.33,0.67))))

photo_ID_season_code<-photo_analysis_calfyear_sql%>%
  mutate(month = month(DATE))%>%
  mutate(season_code = case_when(
    month >= 9 & month <= 12 ~ 0,
    month >= 1 & month <= 4 ~ 0.33,
    month >= 5 & month <= 8 ~ 0.67))

## photo effort timeline Fig. 1a ----

dates_eff<-function(x){x%>%
    left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
    filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
    distinct(DATE, CALFYEAR, SEASON, SURVEY_AREA, POD)%>%
    mutate(Ordinal = yday(DATE))%>%
    mutate(season_ordinal = Ordinal-244)%>%
    mutate(season_ordinal = case_when(
      season_ordinal < 0 ~ 365 + season_ordinal,
      TRUE ~ season_ordinal
    ))
}

PA_dates<-dates_eff(PA_filter)
PA_dates_SA<-dates_eff(PA_filter_SA)

PA_timeline<-ggplot()+
  geom_point(PA_dates%>%filter(SURVEY_AREA != "DOUBTFUL" & SURVEY_AREA != "DUSKY"), mapping = aes(x = season_ordinal, y = as.numeric(CALFYEAR)), color = "red", size = 2, shape = "square", alpha = 0.8)+
  geom_point(PA_dates%>%filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY"), mapping = aes(x = season_ordinal, y = as.numeric(CALFYEAR)), size = 1, shape = "square")+
  scale_x_continuous(breaks = c(0,30,61,91,122,153,181,212,242,273,303,334,366),
                     labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",""), limits = c(0,366))+
  annotate("rect", xmin = 242, xmax = 366, ymin = 2005, ymax = 2023,
           alpha = .1,fill = "blue")+
  annotate("rect", xmin = 1, xmax = 122, ymin = 2005, ymax = 2023,
           alpha = .1,fill = "orange")+
  scale_y_continuous(breaks = c(2005:2023))+
  theme_bw()+
  xlab("Date")+
  ylab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")+
  facet_wrap(~POD)

PA_timeline

ggsave('./figures/PA_timeline.png', PA_timeline, dpi = 320, width = 200, height = 100, units = 'mm')

## Capture histories----

# all photos of Doubtful/Dusky members
all_photo<-photo_ID_season_code 
# only photos of Doubtful/Dusky members taken in the survey areas
SA_photo<-photo_ID_season_code%>%
  filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY")

### format capture histories (ch) into dataframe with all field details----

ch_df<-function(x){
  x%>%
  distinct(ID_NAME, CALFYEAR, month, season_code)%>%
    left_join(lifehist, by = c("ID_NAME"="NAME"))%>%
    filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
    mutate(first_season = case_when(
         month(ymd(FIRST_DATE)) >= 9 & month(ymd(FIRST_DATE)) <= 12 ~ paste0(as.numeric(year(FIRST_DATE))+1),
         month(ymd(FIRST_DATE)) >= 1 & month(ymd(FIRST_DATE)) <= 4 ~ paste0(as.numeric(year(FIRST_DATE))+0.33),
         month(ymd(FIRST_DATE)) >= 5 & month(ymd(FIRST_DATE)) <= 8 ~ paste0(as.numeric(year(FIRST_DATE))+0.67),
         TRUE ~ '999909'
       ))%>%
    mutate(last_season = case_when(
         month(ymd(LAST_DATE)) >= 9 & month(ymd(LAST_DATE)) <= 12 ~ paste0(as.numeric(year(LAST_DATE))+1),
         month(ymd(LAST_DATE)) >= 1 & month(ymd(LAST_DATE)) <= 4 ~ paste0(as.numeric(year(LAST_DATE))+0.33),
         month(ymd(LAST_DATE)) >= 5 & month(ymd(LAST_DATE)) <= 8 ~ paste0(as.numeric(year(LAST_DATE))+0.67),
         TRUE ~ '999909'
       ))%>%
    dplyr::select(CALFYEAR, season_code, POD, NAME = ID_NAME, SEX, FIRST_CALF, BIRTH_YEAR, first_season, DEATH_YEAR, last_season)%>%
  mutate(year_season_code = CALFYEAR + season_code)%>%
  mutate(age = case_when(
    BIRTH_YEAR > 1 ~ year_season_code - as.numeric(BIRTH_YEAR),
    TRUE ~ year_season_code - as.numeric(first_season)))%>%
  mutate(age_value = case_when(
    BIRTH_YEAR > 0 ~ "actual", #birth year known
    BIRTH_YEAR == "" ~ "est" #birth year not known
    ))%>%
  mutate(age = case_when( ## for some reproductive females, we can infer an older age based on calving history
    age_value == "est" & as.numeric(FIRST_CALF) > 0 & (as.numeric(FIRST_CALF) - as.integer(avg_primo_age)) < as.numeric(first_season) ~ age + (as.integer(avg_primo_age) - (as.numeric(FIRST_CALF) - as.numeric(first_season))),
    TRUE ~ age
    ))%>%
  mutate(ageclass = case_when( # PA = pre-adult
    NAME == "2006CALFOFTIP" ~ "PA", #first sighted in Oct 2006, small, but too big to be just born that spring
    age >= as.integer(avg_primo_age) ~ "A",
    BIRTH_YEAR == '' & age >= as.integer(avg_primo_age)-1 ~ "A",
    BIRTH_YEAR > 0 & age <= 1 ~ "C",
    BIRTH_YEAR > 0 & age < avg_primo_age ~ "PA",
    BIRTH_YEAR == '' & age < as.integer(avg_primo_age)-1 ~ "U",
    TRUE ~ "X"
  ))%>%
  filter(year_season_code >= 2005 & year_season_code < 2024)%>%
  distinct()
}

### minimal capture history details for model ----
ch<-function(x){
  x%>%
  dplyr::select(-age)%>%
  arrange(year_season_code)%>%
  distinct(POD, NAME, SEX, year_season_code)%>%
  mutate(ch = 1)%>%
  tidyr::pivot_wider(names_from = year_season_code, values_from = ch)%>%
  arrange(POD, SEX)%>%
  group_by(POD)%>%
  mutate(
  ind = 1:n(),  
  pod_ch = case_when(
    POD == "DOUBTFUL" ~ 1,
    POD == "DUSKY" ~ 2))%>%
  mutate(sex_ch = case_when(
    SEX == "F" ~ 1,
    SEX == "M" ~ 2,
    SEX == "X" ~ 2
  ))%>%
  ungroup()
}

##### Seasonal capture histories from all data ----
everyone<-ch_df(all_photo)

##### Seasonal capture histories in survey areas ----
everyone_SA<-ch_df(SA_photo)
everyone_SA_ch<-ch(everyone_SA)

# To be called in state-space_survey-area_seasonal.R
saveRDS(everyone_SA_ch, "./data/everyone_SA.RDS")

##### Annual capture histories in survey areas ----
# supplement 

everyone_ch_calfyr<-SA_photo%>%
  distinct(ID_NAME, CALFYEAR)%>%
  filter(CALFYEAR >= 2005 & CALFYEAR < 2024)%>%
  left_join(lifehist, by = c("ID_NAME"="NAME"))%>%
  filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
  distinct(POD, "NAME" = ID_NAME, SEX, CALFYEAR)%>%
  arrange(NAME, CALFYEAR)%>%
  mutate(ch = 1)%>%
  tidyr::pivot_wider(names_from = CALFYEAR, values_from = ch)%>%
  arrange(POD, SEX)%>%
  group_by(POD)%>%
  mutate(
    ind = 1:n(),  
    pod_ch = case_when(
      POD == "DOUBTFUL" ~ 1,
      POD == "DUSKY" ~ 2))%>%
  mutate(sex_ch = case_when(
    SEX == "F" ~ 1,
    SEX == "M" ~ 2,
    SEX == "X" ~ 2
  ))%>%
  ungroup()

saveRDS(everyone_ch_calfyr, "./data/everyone_ch_calfyr.RDS")

## Effort ----

# function to determine number of photo days per seasonal period 
photo_days<-function(x){
  x%>%
  filter(CALFYEAR >= 2005 & CALFYEAR < 2024)%>%
  mutate(month = month(DATE))%>%
  mutate(season_code = case_when(
    month >= 9 & month <= 12 ~ 0,
    month >= 1 & month <= 4 ~ 0.33,
    month >= 5 & month <= 8 ~ 0.67))%>%
  mutate(year_season_code = CALFYEAR+season_code)%>%
  distinct(POD, year_season_code, DATE)%>%
  group_by(POD, year_season_code)%>%
  tally()%>%
  as.data.frame()
}

photo_days_all<-photo_days(PA_dates) # based on all effort regardless of survey area
photo_days_SA<-photo_days(PA_dates_SA) # based only on effort in the survey areas

occasions<-yr_season_code%>%
  filter(CALFYEAR >= 2005 & CALFYEAR < 2024)%>%
  mutate(year_season_code = CALFYEAR + season_code)

IDperday_fxn<-function(x,y){

  ID_sampling<-x%>%
    distinct(POD, NAME, year_season_code)%>%
    group_by(POD, year_season_code)%>%
    tally()
  
  occasions_Dbt<-occasions%>%
    mutate(POD = "DOUBTFUL")
  
  occasions_Dus<-occasions%>%
    mutate(POD = "DUSKY")
  
  occ_POD<-occasions_Dbt%>%
    full_join(occasions_Dus, by = c("year_season_code","POD","CALFYEAR","season_code"))
  
  ID_per_day<-occ_POD%>%
    left_join(y, by = c("year_season_code","POD"))%>%
    left_join(ID_sampling, by = c("POD", "year_season_code"))%>%
    mutate(IDperDay = n.y/n.x)%>%
    mutate(season = case_when(
      grepl(".33", year_season_code) ~ "Summer",
      grepl(".67", year_season_code) ~ "Winter",
      TRUE ~ "Spring"
    ))
  
  ID_per_day[is.na(ID_per_day)]<-0
  
  ID_per_day
}

# IDs per day for all photo data of Doubtful and Dusky members regardless of survey area
ID_per_day_all<-IDperday_fxn(everyone,photo_days_all)%>%mutate(area = "All areas")
saveRDS(ID_per_day_all, "./data/ID_per_day_all.RDS") 

# IDs per day of the Doubtful and Dusky members collected in the survey areas only
ID_per_day_SA<-IDperday_fxn(everyone_SA,photo_days_SA)%>%mutate(area = "Complexes only")
saveRDS(ID_per_day_SA, "./data/ID_per_day_SA.RDS") 

# combine dataframes to plot
ID_per_day_together<-ID_per_day_all%>%
  bind_rows(ID_per_day_SA)

### individuals identified per sampling period Fig. 1b----
count_ID_samp<-ggplot(ID_per_day_together)+
  geom_point(aes(x = year_season_code, y = n.y, shape = as.factor(season), color = area))+
  facet_wrap(~POD)+
  theme_bw()+
  xlab(expression("Dolphin year (01Sep"[y-1]~"–31Aug"[y]~")"))+
  ylab("# individuals identified")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")+
  scale_color_manual(values = c("All areas" = "red", "Complexes only" = "black"))

count_ID_samp

ggsave('./figures/count_ID_samp.png', count_ID_samp, dpi = 300, width = 200, height = 100, units = 'mm')

### full figure 1 ----
fig1<-ggpubr::ggarrange(PA_timeline, count_ID_samp, ncol = 1, labels = "auto")
ggsave('./figures/fig1.png', fig1, dpi = 300, width = 200, height = 200, units = 'mm')

### effort data for model ----
# number of days of photo-ID data collection per sampling period

#### seasonal sampling effort in survey area ---

long_samp_SA<-ID_per_day_SA%>%
  dplyr::select(year_season_code, POD, n.x)%>%
  filter(year_season_code != 2007.67)%>%
  tidyr::pivot_wider(names_from = year_season_code, values_from = n.x)

saveRDS(long_samp_SA, "./data/long_samp_SA.RDS") 

#### annual sampling effort in survey area ----

long_samp_calfyr<-ID_per_day_SA%>%
  distinct(CALFYEAR, POD)%>%
  mutate(n = 1)%>%
  tidyr::pivot_wider(names_from = CALFYEAR, values_from = n)

saveRDS(long_samp_calfyr, "./data/long_samp_calfyr.RDS") 


