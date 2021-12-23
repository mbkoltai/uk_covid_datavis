### load settings --------------------------
# rm(list=ls()); currentdir_path=dirname(rstudioapi::getSourceEditorContext()$path); setwd(currentdir_path)
# library(rstudioapi); currentdir_path=dirname(rstudioapi::getSourceEditorContext()$path); setwd(currentdir_path)
# library(tidyverse); library(wesanderson); library(RcppRoll); library(scales); library(lubridate); library(ungeviz) 
lapply(c("tidyverse","wesanderson","RcppRoll","scales","lubridate","ungeviz"),library,character.only=TRUE)
# install.packages("wesanderson")
standard_theme=theme(plot.title=element_text(hjust=0.5,size=16), 
    axis.text.x=element_text(size=13,angle=90,vjust=1/2),axis.text.y=element_text(size=13),
    axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),
    legend.title=element_text(size=16),legend.text=element_text(size=12)) # text=element_text(family="Calibri")
# panel.grid=element_line(linetype="dashed",colour="black",size=0.1),

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# population data
ONS_2019_population_estim <- read_csv("ONS_2019_midpoint_population_estim_modified.csv")
ons_all_age_groups_uk_england_2019 <- read_csv("ons_all_age_groups_uk_england_2019.csv")
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# vacc uptake
url_data<-
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"

vacc_dose_data_eng <- left_join(read_csv(url_data),ONS_2019_population_estim %>% #  %>% select(!contains("UK"))
                                rename(population=England),by="age")
vacc_dose_data_eng <- vacc_dose_data_eng %>% group_by(age) %>% 
  mutate(daily_first_dose_perc_agegroup=1e2*roll_mean(
    newPeopleVaccinatedFirstDoseByVaccinationDate/population,n=7,align="center",fill=NA),
         daily_second_dose_perc_agegroup=1e2*roll_mean(
  newPeopleVaccinatedSecondDoseByVaccinationDate/population,n=7,align="center",fill=NA),
  daily_third_dose_perc_agegroup=1e2*roll_mean(
    newPeopleVaccinatedThirdInjectionByVaccinationDate/population,n=7,align="center",fill=NA),
  date_numeric=as.numeric(date)-as.numeric(min(date)))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# CUMULATIVE %
vacc_dose_data_eng %>% filter(date %in% c(max(date)-14,max(date)-7,max(date))) %>% 
  select(date,age,cumVaccinationFirstDoseUptakeByVaccinationDatePercentage,
         cumVaccinationSecondDoseUptakeByVaccinationDatePercentage,
         cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage) %>% pivot_longer(!c(age,date)) %>%
  mutate(name=ifelse(grepl("First",name),"first dose",
              ifelse(grepl("Second",name),"second dose","third dose")),age=gsub("_","-",age)) %>%
ggplot() + 
  geom_hpline(aes(x=age,y=value,color=name,alpha=factor(date)),width=0.95,size=1) + 
  geom_vline(xintercept=0.5+(0:15),size=1/3,linetype="dashed") + 
  labs(color="",alpha="",caption="denominators: ONS 2019 midpoint estimates") +
  scale_x_discrete(expand=expansion(0.0375,0)) + scale_y_continuous(breaks=(0:10)*10) + 
  geom_text(aes(x=age,y=ifelse(grepl("first",name),value+2.5,value-2.5),
                label=ifelse(date==max(date),paste0(value,ifelse(age %in% c("12-15","16-17"),"%","")),""))) +
  guides(color=guide_legend(nrow=2),alpha=guide_legend(nrow=2)) +
  theme_bw() + standard_theme + theme(axis.text.x=element_text(vjust=1/2),legend.position="top",
      legend.text=element_text(size=10)) + xlab("") + ylab("% age group vaccinated in England")
## SAVE
vaccine_folder<-"vaccine_data/"; if (!dir.exists(vaccine_folder)) {dir.create(vaccine_folder)}
ggsave(paste0(vaccine_folder,"vaccine_by_age_cumul.png"),width=20,height=15,units="cm")
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# daily rate as % of age group
df_plot <- vacc_dose_data_eng %>% # filter(date>as.Date("2020-12-14")) %>%
  select(date,age,daily_first_dose_perc_agegroup,daily_second_dose_perc_agegroup,
         daily_third_dose_perc_agegroup,
         newPeopleVaccinatedFirstDoseByVaccinationDate,newPeopleVaccinatedSecondDoseByVaccinationDate,
         newPeopleVaccinatedThirdInjectionByVaccinationDate,population) %>% 
  mutate(newPeopleVaccinatedFirstDoseByVaccinationDate=
           100*newPeopleVaccinatedFirstDoseByVaccinationDate/population,
    newPeopleVaccinatedSecondDoseByVaccinationDate=
      100*newPeopleVaccinatedSecondDoseByVaccinationDate/population,
 newPeopleVaccinatedThirdInjectionByVaccinationDate=
   100*newPeopleVaccinatedThirdInjectionByVaccinationDate/population) %>%
  pivot_longer(!c(age,date,population)) %>% 
  mutate(value_type=ifelse(grepl("perc",name),"smoothed","daily"),
         dose=ifelse(grepl("first|First",name),"1st dose",
                    ifelse(grepl("second|Second",name),"2nd dose","3rd dose")),
    name=ifelse(value_type %in% "daily",name,
            ifelse(grepl("first",name),"first dose",
            ifelse(grepl("second",name),"second dose","third dose"))),
    age=gsub("_","-",age),value=ifelse(value<0.03,NA,value))
###
p <- ggplot(df_plot,aes(x=date,y=value)) + 
  geom_line(data=df_plot %>% filter(value_type %in% "smoothed"),aes(group=dose) ) + 
  geom_point(data=df_plot %>% filter(!(value_type %in% "smoothed") & as.numeric(date) %% 4),
             aes(color=dose),shape=21,size=1) + # ,show.legend=F,
  facet_wrap(~age,scales="free_y") + scale_size(range=c(0,1)) + # 
  scale_x_date(date_breaks="month",expand=expansion(0.01,0)) + # scale_y_log10(limits=c(0.03,10)) + 
  theme_bw() + standard_theme + theme(axis.text.x=element_text(vjust=1/2)) + 
  xlab("") + ylab("% of age group") + labs(color="") + 
  # ggtitle("7-day average of daily vaccinations as % age group") + 
  theme(strip.text=element_text(size=16),plot.title=element_text(size=18),legend.text=element_text(size=16),
        axis.text.y=element_text(size=13),axis.title.y=element_text(size=18))
# save
# ggsave(paste0("vaccine_by_age_rate.png"),width=45,height=30,units="cm")
p_log<-p + scale_y_log10(limits=c(0.03,10),breaks=c(0.1,0.5,1,2,5,10)); p_log
ggsave(paste0(vaccine_folder,"vaccine_by_age_rate_log.png"),width=45,height=25,units="cm")
p; ggsave(paste0(vaccine_folder,"vaccine_by_age_rate_lin.png"),width=45,height=25,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# daily rate as absolute number
df_plot <- vacc_dose_data_eng %>% 
  mutate(firstdose_smooth=
           roll_mean(newPeopleVaccinatedFirstDoseByVaccinationDate,n=7,align="center",fill=NA),
    second_dose_smooth=
      roll_mean(newPeopleVaccinatedSecondDoseByVaccinationDate,n=7,align="center",fill=NA),
    third_dose_smooth=
      roll_mean(newPeopleVaccinatedThirdInjectionByVaccinationDate,n=7,align="center",fill=NA)) %>%
  select(date,age,firstdose_smooth,second_dose_smooth,third_dose_smooth,
         newPeopleVaccinatedFirstDoseByVaccinationDate,newPeopleVaccinatedSecondDoseByVaccinationDate,
         newPeopleVaccinatedThirdInjectionByVaccinationDate) %>% 
  pivot_longer(!c(age,date)) %>% mutate(value_type=ifelse(grepl("smooth",name),"smoothed","daily")) %>%
  mutate(dose=ifelse(grepl("first|First",name),"1st dose",
                     ifelse(grepl("second|Second",name),"2nd dose","3rd dose")),
         name=ifelse(value_type %in% "daily",name,
           ifelse(grepl("first",name),"1st dose",ifelse(grepl("second",name),"2nd dose","3rd dose")) ),
         age=gsub("_","-",age),value=ifelse(value<1e2,NA,value))
# PLOT
p <- ggplot(df_plot,aes(x=date,y=value)) + 
  geom_line(data=df_plot %>% filter(value_type %in% "smoothed"),aes(group=dose)) + 
  geom_point(data=df_plot %>% filter(!(value_type %in% "smoothed") & as.numeric(date) %% 4),aes(color=dose),
             shape=21,size=1) +
  facet_wrap(~age) + scale_size(range=c(0,1)) + scale_x_date(date_breaks="1 month",expand=expansion(0.02,0)) + 
  # scale_y_log10(limits=c(5e2,2e5)) + # scale_y_continuous(expand=expansion(0.01,0))+
  theme_bw() + standard_theme + theme(axis.text.x=element_text(vjust=1/2)) + 
  xlab("") + ylab("# shots") + labs(color="") + # ggtitle("7-day average of daily vaccinations") + 
  theme(strip.text=element_text(size=16),plot.title=element_text(size=18),legend.text=element_text(size=18),
        axis.text.y=element_text(size=14),axis.title.y=element_text(size=18))
# SAVE
# ggsave(paste0("vaccine_by_age_rate_absnum.png"),width=45,height=30,units="cm")
p_log<-p+scale_y_log10(); ggsave(paste0(vaccine_folder,"vaccine_by_age_rate_absnum_log.png"),
                                 width=45,height=30,units="cm")
p_lin<-p+scale_y_continuous(limits=c(5e2,2e5)); p_lin
ggsave(paste0(vaccine_folder,"vaccine_by_age_rate_absnum_lin.png"),width=45,height=30,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PHASE PLOT vacc rollout

df_plot <- vacc_dose_data_eng %>% filter(date>as.Date("2020-12-14")) %>% mutate(age=gsub("_","-",age)) %>%
 select(matches("date|daily|cumVaccinationFirst|cumVaccinationComplete|age",ignore.case=F) & 
          !matches("complete")) %>%
  pivot_longer(!c(date,date_numeric,age)) %>% mutate(dose=ifelse(grepl("first|First",name),"first",
             ifelse(grepl("second|Second",name),"second","third")),
        type=ifelse(grepl("daily",name),"rate","cumul")) %>% 
  select(!c(name)) %>% pivot_wider(names_from=type) %>% filter(cumul>0.5) %>%
  group_by(age,dose) %>% mutate(start_date=min(date) ) %>% ungroup() %>%
  mutate(date_from_start=as.numeric(date-start_date))
# colors to show passage of time
n_uni_col<-nrow(vacc_dose_data_eng %>% filter(date>as.Date("2020-12-14")) %>% 
                  group_by(date) %>% summarise(unique(date)))
n_col<-(df_plot %>% ungroup() %>% group_by(dose) %>% summarise(n_col=n_distinct(date_from_start)))$n_col
colorpal<-c(colorRampPalette(colors=c("orange","red"))(n_col[1]),
            colorRampPalette(colors=c("grey","black"))(n_col[2]),
            colorRampPalette(colors=c("cyan","blue"))(n_col[3]))
# PLOT
ggplot(df_plot) + 
  geom_point(aes(y=rate,x=cumul,group=dose,color=interaction(date_from_start,dose)),size=1/2,shape=21,fill=NA) +
  scale_color_manual(values=colorpal) + facet_wrap(~age,scales="free_y") +
  scale_x_continuous(breaks=(0:10)*10,expand=expansion(0.03,0)) + 
  scale_y_log10(limits=c(0.03,7),breaks=c(0.05,0.1,0.2,0.5,1,2,4)) + 
  geom_vline(xintercept=c(60,70,80,90),linetype="dashed",size=1/3) +
  theme_bw() + standard_theme + 
  theme(axis.text.x=element_text(vjust=1/2,size=12),axis.text.y=element_text(size=10),
      axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),strip.text=element_text(size=15),
      legend.title=element_text(size=16),legend.text=element_text(size=16),legend.position="top",
      legend.key.width=unit(1.2,'cm'),plot.title=element_text(size=16),
      panel.grid.minor=element_blank()) + # element_line(colour="grey", linetype="dashed",size=1/6)
  ggtitle("Orange to red: 1st dose. Grey to black: 2nd. Cyan to blue: 3rd. Color scales: days from ≥0.5% jabbed.") +
  xlab("cumulative: % age group") + ylab("daily vaccinations: % age group") + guides(color="none")
# save
ggsave(paste0(vaccine_folder,"vaccine_by_age_phaseportrait_both_doses_line_log.png"),
       width=33,height=22,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# all age groups together # vacc_dose_sum
vacc_dose_data_eng_totals <- vacc_dose_data_eng %>% group_by(date,date_numeric) %>% 
  summarise(first_dose_rate=1e2*sum(newPeopleVaccinatedFirstDoseByVaccinationDate)/sum(population), 
  first_dose_total=1e2*sum(cumPeopleVaccinatedFirstDoseByVaccinationDate)/sum(population),
  second_dose_rate=1e2*sum(newPeopleVaccinatedSecondDoseByVaccinationDate)/sum(population),
  second_dose_total=1e2*sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)/sum(population),
  third_dose_total=1e2*sum(cumPeopleVaccinatedThirdInjectionByVaccinationDate)/sum(population),
  third_dose_rate=1e2*sum(newPeopleVaccinatedThirdInjectionByVaccinationDate)/sum(population)) %>% 
  filter(date>as.Date("2020-12-14")) %>% ungroup() %>% 
  mutate(first_dose_rate_smooth=roll_mean(first_dose_rate,n=7,align="center",fill=NA),
         second_dose_rate_smooth=roll_mean(second_dose_rate,n=7,align="center",fill=NA),
         third_dose_rate_smooth=roll_mean(third_dose_rate,n=7,align="center",fill=NA)) %>%
  pivot_longer(!c(date,date_numeric)) %>% 
  mutate(type=ifelse(grepl("rate_smooth",name),"rate_smooth",ifelse(grepl("total",name),"cumul","rate")),
          dose=ifelse(grepl("first",name),"first",ifelse(grepl("second",name),"second","third") ) ) %>% 
  select(!name) %>% pivot_wider(names_from=type,values_from=value) %>% group_by(dose) %>%
  mutate(date_numeric=date-min(date[cumul>0.5])) %>% filter(!is.na(rate) & date_numeric>0)
color_legends <- c("first dose"="blue", "second dose"="red", "third dose"="green"); circle_size=2.5
# plot
ggplot(vacc_dose_data_eng_totals) + #  %>% filter(!grepl("cumul",dose))
  geom_point(aes(y=rate,x=cumul,color=factor(dose),fill=as.numeric(date_numeric)),shape=21,stroke=0.4,size=2) + 
  geom_path(aes(y=rate_smooth,x=cumul,color=factor(dose)),size=1) + # color=factor(dose),
  geom_path(aes(y=rate,x=cumul,color=factor(dose)),size=1/2,linetype="dashed") + # color=factor(dose),
  scale_fill_gradient(low="gray98",high="gray45") + facet_wrap(~dose,nrow=3) + 
  scale_x_continuous(breaks=(0:20)*5,expand=expansion(0.03,0)) + 
  scale_y_continuous(breaks=2*(0:10)/10,expand=expansion(0.03,0)) + 
  xlab("cumulative: % population ≥12y") + ylab("daily vaccinations as % of population ≥12y") +
  theme_bw() + standard_theme + theme(axis.title.x=element_text(size=22),axis.title.y=element_text(size=22),
    axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.title=element_text(size=18),
    strip.text=element_text(size=18),legend.text=element_text(size=17),legend.position="top",
    legend.key.width=unit(1.2,'cm')) + labs(color="",fill="days from >0.5% cumulative coverage")
# save
ggsave(paste0(vaccine_folder,"vaccine_allage_phaseportrait_3rows.png"),width=35,height=32,units="cm")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# CASES 
url_cases_age="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
# c(1:5,6:9,10:14,15:19)
eng_case_age_data <- read_csv(url_cases_age)
eng_case_age_data <- eng_case_age_data %>% filter(!age %in% c("unassigned","00_59","60+")) %>% 
  mutate(age_num=as.numeric(factor(age)), 
         age_categ=case_when(age_num<=5 ~ "0-24",age_num>=6&age_num<=10 ~ "25-49",
  age_num>=11 & age_num<=15 ~ "50-74", age_num>15 ~ "75+"),
  age_num=age_num-(as.numeric(factor(age_categ))-1)*5) %>%  group_by(age) %>% 
  mutate(rollingsum_chng=rollingSum/lag(rollingSum,n=7,order_by=date))
# 
agegr_names=gsub("09","9",gsub("04","4",gsub("^0","",gsub("_","-",unique(eng_case_age_data$age)))))
l_num=lapply(1:4, function(x) (x-1)*5+1:5); l_num[[4]]=l_num[[4]][1:4]; 
agegr_names=paste0(unlist(lapply(l_num, function(x) 
  paste0("[",paste0(agegr_names[x],collapse=","),"]"))),collapse=", ")
# PLOT CHANGE in RATES
start_date<-as.Date("2021-09-01")
ggplot(eng_case_age_data %>% filter(date>start_date),
       aes(x=date,y=(rollingsum_chng-1)*100,color=factor(age_num),group=age_num)) +
  geom_line(size=1.02) + facet_wrap(~age_categ) + # ,scales="free_y") + # 
  geom_hline(yintercept=0,linetype="dashed",size=1/2) + 
  scale_x_date(expand=expansion(0.01,0),breaks="week") + scale_y_continuous(breaks=(-4:10)*25) +
  labs(color="5y age band within age group",caption=paste0("agegroups: ",agegr_names)) +
  xlab("") + ylab("weekly sum % change") +
  theme_bw() + standard_theme + theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
      strip.text=element_text(size=14),legend.title=element_text(size=12),legend.text=element_text(size=12),
      legend.position="bottom",plot.caption=element_text(size=10))
# save
# ggsave(paste0("england_cases_age_4groups_log.png"),width=34,height=22,units="cm")
ggsave(paste0("england_cases_age_4groups_rollingsum_change.png"),width=34,height=22,units="cm")

###################################################
# ABSOLUTE NUMBER of cases in 10-year bands
for (k in 1:2){
p<-eng_case_age_data %>% mutate(ten_year_band_num=round(as.numeric(strsplit(age,"_")[[1]][2])/10)*10,
                             ten_year_band_num=ifelse(is.na(ten_year_band_num),90,ten_year_band_num),
  ten_year_band=ifelse(ten_year_band_num-5<0,"<5",paste0(ten_year_band_num-5,"_",ten_year_band_num+4))) %>%
  mutate(ten_year_band=ifelse(is.na(ten_year_band)|grepl("85",ten_year_band),">85",ten_year_band)) %>%
  group_by(date,ten_year_band) %>% 
  summarise(rollingSum=sum(rollingSum)/7,ten_year_band_num=unique(ten_year_band_num)/10) %>% 
  mutate(ten_year_band_num=ifelse(is.na(ten_year_band_num),9,ten_year_band_num)+1,
         age_meta=round((ten_year_band_num+0.9)/2)) %>% ungroup() %>% arrange(date,ten_year_band_num) %>% 
  mutate(ten_year_band=factor(ten_year_band,levels=unique(ten_year_band))) %>% group_by(age_meta,date) %>% 
  mutate(order_within=c("lower","higher")[row_number()],
         age_meta_name=paste0("[",paste0(ten_year_band,collapse=", "),"]",sep="")) %>% 
  filter(date>as.Date("2021-08-01")) %>%
  ggplot() + geom_line(aes(x=date,y=rollingSum,color=order_within),size=1.1) +
  facet_wrap(~age_meta_name,nrow=2,scales=ifelse(k==1,"free_y","fixed")) + 
  scale_x_date(expand=expansion(0.02,0),breaks="2 week") + 
  theme_bw() + standard_theme + theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=12),
    strip.text=element_text(size=17),legend.title=element_blank(),legend.text=element_text(size=17),
    axis.title.y=element_text(size=19),plot.caption=element_text(size=12),panel.grid.minor.y=element_blank()) + 
  xlab("") + ylab("cases")
if (k==1){p <- p + scale_y_log10(breaks=round(2^seq(3,14,by=1/2))) } else { 
  p<-p+scale_y_continuous(breaks=(0:12)*2e3) } # sapply(10^seq(1,4,by=1/4),function(x) round(x,max(3-round(log(x)),0)))
p
# SAVE # 
ggsave(paste0("england_cases_number_10_yr_agebands",
              ifelse(grepl("log",p$scales$scales[[2]]$trans$name),"_log",""),
              ".png",collapse=""),width=36,height=22,units="cm")
}

######################################################
# RATE of CASES in 10-yr bands
df_cases<-left_join(eng_case_age_data,ons_all_age_groups_uk_england_2019,by="age") %>% 
  select(!c(UK,areaCode,areaType,age_categ,areaName)) %>% rename(population=England) %>% 
  ungroup() %>% mutate(age_num=as.numeric(factor(age)),
                       meta_age=ifelse(ceiling(age_num/2)>8,9,ceiling(age_num/2))) %>% 
  group_by(date,meta_age) %>% 
  summarise(rollingSum=sum(rollingSum),population=sum(population),min_age=unique(age)[1],
            max_age=unique(age)[length(unique(age))]) %>%
  mutate(age=paste0(substr(min_age,1,3),
                    ifelse(is.na(max_age),"",gsub("_","",substr(max_age,nchar(max_age)-2,nchar(max_age)))) ),
         age=ifelse(meta_age==max(meta_age),"80+",age),rollingRate=1e6*rollingSum/(7*population))

# plot CASE RATES in 10-yr groups
plot_settings<-expand.grid(list(c("log","linear"),c("fixed","free"),c("nofacet","facet"))) %>% 
  rename(y_scale=Var1,y_range=Var2,faceting=Var3) %>% filter(!(faceting=="nofacet" & y_range=="fixed"))
start_dates <- c("2020-12-01","2021-07-01")
# LOOP
for (k_start in start_dates) {
  for (k_set in 1:nrow(plot_settings)) {
p <- ggplot(df_cases %>% filter(date>as.Date(k_start)), aes(x=date,y=rollingRate,color=age)) + 
  geom_line(size=1.1) +   # facet_wrap(~age) + #  ,scales="free_y"
  scale_x_date(expand=expansion(0.02,0),date_breaks="1 month") + theme_bw() + standard_theme + xlab("") + 
  ylab("7-day average of CASES per MILLION population") + 
  theme(strip.text=element_text(size=14),panel.grid.minor.y=element_blank()) # panel.grid.minor.x=element_line(linetype="dashed"),
if (plot_settings[k_set,1]=="log") { 
  log_breaks <- 2^(-5:14); if (k_set==5 & k_start>ymd("2021-01-01")) {log_breaks=round(2^seq(-5,14,by=1/2)) }
  p<-p+scale_y_log10(expand=expansion(0.03,0),breaks=log_breaks ) } 
  else { p <- p + scale_y_continuous() } # sapply(seq(-2,4,1/2),function(x) round(10^x,ifelse(x<0,round(x+3),1)))
if (plot_settings[k_set,3]=="facet"){
  if (plot_settings[k_set,2]=="fixed") { p<-p+facet_wrap(~age,scales="fixed") } else {
    p <- p + facet_wrap(~age,scales="free_y") }}
p
# save
print(plot_settings[k_set,])
foldername<-paste0("cases_hosp_deaths_from_",gsub("-","_",as.character(k_start)),"/")
if (!dir.exists(foldername)) {dir.create(foldername)}
filename<-paste0("england_cases_by_age_lineplot",
      ifelse(grepl("log",p$scales$scales[[2]]$trans$name),"_log","_linear"),
      ifelse(class(p$facet)[1]=="FacetNull","_nofacet",""),
      ifelse(plot_settings[k_set,2]=="fixed","_yfixed",""), ".png")
ggsave(paste0(foldername,filename),width=34,height=22,units="cm")
  }
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# hospital admissions by age
hosp_url<-"https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissionsByAge&format=csv"
eng_hosp_age_data <- read_csv(hosp_url) %>% group_by(age) %>% 
  mutate(`new admissions`=roll_mean(value-lag(value,n=1,order_by=date),n=7,align="center",fill=NA),
    rate_chng=rate-lag(rate,n=1,order_by=date),
    rate_chng_smooth=roll_mean(rate_chng,n=7,align="center",fill=NA),
    age=factor(age,levels=c("0_to_5", "6_to_17","18_to_64","65_to_84","85+")))
# plot
# start_dates <- c("2020-12-01","2021-07-01")
for (k_start in start_dates) {
  for (k_set in 1:nrow(plot_settings)) {
p<-ggplot(eng_hosp_age_data %>% filter(date>as.Date(k_start)),
          aes(x=date,y=rate_chng_smooth*10,color=age)) + geom_line(size=1.3) + 
  # geom_hline(aes(yintercept=max(rate_chng_smooth)),color="red") + 
  scale_x_date(expand=expansion(0.02,0),date_breaks="1 month") + 
  xlab("") + ylab("7-day average of admissions per MILLION population") + 
  theme_bw() + standard_theme + theme(strip.text=element_text(size=14),panel.grid.minor.y=element_blank()); p

if (plot_settings[k_set,1]=="log") { 
  log_breaks <- 2^(-4:10); if (k_set==5 & k_start>ymd("2021-01-01")) {log_breaks=round(2^seq(-4,10,by=1/2),1) }
  p<-p+scale_y_log10(expand=expansion(0.03,0), breaks=log_breaks) } else {
  p <- p + scale_y_continuous() }
# sapply(seq(-2,4,1/2),function(x) round(10^x,ifelse(x<0,round(x+3),1)))
if (plot_settings[k_set,3]=="facet"){
  if (plot_settings[k_set,2]=="fixed") { p<-p+facet_wrap(~age,scales="fixed") } else {
    p <- p + facet_wrap(~age,scales="free_y") }}
p; print(plot_settings[k_set,])
# SAVE
foldername<-paste0("cases_hosp_deaths_from_",gsub("-","_",as.character(k_start)),"/")
if (!dir.exists(foldername)) {dir.create(foldername)}
filename<-paste0("england_admissions_by_age",
                 ifelse(grepl("log",p$scales$scales[[2]]$trans$name),"_log","_linear"),
                 ifelse(class(p$facet)[1]=="FacetNull","_nofacet",""),
                 ifelse(plot_settings[k_set,2]=="fixed","_yfixed",""), ".png")
ggsave(paste0(foldername,filename),width=34,height=22,units="cm")
  }
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# DEATHS

death_url<-"https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newDeaths28DaysByDeathDateAgeDemographics&format=csv"
deaths_age<-read_csv(death_url) %>% group_by(age) %>% 
  mutate(rolling_rate_per_alldeaths=roll_mean(deaths,n=7,align="center",fill=NA)) %>% ungroup()  

# lineplots
df_deaths <- left_join(deaths_age,ons_all_age_groups_uk_england_2019 %>% select(!UK),by="age") %>% 
  rename(population=England) %>% mutate(age_uplim=as.numeric(gsub("^.*_","",age)),
            age_grp=ifelse(age_uplim<=49,"00_49",ifelse(age_uplim>=49&age_uplim<=59,"50_59",age)),
            age_grp=ifelse(grepl("\\+",age),age,age_grp)) %>% filter(!age %in% c("00_59","60+")) %>% 
  group_by(age_grp,date) %>%
  summarise(rollingSum=sum(rollingSum),population=sum(population)) %>% ungroup() %>% 
  mutate(agegrp_no=as.numeric(factor(age_grp)),meta_age=ifelse(agegrp_no==1,1, ceiling((agegrp_no+1)/2))) %>%
  group_by(date,meta_age) %>% 
  summarise(rollingSum=sum(rollingSum),population=sum(population),min_age=unique(age_grp)[1],
            max_age=ifelse(agegrp_no==1,unique(age_grp)[1],unique(age_grp)[2])) %>%
  mutate(age_grp=paste0(substr(min_age,1,3),gsub("_","",substr(max_age,nchar(max_age)-2,nchar(max_age)))),
         rollingRate=1e6*rollingSum/(7*population))

# plot DEATHS in 10-yr groups
plot_settings<-expand.grid(list(c("log","linear"),c("fixed","free"),c("nofacet","facet"))) %>% 
  rename(y_scale=Var1,y_range=Var2,faceting=Var3) %>% filter(!(faceting=="nofacet" & y_range=="fixed"))
start_dates <- c("2020-12-01","2021-07-01")
# LOOP
for (k_start in start_dates) {
  for (k_set in 1:nrow(plot_settings)) {
p <- ggplot(df_deaths %>% filter(date>as.Date(k_start)), 
            aes(x=date,y=rollingRate,color=age_grp)) + geom_line(size=1.1) + 
  scale_x_date(expand=expansion(0.02,0),date_breaks="1 month") +
  theme_bw() + standard_theme + theme(strip.text=element_text(size=14),panel.grid.minor.y=element_blank()) +
  xlab("") + ylab("7-day average of deaths per million population") 

  if (plot_settings[k_set,1]=="log") { 
    log_breaks <- 2^(-3:8); if (k_set==5 & k_start>ymd("2021-01-01")) {log_breaks=round(2^seq(-4.5,8,by=1/2),3) }
    p<-p+scale_y_log10(expand=expansion(0.03,0), breaks=log_breaks ) 
        } else {
        p <- p + scale_y_continuous() }
if (plot_settings[k_set,3]=="facet"){
if (plot_settings[k_set,2]=="fixed") { p<-p+facet_wrap(~age_grp,scales="fixed") } else {
                                                          p <- p + facet_wrap(~age_grp,scales="free_y") }}
p
# save
foldername<-paste0("cases_hosp_deaths_from_",gsub("-","_",as.character(k_start)),"/")
if (!dir.exists(foldername)) {dir.create(foldername)}
filename<-paste0("england_deaths_by_age_lineplot",
                 ifelse(grepl("log",p$scales$scales[[2]]$trans$name),"_log","_linear"),
                 ifelse(class(p$facet)[1]=="FacetNull","_nofacet",""),
                 ifelse(plot_settings[k_set,2]=="fixed","_yfixed",""),".png")
ggsave(paste0(foldername,filename),width=34,height=22,units="cm")
print(paste0(filename," (",k_start,")"))
  }
}

##############################################
# cumulative DEATHS
cumul_deaths <- deaths_age %>% mutate(age_uplim=as.numeric(gsub("^.*_","",age)),
    age_grp=ifelse(age_uplim<=49,"00_49",ifelse(age_uplim>=49&age_uplim<=59,"50_59",age)),
    age_grp=ifelse(grepl("\\+",age),age,age_grp)) %>% filter(!age %in% c("00_59","60+")) %>%
  filter(age_grp %in% c("00_49","50_59","60_64","65_69","70_74","75_79","80_84","85_89","90+")) %>% 
  group_by(age_grp,date) %>% 
  summarise(deaths=sum(deaths),
            rolling_rate_per_alldeaths=sum(rolling_rate_per_alldeaths),rollingSum=sum(rollingSum)) %>% 
  rename(age=age_grp) %>% group_by(age) %>% 
  summarise(sum_deaths=sum(deaths)) %>% mutate(share_deaths=sum_deaths/sum(sum_deaths)) %>% 
  pivot_longer(!age) %>% mutate(age=factor(age,unique(age)),age_num=as.numeric(rev(age)))
df_cumuldeath <- cumul_deaths %>% 
  mutate(lower_lim=gsub("00","0",gsub("_[0-9]+","+",as.character(age)))) %>%
  group_by(name,age_num) %>% group_by(name) %>% arrange(age_num) %>% 
  mutate(cum_sum=round(cumsum(value),3), value_str=ifelse(name %in% "share_deaths", 
             paste0(100*round(value,3),"% (",lower_lim,": ",100*cum_sum,"%)"),
             paste0(round(value/1e3,1),"e3")) ) %>%
  mutate(name=ifelse(name %in% "share_deaths","% of all deaths","number of deaths"))

# plot
ggplot(df_cumuldeath) + geom_bar(aes(x=1,y=ifelse(value<1,round(value*1e2,1),value),fill=age),
                                 color="black",position="stack",stat="identity") + 
  geom_text(aes(x=1,y=ifelse(value<1,round(value*1e2,1),value),label=value_str),size=4,
            position=position_stack(vjust = 0.5)) +
  facet_wrap(~name,scales="free") + standard_theme + theme_bw() + xlab("") + ylab("% of all deaths") + 
  scale_x_continuous(expand=expansion(0.1,0)) + scale_y_continuous(expand=expansion(0.001,0)) +
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank(),strip.text=element_text(size=15),
        axis.text.y=element_text(size=15),axis.title.y=element_text(size=15))  
# save
ggsave(paste0("cumul_deaths_by_age.png"),width=18,height=22,units="cm") # _ylog

#
unlink("Rplots.pdf")