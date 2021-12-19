### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# plot CUMUL DEATHS by age
# df_plot_deaths=deaths_age %>% mutate(age_uplim=as.numeric(gsub("^.*_","",age)),
#    age_grp=ifelse(age_uplim<=49,"00_49",ifelse(age_uplim>=49&age_uplim<=59,"50_59",age)),age_grp=ifelse(grepl("\\+",age),age,age_grp)) %>%
#   filter(!age %in% c("00_59","60+")) %>%
#   filter(age_grp %in% c("00_49","50_59","60_64","65_69","70_74","75_79","80_84","85_89","90+")) %>% group_by(age_grp,date) %>% 
#   summarise(deaths=sum(deaths),rolling_rate_per_alldeaths=sum(rolling_rate_per_alldeaths),rollingSum=sum(rollingSum)) %>% 
#   rename(age=age_grp) %>% select(date,age,rolling_rate_per_alldeaths) %>% group_by(date) %>% 
#   mutate(rollingrate_share=rolling_rate_per_alldeaths/sum(rolling_rate_per_alldeaths),age=factor(age,levels=rev(unique(age)))) %>%
#   group_by(age) %>% mutate(rollingrate_share=roll_mean(rollingrate_share,n=12,align="center",fill=NA)) %>% ungroup()
# # plot
# ggplot(df_plot_deaths %>% pivot_longer(!c(date,age))) + geom_area(aes(x=date,y=value,fill=age),color="black",size=0.2) + 
#   facet_wrap(~name,scales="free_y") + # geom_line(aes(x=date,y=rollingRate,color=age)) + 
#   scale_x_date(date_breaks="month",expand=expansion(0.01,0)) + scale_y_continuous(expand=expansion(0,0)) +
#   xlab("") + ylab("deaths by age group") + theme_bw() + standard_theme  # ,scales="free_y"
# # save
# ggsave(paste0("deaths_by_age_area.png"),width=34,height=22,units="cm") # _ylog

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# incidence as stacked area
# ggplot(eng_hosp_age_data %>% filter(date>ymd("2020-08-01")) %>% select(date,`new admissions`,age) %>% group_by(date) %>% 
#          mutate(share_hosp_by_age=`new admissions`/sum(`new admissions`)) %>% pivot_longer(!c(age,date)),
#     aes(x=date,y=value,fill=age)) + geom_area(color="black",size=0.2) + facet_wrap(~name,scales="free_y",nrow=2) +
#   theme_bw() + standard_theme + xlab("") + ylab("COV-19 hospital admissions in England") + theme(strip.text=element_text(size=12)) +
#   scale_x_date(expand=expansion(0,0),date_breaks="month") + scale_y_continuous(expand=expansion(0.01,0))  
# # save
# ggsave(paste0("england_hosp_admissions_by_age_share.png"),width=34,height=22,units="cm") # _ylog

# weekly change in incidence
# n_day=7
# ggplot(eng_hosp_age_data %>% group_by(age) %>%
#        mutate(biweekly_adm=roll_sum(value-lag(value,n=1,order_by=date),n=n_day,align="left",fill=NA)) %>% group_by(age) %>%
#        mutate(chng_weekly_adm=1e2*(biweekly_adm-lag(biweekly_adm,n=n_day,order_by=date))/biweekly_adm) %>%  # ,n=14,align="center",fill=NA)
#        filter(date>ymd("2020-12-01")), aes(x=date,y=chng_weekly_adm)) + geom_line() + facet_wrap(~age,scales="free_y") + # 
#   theme_bw() + standard_theme + xlab("") + ylab("change in 2-week admissions (%)") + theme(strip.text=element_text(size=12),
#   legend.position="top") + scale_x_date(expand=expansion(0.01,0),date_breaks="month") + 
#   geom_hline(yintercept=0,size=1/3,linetype="dashed",color="red") 
# # save
# ggsave(paste0("england_hosp_admissions_change_by_age.png"),width=34,height=22,units="cm") # _ylog
# # each age group separately
# ggplot(eng_case_age_data %>% filter(date>as.Date("2021-06-01")),aes(x=date,y=rollingRate)) + # rollingSum
#   geom_line() + facet_wrap(~age) + xlab("") + ylab("rolling sum cases") + # ,scales="free_y"
#   scale_x_date(expand=expansion(0.01,0),breaks="2 week") + scale_y_log10(expand=expansion(0.01,0)) +
#   theme_bw() + standard_theme + theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
#   strip.text=element_text(size=14),legend.title=element_text(size=12),legend.text=element_text(size=12),legend.position="bottom") +
#   xlab("") + ylab("weekly rate of cases")
# ggsave(paste0("england_cases_age_rollingrate.png"),width=34,height=22,units="cm")
# ggsave(paste0("england_cases_age_rollingrate_y_free.png"),width=34,height=22,units="cm")
# ggsave(paste0("england_cases_age_rollingrate_y_lin.png"),width=34,height=22,units="cm")

# area plot (number + share of all cases)
# p<-ggplot(eng_case_age_data %>% filter(date>as.Date("2021-08-01")) %>% mutate(cases=rollingSum/7) %>% group_by(date) %>%
#          summarise(age=age,age_categ=age_categ,`% total`=cases/sum(cases),cases=cases) %>% 
#          pivot_longer(!c(date,age,age_categ)) %>% group_by(date,age_categ,name) %>% mutate(rank_in_categ=row_number())) + 
#   geom_area(aes(x=date,y=value,fill=factor(rank_in_categ)),color="black",size=0.2) + scale_fill_brewer(palette="YlOrRd") +
#   # geom_line(aes(x=date,y=ifelse(value<1,value*100,value),color=factor(rank_in_categ)),size=1.1) +
#   # scale_y_log10(breaks=sapply(10^seq(-3,4,by=1/2), function(x) round(x,max(3-round(log(x)),0)))) +
#   facet_grid(name~age_categ,scales="free_y") + scale_x_date(expand=expansion(0.01,0),breaks="1 week") + 
#   theme_bw() + standard_theme + theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
#      strip.text=element_text(size=14),legend.title=element_blank(),legend.text=element_text(size=12),plot.caption=element_text(size=12)) + 
#   xlab("") + ylab("cases") + labs(color="5y age band within age group",caption=paste0("agegroups: ",agegr_names)); p
# #### SAVE
# ggsave(paste0("england_cases_age_share_",ifelse(grepl("Area",class(p$layers[[1]]$geom)[[1]]),"area","line_log"),
#               ".png",collapse=""),width=36,height=22,units="cm")
#
#  geom_point(aes(y=daily_first_dose_perc_agegroup,x=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage, #color="first dose"
#     fill=date_numeric),shape=21,stroke=0.2,size=circle_size) + 
# geom_point(aes(y=daily_second_dose_perc_agegroup,x=cumVaccinationCompleteCoverageByVaccinationDatePercentage, #color="second dose",
#                fill=date_numeric), shape=21,stroke=0.2,size=circle_size) + scale_fill_gradient(low="gray98",high="gray45") + 
# geom_path(aes(y=daily_second_dose_perc_agegroup,x=cumVaccinationCompleteCoverageByVaccinationDatePercentage,color=date))

# ### import data --------------------------
# # overview_2020-12-25.csv
# uk_case_data=read_csv("uk_data/specimenDate_ageDemographic-stacked.csv")
# # as.Date("2020-06-01")
# 
# # cumulative numbers
# comp_agegr=c("0_59","60+")
# truthfilter=!uk_case_data$age %in% "unassigned" & !uk_case_data$age %in% comp_agegr & !uk_case_data$areaType %in% "overview"
# n_age=length(unique(uk_case_data[truthfilter,]$age))
# uk_case_data_cumul=uk_case_data[truthfilter,] %>% group_by(areaName,areaType,areaCode,age) %>% 
#           summarise(sum_newCasesBySpecimenDate=sum(newCasesBySpecimenDate),
#                       fact_rate_100k=mean(newCasesBySpecimenDateRollingRate/newCasesBySpecimenDateRollingSum,na.rm=T)) %>%
#           mutate(cases_per100k_agegr=fact_rate_100k*sum_newCasesBySpecimenDate,
#                  cases_100k_totalpop=fact_rate_100k*sum_newCasesBySpecimenDate/n_age) %>%
#           group_by(areaName) %>% mutate(prop_all_cases=sum_newCasesBySpecimenDate/sum(sum_newCasesBySpecimenDate),
#                                         attackrate_perc_pop=sum(cases_100k_totalpop)/1e3)
# agenumlows=as.numeric(sapply(strsplit(unique(uk_case_data_cumul$age),"_|\\+"),"[[",1))
# agegr_ord=unique(uk_case_data_cumul$age)[order(agenumlows)]; uk_case_data_cumul$age=factor(uk_case_data_cumul$age,levels=agegr_ord)
# uk_case_data_cumul[,"nat_subnat"]='subnational'; uk_case_data_cumul$nat_subnat[uk_case_data_cumul$areaType %in% "nation"]="national"
# colnames(uk_case_data_cumul)[colnames(uk_case_data_cumul) %in% "sum_newCasesBySpecimenDate"]="sum_cases"
# uk_case_data_cumul=uk_case_data_cumul %>% pivot_longer(matches("cases|prop_all_cases",ignore.case=T),names_to="variable")
# 
# # PLOT
# truthval_plot=uk_case_data_cumul$variable %in% unique(uk_case_data_cumul$variable)[2:4] &
#   !uk_case_data_cumul$areaType %in% c("ltla","utla","nation"); n_regions=length(unique(uk_case_data_cumul[truthval_plot,]$areaName))
# attackrate_table=uk_case_data_cumul[truthval_plot & uk_case_data_cumul$variable %in% "cases_100k_totalpop",] %>%
#   group_by(areaName,variable) %>% summarise(attackrate_perc_pop=unique(attackrate_perc_pop), var_maxval=max(value))
# ggplot(uk_case_data_cumul[truthval_plot,], aes(x=age,y=value,group=areaName,color=attackrate_perc_pop)) + 
#   geom_line(size=1.04) + geom_point() + facet_grid(variable~areaName,scales="free",switch="y") + 
#   scale_color_gradientn(colours=wes_palette("Zissou1")) + 
#   geom_text(data=attackrate_table,aes(x=4,y=600,label=paste0(round(attackrate_perc_pop,1),"%")),
#             size=3,color="black") + guides(colour=FALSE) + theme_bw() + standard_theme + xlab("") + ylab("")
# #  labs(shape="Region") + scales="free",,nrow=3
# ggsave(paste0("uk_data/regional_rates.png"),width=40,height=16,units="cm")
# 
# ####
# # call COVIDM
# cm_path="~/Desktop/research/models/epid_models/covid_model/covid_lmic_model/covidm/"
# setwd(cm_path); cm_force_rebuild=F; cm_build_verbose=T; cm_version=2; 
# source(file.path(cm_path,"R","covidm.R")); setwd(currentdir_path)
# # countryval="South Africa" ;covid_params=cm_parameters_SEI3R(gsub("Sudan","Ethiopia",countryval))
# # C_m=Reduce('+',covid_params$pop[[1]]$matrices)
# 
# # build parameters for all of UK, down to the regional level (level 2).
# params=cm_parameters_SEI3R(cm_uk_locations("UK",2),deterministic=T);
# # params$pop[[1]]$matrices
# # params$pop[[1]]$u / params$pop[[1]]$y / params$pop[[1]]$fIp / params$pop[[1]]$fIp / params$pop[[1]]$fIs / params$pop[[1]]$fIa
# # $pop[[1]]$rho: Age-specific reporting rate. 
# # $pop[[1]]$tau: Propensity for individuals in each age group to travel to other populs. Only relevant if you have multiple populations.
# # params$pop[[1]]$name: name of region
# # params$pop[[1]]$group_names: age groups
# # params$pop[[1]]$schedule: interventions
# # params = cm_parameters_SEI3R("Italy");
# # 
# # params$pop[[1]]$seed_times = rep(0:6, each = 5) # 5 new infections each day for first 7 days
# # infections start in individuals aged 20-50
# # params$pop[[1]]$dist_seed_ages=cm_age_coefficients(20, 50, 5*(0:length(params$pop[[1]]$size)))
# 
# # run the model
# run = cm_simulate(params, 1); uk_simul=run$dynamics
# # fractions of total popul
# region_pop=uk_simul[uk_simul$t==0 & uk_simul$compartment %in% c("S","E","Ip","Is","R")] %>% group_by(population,group) %>% summarise(pop=sum(value))
# region_pop=left_join(region_pop,region_pop %>% group_by(population) %>% summarise(tot_reg_pop=sum(pop)),by="population")
# uk_simul=left_join(uk_simul,region_pop,by=c("population","group"))
# uk_simul[,"value_regpop_perc"]=round(uk_simul$value/uk_simul$tot_reg_pop,4); uk_simul[,"value_agegrpop_perc"]=round(uk_simul$value/uk_simul$pop,4)
# # fraction of the compartment at each timepoint
# uk_simul=uk_simul %>% group_by(t,compartment,population) %>% mutate(value_comp_t_perc=value/sum(value))
# # show results for all regions
# ggplot(uk_simul[compartment == "cases"]) + geom_line(aes(t, value, colour = group, group = group)) + facet_wrap(~population) + 
#   theme_bw() + standard_theme
# # one region
# varname="value_perc"
# ggplot(uk_simul[compartment == "cases" & population == "UK | SOUTH EAST"]) + 
#   geom_line(aes_string(x="t",y=varname,colour="group",group="group")) + facet_wrap(~group) + theme_bw() + standard_theme
# # age distrib at diff timepoints
# ggplot(uk_simul[compartment == "cases" & population == "UK | SOUTH EAST"]) + 
#   geom_line(aes_string(x="t",y=varname,colour="group",group="group")) + facet_wrap(~group) + theme_bw() + standard_theme
# 
# ### ### ### ###
# download.file(url="https://raw.githubusercontent.com/VictimOfMaths/COVID-19/master/Heatmaps/COVIDCasesxVaxTadpoles.R",
#                                       destfile = "COVIDCasesxVaxTadpoles.R")
# file.edit("COVIDCasesxVaxTadpoles.R")
