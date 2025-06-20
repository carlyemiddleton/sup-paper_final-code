 library(dplyr)
 library(ggplot2)
 library(gridExtra)
 ####################################################################333
 ###   Data for general population
 #######################################################################
 #the location of the output datasets from genpop-query-for-manuscript.sql
 setwd('C:/Users/carly/Box/AUD_Services_SUP_2023/2024_December_Data_Report/Data_with_TCM_Anx_Dep/Gen Pop')
 data16p <- read.csv('2016.Gen.csv')
 data16p$year<-2016
 #head(data16)
 data17p <- read.csv('2017.Gen.csv')
 data17p$year<-2017
 data18p <- read.csv('2018.Gen.csv')
 data18p$year<-2018
 data19p <- read.csv('2019.Gen.csv')
 data19p$year<-2019
 data20p <- read.csv('2020.Gen.csv')
 data20p$year<-2020
 data21p <- read.csv('2021.Gen.csv')
 data21p$year<-2021
 data22p <- read.csv('2022.Gen.csv')
 data22p$year<-2022
 datap<-rbind(data16p, data17p, data18p, data19p, data20p, data21p, data22p)
 
 
 #############################################################################
 ##  AUD population
 ############################################################################
 #the location of the output datasets from AUD-query-for-manuscript.sql
 setwd("C:/Users/carly/Box/AUD_Services_SUP_2023/2024_December_Data_Report/Data_TCM-by-Tel_Anx_Dep/AUD")
 
 data16 <- read.csv('data16.AUD.csv')
 data16$year<-2016
 #head(data16)
 data17 <- read.csv('data17.AUD.csv')
 data17$year<-2017
 data18 <- read.csv('data18.AUD.csv')
 data18$year<-2018
 data19 <- read.csv('data19.AUD.csv')
 data19$year<-2019
 data20 <- read.csv('data20.AUD.csv')
 data20$year<-2020
 data21 <- read.csv('data21.AUD.csv')
 data21$year<-2021
 data22 <- read.csv('data22.AUD.csv')
 data22$year<-2022
 data<-rbind(data16, data17, data18, data19, data20, data21, data22)
 
 
 ### Function to add additional variables
 Expand<-function(data)
 { ### Intervention Varaibles
   data$PS<-ifelse(data$Psclaims>0, 1, 0)
   data$PS.tel<-ifelse(data$PSclaimsbyTel>0, 1, 0)
   data$BT<-ifelse(data$BTclaims>0, 1, 0)
   data$BT.tel<-ifelse(data$BTclaimsbyTel>0, 1, 0)
   data$Tel<-ifelse(data$Telclaims>0, 1, 0)
   data$PS3<-ifelse(data$Psclaims<1, "No.PS",ifelse(data$PSclaimsbyTel>0, "PS.Tel", "PS.f2f"))
   data$BT3<-ifelse(data$BTclaims<1, "No.BT",ifelse(data$BTclaimsbyTel>0, "BT.Tel", "BT.f2f"))
   data$TCM3<-ifelse(data$TCM<1, "No.TCM",ifelse(data$TCMclaimsbyTel>0, "TCM.Tel", "TCM.f2f"))
   data$PS3<-factor(data$PS3, levels=c("No.PS","PS.Tel", "PS.f2f"))
   data$BT3<-factor(data$BT3, levels=c("No.BT","BT.Tel", "BT.f2f"))
   data$TCM3<-factor(data$TCM3, levels=c("No.TCM","TCM.Tel", "TCM.f2f"))
 
   #Outcome variables
   data$cost=data$amt_pait+data$amt_encounter
   data$HOSPITAL<-ifelse(data$InpatientHos+data$InpatientPF>0, 1, 0)
   data$InpatientHos1<-ifelse(data$InpatientHos>0, 1, 0)
   data$HOS.All<-ifelse(data$InpatientHos>0 & data$InpatientPF>0, "Hos_PF",
                        ifelse(data$InpatientHos>0 & data$InpatientPF==0,"Hos_only",
                               ifelse(data$InpatientHos==0 & data$InpatientPF>0,"PF_only", "No")))
   data$ER<-ifelse(data$ERvisit>0, 1, 0)
   data$selfharm.yn<-ifelse(data$selfharm>0, 1, 0)
   data$suicideattempt.yn<-ifelse(data$suicideattempt>0, 1, 0)
   data$suicidalideations.yn<-ifelse(data$suicidalideations>0, 1, 0)
   data$anxdep.yn<-ifelse(data$Anxiety>0 | data$depression > 0, 1, 0)
 
   ## Confounding variables
   data$Metro<-ifelse(data$metro>data$nonmetro, "Yes", "No")
   data$Age.Cat<-cut(data$age, breaks = c(11,18,24,34,44,54,65))
   data$Covid<-ifelse(data$year<2020, 0, 1)
   data$RACE<-factor(data$RACE, levels=c("White","Black","Other", "Not Provided"))
   data$Metro<-factor(data$Metro, levels=c("Yes", "No"))
 
   data <- data[data$GENDER!="U",]
   data<-data[data$ADD_REGION!="Out of State",]
   return(data)
 }
 ##### End of Expand function
 
 data<-Expand(data)
 data$TCM.tel<-ifelse(data$TCMclaimsbyTel>0, 1, 0)
 datap <- Expand(datap)
 
 Tab_Cov_Bin <- function(data, Covariate, Bin_Outcomes) 
 {   data<-data[!is.na(data[,Covariate]),]
 Cov.Bin <- data %>%
   group_by(!!sym(Covariate), year) %>%
   summarize(Outcome = mean(!!sym(Bin_Outcomes))*100)
 return(Cov.Bin)
 }
 
 Tab_Cov_Con <- function(data, Covariate, Con_Outcome) 
 {   data<-data[!is.na(data[,Covariate]),]
 Cov.Bin <- data %>%
   group_by(!!sym(Covariate), year) %>%
   summarize(Outcome = median(!!sym(Con_Outcome)))
 return(Cov.Bin)
 }
 setwd("C:/Users/carly/Documents/sup-paper_final-code/fig2-modes-of-delivery-plots")

##########################################################################
##  Three level services
############################################################################
plot.fun_Service_Outcome<-function(data){  
  ### Behavior therapy
  dat.services<-data %>%group_by(year) %>%
    summarize(BT=mean(BT)*100, BT.tel=mean(BT.tel)*100, BT.f2f=mean(BT-BT.tel))
  dat.serv<-data.frame(year=rep(dat.services$year, 3), Services=rep(c("BT", "BT.tel", "BT.f2f"),each=7), 
                       prop=c(dat.services$BT,dat.services$BT.tel,dat.services$BT.f2f))
  dat.serv$Services<-factor(dat.serv$Services, levels=c("BT", "BT.tel", "BT.f2f"))
  plot_BH_Use <- ggplot(dat.serv, aes(x = year, y = prop, linetype = Services,color = Services, group = Services)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "A1: Behavior Therapy Over Years", 
      x = "Year",
      y = "Behavior therapy (%)"
    ) +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(2,1,2,3))
  
  
  ###############PS
  dat.services<-data %>%group_by(year) %>%
    summarize(PS=mean(PS)*100, PS.tel=mean(PS.tel)*100, PS.f2f=mean(PS-PS.tel))
  dat.serv<-data.frame(year=rep(dat.services$year, 3), Services=rep(c("PS", "PS.tel", "PS.f2f"),each=7), 
                       prop=c(dat.services$PS,dat.services$PS.tel,dat.services$PS.f2f))
  dat.serv$Services<-factor(dat.serv$Services, levels=c("PS", "PS.tel", "PS.f2f"))
  plot_PS_Use <- ggplot(dat.serv, aes(x = year, y = prop, linetype = Services,color = Services, group = Services)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "A2: Peer Support Over Years", 
      x = "Year",
      y = "Peer Support (%)"
    ) +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(2,1,2,3))
  
  
  ###############TCM
  dat.services<-data %>%group_by(year) %>%
    summarize(TCM=mean(TCM)*100, TCM.tel=mean(TCM.tel)*100, TCM.f2f=mean(TCM-TCM.tel))
  dat.serv<-data.frame(year=rep(dat.services$year, 3), Services=rep(c("TCM", "TCM.tel", "TCM.f2f"),each=7), 
                       prop=c(dat.services$TCM,dat.services$TCM.tel,dat.services$TCM.f2f))
  dat.serv$Services<-factor(dat.serv$Services, levels=c("TCM", "TCM.tel", "TCM.f2f"))
  plot_TCM_Use <- ggplot(dat.serv, aes(x = year, y = prop, linetype = Services,color = Services, group = Services)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "A3: TCM Over Years", 
      x = "Year",
      y = "TCM (%)"
    ) +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(2,1,2,3))
  
  
  ## BT3 on cost
  filtered_data_BT3<-Tab_Cov_Con(data, Covariate="BT3", Con_Outcome="cost") 
  plot_BT3_Cost <- ggplot(subset(filtered_data_BT3, !(year %in% c(2016:2019) & 
                                                        filtered_data_BT3$BT3 %in% c('BT.Tel'))),
                          aes(x = year, y = Outcome/1000, linetype = BT3,color = BT3, group = BT3)) +
    geom_line(size = 1.5) +
    geom_point(size = 2, show.legend = FALSE) +
    labs(
      title = "B1: Median Cost Over Years by Behavior Therapy",
      x = "Year",
      y = "Median Cost in $1000") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  
  ## PS3 on cost
  filtered_data_PS3<-Tab_Cov_Con(data, Covariate="PS3", Con_Outcome="cost") 
  plot_PS3_Cost <- ggplot(subset(filtered_data_PS3, !(year %in% c(2016:2019) & 
                                 filtered_data_PS3$PS3 %in% c('PS.Tel'))),
                          aes(x = year, y = Outcome/1000,linetype = PS3,  color = PS3, group = PS3)) +
    geom_line(size = 1.5) +
    geom_point(size = 2, show.legend = FALSE) +
    labs(
      title = "B2: Median Cost Over Years by Peer Support",
      x = "Year",
      y = "Median Cost in $1000") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  ## TCM3 on cost
  filtered_data_TCM3<-Tab_Cov_Con(data, Covariate="TCM3", Con_Outcome="cost") 
  plot_TCM3_Cost <- ggplot(subset(filtered_data_TCM3, !(year %in% c(2016:2019) & 
                                  filtered_data_TCM3$TCM3 %in% c('TCM.Tel'))),
                           aes(x = year, y = Outcome/1000,linetype = TCM3,  color = TCM3, group = TCM3)) +
    geom_line(size = 1.5) +
    geom_point(size = 2, show.legend = FALSE) +
    labs(
      title = "B3: Median Cost Over Years by TCM",
      x = "Year",
      y = "Median Cost in $1000") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  #names(data)"HOSPITAL"      "ER" 
  filtered_data_BT3<-Tab_Cov_Bin(data, Covariate="BT3", Bin_Outcomes="HOSPITAL") 
  plot_BT3_Hos <- ggplot(subset(filtered_data_BT3, !(year %in% c(2016:2019) & 
                                filtered_data_BT3$BT3 %in% c('BT.Tel'))),
                         aes(x = year, y = Outcome,linetype = BT3, color = BT3, group = BT3)) +
    geom_line(size = 1.5) +
    geom_point(size = 2, show.legend = FALSE) +
    labs(
      title = "C1: Hospitalization Over Years by Behavior Therapy",
      x = "Year",
      y = "Hospitalization (%)") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  filtered_data_PS3<-Tab_Cov_Bin(data, Covariate="PS3", Bin_Outcomes="HOSPITAL") 
  plot_PS3_Hos <- ggplot(subset(filtered_data_PS3, !(year %in% c(2016:2019) & 
                                filtered_data_PS3$PS3 %in% c('PS.Tel'))),
                         aes(x = year, y = Outcome,linetype = PS3, color = PS3, group = PS3)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "C2: Hospitalization Over Years by Peer Support",
      x = "Year",
      y = "Hospitalization (%)") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text.y =element_text(size=14),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1,size=18)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  filtered_data_TCM3<-Tab_Cov_Bin(data, Covariate="TCM3", Bin_Outcomes="HOSPITAL") 
  plot_TCM3_Hos <- ggplot(subset(filtered_data_TCM3, !(year %in% c(2016:2019) & 
                                 filtered_data_TCM3$TCM3 %in% c('TCM.Tel'))),
                          aes(x = year, y = Outcome,linetype = TCM3, color = TCM3, group = TCM3)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "C3: Hospitalization Over Years by TCM",
      x = "Year",
      y = "Hospitalization (%)") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  #names(data)"HOSPITAL"      "ER" 
  filtered_data_BT3<-Tab_Cov_Bin(data, Covariate="BT3", Bin_Outcomes="ER") 
  plot_BT3_ER <- ggplot(subset(filtered_data_BT3, !(year %in% c(2016:2019) & 
                               filtered_data_BT3$BT3 %in% c('BT.Tel'))),
                        aes(x = year, y = Outcome, linetype = BT3,color = BT3, group = BT3)) +
    geom_line(size = 1.5) +
    geom_point(size = 2, show.legend = FALSE) +
    labs(
      title = "D1: ER visit Over Years by Behavior Therapy",
      x = "Year",
      y = "ER visits (%)") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  
  filtered_data_PS3<-Tab_Cov_Bin(data, Covariate="PS3", Bin_Outcomes="ER") 
  plot_PS3_ER <- ggplot(subset(filtered_data_PS3, !(year %in% c(2016:2019) & 
                               filtered_data_PS3$PS3 %in% c('PS.Tel'))),
                        aes(x = year, y = Outcome,linetype = PS3,  color = PS3, group = PS3)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "D2: ER visits Over Years by Peer Support",
      x = "Year",
      y = "ER visits (%)") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text.y =element_text(size=14),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1,size=18)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  
  filtered_data_TCM3<-Tab_Cov_Bin(data, Covariate="TCM3", Bin_Outcomes="ER") 
  plot_TCM3_ER <- ggplot(subset(filtered_data_TCM3, !(year %in% c(2016:2019) & 
                                filtered_data_TCM3$TCM3 %in% c('TCM.Tel'))),
                         aes(x = year, y = Outcome,linetype = TCM3,  color = TCM3, group = TCM3)) +
    geom_line(size = 1.5) +  # Thicker lines
    geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
    labs(
      title = "D3: ER visits Over Years by TCM",
      x = "Year",
      y = "ER visits (%)") +
    theme_minimal() +
    theme(
      legend.key.width = unit(3, "line"),
      title =element_text(size=13),
      axis.title =element_text(size=15),
      axis.text =element_text(size=18),
      legend.text =element_text(size=15),
      legend.position = "right",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1)
    )+
    scale_color_manual(values = c("blue", "orange",  "purple")) +
    scale_linetype_manual(values = c(1,2,3)) + 
    theme(axis.text.x=element_text(hjust=0.5))
  
  return(list(
    plot_BH_Use,   plot_PS_Use,   plot_TCM_Use,  
    plot_BT3_Cost, plot_PS3_Cost, plot_TCM3_Cost,
    plot_BT3_Hos,  plot_PS3_Hos,  plot_TCM3_Hos,
    plot_BT3_ER,   plot_PS3_ER,   plot_TCM3_ER
              ))
  #png(file='service-outcome-plot.png',width=1300, height=900)
  #grid.arrange(
  #  plot_BH_Use,   plot_PS_Use,   plot_TCM_Use,  
  #  plot_BT3_Cost, plot_PS3_Cost, plot_TCM3_Cost,
  #  plot_BT3_Hos,  plot_PS3_Hos,  plot_TCM3_Hos,
  #  plot_BT3_ER,   plot_PS3_ER,   plot_TCM3_ER,
  #  ncol = 3  
  #)
  #dev.off()
  
}  #End of plot function


plot.list <- plot.fun_Service_Outcome(data)


#############################################################
## Add 3 more rows of plots for the mental health outcomes ##
#############################################################
#names(data)"HOSPITAL"      "ER" 
filtered_data_BT3<-Tab_Cov_Bin(data, Covariate="BT3", Bin_Outcomes="suicidalideations.yn") 
plot_BT3_SI <- ggplot(subset(filtered_data_BT3, !(year %in% c(2016:2019) & 
                             filtered_data_BT3$BT3 %in% c('BT.Tel'))),
                      aes(x = year, y = Outcome, linetype = BT3,color = BT3, group = BT3)) +
  geom_line(size = 1.5) +
  geom_point(size = 2, show.legend = FALSE) +
  labs(
    title = "E1: Suicidal Ideation Over Years by Behavior Therapy",
    x = "Year",
    y = "Suicidal Ideation (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))



filtered_data_PS3<-Tab_Cov_Bin(data, Covariate="PS3", Bin_Outcomes="suicidalideations.yn") 
plot_PS3_SI <- ggplot(subset(filtered_data_PS3, !(year %in% c(2016:2019) & 
                             filtered_data_PS3$PS3 %in% c('PS.Tel'))),
                      aes(x = year, y = Outcome,linetype = PS3,  color = PS3, group = PS3)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = "E2: Suicidal Ideation Over Years by Peer Support",
    x = "Year",
    y = "Suicidal Ideation (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))


filtered_data_TCM3<-Tab_Cov_Bin(data, Covariate="TCM3", Bin_Outcomes="suicidalideations.yn") 
plot_TCM3_SI <- ggplot(subset(filtered_data_TCM3, !(year %in% c(2016:2019) & 
                              filtered_data_TCM3$TCM3 %in% c('TCM.Tel'))),
                       aes(x = year, y = Outcome,linetype = TCM3,  color = TCM3, group = TCM3)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = "E3: Suicidal Ideation Over Years by TCM",
    x = "Year",
    y = "Suicidal Ideation (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))

filtered_data_BT3<-Tab_Cov_Bin(data, Covariate="BT3", Bin_Outcomes="selfharm.yn") 
plot_BT3_SH <- ggplot(subset(filtered_data_BT3, !(year %in% c(2016:2019) & 
                             filtered_data_BT3$BT3 %in% c('BT.Tel'))),
                      aes(x = year, y = Outcome, linetype = BT3,color = BT3, group = BT3)) +
  geom_line(size = 1.5) +
  geom_point(size = 2, show.legend = FALSE) +
  labs(
    title = "F1: Self Harm Over Years by Behavior Therapy",
    x = "Year",
    y = "Self Harm (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))



filtered_data_PS3<-Tab_Cov_Bin(data, Covariate="PS3", Bin_Outcomes="selfharm.yn") 
plot_PS3_SH <- ggplot(subset(filtered_data_PS3, !(year %in% c(2016:2019) & 
                             filtered_data_PS3$PS3 %in% c('PS.Tel'))),
                      aes(x = year, y = Outcome,linetype = PS3,  color = PS3, group = PS3)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = "F2: Self Harm Over Years by Peer Support",
    x = "Year",
    y = "Self Harm (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))


filtered_data_TCM3<-Tab_Cov_Bin(data, Covariate="TCM3", Bin_Outcomes="selfharm.yn") 
plot_TCM3_SH <- ggplot(subset(filtered_data_TCM3, !(year %in% c(2016:2019) & 
                              filtered_data_TCM3$TCM3 %in% c('TCM.Tel'))),
                       aes(x = year, y = Outcome,linetype = TCM3,  color = TCM3, group = TCM3)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = "F3: Self Harm Over Years by TCM",
    x = "Year",
    y = "Self Harm (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))

filtered_data_BT3<-Tab_Cov_Bin(data, Covariate="BT3", Bin_Outcomes="suicideattempt.yn") 
plot_BT3_SA <- ggplot(subset(filtered_data_BT3, !(year %in% c(2016:2019) & 
                             filtered_data_BT3$BT3 %in% c('BT.Tel'))),
                      aes(x = year, y = Outcome, linetype = BT3,color = BT3, group = BT3)) +
  geom_line(size = 1.5) +
  geom_point(size = 2, show.legend = FALSE) +
  labs(
    title = "G1: Suicide Attempt Over Years by Behavior Therapy",
    x = "Year",
    y = "Suicide Attempt (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))



filtered_data_PS3<-Tab_Cov_Bin(data, Covariate="PS3", Bin_Outcomes="suicideattempt.yn") 
plot_PS3_SA <- ggplot(subset(filtered_data_PS3, !(year %in% c(2016:2019) & 
                             filtered_data_PS3$PS3 %in% c('PS.Tel'))), 
                      aes(x = year, y = Outcome,linetype = PS3,  color = PS3, group = PS3)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = "G2: Suicide Attempt Over Years by Peer Support",
    x = "Year",
    y = "Suicide Attempt (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))


filtered_data_TCM3<-Tab_Cov_Bin(data, Covariate="TCM3", Bin_Outcomes="suicideattempt.yn") 
plot_TCM3_SA <- ggplot(subset(filtered_data_TCM3, !(year %in% c(2016:2019) & 
                                                      filtered_data_TCM3$TCM3 %in% c('TCM.Tel'))),
                       aes(x = year, y = Outcome,linetype = TCM3,  color = TCM3, group = TCM3)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 2, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = "G3: Suicide Attempt Over Years by TCM",
    x = "Year",
    y = "Suicide Attempt (%)") +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=13),
    axis.title =element_text(size=15),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("blue", "orange",  "purple")) +
  scale_linetype_manual(values = c(1,2,3)) + 
  theme(axis.text.x=element_text(hjust=0.5))

#png(file='mental-outcome-plot.png',width=1300, height=675)
grid.arrange(
  plot_BT3_SI, plot_PS3_SI, plot_TCM3_SI,
  plot_BT3_SH, plot_PS3_SH, plot_TCM3_SH,
  plot_BT3_SA, plot_PS3_SA, plot_TCM3_SA,
  ncol = 3  
)
#dev.off()


tiff(file='fig2.tiff',width=13000*.4, height=15000*.4, res=300)
grid.arrange(
  plot.list[[1]],plot.list[[2]],plot.list[[3]],
  plot.list[[4]],plot.list[[5]],plot.list[[6]],
  plot.list[[7]],plot.list[[8]],plot.list[[9]],
  plot.list[[10]],plot.list[[11]],plot.list[[12]],
  plot_BT3_SI, plot_PS3_SI, plot_TCM3_SI,
  plot_BT3_SH, plot_PS3_SH, plot_TCM3_SH,
  plot_BT3_SA, plot_PS3_SA, plot_TCM3_SA,
  ncol = 3  
)
dev.off()


