library(dplyr)
#library(ggplot2)
#library(stringr)
#library(dplyr)
#library(ggplot2)
#library(gridExtra)
#library(readxl) 
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

Summarized_Table_AUD <- data %>%group_by(year) %>%
    summarize(N=n(), 
    Cost=paste(round(median(cost)), "(",round(quantile(cost, probs=0.025)),
     ", ", round(quantile(cost, probs=0.975)), ")", sep=""),  
    Tel = paste(round(mean(Tel)*100, 1), "%", sep=""),
    BT=paste(round(mean(BT)*100, 1), "%", sep=""),
    BT.tel=paste(round(mean(BT.tel)*100, 1), "%", sep=""), 
    PS=paste(round(mean(PS)*100, 1), "%", sep=""),
    PS.tel=paste(round(mean(PS.tel)*100, 1), "%", sep=""),
    Hospitalization=paste(round(mean(HOSPITAL)*100, 1), "%", sep=""), 
    InpatientHos1=paste(round(mean(InpatientHos1)*100, 1), "%", sep=""), 
    ER=paste(round(mean(ER)*100, 1), "%", sep=""),
    self.harm=paste(round(mean(selfharm.yn)*100, 1), "%", sep=""),
    suicideattempt=paste(round(mean(suicideattempt.yn)*100, 1), "%", sep=""),
    suicidalideations=paste(round(mean(suicidalideations.yn)*100, 1), "%", sep=""),
    TCM=paste(round(mean(TCM)*100, 1), "%", sep=""),
    Anxdep=paste(round(mean(anxdep.yn)*100, 1), "%", sep=""))
#write.csv(Summarized_Table_AUD, "Summarized_Table_AUD.csv")  

Summarized_Table_genpop <- datap %>%group_by(year) %>%
  summarize(N=n(), 
            Cost=paste(round(median(cost)), "(",round(quantile(cost, probs=0.025)),
                       ", ", round(quantile(cost, probs=0.975)), ")", sep=""),  
            Tel = paste(round(mean(Tel)*100, 1), "%", sep=""),
            BT=paste(round(mean(BT)*100, 1), "%", sep=""),
            BT.tel=paste(round(mean(BT.tel)*100, 1), "%", sep=""), 
            PS=paste(round(mean(PS)*100, 1), "%", sep=""),
            PS.tel=paste(round(mean(PS.tel)*100, 1), "%", sep=""),
            Hospitalization=paste(round(mean(HOSPITAL)*100, 1), "%", sep=""), 
            InpatientHos1=paste(round(mean(InpatientHos1)*100, 1), "%", sep=""), 
            ER=paste(round(mean(ER)*100, 1), "%", sep=""),
            self.harm=paste(round(mean(selfharm.yn)*100, 1), "%", sep=""),
            suicideattempt=paste(round(mean(suicideattempt.yn)*100, 1), "%", sep=""),
            suicidalideations=paste(round(mean(suicidalideations.yn)*100, 1), "%", sep=""),
            TCM=paste(round(mean(TCM)*100, 1), "%", sep=""),
            Anxdep=paste(round(mean(anxdep.yn)*100, 1), "%", sep=""))
#write.csv(Summarized_Table_genpop, "Summarized_Table_genpop.csv")  

#% AUD in the general population
round(Summarized_Table_AUD$N/Summarized_Table_genpop$N,3)
