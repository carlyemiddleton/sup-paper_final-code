library(lme4)
library(dplyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readxl) 

#########################
####### AUD population ##
#########################
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
  data$TCM.tel<-ifelse(data$TCMclaimsbyTel>0, 1, 0)
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
  data$depression.yn<-ifelse(data$depression>0, 1, 0)
  
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


##data cleaning
data$pandemic <- ifelse(data$year < 2020, 0, 1)
data$RACE <- ifelse(data$RACE == 'Not Provided', NA, data$RACE)
data$RACE <- ifelse(data$RACE==1, 'White',
                    ifelse(data$RACE == 2, 'Black', 'Other'))
data$RACE <- factor(data$RACE)
data$RACE <- relevel(as.factor(data$RACE), ref = 'White')
data$year <- as.factor(data$year)
data$year <- as.numeric(as.character(data$year))
data$yearm2016 <- data$year - 2016
data$yearm2020 <- data$year - 2020


##########################################
## :Logistic regression models for mental health variables:  AUD only
##########################################

#self harm
(start.time <- Sys.time())
fit <- glmer(formula = selfharm.yn ~ depression.yn +  PS +  BT + PS*BT  + Tel +
               age + GENDER + RACE + yearm2016 + pandemic + pandemic*yearm2020 + 
               mco + ADD_REGION + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)


#suicidal ideation
(start.time <- Sys.time())
fit <- glmer(formula = suicidalideations.yn ~ depression.yn +  PS +  BT + PS*BT  + Tel +
               age + GENDER + RACE + yearm2016 + pandemic + pandemic*yearm2020 + 
               mco + ADD_REGION + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)

#Suicide attempt
(start.time <- Sys.time())
fit <- glmer(formula = suicideattempt.yn ~ depression.yn +  PS +  BT + PS*BT  + Tel +
               age + GENDER + RACE + yearm2016 + pandemic + pandemic*yearm2020 + 
               mco + ADD_REGION + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)

##########################################
## :Logistic regression models for service variables:  AUD only
##########################################
library(lme4)
data$mco3 <- ifelse(data$mco != 'UNKWN' & data$mco != 'V4BZZ', 'NAMED', 
                    data$mco) 

##Behavior Therapy
#######

#Hospitalization 
(start.time <- Sys.time())
fit <- glmer(formula = HOSPITAL ~ BT3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)
##From logistic.1-25 notes page 14:
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(exp(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.UB <- round(exp(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.df <- data.frame(Est = round(exp(fixef(fit)[3]-fixef(fit)[2]), 2), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)


#ER 
(start.time <- Sys.time())
fit <- glmer(formula = ER ~ BT3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(exp(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.UB <- round(exp(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.df <- data.frame(Est = round(exp(fixef(fit)[3]-fixef(fit)[2]), 2), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)

#Cost 
(start.time <- Sys.time())
fit <- lmer(formula = cost ~ BT3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data) 
summary(fit)
(start.time <- Sys.time())
#round((fixef(fit)), 2) #coefficients
#round((coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
#round((coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(Est = round(coef(summary(fit))[,1],0), 
                 CI = paste0('(',round(coef(summary(fit))[,1] - 
                                             qnorm(.025, lower.tail = F)*coef(summary(fit))[,2], 0),', ',
                                 round(coef(summary(fit))[,1] + 
                                             qnorm(.025, lower.tail = F)*coef(summary(fit))[,2], 0),')'), 
                 p.val =  round(pnorm(abs(coef(summary(fit))[,1]/coef(summary(fit))[,2]), lower.tail = F), 2)
                 )
View(df)
##From logistic.1-25 notes page 14:
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt), 0)
contrast.UB <- round(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt), 0)
contrast.df <- data.frame(Est = round(fixef(fit)[3]-fixef(fit)[2], 0), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                                   contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)

##Peer Support
#######

#Hospitalization 
(start.time <- Sys.time())
fit <- glmer(formula = HOSPITAL ~ PS3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(exp(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.UB <- round(exp(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.df <- data.frame(Est = round(exp(fixef(fit)[3]-fixef(fit)[2]), 2), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)

#ER 
(start.time <- Sys.time())
fit <- glmer(formula = ER ~ PS3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(exp(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.UB <- round(exp(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.df <- data.frame(Est = round(exp(fixef(fit)[3]-fixef(fit)[2]), 2), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)

#Cost 
(start.time <- Sys.time())
fit <- lmer(formula = cost ~ PS3 + depression.yn + age + GENDER + RACE + 
              yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
              Metro + (1|BEN_ID)
            , data = data) 
summary(fit)
(start.time <- Sys.time())
#round((fixef(fit)), 2) #coefficients
#round((coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
#round((coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(Est = round(coef(summary(fit))[,1],0), 
                 CI = paste0('(',round(coef(summary(fit))[,1] - 
                                         qnorm(.025, lower.tail = F)*coef(summary(fit))[,2], 0),', ',
                             round(coef(summary(fit))[,1] + 
                                     qnorm(.025, lower.tail = F)*coef(summary(fit))[,2], 0),')'), 
                 p.val =  round(pnorm(abs(coef(summary(fit))[,1]/coef(summary(fit))[,2]), lower.tail = F), 2)
)
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt), 0)
contrast.UB <- round(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt), 0)
contrast.df <- data.frame(Est = round(fixef(fit)[3]-fixef(fit)[2], 0), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)


##TCM
#######

#Hospitalization 
(start.time <- Sys.time())
fit <- glmer(formula = HOSPITAL ~ TCM3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(exp(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.UB <- round(exp(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.df <- data.frame(Est = round(exp(fixef(fit)[3]-fixef(fit)[2]), 2), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)

#ER 
(start.time <- Sys.time())
fit <- glmer(formula = ER ~ TCM3 + depression.yn + age + GENDER + RACE + 
               yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
               Metro + (1|BEN_ID)
             , data = data, family=binomial, nAGQ = 0) #nAGQ = 0 helps it run faster
summary(fit)
(start.time <- Sys.time())
round(exp(fixef(fit)), 2) #odds ratios
round(exp(coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
round(exp(coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(OR = round(exp(coef(summary(fit))[,1]),2), CI = paste0('(',round(exp(coef(summary(fit))[,1] - 
                                                                                        qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),', ',round(exp(coef(summary(fit))[,1] + 
                                                                                                                                                                 qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2),')'), p.val = round(coef(summary(fit))[,4],2))
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(exp(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.UB <- round(exp(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt)), 2)
contrast.df <- data.frame(Est = round(exp(fixef(fit)[3]-fixef(fit)[2]), 2), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)

#Cost 
(start.time <- Sys.time())
fit <- lmer(formula = cost ~ TCM3 + depression.yn + age + GENDER + RACE + 
              yearm2016 + pandemic + pandemic*yearm2020 + mco3 +
              Metro + (1|BEN_ID)
            , data = data) 
summary(fit)
(start.time <- Sys.time())
#round((fixef(fit)), 2) #coefficients
#round((coef(summary(fit))[,1] - qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)  #CIs
#round((coef(summary(fit))[,1] + qnorm(.025, lower.tail = F)*coef(summary(fit))[,2]), 2)
df <- data.frame(Est = round(coef(summary(fit))[,1],0), 
                 CI = paste0('(',round(coef(summary(fit))[,1] - 
                                         qnorm(.025, lower.tail = F)*coef(summary(fit))[,2], 0),', ',
                             round(coef(summary(fit))[,1] + 
                                     qnorm(.025, lower.tail = F)*coef(summary(fit))[,2], 0),')'), 
                 p.val =  round(pnorm(abs(coef(summary(fit))[,1]/coef(summary(fit))[,2]), lower.tail = F), 2)
)
View(df)
V <- vcov(fit)
var.bfmbt <- diag(V)[3]+diag(V)[2] - 2*V[2,3]
contrast.LB <- round(fixef(fit)[3]-fixef(fit)[2] - qnorm(.025, lower.tail = F)*sqrt(var.bfmbt), 0)
contrast.UB <- round(fixef(fit)[3]-fixef(fit)[2] + qnorm(.025, lower.tail = F)*sqrt(var.bfmbt), 0)
contrast.df <- data.frame(Est = round(fixef(fit)[3]-fixef(fit)[2], 0), 
                          contrast.CI = paste0('(',contrast.LB,  ', ',
                                               contrast.UB,')'),
                          p.val = round(pnorm(abs(fixef(fit)[3]-fixef(fit)[2])/sqrt(var.bfmbt), lower.tail=F), 2))
View(contrast.df)












