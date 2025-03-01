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

### Generate summarized table for ggplots
Tab_Cov_Bin <- function(data, Covariate, Bin_Outcomes){
  Cov.Bin <- data %>%
  group_by(!!sym(Covariate), year) %>%
  summarize(Outcome = mean(!!sym(Bin_Outcomes))*100)
  return(Cov.Bin)
}

Tab_Cov_Con <- function(data, Covariate, Con_Outcomes){
    Cov.Con <- data %>%
    group_by(!!sym(Covariate), year) %>%
    summarize(Outcome = median(!!sym(Con_Outcomes))/1000)
    return(Cov.Con)
  }

####################
##  Plot function ##
####################

plot.fun<-function(data, Bin_Outcomes=NULL, ylab=NULL, Con_Outcomes=NULL)
  # Plot for age
{ if(is.character(Bin_Outcomes)& Bin_Outcomes!='AUD'){filtered_data_age <- Tab_Cov_Bin(data, Covariate="Age.Cat", Bin_Outcomes=Bin_Outcomes)}
  if(is.character(Con_Outcomes)){filtered_data_age <- Tab_Cov_Con(data, Covariate="Age.Cat", Con_Outcomes=Con_Outcomes)}
  if(Bin_Outcomes=='AUD'){filtered_data_age <- calculate_percentage_aud(covariate="Age.Cat")}
plot_age <- ggplot(filtered_data_age, aes(x = year, y = Outcome, linetype=Age.Cat, color = Age.Cat, group = Age.Cat)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 3, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = paste("B: ", ylab, " Over Years by Age", sep=""),
    x = "Year",
    y = ylab
  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=20),
    axis.title.y =element_text(size=18),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c('red','blue','green','orange','purple','red')) +
  scale_linetype_manual(values = c(1,1,1,1,1,2))


# Plot for gender
if(is.character(Bin_Outcomes) & Bin_Outcomes!='AUD'){filtered_data_gender <- Tab_Cov_Bin(data, Covariate="GENDER", Bin_Outcomes=Bin_Outcomes)}
if(is.character(Con_Outcomes)){filtered_data_gender <- Tab_Cov_Con(data, Covariate="GENDER", Con_Outcomes=Con_Outcomes)}
if(Bin_Outcomes=='AUD'){filtered_data_gender <- calculate_percentage_aud(covariate="GENDER")}
filtered_data_gender<-filtered_data_gender[filtered_data_gender$GENDER %in% c("F","M"),]
plot_gender <- ggplot(filtered_data_gender, aes(x = year, y = Outcome,linetype=GENDER, color = GENDER, group = GENDER)) +
  geom_line(size = 1.5) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(
    title = paste("A: ",ylab, " Over Years by Gender", sep=""),
    x = "Year",
    y = ylab  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=20),
    axis.title.y =element_text(size=18),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c(1,1))


# Plot for race
if(is.character(Bin_Outcomes) & Bin_Outcomes!='AUD'){filtered_data_race <- Tab_Cov_Bin(data, Covariate="RACE", Bin_Outcomes=Bin_Outcomes)}
if(is.character(Con_Outcomes)){filtered_data_race <- Tab_Cov_Con(data, Covariate="RACE", Con_Outcomes=Con_Outcomes)}
if(Bin_Outcomes=='AUD'){filtered_data_race <- calculate_percentage_aud(covariate="RACE")}
plot_race <- ggplot(filtered_data_race, aes(x = year, y = Outcome,  linetype=RACE, color = RACE, group = RACE)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 3, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = paste("C: ",ylab, " Over Years by Race",sep=""),
    x = "Year",
    y = ylab
  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=20),
    axis.title.y =element_text(size=18),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c('red','blue','green','orange')) +
  scale_linetype_manual(values = c(1,1,1,1))


# Plot for metro
if(is.character(Bin_Outcomes) & Bin_Outcomes!='AUD'){filtered_data_metro <- Tab_Cov_Bin(data, Covariate="Metro", Bin_Outcomes=Bin_Outcomes)}
if(is.character(Con_Outcomes)){filtered_data_metro <- Tab_Cov_Con(data, Covariate="Metro", Con_Outcomes=Con_Outcomes)}
if(Bin_Outcomes=='AUD'){filtered_data_metro <- calculate_percentage_aud(covariate="Metro")}
plot_metro <- ggplot(filtered_data_metro, aes(x = year, y = Outcome, linetype = Metro, color = Metro, group = Metro)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 3, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = paste("F: ",ylab, " Over Years by Metro", sep=""),
    x = "Year",
    y = ylab  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=20),
    axis.title.y =element_text(size=18),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c(1,1))



# Plot for MCO
if(is.character(Bin_Outcomes) & Bin_Outcomes!='AUD'){filtered_data_mco <- Tab_Cov_Bin(data, Covariate="mco", Bin_Outcomes=Bin_Outcomes)}
if(is.character(Con_Outcomes)){filtered_data_mco <- Tab_Cov_Con(data, Covariate="mco", Con_Outcomes=Con_Outcomes)}
if(Bin_Outcomes=='AUD'){filtered_data_mco <- calculate_percentage_aud(covariate="mco")}
plot_mco <- ggplot(filtered_data_mco, aes(x = year, y = Outcome, linetype = mco,color = mco, group = mco)) +
  geom_line(size = 1.5) +  # Thicker lines
  geom_point(size = 3, show.legend = FALSE) +  # Add points for visibility
  labs(
    title = paste("E: ",ylab, " Over Years by MCO",sep=""),
    x = "Year",
    y = ylab  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=20),
    axis.title.y =element_text(size=18),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c('red','blue','green','orange','purple', 					
                                "red", "blue", "green")) +
  scale_linetype_manual(values = c(1,1,1,1,
                                   3,3,3,3))

# Plot for ADD_REGION
if(is.character(Bin_Outcomes) & Bin_Outcomes!='AUD'){filtered_data_ADD <- Tab_Cov_Bin(data, Covariate="ADD_REGION", Bin_Outcomes=Bin_Outcomes)}
if(is.character(Con_Outcomes)){filtered_data_ADD <- Tab_Cov_Con(data, Covariate="ADD_REGION", Con_Outcomes=Con_Outcomes)}
if(Bin_Outcomes=='AUD'){filtered_data_ADD <- calculate_percentage_aud(covariate="ADD_REGION")}
plot_add_region <- ggplot(filtered_data_ADD, aes(x = year, y = Outcome,  linetype = ADD_REGION, color = ADD_REGION, group = ADD_REGION)) +
  geom_line(size = 1.5) +
  geom_point(size = 3, show.legend = FALSE) +
  labs(
    title = paste("D: ",ylab, " Over Years by ADD Region", sep=""),
    x = "Year",
    y = ylab  ) +
  theme_minimal() +
  theme(
    legend.key.width = unit(3, "line"),
    title =element_text(size=20),
    axis.title.y =element_text(size=18),
    axis.text =element_text(size=18),
    legend.text =element_text(size=15),
    legend.position = "right",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )+
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple",
                                "red", "blue", "green", "orange", "purple",
                                "red", "blue", "green", "orange", "purple")) +
  scale_linetype_manual(values = c(1,1,1,1,1,
                                   2,2,2,2,2,
                                   3,3,3,3,3))




grid.arrange(
  plot_gender, plot_age, plot_race, plot_add_region, plot_mco, plot_metro,
  ncol = 2  
)
}  #End of plot function

#setwd("C:/Users/carly/Documents/sup-paper_final-code/appendix-line-plots")
png('anxdep.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="anxdep.yn", ylab="Dep/Anx (%)")
dev.off()
png('Tel.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="Tel", ylab="Telehealth (%)")
dev.off()
png('BT.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="BT", ylab="Behavior Therapy (%)")
dev.off()
png('PS.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="PS", ylab="Peer Support (%)")
dev.off()
png('TCM.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="TCM", ylab="TCM (%)")
dev.off()
png('cost.png', width = 1900, height = 1100)
plot.fun(data, Con_Outcomes="cost", ylab="Median Cost in $1000")
dev.off()
png('hospital.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="HOSPITAL", ylab="Hospitalization (%)")
dev.off()
png('ER.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="ER", ylab="ER Visit (%)")
dev.off()
png('selfharm.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="selfharm.yn", ylab="Self Harm (%)")
dev.off()
png('ideation.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="suicidalideations.yn", ylab="Suicidal Ideation (%)")
dev.off()
png('attempt.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="suicideattempt.yn", ylab="Suicide Attempt (%)")
dev.off()

##############
## AUD plot ##
##############

calculate_percentage_aud <- function(general_pop=datap, aud_patients=data, covariate='GENDER'){
  # Count the total population by year and covariate
  general_pop_counts <- general_pop %>%
    group_by(year, !!sym(covariate)) %>%
    dplyr::summarize(total_population = n(), .groups = "drop")
  
  # Count the total AUD patients by year and covariate
  aud_patients_counts <- aud_patients %>%
    group_by(year, !!sym(covariate)) %>%
    dplyr::summarize(total_aud_patients = n(), .groups = "drop")
  
  # Merge and calculate percentages
  combined_data <- general_pop_counts %>%
    inner_join(aud_patients_counts, by = c("year", covariate)) %>%
    mutate(percentage_aud = (total_aud_patients / total_population) * 100)
  combined_data$total_population <- combined_data$total_aud_patients <- NULL
  names(combined_data)[3] <- 'Outcome'
  return(combined_data)
}

png('AUD.png', width = 1900, height = 1100)
plot.fun(data, Bin_Outcomes="AUD", ylab="AUD (%)")
dev.off()









