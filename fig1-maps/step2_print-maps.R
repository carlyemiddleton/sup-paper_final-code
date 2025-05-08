library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(readxl)

states <- map_data("state")
counties <- map_data("county")
ken.region <- subset(states, region %in% c("kentucky"))
ken.county <- subset(counties, region %in% c("kentucky"))
cnames <- aggregate(cbind(long, lat) ~ subregion, data=ken.county, 
                    FUN=function(x)mean(range(x)))

region <- read_excel("ADDregions.xlsx")
region <- apply(region, 2, tolower)
region <- as.data.frame(region)
colnames(region)[6] <- 'FIVCO'
region1 <- region[, 1]
region2 <- region[, 2]
region3 <- region[, 3]
region4 <- region[, 4]
region5 <- region[, 5]
region6 <- region[, 6]
region7 <- region[, 7]
region8 <- region[, 8]
region9 <- region[, 9]
region10 <- region[, 10]
region11 <- region[, 11]
region12 <- region[, 12]
region13 <- region[, 13]
region14 <- region[, 14]
region15 <- region[, 15]
ken.d <- ken.county
ken.d$region <- ifelse(ken.county$subregion %in% region1, "Barren River",
                       ifelse(ken.county$subregion %in% region2, "Big Sandy",
                              ifelse(ken.county$subregion %in% region3, "Bluegrass",
                                     ifelse(ken.county$subregion %in% region4, "Buffalo Trace",
                                            ifelse(ken.county$subregion %in% region5, "Cumberland Valley", 
                                                   ifelse(ken.county$subregion %in% region6, "FIVCO",
                                                          ifelse(ken.county$subregion %in% region7, "Gateway", 
                                                                 ifelse(ken.county$subregion %in% region8, "Green River",
                                                                        ifelse(ken.county$subregion %in% region9, "Kentucky River",
                                                                               ifelse(ken.county$subregion %in% region10, "KIPDA",
                                                                                      ifelse(ken.county$subregion %in% region11, "Lake Cumberland",
                                                                                             ifelse(ken.county$subregion %in% region12, "Lincoln Trail",
                                                                                                    ifelse(ken.county$subregion %in% region13, "Northern Kentucky",
                                                                                                           ifelse(ken.county$subregion %in% region14, "Pennyrile", "Purchase"))))))))))))))
##Make the percentages to put in the map using step1_make-map-percentages, 
##and then paste the result into "Data_2022_PopulationSize_AUDonly_cm.xlsx"

Data_2022_PopulationSize_All <- read_excel("Data_2022_PopulationSize_AUDonly_cm.xlsx")
dep.props <- Data_2022_PopulationSize_All$Dep_Anx ###
Tel.props <- Data_2022_PopulationSize_All$Tel ###
Behavior_Therapy.props <- Data_2022_PopulationSize_All$Behavior_Therapy  #
Peer_Support.props <- Data_2022_PopulationSize_All$Peer_Support   #
TCM.props <- Data_2022_PopulationSize_All$TCM ###
Cost.props <- round(Data_2022_PopulationSize_All$Cost,0) ###
HOSPITAL.props <- Data_2022_PopulationSize_All$HOSPITAL  #
ER_visit.props <- Data_2022_PopulationSize_All$ER_visit ###
selfharm.yn.props <- Data_2022_PopulationSize_All$selfharm.yn ###
suicidalideations.yn.props <- Data_2022_PopulationSize_All$suicidalideations.yn  #
suicideattempt.yn.props <- Data_2022_PopulationSize_All$suicideattempt.yn ###
aud <- Data_2022_PopulationSize_All$AUD_percentage ###

ken.d$suicidalideations.yn.props <- ifelse(ken.d$region == "Barren River", suicidalideations.yn.props[1],
                                           ifelse(ken.d$region == "Big Sandy", suicidalideations.yn.props[2],
                                                  ifelse(ken.d$region == "Bluegrass", suicidalideations.yn.props[3],
                                                         ifelse(ken.d$region == "Buffalo Trace", suicidalideations.yn.props[4],
                                                                ifelse(ken.d$region == "Cumberland Valley", suicidalideations.yn.props[5],
                                                                       ifelse(ken.d$region == "FIVCO", suicidalideations.yn.props[6],
                                                                              ifelse(ken.d$region == "Gateway", suicidalideations.yn.props[7],
                                                                                     ifelse(ken.d$region == "Green River", suicidalideations.yn.props[8],
                                                                                            ifelse(ken.d$region == "Kentucky River", suicidalideations.yn.props[9],
                                                                                                   ifelse(ken.d$region == "KIPDA", suicidalideations.yn.props[10],
                                                                                                          ifelse(ken.d$region == "Lake Cumberland", suicidalideations.yn.props[11],
                                                                                                                 ifelse(ken.d$region == "Lincoln Trail", suicidalideations.yn.props[12],
                                                                                                                        ifelse(ken.d$region == "Northern Kentucky", suicidalideations.yn.props[13],
                                                                                                                               ifelse(ken.d$region == "Pennyrile", suicidalideations.yn.props[14], suicidalideations.yn.props[15]))))))))))))))


ken.d$suicideattempt.yn.props <- ifelse(ken.d$region == "Barren River", suicideattempt.yn.props[1],
                                        ifelse(ken.d$region == "Big Sandy", suicideattempt.yn.props[2],
                                               ifelse(ken.d$region == "Bluegrass", suicideattempt.yn.props[3],
                                                      ifelse(ken.d$region == "Buffalo Trace", suicideattempt.yn.props[4],
                                                             ifelse(ken.d$region == "Cumberland Valley", suicideattempt.yn.props[5],
                                                                    ifelse(ken.d$region == "FIVCO", suicideattempt.yn.props[6],
                                                                           ifelse(ken.d$region == "Gateway", suicideattempt.yn.props[7],
                                                                                  ifelse(ken.d$region == "Green River", suicideattempt.yn.props[8],
                                                                                         ifelse(ken.d$region == "Kentucky River", suicideattempt.yn.props[9],
                                                                                                ifelse(ken.d$region == "KIPDA", suicideattempt.yn.props[10],
                                                                                                       ifelse(ken.d$region == "Lake Cumberland", suicideattempt.yn.props[11],
                                                                                                              ifelse(ken.d$region == "Lincoln Trail", suicideattempt.yn.props[12],
                                                                                                                     ifelse(ken.d$region == "Northern Kentucky", suicideattempt.yn.props[13],
                                                                                                                            ifelse(ken.d$region == "Pennyrile", suicideattempt.yn.props[14], suicideattempt.yn.props[15]))))))))))))))

ken.d$selfharm.yn.props <- ifelse(ken.d$region == "Barren River", selfharm.yn.props[1],
                                  ifelse(ken.d$region == "Big Sandy", selfharm.yn.props[2],
                                         ifelse(ken.d$region == "Bluegrass", selfharm.yn.props[3],
                                                ifelse(ken.d$region == "Buffalo Trace", selfharm.yn.props[4],
                                                       ifelse(ken.d$region == "Cumberland Valley", selfharm.yn.props[5],
                                                              ifelse(ken.d$region == "FIVCO", selfharm.yn.props[6],
                                                                     ifelse(ken.d$region == "Gateway", selfharm.yn.props[7],
                                                                            ifelse(ken.d$region == "Green River", selfharm.yn.props[8],
                                                                                   ifelse(ken.d$region == "Kentucky River", selfharm.yn.props[9],
                                                                                          ifelse(ken.d$region == "KIPDA", selfharm.yn.props[10],
                                                                                                 ifelse(ken.d$region == "Lake Cumberland", selfharm.yn.props[11],
                                                                                                        ifelse(ken.d$region == "Lincoln Trail", selfharm.yn.props[12],
                                                                                                               ifelse(ken.d$region == "Northern Kentucky", selfharm.yn.props[13],
                                                                                                                      ifelse(ken.d$region == "Pennyrile", selfharm.yn.props[14], selfharm.yn.props[15]))))))))))))))

ken.d$ER_visit.props <- ifelse(ken.d$region == "Barren River", ER_visit.props[1],
                               ifelse(ken.d$region == "Big Sandy", ER_visit.props[2],
                                      ifelse(ken.d$region == "Bluegrass", ER_visit.props[3],
                                             ifelse(ken.d$region == "Buffalo Trace", ER_visit.props[4],
                                                    ifelse(ken.d$region == "Cumberland Valley", ER_visit.props[5],
                                                           ifelse(ken.d$region == "FIVCO", ER_visit.props[6],
                                                                  ifelse(ken.d$region == "Gateway", ER_visit.props[7],
                                                                         ifelse(ken.d$region == "Green River", ER_visit.props[8],
                                                                                ifelse(ken.d$region == "Kentucky River", ER_visit.props[9],
                                                                                       ifelse(ken.d$region == "KIPDA", ER_visit.props[10],
                                                                                              ifelse(ken.d$region == "Lake Cumberland", ER_visit.props[11],
                                                                                                     ifelse(ken.d$region == "Lincoln Trail", ER_visit.props[12],
                                                                                                            ifelse(ken.d$region == "Northern Kentucky", ER_visit.props[13],
                                                                                                                   ifelse(ken.d$region == "Pennyrile", ER_visit.props[14], ER_visit.props[15]))))))))))))))

ken.d$dep.props <- ifelse(ken.d$region == "Barren River", dep.props[1],
                          ifelse(ken.d$region == "Big Sandy", dep.props[2],
                                 ifelse(ken.d$region == "Bluegrass", dep.props[3],
                                        ifelse(ken.d$region == "Buffalo Trace", dep.props[4],
                                               ifelse(ken.d$region == "Cumberland Valley", dep.props[5],
                                                      ifelse(ken.d$region == "FIVCO", dep.props[6],
                                                             ifelse(ken.d$region == "Gateway", dep.props[7],
                                                                    ifelse(ken.d$region == "Green River", dep.props[8],
                                                                           ifelse(ken.d$region == "Kentucky River", dep.props[9],
                                                                                  ifelse(ken.d$region == "KIPDA", dep.props[10],
                                                                                         ifelse(ken.d$region == "Lake Cumberland", dep.props[11],
                                                                                                ifelse(ken.d$region == "Lincoln Trail", dep.props[12],
                                                                                                       ifelse(ken.d$region == "Northern Kentucky", dep.props[13],
                                                                                                              ifelse(ken.d$region == "Pennyrile", dep.props[14], dep.props[15]))))))))))))))


ken.d$Tel.props <- ifelse(ken.d$region == "Barren River", Tel.props[1],
                          ifelse(ken.d$region == "Big Sandy", Tel.props[2],
                                 ifelse(ken.d$region == "Bluegrass", Tel.props[3],
                                        ifelse(ken.d$region == "Buffalo Trace", Tel.props[4],
                                               ifelse(ken.d$region == "Cumberland Valley", Tel.props[5],
                                                      ifelse(ken.d$region == "FIVCO", Tel.props[6],
                                                             ifelse(ken.d$region == "Gateway", Tel.props[7],
                                                                    ifelse(ken.d$region == "Green River", Tel.props[8],
                                                                           ifelse(ken.d$region == "Kentucky River", Tel.props[9],
                                                                                  ifelse(ken.d$region == "KIPDA", Tel.props[10],
                                                                                         ifelse(ken.d$region == "Lake Cumberland", Tel.props[11],
                                                                                                ifelse(ken.d$region == "Lincoln Trail", Tel.props[12],
                                                                                                       ifelse(ken.d$region == "Northern Kentucky", Tel.props[13],
                                                                                                              ifelse(ken.d$region == "Pennyrile", Tel.props[14], Tel.props[15]))))))))))))))

ken.d$TCM.props <- ifelse(ken.d$region == "Barren River", TCM.props[1],
                          ifelse(ken.d$region == "Big Sandy", TCM.props[2],
                                 ifelse(ken.d$region == "Bluegrass", TCM.props[3],
                                        ifelse(ken.d$region == "Buffalo Trace", TCM.props[4],
                                               ifelse(ken.d$region == "Cumberland Valley", TCM.props[5],
                                                      ifelse(ken.d$region == "FIVCO", TCM.props[6],
                                                             ifelse(ken.d$region == "Gateway", TCM.props[7],
                                                                    ifelse(ken.d$region == "Green River", TCM.props[8],
                                                                           ifelse(ken.d$region == "Kentucky River", TCM.props[9],
                                                                                  ifelse(ken.d$region == "KIPDA", TCM.props[10],
                                                                                         ifelse(ken.d$region == "Lake Cumberland", TCM.props[11],
                                                                                                ifelse(ken.d$region == "Lincoln Trail", TCM.props[12],
                                                                                                       ifelse(ken.d$region == "Northern Kentucky", TCM.props[13],
                                                                                                              ifelse(ken.d$region == "Pennyrile", TCM.props[14], TCM.props[15]))))))))))))))

ken.d$Cost.props <- ifelse(ken.d$region == "Barren River", Cost.props[1],
                           ifelse(ken.d$region == "Big Sandy", Cost.props[2],
                                  ifelse(ken.d$region == "Bluegrass", Cost.props[3],
                                         ifelse(ken.d$region == "Buffalo Trace", Cost.props[4],
                                                ifelse(ken.d$region == "Cumberland Valley", Cost.props[5],
                                                       ifelse(ken.d$region == "FIVCO", Cost.props[6],
                                                              ifelse(ken.d$region == "Gateway", Cost.props[7],
                                                                     ifelse(ken.d$region == "Green River", Cost.props[8],
                                                                            ifelse(ken.d$region == "Kentucky River", Cost.props[9],
                                                                                   ifelse(ken.d$region == "KIPDA", Cost.props[10],
                                                                                          ifelse(ken.d$region == "Lake Cumberland", Cost.props[11],
                                                                                                 ifelse(ken.d$region == "Lincoln Trail", Cost.props[12],
                                                                                                        ifelse(ken.d$region == "Northern Kentucky", Cost.props[13],
                                                                                                               ifelse(ken.d$region == "Pennyrile", Cost.props[14], Cost.props[15]))))))))))))))

ken.d$HOSPITAL.props <- ifelse(ken.d$region == "Barren River", HOSPITAL.props[1],
                               ifelse(ken.d$region == "Big Sandy", HOSPITAL.props[2],
                                      ifelse(ken.d$region == "Bluegrass", HOSPITAL.props[3],
                                             ifelse(ken.d$region == "Buffalo Trace", HOSPITAL.props[4],
                                                    ifelse(ken.d$region == "Cumberland Valley", HOSPITAL.props[5],
                                                           ifelse(ken.d$region == "FIVCO", HOSPITAL.props[6],
                                                                  ifelse(ken.d$region == "Gateway", HOSPITAL.props[7],
                                                                         ifelse(ken.d$region == "Green River", HOSPITAL.props[8],
                                                                                ifelse(ken.d$region == "Kentucky River", HOSPITAL.props[9],
                                                                                       ifelse(ken.d$region == "KIPDA", HOSPITAL.props[10],
                                                                                              ifelse(ken.d$region == "Lake Cumberland", HOSPITAL.props[11],
                                                                                                     ifelse(ken.d$region == "Lincoln Trail", HOSPITAL.props[12],
                                                                                                            ifelse(ken.d$region == "Northern Kentucky", HOSPITAL.props[13],
                                                                                                                   ifelse(ken.d$region == "Pennyrile", HOSPITAL.props[14], HOSPITAL.props[15]))))))))))))))
ken.d$Behavior_Therapy.props <- ifelse(ken.d$region == "Barren River", Behavior_Therapy.props[1],
                                       ifelse(ken.d$region == "Big Sandy", Behavior_Therapy.props[2],
                                              ifelse(ken.d$region == "Bluegrass", Behavior_Therapy.props[3],
                                                     ifelse(ken.d$region == "Buffalo Trace", Behavior_Therapy.props[4],
                                                            ifelse(ken.d$region == "Cumberland Valley", Behavior_Therapy.props[5],
                                                                   ifelse(ken.d$region == "FIVCO", Behavior_Therapy.props[6],
                                                                          ifelse(ken.d$region == "Gateway", Behavior_Therapy.props[7],
                                                                                 ifelse(ken.d$region == "Green River", Behavior_Therapy.props[8],
                                                                                        ifelse(ken.d$region == "Kentucky River", Behavior_Therapy.props[9],
                                                                                               ifelse(ken.d$region == "KIPDA", Behavior_Therapy.props[10],
                                                                                                      ifelse(ken.d$region == "Lake Cumberland", Behavior_Therapy.props[11],
                                                                                                             ifelse(ken.d$region == "Lincoln Trail", Behavior_Therapy.props[12],
                                                                                                                    ifelse(ken.d$region == "Northern Kentucky", Behavior_Therapy.props[13],
                                                                                                                           ifelse(ken.d$region == "Pennyrile", Behavior_Therapy.props[14], Behavior_Therapy.props[15]))))))))))))))

ken.d$Peer_Support.props <- ifelse(ken.d$region == "Barren River", Peer_Support.props[1],
                                   ifelse(ken.d$region == "Big Sandy", Peer_Support.props[2],
                                          ifelse(ken.d$region == "Bluegrass", Peer_Support.props[3],
                                                 ifelse(ken.d$region == "Buffalo Trace", Peer_Support.props[4],
                                                        ifelse(ken.d$region == "Cumberland Valley", Peer_Support.props[5],
                                                               ifelse(ken.d$region == "FIVCO", Peer_Support.props[6],
                                                                      ifelse(ken.d$region == "Gateway", Peer_Support.props[7],
                                                                             ifelse(ken.d$region == "Green River", Peer_Support.props[8],
                                                                                    ifelse(ken.d$region == "Kentucky River", Peer_Support.props[9],
                                                                                           ifelse(ken.d$region == "KIPDA", Peer_Support.props[10],
                                                                                                  ifelse(ken.d$region == "Lake Cumberland", Peer_Support.props[11],
                                                                                                         ifelse(ken.d$region == "Lincoln Trail", Peer_Support.props[12],
                                                                                                                ifelse(ken.d$region == "Northern Kentucky", Peer_Support.props[13],
                                                                                                                       ifelse(ken.d$region == "Pennyrile", Peer_Support.props[14], Peer_Support.props[15]))))))))))))))

ken.d$aud.props <- ifelse(ken.d$region == "Barren River", aud[1],
                          ifelse(ken.d$region == "Big Sandy", aud[2],
                                 ifelse(ken.d$region == "Bluegrass", aud[3],
                                        ifelse(ken.d$region == "Buffalo Trace", aud[4],
                                               ifelse(ken.d$region == "Cumberland Valley", aud[5],
                                                      ifelse(ken.d$region == "FIVCO", aud[6],
                                                             ifelse(ken.d$region == "Gateway", aud[7],
                                                                    ifelse(ken.d$region == "Green River", aud[8],
                                                                           ifelse(ken.d$region == "Kentucky River", aud[9],
                                                                                  ifelse(ken.d$region == "KIPDA", aud[10],
                                                                                         ifelse(ken.d$region == "Lake Cumberland", aud[11],
                                                                                                ifelse(ken.d$region == "Lincoln Trail", aud[12],
                                                                                                       ifelse(ken.d$region == "Northern Kentucky", aud[13],
                                                                                                              ifelse(ken.d$region == "Pennyrile", aud[14], aud[15]))))))))))))))

## create labels and locate them 
ken.d$dep.label <- paste(ken.d$region, paste(round(ken.d$dep.props, 2), "%", sep = ""), sep="\n")
dep.rname <- aggregate(cbind(long, lat) ~ dep.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$Tel.label <- paste(ken.d$region, paste(round(ken.d$Tel.props, 2), "%", sep = ""), sep="\n")
Tel.rname <- aggregate(cbind(long, lat) ~ Tel.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$TCM.label <- paste(ken.d$region, paste(round(ken.d$TCM.props, 2), "%", sep = ""), sep="\n")
tcm.rname <- aggregate(cbind(long, lat) ~ TCM.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$Cost.label <- paste(ken.d$region, round(ken.d$Cost.props/1000,2), sep="\n")
Cost.rname <- aggregate(cbind(long, lat) ~ Cost.label, data = ken.d, FUN = function(x) median(range(x)))

ken.d$ER_visit.label <- paste(ken.d$region, paste(round(ken.d$ER_visit.props, 2), "%", sep = ""), sep="\n")
ER_visit.rname <- aggregate(cbind(long, lat) ~ ER_visit.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$selfharm.yn.label <- paste(ken.d$region, paste(round(ken.d$selfharm.yn.props, 2), "%", sep = ""), sep="\n")
selfharm.yn.rname <- aggregate(cbind(long, lat) ~ selfharm.yn.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$suicidalideations.yn.label <- paste(ken.d$region, paste(round(ken.d$suicidalideations.yn.props, 2), "%", sep = ""), sep="\n")
suicidalideations.yn.rname <- aggregate(cbind(long, lat) ~ suicidalideations.yn.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$suicideattempt.yn.label <- paste(ken.d$region, paste(round(ken.d$suicideattempt.yn.props, 2), "%", sep = ""), sep="\n")
suicideattempt.yn.rname <- aggregate(cbind(long, lat) ~ suicideattempt.yn.label, data = ken.d, FUN = function(x) mean(range(x)))

#####
ken.d$HOSPITAL.label <- paste(ken.d$region, paste(round(ken.d$HOSPITAL.props, 2), "%", sep = ""), sep="\n")
HOSPITAL.rname <- aggregate(cbind(long, lat) ~ HOSPITAL.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$Behavior_Therapy.label <- paste(ken.d$region, paste(round(ken.d$Behavior_Therapy.props, 2), "%", sep = ""), sep="\n")
Behavior_Therapy.rname <- aggregate(cbind(long, lat) ~ Behavior_Therapy.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$Peer_Support.label <- paste(ken.d$region, paste(round(ken.d$Peer_Support.props, 2), "%", sep = ""), sep="\n")
Peer_Support.rname <- aggregate(cbind(long, lat) ~ Peer_Support.label, data = ken.d, FUN = function(x) mean(range(x)))

ken.d$aud.label <- paste(ken.d$region, paste(round(ken.d$aud.props, 2), "%", sep = ""), sep="\n")
aud.rname <- aggregate(cbind(long, lat) ~ aud.label, data = ken.d, FUN = function(x) mean(range(x)))

## plot
ken_base <- ggplot(data = ken.region, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region1)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region2)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region3)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region4)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region5)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region6)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region7)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region8)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region9)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region10)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region11)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region12)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region13)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region14)) +
  geom_polygon(color = "black", fill = NA, data = filter(ken.county, subregion %in% region15)) +  
  geom_polygon(data = ken.county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)

# aud
#tiff(filename='map/aud-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p1 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = aud.props, color=region)) +
  geom_text(data = aud.rname[c(1,4,7,14,15),], aes(long, lat, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = aud.rname[13,], aes(long-.1, 38.6+.2, label = aud.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = aud.rname[5,], aes(long, 36.8, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = aud.rname[12,], aes(long, lat-.1, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = aud.rname[8,], aes(long, lat+.1, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = aud.rname[9,], aes(long, lat-.1, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = aud.rname[2,], aes(long, lat+.1, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = aud.rname[11,], aes(long, lat+.1, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = aud.rname[6,], aes(long+.3, lat, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = aud.rname[3,], aes(long-.1, lat, label = aud.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  geom_text(data = aud.rname[10,], aes(long, lat, label = aud.label, group = NULL), size = 8, color = "orange", fontface = 2) + #KIPDA
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(1,10), name='% AUD') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('A: AUD (%) in Different ADDs (2022)') + 
  labs(color='map/ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()


# Behavior_Therapy
#tiff(filename='map/Behavior_Therapy-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p2 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = Behavior_Therapy.props, color=region)) +
  geom_text(data = Behavior_Therapy.rname[c(1,4,7,10,14,15),], aes(long, lat, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = Behavior_Therapy.rname[13,], aes(long-.1, 38.6+.2, label = Behavior_Therapy.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = Behavior_Therapy.rname[5,], aes(long, 36.8, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = Behavior_Therapy.rname[12,], aes(long, lat-.1, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = Behavior_Therapy.rname[8,], aes(long, lat+.1, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = Behavior_Therapy.rname[9,], aes(long, lat-.1, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = Behavior_Therapy.rname[2,], aes(long, lat+.1, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = Behavior_Therapy.rname[11,], aes(long, lat+.1, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = Behavior_Therapy.rname[6,], aes(long+.3, lat, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = Behavior_Therapy.rname[3,], aes(long-.1, lat, label = Behavior_Therapy.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(40, 90), name='% BT') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('B: Behavior Therapy (%) in Different ADDs (2022)') + 
  labs(color='map/ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()

#Depression/ anxiety plot 
#tiff(filename='map/depression-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p3<- ken_base + 
  geom_polygon(data = ken.d, aes(fill = dep.props, color=region)) +
  geom_text(data = dep.rname[c(1,4,7,10,14,15),], aes(long, lat, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = dep.rname[13,], aes(long-.1, 38.6+.2, label = dep.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = dep.rname[5,], aes(long, 36.8, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = dep.rname[12,], aes(long, lat-.1, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = dep.rname[8,], aes(long, lat+.1, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = dep.rname[9,], aes(long, lat-.1, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = dep.rname[2,], aes(long, lat+.1, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = dep.rname[11,], aes(long, lat+.1, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = dep.rname[6,], aes(long+.3, lat, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = dep.rname[3,], aes(long-.1, lat, label = dep.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(50, 80), name='% Dep') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('C: Depression/Anxiety (%) in Different ADDs (2022)') + 
  labs(color='map/ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()


#telehealth map 
#tiff(filename='map/tel-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p4 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = Tel.props, color=region)) +
  geom_text(data = Tel.rname[c(1,4,7,10,14,15),], aes(long, lat, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = Tel.rname[13,], aes(long-.1, 38.6+.2, label = Tel.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = Tel.rname[5,], aes(long, 36.8, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = Tel.rname[12,], aes(long, lat-.1, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = Tel.rname[8,], aes(long, lat+.1, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = Tel.rname[9,], aes(long, lat-.1, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = Tel.rname[2,], aes(long, lat+.1, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = Tel.rname[11,], aes(long, lat+.1, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = Tel.rname[6,], aes(long+.3, lat, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = Tel.rname[3,], aes(long-.1, lat, label = Tel.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(30, 90), name='% Tel') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('D: Telehealth (%) in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()

# Peer_Support
#tiff(filename='map/Peer_Support-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p5 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = Peer_Support.props, color=region)) +
  geom_text(data = Peer_Support.rname[c(1,4,7,10,14,15),], aes(long, lat, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = Peer_Support.rname[13,], aes(long-.1, 38.6+.2, label = Peer_Support.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = Peer_Support.rname[5,], aes(long, 36.8, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = Peer_Support.rname[12,], aes(long, lat-.1, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = Peer_Support.rname[8,], aes(long, lat+.1, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = Peer_Support.rname[9,], aes(long, lat-.1, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = Peer_Support.rname[2,], aes(long, lat+.1, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = Peer_Support.rname[11,], aes(long, lat+.1, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = Peer_Support.rname[6,], aes(long+.3, lat, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = Peer_Support.rname[3,], aes(long-.1, lat, label = Peer_Support.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(10, 70), name='% PS') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('E: Peer Support (%) in Different ADDs (2022)') + 
  labs(color='map/ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()


#tcm plot 
#tiff(filename='map/TCM-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p6 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = TCM.props, color=region)) +
  geom_text(data = tcm.rname[c(1,4,7,10,14,15),], aes(long, lat, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = tcm.rname[13,], aes(long-.1, 38.6+.2, label = TCM.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = tcm.rname[5,], aes(long, 36.8, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = tcm.rname[12,], aes(long, lat-.1, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = tcm.rname[8,], aes(long, lat+.1, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = tcm.rname[9,], aes(long, lat-.1, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = tcm.rname[2,], aes(long, lat+.1, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = tcm.rname[11,], aes(long, lat+.1, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = tcm.rname[6,], aes(long+.3, lat, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = tcm.rname[3,], aes(long-.1, lat, label = TCM.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(10, 60), name='% TCM') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('F: TCM (%) in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()


# Cost
#tiff(filename='map/Cost-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p7 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = Cost.props/1000, color=region)) +
  geom_text(data = Cost.rname[c(1,4,7,10,14,15),], aes(long, lat, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = Cost.rname[13,], aes(long-.1, 38.6+.2, label = Cost.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = Cost.rname[5,], aes(long, 36.8, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = Cost.rname[12,], aes(long, lat-.1, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = Cost.rname[8,], aes(long, lat+.1, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = Cost.rname[9,], aes(long, lat-.1, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = Cost.rname[2,], aes(long, lat+.1, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = Cost.rname[11,], aes(long, lat+.1, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = Cost.rname[6,], aes(long+.3, lat, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = Cost.rname[3,], aes(long-.1, lat, label = Cost.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(6, 15), name='Median\nCost \n($1000)') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('G: Median Cost in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()

# HOSPITAL
#tiff(filename='map/Hospitalization-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p8 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = HOSPITAL.props, color=region)) +
  geom_text(data = HOSPITAL.rname[c(1,4,7,10,14,15),], aes(long, lat, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = HOSPITAL.rname[13,], aes(long-.1, 38.6+.2, label = HOSPITAL.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = HOSPITAL.rname[5,], aes(long, 36.8, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = HOSPITAL.rname[12,], aes(long, lat-.1, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = HOSPITAL.rname[8,], aes(long, lat+.1, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = HOSPITAL.rname[9,], aes(long, lat-.1, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = HOSPITAL.rname[2,], aes(long, lat+.1, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = HOSPITAL.rname[11,], aes(long, lat+.1, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = HOSPITAL.rname[6,], aes(long+.3, lat, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = HOSPITAL.rname[3,], aes(long-.1, lat, label = HOSPITAL.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(20, 60), name='% Hosp') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('H: Hospitalization (%) in Different ADDs (2022)') + 
  labs(color='map/ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()


# ER
#tiff(filename='map/ER-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p9 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = ER_visit.props, color=region)) +
  geom_text(data = ER_visit.rname[c(1,4,7,10,14,15),], aes(long, lat, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = ER_visit.rname[13,], aes(long-.1, 38.6+.2, label = ER_visit.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = ER_visit.rname[5,], aes(long, 36.8, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = ER_visit.rname[12,], aes(long, lat-.1, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = ER_visit.rname[8,], aes(long, lat+.1, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = ER_visit.rname[9,], aes(long, lat-.1, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = ER_visit.rname[2,], aes(long, lat+.1, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = ER_visit.rname[11,], aes(long, lat+.1, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = ER_visit.rname[6,], aes(long+.3, lat, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = ER_visit.rname[3,], aes(long-.1, lat, label = ER_visit.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(55, 80), name='% ER \nvisit') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('I: ER visit (%) in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()

# self_harm
#tiff(filename='map/SH-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p10 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = selfharm.yn.props, color=region)) +
  geom_text(data = selfharm.yn.rname[c(1,4,7,10,14,15),], aes(long, lat, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = selfharm.yn.rname[13,], aes(long-.1, 38.6+.2, label = selfharm.yn.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = selfharm.yn.rname[5,], aes(long, 36.8, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = selfharm.yn.rname[12,], aes(long, lat-.1, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = selfharm.yn.rname[8,], aes(long, lat+.1, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = selfharm.yn.rname[9,], aes(long, lat-.1, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = selfharm.yn.rname[2,], aes(long, lat+.1, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = selfharm.yn.rname[11,], aes(long, lat+.1, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = selfharm.yn.rname[6,], aes(long+.3, lat, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = selfharm.yn.rname[3,], aes(long-.1, lat, label = selfharm.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(0, 5), name='% Self\nHarm') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('J: Self Harm (%) in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()

# suicideideation
#tiff(filename='map/SI-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p11 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = suicidalideations.yn.props, color=region)) +
  geom_text(data = suicidalideations.yn.rname[c(1,4,7,10,14,15),], aes(long, lat, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = suicidalideations.yn.rname[13,], aes(long-.1, 38.6+.2, label = suicidalideations.yn.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = suicidalideations.yn.rname[5,], aes(long, 36.8, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = suicidalideations.yn.rname[12,], aes(long, lat-.1, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = suicidalideations.yn.rname[8,], aes(long, lat+.1, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = suicidalideations.yn.rname[9,], aes(long, lat-.1, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = suicidalideations.yn.rname[2,], aes(long, lat+.1, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = suicidalideations.yn.rname[11,], aes(long, lat+.1, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = suicidalideations.yn.rname[6,], aes(long+.3, lat, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = suicidalideations.yn.rname[3,], aes(long-.1, lat, label = suicidalideations.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(5, 20), name='% Suicidal\nIdeation') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('K: Suicidal Ideation (%) in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
  )
#dev.off()

# suicideattempt
#tiff(filename='map/SA-map_ADD2022_AUDonly.tiff', width = 5000 , height = 3000, res=300)
p12 <- ken_base + 
  geom_polygon(data = ken.d, aes(fill = suicideattempt.yn.props, color=region)) +
  geom_text(data = suicideattempt.yn.rname[c(1,4,7,10,14,15),], aes(long, lat, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) +
  geom_text(data = suicideattempt.yn.rname[13,], aes(long-.1, 38.6+.2, label = suicideattempt.yn.label, group=NULL), size = 8, color = "black", fontface = 2) + #Northern kentucky
  geom_text(data = suicideattempt.yn.rname[5,], aes(long, 36.8, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #cumberland valley
  geom_text(data = suicideattempt.yn.rname[12,], aes(long, lat-.1, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #lincoln trail
  geom_text(data = suicideattempt.yn.rname[8,], aes(long, lat+.1, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #green river
  geom_text(data = suicideattempt.yn.rname[9,], aes(long, lat-.1, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #kentucky river
  geom_text(data = suicideattempt.yn.rname[2,], aes(long, lat+.1, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #big sandy
  geom_text(data = suicideattempt.yn.rname[11,], aes(long, lat+.1, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #lake cumberland
  geom_text(data = suicideattempt.yn.rname[6,], aes(long+.3, lat, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #fivco
  geom_text(data = suicideattempt.yn.rname[3,], aes(long-.1, lat, label = suicideattempt.yn.label, group = NULL), size = 8, color = "black", fontface = 2) + #bluegrass
  scale_fill_viridis_c(option = "B", direction = -1, limits = c(0, 5), name='% Suicide \nAttempt') + 
  coord_fixed(1.5) +
  theme_classic() + 
  xlab('Longitude (Degrees)') +
  ylab('Latitude (Degrees)') + 
  ggtitle('L: Suicide Attempt (%) in Different ADDs (2022)') + 
  labs(color='ADD Region') + guides(color='none') + 
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30), title=element_text(size=35),
        legend.text=element_text(size=20, vjust=1),legend.key.size = unit(1.5, 'cm')
        )
#dev.off()

tiff(filename='map/fig1.tiff', width = 15000 , height = 12000, res=300)
(p1 + p2 + p3)/(p4 + p5 + p6)/(p7 + p8 + p9)/(p10 + p11 + p12)
dev.off()


