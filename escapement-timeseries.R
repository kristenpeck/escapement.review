
#This script is to support review and summary of BC16 data
#Part of it is the functions defined in auc_function4.R
#Another part of it was designed for figures used in PSR 2024
#


#libraries

library(readxl)
library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)

#### Babine Watershed escapement data review ####

#create table of Babine streams and SIL/SEN presence
# These three spreadsheets are unaltered exports from Stream Esc_DB_MasterDataset.accdb
#found in S:\Stock Assessment\ESCAPEMENT\Databases on Dec 18th

SILs.raw <- read_excel("MasterStreamInspection2004-2022.xlsx",
                       .name_repair = "universal")

SENs.raw <- read_excel("MastertblSEN2004-2022.xlsx",
                       .name_repair = "universal")

streams <- read_excel("Streams.xlsx")


babine.streams <- streams %>% 
  mutate(wscd.short = substr(WATERSHED_Code,1,3)) %>% 
  filter(wscd.short %in% "480") %>% # select those streams and lakes within the Babine watershed
  select(StreamId, StreamName,WATERSHED_Code, Active)

years.filter <- c(2010:2022) #those years after the last large review

SILs.babine.SK <- babine.streams %>% 
  left_join(SILs.raw, by=c("StreamId"="StreamID"), ) %>% 
  filter(Inspection.Year %in% years.filter,
         TargetSockeye %in% T)

#write.csv(SILs.babine.SK,"SILs.babine.SK.csv")

#make table of SILs by stream and year
sil.summary.by.yr <- SILs.babine.SK %>% 
  group_by(Inspection.Year, StreamName) %>% 
  summarise(sil = length(unique(StreamName))) %>% 
  mutate(silx = ifelse(sil %in% 1,"X",NA)) %>% 
  select(-sil) %>% 
  arrange(StreamName) %>% 
  pivot_wider(names_from = Inspection.Year,
              values_from = silx)

#write.csv(sil.summary.by.yr,"sil.summary.by.yr.csv")



SENs.babine.SK <- babine.streams %>% 
  left_join(SENs.raw, by=c("StreamId"="StreamID")) %>% 
  filter(Year %in% years.filter,
         SockAnnualEst != "N/I") #exclude only those with "N/I"

#make table of SENs by stream and year
sen.summary.by.yr <- SENs.babine.SK %>% 
  group_by(Year, StreamName) %>% 
  summarise(sen = length(unique(StreamName))) %>% 
  mutate(senx = ifelse(sen %in% 1,"X",NA)) %>% 
  select(-sen) %>% 
  arrange(StreamName) %>%
  pivot_wider(names_from = Year,
              values_from = senx)

#write.csv(sen.summary.by.yr,"sen.summary.by.yr.csv")


#Make template of AUC_residencetime.xlsx with Babine SENs:

AUC.restime.bab <- SENs.babine.SK %>% 
  select(StreamName,Year,SockNoStreamInsp,
         SockEstMeth.num = SockEstMeth,
         SockEstClassification.num=SockEstClassification,
         SockAnnualEst, AnnualEstRationale, UnusualFishCond,
         UnusualStreamCondit, GenComments,SockEntStreamEst) %>% 
  arrange(StreamName,Year) %>% 
  mutate(SockEstMeth = recode(SockEstMeth.num, `1` = "peak.liveplusdead",
                              `2` = "peak.livepluscumuldead",
                              `3` = "AUC", `4` = "fixed.site.census",
                              `5` = "expert.opinion",`6` = "redd.count",
                              `8`= "mark.recap", `12`= "other")) %>% 
  mutate(SockEstClassification = recode(SockEstClassification.num, `1` = "census",
                            `2` = "MR.or.breachedfence",`3` = "hi.res.stream.survey", 
                            `4` = "med.res.stream.survey",
                            `5` = "low.res.stream.survey",`6` = "AP.NO.NI.DNS")) %>% 
  mutate(QA.SockNoStreamInsp = NA, residence.time1 = NA, res.time.source1 = NA, 
         residence.time2 = NA, res.time.source2 = NA,
         holders.or.spawners = NA, QA.comments = NA,
         QA.SockEstMeth.num = NA,
         QA.SockEstClassification.num = NA,
         QA.SockAnnualEst = NA) %>% 
  select(StreamName,Year,SockNoStreamInsp,QA.SockNoStreamInsp,SockEstMeth.num,
         SockEstMeth,QA.SockEstMeth.num,
         SockEstClassification.num, SockEstClassification,QA.SockEstClassification.num,
         SockAnnualEst,QA.SockAnnualEst,residence.time1,res.time.source1,
         residence.time2,res.time.source2, holders.or.spawners, QA.comments,
         AnnualEstRationale, UnusualFishCond,
         UnusualStreamCondit, GenComments,SockEntStreamEst)

write_csv(AUC.restime.bab, "AUC.restime.bab.csv", na = "")



####  spatial points files of stream segments ####

#UTM coords:
SILs.babine.SK.mappingUTMs <- SILs.babine.SK %>% 
  select(StreamId, StreamName, Inspection.Year, Affiliation,
         StartBoundary, StopBoundary, StartUtmN,StartUtmE,                           
         StartUtmZone, StopUtmN, StopUtmE, StopUtmZone)

SILs.babine.SK.zone9start <- SILs.babine.SK.mappingUTMs %>% 
  filter(!is.na(StartUtmN), StartUtmZone %in% 9) %>% 
  select(-c(StopUtmN, StopUtmE, StopUtmZone, StartUtmZone))

SILs.babine.SK.zone9stop <- SILs.babine.SK.mappingUTMs %>% 
  filter(!is.na(StopUtmN), StartUtmZone %in% 9) %>% 
  select(-c(StartUtmN,StartUtmE,StartUtmZone, StopUtmZone))

SILs.babine.SK.zone10start <- SILs.babine.SK.mappingUTMs %>% 
  filter(!is.na(StartUtmN), StartUtmZone %in% 10) %>% 
  select(-c(StopUtmN, StopUtmE, StopUtmZone, StartUtmZone))

SILs.babine.SK.zone10stop <- SILs.babine.SK.mappingUTMs %>% 
  filter(!is.na(StopUtmN), StartUtmZone %in% 10) %>% 
  select(-c(StartUtmN,StartUtmE,StartUtmZone, StopUtmZone))

start.zone9 <- st_as_sf(SILs.babine.SK.zone9start, coords = c("StartUtmN","StartUtmE"), crs = 26909)
stop.zone9 <- st_as_sf(SILs.babine.SK.zone9stop, coords = c("StopUtmN","StopUtmE"), crs = 26909)

start.zone10 <- st_as_sf(SILs.babine.SK.zone10start, coords = c("StartUtmN","StartUtmE"), crs = 26910)
stop.zone10 <- st_as_sf(SILs.babine.SK.zone10stop,coords = c("StopUtmN","StopUtmE"), crs = 26910)

zone9 <- rbind(start.zone9,stop.zone9 ) %>% 
  st_transform(crs = 4326) #convert to decimal degrees
zone10 <- rbind(start.zone10,stop.zone10 )%>% 
  st_transform(crs = 4326) #convert to decimal degrees

all.zones <- rbind(zone9, zone10)

st_write(all.zones, dsn = "start.stopUTMs.kml", driver = "kml") #export to google earth kml


#ddmmss files:

SILs.babine.SK.mapping.ddmmss <- SILs.babine.SK %>% 
  select(StreamId, StreamName, Inspection.Year, Affiliation,
         StartBoundary, StopBoundary, StartLatDeg,StartLatMin,                         
         StartLatSec,StartLongDeg,StartLongMin,StartLongSec,                        
         StopLatDeg,StopLatMin,StopLatSec,StopLongDeg,                         
         StopLongMin,StopLongSec) %>% 
  mutate(StartLatMin = ifelse(is.na(StartLatMin),0,StartLatMin),
         StartLatSec= ifelse(is.na(StartLatSec),0,StartLatSec),
         StartLongMin = ifelse(is.na(StartLongMin),0,StartLongMin),
         StartLongSec= ifelse(is.na(StartLongSec),0,StartLongSec),
         StopLatMin = ifelse(is.na(StopLatMin),0,StopLatMin),
         StopLatSec= ifelse(is.na(StopLatSec),0,StopLatSec),
         StopLongMin = ifelse(is.na(StopLongMin),0,StopLongMin),
         StopLongSec= ifelse(is.na(StopLongSec),0,StopLongSec))

SILs.babine.SK.ddmmssstart <- SILs.babine.SK.mapping.ddmmss %>% 
  filter(!is.na(StartLatDeg)) %>% 
  mutate(lat = StartLatDeg+StartLatMin/60+StartLatSec/3600,
         long = -1*(StartLongDeg+StartLongMin/60+StartLongSec/3600)) %>% 
  select(-c(StopLatDeg,StopLatMin,StopLatSec,StopLongDeg,                         
            StopLongMin,StopLongSec,StartLatDeg,StartLatMin,
            StartLatSec,StartLongDeg,StartLongMin,StartLongSec)) %>% 
  filter(!is.na(lat), !is.na(long))

SILs.babine.SK.ddmmssstop <- SILs.babine.SK.mapping.ddmmss %>% 
  filter(!is.na(StopLatDeg)) %>% 
  mutate(lat = StopLatDeg+StopLatMin/60+StopLatSec/3600,
         long = -1*(StopLongDeg+StopLongMin/60+StopLongSec/3600)) %>% 
  select(-c(StartLatDeg,StartLatMin,StartLatSec,StartLongDeg,
            StartLongMin,StartLongSec, StopLatDeg, StopLatMin,
            StopLatSec,StopLongDeg,StopLongMin,StopLongSec)) %>% 
  filter(!is.na(lat), !is.na(long))

start.dd <- st_as_sf(SILs.babine.SK.ddmmssstart, coords = c("long","lat"), crs = 4326)
stop.dd <- st_as_sf(SILs.babine.SK.ddmmssstop, coords = c("long","lat"), crs = 4326)

all.dd <- rbind(start.dd, stop.dd)

st_write(all.dd, dsn = "start.stopDDs.kml", driver = "kml") #export to google earth kml





# Look at SENs and compare to current year 2022


# SENSraw9819 <- read_excel("tblSEN_1998-2019.xlsx") %>% 
#   left_join(streams, by=c("StreamID"="StreamId")) %>% 
#   select(StreamName,Year,SockAnnualEst,SockEstClassification,SockEstMeth,
#          SockPeakSpawnMonth,SockPeakSpawnDay,
#          CohoAnnualEst,CohoEstClassification,CohoEstMeth,
#          CohoPeakSpawnMonth,CohoPeakSpawnDay,
#          PinkAnnualEst,PinkEstClassification,PinkEstMeth,
#          PinkPeakSpawnMonth,PinkPeakSpawnDay,
#          ChinAnnualEst,ChinEstClassification,ChinEstMeth,
#          ChinPeakSpawnMonth,ChinPeakSpawnDay,
#          AnnualEstRationale,UnusualFishCond,
#          UnusualStreamCondit,GenComments,
#          Affiliation,CreatedBy,CreatedByDate) %>% 
#   filter(Year>=2005)
# 
# SENSraw2122 <- read_excel("tblSEN_2022.xlsx") %>% 
#   rbind(read_excel("tblSEN_2021.xlsx")) %>% 
#   left_join(streams, by=c("StreamID"="StreamId")) %>% 
#   select(StreamName,Year,SockAnnualEst,SockEstClassification,SockEstMeth,
#          SockPeakSpawnMonth,SockPeakSpawnDay,
#          CohoAnnualEst,CohoEstClassification,CohoEstMeth,
#          CohoPeakSpawnMonth,CohoPeakSpawnDay,
#          PinkAnnualEst,PinkEstClassification,PinkEstMeth,
#          PinkPeakSpawnMonth,PinkPeakSpawnDay,
#          ChinAnnualEst,ChinEstClassification,ChinEstMeth,
#          ChinPeakSpawnMonth,ChinPeakSpawnDay,
#          AnnualEstRationale,UnusualFishCond,
#          UnusualStreamCondit,GenComments,
#          Affiliation,CreatedBy,CreatedByDate)
# 
# 
# SENS.SK <- SENSraw9819 %>% 
#   rbind(SENSraw2122) %>% 
#   filter(StreamName %in% unique(SK.AUC.estimates$stream)) %>% 
#   mutate(fYear = as.factor(Year)) %>% 
#   select(StreamName, Year,fYear,SockAnnualEst) %>% 
#   rbind(SK.AUC.estimates %>% 
#           mutate(Year = 2024, fYear = as.factor(Year),SockAnnualEst = as.character(auc.est)) %>% 
#           select(StreamName = stream, Year,fYear, SockAnnualEst)) %>% 
#   left_join(groups, by="StreamName") %>% 
#   group_by(group.name)
# 
# 
# ggplot(SENS.SK)+
#   geom_point(aes(x=Year,y=as.numeric(SockAnnualEst), col=group.name))+
#   geom_line(aes(x=Year,y=as.numeric(SockAnnualEst), col=group.name))+
#   #geom_smooth(aes(x=Year,y=as.numeric(SockAnnualEst), col=StreamName, group=StreamName),
#   #              method="loess", linewidth=0.75,se = F)+
#   geom_smooth(aes(x=Year,y=as.numeric(SockAnnualEst), col=group.name),method = "lm",se=T)+
#   #scale_y_continuous(limits = c(0,max(as.numeric(SENS.SK$SockAnnualEst),na.rm=T)))+
#   scale_x_continuous(breaks = seq(min(SENS.SK$Year),max(SENS.SK$Year),2))+
#   facet_wrap(~StreamName,scales = "free_y")+
#   labs(y="Annual Sockeye Estimate")+
#   theme(axis.text.x = element_text(hjust=1,angle=45),
#         legend.position = "none")
# 
# 
# SENS.CH <- SENSraw9819 %>% 
#   rbind(SENSraw2122) %>% 
#   filter(StreamName %in% unique(CH.AUC.estimates$stream)) %>% 
#   mutate(fYear = as.factor(Year)) %>% 
#   select(StreamName, Year,fYear,ChinAnnualEst) %>% 
#   rbind(CH.AUC.estimates %>% 
#           mutate(Year = 2024, fYear = as.factor(Year),ChinAnnualEst = as.character(auc.est)) %>% 
#           select(StreamName = stream, Year,fYear, ChinAnnualEst)) %>% 
#   left_join(groups, by="StreamName") %>% 
#   group_by(group.name)
# 
# 
# ggplot(SENS.CH)+
#   geom_point(aes(x=Year,y=as.numeric(ChinAnnualEst), col=group.name))+
#   geom_line(aes(x=Year,y=as.numeric(ChinAnnualEst), col=group.name))+
#   #geom_smooth(aes(x=Year,y=as.numeric(ChinAnnualEst), col=StreamName, group=StreamName),
#   #              method="loess", linewidth=0.75,se = F)+
#   geom_smooth(aes(x=Year,y=as.numeric(ChinAnnualEst), col=group.name),method = "lm",se=T)+
#   #scale_y_continuous(limits = c(0,max(as.numeric(SENS.SK$ChinAnnualEst),na.rm=T)))+
#   scale_x_continuous(breaks = seq(min(SENS.CH$Year),max(SENS.CH$Year),2))+
#   facet_wrap(~StreamName)+ #,scales = "free_y"
#   labs(y="Annual Chinook Estimate")+
#   theme(axis.text.x = element_text(hjust=1,angle=45),
#         legend.position = "none")
# 
# 
# 
# SENS.CO <- SENSraw9819 %>% 
#   rbind(SENSraw2122) %>% 
#   filter(StreamName %in% unique(CO.AUC.estimates$stream)) %>% 
#   mutate(fYear = as.factor(Year)) %>% 
#   select(StreamName, Year,fYear,CohoAnnualEst) %>% 
#   rbind(CO.AUC.estimates %>% 
#           mutate(Year = 2024, fYear = as.factor(Year),CohoAnnualEst = as.character(auc.est)) %>% 
#           select(StreamName = stream, Year,fYear, CohoAnnualEst)) %>% 
#   left_join(groups, by="StreamName") %>% 
#   group_by(group.name)
# 
# 
# ggplot(SENS.CO)+
#   geom_point(aes(x=Year,y=as.numeric(CohoAnnualEst), col=group.name))+
#   geom_line(aes(x=Year,y=as.numeric(CohoAnnualEst), col=group.name))+
#   #geom_smooth(aes(x=Year,y=as.numeric(CohoAnnualEst), col=StreamName, group=StreamName),
#   #              method="loess", linewidth=0.75,se = F)+
#   geom_smooth(aes(x=Year,y=as.numeric(CohoAnnualEst), col=group.name),method = "lm",se=T)+
#   #scale_y_continuous(limits = c(0,max(as.numeric(SENS.SK$CohoAnnualEst),na.rm=T)))+
#   scale_x_continuous(breaks = seq(min(SENS.CO$Year),max(SENS.CO$Year),2))+
#   facet_wrap(~StreamName,scales = "free_y")+
#   labs(y="Annual Coho Estimate")+
#   theme(axis.text.x = element_text(hjust=1,angle=45),
#         legend.position = "none")


# 
# #PSR 2024:
# 
# # Load most recent NUSEDs data:
# 
# nuseds <- read_excel("North and Central Coast NuSEDS_20241004.xlsx")
# streams <- read_excel("Streams.xlsx")
# 
# kispiox<- data.frame(StreamName = c("BARNES CREEK","CLUB CREEK (LOWER)", "CLUB CREEK - UPPER",
#     "CLIFFORD CREEK","SWAN LAKE CREEK #2 UNNAMED","JACKSON CREEK",
#     "FALLS CREEK"), StatArea = "4")
# 
# kispiox.streams <- kispiox %>% 
#   left_join(streams)
# 
# #the nused stream names:
# kispiox2 <- data.frame(StreamName = c("CLUB CREEK - LOWER (BETWEEN CLUB LAKE AND STEPHENS LAKE)",
# "CLUB CREEK - UPPER","FALLS CREEK","JACKSON CREEK","SWAN LAKE CREEK #2 UNNAMED","BARNES CREEK"))
# 
# nuseds.kisp <- nuseds %>% 
#   filter(WATERBODY %in% kispiox2$StreamName) %>% 
#   filter(ANALYSIS_YR %in% c(2004:2023), SPECIES %in% "Sockeye") %>% 
#   select(year = ANALYSIS_YR, species = SPECIES, stream = WATERBODY,
#          spawners = NATURAL_ADULT_SPAWNERS, 
#          method = ESTIMATE_CLASSIFICATION,
#          inspections = NO_INSPECTIONS_USED,
#          method_type = ESTIMATE_METHOD)
# #write_csv(nuseds.kisp, "nuseds.kisp.csv")
# nuseds.kisp <- read_csv("nuseds.kisp.csv") 
# 
# nused.kisp.nonweir <- nuseds.kisp %>% 
#   filter(method_type != "Weir") %>% 
#   arrange(order(year))
# nused.kisp.weir <- nuseds.kisp %>% 
#   filter(method_type %in% "Weir") 
# 
# 
# plot.kispiox.streams <- ggplot()+
#   geom_bar(data=nused.kisp.nonweir, stat="identity", aes(x=year, y=spawners, fill=stream))+
#   geom_point(data=nused.kisp.weir,shape=20, size=4,
#              aes(x=year, y=spawners),  fill="black")+
#   theme_bw()+
#   scale_x_continuous(breaks = seq(2004, 2024, 2))+
#   scale_y_continuous(breaks = seq(0,20000,2000))+
#   theme(legend.position = "right")+
#   labs(fill="")
# plot.kispiox.streams
# 
# ggsave(plot=plot.kispiox.streams, "plot.kispiox.streams.png", width=8, height=4)
# 
# 
# 
# #Bear River 
# 
# #the nused stream names:
# bear <- data.frame(StreamName = c("AZUKLOTZ CREEK","BEAR LAKE",
#                                   "BEAR RIVER","SALIX CREEK"))
# # x <- grep("SALIX",nuseds$WATERBODY)
# # nuseds$WATERBODY[x]
# 
# 
# nuseds.bear <- nuseds %>% 
#   filter(WATERBODY %in% bear$StreamName) %>% 
#   filter(ANALYSIS_YR %in% c(2004:2023), SPECIES %in% "Sockeye") %>% 
#   select(year = ANALYSIS_YR, species = SPECIES, stream = WATERBODY,
#          spawners = NATURAL_ADULT_SPAWNERS, 
#          method = ESTIMATE_CLASSIFICATION,
#          inspections = NO_INSPECTIONS_USED,
#          method_type = ESTIMATE_METHOD)
# #write_csv(nuseds.bear, "nuseds.bear.csv")
# 
# nuseds.bear <- read_csv("nuseds.bear.csv")
# 
# nused.bear.nonweir <- nuseds.bear %>% 
#   filter(method_type != "Weir") 
# 
# range(nused.bear.nonweir$year)
# 
# nused.bear.weir <- nuseds.bear %>% 
#   filter(method_type %in% "Weir") 
# 
# 
# 
# plot.bear.streams <- ggplot()+
#   geom_bar(data = nused.bear.nonweir, aes(x=year, y=spawners, fill=stream), stat="identity")+
#   geom_point(data=nused.bear.weir,
#              aes(x=year, y=spawners), shape=20, fill="black",
#              size=3)+
#   geom_line(data=nused.bear.weir,
#              aes(x=year, y=spawners))+
#   theme_bw()+
#   scale_x_continuous(breaks = seq(2004, 2024, 2))+
#   scale_y_continuous(breaks = seq(0,16000,2000))+
#   theme(legend.position = "right")+
#   labs(fill="")
# 
# plot.bear.streams
# 
# ggsave(plot=plot.bear.streams, filename="plot.bear.streams.png", width=8, height=4)
# 
#   
#   

