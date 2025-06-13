
#This script is to support review and summary of visual stream estimate data
# in the north coast

#Analysis parts of it are the functions defined in auc_function4.R
#Another part of it was designed for figures used in PSR 2024
#


#libraries

library(readxl)
library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)
library(gridExtra)

# Load the most recent database data ####
# These three spreadsheets are unaltered exports from Stream Esc_DB_MasterDataset.accdb
#found in S:\Stock Assessment\ESCAPEMENT\Databases on Dec 18th

#ggplot theme
theme_babine4 <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text.x = element_text(size = rel(0.80), face = "bold", 
                                 angle=60, hjust = 1.2, vjust=1.2),
      axis.text.y = element_text(size = rel(0.80), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.75), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

# Fetch old versions (see convert_old_dbs.R)

SILs.raw12 <- tibble(read_csv("./old_dbs/StreamInspection2012.csv"), .name_repair = "universal")
SENs.raw12 <- tibble(read_csv("./old_dbs/tblSEN2012.csv"), .name_repair = "universal")

SILs.raw13 <- tibble(read_csv("./old_dbs/StreamInspection2013.csv"), .name_repair = "universal")
SENs.raw13 <- tibble(read_csv("./old_dbs/tblSEN2013.csv"), .name_repair = "universal")


# Fetch Kristen's copy of NCSTAD data for 2023 :
SILs.raw23NCSTAD <- read_excel("MasterStreamInspection2023.xlsx",
                         .name_repair = "universal")

SENs.raw23NCSTAD <- read_excel("MastertblSEN2023.xlsx",
                         .name_repair = "universal")

# Fetch DFO's copy of LBN data for 2023:
SILs.raw23LBN <- read_excel("MasterStreamInspection2023_LBN.xlsx",
                               .name_repair = "universal")

SENs.raw23LBN <- read_excel("MastertblSEN2023_LBN.xlsx",
                               .name_repair = "universal")


# Fetch DFO's copy of 2024 data:
SILs.raw24 <- read_excel("MasterStreamInspection2024.xlsx",
                               .name_repair = "universal")

SENs.raw24 <- read_excel("MastertblSEN2024.xlsx",
                               .name_repair = "universal")

# Fetch compiled database from stock drive:
SILs.raw <- read_excel("MasterStreamInspection2004-2022.xlsx",
                       .name_repair = "universal") %>% 
  rbind(SILs.raw12,SILs.raw13,SILs.raw23NCSTAD,SILs.raw23LBN,SILs.raw24)

SENs.raw <- read_excel("MastertblSEN2004-2022.xlsx",
                       .name_repair = "universal") %>% 
  rbind(SENs.raw12,SENs.raw13,SENs.raw23NCSTAD,SENs.raw23LBN,SENs.raw24)

streams <- read_excel("Streams.xlsx")




# # # # # # # # # # # # # # # # # # # # # # # # # 


#### Watershed review selection - CHANGE THESE TO MATCH YOUR REVIEW ####

review.subject <- "babineSK2010-24" #text to add to file names

# filter for years of interest:
years.filter <- c(2010:2024) #those years after the last large babine review


# filter for streams of interest using grouping identifiers

#babine watershed
review.streams <- streams %>%
  mutate(wscd.short = substr(WATERSHED_Code,1,3)) %>%
  filter(wscd.short %in% "480") %>% # select those streams and lakes within the Babine watershed (480)
  select(StreamId, StreamName,WATERSHED_Code, Active)

exp.streams <- review.streams %>%
  mutate(QA.SegmentStartLatDD = "",QA.SegmentStartLongDD="",
         QA.SegmentStopLatDD = "", QA.SegmentStopLongDD="",
         QA.SegmentComments = "")

#write_csv(exp.streams, paste0("Streams.toreview-",review.subject,".csv"))
#after happy with this export, re-save as excel and colour new columns to be filled in

# Nanika SK and CH

# review.subject <- "nanikaSKCH2012-2013"
# 
# # filter for years of interest:
# years.filter <- c(2012:2013)
# unique(years.filter)
# 
# # filter for streams of interest using grouping identifiers
# 
# str(streams)


# #nanika river only
#  review.streams <- streams %>%
#    filter(StreamId %in% c(428)) %>%
#    select(StreamId, StreamName,WATERSHED_Code, Active)
# 
# 
# exp.streams <- review.streams %>%
#   mutate(QA.SegmentStartLatDD = "",QA.SegmentStartLongDD="",
#          QA.SegmentStopLatDD = "", QA.SegmentStopLongDD="",
#          QA.SegmentComments = "")

#write_csv(exp.streams, paste0("Streams.toreview-",review.subject,".csv"))


# # Morice and Nanika version:

# review.subject <- "morice2023-24"
# 
# # filter for years of interest:
# years.filter <- c(2023:2024) #all available recent yrs
# unique(years.filter)

# # # filter for streams of interest using grouping identifiers
# # 
# # #wedzinkwa watershed, 460
# # review.streams <- streams %>%
# #   mutate(wscd.short = substr(WATERSHED_Code,1,3)) %>%
# #   filter(wscd.short %in% "460") %>% # select those streams and lakes within the Morice watershed (460)
# #   select(StreamId, StreamName,WATERSHED_Code, Active)
# # 
# review.streams <- streams %>%
#   filter(StreamId %in% c(427, 428)) %>% # select those streams and lakes within the Morice watershed (460)
#   select(StreamId, StreamName,WATERSHED_Code, Active)

# exp.streams <- review.streams %>%
#   mutate(QA.SegmentStartLatDD = "",QA.SegmentStartLongDD="",
#          QA.SegmentStopLatDD = "", QA.SegmentStopLongDD="",
#          QA.SegmentComments = "")
# 
# #write_csv(exp.streams, paste0("Streams.toreview-",review.subject,".csv"))
# #after happy with this export, re-save as excel and colour new columns to be filled in
# 
# # # # # # # # # # # # # # # # # # # # # 



#### SOCKEYE version: ####

# filter SILs by above selections - *change species if not sockeye*

SILs.to.review <- review.streams %>% 
  left_join(SILs.raw, by=c("StreamId"="StreamID")) %>% 
  filter(Inspection.Year %in% years.filter) %>% 
         #TargetSockeye %in% T) %>%  #only include SILs where sockeye were targeted
  mutate(julian.day = yday(SilDate),obs.eff = as.numeric(""), QA.comments = "", surveyed.full.extent = "",
         proportion.surveyed=as.numeric(""), barriers.present = "",
         UseForPeakCount = NA,
         UseForAUC = NA, 
         SilDate = as_date(SilDate))

#write_csv(SILs.to.review, paste0("SILs.toreview-",review.subject,".csv"), na = "")
#after happy with this export, re-save as excel and colour new columns to be filled in


# filter SENs of interest by above selections - *change species if not looking for sockeye* 

SENs.review <- review.streams %>% 
  left_join(SENs.raw, by=c("StreamId"="StreamID")) %>% 
  filter(Year %in% years.filter,
         SockAnnualEst != "N/I") %>%  #exclude only those with "N/I" (not inspected)
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
         QA.SockAnnualEst = NA,
         QA.SockEntStreamEst = NA,
         Timing.captured = NA) %>% 
  select(StreamName,Year,SockNoStreamInsp,
         QA.SockNoStreamInsp,Timing.captured,SockEstMeth.num,
         SockEstMeth,QA.SockEstMeth.num,
         SockEstClassification.num, SockEstClassification,QA.SockEstClassification.num,
         SockAnnualEst,QA.SockAnnualEst,residence.time1,res.time.source1,
         residence.time2,res.time.source2, holders.or.spawners, QA.comments,
         AnnualEstRationale, UnusualFishCond,
         UnusualStreamCondit, GenComments,SockEntStreamEst,
         QA.SockEntStreamEst)


#write_csv(SENs.review, paste0("AUC.res.time.review-",review.subject,".csv"), na = "")
#after happy with this export, re-save as excel and colour new columns to be filled in




#### CHINOOK version ####

# filter SILs by above selections 
# 
# SILs.to.review <- review.streams %>%
#   left_join(SILs.raw, by=c("StreamId"="StreamID")) %>%
#   filter(Inspection.Year %in% years.filter) %>% 
#          #TargetChin %in% T) %>%  #only include SILs where CH were targeted
#   mutate(julian.day = yday(SilDate),obs.eff = as.numeric(""), QA.comments = "", surveyed.full.extent = "",
#          proportion.surveyed=as.numeric(""), barriers.present = "")
# 
# write_csv(SILs.to.review, paste0("SILs.toreview-",review.subject,".csv"))
# #after happy with this export, re-save as excel and colour new columns to be filled in
# 
# 
# # filter SENs of interest by above selections - *change species if not looking for sockeye* 
# 
# SENs.review <- review.streams %>% 
#   left_join(SENs.raw, by=c("StreamId"="StreamID")) %>% 
#   filter(Year %in% years.filter,
#          ChinAnnualEst != "N/I") %>%  #exclude only those with "N/I" (not inspected)
#   select(StreamName,Year,ChinNoStreamInsp,
#          ChinEstMeth.num = ChinEstMeth,
#          ChinEstClassification.num=ChinEstClassification,
#          ChinAnnualEst, AnnualEstRationale, UnusualFishCond,
#          UnusualStreamCondit, GenComments,ChinEntStreamEst) %>% 
#   arrange(StreamName,Year) %>% 
#   mutate(ChinEstMeth = recode(ChinEstMeth.num, `1` = "peak.liveplusdead",
#                               `2` = "peak.livepluscumuldead",
#                               `3` = "AUC", `4` = "fixed.site.census",
#                               `5` = "expert.opinion",`6` = "redd.count",
#                               `8`= "mark.recap", `12`= "other")) %>% 
#   mutate(ChinEstClassification = recode(ChinEstClassification.num, `1` = "census",
#                                         `2` = "MR.or.breachedfence",`3` = "hi.res.stream.survey", 
#                                         `4` = "med.res.stream.survey",
#                                         `5` = "low.res.stream.survey",`6` = "AP.NO.NI.DNS")) %>% 
#   mutate(QA.ChinNoStreamInsp = NA, residence.time1 = NA, res.time.source1 = NA, 
#          residence.time2 = NA, res.time.source2 = NA,
#          holders.or.spawners = NA, QA.comments = NA,
#          QA.ChinEstMeth.num = NA,
#          QA.ChinEstClassification.num = NA,
#          QA.ChinAnnualEst = NA,
#          QA.ChinEntStreamEst = NA) %>% 
#   select(StreamName,Year,ChinNoStreamInsp,QA.ChinNoStreamInsp,ChinEstMeth.num,
#          ChinEstMeth,QA.ChinEstMeth.num,
#          ChinEstClassification.num, ChinEstClassification,QA.ChinEstClassification.num,
#          ChinAnnualEst,QA.ChinAnnualEst,residence.time1,res.time.source1,
#          residence.time2,res.time.source2, holders.or.spawners, QA.comments,
#          AnnualEstRationale, UnusualFishCond,
#          UnusualStreamCondit, GenComments,ChinEntStreamEst, QA.ChinEntStreamEst)
# 
# 
# #write_csv(SENs.review, paste0("AUC.res.time.review-",review.subject,".csv"), na = "")
# #after happy with this export, re-save as excel and colour new columns to be filled in
# 
 
 

# Reference figures and tables ####


### Presence of SIL/SENs by yr ####
#make overview table of SILs present by stream and year

sil.summary.by.yr <- SILs.to.review %>% 
  #filter(TargetSockeye %in% T) %>% #uncomment if looking for target of SK
  group_by(Inspection.Year, StreamName) %>% 
  summarise(sil = length(unique(StreamName))) %>% 
  mutate(silx = ifelse(sil %in% 1,"X",NA)) %>% 
  select(-sil) %>% 
  arrange(Inspection.Year) %>% 
  pivot_wider(names_from = Inspection.Year,
              values_from = silx) %>% 
  arrange(StreamName) 
  

#write_csv(sil.summary.by.yr,paste0("SIL.presencebyyr-",review.subject,".csv"), na = "") 


#make table of SENs present by stream and year
sen.summary.by.yr <- SENs.review %>% 
  group_by(Year, StreamName) %>% 
  summarise(sen = length(unique(StreamName))) %>% 
  mutate(senx = ifelse(sen %in% 1,"X",NA)) %>% 
  select(-sen) %>% 
  arrange(Year) %>%
  pivot_wider(names_from = Year,
              values_from = senx) %>% 
  arrange(StreamName)

#write_csv(sen.summary.by.yr,paste0("SEN.presencebyyr-",review.subject,".csv"), na="")


#### Timing of peak spawn versus all targeted surveys ####


# stream inspection timing plots, modified from David's script#
# go see David's script if you want to go one-by-one

stream.ids <- unique(SILs.to.review$StreamId)

#remove streams where SK never recorded, even as a 0, though they were targeted:
#these should probably be corrected in database, either to change to 0 seen or remove as target species:

# SOCKEYE:
(bogus.targets <- SILs.to.review %>%
  group_by(StreamId, StreamName) %>%
  summarize(max.Sock_AL_Spawning = max(Sock_AL_Spawning,na.rm=T)) %>%
  filter(is.na(max.Sock_AL_Spawning)|is.infinite(max.Sock_AL_Spawning)))

stream.ids <- setdiff(stream.ids,bogus.targets$StreamId)

# # CHINOOK:
# (bogus.targets <- SILs.to.review %>% 
#     group_by(StreamId, StreamName) %>% 
#     summarize(max.Chinook_AL_Spawning = max(Chinook_AL_Spawning,na.rm=T)) %>% 
#     filter(is.na(max.Chinook_AL_Spawning)|is.infinite(max.Chinook_AL_Spawning)))
# stream.ids <- setdiff(stream.ids,bogus.targets$StreamId)


#note that if you run this loop below, this is going to save individual 
# timing plots for each stream into a timing.plots folder.
# If you don't want to do this, then comment out and skip.

for(i in stream.ids){
  tmp.spawn <- SILs.to.review %>% 
    filter(StreamId %in% i, !is.na(Sock_AL_Spawning)) %>% 
    mutate(julian = yday(ymd(SilDate)), 
           appr.date = as_date(julian,origin="2025-01-01")) %>% 
    group_by(Inspection.Year, StreamId, StreamName) %>% 
    summarize(start = min(julian), end = max(julian))
  
  peak.spawn <- SILs.to.review %>% 
    filter(StreamId %in% i) %>% 
    mutate(julian = yday(ymd(SilDate)),
           appr.date = as_date(julian,origin="2025-01-01")) %>% 
    group_by(Inspection.Year, StreamId, StreamName) %>% 
    summarize(peak.spawner = max(Sock_AL_Spawning, na.rm=T),
              peak.day = ifelse(Sock_AL_Spawning == peak.spawner, julian, NA)) %>% 
    filter(!is.na(peak.day)) #ugly code, just ignore warning for now
  
  plot.peak.spawn <- ggplot() +
    geom_ribbon(data = tmp.spawn, 
                aes(x=Inspection.Year, ymin = start,ymax=end), fill="grey75")+
    geom_point(data = peak.spawn, aes(x=Inspection.Year,y=peak.day))+
    geom_line(data = peak.spawn, aes(x=Inspection.Year,y=peak.day))+
    scale_x_continuous(breaks = seq(min(peak.spawn$Inspection.Year),
                                    max(peak.spawn$Inspection.Year),1))+
    labs(title = tmp.spawn$StreamName)
  plot.peak.spawn
  
  ggsave(plot = plot.peak.spawn, path = "./timing.plots/",
         filename = paste0(unique(tmp.spawn$StreamName),"-timing.png"),
         width = 7, height=4)
}




####  Spatial points files of start and end of stream surveys ####

#UTM coords:
SILs.review.mappingUTMs <- SILs.to.review %>% 
  select(StreamId, StreamName, Inspection.Year, Affiliation,
         StartBoundary, StopBoundary, StartUtmN,StartUtmE,                           
         StartUtmZone, StopUtmN, StopUtmE, StopUtmZone)

SILs.zone9start <- SILs.review.mappingUTMs %>% 
  filter(!is.na(StartUtmN), StartUtmZone %in% 9) %>% 
  select(-c(StopUtmN, StopUtmE, StopUtmZone, StartUtmZone))

SILs.zone9stop <- SILs.review.mappingUTMs %>% 
  filter(!is.na(StopUtmN), StartUtmZone %in% 9) %>% 
  select(-c(StartUtmN,StartUtmE,StartUtmZone, StopUtmZone))

SILs.zone10start <- SILs.review.mappingUTMs %>% 
  filter(!is.na(StartUtmN), StartUtmZone %in% 10) %>% 
  select(-c(StopUtmN, StopUtmE, StopUtmZone, StartUtmZone))

SILs.zone10stop <- SILs.review.mappingUTMs %>% 
  filter(!is.na(StopUtmN), StartUtmZone %in% 10) %>% 
  select(-c(StartUtmN,StartUtmE,StartUtmZone, StopUtmZone))

start.zone9 <- st_as_sf(SILs.zone9start, coords = c("StartUtmE","StartUtmN"), crs = 26909)
stop.zone9 <- st_as_sf(SILs.zone9stop, coords = c("StopUtmE","StopUtmN"), crs = 26909)

start.zone10 <- st_as_sf(SILs.zone10start, coords = c("StartUtmE","StartUtmN"), crs = 26910)
stop.zone10 <- st_as_sf(SILs.zone10stop,coords = c("StopUtmE","StopUtmN"), crs = 26910)

zone9 <- rbind(start.zone9,stop.zone9 ) %>% 
  st_transform(crs = 4326) #convert to decimal degrees
zone10 <- rbind(start.zone10,stop.zone10 )%>% 
  st_transform(crs = 4326) #convert to decimal degrees

all.zones <- rbind(zone9, zone10)

st_write(all.zones, 
         dsn = paste0("start.stopUTMs-",review.subject,".kml"), 
         driver = "kml") #export to google earth kml


#ddmmss files:

SILs.mapping.ddmmss <- SILs.to.review %>% 
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

SILs.ddmmssstart <- SILs.mapping.ddmmss %>% 
  filter(!is.na(StartLatDeg)) %>% 
  mutate(lat = StartLatDeg+StartLatMin/60+StartLatSec/3600,
         long = -1*(StartLongDeg+StartLongMin/60+StartLongSec/3600)) %>% 
  select(-c(StopLatDeg,StopLatMin,StopLatSec,StopLongDeg,                         
            StopLongMin,StopLongSec,StartLatDeg,StartLatMin,
            StartLatSec,StartLongDeg,StartLongMin,StartLongSec)) %>% 
  filter(!is.na(lat), !is.na(long))

SILs.ddmmssstop <- SILs.mapping.ddmmss %>% 
  filter(!is.na(StopLatDeg)) %>% 
  mutate(lat = StopLatDeg+StopLatMin/60+StopLatSec/3600,
         long = -1*(StopLongDeg+StopLongMin/60+StopLongSec/3600)) %>% 
  select(-c(StartLatDeg,StartLatMin,StartLatSec,StartLongDeg,
            StartLongMin,StartLongSec, StopLatDeg, StopLatMin,
            StopLatSec,StopLongDeg,StopLongMin,StopLongSec)) %>% 
  filter(!is.na(lat), !is.na(long))

start.dd <- st_as_sf(SILs.ddmmssstart, coords = c("long","lat"), crs = 4326)
stop.dd <- st_as_sf(SILs.ddmmssstop, coords = c("long","lat"), crs = 4326)

all.dd <- rbind(start.dd, stop.dd)

st_write(all.dd, 
         dsn = paste0("start.stopDDs-",review.subject,".kml"), 
         driver = "kml") #export to google earth kml



# ANALYSIS ####

# we are trying to get a consistent time series of stream estimates for Babine
# sockeye. Re-calc the auc 

# #AUC functions
# organize_data <- function (data) {
#   data %>%
#     mutate(count=as.numeric(expanded)) %>% 
#     add_row(julian=0, count=0) %>%
#     add_row(julian = 364, count=0) %>% 
#     arrange(julian) %>%
#     select(count, day=julian)%>%
#     mutate(survey = seq(1, nrow(data)+2, by=1)) %>%  #numbers the surveys
#     #add theoretical first day
#     mutate(day.x1 = lead(day,1)-res.time) %>% 
#     mutate(day.x2 = lead(day,1)-
#              lead(count,1)/(abs(lead(count,2)-lead(count,1))/(lead(day,2)-lead(day,1)))) %>% 
#     mutate(day = ifelse(day %in% 0, pmax(day.x1,day.x2, na.rm=T),day)) %>% 
#     #add theoretical last day 
#     mutate(day.x3 = lag(day,1)+res.time) %>% 
#     mutate(day.x4 = lag(day,1)-
#              lag(count,1)/(abs(lag(count,2)-lag(count,1))/(lag(day,2)-lag(day,1)))) %>% 
#     mutate(day = ifelse(day %in% last(day), pmin(day.x3,day.x4, na.rm=T),day)) %>% 
#     select(-c(day.x1,day.x2, day.x3, day.x4))
# }   
# 
# # Fish days function:
# calc_fish_days <- function (day, count, res.time) {
#   
#   data %>% mutate(day_1 = lag(day)) %>% 
#     mutate(count_1 = lag(count)) %>%
#     mutate(int = day - day_1) %>% 
#     mutate(fish_days = c(NA,diff(day)*zoo::rollmean(count,2))) %>% 
#     summarize(fish_days = sum(na.omit(fish_days))) 
# }
# 
# # AUC calc (fish days/RT)
# calc_auc_est <- function (data) {
#   data %>% 
#     test.organize_data() %>% 
#     calc_fish_days() %>% 
#     mutate(auc = fish_days/res.time) %>% 
#     select(auc)
# }


#combining these into one function which also defines residence time:

test.auc.allinone <- function (day, count, res.time){
  data.frame(day, count) %>% 
    add_row(day=0, count=0) %>% 
    add_row(day = 364, count=0) %>% 
    arrange(day) %>% 
    mutate(survey = seq(1, length(day), by=1))  %>%  #numbers the surveys
    #add theoretical first day
    mutate(day.x1 = lead(day,1)-res.time) %>% 
    mutate(day.x2 = lead(day,1)-
             lead(count,1)/(abs(lead(count,2)-lead(count,1))/(lead(day,2)-lead(day,1)))) %>% 
    mutate(day = ifelse(day %in% 0, pmax(day.x1,day.x2, na.rm=T),day)) %>% 
    #add theoretical last day 
    mutate(day.x3 = lag(day,1)+res.time) %>% 
    mutate(day.x4 = lag(day,1)-
             lag(count,1)/(abs(lag(count,2)-lag(count,1))/(lag(day,2)-lag(day,1)))) %>% 
    mutate(day = ifelse(day %in% last(day), pmin(day.x3,day.x4, na.rm=T),day)) %>% 
    select(-c(day.x1,day.x2, day.x3, day.x4)) %>% 
    #good till here
    mutate(day_1 = lag(day))  %>% 
    mutate(count_1 = lag(count))  %>%
    mutate(int = day - day_1) %>% 
    mutate(fish_days = c(NA,diff(day)*zoo::rollmean(count,2))) %>% 
    summarize(fish_days = sum(na.omit(fish_days)))   %>% 
    mutate(auc = fish_days/res.time) %>% 
    select(auc)
}

#example:
test.auc.allinone(day = c(254,263,270), count = c(0/.7, 13300/.7, 35000/.7), 
                  res.time=21)


# import and re-calculate the AUC for AUC estimates.
#make these into a function so it can be re-done repeatedly
#note that this should be the same file as exported earlier 



bab.sils <- read_excel("./babineSK/SILs.babine.SK_12-Jun-2025export.xlsx", 
                       sheet="SILs.babine.SK", na = "NA")

BabS34 <- bab.sils %>% 
  dplyr::filter(StreamId %in% c(463,464)) %>% #babine river s4+s3
  dplyr::select(StreamId, StreamName, Inspection.Year,SilDate,Observer,
         Affiliation,TargetSockeye,PrimaryInspMode,Sock_AL_HoldOutside,
         Sock_AL_Hold,Sock_AL_Spawning,Sock_AL_ObsTotal,
         Sock_AL_EstTotal,Sock_AL_New,Sock_AL_EstReliability,
         Sock_AL_FishCountability,Sock_AD_Obs,Sock_AD_Est,
         Sock_AD_.PreSpawnMort,obs.eff,surveyed.full.extent,
         proportion.surveyed,barriers.present, UseForPeakCount, UseForAUC) %>% 
  mutate(julian = yday(SilDate), 
         obs.eff = ifelse(!is.na(obs.eff),obs.eff,1),
         expanded = Sock_AL_Spawning/obs.eff,
         UseForAUC = as.logical(UseForAUC),
         UseForPeakCount = as.logical(UseForPeakCount)) %>% 
  filter(julian >196 )#%>% # filter dates for after mid-july
  #filter(TargetSockeye %in% T)

ggplot(BabS34)+
  geom_point(aes(x=julian,y=expanded, col=as.factor(StreamName)))+
  geom_line(aes(x=julian,y=expanded,col=as.factor(StreamName)))+
  geom_point(data = BabS34[BabS34$UseForAUC %in% T,], aes(x=julian, y=expanded),
             shape = "star")+
  labs(x="Julian day", y="# spawners (expanded by obs. eff)", col="Stream ")+
  theme_babine4()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=12, hjust=1, angle=45),
        axis.title=element_text(size=14,face="bold"))+
  facet_wrap(~Inspection.Year)

#recalc auc
BabS34aucs.14 <- BabS34.201022 %>% 
  dplyr::filter(UseForAUC %in% T) %>% 
  dplyr::group_by(Inspection.Year, StreamName) %>% 
  dplyr::summarize(test.auc.allinone(day = julian, count= expanded, 
                                     res.time=14))

BabS34aucs.12 <- BabS34.201022 %>% 
  dplyr::filter(UseForAUC %in% T) %>% 
  dplyr::group_by(Inspection.Year, StreamName) %>% 
  dplyr::summarize(test.auc.allinone(day = julian, count= expanded, 
                                     res.time=12))

BabS34aucs.16 <- BabS34.201022 %>% 
  dplyr::filter(UseForAUC %in% T) %>% 
  dplyr::group_by(Inspection.Year, StreamName) %>% 
  dplyr::summarize(test.auc.allinone(day = julian, count= expanded, 
                                     res.time=16))

plot.new.calc.BabS34 <- ggplot()+
  geom_point(data = BabS34aucs.14, aes(x=Inspection.Year, y=auc, col=StreamName))+
  geom_line(data = BabS34aucs.14,aes( x=Inspection.Year, y=auc, col=StreamName))+

  #geom_line(data = BabS34aucs.12, aes(x=Inspection.Year, y=auc), col="gray50")+
  #geom_line(data = BabS34aucs.16, aes( x=Inspection.Year, y=auc), col="gray50")+
  scale_x_continuous(breaks= seq(2010,2022,1))+
  labs(x="", y="Babine R S4 Stream Estimate\n +/- 2 residence days")+
  theme_babine4()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=12, hjust=1, angle=45),
        axis.title=element_text(size=14,face="bold"))
  #scale_y_continuous(limits = c(0,27000), breaks = seq(0,27000,5000))+

plot.new.calc.BabS34

# check what the original estimates by year were

bab.auc <- read_excel("./babineSK/AUC.restime.bab_4-Feb-2025export.xlsx", 
           sheet="AUC.restime.bab")

# BabS34auc <- bab.auc %>% 
#   filter(StreamName %in% "BABINE RIVER (SECTION 4)") %>% 
#   mutate(SockAnnualEst.num = as.numeric(SockAnnualEst))
# 
# plot.old.calc.BabS4 <- ggplot(BabS4auc)+
#   geom_point(aes(x=Year, y=SockAnnualEst.num))+
#   geom_line(aes(x=Year, y=SockAnnualEst.num))+
#   scale_x_continuous(breaks= seq(2010,2022,1))+
#   #scale_y_continuous(limits = c(0,27000), breaks = seq(0,27000,5000))+
#   #theme(legend.position = "bottom")+
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text=element_text(size=12, hjust=1, angle=45),
#         axis.title=element_text(size=14,face="bold"))+
#   labs(x="year", y="Existing Stream Estimate")
# plot.old.calc.BabS4
# 
# 
# ggsave(plot(arrangeGrob(plot.new.calc.BabS4,plot.old.calc.BabS4)),
#        filename = "BabineRS4 Example 2010-2022.png", width=6, height=6)

# Mid-timed redos: 
MorTahlo.201022 <- bab.sils %>% 
  dplyr::filter(StreamId %in% c(474,486)) %>% #babine river s4+s3
  dplyr::select(StreamId, StreamName, Inspection.Year,SilDate,Observer,
                Affiliation,TargetSockeye,PrimaryInspMode,Sock_AL_HoldOutside,
                Sock_AL_Hold,Sock_AL_Spawning,Sock_AL_ObsTotal,
                Sock_AL_EstTotal,Sock_AL_New,Sock_AL_EstReliability,
                Sock_AL_FishCountability,Sock_AD_Obs,Sock_AD_Est,
                Sock_AD_.PreSpawnMort,obs.eff,surveyed.full.extent,
                proportion.surveyed,barriers.present, UseForPeakCount, UseForAUC) %>% 
  mutate(julian = yday(SilDate), 
         obs.eff = ifelse(!is.na(obs.eff),obs.eff,1),
         expanded = Sock_AL_Spawning/obs.eff,
         UseForAUC = as.logical(UseForAUC),
         UseForPeakCount = as.logical(UseForPeakCount)) %>% 
  filter(TargetSockeye %in% T)


#recalc auc
MorTahloaucs.14 <- MorTahlo.201022 %>% 
  dplyr::filter(UseForAUC %in% T) %>% 
  dplyr::group_by(Inspection.Year, StreamName) %>% 
  dplyr::summarize(test.auc.allinone(day = julian, count= expanded, 
                                     res.time=14)) %>% 
  filter(auc >0)

ggplot(MorTahloaucs.14)+
  geom_line(aes(x=Inspection.Year, y=auc, col=StreamName))

MorTahloaucs.14merge <- MorTahloaucs.14 %>%
  mutate(lgSK = auc, SUBAREA = "BABINE") %>% 
  select(-auc, year = Inspection.Year, STREAM.NAME = StreamName) %>% 
  mutate(STREAM.NAME = ifelse(STREAM.NAME %in% "TAHLO CREEK - (LOWER)", "TAHLO CREEK",STREAM.NAME))


#From older file:


Pdrive <- read_excel("./babineSK/4esc_28March2023-copy.xls", sheet = "SELECT CHART - SX",
                     range = "A5:BW105", .name_repair = "universal")

colnames(Pdrive)[5:84] <- substr(colnames(Pdrive)[5:84],4,7)

BabS34aucs.14merge <- BabS34aucs.14 %>% 
  mutate(lgSK = auc, SUBAREA = "BABINE") %>% 
  select(-auc, year = Inspection.Year, STREAM.NAME = StreamName)

Pdrive.bab <- Pdrive %>% 
  select(-c(STAT.AREA, SPECIES)) %>% 
  filter(SUBAREA %in% "BABINE",
         STREAM.NAME %in% c("BABINE RIVER (SECTIONS 1 - 3)","BABINE RIVER (SECTION 4)",
                            "MORRISON CREEK", "TAHLO CREEK", "PIERRE CREEK", 
                            "FOUR-MILE CREEK")) %>% 
  pivot_longer(-c(SUBAREA, STREAM.NAME),
               names_to = "year",
               values_to = "lgSK") %>% 
  filter(year < 2010) %>% 
  rbind(BabS34aucs.14merge) %>% 
  rbind(MorTahloaucs.14merge) %>% 
  mutate(stock = ifelse(STREAM.NAME %in% c("PIERRE CREEK", "FOUR-MILE CREEK"),
                        "Babine-early",ifelse(STREAM.NAME %in% c("MORRISON CREEK", "TAHLO CREEK"), 
                                              "Babine-mid", "Babine-late")),
         year = as.numeric(year), lgSK = as.numeric(lgSK))  
  
options(scipen=999)

ggplot(Pdrive.bab[Pdrive.bab$stock %in% "Babine-late",])+
  geom_line(aes(x=year, y=lgSK, col=STREAM.NAME))+
  geom_smooth(aes(x=year, y=lgSK, col=STREAM.NAME), method = "loess", se=F) +
  labs(x="year",y="Annual Sockeye Estimate", col="")+
  scale_x_continuous(breaks = seq(min(Pdrive.bab$year),max(Pdrive.bab$year),5))+
  scale_y_continuous(labels = scales::comma)+
  theme_babine4()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=16, hjust=1, angle=45),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16, face="bold"))

ggplot(Pdrive.bab[Pdrive.bab$stock %in% "Babine-mid",])+
  geom_line(aes(x=year, y=lgSK, col=STREAM.NAME))+
  geom_smooth(aes(x=year, y=lgSK, col=STREAM.NAME), se=F)+
  labs(x="year",y="Annual Sockeye Estimate", col="")+
  scale_x_continuous(breaks = seq(min(Pdrive.bab$year),max(Pdrive.bab$year),5))+
  scale_y_continuous(labels = scales::comma)+
  theme_babine4()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=16, hjust=1, angle=45),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16, face="bold"))

ggplot(Pdrive.bab[Pdrive.bab$stock %in% "Babine-early",])+
  geom_line(aes(x=year, y=lgSK, col=STREAM.NAME))+
  geom_smooth(aes(x=year, y=lgSK, col=STREAM.NAME), se=F)+
  labs(x="year",y="Annual Sockeye Estimate", col="")+
  scale_x_continuous(breaks = seq(min(Pdrive.bab$year),max(Pdrive.bab$year),5))+
  scale_y_continuous(labels = scales::comma)+
  theme_babine4()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=16, hjust=1, angle=45),
        axis.title=element_text(size=16,face="bold"),
        legend.text = element_text(size=16, face="bold"))



# Nanika SK redo:

nan.auc.sk <- read_excel("./nanikaSK/finished/AUC.res.time.review-nanikaSKCH2004-2022.xlsx",na = "NA")
nan.sil.sk <- read_excel("./nanikaSK/finished/SILs.toreview-nanikaSKCH2004-2022.xlsx",na = "NA")
str(nan.sil.sk)

nansk.200422 <- nan.sil.sk %>% 
  dplyr::select(StreamId, StreamName, Inspection.Year,SilDate,Observer,
                Affiliation,TargetSockeye,PrimaryInspMode,Sock_AL_HoldOutside,
                Sock_AL_Hold,Sock_AL_Spawning,Sock_AL_ObsTotal,
                Sock_AL_EstTotal,Sock_AL_New,Sock_AL_EstReliability,
                Sock_AL_FishCountability,Sock_AD_Obs,Sock_AD_Est,
                Sock_AD_.PreSpawnMort,obs.eff,surveyed.full.extent,
                proportion.surveyed,barriers.present) %>% 
  mutate(julian = yday(SilDate), 
         ave.obs.eff = mean(obs.eff, na.rm= T),
         obs.eff = ifelse(!is.na(obs.eff),obs.eff,ave.obs.eff), #use ave. obs eff 
         expanded = Sock_AL_Spawning/obs.eff,
         PrimaryInspMode = case_match(PrimaryInspMode,"Helicoptor"~"Helicopter",
                "Snorkel/Swin"~"Snorkel/Swim", .default = PrimaryInspMode)) %>% 
  filter(TargetSockeye %in% T,PrimaryInspMode %in% c("Helicopter","Stream Walk"),
         surveyed.full.extent %in% "Yes")
# filter out snorkel

ggplot(nansk.200422)+
  geom_point(aes(x=julian,y=expanded, col=as.factor(Inspection.Year)))+
  geom_line(aes(x=julian,y=expanded,col=as.factor(Inspection.Year)))


#recalc auc
nan.sk.aucs <- nansk.200422 %>% 
  dplyr::group_by(Inspection.Year) %>% 
  dplyr::summarize(test.auc.allinone(day = julian, count= expanded, 
                                     res.time=10))

plot.new.calc.nansk <- ggplot(nan.sk.aucs)+
  geom_point(aes(x=Inspection.Year, y=auc))+
  geom_line(aes(x=Inspection.Year, y=auc))+
  geom_line(data = nan.auc.sk, aes(x=Year, y = as.numeric(QA.SockAnnualEst)), col="red")+
  #scale_x_continuous(breaks= seq(min(nan.sk.aucs$Inspection.Year),
   #                              max(nan.sk.aucs$Inspection.Year),1))+
  #scale_x_continuous(limits= c(2018,2024))+
  scale_y_continuous(limits = c(0,max(nan.sk.aucs$auc, na.rm=T)))
plot.new.calc.nansk





#Kristen's older script:

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

