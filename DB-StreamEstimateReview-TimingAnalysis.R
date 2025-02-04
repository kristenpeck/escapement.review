# install.packages("readxl")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("tidyverse")
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

# THIS SCRIPT WILL GENERATE A TIMING ANALYSIS FOR A SINGLE STREAM
# THIS INCLUDES A TABLE AND A GANTT CHART
# IMPORTANT! MUST UPDATE STREAM ID AND INPUT ANY SPECIAL CONSIDERATIONS BELOW

## Download MasterStreamInspection file
# Only have to do this once!

SILS.raw <- read_excel(
  path = "references/MasterStreamInspection2004-2022.xlsx"
  )

## Filter to desired StreamID
# Start here if you're just regenerating a new stream!
# Streams 464 and 465 have SPECIAL CONSIDERATIONS before running script!

filtered_SILS <- SILS.raw %>% filter(StreamID == 464)

# Filter columns

filtered_SILS <- filtered_SILS %>%
  select(StreamID, `Inspection Year`, SilDate, Sock_AL_Spawning) %>%
  mutate(SilDate = yday(ymd(SilDate))) %>%
  filter(SilDate <= 334)%>%
  arrange(`Inspection Year`) %>%
  mutate(Sock_AL_Spawning = ifelse(is.na(Sock_AL_Spawning), 0, Sock_AL_Spawning))

# Script breaks if included above so this filter is done on its own

filtered_SILS <- filtered_SILS %>%
  filter(`Inspection Year` %in% 2010:2022)

# Create Start365 column

start365 <- filtered_SILS %>%
  group_by(`Inspection Year`) %>%
  filter(SilDate == min(SilDate)) %>%
  ungroup()

start365$start365 <- start365$SilDate
start365$SilDate <- NULL

timing_analysis <- start365

# Create End365 column

end365 <- filtered_SILS %>%
  group_by(`Inspection Year`) %>%
  filter(SilDate == max(SilDate)) %>%
  ungroup()

end365$end365 <- end365$SilDate
end365$SilDate <- NULL

timing_analysis$end365 <- end365$end365

# Create Peak365 column

peak365 <- filtered_SILS %>%
  group_by(`Inspection Year`) %>%
  filter(Sock_AL_Spawning == max(Sock_AL_Spawning)) %>%
  ungroup()

peak365$SilDate[peak365$Sock_AL_Spawning == 0] <- 0

peak365 <- peak365 %>% distinct()

peak365$peak365 <- peak365$SilDate
peak365$SilDate <- NULL

timing_analysis$peak365 <- peak365$peak365
timing_analysis$Sock_AL_Spawning <- NULL

## SPECIAL CONSIDERATIONS FOR BAD PEAK COUNT DATES
# StreamID 464 has a bad peak_start day, see comments

# timing_analysis[11,5] <- 0

# StreamID 463 has a bad start365 day, see comments

# timing_analysis[2,3] <- 250

# Create PeakStart column

PeakStart <- timing_analysis %>%
  filter(peak365 != 0) %>%
  filter(peak365 == min(peak365)) %>%
  slice(1)

timing_analysis$peak_start <- PeakStart$peak365

# Create PeakEnd column

PeakEnd <- timing_analysis %>%
  filter(peak365 != 0) %>%
  filter(peak365 == max(peak365)) %>%
  slice(1)

timing_analysis$peak_end <- PeakEnd$peak365

# Create Gantt Chart combined with Line Chart

ggplot(timing_analysis, aes(x = `Inspection Year`)) +
  geom_ribbon(aes(ymin = start365, ymax = end365), fill = "blue") +
  geom_line(aes(y = peak365), color = "red", size = 2) +
  geom_line(aes(y = peak_start), color = "orange", linetype = "dotted", size = 1.5) +
  geom_line(aes(y = peak_end), color = "orange", linetype = "dotted", size = 1.5) +
  scale_y_continuous(limits = c(180, 340), breaks = seq(180, 340, by = 10)) +
  scale_x_continuous(labels = as.integer, breaks = seq(2010, 2022, by = 1)) +
  labs(title = "Babine River (Section 4)",
       x = "Inspection Year",
       y = "Calendar Days") +
  theme_minimal()

print(timing_analysis)

