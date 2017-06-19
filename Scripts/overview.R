#Getting Zooniverse Dat

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(data.table)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
# grab launch dates
# download updated csv at: https://docs.google.com/spreadsheets/d/1hMmQxmaw-dNljQHek5f8TjUP4RlXp2a_Pkhcfc0Nr9o/edit#gid=0
# dates are MM/DD/YYYY, annoyingly.

recalculate_dates <- function() {
  launch_dates <- read.csv("Data/project_launch_dates_26_may.csv")[,-1] %>% 
    rename(., date = X.1, project = X.2, panoptes_id = Zoo.project_id..Panoptes., platform = Panoptes.VS.Ouroboros)
  launch_dates %<>% mutate(., date = mdy(date), counter = row_number())
  write.csv(launch_dates, "Data/working_project_launches.csv", row.names = F)
}

#recalculate_dates()

# load launch dates
launch_dates <- read.csv("Data/working_project_launches.csv")
labels <- read.csv("Data/project_labels.csv")

launch_dates %<>% 
  mutate(., date = ymd(date)) %>%
  left_join(., labels) %>%
  mutate(., diy = ifelse(diy == 0, "custom", "project-builder"))


getwd()

#load weekly stats
load(file = "Data/stats_by_week.rda")
