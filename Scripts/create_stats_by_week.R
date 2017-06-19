#
# load launch dates
launch_dates <- read.csv("Data/working_project_launches.csv")
labels <- read.csv("Data/project_labels.csv")

launch_dates %<>% 
  mutate(., date = ymd(date)) %>%
  left_join(., labels)

# load user dates
user_creation_dates <- fread("Data/user-growth.csv")

new_users <- user_creation_dates %>% 
  mutate(., counter = row_number(created_at), datetime = created_at) %>%
  separate(., col = created_at, into = c("date", "time"), sep = " ") %>%
  mutate(., date = ymd_hms(datetime), dDate = as.Date(date)) %>%
  mutate(., day = round_date(date, "day"), week = week(date), month = month(date), year = year(date), yearweek = round_date(date, "week"))

users_by_week <- new_users %>%
  group_by(., yearweek) %>%
  summarise(., daily_new_users = length(time), cumulative_users = max(counter)) %>%
  mutate(., Date = as.Date(yearweek), date = ymd(yearweek), month = month(date), year = year(date))

#GRUMBLE GRUMBLE GRUMBLE. dealing with dates and whatnot is a giant fucking faff.
projects_rounded <- launch_dates %>%
  mutate(., yearweek = as.Date(round_date(date, "week"))) %>%
  group_by(., yearweek) %>%
  summarise(., cumulative_projects = max(counter))

all_dates <- data.frame(yearweek = seq(from = min(users_by_week$Date), to = max(users_by_week$Date), by = "week")) %>% 
  left_join(., projects_rounded) %>%
  mutate(., temp = ifelse(is.na(cumulative_projects), 0, cumulative_projects)) %>%
  mutate(., cum_projects = cummax(temp)) %>% 
  select(., Date = yearweek, cum_projects)

stats_by_week <- users_by_week %>% 
  left_join(., all_dates, copy = T) %>%
  select(., Date, date, month, year, daily_new_users, cumulative_users, cumulative_projects = cum_projects)

save(stats_by_week, file = "Data/stats_by_week.rda")

