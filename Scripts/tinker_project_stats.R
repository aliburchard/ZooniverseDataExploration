launch_dates <- read.csv("Data/working_project_launches.csv")
labels <- read.csv("Data/project_labels.csv")

projects <- fread("Data/projects_list.csv")
projects %<>% filter(., launch_approved == "t") %>% 
  mutate(., platform = as.factor(ifelse(name == "", "P", "O"))) %>%
  mutate(., launch_date = ymd_hms(launch_date))
  

glimpse(projects)
summary(projects)



launch_dates %<>% 
  mutate(., date = ymd(date)) %>%
  left_join(., labels) %>% 
  left_join(., projects)


ggplot(data = filter(projects, classifications_count > 100),aes(x = classifications_count, fill = launch_approved)) + geom_histogram(bins = 100) + facet_wrap(~launch_approved, scale = "free") + scale_x_log10()

ggplot(data = projects)

# grab project stats: project name, launch date, platform, # classifications, # users, # talk comments, 
