library(ggplot2)

## Getting Project Release Data - look at # projects over time
# load dates in overview.R

launches <- read.csv("Data/project_launches.csv")
launch_dates <- launches %>% mutate(., date = dmy(as.character(date)), counter = row_number())

# plotting the launch dates
launch_plot <- ggplot(data = launch_dates, aes(x = date, y = counter)) + 
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = counter), alpha = .4) +
  geom_point(aes(colour = type)) 
  

launch_plot + theme_bw() +
  geom_text(aes(label = project), vjust = "inward", hjust = "right", size = 2) +
  theme(panel.border=element_blank()) +
  labs(y = "Number of Projects")

quartz()
ggplot(data = launch_dates, aes(x = date, y = counter)) + 
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = counter), alpha = .4) +
  geom_point(aes(colour = type)) + theme_bw() +
  theme(panel.border=element_blank(), legend.position = "none", text = element_text(size = 16)) +
  labs(y = "Number of Projects")



# plot annual growth (fewer weirdnesses/singularities, and demonstrate the relative proportion of each category)
annual_data <- launch_dates %>% group_by(type, year = year(date)) %>%
  summarise(total = n()) %>%
  mutate(total = cumsum(total)) %>%
  ungroup %>% # need to ungroup to fill in the damn gaps.
  complete(type, year, fill = list(total = 0)) %>% #fills with 0s instead of the last count
  group_by(type) %>% 
  mutate(., cumulative_projects = cummax(total)) #okay, this should work.


annual_plot <- ggplot(annual_data, aes(x = year, y = cumulative_projects, fill = type)) + 
  geom_area(alpha = .4) + theme(text = element_text(size = 16), legend.position = c(.2, .8))
annual_plot


# project totals
launch_dates %>% group_by(., type) %>% summarise(n())

# custom_summaries
type_by_platform <- launch_dates %>% 
  group_by(., diy, type) %>% 
  summarise(., tot = n()) %>%
  mutate(., freq = tot/sum(tot))

blanking <- theme(axis.line=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    legend.position="none",
                    panel.background=element_blank(),
                    panel.border=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    plot.background=element_blank())

diy_summary <- ggplot(type_by_platform, aes(x = diy, y = freq)) + 
  geom_bar(stat = "identity", aes(fill = type), alpha = .5) + coord_flip() +
  theme(legend.position = "none") + blanking + 
  annotate(geom = "text", x = 2.5, y = .1, label = "project builder") + annotate(geom = "text", x = 1.5, y = .1, label = "custom")

diy_summary 


# should we remove dead projects? calc the proportion of LIVE projects?
annual_plot <- ggplot(annual_data, aes(x = year, y = cumulative_projects, fill = type)) + 
  geom_area(alpha = .4)+ theme_bw() + theme(text = element_text(size = 16), legend.position = c(.2, .8)) +
  ylab("Number of Projects")
annual_plot


quartz()
vp <- viewport(width = 0.5, height = 0.4, x = unit(.05, "npc"), y = unit(.95, "npc"), just = c("left", "top"))
pushViewport(vp)
print(annual_plot)
print(diy_summary, vp = vp)


dev.off()

###############################################
############### EXPLORING #####################
###############################################


# Exploring other plots
launch_plot + geom_smooth(data = filter(launch_dates, type == "ecology"), colour = "green") + geom_smooth(data = filter(launch_dates, type != "ecology"))
annual_growth <- launch_dates %>% group_by(., type, year = year(date)) %>% summarise(., count = n())
ggplot(data = annual_growth, aes(x = year, y = count, colour = type)) + geom_line()


## growth before v after panoptes
launch_dates %<>% mutate(., day_since = difftime(time1 = date, time2 = min(date), units = "days")) %>%
  # mutate(., test = date - day_since) # checks to make sure everything is calculated properly, which it is.
  mutate(., days_counted = as.numeric(day_since))

# exploring rates of growth in ecology vs other fields (simplify by calculating relative percentage of ecology projects? As this doesn't do anything)
all_projects <- launch_dates
ecol_projects <- launch_dates %>% filter(., type == "ecology")
mod <- lm(counter ~ days_counted, data = ecol_projects)
summary(mod)
mod <- lm(counter ~ days_counted, data = all_projects)
summary(mod)


