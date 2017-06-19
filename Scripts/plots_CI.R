
launches <- read.csv("Data/project_launches.csv")
launch_dates <- launches %>% mutate(., date = dmy(as.character(date)), counter = row_number())


launch_plot <- ggplot(data = launch_dates, aes(x = date, y = counter)) + 
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = counter), alpha = .4) +
  geom_point(aes(colour = type)) 

launch_plot <- ggplot(data = launch_dates, aes(x = date, y = counter)) + 
  geom_line(size = 1.2) +
  geom_vline(aes(xintercept = as.numeric(as.Date(date))), colour = "gray") +
  geom_point(aes(colour = type), size = 2) 

quartz()
launch_plot + theme_bw() +
  #geom_text(aes(label = project), vjust = "inward", hjust = "right", size = 2) +
  theme(panel.border=element_blank(), legend.position = c(.1, .8)) +
  labs(y = "Number of Projects") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# add vertical lines for every project launch & hide the legend


annual_data <- launch_dates %>% group_by(year = year(date)) %>%
  summarise(total = n()) %>%
  mutate(., year_color = as.factor(year))

annual_data_ecol <- launch_dates %>% mutate(., ecology = ifelse(type == "Ecology", "ecology", "other")) %>%
  group_by(year = year(date), ecology) %>%
  summarise(total = n()) %>%
  mutate(., year_color = as.factor(year))


jpeg(filename = "ecology_v_other.jpg")
ggplot(annual_data_ecol, aes(x = year, y = total, fill = ecology)) + 
  geom_bar(stat = "identity") + scale_x_continuous(breaks = annual_data$year, labels = annual_data$year) +
  theme_bw() + 
  theme(panel.border=element_blank(), panel.grid.major.x = element_blank()) +
  labs(y = "Number of Projects") +
  theme(text = element_text(size = 16), legend.position = c(.1, .9)) + 
  scale_fill_manual(values = c( "#CFBF2D", "#9D9D9D"))
dev.off()

quartz()
annual_plot <- ggplot(annual_data, aes(x = year, y = total, fill = year_color)) + 
  geom_bar(stat = "identity") + scale_x_continuous(breaks = annual_data$year, labels = annual_data$year) +
  theme_bw() + 
  theme(panel.border=element_blank()) +
  labs(y = "Number of Projects") +
 theme(text = element_text(size = 16), legend.position = "none")
annual_plot

# barplot projects by type
quartz()
ggplot(launch_dates, aes(x = type, fill = type)) + 
  geom_bar() +
  theme_bw() + 
  theme(panel.border=element_blank()) +
  labs(y = "Number of Projects", x = "") +
  theme(text = element_text(size = 16), legend.position = "none")

quartz()
types <- launch_dates %>% group_by(., type) %>% summarize(., count = n())
type_plot <- ggplot(types, aes(x = "", y = count, fill = type)) + 
  geom_bar(width = 1, stat = "identity") + 
  #coord_polar("y", start = 0) + theme_minimal() + 
  theme(axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank())

 type_plot  
 