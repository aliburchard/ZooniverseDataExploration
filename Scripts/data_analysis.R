# pulll in dates in overview.r

# plotting the launch dates
launch_plot <- ggplot(data = launch_dates, aes(x = date, y = counter)) + 
  geom_line() +
  geom_point(aes(colour = type)) 


# plot annual growth (fewer weirdnesses/singularities, and demonstrate the relative proportion of each category)
annual_data <- launch_dates %>% group_by(type, year = year(date)) %>%
  summarise(total = n()) %>%
  mutate(total = cumsum(total)) %>%
  ungroup %>%
  complete(type, year, fill = list(total = 0))

annual_plot <- ggplot(annual_data, aes(x = year, y = total, fill = type)) + geom_area(alpha = .4)


#run the plots
launch_plot + geom_text(aes(label = project, colour = type), vjust = "inward", hjust = "inward", size = 3)
annual_plot




# plot new users
# plot 
