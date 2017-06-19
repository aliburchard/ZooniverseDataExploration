#use overview to load
#new_users
#stats_by_week

# ## takes an obscenely long time to calculate new users by day, so do everything by week!
# load new users if we want it - calculated in create_stats_by_week
# summarise by week for much faster rendering

stats_by_week %<>% mutate(., ppl_per_proj = cumulative_users/cumulative_projects)

#use date for x axis because allows for easier format tweaking.
ggplot(data = stats_by_week, aes(x = Date, y = cumulative_users)) + geom_line() + theme_bw() + scale_x_date() + blanking
ggplot(data = stats_by_week, aes(x = Date, y = daily_new_users)) + geom_line() + theme_bw() + scale_x_date()

ggplot(data = stats_by_week, aes(x = Date, y = cumulative_projects)) + geom_line() + theme_bw() + scale_x_date() #don't love the stepping in this.
ggplot(data = stats_by_week, aes(x = Date, y = ppl_per_proj)) + geom_line() + theme_bw() + scale_x_date() #don't love the stepping in this.

launch_plot

#users per project through time?




### create a data frame that has a record for every week and a column for cumulative projects and cumulative users, shouldn't be this hard.


