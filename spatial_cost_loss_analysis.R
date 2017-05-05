library(tidyverse)


results_averaged = results %>%
  group_by(spatial_scale, a, forecast_error) %>%
  summarise(value = mean(value)) %>%
  ungroup()


#############################################
ggplot(filter(results_averaged, value>0), aes(y=value, x=a, color=as.factor(spatial_scale), group=as.factor(spatial_scale))) + 
  geom_point() + 
  geom_line() +
  facet_grid(forecast_error~., scales='free_y')
