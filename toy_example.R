library(tidyverse)


#################################
#The cost loss curves for the toy example
toy_example = read.csv('toy_example_data.csv')
treatment_cost = 10
possible_loss_costs = 10 / seq(0.11, 1, 0.01)
total_km2 = 16
smallest_grain_expense_perfect = (7 * treatment_cost) / total_km2
smallest_grain_expense_always = treatment_cost

values = data.frame()

for(this_scale in c(1,2)){
  data_subset = toy_example %>% filter(spatial_scale == this_scale)
  for(loss_cost in possible_loss_costs){
    smallest_grain_never = 7 * loss_cost / total_km2
    smallest_grain_maximum = min(smallest_grain_never, smallest_grain_expense_always)
    
    this_grain_tp_fp_expense = sum(data_subset$prediction) * treatment_cost
    this_grain_fn_expense = sum(data_subset$presence==1 & data_subset$prediction==0) * loss_cost
    this_grain_expense = (this_grain_fn_expense + this_grain_tp_fp_expense) / total_km2
    
    a = treatment_cost/loss_cost
    
    values = values %>%
      bind_rows(data.frame(spatial_scale = this_scale, expense_forecast = this_grain_expense, expense_max = smallest_grain_maximum,
                           expense_perfect = smallest_grain_expense_perfect, a=a))
  }
}

values$value = with(values, (expense_max - expense_forecast) / (expense_max - expense_perfect))

ggplot(values, aes(x=a, y=value, group=as.factor(spatial_scale), color=as.factor(spatial_scale))) +
  geom_line(size=1.5) +
  ylim(0,1) +
  theme_bw()
