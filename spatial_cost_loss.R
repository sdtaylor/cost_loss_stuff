library(raster)
library(tidyverse)
# unconditional simulations on a 100 x 100 grid using gstat
library(gstat)
library(doParallel)

#Generate presence/absence observations that simulate either a good (low error)
#or bad (high error) forecast.
#p : a vector of probabilites
#error: int between 0-1
observation_from_probability = function(p, error){
  add_or_subtract = sample(c(1,-1),length(p), replace=TRUE)
  
  random_errors = rnorm(length(p), mean = error, sd = 0.01) * add_or_subtract
  
  obs = ifelse(p + random_errors <= 0.5, 0, 1)
  
  return(obs)
}

#Testing the above function
#r = data.frame()
#for(this_error in rbeta(1000, 1,1)){
#  probs = rbeta(100,1,1)
#  obs = observation_from_probability(probs, error=this_error)
#  r = r %>%
#    bind_rows(data.frame('error' = this_error, 'brier'=mean((probs-obs)^2),'brier2'=verification::brier(obs, probs)$bs,
#                         'brier_reliability'=verification::brier(obs, probs)$bs.reliability,'auc'=Metrics::auc(obs, probs)))
#}
#plot(r)

#Create a raster of random spatially correlated probabilites
get_random_prob_layer = function(size=100){
  # create structure
  xy <- expand.grid(1:size, 1:size)
  names(xy) <- c("x","y")
  
  # define the gstat object (spatial model)
  g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025,model="Exp",range=5), nmax=20)
  
  # make simulation based on the stat object
  yy <- predict(g.dummy, newdata=xy, nsim=1)
  
  #Scale from 0 to 1
  yy$sim1 = with(yy, (sim1 - min(sim1)) / (max(sim1) - min(sim1)))
  
  # show one realization
  gridded(yy) = ~x+y
  yy = raster(yy)
  return(yy)
}

#Total cost for observations which were not forecasted to be true. forecast_raster may be a larger grain size than
calculate_loss_cost = function(forecast_raster, observation_raster, L){
  fn = (as.vector(observation_raster)==1 & as.vector(forecast_raster)==0)
  total_loss_area = sum(fn)
  total_loss = total_loss_area * L
  return(total_loss)
}

##################################################
numProcs=2
cl=makeCluster(numProcs)
registerDoParallel(cl)
##################################################

original_extent = 128
total_km2 = original_extent^2
spatial_scales = c(1,2,4,8,16,32, 64)

num_runs = 2

treatment_cost = 10
possible_loss_costs = 10 / seq(0.11, 1, 0.01)
#Add in denser estimates for low values of C/L
possible_loss_costs = c(possible_loss_costs, 10 / seq(0.001, 0.1, 0.001))

results = foreach(run_i = 1:num_runs, .combine=rbind, .packages=c('dplyr','gstat','raster')) %dopar% {
  results_this_run = data.frame()
  for(model_forecast_error in c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3)){
    probability_surface = get_random_prob_layer(size=original_extent)
    observations = raster::calc(probability_surface, fun=function(x){return(observation_from_probability(x, error=model_forecast_error))})
    
    for(loss_cost in possible_loss_costs){
      binary_forecast = raster::calc(probability_surface, fun = function(x){(x>(treatment_cost / loss_cost)) * 1})
      
      smallest_grain_cost_perfect = (sum(as.vector(observations)) * treatment_cost)  / total_km2
      smallest_grain_cost_never = (sum(as.vector(observations)) * loss_cost)  / total_km2
      smallest_grain_cost_maximimum = min(treatment_cost, smallest_grain_cost_never)
      
      for(this_spatial_scale in spatial_scales){
        #Upscale the raster, then return back to the 1x1 resolution for comparison against 1x1 observation
        if(this_spatial_scale > 1){
          binary_forecast_upscaled = disaggregate(aggregate(binary_forecast, fact=this_spatial_scale, fun=max), fact=this_spatial_scale)
        } else {
          binary_forecast_upscaled = binary_forecast
        }
        
        this_scale_treatment_cost = (sum(as.vector(binary_forecast_upscaled)) * treatment_cost)
        this_scale_loss_cost = calculate_loss_cost(forecast_raster = binary_forecast_upscaled, observation_raster = observations, L = loss_cost)
        this_scale_expense = (this_scale_treatment_cost + this_scale_loss_cost) / total_km2
  
        results_this_run = results_this_run %>%
          bind_rows(data.frame('forecast_error' = model_forecast_error,
                               'a' = treatment_cost / loss_cost,
                               'spatial_scale' = this_spatial_scale, 
                               'expense_max' = smallest_grain_cost_maximimum,
                               'expense_perfect' = smallest_grain_cost_perfect, 
                               'expense_forecast' = this_scale_expense,
                               'run'=run_i))
      }
    }
  }
  return(results_this_run)
}
results$value = with(results, (expense_max - expense_forecast) / (expense_max - expense_perfect))

write.csv(results, 'spatial_cost_loss_sim_results.csv', row.names = FALSE)
