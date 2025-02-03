srmse_func <-  function(obs, pred) {
  
  squared_sums <- (obs - pred)^2
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  srmse <- rmse/colSds(as.matrix(obs), na.rm=TRUE)
  return(rmse)
  
}
