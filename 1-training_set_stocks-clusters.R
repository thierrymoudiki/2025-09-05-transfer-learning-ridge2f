source("0-functions.R")

# 2. Bayesian Optimization with rmse Loss ------------------------------------
set.seed(12345)
synthetic_data <- generate_diverse_sv_paths(n_paths = 1000, horizon = 500)
synthetic_paths <- synthetic_data$paths

# Objective function minimizing rmse with probabilistic forecasts
objective_function_rmse <- function(xx) {
  nb_hidden <- floor(xx[1])  
  lambda_1 <- 10^xx[3]
  lambda_2 <- 10^xx[4]
  lags_val <- min(floor(xx[2]), 100)  
  block_length <- floor(xx[5])
  centers <- floor(xx[6])
  rmse_scores <- rep(NA, length(synthetic_paths))
  
  pb <- utils::txtProgressBar(max = length(synthetic_paths), 
  style=3)
  
  for (i in seq_along(synthetic_paths)) {
    series <- synthetic_paths[[i]]
    train <- series[1:400]
    test <- series[401:500]    
    train_mean <- mean(train)
    train_sd <- sd(train)
    scaled_train <- (train - train_mean) / train_sd    
    # Generate probabilistic forecasts with block bootstrap
    fit <- try(ahead::ridge2f(
      y = scaled_train,
      h = length(test),
      lags = lags_val,
      nb_hidden = nb_hidden,
      lambda_1 = lambda_1,
      lambda_2 = lambda_2,
      centers = centers, 
      show_progress = FALSE
    ), silent = TRUE)    
    if (!inherits(fit, "try-error")) {
      # Rescale the predictive samples      
      predictive_samples <- fit$mean * train_sd + train_mean      
      # Calculate rmse for each forecast horizon and average      
      rmse_scores[i] <- sqrt(mean((test - predictive_samples)**2))
    }
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)  
  median(rmse_scores, na.rm = TRUE)  # Minimize median rmse
}

# Run Bayesian optimization minimizing rmse
res_rmse <- bayesianrvfl::bayes_opt(
  objective_function_rmse,
  lower = c(3L, 1L, -4, -4, 5, 0),
  upper = c(40L, 100L, 5, 5, 200, 5L),
  init_points = 10,
  n_iter = 50
)

saveRDS(res_rmse, "best_params_with_clustering.rds")