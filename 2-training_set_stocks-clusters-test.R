# Install and load required packages
if (!require("rugarch")) install.packages("rugarch")
if (!require("fGarch")) install.packages("fGarch")
if (!require("forecast")) install.packages("forecast")
library(rugarch)
library(fGarch)
library(forecast)

best_params <- readRDS(file="best_params_with_clustering.rds")
print("Bayesian Optimization Parameters:")
print(best_params$best_param)

params <- list()
params$nb_hidden <- floor(best_params$best_param[1])
params$lags <- floor(best_params$best_param[2])
params$lambda_1 <- 10**best_params$best_param[3]
params$lambda_2 <- 10**best_params$best_param[4]
params$centers <- floor(best_params$best_param[6])
print("Transformed Parameters:")
print(params)

# Split data into train (90%) and test (10%)
n_series <- ncol(EuStockMarkets)
stock_data <- tail(EuStockMarkets, 501)
returns_stock_data <- diff(log(stock_data))
print(paste("Data dimensions:", dim(returns_stock_data)[1], "x", dim(returns_stock_data)[2]))

splitted_returns_stock_data <- misc::splitts(returns_stock_data, split_prob = 0.8)
n_test <- length(splitted_returns_stock_data$testing)

# Initialize comprehensive results storage
results <- list()

set.seed(123)

pb <- utils::txtProgressBar(min = 0, max = ncol(EuStockMarkets), style = 3)

for (i in seq_len(ncol(EuStockMarkets))) {
  series_name <- colnames(EuStockMarkets)[i]
  train <- splitted_returns_stock_data$training[, i]
  test <- splitted_returns_stock_data$testing[, i]
  
  # Store forecasts for each method
  forecasts <- list()
  metrics <- list()
  
  # --- ridge2f (Transfer Learning Approach) ---
  train_mean <- mean(train)
  train_sd <- sd(train)
  scaled_train <- (train - train_mean) / train_sd
  
  fit_ridge <- try(ahead::ridge2f(
    y = scaled_train,
    h = length(test),
    nb_hidden = params$nb_hidden,
    lags = min(params$lags, length(scaled_train) - 1L),
    lambda_1 = params$lambda_1,
    lambda_2 = params$lambda_2,
    centers = params$centers,
    level = 95,
    B = 250,
    type_pi = "movingblockbootstrap",
  ), silent = TRUE)
  
  if (!inherits(fit_ridge, "try-error")) {
    rescaled_mean <- fit_ridge$mean * train_sd + train_mean
    rescaled_lower <- fit_ridge$lower * train_sd + train_mean
    rescaled_upper <- fit_ridge$upper * train_sd + train_mean
    
    forecasts$ridge2 <- rescaled_mean
    
    # Calculate comprehensive metrics
    metrics$ridge2 <- list(
      winkler = misc::winkler_score(test, rescaled_lower, rescaled_upper, 95),
      coverage = mean((rescaled_lower <= test) & (test <= rescaled_upper)) * 100,
      interval_width = mean(rescaled_upper - rescaled_lower)
    )
  }
  
  # --- GARCH using rugarch ---
  garch_spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
    distribution.model = "norm"
  )
  
  garch_fit <- try(ugarchfit(
    spec = garch_spec,
    data = train,
    solver = "hybrid"
  ), silent = TRUE)
  
  if (!inherits(garch_fit, "try-error")) {
    garch_forecast <- ugarchforecast(garch_fit, n.ahead = length(test))
    garch_mean <- as.numeric(fitted(garch_forecast))
    garch_sigma <- as.numeric(sigma(garch_forecast))
    
    z_value <- qnorm(0.975)
    garch_lower <- garch_mean - z_value * garch_sigma
    garch_upper <- garch_mean + z_value * garch_sigma
    
    forecasts$rugarch <- garch_mean
    
    metrics$rugarch <- list(
      winkler = misc::winkler_score(test, garch_lower, garch_upper, 95),
      coverage = mean((garch_lower <= test) & (test <= garch_upper)) * 100,
      interval_width = mean(garch_upper - garch_lower)
    )
  }
  
  # --- GARCH using fGarch ---
  fgarch_fit <- try(garchFit(
    formula = ~ garch(1, 1),
    data = train,
    include.mean = FALSE,
    trace = FALSE,
    cond.dist = "norm"
  ), silent = TRUE)
  
  if (!inherits(fgarch_fit, "try-error")) {
    fgarch_forecast <- predict(fgarch_fit, n.ahead = length(test))
    fgarch_mean <- rep(0, length(test))  # fGarch assumes zero mean
    fgarch_sigma <- fgarch_forecast$standardDeviation
    
    z_value <- qnorm(0.975)
    fgarch_lower <- -z_value * fgarch_sigma
    fgarch_upper <- z_value * fgarch_sigma
    
    forecasts$fgarch <- fgarch_mean
    
    metrics$fgarch <- list(
      winkler = misc::winkler_score(test, fgarch_lower, fgarch_upper, 95),
      coverage = mean((fgarch_lower <= test) & (test <= fgarch_upper)) * 100,
      interval_width = mean(fgarch_upper - fgarch_lower)
    )
  }
  
  # Store results for this series
  results[[series_name]] <- list(forecasts = forecasts, metrics = metrics)
  
  utils::setTxtProgressBar(pb, i)
}
close(pb)

# Create comprehensive summary table
summary_table <- data.frame()
for (series_name in names(results)) {
  for (method in names(results[[series_name]]$metrics)) {
    metrics <- results[[series_name]]$metrics[[method]]
    summary_table <- rbind(summary_table, data.frame(
      Series = series_name,
      Method = method,
      Winkler = metrics$winkler,
      Coverage = metrics$coverage,
      Interval_Width = metrics$interval_width
    ))
  }
}

print(summary_table)



# Median performance across all series
avg_performance <- aggregate(. ~ Method, data = summary_table[, -1], median)
print("\n=== MEDIAN PERFORMANCE ACROSS ALL SERIES ===")
print(avg_performance)

# Statistical significance testing
cat("\n=== STATISTICAL COMPARISON ===\n")
methods <- unique(summary_table$Method)
for (metric in c("Winkler", "Coverage")) {
  cat(paste("\n", metric, "comparison:\n"))
  for (i in 1:(length(methods)-1)) {
    for (j in (i+1):length(methods)) {
      m1 <- summary_table[summary_table$Method == methods[i], metric]
      m2 <- summary_table[summary_table$Method == methods[j], metric]
      test_result <- t.test(m1, m2, paired = TRUE)
      cat(sprintf("%s vs %s: p-value = %.4f%s\n",
                  methods[i], methods[j], test_result$p.value,
                  ifelse(test_result$p.value < 0.05, " *", "")))
    }
  }
}

