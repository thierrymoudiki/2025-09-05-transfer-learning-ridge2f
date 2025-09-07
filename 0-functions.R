# For Techtonique packages
options(repos = c(techtonique = "https://r-packages.techtonique.net",
                  CRAN = "https://cloud.r-project.org"))

install.packages(c('ahead', 'bayesianrvfl'))

library(forecast)

require(Mcomp)

coverage_score <- function(obj, actual){
  if (is.null(obj$lower))
  {
    return(mean((obj$intervals[, 1] <= actual)*(actual <= obj$intervals[, 2]), na.rm=TRUE)*100)
  }
  return(mean((obj$lower <= actual)*(actual <= obj$upper), na.rm=TRUE)*100)
}
#coverage_score <- memoise::memoise(coverage_score)

winkler_score <- function(obj, actual, level = 95) {
  alpha <- 1 - level / 100
  lt <- try(obj$lower, silent = TRUE)
  ut <- try(obj$upper, silent = TRUE)
  actual <- as.numeric(actual)
  if (is.null(lt) || is.null(ut))
  {
    lt <- as.numeric(obj$intervals[, 1])
    ut <- as.numeric(obj$intervals[, 2])
  }
  n_points <- length(actual)
  stopifnot((n_points == length(lt)) && (n_points == length(ut)))
  diff_lt <- lt - actual
  diff_bounds <- ut - lt
  diff_ut <- actual - ut
  score <- diff_bounds
  score <- score + (2 / alpha) * (pmax(diff_lt, 0) + pmax(diff_ut, 0))
  return(mean(score, na.rm=TRUE))
}
#winkler_score <- memoise::memoise(winkler_score)

accuracy <- function(obj, actual, level = 95)
{
  actual <- as.numeric(actual)
  mean_prediction <- as.numeric(obj$mean)
  me <- mean(mean_prediction - actual)
  rmse <- sqrt(mean((mean_prediction - actual)**2, na.rm=TRUE))
  mae <- mean(abs(mean_prediction - actual), na.rm=TRUE)
  mpe <- mean(mean_prediction/actual-1, na.rm=TRUE)
  mape <- mean(abs(mean_prediction/actual-1), na.rm=TRUE)
  m <- frequency(obj$x)   # e.g., 12 for monthly data
  # Compute scaling factor (MAE of in-sample seasonal naive forecasts)
  if (m > 1) {
    scale <- mean(abs(diff(obj$x, lag = m)), na.rm=TRUE)
  } else {
    scale <- mean(abs(diff(obj$x, lag = 1)), na.rm=TRUE)
  }
  # MASE = mean(|test - forecast|) / scale
  mase <- mean(abs(actual - obj$mean), na.rm=TRUE) / max(scale, 1e-6)
  coverage <- as.numeric(coverage_score(obj, actual))
  winkler <- winkler_score(obj, actual, level = level)
  crps <- mean(scoringRules::crps_sample(y, dat))
  res <- c(me, rmse, mae, mpe,
           mape, mase, coverage, winkler, crps)
  names(res) <- c("me", "rmse", "mae", "mpe",
                  "mape", "mase", "coverage", "winkler", "crps")
  return(res)
}
#accuracy <- memoise::memoise(accuracy)


library(data.table)

# Main synthetic returns generator function
generate_synthetic_returns <- function(
    n_days = 252 * 10,
    mu = 0.0002,
    kappa = 0.05,
    theta = 0.0001,
    sigma_v = 0.01,
    rho = -0.7,
    lambda_jump = 0.05,
    jump_size_dist = "normal",
    sigma_jump = 0.02,
    noise_dist = "normal",
    noise_scale = 0.0005,
    noise_df = 5.0,
    regime_params = NULL,
    random_seed = NULL
) {
  
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  # Default regime switching parameters
  if (is.null(regime_params)) {
    regime_params <- list(
      transition_matrix = matrix(c(0.99, 0.01, 0.03, 0.97), nrow = 2, byrow = TRUE),
      theta_high_multiplier = 3.0,
      kappa_high_multiplier = 2.0
    )
  }
  
  # Validate transition matrix
  if (!is.null(regime_params$transition_matrix)) {
    row_sums <- rowSums(regime_params$transition_matrix)
    if (!all(abs(row_sums - 1) < 1e-6)) {
      stop("Transition matrix rows must sum to 1.")
    }
  }
  
  # Initialize arrays
  n <- n_days
  v <- numeric(n)
  r <- numeric(n)
  regime <- integer(n)
  
  # Initialize starting values
  v[1] <- theta
  regime[1] <- 0
  
  # Pre-generate all random numbers
  z_vol <- rnorm(n)
  z_return <- rnorm(n)
  jump_indicators <- rpois(n, lambda = lambda_jump)
  
  # 1. Simulate the Markov chain for regimes
  for (t in 2:n) {
    prev_regime <- regime[t-1] + 1
    probs <- regime_params$transition_matrix[prev_regime, ]
    regime[t] <- sample(0:1, size = 1, prob = probs)
  }
  
  # 2. Simulate the Heston process with jumps
  for (t in 2:n) {
    current_regime <- regime[t]
    if (current_regime == 1) {
      theta_t <- theta * regime_params$theta_high_multiplier
      kappa_t <- kappa * regime_params$kappa_high_multiplier
    } else {
      theta_t <- theta
      kappa_t <- kappa
    }
    
    # Robust volatility discretization
    v_prev <- v[t-1]
    eta <- z_vol[t]
    
    drift <- kappa_t * (theta_t - max(v_prev, 0))
    volvol_term <- sigma_v * sqrt(max(v_prev, 0)) * eta
    v_new <- v_prev + drift + volvol_term
    v[t] <- max(v_new, 0)
    
    # Return process
    epsilon_t <- rho * eta + sqrt(1 - rho^2) * z_return[t]
    diffusion_component <- sqrt(max(v_prev, 0)) * epsilon_t
    
    # Jump process (single jump per period)
    J <- 0
    if (jump_indicators[t] > 0) {
      if (jump_size_dist == "normal") {
        J <- rnorm(1, mean = 0, sd = sigma_jump)
      } else if (jump_size_dist == "log_normal") {
        log_J <- rnorm(1, mean = -0.5 * sigma_jump^2, sd = sigma_jump)
        J <- exp(log_J) - 1
      } else if (jump_size_dist == "exponential") {
        sign <- sample(c(-1, 1), 1)
        J <- sign * rexp(1, rate = 1/sigma_jump)
      } else {
        stop("Invalid jump_size_dist.")
      }
    }
    
    r[t] <- mu + diffusion_component + J
  }
  
  # 3. Add microstructure noise
  if (noise_dist == "normal") {
    noise <- rnorm(n, mean = 0, sd = noise_scale)
  } else if (noise_dist == "student_t") {
    if (noise_df <= 2) stop("noise_df must be > 2")
    scale_factor <- noise_scale / sqrt(noise_df / (noise_df - 2))
    noise <- rt(n, df = noise_df) * scale_factor
  } else {
    stop("Invalid noise_dist.")
  }
  r <- r + noise
  
  # 4. Create output data.table
  dt <- data.table(
    date = seq.Date(as.Date("1970-01-01"), by = "day", length.out = n),
    returns = r,
    variance = v,
    volatility = sqrt(v),
    regime = factor(regime, levels = c(0, 1), labels = c("Low Vol", "High Vol"))
  )
  
  return(dt)
}

# Wrapper function for generating diverse paths
generate_diverse_sv_paths <- function(
    n_paths = 10000,
    horizon = 252 * 5,
    frequency = "daily",
    jump_type = "mixed",
    include_regime_switching = TRUE,
    random_seed = NULL
) {
  
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  all_paths <- vector("list", n_paths)
  all_params <- vector("list", n_paths)
  
  for (i in 1:n_paths) {
    
    params <- list()
    
    # Sample diverse parameters
    params$mu <- runif(1, -0.0005, 0.0005)
    params$kappa <- runif(1, 0.02, 0.15)
    params$theta <- runif(1, 5e-5, 3e-4)
    params$sigma_v <- runif(1, 0.005, 0.03)
    params$rho <- runif(1, -0.85, -0.4)
    
    # Jump parameters
    params$lambda_jump <- sample(c(
      runif(1, 0.005, 0.02),
      runif(1, 0.02, 0.08),
      runif(1, 0.08, 0.15)
    ), 1)
    
    if (jump_type == "mixed") {
      params$jump_size_dist <- sample(
        c("normal", "log_normal", "exponential"), 
        1,
        prob = c(0.4, 0.4, 0.2)
      )
    } else {
      params$jump_size_dist <- jump_type
    }
    
    params$sigma_jump <- runif(1, 0.01, 0.05)
    params$noise_dist <- sample(c("normal", "student_t"), 1, prob = c(0.7, 0.3))
    params$noise_scale <- runif(1, 1e-5, 2e-4)
    params$noise_df <- runif(1, 3, 8)
    
    # Regime switching parameters with FIXED transition matrices
    if (include_regime_switching) {
      regime_type <- sample(1:3, 1)
      
      if (regime_type == 1) {
        # Persistent regimes - FIXED: ensure rows sum to 1
        p11 <- runif(1, 0.97, 0.995)
        p12 <- 1 - p11
        p21 <- runif(1, 0.01, 0.05)
        p22 <- 1 - p21
        transition_matrix <- matrix(c(p11, p12, p21, p22), nrow = 2, byrow = TRUE)
      } else if (regime_type == 2) {
        # Mean-reverting regimes - FIXED: ensure rows sum to 1
        p11 <- runif(1, 0.85, 0.92)
        p12 <- 1 - p11
        p21 <- runif(1, 0.08, 0.15)
        p22 <- 1 - p21
        transition_matrix <- matrix(c(p11, p12, p21, p22), nrow = 2, byrow = TRUE)
      } else {
        # Rapid switching regimes - FIXED: ensure rows sum to 1
        p11 <- runif(1, 0.7, 0.8)
        p12 <- 1 - p11
        p21 <- runif(1, 0.2, 0.3)
        p22 <- 1 - p21
        transition_matrix <- matrix(c(p11, p12, p21, p22), nrow = 2, byrow = TRUE)
      }
      
      params$regime_params <- list(
        transition_matrix = transition_matrix,
        theta_high_multiplier = runif(1, 2.0, 5.0),
        kappa_high_multiplier = runif(1, 1.5, 3.0)
      )
    } else {
      params$regime_params <- NULL
    }
    
    # Generate the path
    path_data <- generate_synthetic_returns(
      n_days = horizon,
      mu = params$mu,
      kappa = params$kappa,
      theta = params$theta,
      sigma_v = params$sigma_v,
      rho = params$rho,
      lambda_jump = params$lambda_jump,
      jump_size_dist = params$jump_size_dist,
      sigma_jump = params$sigma_jump,
      noise_dist = params$noise_dist,
      noise_scale = params$noise_scale,
      noise_df = params$noise_df,
      regime_params = params$regime_params
    )
    
    all_paths[[i]] <- path_data$returns
    all_params[[i]] <- params
    
    if (i %% 1000 == 0) {
      message(sprintf("Generated %d/%d paths", i, n_paths))
    }
  }
  
  result <- list(
    paths = all_paths,
    parameters = all_params,
    horizon = horizon,
    frequency = frequency,
    n_paths = n_paths,
    generation_date = Sys.time()
  )
  
  class(result) <- "diverse_sv_paths"
  
  return(result)
}

# Summary method for the generated dataset
summary.diverse_sv_paths <- function(object, ...) {
  cat("Diverse Stochastic Volatility Paths Dataset\n")
  cat("===========================================\n")
  cat(sprintf("Number of paths: %d\n", object$n_paths))
  cat(sprintf("Horizon per path: %d days\n", object$horizon))
  cat(sprintf("Generation date: %s\n", object$generation_date))
  cat(sprintf("Total observations: %d\n", object$n_paths * object$horizon))
  
  # Sample statistics
  sample_paths <- sample(1:object$n_paths, min(100, object$n_paths))
  returns <- unlist(lapply(object$paths[sample_paths], function(x) x))
  
  cat("\nSummary statistics (sample):\n")
  cat(sprintf("Mean return: %.6f\n", mean(returns)))
  cat(sprintf("Return SD: %.6f\n", sd(returns)))
  cat(sprintf("Skewness: %.3f\n", moments::skewness(returns)))
  cat(sprintf("Kurtosis: %.3f\n", moments::kurtosis(returns)))
  cat(sprintf("Min return: %.6f\n", min(returns)))
  cat(sprintf("Max return: %.6f\n", max(returns)))
}

# Example usage (should work without errors now):
# set.seed(123)
# diverse_paths <- generate_diverse_sv_paths(n_paths = 10)
# summary(diverse_paths)
