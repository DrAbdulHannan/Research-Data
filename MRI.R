View(hpsa2)
str(hpsa2)
names(hpsa2)
# Load required libraries
library(tidyverse)
library(broom)
# Load required libraries
library(tidyverse)
library(broom)

# First, convert the data from wide to long format
# Identify quarter columns (all except 'State')
quarter_cols <- names(hpsa1)[names(hpsa1) != "State"]

# Convert to long format and create time variable
hpsa1_long <- hpsa1 %>%
  pivot_longer(cols = all_of(quarter_cols), 
               names_to = "Quarter", 
               values_to = "Value") %>%
  # Convert Value to numeric (fixing any character columns like Q42021)
  mutate(Value = as.numeric(Value)) %>%
  # Create time index (1, 2, 3, ... for each quarter)
  group_by(State) %>%
  mutate(Time = row_number()) %>%
  ungroup() %>%
  # Remove any rows with NA values
  filter(!is.na(Value))

# Function to perform regression and extract statistics
run_regression <- function(data) {
  # Check if we have enough data points (at least 2)
  if(nrow(data) < 2) {
    return(tibble(
      State = unique(data$State),
      Intercept = NA,
      Slope = NA,
      Beta = NA,
      R2 = NA,
      R2_Adj = NA,
      p_value = NA,
      Trend = "Insufficient data"
    ))
  }
  
  # Run linear regression
  model <- lm(Value ~ Time, data = data)
  
  # Extract statistics
  summary_model <- summary(model)
  
  # Get slope and its p-value
  slope <- coef(summary_model)[2, 1]
  p_value <- coef(summary_model)[2, 4]
  
  # Determine trend
  trend <- ifelse(p_value < 0.05, 
                  ifelse(slope > 0, "Increasing", "Decreasing"), 
                  "Stable")
  
  # Return results
  tibble(
    State = unique(data$State),
    Intercept = coef(model)[1],
    Slope = slope,
    Beta = slope,  # Beta is the same as slope in simple regression
    R2 = summary_model$r.squared,
    R2_Adj = summary_model$adj.r.squared,
    p_value = p_value,
    Trend = trend
  )
}

# Apply regression to each state
results <- hpsa1_long %>%
  group_by(State) %>%
  nest() %>%
  mutate(regression_results = map(data, run_regression)) %>%
  unnest(regression_results) %>%
  select(-data)

# View results
print(results)

# Export to CSV
write.csv(results, "state_regression_results.csv", row.names = FALSE)

# If you want to export to Excel
library(writexl)
write_xlsx(results, "state_regression_results.xlsx")

# Display summary of trends
cat("\nTrend Summary:\n")
print(table(results$Trend))












With Repalcing Missing Values

# Load required libraries
library(tidyverse)
library(broom)

# First, convert hpsa2 from wide to long format
# Identify quarter columns (all except 'State')
quarter_cols <- names(hpsa2)[names(hpsa2) != "State"]

# Convert to long format and create time variable
hpsa2_long <- hpsa2 %>%
  pivot_longer(cols = all_of(quarter_cols), 
               names_to = "Quarter", 
               values_to = "Value") %>%
  # Convert Value to numeric (in case any columns are character)
  mutate(Value = as.numeric(Value)) %>%
  # Create time index (1, 2, 3, ... for each quarter)
  group_by(State) %>%
  mutate(Time = row_number()) %>%
  ungroup() %>%
  # Remove any rows with NA values
  filter(!is.na(Value))

# Function to perform regression and extract statistics
run_regression <- function(data) {
  # Check if we have enough data points (at least 2)
  if(nrow(data) < 2) {
    return(tibble(
      State = unique(data$State),
      Intercept = NA,
      Slope = NA,
      Beta = NA,
      R2 = NA,
      R2_Adj = NA,
      p_value = NA,
      Trend = "Insufficient data"
    ))
  }
  
  # Run linear regression
  model <- lm(Value ~ Time, data = data)
  
  # Extract statistics
  summary_model <- summary(model)
  
  # Get slope and its p-value
  slope <- coef(summary_model)[2, 1]
  p_value <- coef(summary_model)[2, 4]
  
  # Determine trend
  trend <- ifelse(p_value < 0.05, 
                  ifelse(slope > 0, "Increasing", "Decreasing"), 
                  "Stable")
  
  # Return results
  tibble(
    State = unique(data$State),
    Intercept = coef(model)[1],
    Slope = slope,
    Beta = slope,  # Beta is the same as slope in simple regression
    R2 = summary_model$r.squared,
    R2_Adj = summary_model$adj.r.squared,
    p_value = p_value,
    Trend = trend
  )
}

# Apply regression to each state for hpsa2
results_hpsa2 <- hpsa2_long %>%
  group_by(State) %>%
  nest() %>%
  mutate(regression_results = map(data, run_regression)) %>%
  unnest(regression_results) %>%
  select(-data)

# View results
print(results_hpsa2)

# Export to CSV
write.csv(results_hpsa2, "hpsa2_regression_results.csv", row.names = FALSE)

# Export to Excel (requires writexl package)
library(writexl)
write_xlsx(results_hpsa2, "hpsa2_regression_results.xlsx")

# Display summary of trends
cat("\nTrend Summary for hpsa2:\n")
print(table(results_hpsa2$Trend))

# Show top 10 states by slope (fastest increasing)
cat("\nTop 10 states with highest increasing rate (Beta):\n")
results_hpsa2 %>%
  filter(Trend == "Increasing") %>%
  arrange(desc(Slope)) %>%
  select(State, Slope, R2, p_value) %>%
  head(10) %>%
  print()

# Show bottom 10 states by slope (fastest decreasing)
cat("\nBottom 10 states with most negative rate (Beta):\n")
results_hpsa2 %>%
  filter(Trend == "Decreasing") %>%
  arrange(Slope) %>%
  select(State, Slope, R2, p_value) %>%
  head(10) %>%
  print()

xxxxxxxxxxxxxxxxxxxxxx
Time Sreies---1


# Load required libraries
library(forecast)
library(tidyverse)
library(tseries)
library(writexl)
library(lubridate)


hpsa1_clean <- hpsa1


# Load required libraries
library(forecast)
library(tidyverse)
library(tseries)
library(writexl)
library(lubridate)

# FIRST: Clean and convert the data to numeric
# Make a copy of your data
hpsa1_clean <- hpsa1

# Convert all quarterly columns to numeric (except State column)
quarter_columns <- names(hpsa1_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  # Convert to character first, then numeric to catch any issues
  hpsa1_clean[[col]] <- as.numeric(as.character(hpsa1_clean[[col]]))
}

# Check if any conversion introduced NAs
cat("Checking for non-numeric values:\n")
for(col in quarter_columns) {
  na_count <- sum(is.na(hpsa1_clean[[col]]))
  if(na_count > 0) {
    cat(col, "has", na_count, "NA values\n")
    # Print original values that caused NA
    original_vals <- hpsa1[[col]][is.na(hpsa1_clean[[col]])]
    cat("  Problematic values:", unique(original_vals), "\n")
  }
}

# Remove any rows with NAs if necessary
# For now, we'll handle NAs in the time series creation

# Function to prepare time series from state data (with error handling)
prepare_state_ts <- function(state_data) {
  # Extract all quarterly values (columns 2 to end)
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  values <- values[!is.na(values)]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create time series (starting Q1 2018, quarterly frequency)
  ts_data <- ts(values, start = c(2018, 1), frequency = 4)
  return(ts_data)
}

# Function to get ARIMA model equation
get_arima_equation <- function(model) {
  order <- arimaorder(model)
  p <- order[1]
  d <- order[2]
  q <- order[3]
  P <- order[4]
  D <- order[5]
  Q <- order[6]
  period <- order[7]
  
  if(is.na(P) || P == 0 && D == 0 && Q == 0) {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  } else {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")[", period, "]")
  }
  return(eq)
}

# Function to extract full equation with coefficients
get_full_equation <- function(model) {
  coefs <- coef(model)
  if(length(coefs) == 0) return("No coefficients")
  
  model_type <- get_arima_equation(model)
  
  coef_parts <- c()
  for(i in 1:length(coefs)) {
    coef_parts <- c(coef_parts, paste0(names(coefs)[i], "=", round(coefs[i], 4)))
  }
  
  full_eq <- paste0(model_type, " [", paste(coef_parts, collapse = ", "), "]")
  return(full_eq)
}

# Function to extract intercept
get_intercept <- function(model) {
  if("intercept" %in% names(coef(model))) {
    return(round(coef(model)["intercept"], 4))
  } else if("drift" %in% names(coef(model))) {
    return(round(coef(model)["drift"], 4))
  } else {
    return(NA)
  }
}

# Function to calculate trend (simplified, avoiding complex operations)
calculate_trend <- function(ts_data, model) {
  # Use original time series instead of fitted values to avoid issues
  if(length(ts_data) < 2) {
    return(list(beta = NA, trend = "Insufficient data", p_value = NA))
  }
  
  time_points <- 1:length(ts_data)
  
  # Simple linear regression on original data
  trend_model <- lm(as.numeric(ts_data) ~ time_points)
  beta <- round(coef(trend_model)[2], 6)
  p_value_trend <- summary(trend_model)$coefficients[2,4]
  
  if(!is.na(p_value_trend) && p_value_trend < 0.05) {
    trend_direction <- ifelse(beta > 0, "Increasing", "Decreasing")
  } else {
    trend_direction <- "Stable (not significant)"
  }
  
  return(list(beta = beta, trend = trend_direction, p_value = p_value_trend))
}

# Main analysis function with better error handling
analyze_state_arima <- function(state_name, data_df) {
  tryCatch({
    cat("Processing:", state_name, "\n")
    
    # Get state data
    state_data <- data_df %>% filter(State == state_name)
    
    if(nrow(state_data) == 0) {
      cat("  No data for", state_name, "\n")
      return(NULL)
    }
    
    # Prepare time series
    ts_data <- prepare_state_ts(state_data)
    
    if(is.null(ts_data)) {
      cat("  Insufficient valid data for", state_name, "\n")
      return(NULL)
    }
    
    # Check if we have enough observations
    if(length(ts_data) < 6) {
      cat("  Too few observations (", length(ts_data), ") for", state_name, "\n")
      return(NULL)
    }
    
    # Fit auto.arima model with simplified parameters
    model <- tryCatch({
      auto.arima(ts_data, 
                 seasonal = TRUE,
                 stepwise = TRUE,  # Changed to TRUE for faster processing
                 approximation = TRUE,  # Changed to TRUE to avoid convergence issues
                 trace = FALSE,
                 allowdrift = TRUE,
                 allowmean = TRUE,
                 lambda = NULL)  # No Box-Cox transformation
    }, error = function(e) {
      cat("  auto.arima failed, trying simpler model...\n")
      # Try with non-seasonal model
      auto.arima(ts_data, 
                 seasonal = FALSE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 allowdrift = TRUE,
                 allowmean = TRUE)
    })
    
    if(is.null(model)) {
      cat("  Could not fit model for", state_name, "\n")
      return(NULL)
    }
    
    # Extract model information
    model_type <- get_arima_equation(model)
    full_equation <- get_full_equation(model)
    intercept <- get_intercept(model)
    
    # Calculate trend and beta
    trend_info <- calculate_trend(ts_data, model)
    beta <- trend_info$beta
    trend <- trend_info$trend
    trend_p_value <- trend_info$p_value
    
    # Model fit statistics
    residuals <- na.omit(residuals(model))
    fitted_vals <- na.omit(fitted(model))
    
    if(length(residuals) == 0 || length(fitted_vals) == 0) {
      cat("  No residuals/fitted values for", state_name, "\n")
      return(NULL)
    }
    
    n <- length(na.omit(ts_data))
    k <- max(1, length(coef(model)))
    
    # Calculate R-squared
    ss_res <- sum(residuals^2, na.rm = TRUE)
    ss_tot <- sum((ts_data - mean(ts_data, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- 1 - (ss_res / ss_tot)
    r2_adj <- 1 - (1 - r2) * ((n - 1) / (n - k - 1))
    
    # Calculate MAPE (avoid division by zero)
    mape <- tryCatch({
      mean(abs(residuals / ts_data), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # AIC and BIC
    aic <- tryCatch(AIC(model), error = function(e) NA)
    bic <- tryCatch(BIC(model), error = function(e) NA)
    
    # Ljung-Box test
    lb_test <- tryCatch({
      Box.test(residuals, lag = min(6, length(residuals)%/%4), type = "Ljung-Box")
    }, error = function(e) list(p.value = NA))
    p_value_lb <- ifelse(is.null(lb_test$p.value), NA, lb_test$p.value)
    
    # Historical average
    historical_avg <- round(mean(ts_data, na.rm = TRUE), 2)
    
    # Forecast for 2026-2030
    forecast_obj <- tryCatch({
      forecast(model, h = 20)
    }, error = function(e) {
      cat("  Forecasting failed for", state_name, "\n")
      return(NULL)
    })
    
    if(is.null(forecast_obj)) {
      forecast_2026 <- forecast_2027 <- forecast_2028 <- forecast_2029 <- forecast_2030 <- NA
    } else {
      forecast_2026 <- round(forecast_obj$mean[4], 2)
      forecast_2027 <- round(forecast_obj$mean[8], 2)
      forecast_2028 <- round(forecast_obj$mean[12], 2)
      forecast_2029 <- round(forecast_obj$mean[16], 2)
      forecast_2030 <- round(forecast_obj$mean[20], 2)
    }
    
    # Create results dataframe
    result <- data.frame(
      State = state_name,
      Model_Type = model_type,
      Full_Equation = full_equation,
      Intercept = ifelse(is.na(intercept), "None", as.character(intercept)),
      Beta_Rate_of_Change = beta,
      Trend_Significance_p = round(trend_p_value, 4),
      R_Squared = round(r2, 4),
      Adj_R_Squared = round(r2_adj, 4),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Ljung_Box_p_value = round(p_value_lb, 4),
      Average_2018_2025 = historical_avg,
      Projected_2026_Q4 = forecast_2026,
      Projected_2027_Q4 = forecast_2027,
      Projected_2028_Q4 = forecast_2028,
      Projected_2029_Q4 = forecast_2029,
      Projected_2030_Q4 = forecast_2030,
      Trend_Classification = trend,
      stringsAsFactors = FALSE
    )
    
    cat("  Successfully processed", state_name, "- Model:", model_type, "\n")
    return(result)
    
  }, error = function(e) {
    cat("  Error processing", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Clean the data first
cat("STEP 1: Cleaning and converting data to numeric...\n")
cat("===========================================\n")

hpsa1_clean <- hpsa1

# Convert all quarterly columns to numeric
quarter_columns <- names(hpsa1_clean)[-1]

for(col in quarter_columns) {
  hpsa1_clean[[col]] <- suppressWarnings(as.numeric(as.character(hpsa1_clean[[col]])))
}

# Check for successful conversion
cat("Data cleaning complete. Summary:\n")
cat("Total rows:", nrow(hpsa1_clean), "\n")
cat("Total columns:", ncol(hpsa1_clean), "\n\n")

# STEP 2: Run the analysis
cat("STEP 2: Running ARIMA analysis for all states...\n")
cat("===========================================\n")

results_list <- list()

# Get unique states (excluding any potential "Natioal" typo if exists)
states_to_process <- unique(hpsa1_clean$State)
states_to_process <- states_to_process[states_to_process != "Natioal"]  # Remove typo if present

for(state in states_to_process) {
  result <- analyze_state_arima(state, hpsa1_clean)
  if(!is.null(result)) {
    results_list[[state]] <- result
  }
}

# STEP 3: Combine and export results
cat("\nSTEP 3: Combining results and exporting...\n")
cat("===========================================\n")

if(length(results_list) > 0) {
  final_results <- bind_rows(results_list)
  
  # Sort by state name
  final_results <- final_results %>% arrange(State)
  
  # Display first few rows
  print(head(final_results))
  
  # Create summary statistics
  summary_stats <- data.frame(
    Metric = c("Total States Processed", 
               "Average R-squared", 
               "Average MAPE", 
               "Increasing Trend", 
               "Decreasing Trend", 
               "Stable Trend",
               "Successful Models"),
    Value = c(
      nrow(final_results),
      round(mean(final_results$R_Squared, na.rm = TRUE), 4),
      round(mean(final_results$MAPE_percent, na.rm = TRUE), 2),
      sum(final_results$Trend_Classification == "Increasing", na.rm = TRUE),
      sum(final_results$Trend_Classification == "Decreasing", na.rm = TRUE),
      sum(grepl("Stable", final_results$Trend_Classification), na.rm = TRUE),
      sum(!is.na(final_results$R_Squared))
    )
  )
  
  # Export to Excel
  write_xlsx(list(
    Detailed_Results = final_results,
    Summary_Statistics = summary_stats,
    Model_Distribution = as.data.frame(table(final_results$Model_Type))
  ), "ARIMA_Statewise_Analysis.xlsx")
  
  cat("\n✓ Analysis complete!\n")
  cat("✓ Results saved to 'ARIMA_Statewise_Analysis.xlsx'\n\n")
  
  print(summary_stats)
  
  # Print model type distribution
  cat("\nModel Types Distribution:\n")
  print(table(final_results$Model_Type))
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
  cat("  Make sure all quarterly columns contain numeric values.\n")
}

# STEP 4: Diagnostic information
cat("\nSTEP 4: Diagnostic Information\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list), "\n")
cat("Number of states that failed:", length(states_to_process) - length(results_list), "\n")

if(length(results_list) < length(states_to_process)) {
  failed_states <- setdiff(states_to_process, names(results_list))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
}

xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Load required libraries
library(forecast)
library(tidyverse)
library(tseries)
library(writexl)
library(lubridate)

# FIRST: Clean and convert the data to numeric
# Make a copy of your data
hpsa1_clean <- hpsa1

# Convert all quarterly columns to numeric (except State column)
quarter_columns <- names(hpsa1_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  # Convert to character first, then numeric to catch any issues
  hpsa1_clean[[col]] <- suppressWarnings(as.numeric(as.character(hpsa1_clean[[col]])))
}

# Check if any conversion introduced NAs
cat("Checking for non-numeric values:\n")
for(col in quarter_columns) {
  na_count <- sum(is.na(hpsa1_clean[[col]]))
  if(na_count > 0) {
    cat(col, "has", na_count, "NA values\n")
    # Print original values that caused NA
    original_vals <- hpsa1[[col]][is.na(hpsa1_clean[[col]])]
    cat("  Problematic values:", unique(original_vals), "\n")
  }
}

# Function to prepare time series from state data (with error handling)
prepare_state_ts <- function(state_data) {
  # Extract all quarterly values (columns 2 to end)
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  values <- values[!is.na(values)]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create time series (starting Q1 2018, quarterly frequency)
  ts_data <- ts(values, start = c(2018, 1), frequency = 4)
  return(ts_data)
}

# Function to get ARIMA model equation
get_arima_equation <- function(model) {
  order <- arimaorder(model)
  p <- order[1]
  d <- order[2]
  q <- order[3]
  P <- order[4]
  D <- order[5]
  Q <- order[6]
  period <- order[7]
  
  if(is.na(P) || (P == 0 && D == 0 && Q == 0)) {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  } else {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")[", period, "]")
  }
  return(eq)
}

# Function to extract full equation with coefficients
get_full_equation <- function(model) {
  coefs <- coef(model)
  if(length(coefs) == 0) return("No coefficients")
  
  model_type <- get_arima_equation(model)
  
  coef_parts <- c()
  for(i in 1:length(coefs)) {
    coef_parts <- c(coef_parts, paste0(names(coefs)[i], "=", round(coefs[i], 4)))
  }
  
  full_eq <- paste0(model_type, " [", paste(coef_parts, collapse = ", "), "]")
  return(full_eq)
}

# Function to extract intercept
get_intercept <- function(model) {
  if("intercept" %in% names(coef(model))) {
    return(round(coef(model)["intercept"], 4))
  } else if("drift" %in% names(coef(model))) {
    return(round(coef(model)["drift"], 4))
  } else {
    return(NA)
  }
}

# Function to calculate trend (simplified, avoiding complex operations)
calculate_trend <- function(ts_data, model) {
  # Use original time series instead of fitted values to avoid issues
  if(length(ts_data) < 2) {
    return(list(beta = NA, trend = "Insufficient data", p_value = NA))
  }
  
  time_points <- 1:length(ts_data)
  
  # Simple linear regression on original data
  trend_model <- lm(as.numeric(ts_data) ~ time_points)
  beta <- round(coef(trend_model)[2], 6)
  p_value_trend <- summary(trend_model)$coefficients[2,4]
  
  if(!is.na(p_value_trend) && p_value_trend < 0.05) {
    trend_direction <- ifelse(beta > 0, "Increasing", "Decreasing")
  } else {
    trend_direction <- "Stable (not significant)"
  }
  
  return(list(beta = beta, trend = trend_direction, p_value = p_value_trend))
}

# Main analysis function with better error handling
analyze_state_arima <- function(state_name, data_df) {
  tryCatch({
    cat("Processing:", state_name, "\n")
    
    # Get state data
    state_data <- data_df %>% filter(State == state_name)
    
    if(nrow(state_data) == 0) {
      cat("  No data for", state_name, "\n")
      return(NULL)
    }
    
    # Prepare time series
    ts_data <- prepare_state_ts(state_data)
    
    if(is.null(ts_data)) {
      cat("  Insufficient valid data for", state_name, "\n")
      return(NULL)
    }
    
    # Check if we have enough observations
    if(length(ts_data) < 6) {
      cat("  Too few observations (", length(ts_data), ") for", state_name, "\n")
      return(NULL)
    }
    
    # Fit auto.arima model with simplified parameters
    model <- tryCatch({
      auto.arima(ts_data, 
                 seasonal = TRUE,
                 stepwise = TRUE,  # Changed to TRUE for faster processing
                 approximation = TRUE,  # Changed to TRUE to avoid convergence issues
                 trace = FALSE,
                 allowdrift = TRUE,
                 allowmean = TRUE,
                 lambda = NULL)  # No Box-Cox transformation
    }, error = function(e) {
      cat("  auto.arima failed, trying simpler model...\n")
      # Try with non-seasonal model
      auto.arima(ts_data, 
                 seasonal = FALSE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 allowdrift = TRUE,
                 allowmean = TRUE)
    })
    
    if(is.null(model)) {
      cat("  Could not fit model for", state_name, "\n")
      return(NULL)
    }
    
    # Extract model information
    model_type <- get_arima_equation(model)
    full_equation <- get_full_equation(model)
    intercept <- get_intercept(model)
    
    # Calculate trend and beta
    trend_info <- calculate_trend(ts_data, model)
    beta <- trend_info$beta
    trend <- trend_info$trend
    trend_p_value <- trend_info$p_value
    
    # Model fit statistics
    residuals <- na.omit(residuals(model))
    fitted_vals <- na.omit(fitted(model))
    
    if(length(residuals) == 0 || length(fitted_vals) == 0) {
      cat("  No residuals/fitted values for", state_name, "\n")
      return(NULL)
    }
    
    n <- length(na.omit(ts_data))
    k <- max(1, length(coef(model)))
    
    # Calculate R-squared
    ss_res <- sum(residuals^2, na.rm = TRUE)
    ss_tot <- sum((ts_data - mean(ts_data, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- 1 - (ss_res / ss_tot)
    r2_adj <- 1 - (1 - r2) * ((n - 1) / (n - k - 1))
    
    # Calculate MAPE (avoid division by zero)
    mape <- tryCatch({
      mean(abs(residuals / ts_data), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # AIC and BIC
    aic <- tryCatch(AIC(model), error = function(e) NA)
    bic <- tryCatch(BIC(model), error = function(e) NA)
    
    # Ljung-Box test
    lb_test <- tryCatch({
      Box.test(residuals, lag = min(6, length(residuals)%/%4), type = "Ljung-Box")
    }, error = function(e) list(p.value = NA))
    p_value_lb <- ifelse(is.null(lb_test$p.value), NA, lb_test$p.value)
    
    # Historical average
    historical_avg <- round(mean(ts_data, na.rm = TRUE), 2)
    
    # Forecast for 2026-2030
    forecast_obj <- tryCatch({
      forecast(model, h = 20)
    }, error = function(e) {
      cat("  Forecasting failed for", state_name, "\n")
      return(NULL)
    })
    
    if(is.null(forecast_obj)) {
      forecast_2026 <- forecast_2027 <- forecast_2028 <- forecast_2029 <- forecast_2030 <- NA
    } else {
      forecast_2026 <- round(forecast_obj$mean[4], 2)
      forecast_2027 <- round(forecast_obj$mean[8], 2)
      forecast_2028 <- round(forecast_obj$mean[12], 2)
      forecast_2029 <- round(forecast_obj$mean[16], 2)
      forecast_2030 <- round(forecast_obj$mean[20], 2)
    }
    
    # Create results dataframe
    result <- data.frame(
      State = state_name,
      Model_Type = model_type,
      Full_Equation = full_equation,
      Intercept = ifelse(is.na(intercept), "None", as.character(intercept)),
      Beta_Rate_of_Change = beta,
      Trend_Significance_p = round(trend_p_value, 4),
      R_Squared = round(r2, 4),
      Adj_R_Squared = round(r2_adj, 4),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Ljung_Box_p_value = round(p_value_lb, 4),
      Average_2018_2025 = historical_avg,
      Projected_2026_Q4 = forecast_2026,
      Projected_2027_Q4 = forecast_2027,
      Projected_2028_Q4 = forecast_2028,
      Projected_2029_Q4 = forecast_2029,
      Projected_2030_Q4 = forecast_2030,
      Trend_Classification = trend,
      stringsAsFactors = FALSE
    )
    
    cat("  Successfully processed", state_name, "- Model:", model_type, "\n")
    return(result)
    
  }, error = function(e) {
    cat("  Error processing", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Clean the data first
cat("STEP 1: Cleaning and converting data to numeric...\n")
cat("===========================================\n")

hpsa1_clean <- hpsa1

# Convert all quarterly columns to numeric
quarter_columns <- names(hpsa1_clean)[-1]

for(col in quarter_columns) {
  hpsa1_clean[[col]] <- suppressWarnings(as.numeric(as.character(hpsa1_clean[[col]])))
}

# Check for successful conversion
cat("Data cleaning complete. Summary:\n")
cat("Total rows:", nrow(hpsa1_clean), "\n")
cat("Total columns:", ncol(hpsa1_clean), "\n\n")

# STEP 2: Run the analysis
cat("STEP 2: Running ARIMA analysis for all states...\n")
cat("===========================================\n")

results_list <- list()

# Get unique states - include ALL states including "Natioal"
states_to_process <- unique(hpsa1_clean$State)

# Make sure "Natioal" is included (fix typo if needed)
if("Natioal" %in% states_to_process) {
  cat("Note: Including 'Natioal' (National) in analysis\n")
}

for(state in states_to_process) {
  result <- analyze_state_arima(state, hpsa1_clean)
  if(!is.null(result)) {
    results_list[[state]] <- result
  }
}

# STEP 3: Combine and export results
cat("\nSTEP 3: Combining results and exporting...\n")
cat("===========================================\n")

if(length(results_list) > 0) {
  final_results <- bind_rows(results_list)
  
  # Sort by state name (Natioal will appear alphabetically)
  final_results <- final_results %>% arrange(State)
  
  # Display first few rows
  print(head(final_results, 10))
  
  # Create summary statistics (including Natioal)
  summary_stats <- data.frame(
    Metric = c("Total States Processed", 
               "Average R-squared", 
               "Average MAPE", 
               "Increasing Trend", 
               "Decreasing Trend", 
               "Stable Trend",
               "Successful Models"),
    Value = c(
      nrow(final_results),
      round(mean(final_results$R_Squared, na.rm = TRUE), 4),
      round(mean(final_results$MAPE_percent, na.rm = TRUE), 2),
      sum(final_results$Trend_Classification == "Increasing", na.rm = TRUE),
      sum(final_results$Trend_Classification == "Decreasing", na.rm = TRUE),
      sum(grepl("Stable", final_results$Trend_Classification), na.rm = TRUE),
      sum(!is.na(final_results$R_Squared))
    )
  )
  
  # Create a separate sheet for National summary
  national_data <- final_results %>% filter(State == "Natioal")
  
  if(nrow(national_data) > 0) {
    cat("\n✓ National (Natioal) analysis completed successfully\n")
  }
  
  # Export to Excel with multiple sheets
  write_xlsx(list(
    Detailed_Results = final_results,
    Summary_Statistics = summary_stats,
    National_Summary = national_data,
    Model_Distribution = as.data.frame(table(final_results$Model_Type)),
    Trend_Analysis = final_results %>% 
      group_by(Trend_Classification) %>% 
      summarise(Count = n(), 
                Average_R2 = mean(R_Squared, na.rm = TRUE),
                Average_MAPE = mean(MAPE_percent, na.rm = TRUE))
  ), "ARIMA_Statewise_Analysis_with_National.xlsx")
  
  cat("\n✓ Analysis complete!\n")
  cat("✓ Results saved to 'ARIMA_Statewise_Analysis_with_National.xlsx'\n\n")
  
  print(summary_stats)
  
  # Print National results separately
  if(nrow(national_data) > 0) {
    cat("\n=== NATIONAL (Natioal) RESULTS ===\n")
    print(national_data)
  }
  
  # Print model type distribution
  cat("\nModel Types Distribution:\n")
  print(table(final_results$Model_Type))
  
  # Create visualization-ready summary
  cat("\nGenerating additional summary...\n")
  
  # Create a summary by trend type
  trend_summary <- final_results %>%
    group_by(Trend_Classification) %>%
    summarise(
      Number_of_States = n(),
      Average_R2 = round(mean(R_Squared, na.rm = TRUE), 4),
      Average_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2),
      Average_Projected_2030 = round(mean(Projected_2030_Q4, na.rm = TRUE), 2)
    )
  
  print(trend_summary)
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
  cat("  Make sure all quarterly columns contain numeric values.\n")
}

# STEP 4: Diagnostic information
cat("\nSTEP 4: Diagnostic Information\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list), "\n")
cat("Number of states that failed:", length(states_to_process) - length(results_list), "\n")

if(length(results_list) < length(states_to_process)) {
  failed_states <- setdiff(states_to_process, names(results_list))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
} else {
  cat("\n✓ All states (including National) were processed successfully!\n")
}

# Optional: Create a comparative plot for National vs Selected States
cat("\nSTEP 5: Creating comparative analysis (optional)...\n")
cat("===========================================\n")

# Function to create a comparison chart for National vs Top 5 states
create_comparison_chart <- function(results_df) {
  if(nrow(results_df) > 0) {
    # Select National and top/bottom states for comparison
    compare_states <- c("Natioal", "California", "Texas", "New York", "Florida", "Alaska")
    compare_data <- results_df %>% filter(State %in% compare_states)
    
    if(nrow(compare_data) > 0) {
      # Save comparison as PNG
      png(filename = "State_Comparison_National_vs_States.png", 
          width = 1200, height = 800)
      
      par(mfrow = c(2, 2))
      
      # R-squared comparison
      barplot(compare_data$R_Squared, 
              names.arg = compare_data$State,
              main = "R-squared Comparison",
              ylab = "R-squared",
              col = ifelse(compare_data$State == "Natioal", "red", "steelblue"),
              las = 2)
      
      # MAPE comparison
      barplot(compare_data$MAPE_percent, 
              names.arg = compare_data$State,
              main = "MAPE Comparison (%)",
              ylab = "MAPE (%)",
              col = ifelse(compare_data$State == "Natioal", "red", "steelblue"),
              las = 2)
      
      # Projected 2030 values
      barplot(compare_data$Projected_2030_Q4, 
              names.arg = compare_data$State,
              main = "Projected Values for 2030",
              ylab = "Projected Value",
              col = ifelse(compare_data$State == "Natioal", "red", "steelblue"),
              las = 2)
      
      # Historical Average
      barplot(compare_data$Average_2018_2025, 
              names.arg = compare_data$State,
              main = "Historical Average (2018-2025)",
              ylab = "Average Value",
              col = ifelse(compare_data$State == "Natioal", "red", "steelblue"),
              las = 2)
      
      dev.off()
      cat("✓ Comparison chart saved as 'State_Comparison_National_vs_States.png'\n")
    }
  }
}

# Create comparison chart
create_comparison_chart(final_results)

cat("\n===========================================\n")
cat("Analysis Complete! Check the Excel file and PNG chart.\n")


xxxxxxxxxxxxxxxxxxxxxxxxxxxx
by replacing missing values



# Load required libraries
library(forecast)
library(tidyverse)
library(tseries)
library(writexl)
library(lubridate)

# FIRST: Clean and convert the data to numeric
cat("===========================================\n")
cat("ARIMA ANALYSIS FOR hpsa2 DATASET\n")
cat("===========================================\n\n")

cat("STEP 1: Cleaning and converting data to numeric...\n")
cat("-------------------------------------------\n")

hpsa2_clean <- hpsa2

# Convert all quarterly columns to numeric (except State column)
quarter_columns <- names(hpsa2_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  hpsa2_clean[[col]] <- suppressWarnings(as.numeric(as.character(hpsa2_clean[[col]])))
}

# Check for successful conversion
cat("Data cleaning complete. Summary:\n")
cat("Total rows:", nrow(hpsa2_clean), "\n")
cat("Total columns:", ncol(hpsa2_clean), "\n")
cat("Number of quarters:", length(quarter_columns), "\n\n")

# Function to prepare time series from state data
prepare_state_ts <- function(state_data) {
  # Extract all quarterly values (columns 2 to end)
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  values <- values[!is.na(values)]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create time series (starting Q1 2018, frequency 4)
  ts_data <- ts(values, start = c(2018, 1), frequency = 4)
  return(ts_data)
}

# Function to get ARIMA model equation
get_arima_equation <- function(model) {
  order <- arimaorder(model)
  p <- order[1]
  d <- order[2]
  q <- order[3]
  P <- order[4]
  D <- order[5]
  Q <- order[6]
  period <- order[7]
  
  if(is.na(P) || (P == 0 && D == 0 && Q == 0)) {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  } else {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")[", period, "]")
  }
  return(eq)
}

# Function to extract full equation with coefficients
get_full_equation <- function(model) {
  coefs <- coef(model)
  if(length(coefs) == 0) return("No coefficients")
  
  model_type <- get_arima_equation(model)
  
  coef_parts <- c()
  for(i in 1:length(coefs)) {
    coef_parts <- c(coef_parts, paste0(names(coefs)[i], "=", round(coefs[i], 4)))
  }
  
  full_eq <- paste0(model_type, " [", paste(coef_parts, collapse = ", "), "]")
  return(full_eq)
}

# Function to extract intercept
get_intercept <- function(model) {
  if("intercept" %in% names(coef(model))) {
    return(round(coef(model)["intercept"], 4))
  } else if("drift" %in% names(coef(model))) {
    return(round(coef(model)["drift"], 4))
  } else {
    return(NA)
  }
}

# Function to calculate trend
calculate_trend <- function(ts_data, model) {
  if(length(ts_data) < 2) {
    return(list(beta = NA, trend = "Insufficient data", p_value = NA))
  }
  
  time_points <- 1:length(ts_data)
  
  # Simple linear regression on original data
  trend_model <- lm(as.numeric(ts_data) ~ time_points)
  beta <- round(coef(trend_model)[2], 6)
  p_value_trend <- summary(trend_model)$coefficients[2,4]
  
  if(!is.na(p_value_trend) && p_value_trend < 0.05) {
    trend_direction <- ifelse(beta > 0, "Increasing", "Decreasing")
  } else {
    trend_direction <- "Stable (not significant)"
  }
  
  return(list(beta = beta, trend = trend_direction, p_value = p_value_trend))
}

# Main analysis function
analyze_state_arima <- function(state_name, data_df) {
  tryCatch({
    cat("Processing:", state_name, "\n")
    
    # Get state data
    state_data <- data_df %>% filter(State == state_name)
    
    if(nrow(state_data) == 0) {
      cat("  No data for", state_name, "\n")
      return(NULL)
    }
    
    # Prepare time series
    ts_data <- prepare_state_ts(state_data)
    
    if(is.null(ts_data)) {
      cat("  Insufficient valid data for", state_name, "\n")
      return(NULL)
    }
    
    # Check if we have enough observations
    if(length(ts_data) < 6) {
      cat("  Too few observations (", length(ts_data), ") for", state_name, "\n")
      return(NULL)
    }
    
    # Fit auto.arima model
    model <- tryCatch({
      auto.arima(ts_data, 
                 seasonal = TRUE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 trace = FALSE,
                 allowdrift = TRUE,
                 allowmean = TRUE,
                 lambda = NULL)
    }, error = function(e) {
      cat("  auto.arima failed, trying simpler model...\n")
      auto.arima(ts_data, 
                 seasonal = FALSE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 allowdrift = TRUE,
                 allowmean = TRUE)
    })
    
    if(is.null(model)) {
      cat("  Could not fit model for", state_name, "\n")
      return(NULL)
    }
    
    # Extract model information
    model_type <- get_arima_equation(model)
    full_equation <- get_full_equation(model)
    intercept <- get_intercept(model)
    
    # Calculate trend and beta
    trend_info <- calculate_trend(ts_data, model)
    beta <- trend_info$beta
    trend <- trend_info$trend
    trend_p_value <- trend_info$p_value
    
    # Model fit statistics
    residuals <- na.omit(residuals(model))
    
    if(length(residuals) == 0) {
      cat("  No residuals for", state_name, "\n")
      return(NULL)
    }
    
    n <- length(na.omit(ts_data))
    k <- max(1, length(coef(model)))
    
    # Calculate R-squared
    ss_res <- sum(residuals^2, na.rm = TRUE)
    ss_tot <- sum((ts_data - mean(ts_data, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- 1 - (ss_res / ss_tot)
    r2_adj <- 1 - (1 - r2) * ((n - 1) / (n - k - 1))
    
    # Calculate MAPE
    mape <- tryCatch({
      mean(abs(residuals / ts_data), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # AIC and BIC
    aic <- tryCatch(AIC(model), error = function(e) NA)
    bic <- tryCatch(BIC(model), error = function(e) NA)
    
    # Ljung-Box test
    lb_test <- tryCatch({
      Box.test(residuals, lag = min(12, length(residuals)%/%4), type = "Ljung-Box")
    }, error = function(e) list(p.value = NA))
    p_value_lb <- ifelse(is.null(lb_test$p.value), NA, lb_test$p.value)
    
    # Historical average
    historical_avg <- round(mean(ts_data, na.rm = TRUE), 2)
    
    # Forecast for 2026-2030 (5 years = 20 quarters ahead)
    forecast_obj <- tryCatch({
      forecast(model, h = 20)
    }, error = function(e) {
      cat("  Forecasting failed for", state_name, "\n")
      return(NULL)
    })
    
    if(is.null(forecast_obj)) {
      forecast_2026 <- forecast_2027 <- forecast_2028 <- forecast_2029 <- forecast_2030 <- NA
    } else {
      forecast_2026 <- round(forecast_obj$mean[4], 2)   # Q4 2026
      forecast_2027 <- round(forecast_obj$mean[8], 2)   # Q4 2027
      forecast_2028 <- round(forecast_obj$mean[12], 2)  # Q4 2028
      forecast_2029 <- round(forecast_obj$mean[16], 2)  # Q4 2029
      forecast_2030 <- round(forecast_obj$mean[20], 2)  # Q4 2030
    }
    
    # Create results dataframe
    result <- data.frame(
      State = state_name,
      Model_Type = model_type,
      Full_Equation = full_equation,
      Intercept = ifelse(is.na(intercept), "None", as.character(intercept)),
      Beta_Rate_of_Change = beta,
      Trend_Significance_p = round(trend_p_value, 4),
      R_Squared = round(r2, 4),
      Adj_R_Squared = round(r2_adj, 4),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Ljung_Box_p_value = round(p_value_lb, 4),
      Average_2018_2025 = historical_avg,
      Projected_2026_Q4 = forecast_2026,
      Projected_2027_Q4 = forecast_2027,
      Projected_2028_Q4 = forecast_2028,
      Projected_2029_Q4 = forecast_2029,
      Projected_2030_Q4 = forecast_2030,
      Trend_Classification = trend,
      stringsAsFactors = FALSE
    )
    
    cat("  ✓ Successfully processed", state_name, "- Model:", model_type, "\n")
    return(result)
    
  }, error = function(e) {
    cat("  ✗ Error processing", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Run the analysis for hpsa2
cat("STEP 2: Running ARIMA analysis for all states (hpsa2)...\n")
cat("-------------------------------------------\n")

results_list_hpsa2 <- list()

# Get unique states
states_to_process <- unique(hpsa2_clean$State)

for(state in states_to_process) {
  result <- analyze_state_arima(state, hpsa2_clean)
  if(!is.null(result)) {
    results_list_hpsa2[[state]] <- result
  }
}

# Combine results
cat("\nSTEP 3: Combining results...\n")
cat("-------------------------------------------\n")

if(length(results_list_hpsa2) > 0) {
  final_results_hpsa2 <- bind_rows(results_list_hpsa2)
  
  # Sort by state name
  final_results_hpsa2 <- final_results_hpsa2 %>% arrange(State)
  
  # Display first few rows
  cat("\nFirst 10 rows of results:\n")
  print(head(final_results_hpsa2, 10))
  
  # Create summary statistics
  summary_stats_hpsa2 <- data.frame(
    Metric = c("Total States Processed", 
               "Average R-squared", 
               "Average MAPE", 
               "Increasing Trend", 
               "Decreasing Trend", 
               "Stable Trend",
               "Successful Models",
               "Number of Quarters"),
    Value = c(
      nrow(final_results_hpsa2),
      round(mean(final_results_hpsa2$R_Squared, na.rm = TRUE), 4),
      round(mean(final_results_hpsa2$MAPE_percent, na.rm = TRUE), 2),
      sum(final_results_hpsa2$Trend_Classification == "Increasing", na.rm = TRUE),
      sum(final_results_hpsa2$Trend_Classification == "Decreasing", na.rm = TRUE),
      sum(grepl("Stable", final_results_hpsa2$Trend_Classification), na.rm = TRUE),
      sum(!is.na(final_results_hpsa2$R_Squared)),
      length(quarter_columns)
    )
  )
  
  # Create model distribution
  model_distribution <- as.data.frame(table(final_results_hpsa2$Model_Type))
  colnames(model_distribution) <- c("Model_Type", "Count")
  model_distribution <- model_distribution %>% arrange(desc(Count))
  
  # Create trend analysis summary
  trend_summary <- final_results_hpsa2 %>%
    group_by(Trend_Classification) %>%
    summarise(
      Number_of_States = n(),
      Average_R2 = round(mean(R_Squared, na.rm = TRUE), 4),
      Average_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2),
      Average_Projected_2030 = round(mean(Projected_2030_Q4, na.rm = TRUE), 2),
      Min_Value = round(min(Average_2018_2025, na.rm = TRUE), 2),
      Max_Value = round(max(Average_2018_2025, na.rm = TRUE), 2)
    )
  
  # Create a summary of top/bottom states by R-squared
  best_fit_states <- final_results_hpsa2 %>%
    arrange(desc(R_Squared)) %>%
    select(State, R_Squared, MAPE_percent, Model_Type) %>%
    head(10)
  
  worst_fit_states <- final_results_hpsa2 %>%
    arrange(R_Squared) %>%
    select(State, R_Squared, MAPE_percent, Model_Type) %>%
    head(10)
  
  # Export to Excel with multiple sheets
  cat("\nSTEP 4: Exporting to Excel...\n")
  cat("-------------------------------------------\n")
  
  write_xlsx(list(
    Detailed_Results = final_results_hpsa2,
    Summary_Statistics = summary_stats_hpsa2,
    Model_Distribution = model_distribution,
    Trend_Analysis = trend_summary,
    Best_Fit_States = best_fit_states,
    Worst_Fit_States = worst_fit_states,
    Forecast_Summary = final_results_hpsa2 %>%
      select(State, Projected_2026_Q4, Projected_2027_Q4, 
             Projected_2028_Q4, Projected_2029_Q4, Projected_2030_Q4, Trend_Classification)
  ), "ARIMA_hpsa2_Analysis_Results.xlsx")
  
  cat("\n✓ Analysis complete for hpsa2!\n")
  cat("✓ Results saved to 'ARIMA_hpsa2_Analysis_Results.xlsx'\n\n")
  
  # Print summary
  cat("===========================================\n")
  cat("SUMMARY STATISTICS (hpsa2)\n")
  cat("===========================================\n")
  print(summary_stats_hpsa2)
  
  cat("\n===========================================\n")
  cat("TOP 10 MODEL TYPES\n")
  cat("===========================================\n")
  print(head(model_distribution, 10))
  
  cat("\n===========================================\n")
  cat("TREND ANALYSIS SUMMARY\n")
  cat("===========================================\n")
  print(trend_summary)
  
  cat("\n===========================================\n")
  cat("TOP 10 BEST FITTING STATES (Highest R²)\n")
  cat("===========================================\n")
  print(best_fit_states)
  
  cat("\n===========================================\n")
  cat("TOP 10 WORST FITTING STATES (Lowest R²)\n")
  cat("===========================================\n")
  print(worst_fit_states)
  
  # Create visualization
  cat("\nSTEP 5: Creating visualizations...\n")
  cat("-------------------------------------------\n")
  
  # Create comparison charts
  png(filename = "hpsa2_analysis_summary.png", width = 1200, height = 800)
  par(mfrow = c(2, 2))
  
  # R-squared distribution
  hist(final_results_hpsa2$R_Squared, 
       main = "Distribution of R-squared Values",
       xlab = "R-squared", 
       col = "steelblue",
       breaks = 20)
  abline(v = mean(final_results_hpsa2$R_Squared, na.rm = TRUE), 
         col = "red", lwd = 2, lty = 2)
  
  # MAPE distribution
  hist(final_results_hpsa2$MAPE_percent, 
       main = "Distribution of MAPE Values",
       xlab = "MAPE (%)", 
       col = "lightgreen",
       breaks = 20)
  abline(v = mean(final_results_hpsa2$MAPE_percent, na.rm = TRUE), 
         col = "red", lwd = 2, lty = 2)
  
  # Trend classification pie chart
  trend_counts <- table(final_results_hpsa2$Trend_Classification)
  pie(trend_counts, 
      main = "Trend Classification",
      col = c("green", "red", "gray"))
  
  # Model types bar plot (top 10)
  top_models <- head(model_distribution, 10)
  barplot(top_models$Count, 
          names.arg = top_models$Model_Type,
          main = "Top 10 Model Types",
          ylab = "Count",
          col = "steelblue",
          las = 2)
  
  dev.off()
  cat("✓ Visualization saved as 'hpsa2_analysis_summary.png'\n")
  
  # Create forecast comparison for selected states
  selected_states <- c("Alabama", "California", "Texas", "New York", "Florida")
  forecast_comparison <- final_results_hpsa2 %>%
    filter(State %in% selected_states) %>%
    select(State, Projected_2026_Q4, Projected_2027_Q4, 
           Projected_2028_Q4, Projected_2029_Q4, Projected_2030_Q4)
  
  # Reshape for plotting
  forecast_long <- forecast_comparison %>%
    pivot_longer(cols = -State, names_to = "Year", values_to = "Projected_Value")
  
  png(filename = "hpsa2_forecast_comparison.png", width = 1000, height = 600)
  library(ggplot2)
  p <- ggplot(forecast_long, aes(x = Year, y = Projected_Value, color = State, group = State)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(title = "Forecast Comparison for Selected States (hpsa2)",
         x = "Year", y = "Projected Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
  dev.off()
  cat("✓ Forecast comparison saved as 'hpsa2_forecast_comparison.png'\n")
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
}

# Diagnostic information
cat("\n===========================================\n")
cat("DIAGNOSTIC INFORMATION\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list_hpsa2), "\n")
cat("Number of states in dataset:", length(states_to_process), "\n")
cat("Success rate:", round(length(results_list_hpsa2)/length(states_to_process)*100, 1), "%\n")

if(length(results_list_hpsa2) < length(states_to_process)) {
  failed_states <- setdiff(states_to_process, names(results_list_hpsa2))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
} else {
  cat("\n✓ All states were processed successfully!\n")
}

cat("\n===========================================\n")
cat("ANALYSIS COMPLETE FOR hpsa2!\n")
cat("===========================================\n")





























# Load required libraries
library(forecast)
library(tidyverse)
library(tseries)
library(writexl)
library(lubridate)
library(ggplot2)

# FIRST: Clean and convert the data to numeric
cat("===========================================\n")
cat("ARIMA ANALYSIS FOR hpsa2 DATASET (with National)\n")
cat("===========================================\n\n")

cat("STEP 1: Cleaning and converting data to numeric...\n")
cat("-------------------------------------------\n")

hpsa2_clean <- hpsa2

# Convert all quarterly columns to numeric (except State column)
quarter_columns <- names(hpsa2_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  hpsa2_clean[[col]] <- suppressWarnings(as.numeric(as.character(hpsa2_clean[[col]])))
}

# Check for National row
cat("Checking for National/National row...\n")
unique_states <- unique(hpsa2_clean$State)
if("National" %in% unique_states) {
  cat("✓ Found 'National' row in data\n")
} else if("Natioal" %in% unique_states) {
  cat("✓ Found 'Natioal' row (likely typo for National)\n")
} else {
  cat("Note: No National row found. Adding it if data exists...\n")
}

# Data cleaning summary
cat("\nData cleaning complete. Summary:\n")
cat("Total rows:", nrow(hpsa2_clean), "\n")
cat("Total columns:", ncol(hpsa2_clean), "\n")
cat("Number of quarters:", length(quarter_columns), "\n")
cat("States included:", paste(unique(hpsa2_clean$State), collapse = ", "), "\n\n")

# Function to prepare time series from state data
prepare_state_ts <- function(state_data) {
  # Extract all quarterly values (columns 2 to end)
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  values <- values[!is.na(values)]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create time series (starting Q1 2018, frequency 4)
  ts_data <- ts(values, start = c(2018, 1), frequency = 4)
  return(ts_data)
}

# Function to get ARIMA model equation
get_arima_equation <- function(model) {
  order <- arimaorder(model)
  p <- order[1]
  d <- order[2]
  q <- order[3]
  P <- order[4]
  D <- order[5]
  Q <- order[6]
  period <- order[7]
  
  if(is.na(P) || (P == 0 && D == 0 && Q == 0)) {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  } else {
    eq <- paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")[", period, "]")
  }
  return(eq)
}

# Function to extract full equation with coefficients
get_full_equation <- function(model) {
  coefs <- coef(model)
  if(length(coefs) == 0) return("No coefficients")
  
  model_type <- get_arima_equation(model)
  
  coef_parts <- c()
  for(i in 1:length(coefs)) {
    coef_parts <- c(coef_parts, paste0(names(coefs)[i], "=", round(coefs[i], 4)))
  }
  
  full_eq <- paste0(model_type, " [", paste(coef_parts, collapse = ", "), "]")
  return(full_eq)
}

# Function to extract intercept
get_intercept <- function(model) {
  if("intercept" %in% names(coef(model))) {
    return(round(coef(model)["intercept"], 4))
  } else if("drift" %in% names(coef(model))) {
    return(round(coef(model)["drift"], 4))
  } else {
    return(NA)
  }
}

# Function to calculate trend
calculate_trend <- function(ts_data, model) {
  if(length(ts_data) < 2) {
    return(list(beta = NA, trend = "Insufficient data", p_value = NA))
  }
  
  time_points <- 1:length(ts_data)
  
  # Simple linear regression on original data
  trend_model <- lm(as.numeric(ts_data) ~ time_points)
  beta <- round(coef(trend_model)[2], 6)
  p_value_trend <- summary(trend_model)$coefficients[2,4]
  
  if(!is.na(p_value_trend) && p_value_trend < 0.05) {
    trend_direction <- ifelse(beta > 0, "Increasing", "Decreasing")
  } else {
    trend_direction <- "Stable (not significant)"
  }
  
  return(list(beta = beta, trend = trend_direction, p_value = p_value_trend))
}

# Main analysis function
analyze_state_arima <- function(state_name, data_df) {
  tryCatch({
    cat("Processing:", state_name, "\n")
    
    # Get state data
    state_data <- data_df %>% filter(State == state_name)
    
    if(nrow(state_data) == 0) {
      cat("  No data for", state_name, "\n")
      return(NULL)
    }
    
    # Prepare time series
    ts_data <- prepare_state_ts(state_data)
    
    if(is.null(ts_data)) {
      cat("  Insufficient valid data for", state_name, "\n")
      return(NULL)
    }
    
    # Check if we have enough observations
    if(length(ts_data) < 6) {
      cat("  Too few observations (", length(ts_data), ") for", state_name, "\n")
      return(NULL)
    }
    
    # Fit auto.arima model
    model <- tryCatch({
      auto.arima(ts_data, 
                 seasonal = TRUE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 trace = FALSE,
                 allowdrift = TRUE,
                 allowmean = TRUE,
                 lambda = NULL)
    }, error = function(e) {
      cat("  auto.arima failed, trying simpler model...\n")
      auto.arima(ts_data, 
                 seasonal = FALSE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 allowdrift = TRUE,
                 allowmean = TRUE)
    })
    
    if(is.null(model)) {
      cat("  Could not fit model for", state_name, "\n")
      return(NULL)
    }
    
    # Extract model information
    model_type <- get_arima_equation(model)
    full_equation <- get_full_equation(model)
    intercept <- get_intercept(model)
    
    # Calculate trend and beta
    trend_info <- calculate_trend(ts_data, model)
    beta <- trend_info$beta
    trend <- trend_info$trend
    trend_p_value <- trend_info$p_value
    
    # Model fit statistics
    residuals <- na.omit(residuals(model))
    
    if(length(residuals) == 0) {
      cat("  No residuals for", state_name, "\n")
      return(NULL)
    }
    
    n <- length(na.omit(ts_data))
    k <- max(1, length(coef(model)))
    
    # Calculate R-squared
    ss_res <- sum(residuals^2, na.rm = TRUE)
    ss_tot <- sum((ts_data - mean(ts_data, na.rm = TRUE))^2, na.rm = TRUE)
    r2 <- 1 - (ss_res / ss_tot)
    r2_adj <- 1 - (1 - r2) * ((n - 1) / (n - k - 1))
    
    # Calculate MAPE
    mape <- tryCatch({
      mean(abs(residuals / ts_data), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # AIC and BIC
    aic <- tryCatch(AIC(model), error = function(e) NA)
    bic <- tryCatch(BIC(model), error = function(e) NA)
    
    # Ljung-Box test
    lb_test <- tryCatch({
      Box.test(residuals, lag = min(12, length(residuals)%/%4), type = "Ljung-Box")
    }, error = function(e) list(p.value = NA))
    p_value_lb <- ifelse(is.null(lb_test$p.value), NA, lb_test$p.value)
    
    # Historical average
    historical_avg <- round(mean(ts_data, na.rm = TRUE), 2)
    
    # Forecast for 2026-2030 (5 years = 20 quarters ahead)
    forecast_obj <- tryCatch({
      forecast(model, h = 20)
    }, error = function(e) {
      cat("  Forecasting failed for", state_name, "\n")
      return(NULL)
    })
    
    if(is.null(forecast_obj)) {
      forecast_2026 <- forecast_2027 <- forecast_2028 <- forecast_2029 <- forecast_2030 <- NA
    } else {
      forecast_2026 <- round(forecast_obj$mean[4], 2)   # Q4 2026
      forecast_2027 <- round(forecast_obj$mean[8], 2)   # Q4 2027
      forecast_2028 <- round(forecast_obj$mean[12], 2)  # Q4 2028
      forecast_2029 <- round(forecast_obj$mean[16], 2)  # Q4 2029
      forecast_2030 <- round(forecast_obj$mean[20], 2)  # Q4 2030
    }
    
    # Create results dataframe
    result <- data.frame(
      State = state_name,
      Model_Type = model_type,
      Full_Equation = full_equation,
      Intercept = ifelse(is.na(intercept), "None", as.character(intercept)),
      Beta_Rate_of_Change = beta,
      Trend_Significance_p = round(trend_p_value, 4),
      R_Squared = round(r2, 4),
      Adj_R_Squared = round(r2_adj, 4),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Ljung_Box_p_value = round(p_value_lb, 4),
      Average_2018_2025 = historical_avg,
      Projected_2026_Q4 = forecast_2026,
      Projected_2027_Q4 = forecast_2027,
      Projected_2028_Q4 = forecast_2028,
      Projected_2029_Q4 = forecast_2029,
      Projected_2030_Q4 = forecast_2030,
      Trend_Classification = trend,
      stringsAsFactors = FALSE
    )
    
    # Add a flag for National
    if(grepl("Natio|National", state_name, ignore.case = TRUE)) {
      result$Is_National <- "Yes"
      cat("  ★ NATIONAL DATA successfully processed ★\n")
    } else {
      result$Is_National <- "No"
    }
    
    cat("  ✓ Successfully processed", state_name, "- Model:", model_type, "\n")
    return(result)
    
  }, error = function(e) {
    cat("  ✗ Error processing", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Run the analysis for hpsa2
cat("STEP 2: Running ARIMA analysis for all states (including National)...\n")
cat("-------------------------------------------\n")

results_list_hpsa2 <- list()

# Get unique states - include ALL states including National
states_to_process <- unique(hpsa2_clean$State)

# Make sure National/Natioal is included
national_rows <- grep("Natio|National", states_to_process, value = TRUE, ignore.case = TRUE)
if(length(national_rows) > 0) {
  cat(paste0("\n★ Including National row(s): ", paste(national_rows, collapse = ", "), " ★\n\n"))
}

for(state in states_to_process) {
  result <- analyze_state_arima(state, hpsa2_clean)
  if(!is.null(result)) {
    results_list_hpsa2[[state]] <- result
  }
}

# Combine results
cat("\nSTEP 3: Combining results...\n")
cat("-------------------------------------------\n")

if(length(results_list_hpsa2) > 0) {
  final_results_hpsa2 <- bind_rows(results_list_hpsa2)
  
  # Sort by state name (National will appear alphabetically)
  final_results_hpsa2 <- final_results_hpsa2 %>% arrange(State)
  
  # Display first few rows
  cat("\nFirst 10 rows of results:\n")
  print(head(final_results_hpsa2, 10))
  
  # Extract National results separately
  national_results <- final_results_hpsa2 %>%
    filter(grepl("Natio|National", State, ignore.case = TRUE))
  
  if(nrow(national_results) > 0) {
    cat("\n★ NATIONAL RESULTS ★\n")
    print(national_results)
  } else {
    cat("\n⚠ No National row found in results\n")
  }
  
  # Create summary statistics
  summary_stats_hpsa2 <- data.frame(
    Metric = c("Total States Processed", 
               "States (excluding National)",
               "Average R-squared", 
               "Average MAPE", 
               "Increasing Trend", 
               "Decreasing Trend", 
               "Stable Trend",
               "Successful Models",
               "Number of Quarters"),
    Value = c(
      nrow(final_results_hpsa2),
      nrow(final_results_hpsa2) - nrow(national_results),
      round(mean(final_results_hpsa2$R_Squared[!grepl("Natio|National", final_results_hpsa2$State)], na.rm = TRUE), 4),
      round(mean(final_results_hpsa2$MAPE_percent[!grepl("Natio|National", final_results_hpsa2$State)], na.rm = TRUE), 2),
      sum(final_results_hpsa2$Trend_Classification == "Increasing" & !grepl("Natio|National", final_results_hpsa2$State), na.rm = TRUE),
      sum(final_results_hpsa2$Trend_Classification == "Decreasing" & !grepl("Natio|National", final_results_hpsa2$State), na.rm = TRUE),
      sum(grepl("Stable", final_results_hpsa2$Trend_Classification) & !grepl("Natio|National", final_results_hpsa2$State), na.rm = TRUE),
      sum(!is.na(final_results_hpsa2$R_Squared)),
      length(quarter_columns)
    )
  )
  
  # Create model distribution
  model_distribution <- as.data.frame(table(final_results_hpsa2$Model_Type))
  colnames(model_distribution) <- c("Model_Type", "Count")
  model_distribution <- model_distribution %>% arrange(desc(Count))
  
  # Create trend analysis summary
  trend_summary <- final_results_hpsa2 %>%
    group_by(Trend_Classification) %>%
    summarise(
      Number_of_States = n(),
      Average_R2 = round(mean(R_Squared, na.rm = TRUE), 4),
      Average_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2),
      Average_Projected_2030 = round(mean(Projected_2030_Q4, na.rm = TRUE), 2),
      Min_Value = round(min(Average_2018_2025, na.rm = TRUE), 2),
      Max_Value = round(max(Average_2018_2025, na.rm = TRUE), 2)
    )
  
  # Create a summary of top/bottom states by R-squared
  best_fit_states <- final_results_hpsa2 %>%
    filter(!grepl("Natio|National", State)) %>%
    arrange(desc(R_Squared)) %>%
    select(State, R_Squared, MAPE_percent, Model_Type) %>%
    head(10)
  
  worst_fit_states <- final_results_hpsa2 %>%
    filter(!grepl("Natio|National", State)) %>%
    arrange(R_Squared) %>%
    select(State, R_Squared, MAPE_percent, Model_Type) %>%
    head(10)
  
  # Compare National with other states
  if(nrow(national_results) > 0) {
    national_comparison <- final_results_hpsa2 %>%
      mutate(Comparison_Group = ifelse(grepl("Natio|National", State), "National", "Other States")) %>%
      group_by(Comparison_Group) %>%
      summarise(
        Count = n(),
        Avg_R2 = round(mean(R_Squared, na.rm = TRUE), 4),
        Avg_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2),
        Avg_Projected_2030 = round(mean(Projected_2030_Q4, na.rm = TRUE), 2),
        Avg_Historical = round(mean(Average_2018_2025, na.rm = TRUE), 2)
      )
    
    cat("\n★ National vs Other States Comparison ★\n")
    print(national_comparison)
  }
  
  # Export to Excel with multiple sheets
  cat("\nSTEP 4: Exporting to Excel...\n")
  cat("-------------------------------------------\n")
  
  # Create list of sheets for Excel
  excel_sheets_list <- list(
    Detailed_Results = final_results_hpsa2,
    Summary_Statistics = summary_stats_hpsa2,
    National_Results = national_results,
    Model_Distribution = model_distribution,
    Trend_Analysis = trend_summary,
    Best_Fit_States = best_fit_states,
    Worst_Fit_States = worst_fit_states,
    Forecast_Summary = final_results_hpsa2 %>%
      select(State, Projected_2026_Q4, Projected_2027_Q4, 
             Projected_2028_Q4, Projected_2029_Q4, Projected_2030_Q4, Trend_Classification)
  )
  
  # Add national comparison if available
  if(exists("national_comparison")) {
    excel_sheets_list$National_Comparison = national_comparison
  }
  
  write_xlsx(excel_sheets_list, "ARIMA_hpsa2_Analysis_with_National.xlsx")
  
  cat("\n✓ Analysis complete for hpsa2 (including National)!\n")
  cat("✓ Results saved to 'ARIMA_hpsa2_Analysis_with_National.xlsx'\n\n")
  
  # Print summary
  cat("===========================================\n")
  cat("SUMMARY STATISTICS (hpsa2 with National)\n")
  cat("===========================================\n")
  print(summary_stats_hpsa2)
  
  if(nrow(national_results) > 0) {
    cat("\n===========================================\n")
    cat("NATIONAL DETAILS\n")
    cat("===========================================\n")
    print(national_results %>% select(-Is_National))
  }
  
  cat("\n===========================================\n")
  cat("TOP 10 MODEL TYPES\n")
  cat("===========================================\n")
  print(head(model_distribution, 10))
  
  cat("\n===========================================\n")
  cat("TREND ANALYSIS SUMMARY\n")
  cat("===========================================\n")
  print(trend_summary)
  
  # Create visualizations
  cat("\nSTEP 5: Creating visualizations...\n")
  cat("-------------------------------------------\n")
  
  # Create comparison charts
  png(filename = "hpsa2_analysis_with_national.png", width = 1400, height = 1000)
  par(mfrow = c(2, 3))
  
  # R-squared distribution
  hist(final_results_hpsa2$R_Squared, 
       main = "Distribution of R-squared Values\n(Blue=National)",
       xlab = "R-squared", 
       col = "steelblue",
       breaks = 20)
  if(nrow(national_results) > 0) {
    abline(v = national_results$R_Squared, col = "red", lwd = 3, lty = 2)
    legend("topright", legend = "National", col = "red", lwd = 3, lty = 2)
  }
  abline(v = mean(final_results_hpsa2$R_Squared[!grepl("Natio|National", final_results_hpsa2$State)], na.rm = TRUE), 
         col = "darkgreen", lwd = 2, lty = 3)
  
  # MAPE distribution
  hist(final_results_hpsa2$MAPE_percent, 
       main = "Distribution of MAPE Values\n(Blue=National)",
       xlab = "MAPE (%)", 
       col = "lightgreen",
       breaks = 20)
  if(nrow(national_results) > 0) {
    abline(v = national_results$MAPE_percent, col = "red", lwd = 3, lty = 2)
  }
  
  # Trend classification pie chart
  trend_counts <- table(final_results_hpsa2$Trend_Classification)
  pie(trend_counts, 
      main = "Trend Classification\n(All States including National)",
      col = c("green", "red", "gray", "orange"))
  
  # Model types bar plot (top 10)
  top_models <- head(model_distribution, 10)
  barplot(top_models$Count, 
          names.arg = top_models$Model_Type,
          main = "Top 10 Model Types",
          ylab = "Count",
          col = "steelblue",
          las = 2,
          cex.names = 0.7)
  
  # National vs Others R-squared comparison
  if(nrow(national_results) > 0) {
    comp_data <- data.frame(
      Category = c("National", "Average of Others"),
      R_Squared = c(national_results$R_Squared, 
                    mean(final_results_hpsa2$R_Squared[!grepl("Natio|National", final_results_hpsa2$State)], na.rm = TRUE))
    )
    barplot(comp_data$R_Squared, 
            names.arg = comp_data$Category,
            main = "R-squared: National vs Others",
            ylab = "R-squared",
            col = c("red", "steelblue"),
            ylim = c(0, 1))
  }
  
  # Projected values comparison (2030)
  if(nrow(national_results) > 0) {
    proj_data <- data.frame(
      Category = c("National", "Average of Others"),
      Projected_2030 = c(national_results$Projected_2030_Q4, 
                         mean(final_results_hpsa2$Projected_2030_Q4[!grepl("Natio|National", final_results_hpsa2$State)], na.rm = TRUE))
    )
    barplot(proj_data$Projected_2030, 
            names.arg = proj_data$Category,
            main = "2030 Projections: National vs Others",
            ylab = "Projected Value",
            col = c("red", "steelblue"))
  }
  
  dev.off()
  cat("✓ Visualization saved as 'hpsa2_analysis_with_national.png'\n")
  
  # Create forecast comparison including National
  selected_states <- c("National", "Alabama", "California", "Texas", "New York", "Florida")
  # Check if National exists in results, if not try "Natioal"
  if(!any(grepl("National", selected_states)) && nrow(national_results) > 0) {
    selected_states[1] <- national_results$State[1]
  }
  
  forecast_comparison <- final_results_hpsa2 %>%
    filter(State %in% selected_states | grepl("Natio|National", State)) %>%
    select(State, Projected_2026_Q4, Projected_2027_Q4, 
           Projected_2028_Q4, Projected_2029_Q4, Projected_2030_Q4)
  
  # Reshape for plotting
  forecast_long <- forecast_comparison %>%
    pivot_longer(cols = -State, names_to = "Year", values_to = "Projected_Value")
  
  # Highlight National in the plot
  p <- ggplot(forecast_long, aes(x = Year, y = Projected_Value, color = State, group = State, 
                                 size = ifelse(grepl("Natio|National", State), 1.5, 1))) +
    geom_line() +
    geom_point() +
    scale_size_identity() +
    labs(title = "Forecast Comparison: National vs Selected States (hpsa2)",
         x = "Year", y = "Projected Value",
         caption = "National data highlighted with thicker lines") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  ggsave("hpsa2_forecast_with_national.png", p, width = 12, height = 7)
  cat("✓ Forecast comparison with National saved as 'hpsa2_forecast_with_national.png'\n")
  
  # Create National-specific forecast plot
  if(nrow(national_results) > 0) {
    # Get the actual time series for National
    national_data <- hpsa2_clean %>% filter(grepl("Natio|National", State))
    national_ts <- prepare_state_ts(national_data)
    
    if(!is.null(national_ts)) {
      national_model <- auto.arima(national_ts, seasonal = TRUE)
      national_forecast <- forecast(national_model, h = 20)
      
      png(filename = "hpsa2_national_forecast.png", width = 1000, height = 600)
      plot(national_forecast, main = "National (United States) - ARIMA Forecast",
           xlab = "Year", ylab = "Value", 
           col.main = "darkblue", col.lab = "darkblue")
      dev.off()
      cat("✓ National-specific forecast plot saved as 'hpsa2_national_forecast.png'\n")
    }
  }
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
}

# Diagnostic information
cat("\n===========================================\n")
cat("DIAGNOSTIC INFORMATION\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list_hpsa2), "\n")
cat("Number of states in dataset:", length(states_to_process), "\n")
cat("Success rate:", round(length(results_list_hpsa2)/length(states_to_process)*100, 1), "%\n")

if(length(results_list_hpsa2) < length(states_to_process)) {
  failed_states <- setdiff(states_to_process, names(results_list_hpsa2))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
} else {
  cat("\n✓ All states (including National) were processed successfully!\n")
}

# Final summary with National emphasis
if(exists("national_results") && nrow(national_results) > 0) {
  cat("\n===========================================\n")
  cat("FINAL SUMMARY WITH NATIONAL HIGHLIGHTS\n")
  cat("===========================================\n")
  cat("\n★ NATIONAL (", national_results$State[1], ") RESULTS ★\n", sep = "")
  cat("  Model Type:", national_results$Model_Type, "\n")
  cat("  R-squared:", national_results$R_Squared, "\n")
  cat("  MAPE:", national_results$MAPE_percent, "%\n")
  cat("  Trend:", national_results$Trend_Classification, "\n")
  cat("  Historical Average (2018-2025):", national_results$Average_2018_2025, "\n")
  cat("  Projected Value for 2030:", national_results$Projected_2030_Q4, "\n")
}

cat("\n===========================================\n")
cat("ANALYSIS COMPLETE FOR hpsa2 (with National)!\n")
cat("===========================================\n")




View(Hpsa11)

names(Hpsa11)


str(Hpsa11)


# Load required libraries
library(tidyverse)
library(writexl)
library(ggplot2)
library(broom)
library(lubridate)

#===========================================
# REGRESSION ANALYSIS FOR Hpsa11 DATASET
# Starting from Q1 2019 to Q4 2025
#===========================================

cat("===========================================\n")
cat("REGRESSION ANALYSIS FOR Hpsa11 DATASET\n")
cat("===========================================\n\n")

# STEP 1: Clean and prepare data
cat("STEP 1: Preparing data for regression analysis...\n")
cat("-------------------------------------------\n")

# Make a copy of the data
Hpsa11_clean <- Hpsa11

# Convert all quarterly columns to numeric (ensure no character issues)
quarter_columns <- names(Hpsa11_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  Hpsa11_clean[[col]] <- suppressWarnings(as.numeric(as.character(Hpsa11_clean[[col]])))
}

# Create a time index (quarters from Q1 2019 to Q4 2025)
n_quarters <- length(quarter_columns)
start_date <- as.Date("2019-01-01")
quarter_seq <- seq.Date(start_date, by = "quarter", length.out = n_quarters)
time_index <- 1:n_quarters

# Create a data frame with time periods for reference
time_periods <- data.frame(
  Quarter = quarter_columns,
  Time_Index = time_index,
  Date = quarter_seq
)

cat("Data ready for analysis\n")
cat("Number of quarters:", n_quarters, "\n")
cat("Time period: Q1 2019 to Q4 2025\n\n")

# Function to prepare state data for regression
prepare_state_data <- function(state_data, state_name) {
  # Extract quarterly values
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  valid_idx <- !is.na(values)
  values <- values[valid_idx]
  time_idx <- time_index[valid_idx]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create data frame for regression
  df <- data.frame(
    Time = time_idx,
    Value = values,
    Quarter_Number = 1:length(values)
  )
  
  return(df)
}

# Function to perform linear regression and extract statistics
perform_regression <- function(df, state_name) {
  tryCatch({
    # Linear regression: Value ~ Time
    model <- lm(Value ~ Time, data = df)
    
    # Extract coefficients
    intercept <- coef(model)[1]
    beta <- coef(model)[2]  # rate of change per quarter
    
    # Model summary
    summary_model <- summary(model)
    
    # R-squared and Adjusted R-squared
    r_squared <- summary_model$r.squared
    adj_r_squared <- summary_model$adj.r.squared
    
    # P-value for the slope (beta)
    p_value <- summary_model$coefficients[2, 4]
    
    # Create full equation string
    equation <- paste0("Value = ", round(intercept, 4), " + ", 
                       round(beta, 4), " * Time")
    
    # Determine trend based on p-value and beta sign
    if(p_value < 0.05) {
      if(beta > 0) {
        trend <- "Increasing"
      } else {
        trend <- "Decreasing"
      }
    } else {
      trend <- "Stable (not significant)"
    }
    
    # Calculate historical average (2019-2025)
    historical_avg <- round(mean(df$Value), 2)
    
    # Calculate fitted values and residuals for additional metrics
    fitted_values <- fitted(model)
    residuals <- resid(model)
    
    # Calculate MAPE (Mean Absolute Percentage Error)
    mape <- tryCatch({
      mean(abs(residuals / df$Value), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # Calculate AIC and BIC
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Calculate standard error of beta
    beta_se <- summary_model$coefficients[2, 2]
    
    # Calculate confidence intervals for beta (95%)
    beta_ci_lower <- beta - 1.96 * beta_se
    beta_ci_upper <- beta + 1.96 * beta_se
    
    # Create result dataframe
    result <- data.frame(
      State = state_name,
      Full_Equation = equation,
      Intercept = round(intercept, 4),
      Beta_Rate_of_Change = round(beta, 6),
      Beta_Std_Error = round(beta_se, 6),
      Beta_CI_Lower_95 = round(beta_ci_lower, 6),
      Beta_CI_Upper_95 = round(beta_ci_upper, 6),
      R_Squared = round(r_squared, 4),
      Adj_R_Squared = round(adj_r_squared, 4),
      P_Value = round(p_value, 6),
      Significance_Code = ifelse(p_value < 0.001, "***", 
                                 ifelse(p_value < 0.01, "**",
                                        ifelse(p_value < 0.05, "*", "ns"))),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Average_2019_2025 = historical_avg,
      Trend_Classification = trend,
      Num_Observations = nrow(df),
      stringsAsFactors = FALSE
    )
    
    return(result)
    
  }, error = function(e) {
    cat("  Error in regression for", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# STEP 2: Run regression for all states
cat("\nSTEP 2: Running regression analysis for all states...\n")
cat("-------------------------------------------\n")

results_list <- list()
states_list <- unique(Hpsa11_clean$State)

for(state in states_list) {
  cat("Processing:", state, "\n")
  
  # Get state data
  state_data <- Hpsa11_clean %>% filter(State == state)
  
  if(nrow(state_data) == 0) {
    cat("  No data for", state, "\n")
    next
  }
  
  # Prepare data for regression
  df <- prepare_state_data(state_data, state)
  
  if(is.null(df)) {
    cat("  Insufficient data for", state, "\n")
    next
  }
  
  # Perform regression
  result <- perform_regression(df, state)
  
  if(!is.null(result)) {
    results_list[[state]] <- result
    cat("  ✓ Completed - R²:", result$R_Squared, ", Trend:", result$Trend_Classification, "\n")
  }
}

# STEP 3: Combine results
cat("\nSTEP 3: Combining results...\n")
cat("-------------------------------------------\n")

if(length(results_list) > 0) {
  final_results <- bind_rows(results_list)
  
  # Sort by state name
  final_results <- final_results %>% arrange(State)
  
  # Display first few rows
  cat("\nFirst 10 rows of results:\n")
  print(head(final_results, 10))
  
  # STEP 4: Create summary statistics
  cat("\nSTEP 4: Creating summary statistics...\n")
  cat("-------------------------------------------\n")
  
  summary_stats <- data.frame(
    Metric = c("Total States Processed",
               "States with Increasing Trend",
               "States with Decreasing Trend",
               "States with Stable Trend",
               "Average R-squared",
               "Average Adj R-squared",
               "Average MAPE",
               "Average Beta (Rate of Change)",
               "Median Beta",
               "Range of Beta (Min)",
               "Range of Beta (Max)"),
    Value = c(
      nrow(final_results),
      sum(final_results$Trend_Classification == "Increasing"),
      sum(final_results$Trend_Classification == "Decreasing"),
      sum(final_results$Trend_Classification == "Stable (not significant)"),
      round(mean(final_results$R_Squared), 4),
      round(mean(final_results$Adj_R_Squared), 4),
      round(mean(final_results$MAPE_percent, na.rm = TRUE), 2),
      round(mean(final_results$Beta_Rate_of_Change), 6),
      round(median(final_results$Beta_Rate_of_Change), 6),
      round(min(final_results$Beta_Rate_of_Change), 6),
      round(max(final_results$Beta_Rate_of_Change), 6)
    )
  )
  
  # Significance summary
  significance_summary <- final_results %>%
    group_by(Significance_Code) %>%
    summarise(Count = n()) %>%
    mutate(Interpretation = case_when(
      Significance_Code == "***" ~ "p < 0.001 (Highly significant)",
      Significance_Code == "**" ~ "p < 0.01 (Significant)",
      Significance_Code == "*" ~ "p < 0.05 (Marginally significant)",
      Significance_Code == "ns" ~ "p >= 0.05 (Not significant)"
    ))
  
  # Top increasing and decreasing states
  top_increasing <- final_results %>%
    filter(Trend_Classification == "Increasing") %>%
    arrange(desc(Beta_Rate_of_Change)) %>%
    select(State, Beta_Rate_of_Change, R_Squared, P_Value) %>%
    head(10)
  
  top_decreasing <- final_results %>%
    filter(Trend_Classification == "Decreasing") %>%
    arrange(Beta_Rate_of_Change) %>%
    select(State, Beta_Rate_of_Change, R_Squared, P_Value) %>%
    head(10)
  
  # Best and worst fit states
  best_fit <- final_results %>%
    arrange(desc(R_Squared)) %>%
    select(State, R_Squared, Adj_R_Squared, MAPE_percent, Trend_Classification) %>%
    head(10)
  
  worst_fit <- final_results %>%
    arrange(R_Squared) %>%
    select(State, R_Squared, Adj_R_Squared, MAPE_percent, Trend_Classification) %>%
    head(10)
  
  # STEP 5: Export to Excel
  cat("\nSTEP 5: Exporting to Excel...\n")
  cat("-------------------------------------------\n")
  
  write_xlsx(list(
    Detailed_Results = final_results,
    Summary_Statistics = summary_stats,
    Significance_Summary = significance_summary,
    Top_Increasing_States = top_increasing,
    Top_Decreasing_States = top_decreasing,
    Best_Fit_States = best_fit,
    Worst_Fit_States = worst_fit,
    Regression_Details = final_results %>%
      select(State, Beta_Rate_of_Change, Beta_Std_Error, Beta_CI_Lower_95, 
             Beta_CI_Upper_95, R_Squared, P_Value, Significance_Code),
    Trend_Summary = final_results %>%
      group_by(Trend_Classification) %>%
      summarise(
        Count = n(),
        Average_Beta = round(mean(Beta_Rate_of_Change), 6),
        Average_R2 = round(mean(R_Squared), 4),
        Average_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2)
      )
  ), "Regression_Hpsa11_Analysis_Results.xlsx")
  
  cat("✓ Results saved to 'Regression_Hpsa11_Analysis_Results.xlsx'\n\n")
  
  # STEP 6: Create visualizations
  cat("STEP 6: Creating visualizations...\n")
  cat("-------------------------------------------\n")
  
  # Create summary plots
  png(filename = "Hpsa11_Regression_Summary.png", width = 1400, height = 1000)
  par(mfrow = c(2, 3))
  
  # 1. Distribution of R-squared values
  hist(final_results$R_Squared, 
       main = "Distribution of R-squared Values",
       xlab = "R-squared", 
       col = "steelblue",
       breaks = 20,
       xlim = c(0, 1))
  abline(v = mean(final_results$R_Squared), col = "red", lwd = 2, lty = 2)
  legend("topright", legend = paste("Mean =", round(mean(final_results$R_Squared), 3)), 
         col = "red", lwd = 2, lty = 2)
  
  # 2. Distribution of Beta (Rate of Change)
  hist(final_results$Beta_Rate_of_Change, 
       main = "Distribution of Beta (Rate of Change per Quarter)",
       xlab = "Beta Value", 
       col = "lightgreen",
       breaks = 20)
  abline(v = 0, col = "red", lwd = 2, lty = 2)
  abline(v = mean(final_results$Beta_Rate_of_Change), col = "blue", lwd = 2, lty = 2)
  
  # 3. Trend classification pie chart
  trend_counts <- table(final_results$Trend_Classification)
  pie(trend_counts, 
      main = "Trend Classification",
      col = c("green", "red", "gray"))
  
  # 4. Significance levels bar plot
  sig_counts <- table(final_results$Significance_Code)
  barplot(sig_counts, 
          main = "Statistical Significance Levels",
          xlab = "Significance Code", 
          ylab = "Number of States",
          col = c("darkred", "red", "pink", "lightgray"))
  
  # 5. Top 10 states by R-squared
  top10_r2 <- head(final_results[order(-final_results$R_Squared),], 10)
  barplot(top10_r2$R_Squared, 
          names.arg = top10_r2$State,
          main = "Top 10 States by R-squared",
          ylab = "R-squared",
          col = "steelblue",
          las = 2,
          cex.names = 0.7)
  
  # 6. Top 10 states by absolute Beta
  top10_beta <- head(final_results[order(-abs(final_results$Beta_Rate_of_Change)),], 10)
  barplot(top10_beta$Beta_Rate_of_Change, 
          names.arg = top10_beta$State,
          main = "Top 10 States by |Beta| (Rate of Change)",
          ylab = "Beta Value",
          col = ifelse(top10_beta$Beta_Rate_of_Change > 0, "green", "red"),
          las = 2,
          cex.names = 0.7)
  legend("topright", legend = c("Positive", "Negative"), fill = c("green", "red"))
  
  dev.off()
  cat("✓ Summary visualization saved as 'Hpsa11_Regression_Summary.png'\n")
  
  # Create detailed trend plot for selected states
  selected_states <- c("California", "Texas", "New York", "Florida", "Alaska")
  selected_data <- final_results %>% filter(State %in% selected_states)
  
  # Prepare time series data for plotting
  plot_data <- data.frame()
  for(state in selected_states) {
    state_data <- Hpsa11_clean %>% filter(State == state)
    values <- as.numeric(unlist(state_data[,-1]))
    valid_idx <- !is.na(values)
    values <- values[valid_idx]
    time_idx <- time_index[valid_idx]
    
    temp_df <- data.frame(
      State = state,
      Time = time_idx,
      Value = values
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # Create trend lines plot
  p <- ggplot(plot_data, aes(x = Time, y = Value, color = State)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95) +
    labs(title = "Linear Trends for Selected States (Hpsa11)",
         subtitle = "Q1 2019 to Q4 2025",
         x = "Time Index (Quarters)", 
         y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set1")
  
  ggsave("Hpsa11_Selected_States_Trends.png", p, width = 12, height = 7)
  cat("✓ Selected states trends plot saved as 'Hpsa11_Selected_States_Trends.png'\n")
  
  # Create heatmap of R-squared values by region (if regional data available)
  # For now, create a simple bar plot of all states' R-squared
  p2 <- ggplot(final_results, aes(x = reorder(State, R_Squared), y = R_Squared, 
                                  fill = Trend_Classification)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "R-squared Values by State",
         subtitle = "Ordered from lowest to highest R-squared",
         x = "State", 
         y = "R-squared") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6)) +
    scale_fill_manual(values = c("Increasing" = "green", 
                                 "Decreasing" = "red", 
                                 "Stable (not significant)" = "gray"))
  
  ggsave("Hpsa11_R2_by_State.png", p2, width = 12, height = 10)
  cat("✓ R-squared by state plot saved as 'Hpsa11_R2_by_State.png'\n")
  
  # STEP 7: Print summary reports
  cat("\n===========================================\n")
  cat("REGRESSION ANALYSIS SUMMARY\n")
  cat("===========================================\n")
  print(summary_stats)
  
  cat("\n===========================================\n")
  cat("SIGNIFICANCE LEVELS\n")
  cat("===========================================\n")
  print(significance_summary)
  
  cat("\n===========================================\n")
  cat("TOP 10 INCREASING STATES (Fastest Growth)\n")
  cat("===========================================\n")
  print(top_increasing)
  
  cat("\n===========================================\n")
  cat("TOP 10 DECREASING STATES (Fastest Decline)\n")
  cat("===========================================\n")
  print(top_decreasing)
  
  cat("\n===========================================\n")
  cat("BEST FIT STATES (Highest R²)\n")
  cat("===========================================\n")
  print(best_fit)
  
  cat("\n===========================================\n")
  cat("WORST FIT STATES (Lowest R²)\n")
  cat("===========================================\n")
  print(worst_fit)
  
  # Additional analysis: Correlation between Beta and R-squared
  cat("\n===========================================\n")
  cat("ADDITIONAL INSIGHTS\n")
  cat("===========================================\n")
  
  # Calculate correlation
  cor_beta_r2 <- cor(final_results$Beta_Rate_of_Change, final_results$R_Squared)
  cat("Correlation between Beta and R-squared:", round(cor_beta_r2, 4), "\n")
  
  # Find states with significant trends
  significant_states <- final_results %>% 
    filter(P_Value < 0.05) %>%
    select(State, Beta_Rate_of_Change, Trend_Classification, R_Squared)
  
  cat("\nStates with statistically significant trends (p < 0.05):", nrow(significant_states), "out of", nrow(final_results), "\n")
  cat("  - Increasing:", sum(significant_states$Trend_Classification == "Increasing"), "\n")
  cat("  - Decreasing:", sum(significant_states$Trend_Classification == "Decreasing"), "\n")
  
  # Average quarterly change
  cat("\nAverage quarterly change across all states:", 
      round(mean(final_results$Beta_Rate_of_Change), 6), "units\n")
  
  # Projection for end of 2026 (approximately 4 quarters after Q4 2025)
  # Time index for Q4 2025 is max(time_index) = 28
  # Q4 2026 would be time index 32
  final_time <- max(time_index)
  future_time <- final_time + 4  # Q4 2026
  
  final_results <- final_results %>%
    mutate(
      Projected_Q4_2026 = Intercept + Beta_Rate_of_Change * future_time,
      Projected_Change_2025_to_2026 = Projected_Q4_2026 - (Intercept + Beta_Rate_of_Change * final_time)
    )
  
  # Add projections to Excel
  write_xlsx(list(
    Detailed_Results_with_Projections = final_results,
    Summary_Statistics = summary_stats,
    Significance_Summary = significance_summary,
    Top_Increasing_States = top_increasing,
    Top_Decreasing_States = top_decreasing,
    Best_Fit_States = best_fit,
    Worst_Fit_States = worst_fit,
    Significant_Trends = significant_states
  ), "Regression_Hpsa11_Complete_Analysis.xlsx")
  
  cat("\n✓ Complete analysis with 2026 projections saved to 'Regression_Hpsa11_Complete_Analysis.xlsx'\n")
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
}

# Final diagnostic
cat("\n===========================================\n")
cat("DIAGNOSTIC INFORMATION\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list), "\n")
cat("Total states in dataset:", length(states_list), "\n")
cat("Success rate:", round(length(results_list)/length(states_list)*100, 1), "%\n")
cat("Number of quarters analyzed:", n_quarters, "\n")
cat("Time period: Q1 2019 - Q4 2025\n")

if(length(results_list) < length(states_list)) {
  failed_states <- setdiff(states_list, names(results_list))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
}

cat("\n===========================================\n")
cat("ANALYSIS COMPLETE FOR Hpsa11!\n")
cat("===========================================\n")











# Load required libraries
library(tidyverse)
library(writexl)
library(ggplot2)
library(broom)
library(lubridate)

#===========================================
# REGRESSION ANALYSIS FOR Hpsa11 DATASET
# Starting from Q1 2019 to Q4 2025
# INCLUDING NATIONAL ROW
#===========================================

cat("===========================================\n")
cat("REGRESSION ANALYSIS FOR Hpsa11 DATASET (with National)\n")
cat("===========================================\n\n")

# STEP 1: Clean and prepare data
cat("STEP 1: Preparing data for regression analysis...\n")
cat("-------------------------------------------\n")

# Make a copy of the data
Hpsa11_clean <- Hpsa11

# Convert all quarterly columns to numeric (ensure no character issues)
quarter_columns <- names(Hpsa11_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  Hpsa11_clean[[col]] <- suppressWarnings(as.numeric(as.character(Hpsa11_clean[[col]])))
}

# Check for National row
cat("Checking for National row...\n")
unique_states <- unique(Hpsa11_clean$State)
if("National" %in% unique_states) {
  cat("✓ Found 'National' row in data\n")
} else if("Natioal" %in% unique_states) {
  cat("✓ Found 'Natioal' row (likely typo for National)\n")
} else if(any(grepl("Nation", unique_states, ignore.case = TRUE))) {
  national_row <- unique_states[grepl("Nation", unique_states, ignore.case = TRUE)]
  cat("✓ Found '", national_row, "' row (likely National)\n", sep = "")
} else {
  cat("⚠ No National row found. Please check the last row of your data.\n")
}

# Create a time index (quarters from Q1 2019 to Q4 2025)
n_quarters <- length(quarter_columns)
start_date <- as.Date("2019-01-01")
quarter_seq <- seq.Date(start_date, by = "quarter", length.out = n_quarters)
time_index <- 1:n_quarters

# Create a data frame with time periods for reference
time_periods <- data.frame(
  Quarter = quarter_columns,
  Time_Index = time_index,
  Date = quarter_seq
)

cat("Data ready for analysis\n")
cat("Number of quarters:", n_quarters, "\n")
cat("Time period: Q1 2019 to Q4 2025\n")
cat("Total states (including National):", length(unique_states), "\n\n")

# Function to prepare state data for regression
prepare_state_data <- function(state_data, state_name) {
  # Extract quarterly values
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  valid_idx <- !is.na(values)
  values <- values[valid_idx]
  time_idx <- time_index[valid_idx]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create data frame for regression
  df <- data.frame(
    Time = time_idx,
    Value = values,
    Quarter_Number = 1:length(values)
  )
  
  return(df)
}

# Function to perform linear regression and extract statistics
perform_regression <- function(df, state_name) {
  tryCatch({
    # Linear regression: Value ~ Time
    model <- lm(Value ~ Time, data = df)
    
    # Extract coefficients
    intercept <- coef(model)[1]
    beta <- coef(model)[2]  # rate of change per quarter
    
    # Model summary
    summary_model <- summary(model)
    
    # R-squared and Adjusted R-squared
    r_squared <- summary_model$r.squared
    adj_r_squared <- summary_model$adj.r.squared
    
    # P-value for the slope (beta)
    p_value <- summary_model$coefficients[2, 4]
    
    # Create full equation string
    equation <- paste0("Value = ", round(intercept, 4), " + ", 
                       round(beta, 4), " * Time")
    
    # Determine trend based on p-value and beta sign
    if(p_value < 0.05) {
      if(beta > 0) {
        trend <- "Increasing"
      } else {
        trend <- "Decreasing"
      }
    } else {
      trend <- "Stable (not significant)"
    }
    
    # Calculate historical average (2019-2025)
    historical_avg <- round(mean(df$Value), 2)
    
    # Calculate fitted values and residuals for additional metrics
    fitted_values <- fitted(model)
    residuals <- resid(model)
    
    # Calculate MAPE (Mean Absolute Percentage Error)
    mape <- tryCatch({
      mean(abs(residuals / df$Value), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # Calculate AIC and BIC
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Calculate standard error of beta
    beta_se <- summary_model$coefficients[2, 2]
    
    # Calculate confidence intervals for beta (95%)
    beta_ci_lower <- beta - 1.96 * beta_se
    beta_ci_upper <- beta + 1.96 * beta_se
    
    # Create result dataframe
    result <- data.frame(
      State = state_name,
      Full_Equation = equation,
      Intercept = round(intercept, 4),
      Beta_Rate_of_Change = round(beta, 6),
      Beta_Std_Error = round(beta_se, 6),
      Beta_CI_Lower_95 = round(beta_ci_lower, 6),
      Beta_CI_Upper_95 = round(beta_ci_upper, 6),
      R_Squared = round(r_squared, 4),
      Adj_R_Squared = round(adj_r_squared, 4),
      P_Value = round(p_value, 6),
      Significance_Code = ifelse(p_value < 0.001, "***", 
                                 ifelse(p_value < 0.01, "**",
                                        ifelse(p_value < 0.05, "*", "ns"))),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Average_2019_2025 = historical_avg,
      Trend_Classification = trend,
      Num_Observations = nrow(df),
      stringsAsFactors = FALSE
    )
    
    # Add flag for National
    if(grepl("Natio|National", state_name, ignore.case = TRUE)) {
      result$Is_National <- "Yes"
    } else {
      result$Is_National <- "No"
    }
    
    return(result)
    
  }, error = function(e) {
    cat("  Error in regression for", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# STEP 2: Run regression for all states (including National)
cat("\nSTEP 2: Running regression analysis for all states (including National)...\n")
cat("-------------------------------------------\n")

results_list <- list()
states_list <- unique(Hpsa11_clean$State)

# Identify National row for special highlighting
national_state <- states_list[grepl("Natio|National", states_list, ignore.case = TRUE)]
if(length(national_state) > 0) {
  cat(paste0("\n★ Including National row: '", national_state, "' ★\n\n"))
} else {
  cat("\n⚠ No National row identified. Processing all states as regular states.\n\n")
}

for(state in states_list) {
  cat("Processing:", state, ifelse(grepl("Natio|National", state, ignore.case = TRUE), " ★NATIONAL★", ""), "\n")
  
  # Get state data
  state_data <- Hpsa11_clean %>% filter(State == state)
  
  if(nrow(state_data) == 0) {
    cat("  No data for", state, "\n")
    next
  }
  
  # Prepare data for regression
  df <- prepare_state_data(state_data, state)
  
  if(is.null(df)) {
    cat("  Insufficient data for", state, "\n")
    next
  }
  
  # Perform regression
  result <- perform_regression(df, state)
  
  if(!is.null(result)) {
    results_list[[state]] <- result
    if(grepl("Natio|National", state, ignore.case = TRUE)) {
      cat("  ✓ NATIONAL completed - R²:", result$R_Squared, ", Trend:", result$Trend_Classification, "\n")
    } else {
      cat("  ✓ Completed - R²:", result$R_Squared, ", Trend:", result$Trend_Classification, "\n")
    }
  }
}

# STEP 3: Combine results
cat("\nSTEP 3: Combining results...\n")
cat("-------------------------------------------\n")

if(length(results_list) > 0) {
  final_results <- bind_rows(results_list)
  
  # Sort by state name (National will appear alphabetically)
  final_results <- final_results %>% arrange(State)
  
  # Extract National results separately
  national_results <- final_results %>%
    filter(grepl("Natio|National", State, ignore.case = TRUE))
  
  # Display first few rows
  cat("\nFirst 10 rows of results:\n")
  print(head(final_results, 10))
  
  if(nrow(national_results) > 0) {
    cat("\n★ NATIONAL RESULTS ★\n")
    print(national_results)
  } else {
    cat("\n⚠ No National results found\n")
  }
  
  # STEP 4: Create summary statistics
  cat("\nSTEP 4: Creating summary statistics...\n")
  cat("-------------------------------------------\n")
  
  # Summary including National
  summary_stats <- data.frame(
    Metric = c("Total States Processed",
               "States (excluding National)",
               "States with Increasing Trend",
               "States with Decreasing Trend",
               "States with Stable Trend",
               "Average R-squared",
               "Average Adj R-squared",
               "Average MAPE",
               "Average Beta (Rate of Change)",
               "Median Beta",
               "Range of Beta (Min)",
               "Range of Beta (Max)"),
    Value = c(
      nrow(final_results),
      nrow(final_results) - nrow(national_results),
      sum(final_results$Trend_Classification == "Increasing" & final_results$State != national_results$State[1]),
      sum(final_results$Trend_Classification == "Decreasing" & final_results$State != national_results$State[1]),
      sum(final_results$Trend_Classification == "Stable (not significant)" & final_results$State != national_results$State[1]),
      round(mean(final_results$R_Squared[!grepl("Natio|National", final_results$State)]), 4),
      round(mean(final_results$Adj_R_Squared[!grepl("Natio|National", final_results$State)]), 4),
      round(mean(final_results$MAPE_percent[!grepl("Natio|National", final_results$State)], na.rm = TRUE), 2),
      round(mean(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)]), 6),
      round(median(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)]), 6),
      round(min(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)]), 6),
      round(max(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)]), 6)
    )
  )
  
  # National comparison with others
  if(nrow(national_results) > 0) {
    national_comparison <- data.frame(
      Metric = c("National Beta", "Average Beta (Other States)",
                 "National R-squared", "Average R-squared (Other States)",
                 "National MAPE", "Average MAPE (Other States)",
                 "National Trend", "Most Common Trend (Other States)"),
      Value = c(
        national_results$Beta_Rate_of_Change,
        round(mean(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)]), 6),
        national_results$R_Squared,
        round(mean(final_results$R_Squared[!grepl("Natio|National", final_results$State)]), 4),
        national_results$MAPE_percent,
        round(mean(final_results$MAPE_percent[!grepl("Natio|National", final_results$State)], na.rm = TRUE), 2),
        national_results$Trend_Classification,
        names(sort(table(final_results$Trend_Classification[!grepl("Natio|National", final_results$State)]), decreasing = TRUE)[1])
      )
    )
    
    cat("\n★ National vs Other States ★\n")
    print(national_comparison)
  }
  
  # Significance summary
  significance_summary <- final_results %>%
    filter(!grepl("Natio|National", State)) %>%
    group_by(Significance_Code) %>%
    summarise(Count = n()) %>%
    mutate(Interpretation = case_when(
      Significance_Code == "***" ~ "p < 0.001 (Highly significant)",
      Significance_Code == "**" ~ "p < 0.01 (Significant)",
      Significance_Code == "*" ~ "p < 0.05 (Marginally significant)",
      Significance_Code == "ns" ~ "p >= 0.05 (Not significant)"
    ))
  
  # Top increasing and decreasing states (excluding National)
  top_increasing <- final_results %>%
    filter(!grepl("Natio|National", State), Trend_Classification == "Increasing") %>%
    arrange(desc(Beta_Rate_of_Change)) %>%
    select(State, Beta_Rate_of_Change, R_Squared, P_Value) %>%
    head(10)
  
  top_decreasing <- final_results %>%
    filter(!grepl("Natio|National", State), Trend_Classification == "Decreasing") %>%
    arrange(Beta_Rate_of_Change) %>%
    select(State, Beta_Rate_of_Change, R_Squared, P_Value) %>%
    head(10)
  
  # Best and worst fit states (excluding National)
  best_fit <- final_results %>%
    filter(!grepl("Natio|National", State)) %>%
    arrange(desc(R_Squared)) %>%
    select(State, R_Squared, Adj_R_Squared, MAPE_percent, Trend_Classification) %>%
    head(10)
  
  worst_fit <- final_results %>%
    filter(!grepl("Natio|National", State)) %>%
    arrange(R_Squared) %>%
    select(State, R_Squared, Adj_R_Squared, MAPE_percent, Trend_Classification) %>%
    head(10)
  
  # STEP 5: Export to Excel
  cat("\nSTEP 5: Exporting to Excel...\n")
  cat("-------------------------------------------\n")
  
  # Create list of sheets for Excel
  excel_sheets_list <- list(
    Detailed_Results = final_results,
    Summary_Statistics = summary_stats,
    National_Results = national_results,
    Significance_Summary = significance_summary,
    Top_Increasing_States = top_increasing,
    Top_Decreasing_States = top_decreasing,
    Best_Fit_States = best_fit,
    Worst_Fit_States = worst_fit,
    Regression_Details = final_results %>%
      select(State, Beta_Rate_of_Change, Beta_Std_Error, Beta_CI_Lower_95, 
             Beta_CI_Upper_95, R_Squared, P_Value, Significance_Code),
    Trend_Summary = final_results %>%
      group_by(Trend_Classification) %>%
      summarise(
        Count = n(),
        Average_Beta = round(mean(Beta_Rate_of_Change), 6),
        Average_R2 = round(mean(R_Squared), 4),
        Average_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2)
      )
  )
  
  # Add national comparison if available
  if(exists("national_comparison")) {
    excel_sheets_list$National_Comparison = national_comparison
  }
  
  write_xlsx(excel_sheets_list, "Regression_Hpsa11_Analysis_with_National.xlsx")
  
  cat("✓ Results saved to 'Regression_Hpsa11_Analysis_with_National.xlsx'\n\n")
  
  # STEP 6: Create visualizations
  cat("STEP 6: Creating visualizations...\n")
  cat("-------------------------------------------\n")
  
  # Create summary plots
  png(filename = "Hpsa11_Regression_Summary_with_National.png", width = 1400, height = 1000)
  par(mfrow = c(2, 3))
  
  # 1. Distribution of R-squared values (highlight National)
  hist(final_results$R_Squared[!grepl("Natio|National", final_results$State)], 
       main = "Distribution of R-squared Values\n(Blue=National)",
       xlab = "R-squared", 
       col = "steelblue",
       breaks = 20,
       xlim = c(0, 1))
  if(nrow(national_results) > 0) {
    abline(v = national_results$R_Squared, col = "red", lwd = 3, lty = 2)
    legend("topright", legend = "National", col = "red", lwd = 3, lty = 2)
  }
  abline(v = mean(final_results$R_Squared[!grepl("Natio|National", final_results$State)]), 
         col = "darkgreen", lwd = 2, lty = 3)
  
  # 2. Distribution of Beta (highlight National)
  hist(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)], 
       main = "Distribution of Beta (Rate of Change per Quarter)\n(Blue=National)",
       xlab = "Beta Value", 
       col = "lightgreen",
       breaks = 20)
  if(nrow(national_results) > 0) {
    abline(v = national_results$Beta_Rate_of_Change, col = "red", lwd = 3, lty = 2)
  }
  abline(v = 0, col = "black", lwd = 1, lty = 2)
  
  # 3. Trend classification pie chart (including National)
  trend_counts <- table(final_results$Trend_Classification)
  pie(trend_counts, 
      main = "Trend Classification\n(Includes National)",
      col = c("green", "red", "gray"))
  
  # 4. Significance levels bar plot
  sig_counts <- table(final_results$Significance_Code[!grepl("Natio|National", final_results$State)])
  barplot(sig_counts, 
          main = "Statistical Significance Levels\n(Excluding National)",
          xlab = "Significance Code", 
          ylab = "Number of States",
          col = c("darkred", "red", "pink", "lightgray"))
  
  # 5. Top 10 states by R-squared (including National if in top 10)
  top10_r2 <- head(final_results[order(-final_results$R_Squared),], 10)
  bar_colors <- ifelse(grepl("Natio|National", top10_r2$State), "red", "steelblue")
  barplot(top10_r2$R_Squared, 
          names.arg = top10_r2$State,
          main = "Top 10 States by R-squared\n(Red=National)",
          ylab = "R-squared",
          col = bar_colors,
          las = 2,
          cex.names = 0.7)
  
  # 6. National vs Average comparison
  if(nrow(national_results) > 0) {
    comp_data <- data.frame(
      Metric = c("Beta", "R-squared", "MAPE"),
      National = c(national_results$Beta_Rate_of_Change, 
                   national_results$R_Squared,
                   national_results$MAPE_percent),
      Average_Others = c(mean(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)]),
                         mean(final_results$R_Squared[!grepl("Natio|National", final_results$State)]),
                         mean(final_results$MAPE_percent[!grepl("Natio|National", final_results$State)], na.rm = TRUE))
    )
    
    # Create side-by-side bar plot
    barplot(t(as.matrix(comp_data[,2:3])), 
            beside = TRUE,
            names.arg = comp_data$Metric,
            main = "National vs Average of Other States",
            ylab = "Value",
            col = c("red", "steelblue"),
            legend.text = c("National", "Average Others"),
            args.legend = list(x = "topright"))
  }
  
  dev.off()
  cat("✓ Summary visualization saved as 'Hpsa11_Regression_Summary_with_National.png'\n")
  
  # Create detailed trend plot including National
  selected_states <- c(national_results$State[1], "California", "Texas", "New York", "Florida")
  selected_data <- final_results %>% filter(State %in% selected_states)
  
  # Prepare time series data for plotting
  plot_data <- data.frame()
  for(state in selected_states) {
    state_data <- Hpsa11_clean %>% filter(State == state)
    values <- as.numeric(unlist(state_data[,-1]))
    valid_idx <- !is.na(values)
    values <- values[valid_idx]
    time_idx <- time_index[valid_idx]
    
    temp_df <- data.frame(
      State = state,
      Time = time_idx,
      Value = values
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # Create trend lines plot with National highlighted
  p <- ggplot(plot_data, aes(x = Time, y = Value, color = State, 
                             size = ifelse(State == national_results$State[1], 1.2, 0.8))) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, level = 0.95) +
    scale_size_identity() +
    labs(title = "Linear Trends for National and Selected States",
         subtitle = paste("Q1 2019 to Q4 2025 | National:", national_results$State[1]),
         x = "Time Index (Quarters)", 
         y = "Value",
         caption = "National data highlighted with thicker lines") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set1")
  
  ggsave("Hpsa11_Trends_with_National.png", p, width = 12, height = 7)
  cat("✓ Trends plot with National saved as 'Hpsa11_Trends_with_National.png'\n")
  
  # Create National vs Others comparison plot
  if(nrow(national_results) > 0) {
    # Calculate fitted lines for National and average of others
    national_fitted <- data.frame(
      Time = time_index,
      Value = national_results$Intercept + national_results$Beta_Rate_of_Change * time_index,
      Type = "National"
    )
    
    avg_beta <- mean(final_results$Beta_Rate_of_Change[!grepl("Natio|National", final_results$State)])
    avg_intercept <- mean(final_results$Intercept[!grepl("Natio|National", final_results$State)])
    
    avg_fitted <- data.frame(
      Time = time_index,
      Value = avg_intercept + avg_beta * time_index,
      Type = "Average of Other States"
    )
    
    comparison_data <- rbind(national_fitted, avg_fitted)
    
    p2 <- ggplot(comparison_data, aes(x = Time, y = Value, color = Type, linetype = Type)) +
      geom_line(size = 1.2) +
      labs(title = "National Trend vs Average of Other States",
           subtitle = paste("National Beta:", round(national_results$Beta_Rate_of_Change, 4),
                            "| Avg Others Beta:", round(avg_beta, 4)),
           x = "Time Index (Quarters)", 
           y = "Value") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(values = c("National" = "red", "Average of Other States" = "steelblue"))
    
    ggsave("Hpsa11_National_vs_Others.png", p2, width = 10, height = 6)
    cat("✓ National vs Others comparison saved as 'Hpsa11_National_vs_Others.png'\n")
  }
  
  # STEP 7: Print summary reports
  cat("\n===========================================\n")
  cat("REGRESSION ANALYSIS SUMMARY\n")
  cat("===========================================\n")
  print(summary_stats)
  
  if(nrow(national_results) > 0) {
    cat("\n===========================================\n")
    cat("★ NATIONAL HIGHLIGHTS ★\n")
    cat("===========================================\n")
    cat("State Name:", national_results$State, "\n")
    cat("Equation:", national_results$Full_Equation, "\n")
    cat("Beta (Quarterly Change):", national_results$Beta_Rate_of_Change, "\n")
    cat("R-squared:", national_results$R_Squared, "\n")
    cat("P-value:", national_results$P_Value, national_results$Significance_Code, "\n")
    cat("Trend:", national_results$Trend_Classification, "\n")
    cat("Historical Average (2019-2025):", national_results$Average_2019_2025, "\n")
    cat("Interpretation:", 
        ifelse(national_results$Beta_Rate_of_Change > 0, 
               "National is INCREASING over time", 
               ifelse(national_results$Beta_Rate_of_Change < 0, 
                      "National is DECREASING over time",
                      "National is STABLE over time")), "\n")
  }
  
  cat("\n===========================================\n")
  cat("SIGNIFICANCE LEVELS (Excluding National)\n")
  cat("===========================================\n")
  print(significance_summary)
  
  cat("\n===========================================\n")
  cat("TOP 5 INCREASING STATES (Fastest Growth)\n")
  cat("===========================================\n")
  print(head(top_increasing, 5))
  
  cat("\n===========================================\n")
  cat("TOP 5 DECREASING STATES (Fastest Decline)\n")
  cat("===========================================\n")
  print(head(top_decreasing, 5))
  
  # Additional analysis: Where does National rank?
  if(nrow(national_results) > 0) {
    national_rank_beta <- sum(final_results$Beta_Rate_of_Change > national_results$Beta_Rate_of_Change) + 1
    national_rank_r2 <- sum(final_results$R_Squared > national_results$R_Squared) + 1
    
    cat("\n===========================================\n")
    cat("NATIONAL RANKINGS\n")
    cat("===========================================\n")
    cat("National Beta Rank:", national_rank_beta, "out of", nrow(final_results), "\n")
    cat("National R-squared Rank:", national_rank_r2, "out of", nrow(final_results), "\n")
  }
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
}

# Final diagnostic
cat("\n===========================================\n")
cat("DIAGNOSTIC INFORMATION\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list), "\n")
cat("Total states in dataset:", length(states_list), "\n")
cat("Success rate:", round(length(results_list)/length(states_list)*100, 1), "%\n")
cat("Number of quarters analyzed:", n_quarters, "\n")
cat("Time period: Q1 2019 - Q4 2025\n")

if(length(results_list) < length(states_list)) {
  failed_states <- setdiff(states_list, names(results_list))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
} else {
  cat("\n✓ All states (including National) were processed successfully!\n")
}

cat("\n===========================================\n")
cat("ANALYSIS COMPLETE FOR Hpsa11 (with National)!\n")
cat("===========================================\n")






# Load required libraries
library(tidyverse)
library(writexl)
library(ggplot2)
library(broom)
library(lubridate)
library(gridExtra)
# Load required libraries
library(tidyverse)
library(writexl)
library(ggplot2)
library(broom)
library(lubridate)
library(gridExtra)

#===========================================
# REGRESSION ANALYSIS FOR Hpsa22 DATASET
# Starting from Q1 2019 to Q4 2025
# INCLUDING NATIONAL ROW
#===========================================

cat("===========================================\n")
cat("REGRESSION ANALYSIS FOR Hpsa22 DATASET (with National)\n")
cat("===========================================\n\n")

# STEP 1: Clean and prepare data
cat("STEP 1: Preparing data for regression analysis...\n")
cat("-------------------------------------------\n")

# Make a copy of the data
Hpsa22_clean <- Hpsa22

# Convert all quarterly columns to numeric (ensure no character issues)
quarter_columns <- names(Hpsa22_clean)[-1]  # all columns except 'State'

for(col in quarter_columns) {
  Hpsa22_clean[[col]] <- suppressWarnings(as.numeric(as.character(Hpsa22_clean[[col]])))
}

# Check for National row
cat("Checking for National row...\n")
unique_states <- unique(Hpsa22_clean$State)
national_row_found <- FALSE
national_state_name <- NULL

if("National" %in% unique_states) {
  national_row_found <- TRUE
  national_state_name <- "National"
  cat("✓ Found 'National' row in data\n")
} else if("Natioal" %in% unique_states) {
  national_row_found <- TRUE
  national_state_name <- "Natioal"
  cat("✓ Found 'Natioal' row (likely typo for National)\n")
} else if(any(grepl("Nation", unique_states, ignore.case = TRUE))) {
  national_row_found <- TRUE
  national_state_name <- unique_states[grepl("Nation", unique_states, ignore.case = TRUE)][1]
  cat("✓ Found '", national_state_name, "' row (likely National)\n", sep = "")
} else {
  cat("⚠ No National row found. Please check the last row of your data.\n")
}

# Create a time index (quarters from Q1 2019 to Q4 2025)
n_quarters <- length(quarter_columns)
start_date <- as.Date("2019-01-01")
quarter_seq <- seq.Date(start_date, by = "quarter", length.out = n_quarters)
time_index <- 1:n_quarters

# Create a data frame with time periods for reference
time_periods <- data.frame(
  Quarter = quarter_columns,
  Time_Index = time_index,
  Date = quarter_seq,
  Year_Quarter = paste0(year(quarter_seq), " Q", quarter(quarter_seq))
)

cat("\nData ready for analysis\n")
cat("Number of quarters:", n_quarters, "\n")
cat("Time period:", min(time_periods$Year_Quarter), "to", max(time_periods$Year_Quarter), "\n")
cat("Total states (including National if present):", length(unique_states), "\n\n")

# Function to prepare state data for regression
prepare_state_data <- function(state_data, state_name) {
  # Extract quarterly values
  values <- as.numeric(unlist(state_data[,-1]))
  
  # Remove any NA values
  valid_idx <- !is.na(values)
  values <- values[valid_idx]
  time_idx <- time_index[valid_idx]
  
  # Check if we have enough data
  if(length(values) < 4) {
    return(NULL)
  }
  
  # Create data frame for regression
  df <- data.frame(
    Time = time_idx,
    Value = values,
    Quarter_Number = 1:length(values)
  )
  
  return(df)
}

# Function to perform linear regression and extract statistics
perform_regression <- function(df, state_name) {
  tryCatch({
    # Linear regression: Value ~ Time
    model <- lm(Value ~ Time, data = df)
    
    # Extract coefficients
    intercept <- coef(model)[1]
    beta <- coef(model)[2]  # rate of change per quarter
    
    # Model summary
    summary_model <- summary(model)
    
    # R-squared and Adjusted R-squared
    r_squared <- summary_model$r.squared
    adj_r_squared <- summary_model$adj.r.squared
    
    # P-value for the slope (beta)
    p_value <- summary_model$coefficients[2, 4]
    
    # F-statistic and overall model p-value
    f_statistic <- summary_model$fstatistic[1]
    f_p_value <- pf(f_statistic, summary_model$fstatistic[2], 
                    summary_model$fstatistic[3], lower.tail = FALSE)
    
    # Create full equation string
    if(beta >= 0) {
      equation <- paste0("Value = ", round(intercept, 4), " + ", 
                         round(beta, 6), " * Time")
    } else {
      equation <- paste0("Value = ", round(intercept, 4), " - ", 
                         round(abs(beta), 6), " * Time")
    }
    
    # Determine trend based on p-value and beta sign
    if(p_value < 0.05) {
      if(beta > 0) {
        trend <- "Increasing"
        trend_icon <- "↑"
      } else {
        trend <- "Decreasing"
        trend_icon <- "↓"
      }
    } else {
      trend <- "Stable (not significant)"
      trend_icon <- "→"
    }
    
    # Calculate historical average (2019-2025)
    historical_avg <- round(mean(df$Value), 2)
    historical_sd <- round(sd(df$Value), 2)
    historical_min <- round(min(df$Value), 2)
    historical_max <- round(max(df$Value), 2)
    
    # Calculate fitted values and residuals for additional metrics
    fitted_values <- fitted(model)
    residuals <- resid(model)
    
    # Calculate MAPE (Mean Absolute Percentage Error)
    mape <- tryCatch({
      mean(abs(residuals / df$Value), na.rm = TRUE) * 100
    }, error = function(e) NA)
    
    # Calculate RMSE
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    
    # Calculate MAE (Mean Absolute Error)
    mae <- mean(abs(residuals), na.rm = TRUE)
    
    # Calculate AIC and BIC
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Calculate standard error of beta
    beta_se <- summary_model$coefficients[2, 2]
    
    # Calculate t-statistic
    t_statistic <- summary_model$coefficients[2, 3]
    
    # Calculate confidence intervals for beta (95%)
    beta_ci_lower <- beta - 1.96 * beta_se
    beta_ci_upper <- beta + 1.96 * beta_se
    
    # Calculate Durbin-Watson test for autocorrelation
    dw_test <- tryCatch({
      car::durbinWatson(model)
    }, error = function(e) NA)
    
    # Create result dataframe
    result <- data.frame(
      State = state_name,
      Full_Equation = equation,
      Intercept = round(intercept, 4),
      Beta_Rate_of_Change = round(beta, 6),
      Beta_Std_Error = round(beta_se, 6),
      Beta_CI_Lower_95 = round(beta_ci_lower, 6),
      Beta_CI_Upper_95 = round(beta_ci_upper, 6),
      T_Statistic = round(t_statistic, 3),
      P_Value = round(p_value, 6),
      Significance_Code = ifelse(p_value < 0.001, "***", 
                                 ifelse(p_value < 0.01, "**",
                                        ifelse(p_value < 0.05, "*", "ns"))),
      F_Statistic = round(f_statistic, 2),
      Model_P_Value = round(f_p_value, 6),
      R_Squared = round(r_squared, 4),
      Adj_R_Squared = round(adj_r_squared, 4),
      MAPE_percent = round(mape, 2),
      RMSE = round(rmse, 4),
      MAE = round(mae, 4),
      AIC = round(aic, 2),
      BIC = round(bic, 2),
      Average_2019_2025 = historical_avg,
      Std_Dev_2019_2025 = historical_sd,
      Min_Value = historical_min,
      Max_Value = historical_max,
      Trend_Classification = trend,
      Trend_Icon = trend_icon,
      Num_Observations = nrow(df),
      stringsAsFactors = FALSE
    )
    
    # Add flag for National
    if(grepl("Natio|National", state_name, ignore.case = TRUE)) {
      result$Is_National <- "Yes"
    } else {
      result$Is_National <- "No"
    }
    
    return(result)
    
  }, error = function(e) {
    cat("  Error in regression for", state_name, ":", e$message, "\n")
    return(NULL)
  })
}

# STEP 2: Run regression for all states (including National)
cat("\nSTEP 2: Running regression analysis for all states (including National)...\n")
cat("-------------------------------------------\n")

results_list <- list()
states_list <- unique(Hpsa22_clean$State)

# Identify National row for special highlighting
if(national_row_found) {
  cat(paste0("\n★ Including National row: '", national_state_name, "' ★\n\n"))
} else {
  cat("\n⚠ No National row identified. Processing all states as regular states.\n\n")
}

for(state in states_list) {
  is_national <- grepl("Natio|National", state, ignore.case = TRUE)
  cat("Processing:", state, ifelse(is_national, " ★NATIONAL★", ""), "\n")
  
  # Get state data
  state_data <- Hpsa22_clean %>% filter(State == state)
  
  if(nrow(state_data) == 0) {
    cat("  No data for", state, "\n")
    next
  }
  
  # Prepare data for regression
  df <- prepare_state_data(state_data, state)
  
  if(is.null(df)) {
    cat("  Insufficient data for", state, "\n")
    next
  }
  
  # Perform regression
  result <- perform_regression(df, state)
  
  if(!is.null(result)) {
    results_list[[state]] <- result
    if(is_national) {
      cat("  ✓ NATIONAL completed - R²:", result$R_Squared, 
          ", Beta:", result$Beta_Rate_of_Change, 
          ", Trend:", result$Trend_Classification, "\n")
    } else {
      cat("  ✓ Completed - R²:", result$R_Squared, 
          ", Beta:", result$Beta_Rate_of_Change, 
          ", Trend:", result$Trend_Classification, "\n")
    }
  }
}

# STEP 3: Combine results
cat("\nSTEP 3: Combining results...\n")
cat("-------------------------------------------\n")

if(length(results_list) > 0) {
  final_results <- bind_rows(results_list)
  
  # Sort by state name (National will appear alphabetically)
  final_results <- final_results %>% arrange(State)
  
  # Extract National results separately
  national_results <- final_results %>%
    filter(grepl("Natio|National", State, ignore.case = TRUE))
  
  # Display first few rows
  cat("\nFirst 10 rows of results:\n")
  print(head(final_results, 10))
  
  if(nrow(national_results) > 0) {
    cat("\n★ NATIONAL RESULTS ★\n")
    print(national_results %>% 
            select(State, Full_Equation, Beta_Rate_of_Change, R_Squared, 
                   P_Value, Trend_Classification, Average_2019_2025))
  } else {
    cat("\n⚠ No National results found\n")
  }
  
  # STEP 4: Create comprehensive summary statistics
  cat("\nSTEP 4: Creating summary statistics...\n")
  cat("-------------------------------------------\n")
  
  # Summary including National
  states_excluding_national <- final_results %>% 
    filter(!grepl("Natio|National", State))
  
  summary_stats <- data.frame(
    Metric = c("Total States Processed",
               "States (excluding National)",
               "States with Increasing Trend",
               "States with Decreasing Trend",
               "States with Stable Trend",
               "Average R-squared",
               "Average Adj R-squared",
               "Average MAPE (%)",
               "Average RMSE",
               "Average Beta (Rate of Change)",
               "Median Beta",
               "Range of Beta (Min)",
               "Range of Beta (Max)",
               "Total Quarters Analyzed",
               "Time Period"),
    Value = c(
      nrow(final_results),
      nrow(states_excluding_national),
      sum(states_excluding_national$Trend_Classification == "Increasing"),
      sum(states_excluding_national$Trend_Classification == "Decreasing"),
      sum(states_excluding_national$Trend_Classification == "Stable (not significant)"),
      round(mean(states_excluding_national$R_Squared, na.rm = TRUE), 4),
      round(mean(states_excluding_national$Adj_R_Squared, na.rm = TRUE), 4),
      round(mean(states_excluding_national$MAPE_percent, na.rm = TRUE), 2),
      round(mean(states_excluding_national$RMSE, na.rm = TRUE), 4),
      round(mean(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 6),
      round(median(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 6),
      round(min(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 6),
      round(max(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 6),
      n_quarters,
      paste0(min(time_periods$Year_Quarter), " to ", max(time_periods$Year_Quarter))
    )
  )
  
  # National comparison with others
  if(nrow(national_results) > 0) {
    national_comparison <- data.frame(
      Metric = c("National Beta", "Average Beta (Other States)",
                 "Difference (National - Others)",
                 "National R-squared", "Average R-squared (Other States)",
                 "Difference (National - Others)",
                 "National MAPE (%)", "Average MAPE (Other States)",
                 "National Average Value", "Average Value (Other States)",
                 "National Trend", "Most Common Trend (Other States)"),
      Value = c(
        national_results$Beta_Rate_of_Change[1],
        round(mean(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 6),
        round(national_results$Beta_Rate_of_Change[1] - 
                mean(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 6),
        national_results$R_Squared[1],
        round(mean(states_excluding_national$R_Squared, na.rm = TRUE), 4),
        round(national_results$R_Squared[1] - 
                mean(states_excluding_national$R_Squared, na.rm = TRUE), 4),
        national_results$MAPE_percent[1],
        round(mean(states_excluding_national$MAPE_percent, na.rm = TRUE), 2),
        national_results$Average_2019_2025[1],
        round(mean(states_excluding_national$Average_2019_2025, na.rm = TRUE), 2),
        national_results$Trend_Classification[1],
        names(sort(table(states_excluding_national$Trend_Classification), decreasing = TRUE)[1])
      )
    )
    
    cat("\n★ National vs Other States Comparison ★\n")
    print(national_comparison)
  }
  
  # Significance summary
  significance_summary <- states_excluding_national %>%
    group_by(Significance_Code) %>%
    summarise(Count = n()) %>%
    mutate(Interpretation = case_when(
      Significance_Code == "***" ~ "p < 0.001 (Highly significant)",
      Significance_Code == "**" ~ "p < 0.01 (Significant)",
      Significance_Code == "*" ~ "p < 0.05 (Marginally significant)",
      Significance_Code == "ns" ~ "p >= 0.05 (Not significant)"
    ))
  
  # Top increasing and decreasing states (excluding National)
  top_increasing <- states_excluding_national %>%
    filter(Trend_Classification == "Increasing") %>%
    arrange(desc(Beta_Rate_of_Change)) %>%
    select(State, Beta_Rate_of_Change, R_Squared, P_Value, Trend_Icon) %>%
    head(10)
  
  top_decreasing <- states_excluding_national %>%
    filter(Trend_Classification == "Decreasing") %>%
    arrange(Beta_Rate_of_Change) %>%
    select(State, Beta_Rate_of_Change, R_Squared, P_Value, Trend_Icon) %>%
    head(10)
  
  # Best and worst fit states (excluding National)
  best_fit <- states_excluding_national %>%
    arrange(desc(R_Squared)) %>%
    select(State, R_Squared, Adj_R_Squared, MAPE_percent, Beta_Rate_of_Change, Trend_Classification) %>%
    head(10)
  
  worst_fit <- states_excluding_national %>%
    arrange(R_Squared) %>%
    select(State, R_Squared, Adj_R_Squared, MAPE_percent, Beta_Rate_of_Change, Trend_Classification) %>%
    head(10)
  
  # States with most dramatic changes
  most_volatile <- states_excluding_national %>%
    arrange(desc(abs(Beta_Rate_of_Change))) %>%
    select(State, Beta_Rate_of_Change, Trend_Classification, R_Squared, MAPE_percent) %>%
    head(10)
  
  # Most stable states (lowest absolute beta)
  most_stable <- states_excluding_national %>%
    arrange(abs(Beta_Rate_of_Change)) %>%
    select(State, Beta_Rate_of_Change, Trend_Classification, R_Squared, Average_2019_2025) %>%
    head(10)
  
  # STEP 5: Export to Excel
  cat("\nSTEP 5: Exporting to Excel...\n")
  cat("-------------------------------------------\n")
  
  # Create list of sheets for Excel
  excel_sheets_list <- list(
    Detailed_Results = final_results,
    Summary_Statistics = summary_stats,
    National_Results = national_results,
    Significance_Summary = significance_summary,
    Top_Increasing_States = top_increasing,
    Top_Decreasing_States = top_decreasing,
    Best_Fit_States = best_fit,
    Worst_Fit_States = worst_fit,
    Most_Volatile_States = most_volatile,
    Most_Stable_States = most_stable,
    Regression_Coefficients = final_results %>%
      select(State, Intercept, Beta_Rate_of_Change, Beta_Std_Error, 
             Beta_CI_Lower_95, Beta_CI_Upper_95, T_Statistic, P_Value, Significance_Code),
    Model_Fit_Metrics = final_results %>%
      select(State, R_Squared, Adj_R_Squared, MAPE_percent, RMSE, MAE, AIC, BIC),
    Historical_Summary = final_results %>%
      select(State, Average_2019_2025, Std_Dev_2019_2025, Min_Value, Max_Value, Trend_Classification),
    Trend_Summary = final_results %>%
      group_by(Trend_Classification) %>%
      summarise(
        Count = n(),
        Percentage = round(n()/nrow(final_results)*100, 1),
        Average_Beta = round(mean(Beta_Rate_of_Change), 6),
        Average_R2 = round(mean(R_Squared), 4),
        Average_MAPE = round(mean(MAPE_percent, na.rm = TRUE), 2)
      )
  )
  
  # Add national comparison if available
  if(exists("national_comparison") && nrow(national_results) > 0) {
    excel_sheets_list$National_Comparison = national_comparison
  }
  
  write_xlsx(excel_sheets_list, "Regression_Hpsa22_Analysis_with_National.xlsx")
  
  cat("✓ Results saved to 'Regression_Hpsa22_Analysis_with_National.xlsx'\n\n")
  
  # STEP 6: Create visualizations
  cat("STEP 6: Creating visualizations...\n")
  cat("-------------------------------------------\n")
  
  # Create summary plots
  png(filename = "Hpsa22_Regression_Summary_with_National.png", width = 1600, height = 1200)
  par(mfrow = c(2, 3))
  
  # 1. Distribution of R-squared values (highlight National)
  hist(states_excluding_national$R_Squared, 
       main = "Distribution of R-squared Values\n(Red line = National)",
       xlab = "R-squared", 
       col = "steelblue",
       breaks = 20,
       xlim = c(0, 1))
  if(nrow(national_results) > 0) {
    abline(v = national_results$R_Squared[1], col = "red", lwd = 3, lty = 2)
    legend("topright", legend = paste("National:", round(national_results$R_Squared[1], 3)), 
           col = "red", lwd = 3, lty = 2)
  }
  abline(v = mean(states_excluding_national$R_Squared, na.rm = TRUE), 
         col = "darkgreen", lwd = 2, lty = 3)
  
  # 2. Distribution of Beta (highlight National)
  hist(states_excluding_national$Beta_Rate_of_Change, 
       main = "Distribution of Beta (Rate of Change)\n(Red line = National)",
       xlab = "Beta Value (Change per Quarter)", 
       col = "lightgreen",
       breaks = 20)
  if(nrow(national_results) > 0) {
    abline(v = national_results$Beta_Rate_of_Change[1], col = "red", lwd = 3, lty = 2)
  }
  abline(v = 0, col = "black", lwd = 1, lty = 2)
  abline(v = mean(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE), 
         col = "blue", lwd = 2, lty = 3)
  
  # 3. Trend classification pie chart (including National)
  trend_counts <- table(final_results$Trend_Classification)
  pie(trend_counts, 
      main = "Trend Classification\n(Includes National)",
      col = c("green", "red", "gray"),
      cex = 0.8)
  
  # 4. Significance levels bar plot
  sig_counts <- table(states_excluding_national$Significance_Code)
  barplot(sig_counts, 
          main = "Statistical Significance Levels\n(Excluding National)",
          xlab = "Significance Code", 
          ylab = "Number of States",
          col = c("darkred", "red", "pink", "lightgray"),
          ylim = c(0, max(sig_counts) * 1.1))
  
  # 5. Top 10 states by R-squared (including National if in top 10)
  top10_r2 <- head(final_results[order(-final_results$R_Squared),], 10)
  bar_colors <- ifelse(grepl("Natio|National", top10_r2$State), "red", "steelblue")
  barplot(top10_r2$R_Squared, 
          names.arg = top10_r2$State,
          main = "Top 10 States by R-squared\n(Red=National)",
          ylab = "R-squared",
          col = bar_colors,
          las = 2,
          cex.names = 0.7,
          ylim = c(0, max(top10_r2$R_Squared) * 1.1))
  
  # 6. Top 10 absolute Beta values
  top10_beta <- head(states_excluding_national[order(-abs(states_excluding_national$Beta_Rate_of_Change)),], 10)
  bar_colors2 <- ifelse(top10_beta$Beta_Rate_of_Change > 0, "green", "red")
  barplot(top10_beta$Beta_Rate_of_Change, 
          names.arg = top10_beta$State,
          main = "Top 10 States by |Beta| (Rate of Change)\n(Green=Increasing, Red=Decreasing)",
          ylab = "Beta Value",
          col = bar_colors2,
          las = 2,
          cex.names = 0.7)
  legend("topright", legend = c("Increasing", "Decreasing"), fill = c("green", "red"))
  
  dev.off()
  cat("✓ Summary visualization saved as 'Hpsa22_Regression_Summary_with_National.png'\n")
  
  # Create detailed trend plot including National
  if(nrow(national_results) > 0) {
    selected_states_for_plot <- c(national_results$State[1], 
                                  head(top_increasing$State, 2), 
                                  head(top_decreasing$State, 2))
    selected_states_for_plot <- unique(selected_states_for_plot[1:min(5, length(selected_states_for_plot))])
  } else {
    selected_states_for_plot <- c(head(top_increasing$State, 2), 
                                  head(top_decreasing$State, 2),
                                  head(most_stable$State, 1))
    selected_states_for_plot <- unique(selected_states_for_plot[1:5])
  }
  
  # Prepare time series data for plotting
  plot_data <- data.frame()
  for(state in selected_states_for_plot) {
    state_data <- Hpsa22_clean %>% filter(State == state)
    if(nrow(state_data) > 0) {
      values <- as.numeric(unlist(state_data[,-1]))
      valid_idx <- !is.na(values)
      values <- values[valid_idx]
      time_idx <- time_index[valid_idx]
      
      # Get trend info for this state
      state_result <- final_results %>% filter(State == state)
      trend_info <- ifelse(nrow(state_result) > 0, 
                           paste(state_result$Trend_Icon[1], state_result$Trend_Classification[1]),
                           "Unknown")
      
      temp_df <- data.frame(
        State = paste0(state, "\n(", trend_info, ")"),
        Time = time_idx,
        Value = values
      )
      plot_data <- rbind(plot_data, temp_df)
    }
  }
  
  # Create trend lines plot
  p <- ggplot(plot_data, aes(x = Time, y = Value, color = State, group = State)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
    labs(title = "Linear Trends for Selected States (Hpsa22)",
         subtitle = paste0("Q1 2019 to Q4 2025 | Based on ", n_quarters, " quarters of data"),
         x = "Time Index (Quarters)", 
         y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10)) +
    scale_color_brewer(palette = "Set1")
  
  ggsave("Hpsa22_Trends_Selected_States.png", p, width = 12, height = 7)
  cat("✓ Trends plot for selected states saved as 'Hpsa22_Trends_Selected_States.png'\n")
  
  # Create National vs Others comparison plot if National exists
  if(nrow(national_results) > 0) {
    # Calculate fitted lines for National and average of others
    national_fitted <- data.frame(
      Time = time_index,
      Actual = national_ts <- {
        nat_data <- Hpsa22_clean %>% filter(State == national_results$State[1])
        as.numeric(unlist(nat_data[,-1]))
      },
      Fitted = national_results$Intercept[1] + national_results$Beta_Rate_of_Change[1] * time_index,
      Type = "National"
    )
    
    avg_beta <- mean(states_excluding_national$Beta_Rate_of_Change, na.rm = TRUE)
    avg_intercept <- mean(states_excluding_national$Intercept, na.rm = TRUE)
    
    avg_fitted <- data.frame(
      Time = time_index,
      Actual = NA,
      Fitted = avg_intercept + avg_beta * time_index,
      Type = "Average of Other States"
    )
    
    comparison_data <- rbind(national_fitted, avg_fitted)
    
    p2 <- ggplot(comparison_data, aes(x = Time, y = Fitted, color = Type, linetype = Type)) +
      geom_line(size = 1.2) +
      geom_point(data = national_fitted, aes(y = Actual), size = 1, alpha = 0.5) +
      labs(title = "National Trend vs Average of Other States",
           subtitle = paste0("National Beta: ", round(national_results$Beta_Rate_of_Change[1], 4),
                             " | Avg Others Beta: ", round(avg_beta, 4),
                             "\nNational R²: ", round(national_results$R_Squared[1], 3),
                             " | Avg Others R²: ", round(mean(states_excluding_national$R_Squared), 3)),
           x = "Time Index (Quarters)", 
           y = "Value",
           caption = "Points show actual National values") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_manual(values = c("National" = "red", "Average of Other States" = "steelblue"))
    
    ggsave("Hpsa22_National_vs_Others.png", p2, width = 10, height = 6)
    cat("✓ National vs Others comparison saved as 'Hpsa22_National_vs_Others.png'\n")
    
    # Create National forecast plot
    national_ts_data <- prepare_state_data(
      Hpsa22_clean %>% filter(State == national_results$State[1]), 
      national_results$State[1]
    )
    
    if(!is.null(national_ts_data)) {
      national_lm <- lm(Value ~ Time, data = national_ts_data)
      future_times <- data.frame(Time = seq(max(national_ts_data$Time) + 1, 
                                            max(national_ts_data$Time) + 8, by = 1))
      predictions <- predict(national_lm, newdata = future_times, interval = "confidence", level = 0.95)
      
      forecast_df <- data.frame(
        Time = c(national_ts_data$Time, future_times$Time),
        Value = c(national_ts_data$Value, predictions[,1]),
        Lower = c(rep(NA, nrow(national_ts_data)), predictions[,2]),
        Upper = c(rep(NA, nrow(national_ts_data)), predictions[,3]),
        Type = c(rep("Historical", nrow(national_ts_data)), rep("Forecast", nrow(future_times)))
      )
      
      p3 <- ggplot(forecast_df, aes(x = Time, y = Value)) +
        geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Type), alpha = 0.3) +
        geom_line(data = subset(forecast_df, Type == "Historical"), color = "blue", size = 1) +
        geom_line(data = subset(forecast_df, Type == "Forecast"), color = "red", size = 1, linetype = "dashed") +
        geom_point(data = subset(forecast_df, Type == "Historical"), color = "blue", size = 1) +
        labs(title = paste("National (", national_results$State[1], ") - Linear Trend Forecast"),
             subtitle = "Based on Q1 2019 to Q4 2025 data | 95% Confidence Interval",
             x = "Time Index (Quarters)", 
             y = "Value") +
        theme_minimal() +
        scale_fill_manual(values = c("Historical" = "gray80", "Forecast" = "pink"))
      
      ggsave("Hpsa22_National_Forecast.png", p3, width = 10, height = 6)
      cat("✓ National forecast plot saved as 'Hpsa22_National_Forecast.png'\n")
    }
  }
  
  # Create heatmap of Beta values by state
  if(nrow(states_excluding_national) >= 10) {
    beta_heatmap_data <- states_excluding_national %>%
      arrange(desc(Beta_Rate_of_Change)) %>%
      mutate(State = factor(State, levels = State),
             Direction = ifelse(Beta_Rate_of_Change > 0, "Positive", "Negative"))
    
    p4 <- ggplot(beta_heatmap_data, aes(x = 1, y = State, fill = Beta_Rate_of_Change)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                           midpoint = 0, name = "Beta Value") +
      labs(title = "Rate of Change by State (Beta Values)",
           subtitle = "Green = Increasing, Red = Decreasing",
           x = "", y = "State") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid = element_blank())
    
    ggsave("Hpsa22_Beta_Heatmap.png", p4, width = 8, height = 12)
    cat("✓ Beta heatmap saved as 'Hpsa22_Beta_Heatmap.png'\n")
  }
  
  # STEP 7: Print summary reports
  cat("\n===========================================\n")
  cat("REGRESSION ANALYSIS SUMMARY\n")
  cat("===========================================\n")
  print(summary_stats)
  
  if(nrow(national_results) > 0) {
    cat("\n===========================================\n")
    cat("★ NATIONAL HIGHLIGHTS ★\n")
    cat("===========================================\n")
    cat("State Name:", national_results$State[1], "\n")
    cat("Equation:", national_results$Full_Equation[1], "\n")
    cat("Beta (Quarterly Change):", national_results$Beta_Rate_of_Change[1], "\n")
    cat("R-squared:", national_results$R_Squared[1], "\n")
    cat("P-value:", national_results$P_Value[1], national_results$Significance_Code[1], "\n")
    cat("Trend:", national_results$Trend_Classification[1], national_results$Trend_Icon[1], "\n")
    cat("Historical Average (2019-2025):", national_results$Average_2019_2025[1], "\n")
    cat("Historical Std Dev:", national_results$Std_Dev_2019_2025[1], "\n")
    cat("Range:", national_results$Min_Value[1], "to", national_results$Max_Value[1], "\n")
    cat("\nInterpretation:", 
        ifelse(national_results$Beta_Rate_of_Change[1] > 0, 
               "National is INCREASING over time", 
               ifelse(national_results$Beta_Rate_of_Change[1] < 0, 
                      "National is DECREASING over time",
                      "National is STABLE over time")), "\n")
    
    # Add interpretation of magnitude
    beta_abs <- abs(national_results$Beta_Rate_of_Change[1])
    if(beta_abs < 0.01) {
      cat("Magnitude: Very slow change\n")
    } else if(beta_abs < 0.05) {
      cat("Magnitude: Moderate change\n")
    } else if(beta_abs < 0.1) {
      cat("Magnitude: Fast change\n")
    } else {
      cat("Magnitude: Very fast change\n")
    }
  }
  
  cat("\n===========================================\n")
  cat("SIGNIFICANCE LEVELS (Excluding National)\n")
  cat("===========================================\n")
  print(significance_summary)
  
  cat("\n===========================================\n")
  cat("TOP 5 INCREASING STATES (Fastest Growth)\n")
  cat("===========================================\n")
  print(head(top_increasing, 5))
  
  cat("\n===========================================\n")
  cat("TOP 5 DECREASING STATES (Fastest Decline)\n")
  cat("===========================================\n")
  print(head(top_decreasing, 5))
  
  cat("\n===========================================\n")
  cat("MOST VOLATILE STATES (Highest |Beta|)\n")
  cat("===========================================\n")
  print(head(most_volatile, 5))
  
  cat("\n===========================================\n")
  cat("MOST STABLE STATES (Lowest |Beta|)\n")
  cat("===========================================\n")
  print(head(most_stable, 5))
  
  # Where does National rank?
  if(nrow(national_results) > 0 && nrow(states_excluding_national) > 0) {
    national_beta <- national_results$Beta_Rate_of_Change[1]
    national_r2 <- national_results$R_Squared[1]
    
    national_rank_beta_abs <- sum(abs(states_excluding_national$Beta_Rate_of_Change) > abs(national_beta)) + 1
    national_rank_beta <- sum(states_excluding_national$Beta_Rate_of_Change > national_beta) + 1
    national_rank_r2 <- sum(states_excluding_national$R_Squared > national_r2) + 1
    
    cat("\n===========================================\n")
    cat("NATIONAL RANKINGS\n")
    cat("===========================================\n")
    cat("National Beta Rank (absolute):", national_rank_beta_abs, "out of", nrow(states_excluding_national), "\n")
    cat("  (1 = most volatile,", nrow(states_excluding_national), "= most stable)\n")
    cat("National Beta Rank (directional):", national_rank_beta, "out of", nrow(states_excluding_national), "\n")
    cat("  (1 = highest increasing,", nrow(states_excluding_national), "= highest decreasing)\n")
    cat("National R-squared Rank:", national_rank_r2, "out of", nrow(states_excluding_national), "\n")
    cat("  (1 = best fit,", nrow(states_excluding_national), "= worst fit)\n")
  }
  
} else {
  cat("✗ No results generated. Please check your data format.\n")
}

# Final diagnostic
cat("\n===========================================\n")
cat("DIAGNOSTIC INFORMATION\n")
cat("===========================================\n")
cat("Number of states successfully analyzed:", length(results_list), "\n")
cat("Total states in dataset:", length(states_list), "\n")
cat("Success rate:", round(length(results_list)/length(states_list)*100, 1), "%\n")
cat("Number of quarters analyzed:", n_quarters, "\n")
cat("Time period:", min(time_periods$Year_Quarter), "to", max(time_periods$Year_Quarter), "\n")

if(length(results_list) < length(states_list)) {
  failed_states <- setdiff(states_list, names(results_list))
  cat("\nStates that failed:", paste(failed_states, collapse = ", "), "\n")
} else {
  cat("\n✓ All states (including National) were processed successfully!\n")
}

cat("\n===========================================\n")
cat("ANALYSIS COMPLETE FOR Hpsa22 (with National)!\n")
cat("===========================================\n")
cat("\nOutput Files Created:\n")
cat("1. Regression_Hpsa22_Analysis_with_National.xlsx - Main Excel results\n")
cat("2. Hpsa22_Regression_Summary_with_National.png - Summary visualizations\n")
cat("3. Hpsa22_Trends_Selected_States.png - Trend lines for selected states\n")
if(nrow(national_results) > 0) {
  cat("4. Hpsa22_National_vs_Others.png - National vs others comparison\n")
  cat("5. Hpsa22_National_Forecast.png - National forecast plot\n")
}
cat("6. Hpsa22_Beta_Heatmap.png - Heatmap of Beta values by state\n")





Hpsa22


# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(openxlsx)
library(lubridate)

# Assuming your data is already loaded as Hpsa22
# Hpsa22 <- your_data_frame

# Function to convert quarterly data to time series and fit auto.arima
state_arima_analysis <- function(state_data, state_name) {
  
  # Extract quarterly values (assuming columns from Q12019 to Q42025)
  quarterly_values <- state_data %>%
    select(starts_with("Q")) %>%
    as.numeric()
  
  # Create time series object (quarterly, starting 2019 Q1)
  ts_data <- ts(quarterly_values, 
                start = c(2019, 1), 
                frequency = 4)
  
  # Auto ARIMA model
  model <- auto.arima(ts_data, 
                      seasonal = TRUE,
                      stepwise = FALSE,
                      approximation = FALSE,
                      trace = FALSE)
  
  # Model summary
  model_summary <- summary(model)
  
  # Extract model components
  arima_order <- paste0("ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], 
                        ")(", model$arma[3], ",", model$arma[7], ",", model$arma[4], ")[", model$arma[5], "]")
  
  # Get coefficients
  coeff <- coef(model)
  
  # Identify intercept (may be named "intercept" or "mu")
  if("intercept" %in% names(coeff)) {
    intercept <- coeff["intercept"]
  } else if("mu" %in% names(coeff)) {
    intercept <- coeff["mu"]
  } else {
    intercept <- NA
  }
  
  # Rate of change (first AR coefficient as a proxy for trend)
  ar_coeff <- ifelse("ar1" %in% names(coeff), coeff["ar1"], NA)
  
  # Calculate fitted values
  fitted_values <- fitted(model)
  
  # Calculate MAPE manually
  mape <- mean(abs((ts_data - fitted_values) / ts_data), na.rm = TRUE) * 100
  
  # RMSE
  rmse <- sqrt(mean((ts_data - fitted_values)^2, na.rm = TRUE))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # Extract R-squared (approximate using correlation between actual and fitted)
  r_squared <- cor(ts_data, fitted_values)^2
  
  # Adjusted R-squared
  n <- length(ts_data)
  p <- length(coeff)
  r_squared_adj <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
  
  # Ljung-Box test p-value (test for residual autocorrelation)
  lb_test <- Box.test(residuals(model), type = "Ljung-Box", lag = 10)
  p_value <- lb_test$p.value
  
  # Calculate average from 2018-2025 (note: data starts 2019)
  avg_2018_2025 <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 2026-2030 (5 years of quarterly forecasts = 20 quarters ahead)
  forecast_periods <- 20 # 5 years * 4 quarters
  forecast_result <- forecast(model, h = forecast_periods)
  
  # Extract annual projected values (average of quarters for each year)
  forecast_vals <- as.numeric(forecast_result$mean)
  
  projected_2026 <- mean(forecast_vals[29:32], na.rm = TRUE)   # 2026 Q1-Q4
  projected_2027 <- mean(forecast_vals[33:36], na.rm = TRUE)   # 2027 Q1-Q4
  projected_2028 <- mean(forecast_vals[37:40], na.rm = TRUE)   # 2028 Q1-Q4
  projected_2029 <- mean(forecast_vals[41:44], na.rm = TRUE)   # 2029 Q1-Q4
  projected_2030 <- mean(forecast_vals[45:48], na.rm = TRUE)   # 2030 Q1-Q4
  
  # Determine trend significance
  # Extract trend component (if exists in model)
  trend_slope <- NA
  if("drift" %in% names(coeff)) {
    trend_slope <- coeff["drift"]
  } else if("trend" %in% names(coeff)) {
    trend_slope <- coeff["trend"]
  }
  
  # Check significance of trend using confidence intervals
  if(!is.na(trend_slope)) {
    conf_int <- tryCatch({
      confint(model, level = 0.95)
    }, error = function(e) NULL)
    
    if(!is.null(conf_int) && "drift" %in% rownames(conf_int)) {
      if(conf_int["drift", 1] > 0 && conf_int["drift", 2] > 0) {
        trend <- "Increasing (significant)"
      } else if(conf_int["drift", 1] < 0 && conf_int["drift", 2] < 0) {
        trend <- "Decreasing (significant)"
      } else {
        trend <- "Stable (non-significant)"
      }
    } else {
      trend <- "Stable (non-significant)"
    }
  } else {
    trend <- "Stable (non-significant)"
  }
  
  # Build equation string
  equation_parts <- c()
  if(!is.na(intercept)) {
    equation_parts <- c(equation_parts, paste0(round(intercept, 4)))
  }
  
  # Add AR terms
  ar_terms <- model$arma[1]
  if(ar_terms > 0) {
    for(i in 1:ar_terms) {
      ar_name <- paste0("ar", i)
      if(ar_name %in% names(coeff)) {
        equation_parts <- c(equation_parts, paste0(ifelse(coeff[ar_name] >= 0, "+", ""), 
                                                   round(coeff[ar_name], 4), " * y_{t-", i, "}"))
      }
    }
  }
  
  # Add MA terms
  ma_terms <- model$arma[2]
  if(ma_terms > 0) {
    for(i in 1:ma_terms) {
      ma_name <- paste0("ma", i)
      if(ma_name %in% names(coeff)) {
        equation_parts <- c(equation_parts, paste0(ifelse(coeff[ma_name] >= 0, "+", ""), 
                                                   round(coeff[ma_name], 4), " * ε_{t-", i, "}"))
      }
    }
  }
  
  equation <- paste(equation_parts, collapse = " ")
  if(nchar(equation) == 0) equation <- "Constant model"
  
  # Create result tibble
  result <- tibble(
    State = state_name,
    ARIMA_Model = arima_order,
    Full_Equation = equation,
    Intercept = intercept,
    Rate_of_Change_or_Beta = ar_coeff,
    R2 = r_squared,
    R2_Adj = r_squared_adj,
    MAPE = mape,
    RMSE = rmse,
    AIC = aic,
    BIC = bic,
    Ljung_Box_p_value = p_value,
    Average_2018_2025 = avg_2018_2025,
    Projected_2026 = projected_2026,
    Projected_2027 = projected_2027,
    Projected_2028 = projected_2028,
    Projected_2029 = projected_2029,
    Projected_2030 = projected_2030,
    Trend = trend
  )
  
  return(result)
}

# Process all states
results_list <- list()

for(i in 1:nrow(Hpsa22)) {
  state_name <- Hpsa22$State[i]
  state_data <- Hpsa22[i, ]
  
  cat("Processing:", state_name, "\n")
  
  tryCatch({
    result <- state_arima_analysis(state_data, state_name)
    results_list[[i]] <- result
  }, error = function(e) {
    cat("Error processing", state_name, ":", e$message, "\n")
    # Return NA results for failed states
    results_list[[i]] <- tibble(
      State = state_name,
      ARIMA_Model = "ERROR",
      Full_Equation = "Model failed to converge",
      Intercept = NA,
      Rate_of_Change_or_Beta = NA,
      R2 = NA,
      R2_Adj = NA,
      MAPE = NA,
      RMSE = NA,
      AIC = NA,
      BIC = NA,
      Ljung_Box_p_value = NA,
      Average_2018_2025 = NA,
      Projected_2026 = NA,
      Projected_2027 = NA,
      Projected_2028 = NA,
      Projected_2029 = NA,
      Projected_2030 = NA,
      Trend = "Model Failed"
    )
  })
}

# Combine all results
final_results <- bind_rows(results_list)

# Export to Excel
write.xlsx(final_results, "AutoARIMA_State_Analysis_Results.xlsx", 
           sheetName = "ARIMA_Results", 
           rowNames = FALSE,
           colWidths = "auto")

# Also save as CSV for backup
write.csv(final_results, "AutoARIMA_State_Analysis_Results.csv", row.names = FALSE)

# Print summary
cat("\n\nAnalysis Complete!\n")
cat("Total states processed:", nrow(final_results), "\n")
cat("Successfully modeled:", sum(!is.na(final_results$R2)), "\n")
cat("Failed:", sum(is.na(final_results$R2)), "\n")
cat("\nResults saved to: AutoARIMA_State_Analysis_Results.xlsx\n")

# Display first few results
print(head(final_results, 10))

# Optional: Create diagnostic plots for first state
if(nrow(final_results) > 0 && !is.na(final_results$R2[1])) {
  # Extract first state data for plotting example
  first_state <- Hpsa22$State[1]
  first_data <- Hpsa22[1, ] %>% select(starts_with("Q")) %>% as.numeric()
  ts_first <- ts(first_data, start = c(2019, 1), frequency = 4)
  model_first <- auto.arima(ts_first, seasonal = TRUE)
  
  # Create diagnostic plots
  pdf("AutoARIMA_Diagnostic_Plots.pdf", width = 11, height = 8.5)
  par(mfrow = c(2, 2))
  plot(forecast(model_first, h = 20), main = paste("Forecast for", first_state))
  acf(residuals(model_first), main = "ACF of Residuals")
  pacf(residuals(model_first), main = "PACF of Residuals")
  hist(residuals(model_first), main = "Histogram of Residuals", xlab = "Residuals")
  dev.off()
  
  cat("Diagnostic plots saved to: AutoARIMA_Diagnostic_Plots.pdf\n")
}

# Create a summary statistics file
summary_stats <- final_results %>%
  summarise(
    Average_R2 = mean(R2, na.rm = TRUE),
    Average_MAPE = mean(MAPE, na.rm = TRUE),
    Average_RMSE = mean(RMSE, na.rm = TRUE),
    Total_Increasing_Trend = sum(Trend == "Increasing (significant)", na.rm = TRUE),
    Total_Decreasing_Trend = sum(Trend == "Decreasing (significant)", na.rm = TRUE),
    Total_Stable_Trend = sum(Trend == "Stable (non-significant)", na.rm = TRUE)
  )

write.xlsx(summary_stats, "AutoARIMA_Summary_Statistics.xlsx", 
           sheetName = "Summary", rowNames = FALSE)

cat("\nSummary statistics saved to: AutoARIMA_Summary_Statistics.xlsx\n")



# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(openxlsx)
library(lubridate)
# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(openxlsx)

# First, let's properly structure your data
# Assuming your data is already loaded as Hpsa22
# Hpsa22 <- read.csv("your_file.csv") or however you loaded it

# Function to clean and prepare time series data
prepare_timeseries <- function(state_data, state_name) {
  # Extract quarterly values - exclude State column and any non-quarter columns
  quarterly_cols <- state_data %>%
    select(-State) %>%
    select(matches("^Q\\d{4}$")) %>%
    as.numeric()
  
  # Remove NAs
  quarterly_values <- quarterly_cols[!is.na(quarterly_cols)]
  
  # Check length
  if(length(quarterly_values) == 0) {
    return(NULL)
  }
  
  # Create time series (27 quarters from 2019 Q1 to 2025 Q4)
  ts_data <- ts(quarterly_values, 
                start = c(2019, 1), 
                end = c(2025, 4),
                frequency = 4)
  
  return(ts_data)
}

# Main analysis function
state_arima_analysis <- function(state_data, state_name) {
  
  cat("Processing:", state_name, "\n")
  
  # Prepare time series
  ts_data <- prepare_timeseries(state_data, state_name)
  
  if(is.null(ts_data) || length(ts_data) < 8) {
    return(create_error_result(state_name, "Insufficient data"))
  }
  
  # Try auto.arima with simpler settings for problematic series
  model <- tryCatch({
    auto.arima(ts_data, 
               seasonal = TRUE,
               stepwise = TRUE,
               approximation = TRUE,
               trace = FALSE)
  }, error = function(e) {
    # Fallback to non-seasonal model
    tryCatch({
      auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE)
    }, error = function(e2) {
      NULL
    })
  })
  
  if(is.null(model)) {
    return(create_error_result(state_name, "Model failed to converge"))
  }
  
  # Extract model components
  arima_order <- paste0("ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], 
                        ")(", model$arma[3], ",", model$arma[7], ",", model$arma[4], ")[", model$arma[5], "]")
  
  # Get coefficients
  coeff <- coef(model)
  
  # Identify intercept
  intercept <- NA
  if("intercept" %in% names(coeff)) {
    intercept <- coeff["intercept"]
  } else if("mu" %in% names(coeff)) {
    intercept <- coeff["mu"]
  }
  
  # Rate of change (first non-seasonal AR coefficient)
  ar_coeff <- NA
  if("ar1" %in% names(coeff)) {
    ar_coeff <- coeff["ar1"]
  } else if("sar1" %in% names(coeff)) {
    ar_coeff <- coeff["sar1"]
  }
  
  # Calculate fitted values and metrics
  fitted_values <- fitted(model)
  residuals_model <- residuals(model)
  
  # MAPE (avoid division by zero)
  non_zero_idx <- ts_data != 0
  if(sum(non_zero_idx) > 0) {
    mape <- mean(abs((ts_data[non_zero_idx] - fitted_values[non_zero_idx]) / ts_data[non_zero_idx]), na.rm = TRUE) * 100
  } else {
    mape <- NA
  }
  
  # RMSE
  rmse <- sqrt(mean((ts_data - fitted_values)^2, na.rm = TRUE))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # R-squared (correlation between actual and fitted)
  r_squared <- cor(ts_data, fitted_values)^2
  
  # Adjusted R-squared
  n <- length(ts_data)
  p <- length(coeff)
  r_squared_adj <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
  
  # Ljung-Box test
  lag_val <- min(10, floor(length(residuals_model)/4))
  lb_test <- Box.test(residuals_model, type = "Ljung-Box", lag = lag_val)
  p_value <- lb_test$p.value
  
  # Average of available data
  avg_available <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 20 quarters (5 years: 2026-2030)
  forecast_result <- tryCatch({
    forecast(model, h = 20)
  }, error = function(e) {
    NULL
  })
  
  if(is.null(forecast_result)) {
    projected_2026 <- NA
    projected_2027 <- NA
    projected_2028 <- NA
    projected_2029 <- NA
    projected_2030 <- NA
    forecast_lower <- NA
    forecast_upper <- NA
  } else {
    forecast_vals <- as.numeric(forecast_result$mean)
    
    # Calculate annual averages for 2026-2030
    # 20 quarters = 2026 Q1 to 2030 Q4
    projected_2026 <- mean(forecast_vals[1:4], na.rm = TRUE)
    projected_2027 <- mean(forecast_vals[5:8], na.rm = TRUE)
    projected_2028 <- mean(forecast_vals[9:12], na.rm = TRUE)
    projected_2029 <- mean(forecast_vals[13:16], na.rm = TRUE)
    projected_2030 <- mean(forecast_vals[17:20], na.rm = TRUE)
    
    # Get confidence intervals for the last forecast point
    forecast_lower <- forecast_result$lower[20, 1]
    forecast_upper <- forecast_result$upper[20, 1]
  }
  
  # Determine trend significance
  trend <- determine_trend(model, coeff)
  
  # Build equation string
  equation <- build_equation(model, coeff, intercept)
  
  # Create result tibble
  result <- tibble(
    State = state_name,
    ARIMA_Model = arima_order,
    Full_Equation = equation,
    Intercept = round(intercept, 4),
    Rate_of_Change = round(ar_coeff, 4),
    R2 = round(r_squared, 4),
    R2_Adj = round(r_squared_adj, 4),
    MAPE = round(mape, 2),
    RMSE = round(rmse, 4),
    AIC = round(aic, 2),
    BIC = round(bic, 2),
    Ljung_Box_p_value = round(p_value, 4),
    Average_2019_2025 = round(avg_available, 2),
    Projected_2026 = round(projected_2026, 2),
    Projected_2027 = round(projected_2027, 2),
    Projected_2028 = round(projected_2028, 2),
    Projected_2029 = round(projected_2029, 2),
    Projected_2030 = round(projected_2030, 2),
    Trend = trend,
    Forecast_CI_80_Lower = round(forecast_lower, 2),
    Forecast_CI_80_Upper = round(forecast_upper, 2)
  )
  
  return(result)
}

# Helper function for error results
create_error_result <- function(state_name, error_msg) {
  return(tibble(
    State = state_name,
    ARIMA_Model = "ERROR",
    Full_Equation = error_msg,
    Intercept = NA,
    Rate_of_Change = NA,
    R2 = NA,
    R2_Adj = NA,
    MAPE = NA,
    RMSE = NA,
    AIC = NA,
    BIC = NA,
    Ljung_Box_p_value = NA,
    Average_2019_2025 = NA,
    Projected_2026 = NA,
    Projected_2027 = NA,
    Projected_2028 = NA,
    Projected_2029 = NA,
    Projected_2030 = NA,
    Trend = "Model Failed",
    Forecast_CI_80_Lower = NA,
    Forecast_CI_80_Upper = NA
  ))
}

# Helper function to determine trend
determine_trend <- function(model, coeff) {
  # Check for drift or trend component
  if("drift" %in% names(coeff)) {
    drift_val <- coeff["drift"]
    # Get standard error
    se <- tryCatch({
      sqrt(diag(vcov(model)))["drift"]
    }, error = function(e) NA)
    
    if(!is.na(se) && abs(drift_val/se) > 1.96) {
      return(ifelse(drift_val > 0, "Increasing (significant)", "Decreasing (significant)"))
    } else {
      return("Stable (non-significant)")
    }
  } else {
    # Check if there's a clear upward or downward pattern in the last few years
    return("No significant trend detected")
  }
}

# Helper function to build equation string
build_equation <- function(model, coeff, intercept) {
  parts <- c()
  
  if(!is.na(intercept)) {
    parts <- c(parts, paste0(round(intercept, 4)))
  }
  
  # AR terms
  for(i in 1:model$arma[1]) {
    ar_name <- paste0("ar", i)
    if(ar_name %in% names(coeff)) {
      parts <- c(parts, sprintf("%+.4f * y_{t-%d}", coeff[ar_name], i))
    }
  }
  
  # MA terms
  for(i in 1:model$arma[2]) {
    ma_name <- paste0("ma", i)
    if(ma_name %in% names(coeff)) {
      parts <- c(parts, sprintf("%+.4f * ε_{t-%d}", coeff[ma_name], i))
    }
  }
  
  if(length(parts) == 0) {
    return("Constant model")
  }
  
  return(paste(parts, collapse = " "))
}

# Process all states
process_all_states <- function(data) {
  results <- list()
  
  for(i in 1:nrow(data)) {
    state_name <- data$State[i]
    state_data <- data[i, ]
    
    result <- tryCatch({
      state_arima_analysis(state_data, state_name)
    }, error = function(e) {
      cat("Error with", state_name, ":", e$message, "\n")
      create_error_result(state_name, e$message)
    })
    
    results[[i]] <- result
  }
  
  return(bind_rows(results))
}

# Run the analysis
cat("Starting ARIMA analysis for all states...\n")
final_results <- process_all_states(Hpsa22)

# Display summary
cat("\n\nAnalysis Complete!\n")
cat("Total states processed:", nrow(final_results), "\n")
cat("Successful models:", sum(final_results$ARIMA_Model != "ERROR"), "\n")
cat("Failed models:", sum(final_results$ARIMA_Model == "ERROR"), "\n")

# Export to Excel with multiple sheets
wb <- createWorkbook()

# Main results sheet
addWorksheet(wb, "ARIMA_Results")
writeData(wb, "ARIMA_Results", final_results)

# Summary statistics sheet
summary_stats <- final_results %>%
  filter(ARIMA_Model != "ERROR") %>%
  summarise(
    Metric = c("Average R²", "Average MAPE (%)", "Average RMSE", 
               "Total Increasing", "Total Decreasing", "Total Stable"),
    Value = c(
      round(mean(R2, na.rm = TRUE), 4),
      round(mean(MAPE, na.rm = TRUE), 2),
      round(mean(RMSE, na.rm = TRUE), 4),
      sum(str_detect(Trend, "Increasing"), na.rm = TRUE),
      sum(str_detect(Trend, "Decreasing"), na.rm = TRUE),
      sum(str_detect(Trend, "Stable|No significant"), na.rm = TRUE)
    )
  )

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)

# Projections summary
projections_summary <- final_results %>%
  filter(ARIMA_Model != "ERROR") %>%
  select(State, Projected_2026, Projected_2027, Projected_2028, Projected_2029, Projected_2030, Trend)

addWorksheet(wb, "Projections")
writeData(wb, "Projections", projections_summary)

# Save workbook
saveWorkbook(wb, "AutoARIMA_Complete_Analysis.xlsx", overwrite = TRUE)

# Also save as CSV for easy viewing
write.csv(final_results, "AutoARIMA_Results.csv", row.names = FALSE)

cat("\nResults saved to:\n")
cat("  - AutoARIMA_Complete_Analysis.xlsx (Excel with multiple sheets)\n")
cat("  - AutoARIMA_Results.csv (CSV format)\n")

# Display first few results
print(head(final_results %>% select(State, ARIMA_Model, R2, MAPE, Projected_2026, Trend), 10))# First, let's check your data structure
cat("Data structure:\n")
str(Hpsa22)

cat("\nFirst few rows:\n")
print(head(Hpsa22))





# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(openxlsx)

# First, let's check your data structure
cat("Data structure:\n")
str(Hpsa22)

cat("\nFirst few rows:\n")
print(head(Hpsa22))

# Let's manually create the data from your provided table to ensure it's correct
# Since you provided the data in the chat, let me recreate it properly

# Function to clean and prepare time series data - FIXED VERSION
prepare_timeseries <- function(state_row) {
  # Convert the row to numeric, excluding the State column
  # The row should have 27 quarterly values from Q12019 to Q42025
  quarterly_values <- as.numeric(state_row[2:length(state_row)])
  
  # Remove any NA values
  quarterly_values <- quarterly_values[!is.na(quarterly_values)]
  
  cat("Length of quarterly values:", length(quarterly_values), "\n")
  
  if(length(quarterly_values) < 4) {
    cat("Insufficient data points\n")
    return(NULL)
  }
  
  # Create time series (assuming quarterly data starting 2019 Q1)
  ts_data <- ts(quarterly_values, 
                start = c(2019, 1), 
                frequency = 4)
  
  return(ts_data)
}

# Improved ARIMA analysis function
state_arima_analysis <- function(state_name, quarterly_values) {
  
  cat("\nProcessing:", state_name, "\n")
  cat("Number of quarters:", length(quarterly_values), "\n")
  
  # Check if we have enough data
  if(length(quarterly_values) < 8) {
    cat("Insufficient data for", state_name, "\n")
    return(create_error_result(state_name, "Insufficient data (less than 8 quarters)"))
  }
  
  # Create time series
  ts_data <- ts(quarterly_values, start = c(2019, 1), frequency = 4)
  
  # Print first few values for debugging
  cat("First 5 values:", head(quarterly_values, 5), "\n")
  
  # Try auto.arima with different settings
  model <- tryCatch({
    # First try with seasonal
    auto.arima(ts_data, 
               seasonal = TRUE,
               stepwise = TRUE,
               approximation = TRUE,
               ic = "aicc",
               trace = FALSE)
  }, error = function(e) {
    cat("Seasonal model failed:", e$message, "\nTrying non-seasonal...\n")
    tryCatch({
      auto.arima(ts_data, 
                 seasonal = FALSE,
                 stepwise = TRUE,
                 approximation = TRUE,
                 ic = "aicc",
                 trace = FALSE)
    }, error = function(e2) {
      cat("Non-seasonal model also failed:", e2$message, "\n")
      return(NULL)
    })
  })
  
  if(is.null(model)) {
    return(create_error_result(state_name, "Model failed to converge"))
  }
  
  cat("Model selected:", model$arma, "\n")
  
  # Extract model components
  arima_order <- paste0("ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], 
                        ")(", model$arma[3], ",", model$arma[7], ",", model$arma[4], ")[", model$arma[5], "]")
  
  # Get coefficients
  coeff <- coef(model)
  cat("Coefficients:", names(coeff), "\n")
  
  # Identify intercept
  intercept <- NA
  if("intercept" %in% names(coeff)) {
    intercept <- coeff["intercept"]
  } else if("mu" %in% names(coeff)) {
    intercept <- coeff["mu"]
  }
  
  # Calculate fitted values and metrics
  fitted_values <- fitted(model)
  residuals_model <- residuals(model)
  
  # MAPE
  non_zero_idx <- which(ts_data != 0)
  if(length(non_zero_idx) > 0) {
    mape <- mean(abs((ts_data[non_zero_idx] - fitted_values[non_zero_idx]) / ts_data[non_zero_idx]), na.rm = TRUE) * 100
  } else {
    mape <- NA
  }
  
  # RMSE
  rmse <- sqrt(mean((ts_data - fitted_values)^2, na.rm = TRUE))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # R-squared
  r_squared <- cor(ts_data, fitted_values)^2
  
  # Adjusted R-squared
  n <- length(ts_data)
  p <- length(coeff)
  r_squared_adj <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
  
  # Ljung-Box test
  lag_val <- min(10, floor(length(residuals_model)/4))
  lb_test <- Box.test(residuals_model, type = "Ljung-Box", lag = lag_val)
  p_value <- lb_test$p.value
  
  # Average
  avg_available <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 20 quarters (2026-2030)
  forecast_result <- tryCatch({
    forecast(model, h = 20)
  }, error = function(e) {
    cat("Forecast failed:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(forecast_result)) {
    projected_2026 <- NA
    projected_2027 <- NA
    projected_2028 <- NA
    projected_2029 <- NA
    projected_2030 <- NA
  } else {
    forecast_vals <- as.numeric(forecast_result$mean)
    cat("Forecast values generated:", length(forecast_vals), "\n")
    
    # Annual averages for 2026-2030
    if(length(forecast_vals) >= 20) {
      projected_2026 <- mean(forecast_vals[1:4], na.rm = TRUE)
      projected_2027 <- mean(forecast_vals[5:8], na.rm = TRUE)
      projected_2028 <- mean(forecast_vals[9:12], na.rm = TRUE)
      projected_2029 <- mean(forecast_vals[13:16], na.rm = TRUE)
      projected_2030 <- mean(forecast_vals[17:20], na.rm = TRUE)
    } else {
      projected_2026 <- NA
      projected_2027 <- NA
      projected_2028 <- NA
      projected_2029 <- NA
      projected_2030 <- NA
    }
  }
  
  # Determine trend
  trend <- determine_trend(model, coeff, ts_data)
  
  # Create result
  result <- tibble(
    State = state_name,
    ARIMA_Model = arima_order,
    Full_Equation = "See coefficients",
    Intercept = ifelse(is.na(intercept), NA, round(intercept, 4)),
    Rate_of_Change = ifelse("ar1" %in% names(coeff), round(coeff["ar1"], 4), NA),
    R2 = round(r_squared, 4),
    R2_Adj = round(r_squared_adj, 4),
    MAPE = round(mape, 2),
    RMSE = round(rmse, 4),
    AIC = round(aic, 2),
    BIC = round(bic, 2),
    Ljung_Box_p_value = round(p_value, 4),
    Average_2019_2025 = round(avg_available, 2),
    Projected_2026 = round(projected_2026, 2),
    Projected_2027 = round(projected_2027, 2),
    Projected_2028 = round(projected_2028, 2),
    Projected_2029 = round(projected_2029, 2),
    Projected_2030 = round(projected_2030, 2),
    Trend = trend
  )
  
  return(result)
}

# Helper function for error results
create_error_result <- function(state_name, error_msg) {
  return(tibble(
    State = state_name,
    ARIMA_Model = "ERROR",
    Full_Equation = error_msg,
    Intercept = NA,
    Rate_of_Change = NA,
    R2 = NA,
    R2_Adj = NA,
    MAPE = NA,
    RMSE = NA,
    AIC = NA,
    BIC = NA,
    Ljung_Box_p_value = NA,
    Average_2019_2025 = NA,
    Projected_2026 = NA,
    Projected_2027 = NA,
    Projected_2028 = NA,
    Projected_2029 = NA,
    Projected_2030 = NA,
    Trend = "Model Failed"
  ))
}

# Helper function to determine trend
determine_trend <- function(model, coeff, ts_data) {
  # Check for drift
  if("drift" %in% names(coeff)) {
    drift_val <- coeff["drift"]
    se <- tryCatch({
      sqrt(diag(vcov(model)))["drift"]
    }, error = function(e) NA)
    
    if(!is.na(se) && abs(drift_val/se) > 1.96) {
      return(ifelse(drift_val > 0, "Increasing (significant)", "Decreasing (significant)"))
    } else {
      return("Stable (non-significant)")
    }
  } else {
    # Simple check: compare first and last 4 quarters
    n <- length(ts_data)
    if(n >= 8) {
      first_avg <- mean(ts_data[1:4])
      last_avg <- mean(ts_data[(n-3):n])
      change_pct <- ((last_avg - first_avg) / first_avg) * 100
      
      if(abs(change_pct) > 5) {
        return(ifelse(change_pct > 0, "Increasing trend (>5%)", "Decreasing trend (>5%)"))
      } else {
        return("Stable (change <5%)")
      }
    } else {
      return("Insufficient data for trend detection")
    }
  }
}

# Manually create data from your table
# I'll create a sample for Alabama to test first
create_state_data <- function(state_name, values_vector) {
  df <- data.frame(
    State = state_name,
    Q12019 = values_vector[1],
    Q22019 = values_vector[2],
    Q32019 = values_vector[3],
    Q42019 = values_vector[4],
    Q12020 = values_vector[5],
    Q22020 = values_vector[6],
    Q32020 = values_vector[7],
    Q42020 = values_vector[8],
    Q12021 = values_vector[9],
    Q22021 = values_vector[10],
    Q32021 = values_vector[11],
    Q42021 = values_vector[12],
    Q12022 = values_vector[13],
    Q22022 = values_vector[14],
    Q32022 = values_vector[15],
    Q42022 = values_vector[16],
    Q12023 = values_vector[17],
    Q22023 = values_vector[18],
    Q32023 = values_vector[19],
    Q42023 = values_vector[20],
    Q12024 = values_vector[21],
    Q22024 = values_vector[22],
    Q32024 = values_vector[23],
    Q42024 = values_vector[24],
    Q12025 = values_vector[25],
    Q22025 = values_vector[26],
    Q42025 = values_vector[27]
  )
  return(df)
}

# Test with Alabama data from your table
alabama_values <- c(20.59, 20.59, 20.57, 20.57, 20.56, 20.56, 20.56, 20.56, 
                    20.56, 20.56, 20.56, 20.56, 20.56, 20.56, 20.5, 20.5, 
                    20.5, 20.47, 21.77, 21.85, 21.85, 25.09, 25.09, 25.09, 
                    25.09, 25.04, 24.33)

# Create test data
test_alabama <- create_state_data("Alabama", alabama_values)

# Test the function
test_result <- state_arima_analysis("Alabama", alabama_values)

if(!is.null(test_result)) {
  print(test_result)
}

# Now process all states - you'll need to create the full dataset
# For now, let me show you how to extract from your existing Hpsa22
# If Hpsa22 is already loaded correctly, use this:

process_all_states <- function(data) {
  results <- list()
  
  for(i in 1:nrow(data)) {
    state_name <- data$State[i]
    
    # Extract quarterly values from the row
    quarterly_values <- as.numeric(data[i, 2:ncol(data)])
    quarterly_values <- quarterly_values[!is.na(quarterly_values)]
    
    result <- tryCatch({
      state_arima_analysis(state_name, quarterly_values)
    }, error = function(e) {
      cat("Error with", state_name, ":", e$message, "\n")
      create_error_result(state_name, e$message)
    })
    
    results[[i]] <- result
  }
  
  return(bind_rows(results))
}

# Run the analysis
cat("\n\nStarting analysis...\n")
final_results <- process_all_states(Hpsa22)

# Display results
print(head(final_results))

# Export results
write.xlsx(final_results, "ARIMA_Results_Fixed.xlsx", rowNames = FALSE)
write.csv(final_results, "ARIMA_Results_Fixed.csv", rowNames = FALSE)

cat("\nResults saved to ARIMA_Results_Fixed.xlsx and ARIMA_Results_Fixed.csv\n")




# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(openxlsx)

# Function to process each state
state_arima_analysis <- function(state_data, state_name) {
  
  cat("\nProcessing:", state_name)
  
  # Extract quarterly values - skip the State column
  quarterly_values <- state_data %>%
    select(-State) %>%
    as.numeric()
  
  # Remove any NA values at the end
  quarterly_values <- quarterly_values[!is.na(quarterly_values)]
  
  cat(" - Quarters:", length(quarterly_values))
  
  # Check if we have enough data
  if(length(quarterly_values) < 8) {
    cat(" - INSUFFICIENT DATA\n")
    return(create_error_result(state_name, "Insufficient data (less than 8 quarters)"))
  }
  
  # Create time series (quarterly, starting 2019 Q1)
  ts_data <- ts(quarterly_values, 
                start = c(2019, 1), 
                frequency = 4)
  
  # Try auto.arima with simplified settings
  model <- tryCatch({
    auto.arima(ts_data, 
               seasonal = TRUE,
               stepwise = TRUE,
               approximation = TRUE,
               ic = "aic",
               trace = FALSE)
  }, error = function(e) {
    # If seasonal fails, try non-seasonal
    tryCatch({
      auto.arima(ts_data, 
                 seasonal = FALSE,
                 stepwise = TRUE,
                 approximation = TRUE)
    }, error = function(e2) {
      NULL
    })
  })
  
  if(is.null(model)) {
    cat(" - MODEL FAILED\n")
    return(create_error_result(state_name, "Model failed to converge"))
  }
  
  cat(" - Model:", model$arma[1], model$arma[6], model$arma[2], 
      "Seasonal:", model$arma[3], model$arma[7], model$arma[4])
  
  # Extract model information
  arima_order <- paste0("ARIMA(", model$arma[1], ",", model$arma[6], ",", model$arma[2], 
                        ")(", model$arma[3], ",", model$arma[7], ",", model$arma[4], ")[", model$arma[5], "]")
  
  # Get coefficients
  coeff <- coef(model)
  
  # Find intercept (could be named "intercept" or "mu")
  intercept <- NA
  if("intercept" %in% names(coeff)) {
    intercept <- coeff["intercept"]
  } else if("mu" %in% names(coeff)) {
    intercept <- coeff["mu"]
  }
  
  # Get first AR coefficient as rate of change
  rate_of_change <- NA
  if("ar1" %in% names(coeff)) {
    rate_of_change <- coeff["ar1"]
  } else if("sar1" %in% names(coeff)) {
    rate_of_change <- coeff["sar1"]
  }
  
  # Calculate fitted values and residuals
  fitted_values <- fitted(model)
  residuals_model <- residuals(model)
  
  # Calculate metrics
  # MAPE (avoid division by zero)
  non_zero <- ts_data != 0
  if(sum(non_zero) > 0) {
    mape <- mean(abs((ts_data[non_zero] - fitted_values[non_zero]) / ts_data[non_zero]), na.rm = TRUE) * 100
  } else {
    mape <- NA
  }
  
  # RMSE
  rmse <- sqrt(mean((ts_data - fitted_values)^2, na.rm = TRUE))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # R-squared (correlation between actual and fitted)
  r_squared <- cor(ts_data, fitted_values)^2
  
  # Adjusted R-squared
  n <- length(ts_data)
  p <- length(coeff)
  r_squared_adj <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
  
  # Ljung-Box test
  lag_val <- min(10, floor(length(residuals_model)/4))
  lb_test <- Box.test(residuals_model, type = "Ljung-Box", lag = lag_val)
  p_value <- lb_test$p.value
  
  # Average of available data
  avg_2019_2025 <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 2026-2030 (20 quarters ahead)
  forecast_result <- tryCatch({
    forecast(model, h = 20)
  }, error = function(e) {
    NULL
  })
  
  if(is.null(forecast_result)) {
    projected_2026 <- NA
    projected_2027 <- NA
    projected_2028 <- NA
    projected_2029 <- NA
    projected_2030 <- NA
  } else {
    forecast_vals <- as.numeric(forecast_result$mean)
    
    # Calculate annual averages (2026-2030)
    if(length(forecast_vals) >= 20) {
      projected_2026 <- mean(forecast_vals[1:4], na.rm = TRUE)
      projected_2027 <- mean(forecast_vals[5:8], na.rm = TRUE)
      projected_2028 <- mean(forecast_vals[9:12], na.rm = TRUE)
      projected_2029 <- mean(forecast_vals[13:16], na.rm = TRUE)
      projected_2030 <- mean(forecast_vals[17:20], na.rm = TRUE)
    } else {
      projected_2026 <- NA
      projected_2027 <- NA
      projected_2028 <- NA
      projected_2029 <- NA
      projected_2030 <- NA
    }
  }
  
  # Determine trend significance
  trend <- determine_trend_significance(model, coeff, ts_data)
  
  # Build equation string
  equation <- build_equation_string(model, coeff, intercept)
  
  cat(" - Success!\n")
  
  # Create result tibble
  result <- tibble(
    State = state_name,
    ARIMA_Model = arima_order,
    Full_Equation = equation,
    Intercept = round(intercept, 4),
    Rate_of_Change_Beta = round(rate_of_change, 4),
    R2 = round(r_squared, 4),
    R2_Adj = round(r_squared_adj, 4),
    MAPE = round(mape, 2),
    RMSE = round(rmse, 4),
    AIC = round(aic, 2),
    BIC = round(bic, 2),
    Ljung_Box_p_value = round(p_value, 4),
    Average_2019_2025 = round(avg_2019_2025, 2),
    Projected_2026 = round(projected_2026, 2),
    Projected_2027 = round(projected_2027, 2),
    Projected_2028 = round(projected_2028, 2),
    Projected_2029 = round(projected_2029, 2),
    Projected_2030 = round(projected_2030, 2),
    Trend = trend
  )
  
  return(result)
}

# Helper function for error results
create_error_result <- function(state_name, error_msg) {
  return(tibble(
    State = state_name,
    ARIMA_Model = "ERROR",
    Full_Equation = error_msg,
    Intercept = NA,
    Rate_of_Change_Beta = NA,
    R2 = NA,
    R2_Adj = NA,
    MAPE = NA,
    RMSE = NA,
    AIC = NA,
    BIC = NA,
    Ljung_Box_p_value = NA,
    Average_2019_2025 = NA,
    Projected_2026 = NA,
    Projected_2027 = NA,
    Projected_2028 = NA,
    Projected_2029 = NA,
    Projected_2030 = NA,
    Trend = "Model Failed"
  ))
}

# Helper function to determine trend significance
determine_trend_significance <- function(model, coeff, ts_data) {
  # Check for drift component in the model
  if("drift" %in% names(coeff)) {
    drift_val <- coeff["drift"]
    # Get standard error
    se <- tryCatch({
      sqrt(diag(vcov(model)))["drift"]
    }, error = function(e) NA)
    
    if(!is.na(se) && abs(drift_val/se) > 1.96) {
      return(ifelse(drift_val > 0, "Increasing (significant)", "Decreasing (significant)"))
    } else if(!is.na(se)) {
      return("Stable (non-significant)")
    }
  }
  
  # Alternative: Check trend using simple linear regression on time
  time_points <- 1:length(ts_data)
  lm_model <- lm(as.numeric(ts_data) ~ time_points)
  slope <- coef(lm_model)[2]
  slope_pvalue <- summary(lm_model)$coefficients[2, 4]
  
  if(slope_pvalue < 0.05) {
    return(ifelse(slope > 0, "Increasing (significant)", "Decreasing (significant)"))
  } else {
    return("Stable (non-significant)")
  }
}

# Helper function to build equation string
build_equation_string <- function(model, coeff, intercept) {
  equation_parts <- c()
  
  # Add intercept
  if(!is.na(intercept)) {
    equation_parts <- c(equation_parts, sprintf("%.4f", intercept))
  }
  
  # Add AR terms
  ar_terms <- model$arma[1]
  if(ar_terms > 0) {
    for(i in 1:ar_terms) {
      ar_name <- paste0("ar", i)
      if(ar_name %in% names(coeff)) {
        sign_char <- ifelse(coeff[ar_name] >= 0, " + ", " - ")
        abs_val <- abs(coeff[ar_name])
        equation_parts <- c(equation_parts, sprintf("%s%.4f*y_{t-%d}", 
                                                    ifelse(i==1 & !is.na(intercept), sign_char, 
                                                           ifelse(i==1, "", sign_char)),
                                                    abs_val, i))
      }
    }
  }
  
  # Add MA terms
  ma_terms <- model$arma[2]
  if(ma_terms > 0) {
    for(i in 1:ma_terms) {
      ma_name <- paste0("ma", i)
      if(ma_name %in% names(coeff)) {
        sign_char <- ifelse(coeff[ma_name] >= 0, " + ", " - ")
        abs_val <- abs(coeff[ma_name])
        equation_parts <- c(equation_parts, sprintf("%s%.4f*ε_{t-%d}", sign_char, abs_val, i))
      }
    }
  }
  
  if(length(equation_parts) == 0) {
    return("Constant model (mean)")
  }
  
  return(paste(equation_parts, collapse = ""))
}

# Main execution
cat("Starting AutoARIMA analysis for all states...\n")
cat("===============================================\n")

# Process all states
results_list <- list()

for(i in 1:nrow(Hpsa22)) {
  state_name <- Hpsa22$State[i]
  state_row <- Hpsa22[i, ]
  
  result <- tryCatch({
    state_arima_analysis(state_row, state_name)
  }, error = function(e) {
    cat("\nERROR processing", state_name, ":", e$message, "\n")
    create_error_result(state_name, paste("Error:", e$message))
  })
  
  results_list[[i]] <- result
}

# Combine all results
final_results <- bind_rows(results_list)

cat("\n===============================================\n")
cat("Analysis Complete!\n")
cat("Total states processed:", nrow(final_results), "\n")
cat("Successful models:", sum(final_results$ARIMA_Model != "ERROR"), "\n")
cat("Failed models:", sum(final_results$ARIMA_Model == "ERROR"), "\n")

# Display first 10 results
cat("\nFirst 10 results:\n")
print(final_results %>% 
        select(State, ARIMA_Model, R2, MAPE, Projected_2026, Trend) %>% 
        head(10))

# Export to Excel with multiple sheets
wb <- createWorkbook()

# Sheet 1: Full results
addWorksheet(wb, "ARIMA_Results")
writeData(wb, "ARIMA_Results", final_results)

# Sheet 2: Summary statistics
summary_stats <- final_results %>%
  filter(ARIMA_Model != "ERROR") %>%
  summarise(
    Metric = c("Average R²", "Average MAPE (%)", "Average RMSE", 
               "States with Increasing Trend", "States with Decreasing Trend", 
               "States with Stable Trend", "Successful Models", "Failed Models"),
    Value = c(
      round(mean(R2, na.rm = TRUE), 4),
      round(mean(MAPE, na.rm = TRUE), 2),
      round(mean(RMSE, na.rm = TRUE), 4),
      sum(str_detect(Trend, "Increasing"), na.rm = TRUE),
      sum(str_detect(Trend, "Decreasing"), na.rm = TRUE),
      sum(str_detect(Trend, "Stable"), na.rm = TRUE),
      sum(ARIMA_Model != "ERROR"),
      sum(ARIMA_Model == "ERROR")
    )
  )

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)

# Sheet 3: Projections only
projections <- final_results %>%
  filter(ARIMA_Model != "ERROR") %>%
  select(State, starts_with("Projected"), Trend)

addWorksheet(wb, "Projections")
writeData(wb, "Projections", projections)

# Save workbook
saveWorkbook(wb, "AutoARIMA_State_Analysis.xlsx", overwrite = TRUE)

# Also save as CSV
write.csv(final_results, "AutoARIMA_State_Analysis.csv", row.names = FALSE)

cat("\nResults saved to:\n")
cat("  - AutoARIMA_State_Analysis.xlsx (Excel file with multiple sheets)\n")
cat("  - AutoARIMA_State_Analysis.csv (CSV format)\n")


# Load required libraries
library(tidyverse)
library(forecast)
library(openxlsx)

# Create the dataset directly from your table
data_text <- "State	Q12019	Q22019	Q32019	Q42019	Q12020	Q22020	Q32020	Q42020	Q12021	Q22021	Q32021	Q42021	Q12022	Q22022	Q32022	Q42022	Q12023	Q22023	Q32023	Q42023	Q12024	Q22024	Q32024	Q42024	Q12025	Q22025	Q42025
Alabama	20.59	20.59	20.57	20.57	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.5	20.5	20.5	20.47	21.77	21.85	21.85	25.09	25.09	25.09	25.09	25.04	24.33
Alaska	32.97	32.97	32.97	32.97	32.97	32.97	26.32	26.32	26.32	26.32	26.32	28.59	28.59	27.11	34.71	34.71	34.71	34.71	34.71	34.71	34.98	34.2	34.21	34.21	34.2	34.4	33.83
Arizona	32.95	32.95	34.5	34.5	34.49	34.49	34.49	34.49	34.49	34.45	34.39	30.35	30.37	30.51	30.49	32.94	33.67	33.67	33.67	33.67	33.67	34.48	34.48	34.48	34.48	34.48	34.31
Arkansas	36.02	36.02	37.27	36.94	36.94	36.94	36.94	37.24	36.62	36.62	36.62	36.5	36.54	36.23	36.78	36.78	36.78	36.78	36.78	36.78	36.78	36.7	36.71	36.71	36.7	36.7	33.43
California	31.25	31.25	22.4	22.29	22.28	23.99	25.33	24.82	23.92	25.45	25.26	27.78	27.86	28.27	31.8	32.98	35.18	35.18	34.76	36.14	35.8	38.32	37.76	37.76	37.43	37.43	36.73
Colorado	36.76	36.76	37.99	37.99	37.99	37.99	37.99	38.09	38.09	38.09	37.89	49.57	49.36	49.88	49.03	49.03	53.41	53.48	53.48	53.48	53.48	58.12	58.13	58.13	58.12	58.12	58.77"

# Read the data
Hpsa22 <- read.table(text = data_text, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Function to analyze single state
analyze_state <- function(state_name, values) {
  
  # Create time series (27 quarters from 2019 Q1 to 2025 Q4)
  ts_data <- ts(values, start = c(2019, 1), frequency = 4)
  
  # Fit auto ARIMA model
  model <- tryCatch({
    auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE)
  }, error = function(e) {
    tryCatch({
      auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE)
    }, error = function(e2) {
      return(NULL)
    })
  })
  
  if(is.null(model)) {
    return(NULL)
  }
  
  # Get model specifications
  p <- model$arma[1]
  d <- model$arma[6]
  q <- model$arma[2]
  P <- model$arma[3]
  D <- model$arma[7]
  Q <- model$arma[4]
  s <- model$arma[5]
  
  model_type <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  if(P > 0 | D > 0 | Q > 0) {
    model_type <- paste0(model_type, "(", P, ",", D, ",", Q, ")[", s, "]")
  }
  
  # Get coefficients
  coeff <- coef(model)
  
  # Calculate metrics
  fitted_vals <- fitted(model)
  resid_vals <- residuals(model)
  
  # MAPE
  mape <- mean(abs(resid_vals / ts_data), na.rm = TRUE) * 100
  
  # RMSE
  rmse <- sqrt(mean(resid_vals^2, na.rm = TRUE))
  
  # R-squared
  r2 <- cor(ts_data, fitted_vals)^2
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # Ljung-Box test
  lb_pvalue <- Box.test(resid_vals, type = "Ljung-Box", lag = 10)$p.value
  
  # Average
  avg_value <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 20 quarters (2026-2030)
  forecast_result <- forecast(model, h = 20)
  forecast_vals <- as.numeric(forecast_result$mean)
  
  # Projections (annual averages)
  proj_2026 <- mean(forecast_vals[1:4])
  proj_2027 <- mean(forecast_vals[5:8])
  proj_2028 <- mean(forecast_vals[9:12])
  proj_2029 <- mean(forecast_vals[13:16])
  proj_2030 <- mean(forecast_vals[17:20])
  
  # Trend detection using linear regression
  time_points <- 1:length(ts_data)
  lm_model <- lm(as.numeric(ts_data) ~ time_points)
  slope <- coef(lm_model)[2]
  slope_pval <- summary(lm_model)$coefficients[2, 4]
  
  if(slope_pval < 0.05) {
    trend <- ifelse(slope > 0, "Increasing (significant)", "Decreasing (significant)")
  } else {
    trend <- "Stable (non-significant)"
  }
  
  # Build equation
  intercept_val <- ifelse("intercept" %in% names(coeff), coeff["intercept"], 
                          ifelse("mu" %in% names(coeff), coeff["mu"], NA))
  
  # Create result
  result <- data.frame(
    State = state_name,
    ARIMA_Model = model_type,
    Intercept = ifelse(is.na(intercept_val), NA, round(intercept_val, 4)),
    AR1 = ifelse("ar1" %in% names(coeff), round(coeff["ar1"], 4), NA),
    MA1 = ifelse("ma1" %in% names(coeff), round(coeff["ma1"], 4), NA),
    R2 = round(r2, 4),
    R2_Adj = round(1 - (1 - r2) * ((length(ts_data) - 1) / (length(ts_data) - length(coeff) - 1)), 4),
    MAPE = round(mape, 2),
    RMSE = round(rmse, 4),
    AIC = round(aic, 2),
    BIC = round(bic, 2),
    Ljung_Box_pvalue = round(lb_pvalue, 4),
    Average_2019_2025 = round(avg_value, 2),
    Projected_2026 = round(proj_2026, 2),
    Projected_2027 = round(proj_2027, 2),
    Projected_2028 = round(proj_2028, 2),
    Projected_2029 = round(proj_2029, 2),
    Projected_2030 = round(proj_2030, 2),
    Trend = trend,
    Forecast_CI_Lower = round(forecast_result$lower[20, 1], 2),
    Forecast_CI_Upper = round(forecast_result$upper[20, 1], 2)
  )
  
  return(result)
}

# Process all states
results <- list()

for(i in 1:nrow(Hpsa22)) {
  state_name <- Hpsa22$State[i]
  values <- as.numeric(Hpsa22[i, -1])  # Exclude State column
  
  cat("Processing", state_name, "... ")
  
  result <- tryCatch({
    analyze_state(state_name, values)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if(!is.null(result)) {
    results[[i]] <- result
    cat("Success\n")
  } else {
    cat("Failed\n")
  }
}

# Combine results
final_results <- bind_rows(results)

# Display results
print(final_results)

# Export to Excel
write.xlsx(final_results, "AutoARIMA_Results.xlsx", rowNames = FALSE)

# Also save as CSV
write.csv(final_results, "AutoARIMA_Results.csv", row.names = FALSE)

cat("\n\nAnalysis Complete!\n")
cat("Successfully processed:", nrow(final_results), "states\n")
cat("Results saved to AutoARIMA_Results.xlsx and AutoARIMA_Results.csv\n")

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Average R-squared:", round(mean(final_results$R2, na.rm = TRUE), 4), "\n")
cat("Average MAPE:", round(mean(final_results$MAPE, na.rm = TRUE), 2), "%\n")
cat("Average RMSE:", round(mean(final_results$RMSE, na.rm = TRUE), 4), "\n")
cat("\nTrend Distribution:\n")
print(table(final_results$Trend))
cat("\n2026-2030 Projections Summary:\n")
print(summary(final_results[, c("Projected_2026", "Projected_2027", "Projected_2028", "Projected_2029", "Projected_2030")]))



# Load required libraries
library(tidyverse)
library(forecast)
library(openxlsx)

# Complete dataset with all 50 states + National
complete_data <- "
State	Q12019	Q22019	Q32019	Q42019	Q12020	Q22020	Q32020	Q42020	Q12021	Q22021	Q32021	Q42021	Q12022	Q22022	Q32022	Q42022	Q12023	Q22023	Q32023	Q42023	Q12024	Q22024	Q32024	Q42024	Q12025	Q22025	Q42025
Alabama	20.59	20.59	20.57	20.57	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.5	20.5	20.5	20.47	21.77	21.85	21.85	25.09	25.09	25.09	25.09	25.04	24.33
Alaska	32.97	32.97	32.97	32.97	32.97	32.97	26.32	26.32	26.32	26.32	26.32	28.59	28.59	27.11	34.71	34.71	34.71	34.71	34.71	34.71	34.98	34.2	34.21	34.21	34.2	34.4	33.83
Arizona	32.95	32.95	34.5	34.5	34.49	34.49	34.49	34.49	34.49	34.45	34.39	30.35	30.37	30.51	30.49	32.94	33.67	33.67	33.67	33.67	33.67	34.48	34.48	34.48	34.48	34.48	34.31
Arkansas	36.02	36.02	37.27	36.94	36.94	36.94	36.94	37.24	36.62	36.62	36.62	36.5	36.54	36.23	36.78	36.78	36.78	36.78	36.78	36.78	36.78	36.7	36.71	36.71	36.7	36.7	33.43
California	31.25	31.25	22.4	22.29	22.28	23.99	25.33	24.82	23.92	25.45	25.26	27.78	27.86	28.27	31.8	32.98	35.18	35.18	34.76	36.14	35.8	38.32	37.76	37.76	37.43	37.43	36.73
Colorado	36.76	36.76	37.99	37.99	37.99	37.99	37.99	38.09	38.09	38.09	37.89	49.57	49.36	49.88	49.03	49.03	53.41	53.48	53.48	53.48	53.48	58.12	58.13	58.13	58.12	58.12	58.77
Delaware	6.84	6.84	6.84	6.84	6.83	6.83	6.83	6.83	6.83	6.83	6.83	7.99	4.98	4.98	4.98	4.98	4.98	4.98	4.98	4.98	4.98	7.41	7.42	7.42	5.46	5.46	5.46
District of Columbia	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.9	2.9	2.9	2.9	2.9	2.9	2.9	2.91	2.91	2.9	2.9	2.9
Florida	13.22	13.22	12.79	12.6	12.58	12.58	12.68	13.36	13.31	13.31	13.3	14	13.98	14.14	15.59	16.6	16.64	16.64	16.64	16.95	16.68	18.81	18.83	18.76	18.76	18.72	18.78
Georgia	22.87	22.87	22.58	22.58	22.57	22.57	22.53	22.54	22.89	22.19	20.22	18.57	18.63	18.14	18.04	18.04	18.04	18.04	18.02	18.02	18.02	17.15	17.15	17.26	17.26	17.26	17.78
Hawaii	52.63	52.63	56.49	55.05	53.34	51.85	47.73	47.73	43.39	43.4	40.32	43.37	43.37	43.37	43.21	43.21	43.21	43.21	43.21	43.21	45.67	40.34	40.34	40.34	40.34	40.34	40.34
Idaho	45.25	45.25	44.24	44.24	43.16	43.16	43.16	52.61	53.02	59.73	63.5	48.85	48.04	48.04	47.46	47.46	47.46	47.46	48.32	48.32	48.32	49.7	49.7	48.44	48.65	48.65	37.48
Illinois	37.11	37.11	38.43	38.14	38.12	37.65	35.32	35.32	35.32	33.52	35.32	28.97	28.32	28.41	29.17	29.08	29.08	29.08	29.08	29.08	29.08	24.71	24.71	24.71	24.74	24.74	28.39
Indiana	30.57	30.57	32.41	32.41	32.4	32.4	32.14	32.14	32.14	32.14	32.14	32.65	32.65	32.65	32.7	33.14	33.14	33.14	33.14	33.18	33.18	36.73	43.36	38.91	34.46	34.46	27.84
Iowa	35.36	35.36	35.15	35.15	35.15	35.15	38.17	38.08	34.94	34.94	34.94	32.89	32.89	31.83	31.83	31.99	31.99	31.99	28.87	28.87	28.87	20.63	20.64	16.79	16.78	16.78	27.27
Kansas	24.49	24.49	24.87	24.87	25.91	30.05	40.21	40.21	40.21	40.21	40.21	38.55	38.55	38.55	38.49	38.49	38.49	38.49	38.49	38.49	38.69	27.72	28.01	28.01	28.01	28.01	29.47
Kentucky	49.72	49.72	49.55	40.88	39.13	34.23	22.68	22.52	20.34	20.91	20.68	16.85	17.21	17.88	16.66	16.66	16.66	16.66	16.01	16.06	16.36	13.93	13.68	13.59	11.25	11.25	10.4
Louisiana	55.18	55.18	54.33	53.96	53.96	53.96	54.01	43.34	53.27	53.27	53.24	54.21	53.94	50.06	49.94	49.94	49.94	49.94	49.94	49.94	49.94	46.31	46.31	46.31	46.31	46.31	45.8
Maine	40.1	40.1	40	40.1	39.87	39.87	39.87	39.87	39.87	39.87	37	32.24	30.42	30.42	32	32.29	32.29	32.29	32.29	32.29	31.92	27.97	27.97	27.97	27.97	27.97	28.88
Maryland	17.91	17.91	21.62	21.62	21.61	21.61	21.61	21.61	19.97	19.97	19.97	29.4	35.37	34.92	31.94	33.94	33.94	33.94	33.94	33.94	33.94	38.92	38.93	41.86	41.86	41.86	42.98
Massachusetts	66.17	66.17	76.12	76.12	76.11	76.11	76.13	76.13	76.13	76.13	76.13	76.13	76.13	76.39	75.95	75.95	75.2	75.2	74.31	74.31	74.31	74	63.88	67.43	63.87	63.87	63.87
Michigan	5.56	5.56	6.22	6.22	6.22	6.22	6.22	5.73	6.29	6.29	6.29	26.29	26.29	26.35	26.35	26.02	26.02	26.02	26.62	26.62	26.62	27.39	27.4	27.65	27.65	27.65	29.9
Minnesota	15.58	15.58	15.57	22.92	23.5	27.52	26.19	26.19	26.05	26.04	31.46	29.05	29.06	39.3	41.16	41.16	41.16	41.16	41.16	39.9	38.73	44.86	44.86	45.21	45.3	45.3	44.05
Mississippi	45.53	45.53	45.83	45.83	45.82	45.82	45.82	45.82	45.82	45.82	45.66	56.4	54.65	56.43	54.86	54.98	54.98	54.98	54.98	54.98	54.98	50.2	50.2	50.2	50.2	50.2	40.69
Missouri	12.5	12.5	12.34	12.34	12.34	12.31	12.31	12.31	12.31	12.31	12.31	12.27	12.27	11.93	20.86	22.89	22.89	22.89	22.89	22.89	22.89	20.81	20.81	20.81	20.81	20.81	20.87
Montana	31.58	31.58	31.98	31.98	31.97	31.97	31.97	31.97	31.97	31.97	31.97	40.95	40.95	40.66	40.63	40.63	40.63	40.63	40.63	40.63	40.63	42.56	42.56	42.56	42.56	42.64	45.86
Nevada	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	28.59	30.81	30.36	30.36	30.11	30.11	30.11	30.11	30.11	30.11	26.48	26.48	26.48	26.48	26.48	26.18
New Hampshire	40.32	40.32	40.46	40.46	40.45	40.45	38.21	38.21	38.21	38.21	38.21	24.4	24.4	23.16	21.48	21.48	21.48	21.48	21.48	21.48	21.48	26.51	26.52	26.52	26.51	26.51	21.63
New Jersey	25.96	25.96	42.94	42.94	42.94	42.94	42.94	42.94	42.94	42.94	41.79	41.79	41.79	39.83	38.1	37.63	37.63	37.63	37.63	29.18	29.18	27.77	27.78	27.78	27.77	43.64	50.83
New Mexico	21.44	21.44	21.14	21.14	21.14	21.14	21.14	21.14	21.14	21.14	21.14	27.05	21.86	23.72	23.72	23.72	23.72	23.72	23.72	22.81	22.81	20.6	20.6	20.6	20.6	20.6	21.44
New York	23.1	23.1	19.32	18.22	17.81	17.81	17.33	17.33	17.33	17.32	17.32	17.34	17.3	17.31	17.31	17.31	17.31	17.31	17.77	16.68	16.68	15.9	16.15	16.15	16.15	16.15	16.15
North Carolina	18.69	18.69	19.55	19.55	12.41	12.01	12.12	16.07	18.45	18.45	18.45	26.93	26.74	26.74	26.58	26.55	26.42	26.66	26.83	26.83	26.83	22.15	22.16	22.16	22	22.01	22.01
North Dakota	33.66	33.66	33.66	33.79	33.78	31.6	31.6	41.52	41.52	41.52	41.52	44.11	51.35	50.1	50.1	52.48	52.48	52.48	52.48	52.48	50.51	51.42	51.42	51.42	51.42	50.06	40.44
Ohio	34.13	34.13	34.24	34.24	34.23	34	34	34	34	34.05	33.89	27.19	26.98	27.45	27.45	27.45	27.45	27.45	27.45	27.45	27.61	26.84	26.84	26.84	28.07	36.18	39.64
Oklahoma	35.52	35.52	40.14	40.14	40.13	40.13	39.52	39.52	39.52	39.52	39.52	34.09	34.09	34.75	34.75	34.75	34.75	34.75	34.55	34.55	34.55	33.52	33.53	29.56	29.55	29.55	41.56
Oregon	20.37	20.37	20.42	20.42	20.41	20.41	21.92	21.92	21.81	21.31	22.61	27.82	31.93	31.95	31.95	31.93	31.83	31.83	32.34	32.34	33.27	39.34	39.35	39.5	39.49	39.49	40.88
Pennsylvania	47.08	47.08	47.87	47.87	47.86	47.86	47.86	47.87	47.87	47.79	47.63	39.32	39.02	36.33	36.33	36.33	36.33	36.33	36.33	36.09	37.27	35.93	35.94	35.94	35.56	35.56	39.96
Rhode Island	34.88	34.88	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	33.54	35.84	35.84	35.9	35.9	35.9	35.9	35.72	35.72	35.72	35.72	35.72	38
South Carolina	40.57	40.57	41	41	41.14	45.3	44.99	44.99	44.99	45.16	45.16	44.91	44.91	44.88	44.8	44.8	44.8	44.8	44.8	44.8	44.8	44.71	44.88	44.88	44.88	44.88	45.32
South Dakota	27.12	27.12	25.46	25.46	23.14	23.35	22.93	22.93	22.93	22.93	24.55	20.53	20.53	32.56	32.56	32.56	32.56	32.56	56.24	31.09	31.09	30.85	30.86	30.63	31.37	29.51	26.98
Tennessee	29.28	29.28	29.34	29.34	29.33	29.33	29.33	29.33	29.33	29.33	29.33	31.04	31.04	31.58	34.09	34.35	34.17	34.17	34.17	34.17	34.17	35.23	35.24	35.24	35.23	35.23	31.07
Texas	45.88	45.88	47.63	44.55	44.15	45.97	43.63	43.41	43.23	43.23	42.85	42.41	42.51	41.18	41.36	41.36	41.36	41.36	41.41	41.41	41.52	29.1	29.1	29.1	28.99	28.59	29.87
Utah	59.05	59.05	58.69	58.35	58.39	58.39	58.69	58.69	58.7	58.7	58.7	56.11	56.1	56.1	56.1	56.08	56.24	56.24	32.56	56.24	55.58	49.28	49.09	49.09	48.71	48.71	49.2
Vermont	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	58.57	58.57	58.57	77.62	77.62	77.62	77.62	77.62	77.62
Virginia	43.21	43.21	43.9	44.01	44.39	44.38	44.38	44.38	41.51	41.25	40.73	37.38	37.11	37.11	36.77	36.73	38.44	38.44	39.03	56.23	55.85	56.85	56.86	54.05	55.34	55.34	54.16
Washington	20.46	20.46	20.58	20.58	20.57	20.57	20.57	20.57	20.57	20.57	20.57	22.26	46.18	46.18	46.09	46.09	46.09	46.09	46.09	46.09	46.09	57.17	57.17	57.17	57.17	57.17	56.09
West Virginia	28.67	28.67	28.78	28.7	28.7	28.7	28.7	28.7	28.7	28.48	28.48	29	28.6	28.62	28.34	28.34	28.34	28.34	28.34	28.34	28.34	27.65	27.65	27.98	27.97	27.97	22.24
Wisconsin	36.33	36.33	36.3	36.3	36.3	36.3	36.23	36.23	36.23	36.23	36.23	36.28	36.28	32.24	32.26	32.58	32.51	32.51	31.82	31.82	31.82	26.23	26.16	26.16	26.16	25.91	29.27
Wyoming	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	34.36	34.36	31.78	31.78	31.78	31.78	31.78	31.78	31.78	31.78	33.3	33.3	33.3	33.3	33.3	31.28
National	29.26	29.26	30.17	29.78	29.19	29.8	29.25	29.28	29.23	29.25	29.22	30.78	30.92	30.92	31.41	31.75	31.93	31.94	31.86	32.06	32.36	32.42	32.43	32.37	32.23	32.5	32.84
"

# Read the complete data
Hpsa22 <- read.table(text = complete_data, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

cat("Total states loaded:", nrow(Hpsa22), "\n")
cat("State names:", paste(head(Hpsa22$State, 10), collapse=", "), "...\n")

# Function to analyze single state
analyze_state <- function(state_name, values) {
  
  # Remove any NA values
  values <- values[!is.na(values)]
  
  if(length(values) < 8) {
    cat("Warning:", state_name, "has insufficient data\n")
    return(NULL)
  }
  
  # Create time series
  ts_data <- ts(values, start = c(2019, 1), frequency = 4)
  
  # Fit auto ARIMA model
  model <- tryCatch({
    auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
  }, error = function(e) {
    tryCatch({
      auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE, approximation = TRUE)
    }, error = function(e2) {
      return(NULL)
    })
  })
  
  if(is.null(model)) {
    cat("Warning:", state_name, "- model failed\n")
    return(NULL)
  }
  
  # Get model specifications
  p <- model$arma[1]
  d <- model$arma[6]
  q <- model$arma[2]
  P <- model$arma[3]
  D <- model$arma[7]
  Q <- model$arma[4]
  s <- model$arma[5]
  
  model_type <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  if(P > 0 | D > 0 | Q > 0) {
    model_type <- paste0(model_type, "(", P, ",", D, ",", Q, ")[", s, "]")
  }
  
  # Get coefficients
  coeff <- coef(model)
  
  # Calculate metrics
  fitted_vals <- fitted(model)
  resid_vals <- residuals(model)
  
  # MAPE
  non_zero <- ts_data != 0
  if(sum(non_zero) > 0) {
    mape <- mean(abs(resid_vals[non_zero] / ts_data[non_zero]), na.rm = TRUE) * 100
  } else {
    mape <- NA
  }
  
  # RMSE
  rmse <- sqrt(mean(resid_vals^2, na.rm = TRUE))
  
  # R-squared
  r2 <- cor(ts_data, fitted_vals)^2
  
  # Adjusted R-squared
  n <- length(ts_data)
  k <- length(coeff)
  r2_adj <- 1 - (1 - r2) * ((n - 1) / (n - k - 1))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # Ljung-Box test
  lb_pvalue <- Box.test(resid_vals, type = "Ljung-Box", lag = min(10, length(resid_vals)/4))$p.value
  
  # Average
  avg_value <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 20 quarters (2026-2030)
  forecast_result <- forecast(model, h = 20)
  forecast_vals <- as.numeric(forecast_result$mean)
  
  # Projections (annual averages)
  proj_2026 <- mean(forecast_vals[1:4])
  proj_2027 <- mean(forecast_vals[5:8])
  proj_2028 <- mean(forecast_vals[9:12])
  proj_2029 <- mean(forecast_vals[13:16])
  proj_2030 <- mean(forecast_vals[17:20])
  
  # Trend detection using linear regression
  time_points <- 1:length(ts_data)
  lm_model <- lm(as.numeric(ts_data) ~ time_points)
  slope <- coef(lm_model)[2]
  slope_pval <- summary(lm_model)$coefficients[2, 4]
  
  if(slope_pval < 0.05) {
    trend <- ifelse(slope > 0, "Increasing (significant)", "Decreasing (significant)")
  } else {
    trend <- "Stable (non-significant)"
  }
  
  # Get intercept and AR1 coefficient
  intercept_val <- ifelse("intercept" %in% names(coeff), coeff["intercept"], 
                          ifelse("mu" %in% names(coeff), coeff["mu"], NA))
  
  ar1_val <- ifelse("ar1" %in% names(coeff), coeff["ar1"], NA)
  ma1_val <- ifelse("ma1" %in% names(coeff), coeff["ma1"], NA)
  
  # Create result
  result <- data.frame(
    State = state_name,
    ARIMA_Model = model_type,
    Intercept = ifelse(is.na(intercept_val), NA, round(intercept_val, 4)),
    AR1 = ifelse(is.na(ar1_val), NA, round(ar1_val, 4)),
    MA1 = ifelse(is.na(ma1_val), NA, round(ma1_val, 4)),
    R2 = round(r2, 4),
    R2_Adj = round(r2_adj, 4),
    MAPE = round(mape, 2),
    RMSE = round(rmse, 4),
    AIC = round(aic, 2),
    BIC = round(bic, 2),
    Ljung_Box_pvalue = round(lb_pvalue, 4),
    Average_2019_2025 = round(avg_value, 2),
    Projected_2026 = round(proj_2026, 2),
    Projected_2027 = round(proj_2027, 2),
    Projected_2028 = round(proj_2028, 2),
    Projected_2029 = round(proj_2029, 2),
    Projected_2030 = round(proj_2030, 2),
    Trend = trend,
    Forecast_CI_Lower = round(forecast_result$lower[20, 1], 2),
    Forecast_CI_Upper = round(forecast_result$upper[20, 1], 2),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Process all states
results <- list()
successful_states <- 0
failed_states <- 0

for(i in 1:nrow(Hpsa22)) {
  state_name <- Hpsa22$State[i]
  values <- as.numeric(Hpsa22[i, -1])  # Exclude State column
  
  cat(sprintf("Processing %2d/%-2d: %-20s", i, nrow(Hpsa22), state_name))
  
  result <- tryCatch({
    analyze_state(state_name, values)
  }, error = function(e) {
    cat(" - ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if(!is.null(result)) {
    results[[length(results) + 1]] <- result
    successful_states <- successful_states + 1
    cat(" - SUCCESS\n")
  } else {
    failed_states <- failed_states + 1
    cat(" - FAILED\n")
  }
}

# Combine results
if(length(results) > 0) {
  final_results <- bind_rows(results)
  
  # Display summary
  cat("\n\n" + paste(rep("=", 60), collapse=""), "\n")
  cat("ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat("Total states processed:", nrow(Hpsa22), "\n")
  cat("Successful models:", successful_states, "\n")
  cat("Failed models:", failed_states, "\n")
  
  # Display first few results
  cat("\nFirst 10 results:\n")
  print(final_results[1:min(10, nrow(final_results)), 
                      c("State", "ARIMA_Model", "R2", "MAPE", "Projected_2026", "Trend")])
  
  # Export to Excel
  write.xlsx(final_results, "AutoARIMA_Complete_Results.xlsx", rowNames = FALSE)
  
  # Also save as CSV
  write.csv(final_results, "AutoARIMA_Complete_Results.csv", row.names = FALSE)
  
  # Create summary statistics
  summary_stats <- data.frame(
    Metric = c("Total States", "Successful Models", "Failed Models",
               "Average R²", "Average MAPE", "Average RMSE",
               "Increasing Trends", "Decreasing Trends", "Stable Trends"),
    Value = c(nrow(Hpsa22), successful_states, failed_states,
              round(mean(final_results$R2, na.rm=TRUE), 4),
              round(mean(final_results$MAPE, na.rm=TRUE), 2),
              round(mean(final_results$RMSE, na.rm=TRUE), 4),
              sum(grepl("Increasing", final_results$Trend)),
              sum(grepl("Decreasing", final_results$Trend)),
              sum(grepl("Stable", final_results$Trend)))
  )
  
  write.xlsx(summary_stats, "AutoARIMA_Summary.xlsx", rowNames = FALSE)
  
  cat("\n\nFiles saved:\n")
  cat("  - AutoARIMA_Complete_Results.xlsx (full results)\n")
  cat("  - AutoARIMA_Complete_Results.csv (CSV format)\n")
  cat("  - AutoARIMA_Summary.xlsx (summary statistics)\n")
  
} else {
  cat("\nNo successful models were created. Please check your data.\n")
}


# Load required libraries
library(tidyverse)
library(forecast)
library(openxlsx)

# Complete dataset with all 50 states + National
complete_data <- "
State	Q12019	Q22019	Q32019	Q42019	Q12020	Q22020	Q32020	Q42020	Q12021	Q22021	Q32021	Q42021	Q12022	Q22022	Q32022	Q42022	Q12023	Q22023	Q32023	Q42023	Q12024	Q22024	Q32024	Q42024	Q12025	Q22025	Q42025
Alabama	20.59	20.59	20.57	20.57	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.56	20.5	20.5	20.5	20.47	21.77	21.85	21.85	25.09	25.09	25.09	25.09	25.04	24.33
Alaska	32.97	32.97	32.97	32.97	32.97	32.97	26.32	26.32	26.32	26.32	26.32	28.59	28.59	27.11	34.71	34.71	34.71	34.71	34.71	34.71	34.98	34.2	34.21	34.21	34.2	34.4	33.83
Arizona	32.95	32.95	34.5	34.5	34.49	34.49	34.49	34.49	34.49	34.45	34.39	30.35	30.37	30.51	30.49	32.94	33.67	33.67	33.67	33.67	33.67	34.48	34.48	34.48	34.48	34.48	34.31
Arkansas	36.02	36.02	37.27	36.94	36.94	36.94	36.94	37.24	36.62	36.62	36.62	36.5	36.54	36.23	36.78	36.78	36.78	36.78	36.78	36.78	36.78	36.7	36.71	36.71	36.7	36.7	33.43
California	31.25	31.25	22.4	22.29	22.28	23.99	25.33	24.82	23.92	25.45	25.26	27.78	27.86	28.27	31.8	32.98	35.18	35.18	34.76	36.14	35.8	38.32	37.76	37.76	37.43	37.43	36.73
Colorado	36.76	36.76	37.99	37.99	37.99	37.99	37.99	38.09	38.09	38.09	37.89	49.57	49.36	49.88	49.03	49.03	53.41	53.48	53.48	53.48	53.48	58.12	58.13	58.13	58.12	58.12	58.77
Delaware	6.84	6.84	6.84	6.84	6.83	6.83	6.83	6.83	6.83	6.83	6.83	7.99	4.98	4.98	4.98	4.98	4.98	4.98	4.98	4.98	4.98	7.41	7.42	7.42	5.46	5.46	5.46
District of Columbia	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.41	2.9	2.9	2.9	2.9	2.9	2.9	2.9	2.91	2.91	2.9	2.9	2.9
Florida	13.22	13.22	12.79	12.6	12.58	12.58	12.68	13.36	13.31	13.31	13.3	14	13.98	14.14	15.59	16.6	16.64	16.64	16.64	16.95	16.68	18.81	18.83	18.76	18.76	18.72	18.78
Georgia	22.87	22.87	22.58	22.58	22.57	22.57	22.53	22.54	22.89	22.19	20.22	18.57	18.63	18.14	18.04	18.04	18.04	18.04	18.02	18.02	18.02	17.15	17.15	17.26	17.26	17.26	17.78
Hawaii	52.63	52.63	56.49	55.05	53.34	51.85	47.73	47.73	43.39	43.4	40.32	43.37	43.37	43.37	43.21	43.21	43.21	43.21	43.21	43.21	45.67	40.34	40.34	40.34	40.34	40.34	40.34
Idaho	45.25	45.25	44.24	44.24	43.16	43.16	43.16	52.61	53.02	59.73	63.5	48.85	48.04	48.04	47.46	47.46	47.46	47.46	48.32	48.32	48.32	49.7	49.7	48.44	48.65	48.65	37.48
Illinois	37.11	37.11	38.43	38.14	38.12	37.65	35.32	35.32	35.32	33.52	35.32	28.97	28.32	28.41	29.17	29.08	29.08	29.08	29.08	29.08	29.08	24.71	24.71	24.71	24.74	24.74	28.39
Indiana	30.57	30.57	32.41	32.41	32.4	32.4	32.14	32.14	32.14	32.14	32.14	32.65	32.65	32.65	32.7	33.14	33.14	33.14	33.14	33.18	33.18	36.73	43.36	38.91	34.46	34.46	27.84
Iowa	35.36	35.36	35.15	35.15	35.15	35.15	38.17	38.08	34.94	34.94	34.94	32.89	32.89	31.83	31.83	31.99	31.99	31.99	28.87	28.87	28.87	20.63	20.64	16.79	16.78	16.78	27.27
Kansas	24.49	24.49	24.87	24.87	25.91	30.05	40.21	40.21	40.21	40.21	40.21	38.55	38.55	38.55	38.49	38.49	38.49	38.49	38.49	38.49	38.69	27.72	28.01	28.01	28.01	28.01	29.47
Kentucky	49.72	49.72	49.55	40.88	39.13	34.23	22.68	22.52	20.34	20.91	20.68	16.85	17.21	17.88	16.66	16.66	16.66	16.66	16.01	16.06	16.36	13.93	13.68	13.59	11.25	11.25	10.4
Louisiana	55.18	55.18	54.33	53.96	53.96	53.96	54.01	43.34	53.27	53.27	53.24	54.21	53.94	50.06	49.94	49.94	49.94	49.94	49.94	49.94	49.94	46.31	46.31	46.31	46.31	46.31	45.8
Maine	40.1	40.1	40	40.1	39.87	39.87	39.87	39.87	39.87	39.87	37	32.24	30.42	30.42	32	32.29	32.29	32.29	32.29	32.29	31.92	27.97	27.97	27.97	27.97	27.97	28.88
Maryland	17.91	17.91	21.62	21.62	21.61	21.61	21.61	21.61	19.97	19.97	19.97	29.4	35.37	34.92	31.94	33.94	33.94	33.94	33.94	33.94	33.94	38.92	38.93	41.86	41.86	41.86	42.98
Massachusetts	66.17	66.17	76.12	76.12	76.11	76.11	76.13	76.13	76.13	76.13	76.13	76.13	76.13	76.39	75.95	75.95	75.2	75.2	74.31	74.31	74.31	74	63.88	67.43	63.87	63.87	63.87
Michigan	5.56	5.56	6.22	6.22	6.22	6.22	6.22	5.73	6.29	6.29	6.29	26.29	26.29	26.35	26.35	26.02	26.02	26.02	26.62	26.62	26.62	27.39	27.4	27.65	27.65	27.65	29.9
Minnesota	15.58	15.58	15.57	22.92	23.5	27.52	26.19	26.19	26.05	26.04	31.46	29.05	29.06	39.3	41.16	41.16	41.16	41.16	41.16	39.9	38.73	44.86	44.86	45.21	45.3	45.3	44.05
Mississippi	45.53	45.53	45.83	45.83	45.82	45.82	45.82	45.82	45.82	45.82	45.66	56.4	54.65	56.43	54.86	54.98	54.98	54.98	54.98	54.98	54.98	50.2	50.2	50.2	50.2	50.2	40.69
Missouri	12.5	12.5	12.34	12.34	12.34	12.31	12.31	12.31	12.31	12.31	12.31	12.27	12.27	11.93	20.86	22.89	22.89	22.89	22.89	22.89	22.89	20.81	20.81	20.81	20.81	20.81	20.87
Montana	31.58	31.58	31.98	31.98	31.97	31.97	31.97	31.97	31.97	31.97	31.97	40.95	40.95	40.66	40.63	40.63	40.63	40.63	40.63	40.63	40.63	42.56	42.56	42.56	42.56	42.64	45.86
Nevada	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	32.99	28.59	30.81	30.36	30.36	30.11	30.11	30.11	30.11	30.11	30.11	26.48	26.48	26.48	26.48	26.48	26.18
New Hampshire	40.32	40.32	40.46	40.46	40.45	40.45	38.21	38.21	38.21	38.21	38.21	24.4	24.4	23.16	21.48	21.48	21.48	21.48	21.48	21.48	21.48	26.51	26.52	26.52	26.51	26.51	21.63
New Jersey	25.96	25.96	42.94	42.94	42.94	42.94	42.94	42.94	42.94	42.94	41.79	41.79	41.79	39.83	38.1	37.63	37.63	37.63	37.63	29.18	29.18	27.77	27.78	27.78	27.77	43.64	50.83
New Mexico	21.44	21.44	21.14	21.14	21.14	21.14	21.14	21.14	21.14	21.14	21.14	27.05	21.86	23.72	23.72	23.72	23.72	23.72	23.72	22.81	22.81	20.6	20.6	20.6	20.6	20.6	21.44
New York	23.1	23.1	19.32	18.22	17.81	17.81	17.33	17.33	17.33	17.32	17.32	17.34	17.3	17.31	17.31	17.31	17.31	17.31	17.77	16.68	16.68	15.9	16.15	16.15	16.15	16.15	16.15
North Carolina	18.69	18.69	19.55	19.55	12.41	12.01	12.12	16.07	18.45	18.45	18.45	26.93	26.74	26.74	26.58	26.55	26.42	26.66	26.83	26.83	26.83	22.15	22.16	22.16	22	22.01	22.01
North Dakota	33.66	33.66	33.66	33.79	33.78	31.6	31.6	41.52	41.52	41.52	41.52	44.11	51.35	50.1	50.1	52.48	52.48	52.48	52.48	52.48	50.51	51.42	51.42	51.42	51.42	50.06	40.44
Ohio	34.13	34.13	34.24	34.24	34.23	34	34	34	34	34.05	33.89	27.19	26.98	27.45	27.45	27.45	27.45	27.45	27.45	27.45	27.61	26.84	26.84	26.84	28.07	36.18	39.64
Oklahoma	35.52	35.52	40.14	40.14	40.13	40.13	39.52	39.52	39.52	39.52	39.52	34.09	34.09	34.75	34.75	34.75	34.75	34.75	34.55	34.55	34.55	33.52	33.53	29.56	29.55	29.55	41.56
Oregon	20.37	20.37	20.42	20.42	20.41	20.41	21.92	21.92	21.81	21.31	22.61	27.82	31.93	31.95	31.95	31.93	31.83	31.83	32.34	32.34	33.27	39.34	39.35	39.5	39.49	39.49	40.88
Pennsylvania	47.08	47.08	47.87	47.87	47.86	47.86	47.86	47.87	47.87	47.79	47.63	39.32	39.02	36.33	36.33	36.33	36.33	36.33	36.33	36.09	37.27	35.93	35.94	35.94	35.56	35.56	39.96
Rhode Island	34.88	34.88	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	43.63	33.54	35.84	35.84	35.9	35.9	35.9	35.9	35.72	35.72	35.72	35.72	35.72	38
South Carolina	40.57	40.57	41	41	41.14	45.3	44.99	44.99	44.99	45.16	45.16	44.91	44.91	44.88	44.8	44.8	44.8	44.8	44.8	44.8	44.8	44.71	44.88	44.88	44.88	44.88	45.32
South Dakota	27.12	27.12	25.46	25.46	23.14	23.35	22.93	22.93	22.93	22.93	24.55	20.53	20.53	32.56	32.56	32.56	32.56	32.56	56.24	31.09	31.09	30.85	30.86	30.63	31.37	29.51	26.98
Tennessee	29.28	29.28	29.34	29.34	29.33	29.33	29.33	29.33	29.33	29.33	29.33	31.04	31.04	31.58	34.09	34.35	34.17	34.17	34.17	34.17	34.17	35.23	35.24	35.24	35.23	35.23	31.07
Texas	45.88	45.88	47.63	44.55	44.15	45.97	43.63	43.41	43.23	43.23	42.85	42.41	42.51	41.18	41.36	41.36	41.36	41.36	41.41	41.41	41.52	29.1	29.1	29.1	28.99	28.59	29.87
Utah	59.05	59.05	58.69	58.35	58.39	58.39	58.69	58.69	58.7	58.7	58.7	56.11	56.1	56.1	56.1	56.08	56.24	56.24	32.56	56.24	55.58	49.28	49.09	49.09	48.71	48.71	49.2
Vermont	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	53.66	58.57	58.57	58.57	77.62	77.62	77.62	77.62	77.62	77.62
Virginia	43.21	43.21	43.9	44.01	44.39	44.38	44.38	44.38	41.51	41.25	40.73	37.38	37.11	37.11	36.77	36.73	38.44	38.44	39.03	56.23	55.85	56.85	56.86	54.05	55.34	55.34	54.16
Washington	20.46	20.46	20.58	20.58	20.57	20.57	20.57	20.57	20.57	20.57	20.57	22.26	46.18	46.18	46.09	46.09	46.09	46.09	46.09	46.09	46.09	57.17	57.17	57.17	57.17	57.17	56.09
West Virginia	28.67	28.67	28.78	28.7	28.7	28.7	28.7	28.7	28.7	28.48	28.48	29	28.6	28.62	28.34	28.34	28.34	28.34	28.34	28.34	28.34	27.65	27.65	27.98	27.97	27.97	22.24
Wisconsin	36.33	36.33	36.3	36.3	36.3	36.3	36.23	36.23	36.23	36.23	36.23	36.28	36.28	32.24	32.26	32.58	32.51	32.51	31.82	31.82	31.82	26.23	26.16	26.16	26.16	25.91	29.27
Wyoming	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	51.72	34.36	34.36	31.78	31.78	31.78	31.78	31.78	31.78	31.78	31.78	33.3	33.3	33.3	33.3	33.3	31.28
National	29.26	29.26	30.17	29.78	29.19	29.8	29.25	29.28	29.23	29.25	29.22	30.78	30.92	30.92	31.41	31.75	31.93	31.94	31.86	32.06	32.36	32.42	32.43	32.37	32.23	32.5	32.84
"

# Read the complete data
Hpsa22 <- read.table(text = complete_data, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

cat("Total states loaded:", nrow(Hpsa22), "\n")
cat("State names:", paste(head(Hpsa22$State, 10), collapse=", "), "...\n")

# Function to analyze single state
analyze_state <- function(state_name, values) {
  
  # Remove any NA values
  values <- values[!is.na(values)]
  
  if(length(values) < 8) {
    return(NULL)
  }
  
  # Create time series
  ts_data <- ts(values, start = c(2019, 1), frequency = 4)
  
  # Fit auto ARIMA model
  model <- tryCatch({
    auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
  }, error = function(e) {
    tryCatch({
      auto.arima(ts_data, seasonal = FALSE, stepwise = TRUE, approximation = TRUE)
    }, error = function(e2) {
      return(NULL)
    })
  })
  
  if(is.null(model)) {
    return(NULL)
  }
  
  # Get model specifications
  p <- model$arma[1]
  d <- model$arma[6]
  q <- model$arma[2]
  P <- model$arma[3]
  D <- model$arma[7]
  Q <- model$arma[4]
  s <- model$arma[5]
  
  model_type <- paste0("ARIMA(", p, ",", d, ",", q, ")")
  if(P > 0 | D > 0 | Q > 0) {
    model_type <- paste0(model_type, "(", P, ",", D, ",", Q, ")[", s, "]")
  }
  
  # Get coefficients
  coeff <- coef(model)
  
  # Calculate metrics
  fitted_vals <- fitted(model)
  resid_vals <- residuals(model)
  
  # MAPE
  non_zero <- ts_data != 0
  if(sum(non_zero) > 0) {
    mape <- mean(abs(resid_vals[non_zero] / ts_data[non_zero]), na.rm = TRUE) * 100
  } else {
    mape <- NA
  }
  
  # RMSE
  rmse <- sqrt(mean(resid_vals^2, na.rm = TRUE))
  
  # R-squared
  r2 <- cor(ts_data, fitted_vals)^2
  
  # Adjusted R-squared
  n <- length(ts_data)
  k <- length(coeff)
  r2_adj <- 1 - (1 - r2) * ((n - 1) / (n - k - 1))
  
  # AIC and BIC
  aic <- AIC(model)
  bic <- BIC(model)
  
  # Ljung-Box test
  lb_pvalue <- Box.test(resid_vals, type = "Ljung-Box", lag = min(10, length(resid_vals)/4))$p.value
  
  # Average
  avg_value <- mean(ts_data, na.rm = TRUE)
  
  # Forecast for 20 quarters (2026-2030)
  forecast_result <- forecast(model, h = 20)
  forecast_vals <- as.numeric(forecast_result$mean)
  
  # Projections (annual averages)
  proj_2026 <- mean(forecast_vals[1:4])
  proj_2027 <- mean(forecast_vals[5:8])
  proj_2028 <- mean(forecast_vals[9:12])
  proj_2029 <- mean(forecast_vals[13:16])
  proj_2030 <- mean(forecast_vals[17:20])
  
  # Trend detection using linear regression
  time_points <- 1:length(ts_data)
  lm_model <- lm(as.numeric(ts_data) ~ time_points)
  slope <- coef(lm_model)[2]
  slope_pval <- summary(lm_model)$coefficients[2, 4]
  
  if(slope_pval < 0.05) {
    trend <- ifelse(slope > 0, "Increasing (significant)", "Decreasing (significant)")
  } else {
    trend <- "Stable (non-significant)"
  }
  
  # Get intercept and AR1 coefficient
  intercept_val <- ifelse("intercept" %in% names(coeff), coeff["intercept"], 
                          ifelse("mu" %in% names(coeff), coeff["mu"], NA))
  
  ar1_val <- ifelse("ar1" %in% names(coeff), coeff["ar1"], NA)
  ma1_val <- ifelse("ma1" %in% names(coeff), coeff["ma1"], NA)
  
  # Create result
  result <- data.frame(
    State = state_name,
    ARIMA_Model = model_type,
    Intercept = ifelse(is.na(intercept_val), NA, round(intercept_val, 4)),
    AR1 = ifelse(is.na(ar1_val), NA, round(ar1_val, 4)),
    MA1 = ifelse(is.na(ma1_val), NA, round(ma1_val, 4)),
    R2 = round(r2, 4),
    R2_Adj = round(r2_adj, 4),
    MAPE = round(mape, 2),
    RMSE = round(rmse, 4),
    AIC = round(aic, 2),
    BIC = round(bic, 2),
    Ljung_Box_pvalue = round(lb_pvalue, 4),
    Average_2019_2025 = round(avg_value, 2),
    Projected_2026 = round(proj_2026, 2),
    Projected_2027 = round(proj_2027, 2),
    Projected_2028 = round(proj_2028, 2),
    Projected_2029 = round(proj_2029, 2),
    Projected_2030 = round(proj_2030, 2),
    Trend = trend,
    Forecast_CI_Lower = round(forecast_result$lower[20, 1], 2),
    Forecast_CI_Upper = round(forecast_result$upper[20, 1], 2),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Process all states
results <- list()
successful_states <- 0
failed_states <- 0

for(i in 1:nrow(Hpsa22)) {
  state_name <- Hpsa22$State[i]
  values <- as.numeric(Hpsa22[i, -1])  # Exclude State column
  
  cat(sprintf("Processing %2d/%-2d: %-20s", i, nrow(Hpsa22), state_name))
  
  result <- tryCatch({
    analyze_state(state_name, values)
  }, error = function(e) {
    cat(" - ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if(!is.null(result)) {
    results[[length(results) + 1]] <- result
    successful_states <- successful_states + 1
    cat(" - SUCCESS\n")
  } else {
    failed_states <- failed_states + 1
    cat(" - FAILED\n")
  }
}

# Combine results
if(length(results) > 0) {
  final_results <- bind_rows(results)
  
  # Display summary
  cat("\n\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat("ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat("Total states processed:", nrow(Hpsa22), "\n")
  cat("Successful models:", successful_states, "\n")
  cat("Failed models:", failed_states, "\n")
  
  # Display first few results
  cat("\nFirst 10 results:\n")
  print(final_results[1:min(10, nrow(final_results)), 
                      c("State", "ARIMA_Model", "R2", "MAPE", "Projected_2026", "Trend")])
  
  # Export to Excel
  write.xlsx(final_results, "AutoARIMA_Complete_Results.xlsx", rowNames = FALSE)
  
  # Also save as CSV
  write.csv(final_results, "AutoARIMA_Complete_Results.csv", row.names = FALSE)
  
  # Create summary statistics
  summary_stats <- data.frame(
    Metric = c("Total States", "Successful Models", "Failed Models",
               "Average R²", "Average MAPE", "Average RMSE",
               "Increasing Trends", "Decreasing Trends", "Stable Trends"),
    Value = c(nrow(Hpsa22), successful_states, failed_states,
              round(mean(final_results$R2, na.rm=TRUE), 4),
              round(mean(final_results$MAPE, na.rm=TRUE), 2),
              round(mean(final_results$RMSE, na.rm=TRUE), 4),
              sum(grepl("Increasing", final_results$Trend)),
              sum(grepl("Decreasing", final_results$Trend)),
              sum(grepl("Stable", final_results$Trend)))
  )
  
  write.xlsx(summary_stats, "AutoARIMA_Summary.xlsx", rowNames = FALSE)
  
  cat("\n\nFiles saved:\n")
  cat("  - AutoARIMA_Complete_Results.xlsx (full results)\n")
  cat("  - AutoARIMA_Complete_Results.csv (CSV format)\n")
  cat("  - AutoARIMA_Summary.xlsx (summary statistics)\n")
  
} else {
  cat("\nNo successful models were created. Please check your data.\n")
}

names(Reg2)

str(Reg2)

# Load required libraries
library(dplyr)
library(tidyr)

# Load required libraries
library(dplyr)
library(tidyr)

# ------------------------------
# 1. CLEAN THE DATA
# ------------------------------

# Convert Percentage to numeric (remove % sign if present)
Reg2 <- Reg2 %>%
  mutate(`Percentage of All Designations` = as.numeric(gsub("%", "", `Percentage of All Designations`)))

# Filter for relevant rows
rural_data <- Reg2 %>%
  filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == "Rural")

nonrural_data <- Reg2 %>%
  filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == "Non-Rural")

combined_data <- Reg2 %>%
  filter(!`Health Professional Shortage Areas: Rural/Non-Rural Classification` %in% 
           c("Dental HPSA Totals", "Partially Rural", "Partially Rural(11)"))

# Check counts
cat("Rows in Rural:", nrow(rural_data), "\n")
cat("Rows in Non-Rural:", nrow(nonrural_data), "\n")
cat("Rows in Combined:", nrow(combined_data), "\n")

# ------------------------------
# 2. FUNCTION TO RUN REGRESSION AND PRINT RESULTS
# ------------------------------

run_regression <- function(data, name, dependent_col = "Number of Designations") {
  cat("\n========================================\n")
  cat("REGRESSION FOR:", name, "\n")
  cat("========================================\n")
  
  # Extract x and y using backticks for column names with spaces
  y <- data[[dependent_col]]
  x <- data[["Year"]]
  
  # Remove any NA values
  valid_idx <- complete.cases(x, y)
  y <- y[valid_idx]
  x <- x[valid_idx]
  
  if(length(x) < 3) {
    cat("Not enough data points for regression\n")
    return(NULL)
  }
  
  # Run linear regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Extract coefficients
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # Equation
  cat("\n📈 REGRESSION EQUATION:\n")
  cat(sprintf("%s = %.2f × Year + %.2f\n", dependent_col, slope, intercept))
  
  # R-squared
  cat("\n📊 R-SQUARED:\n")
  cat(sprintf("R² = %.4f\n", summary_model$r.squared))
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat("\n🎯 P-VALUE:\n")
  cat(sprintf("p = %.6f", p_value))
  if(p_value < 0.05) cat(" (SIGNIFICANT at 95% CI)") else cat(" (NOT significant at 95% CI)")
  cat("\n")
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  cat("\n🔍 95% CONFIDENCE INTERVALS:\n")
  cat(sprintf("Slope: [%.2f , %.2f]\n", ci[2,1], ci[2,2]))
  cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
  
  # Additional stats
  cat("\n📋 ADDITIONAL STATISTICS:\n")
  cat(sprintf("Standard Error of Slope: %.2f\n", summary_model$coefficients[2,2]))
  cat(sprintf("t-statistic: %.2f\n", summary_model$coefficients[2,3]))
  
  # Return model invisibly
  invisible(list(model = model, summary = summary_model, p_value = p_value, ci = ci))
}

# ------------------------------
# 3. RUN REGRESSIONS
# ------------------------------

# Rural regression - Number of Designations
rural_result <- run_regression(rural_data, "RURAL (All Years)", "Number of Designations")

# Non-Rural regression - Number of Designations
nonrural_result <- run_regression(nonrural_data, "NON-RURAL (All Years)", "Number of Designations")

# Combined regression - Number of Designations
combined_result <- run_regression(combined_data, "COMBINED (Rural + Non-Rural)", "Number of Designations")

# ------------------------------
# 4. REGRESSION WITH OTHER DEPENDENT VARIABLES
# ------------------------------

cat("\n\n========================================\n")
cat("ADDITIONAL ANALYSES\n")
cat("========================================\n")

# Population analysis
run_regression(rural_data, "RURAL - Population of HPSAs", "Population of Designated HPSAs")
run_regression(nonrural_data, "NON-RURAL - Population of HPSAs", "Population of Designated HPSAs")

# Practitioners needed analysis
run_regression(rural_data, "RURAL - Practitioners Needed", "Practitioners Needed to Remove Designations")
run_regression(nonrural_data, "NON-RURAL - Practitioners Needed", "Practitioners Needed to Remove Designations")

# ------------------------------
# 5. VISUALIZATIONS
# ------------------------------

# Set up plotting area
par(mfrow = c(1, 3))

# Rural plot
if(!is.null(rural_result)) {
  plot(rural_data$Year, rural_data$`Number of Designations`, 
       main = "Rural: Designations over Time",
       xlab = "Year", ylab = "Number of Designations", pch = 16, col = "blue")
  abline(rural_result$model, col = "red", lwd = 2)
}

# Non-Rural plot
if(!is.null(nonrural_result)) {
  plot(nonrural_data$Year, nonrural_data$`Number of Designations`, 
       main = "Non-Rural: Designations over Time",
       xlab = "Year", ylab = "Number of Designations", pch = 16, col = "blue")
  abline(nonrural_result$model, col = "red", lwd = 2)
}

# Combined plot
if(!is.null(combined_result)) {
  plot(combined_data$Year, combined_data$`Number of Designations`, 
       main = "Combined: Designations over Time",
       xlab = "Year", ylab = "Number of Designations", pch = 16, col = "blue")
  abline(combined_result$model, col = "red", lwd = 2)
}

# ------------------------------
# 6. SUMMARY TABLE
# ------------------------------

# Helper function to get slope safely
get_slope <- function(result) {
  if(is.null(result)) return(NA)
  return(coef(result$model)[2])
}

get_intercept <- function(result) {
  if(is.null(result)) return(NA)
  return(coef(result$model)[1])
}

get_r2 <- function(result) {
  if(is.null(result)) return(NA)
  return(result$summary$r.squared)
}

get_pvalue <- function(result) {
  if(is.null(result)) return(NA)
  return(result$p_value)
}

# Create summary table
summary_table <- data.frame(
  Dataset = c("Rural", "Non-Rural", "Combined"),
  Slope = c(get_slope(rural_result), get_slope(nonrural_result), get_slope(combined_result)),
  Intercept = c(get_intercept(rural_result), get_intercept(nonrural_result), get_intercept(combined_result)),
  R_Squared = c(get_r2(rural_result), get_r2(nonrural_result), get_r2(combined_result)),
  P_Value = c(get_pvalue(rural_result), get_pvalue(nonrural_result), get_pvalue(combined_result)),
  Significant = c(get_pvalue(rural_result) < 0.05, 
                  get_pvalue(nonrural_result) < 0.05, 
                  get_pvalue(combined_result) < 0.05)
)

cat("\n\n========================================\n")
cat("SUMMARY TABLE\n")
cat("========================================\n")
print(summary_table)

# ------------------------------
# 7. SIMPLER ALTERNATIVE USING COLUMN INDEXES
# ------------------------------

# If backticks still cause issues, use column positions:
# Column 1 = Year
# Column 3 = Number of Designations
# Column 5 = Population of Designated HPSAs
# Column 6 = Practitioners Needed

run_regression_simple <- function(data, name, y_col_index = 3) {
  cat("\n========================================\n")
  cat("REGRESSION FOR:", name, "\n")
  cat("========================================\n")
  
  y <- data[[y_col_index]]
  x <- data[[1]]  # Year is column 1
  
  valid_idx <- complete.cases(x, y)
  y <- y[valid_idx]
  x <- x[valid_idx]
  
  if(length(x) < 3) {
    cat("Not enough data points\n")
    return(NULL)
  }
  
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  cat(sprintf("\nEquation: y = %.2f × Year + %.2f\n", coef(model)[2], coef(model)[1]))
  cat(sprintf("R² = %.4f\n", summary_model$r.squared))
  
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat(sprintf("p-value = %.6f %s\n", p_value, ifelse(p_value < 0.05, "(SIGNIFICANT)", "(NOT significant)")))
  
  ci <- confint(model, level = 0.95)
  cat(sprintf("95%% CI for Slope: [%.2f , %.2f]\n", ci[2,1], ci[2,2]))
  
  invisible(list(model = model, p_value = p_value))
}

# Run using column indexes
cat("\n\n========== USING COLUMN INDEXES (Simpler) ==========\n")
run_regression_simple(rural_data, "RURAL", 3)      # Column 3 = Number of Designations
run_regression_simple(nonrural_data, "NON-RURAL", 3)
run_regression_simple(combined_data, "COMBINED", 3)


# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# ------------------------------
# 1. EXTRACT DATA FOR THREE CATEGORIES
# ------------------------------

# Rural data
rural_data <- Reg2 %>%
  filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == "Rural") %>%
  select(Year, `Number of Designations`) %>%
  mutate(Category = "Rural")

# Non-Rural data
nonrural_data <- Reg2 %>%
  filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == "Non-Rural") %>%
  select(Year, `Number of Designations`) %>%
  mutate(Category = "Non-Rural")

# Partially Rural data (note: your data has "Partially Rural" and "Partially Rural(11)")
partially_rural_data <- Reg2 %>%
  filter(grepl("Partially Rural", `Health Professional Shortage Areas: Rural/Non-Rural Classification`)) %>%
  select(Year, `Number of Designations`) %>%
  mutate(Category = "Partially Rural")

# Combine all three
all_categories <- bind_rows(rural_data, nonrural_data, partially_rural_data)

# Remove any NA values
all_categories <- all_categories %>%
  filter(!is.na(`Number of Designations`), !is.na(Year))

# View data structure
cat("Data Summary:\n")
cat("Rural:", nrow(rural_data), "observations\n")
cat("Non-Rural:", nrow(nonrural_data), "observations\n")
cat("Partially Rural:", nrow(partially_rural_data), "observations\n")

# ------------------------------
# 2. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression_analysis <- function(data, category_name) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("REGRESSION ANALYSIS:", category_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Extract variables
  x <- data$Year
  y <- data$`Number of Designations`
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Get coefficients
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Equation
  cat("\n📈 REGRESSION EQUATION:\n")
  cat(sprintf("HPSA Designations = %.2f × Year + %.2f\n", slope, intercept))
  
  # R-squared
  cat("\n📊 R-SQUARED:\n")
  cat(sprintf("R² = %.4f (%.1f%% of variance explained)\n", 
              summary_model$r.squared, summary_model$r.squared * 100))
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat("\n🎯 P-VALUE:\n")
  if(p_value < 0.001) {
    cat(sprintf("p < 0.001 (%.2e) - HIGHLY SIGNIFICANT\n", p_value))
  } else if(p_value < 0.05) {
    cat(sprintf("p = %.4f - SIGNIFICANT at 95%% CI\n", p_value))
  } else {
    cat(sprintf("p = %.4f - NOT significant at 95%% CI\n", p_value))
  }
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  cat("\n🔍 95% CONFIDENCE INTERVALS:\n")
  cat(sprintf("Slope: [%.2f , %.2f] (%.1f%% range)\n", ci[2,1], ci[2,2], 
              (ci[2,2] - ci[2,1])/abs(slope)*100))
  cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
  
  # Additional statistics
  cat("\n📋 ADDITIONAL STATISTICS:\n")
  cat(sprintf("Standard Error of Slope: %.2f\n", summary_model$coefficients[2,2]))
  cat(sprintf("t-statistic: %.2f\n", summary_model$coefficients[2,3]))
  cat(sprintf("Degrees of Freedom: %d\n", summary_model$df[2]))
  cat(sprintf("Adjusted R²: %.4f\n", summary_model$adj.r.squared))
  
  # Yearly change interpretation
  cat("\n💡 INTERPRETATION:\n")
  if(slope > 0) {
    cat(sprintf("Each year, HPSA designations INCREASE by approximately %.1f\n", slope))
  } else {
    cat(sprintf("Each year, HPSA designations DECREASE by approximately %.1f\n", abs(slope)))
  }
  
  # Return model object
  return(list(model = model, summary = summary_model, slope = slope, p_value = p_value))
}

# ------------------------------
# 3. RUN REGRESSIONS FOR ALL THREE CATEGORIES
# ------------------------------

rural_result <- run_regression_analysis(rural_data, "RURAL")
nonrural_result <- run_regression_analysis(nonrural_data, "NON-RURAL")
partially_result <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL")

# ------------------------------
# 4. COMBINED REGRESSION (All three together as factor)
# ------------------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("COMBINED REGRESSION WITH CATEGORY INTERACTION\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Model with interaction (separate slopes for each category)
combined_model <- lm(`Number of Designations` ~ Year * Category, data = all_categories)
combined_summary <- summary(combined_model)

cat("\n📈 COMPLETE MODEL (Different slopes for each category):\n")
print(combined_summary$coefficients)

cat("\n📊 MODEL FIT:\n")
cat(sprintf("R² = %.4f (%.1f%% of variance explained)\n", 
            combined_summary$r.squared, combined_summary$r.squared * 100))
cat(sprintf("Adjusted R² = %.4f\n", combined_summary$adj.r.squared))

# F-test for model significance
f <- combined_summary$fstatistic
p_combined <- pf(f[1], f[2], f[3], lower.tail = FALSE)
cat(sprintf("Overall Model P-value: %.2e\n", p_combined))

# ------------------------------
# 5. SUMMARY TABLE
# ------------------------------

summary_table <- data.frame(
  Category = c("Rural", "Non-Rural", "Partially Rural"),
  Slope = c(rural_result$slope, nonrural_result$slope, partially_result$slope),
  Intercept = c(coef(rural_result$model)[1], coef(nonrural_result$model)[1], coef(partially_result$model)[1]),
  R_Squared = c(rural_result$summary$r.squared, 
                nonrural_result$summary$r.squared, 
                partially_result$summary$r.squared),
  P_Value = c(rural_result$p_value, nonrural_result$p_value, partially_result$p_value),
  Significant = c(rural_result$p_value < 0.05, 
                  nonrural_result$p_value < 0.05, 
                  partially_result$p_value < 0.05),
  Trend = c(ifelse(rural_result$slope > 0, "Increasing", "Decreasing"),
            ifelse(nonrural_result$slope > 0, "Increasing", "Decreasing"),
            ifelse(partially_result$slope > 0, "Increasing", "Decreasing"))
)

cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SUMMARY TABLE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
print(summary_table)

# ------------------------------
# 6. VISUALIZATIONS
# ------------------------------

# Plot 1: Individual scatter plots with regression lines
par(mfrow = c(1, 3))

# Rural plot
plot(rural_data$Year, rural_data$`Number of Designations`,
     main = "Rural HPSA Designations",
     xlab = "Year", ylab = "Number of Designations",
     pch = 19, col = "blue", cex = 1.2)
abline(rural_result$model, col = "red", lwd = 2)
text(2018, max(rural_data$`Number of Designations`), 
     paste("R² =", round(rural_result$summary$r.squared, 3)), pos = 4)

# Non-Rural plot
plot(nonrural_data$Year, nonrural_data$`Number of Designations`,
     main = "Non-Rural HPSA Designations",
     xlab = "Year", ylab = "Number of Designations",
     pch = 19, col = "green", cex = 1.2)
abline(nonrural_result$model, col = "red", lwd = 2)
text(2018, max(nonrural_data$`Number of Designations`), 
     paste("R² =", round(nonrural_result$summary$r.squared, 3)), pos = 4)

# Partially Rural plot
plot(partially_rural_data$Year, partially_rural_data$`Number of Designations`,
     main = "Partially Rural HPSA Designations",
     xlab = "Year", ylab = "Number of Designations",
     pch = 19, col = "purple", cex = 1.2)
abline(partially_result$model, col = "red", lwd = 2)
text(2018, max(partially_rural_data$`Number of Designations`), 
     paste("R² =", round(partially_result$summary$r.squared, 3)), pos = 4)

# Plot 2: All three on same graph using ggplot2
p <- ggplot(all_categories, aes(x = Year, y = `Number of Designations`, 
                                color = Category, group = Category)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  labs(title = "HPSA Designations Trends: Rural vs Non-Rural vs Partially Rural",
       x = "Year",
       y = "Number of HPSA Designations",
       caption = "Shaded areas represent 95% confidence intervals") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "green", 
                                "Partially Rural" = "purple"))

print(p)

# Save the plot
ggsave("HPSA_regression_comparison.png", p, width = 10, height = 6, dpi = 300)

# ------------------------------
# 7. PREDICTIONS FOR FUTURE YEARS
# ------------------------------

cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
cat("PREDICTIONS FOR 2026-2030\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

future_years <- data.frame(Year = c(2026, 2027, 2028, 2029, 2030))

# Predict for each category
predictions <- data.frame(Year = future_years$Year)

for(cat in c("Rural", "Non-Rural", "Partially Rural")) {
  if(cat == "Rural") model <- rural_result$model
  else if(cat == "Non-Rural") model <- nonrural_result$model
  else model <- partially_result$model
  
  pred <- predict(model, newdata = future_years, interval = "confidence", level = 0.95)
  predictions[[paste(cat, "Fit")]] <- round(pred[,1], 0)
  predictions[[paste(cat, "Lower")]] <- round(pred[,2], 0)
  predictions[[paste(cat, "Upper")]] <- round(pred[,3], 0)
}

print(predictions)

# ------------------------------
# 8. EXPORT RESULTS
# ------------------------------

# Save summary table
write.csv(summary_table, "HPSA_regression_summary.csv", row.names = FALSE)
write.csv(predictions, "HPSA_predictions_2026_2030.csv", row.names = FALSE)

# Save models
saveRDS(rural_result$model, "rural_model.rds")
saveRDS(nonrural_result$model, "nonrural_model.rds")
saveRDS(partially_result$model, "partially_rural_model.rds")
saveRDS(combined_model, "combined_model.rds")

cat("\n\n✅ Analysis complete! Results saved to CSV files.\n")


# Check what values exist in the classification column
unique_classifications <- unique(Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`)
print(unique_classifications)

# Also check for any variations with spaces
unique_classifications_trimmed <- unique(trimws(Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`))
print(unique_classifications_trimmed)



# Load required libraries
library(dplyr)
library(ggplot2)

# ------------------------------
# 1. FIRST, EXPLORE YOUR DATA
# ------------------------------

cat("=== EXPLORING YOUR DATA ===\n")
cat("Column names:\n")
print(names(Reg2))

cat("\nUnique values in classification column:\n")
unique_vals <- unique(Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`)
print(unique_vals)

cat("\nFirst 20 rows of data:\n")
print(head(Reg2, 20))

# ------------------------------
# 2. IDENTIFY CATEGORIES AUTOMATICALLY
# ------------------------------

# Look for rows that contain "Non-Rural" (case insensitive)
nonrural_rows <- grepl("Non-Rural", Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`, ignore.case = TRUE)
rural_rows <- grepl("^Rural$", Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`, ignore.case = TRUE)
partially_rows <- grepl("Partially Rural", Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`, ignore.case = TRUE)

# Also check for "Non-Rural" with possible spaces or different formatting
cat("\nChecking for Non-Rural patterns:\n")
nonrural_patterns <- Reg2[grep("Non", Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`, ignore.case = TRUE), ]
print(head(nonrural_patterns))

# ------------------------------
# 3. EXTRACT DATA BASED ON ACTUAL VALUES
# ------------------------------

# Based on your original data print, here's how to properly extract:

# Method 1: Using exact string matching from your data
# From your data, I see "Non-Rural" appears with a possible space or tab

rural_data <- Reg2 %>%
  filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == "Rural")

# Look for Non-Rural (might have leading/trailing spaces)
nonrural_data <- Reg2 %>%
  filter(trimws(`Health Professional Shortage Areas: Rural/Non-Rural Classification`) == "Non-Rural")

# If that doesn't work, try pattern matching
if(nrow(nonrural_data) == 0) {
  nonrural_data <- Reg2 %>%
    filter(grepl("Non-Rural", `Health Professional Shortage Areas: Rural/Non-Rural Classification`, fixed = TRUE))
}

# Partially Rural
partially_rural_data <- Reg2 %>%
  filter(grepl("Partially Rural", `Health Professional Shortage Areas: Rural/Non-Rural Classification`))

# Check if we got data
cat("\n=== DATA COUNTS ===\n")
cat("Rural rows:", nrow(rural_data), "\n")
cat("Non-Rural rows:", nrow(nonrural_data), "\n")
cat("Partially Rural rows:", nrow(partially_rural_data), "\n")

# ------------------------------
# 4. CREATE CLEAN DATASETS FOR REGRESSION
# ------------------------------

# Ensure we have numeric columns
prepare_data <- function(data, name) {
  if(nrow(data) == 0) {
    cat("WARNING: No data found for", name, "\n")
    return(NULL)
  }
  
  result <- data %>%
    select(Year, `Number of Designations`) %>%
    filter(!is.na(`Number of Designations`), !is.na(Year)) %>%
    mutate(Category = name)
  
  cat(name, "-", nrow(result), "valid observations\n")
  return(result)
}

rural_clean <- prepare_data(rural_data, "Rural")
nonrural_clean <- prepare_data(nonrural_data, "Non-Rural")
partially_clean <- prepare_data(partially_rural_data, "Partially Rural")

# ------------------------------
# 5. FUNCTION FOR REGRESSION (with error handling)
# ------------------------------

run_regression_analysis <- function(data, category_name) {
  if(is.null(data) || nrow(data) < 3) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("REGRESSION ANALYSIS:", category_name, "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat("❌ ERROR: Insufficient data (need at least 3 observations)\n")
    cat(sprintf("   Only %d observations available\n", ifelse(is.null(data), 0, nrow(data))))
    return(NULL)
  }
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("REGRESSION ANALYSIS:", category_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Extract variables
  x <- data$Year
  y <- data$`Number of Designations`
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Get coefficients
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Equation
  cat("\n📈 REGRESSION EQUATION:\n")
  cat(sprintf("HPSA Designations = %.2f × Year + %.2f\n", slope, intercept))
  
  # R-squared
  cat("\n📊 R-SQUARED:\n")
  cat(sprintf("R² = %.4f (%.1f%% of variance explained)\n", 
              summary_model$r.squared, summary_model$r.squared * 100))
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat("\n🎯 P-VALUE:\n")
  if(p_value < 0.001) {
    cat(sprintf("p < 0.001 (%.2e) - HIGHLY SIGNIFICANT\n", p_value))
  } else if(p_value < 0.05) {
    cat(sprintf("p = %.4f - SIGNIFICANT at 95%% CI\n", p_value))
  } else {
    cat(sprintf("p = %.4f - NOT significant at 95%% CI\n", p_value))
  }
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  cat("\n🔍 95% CONFIDENCE INTERVALS:\n")
  cat(sprintf("Slope: [%.2f , %.2f]\n", ci[2,1], ci[2,2]))
  cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
  
  # Additional statistics
  cat("\n📋 ADDITIONAL STATISTICS:\n")
  cat(sprintf("Standard Error of Slope: %.2f\n", summary_model$coefficients[2,2]))
  cat(sprintf("t-statistic: %.2f\n", summary_model$coefficients[2,3]))
  cat(sprintf("Degrees of Freedom: %d\n", summary_model$df[2]))
  
  # Yearly change interpretation
  cat("\n💡 INTERPRETATION:\n")
  if(slope > 0) {
    cat(sprintf("Each year, HPSA designations INCREASE by approximately %.1f\n", slope))
  } else {
    cat(sprintf("Each year, HPSA designations DECREASE by approximately %.1f\n", abs(slope)))
  }
  
  # Return model object
  return(list(model = model, summary = summary_model, slope = slope, p_value = p_value, data = data))
}

# ------------------------------
# 6. RUN REGRESSIONS
# ------------------------------

rural_result <- run_regression_analysis(rural_clean, "RURAL")
nonrural_result <- run_regression_analysis(nonrural_clean, "NON-RURAL")
partially_result <- run_regression_analysis(partially_clean, "PARTIALLY RURAL")

# ------------------------------
# 7. COMBINED DATA FOR PLOTTING
# ------------------------------

# Combine only non-NULL datasets
all_categories <- bind_rows(
  if(!is.null(rural_clean)) rural_clean,
  if(!is.null(nonrural_clean)) nonrural_clean,
  if(!is.null(partially_clean)) partially_clean
)

if(nrow(all_categories) > 0) {
  # Plot using ggplot2
  p <- ggplot(all_categories, aes(x = Year, y = `Number of Designations`, 
                                  color = Category, group = Category)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95) +
    labs(title = "HPSA Designations Trends by Classification",
         x = "Year",
         y = "Number of HPSA Designations",
         caption = "Shaded areas represent 95% confidence intervals") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  print(p)
  ggsave("HPSA_regression_comparison.png", p, width = 10, height = 6, dpi = 300)
}

# ------------------------------
# 8. SUMMARY TABLE (only for successful regressions)
# ------------------------------

summary_table <- data.frame()

add_to_table <- function(result, name) {
  if(!is.null(result)) {
    return(data.frame(
      Category = name,
      Slope = round(result$slope, 2),
      Intercept = round(coef(result$model)[1], 2),
      R_Squared = round(result$summary$r.squared, 4),
      P_Value = format(result$p_value, scientific = TRUE, digits = 4),
      Significant = ifelse(result$p_value < 0.05, "YES", "NO"),
      Trend = ifelse(result$slope > 0, "Increasing", "Decreasing"),
      Observations = nrow(result$data)
    ))
  }
  return(NULL)
}

summary_table <- bind_rows(
  add_to_table(rural_result, "Rural"),
  add_to_table(nonrural_result, "Non-Rural"),
  add_to_table(partially_result, "Partially Rural")
)

if(nrow(summary_table) > 0) {
  cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("SUMMARY TABLE\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  print(summary_table)
  
  # Save summary
  write.csv(summary_table, "HPSA_regression_summary.csv", row.names = FALSE)
}

# ------------------------------
# 9. QUICK CHECK - PRINT ACTUAL NON-RURAL ROWS
# ------------------------------

cat("\n\n=== ACTUAL ROWS CONTAINING 'NON-RURAL' ===\n")
nonrural_check <- Reg2[grep("Non", Reg2$`Health Professional Shortage Areas: Rural/Non-Rural Classification`, ignore.case = TRUE), ]
print(nonrural_check[, c(1,2,3)])

# If you see the actual values, you can then use the exact string
# For example, if it shows " Non-Rural" with a space, use that



View(Reg2)
names(Reg2)
str(Reg2)


# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. AGGREGATE DATA BY YEAR AND MAIN CATEGORY
# ------------------------------

# Function to aggregate data for each main category
aggregate_category <- function(data, category_name) {
  data %>%
    filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == category_name) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed to Remove Designations`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_name)
}

# Aggregate for each main category
rural_data <- aggregate_category(Reg2, "Rural")
nonrural_data <- aggregate_category(Reg2, "Non-Rural")
partially_rural_data <- aggregate_category(Reg2, "Partially Rural")

# Also include "Partially Rural(11)" and combine with Partially Rural
partially_rural_data2 <- aggregate_category(Reg2, "Partially Rural(11)")

if(nrow(partially_rural_data2) > 0) {
  partially_rural_data <- bind_rows(partially_rural_data, partially_rural_data2) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed`, na.rm = TRUE),
      Category = "Partially Rural",
      .groups = 'drop'
    )
}

# ------------------------------
# 2. CONVERT POPULATION TO MILLIONS
# ------------------------------

convert_to_millions <- function(data) {
  data %>%
    mutate(
      `Population (Millions)` = `Population of Designated HPSAs` / 1e6
    )
}

rural_data <- convert_to_millions(rural_data)
nonrural_data <- convert_to_millions(nonrural_data)
partially_rural_data <- convert_to_millions(partially_rural_data)

# View aggregated data
cat("=== AGGREGATED DATA (Population in Millions) ===\n")
cat("\nRURAL DATA (by year):\n")
print(rural_data)

cat("\nNON-RURAL DATA (by year):\n")
print(nonrural_data)

cat("\nPARTIALLY RURAL DATA (by year):\n")
print(partially_rural_data)

# ------------------------------
# 3. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression_analysis <- function(data, category_name, dependent_var = "Number of Designations") {
  if(is.null(data) || nrow(data) < 3) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("REGRESSION ANALYSIS:", category_name, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("❌ ERROR: Insufficient data (need at least 3 observations)\n")
    return(NULL)
  }
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("REGRESSION ANALYSIS:", category_name, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  # Extract variables
  x <- data$Year
  y <- data[[dependent_var]]
  
  # Remove NA values
  valid_idx <- complete.cases(x, y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if(length(x) < 3) {
    cat("❌ Not enough valid data points\n")
    return(NULL)
  }
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Get coefficients
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Equation
  cat("\n📈 REGRESSION EQUATION:\n")
  if(dependent_var == "Population (Millions)") {
    cat(sprintf("Population (Millions) = %.4f × Year + %.2f\n", slope, intercept))
  } else {
    cat(sprintf("%s = %.2f × Year + %.2f\n", dependent_var, slope, intercept))
  }
  
  # R-squared
  cat("\n📊 R-SQUARED:\n")
  cat(sprintf("R² = %.4f (%.1f%% of variance explained)\n", 
              summary_model$r.squared, summary_model$r.squared * 100))
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat("\n🎯 P-VALUE:\n")
  if(p_value < 0.001) {
    cat(sprintf("p < 0.001 (%.2e) - HIGHLY SIGNIFICANT\n", p_value))
  } else if(p_value < 0.05) {
    cat(sprintf("p = %.4f - SIGNIFICANT at 95%% CI\n", p_value))
  } else {
    cat(sprintf("p = %.4f - NOT significant at 95%% CI\n", p_value))
  }
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  cat("\n🔍 95% CONFIDENCE INTERVALS:\n")
  cat(sprintf("Slope: [%.4f , %.4f]\n", ci[2,1], ci[2,2]))
  cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
  
  # Additional statistics
  cat("\n📋 ADDITIONAL STATISTICS:\n")
  cat(sprintf("Standard Error of Slope: %.4f\n", summary_model$coefficients[2,2]))
  cat(sprintf("t-statistic: %.2f\n", summary_model$coefficients[2,3]))
  cat(sprintf("Degrees of Freedom: %d\n", summary_model$df[2]))
  cat(sprintf("Adjusted R²: %.4f\n", summary_model$adj.r.squared))
  
  # Yearly change interpretation
  cat("\n💡 INTERPRETATION:\n")
  if(slope > 0) {
    if(dependent_var == "Population (Millions)") {
      cat(sprintf("Each year, population INCREASES by approximately %.2f million people\n", slope))
    } else if(dependent_var == "Number of Designations") {
      cat(sprintf("Each year, HPSA designations INCREASE by approximately %.1f\n", slope))
    } else {
      cat(sprintf("Each year, %s INCREASES by approximately %.1f\n", dependent_var, slope))
    }
  } else {
    if(dependent_var == "Population (Millions)") {
      cat(sprintf("Each year, population DECREASES by approximately %.2f million people\n", abs(slope)))
    } else if(dependent_var == "Number of Designations") {
      cat(sprintf("Each year, HPSA designations DECREASE by approximately %.1f\n", abs(slope)))
    } else {
      cat(sprintf("Each year, %s DECREASES by approximately %.1f\n", dependent_var, abs(slope)))
    }
  }
  
  # Return model object
  return(list(model = model, summary = summary_model, slope = slope, 
              p_value = p_value, data = data, ci = ci))
}

# ------------------------------
# 4. RUN REGRESSIONS - NUMBER OF DESIGNATIONS
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 1: NUMBER OF DESIGNATIONS                        ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_desig <- run_regression_analysis(rural_data, "RURAL", "Number of Designations")
nonrural_result_desig <- run_regression_analysis(nonrural_data, "NON-RURAL", "Number of Designations")
partially_result_desig <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Number of Designations")

# ------------------------------
# 5. RUN REGRESSIONS - POPULATION (IN MILLIONS)
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 2: POPULATION (IN MILLIONS)                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_pop <- run_regression_analysis(rural_data, "RURAL", "Population (Millions)")
nonrural_result_pop <- run_regression_analysis(nonrural_data, "NON-RURAL", "Population (Millions)")
partially_result_pop <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Population (Millions)")

# ------------------------------
# 6. RUN REGRESSIONS - PRACTITIONERS NEEDED
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 3: PRACTITIONERS NEEDED                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_prac <- run_regression_analysis(rural_data, "RURAL", "Practitioners Needed")
nonrural_result_prac <- run_regression_analysis(nonrural_data, "NON-RURAL", "Practitioners Needed")
partially_result_prac <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Practitioners Needed")

# ------------------------------
# 7. COMBINED DATA FOR PLOTTING
# ------------------------------

all_data_desig <- bind_rows(
  rural_data %>% select(Year, `Number of Designations`, Category),
  nonrural_data %>% select(Year, `Number of Designations`, Category),
  partially_rural_data %>% select(Year, `Number of Designations`, Category)
)

all_data_pop <- bind_rows(
  rural_data %>% select(Year, `Population (Millions)`, Category),
  nonrural_data %>% select(Year, `Population (Millions)`, Category),
  partially_rural_data %>% select(Year, `Population (Millions)`, Category)
)

all_data_prac <- bind_rows(
  rural_data %>% select(Year, `Practitioners Needed`, Category),
  nonrural_data %>% select(Year, `Practitioners Needed`, Category),
  partially_rural_data %>% select(Year, `Practitioners Needed`, Category)
)

# ------------------------------
# 8. SUMMARY TABLES
# ------------------------------

create_summary_table <- function(result_r, result_nr, result_pr, metric_name) {
  data.frame(
    Category = c("Rural", "Non-Rural", "Partially Rural"),
    Metric = metric_name,
    Slope = c(result_r$slope, result_nr$slope, result_pr$slope),
    Intercept = c(coef(result_r$model)[1], coef(result_nr$model)[1], coef(result_pr$model)[1]),
    R_Squared = c(result_r$summary$r.squared, result_nr$summary$r.squared, result_pr$summary$r.squared),
    P_Value = c(result_r$p_value, result_nr$p_value, result_pr$p_value),
    Significant_95 = c(result_r$p_value < 0.05, result_nr$p_value < 0.05, result_pr$p_value < 0.05),
    Trend = c(ifelse(result_r$slope > 0, "Increasing", "Decreasing"),
              ifelse(result_nr$slope > 0, "Increasing", "Decreasing"),
              ifelse(result_pr$slope > 0, "Increasing", "Decreasing")),
    CI_Lower = c(result_r$ci[2,1], result_nr$ci[2,1], result_pr$ci[2,1]),
    CI_Upper = c(result_r$ci[2,2], result_nr$ci[2,2], result_pr$ci[2,2])
  )
}

summary_designations <- create_summary_table(rural_result_desig, nonrural_result_desig, partially_result_desig, "Number of Designations")
summary_population <- create_summary_table(rural_result_pop, nonrural_result_pop, partially_result_pop, "Population (Millions)")
summary_practitioners <- create_summary_table(rural_result_prac, nonrural_result_prac, partially_result_prac, "Practitioners Needed")

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 9. VISUALIZATIONS
# ------------------------------

# Plot 1: Number of Designations
p1 <- ggplot(all_data_desig, aes(x = Year, y = `Number of Designations`, 
                                 color = Category, group = Category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
  labs(title = "HPSA Designations Trends (2018-2025)",
       x = "Year",
       y = "Number of HPSA Designations",
       caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "darkgreen", 
                                "Partially Rural" = "purple")) +
  scale_x_continuous(breaks = seq(2018, 2025, by = 1))

# Plot 2: Population (in Millions)
p2 <- ggplot(all_data_pop, aes(x = Year, y = `Population (Millions)`, 
                               color = Category, group = Category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
  labs(title = "Population of Designated HPSAs (in Millions)",
       x = "Year",
       y = "Population (Millions)",
       caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "darkgreen", 
                                "Partially Rural" = "purple")) +
  scale_x_continuous(breaks = seq(2018, 2025, by = 1))

# Plot 3: Practitioners Needed
p3 <- ggplot(all_data_prac, aes(x = Year, y = `Practitioners Needed`, 
                                color = Category, group = Category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
  labs(title = "Practitioners Needed to Remove Designations",
       x = "Year",
       y = "Number of Practitioners Needed",
       caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "darkgreen", 
                                "Partially Rural" = "purple")) +
  scale_x_continuous(breaks = seq(2018, 2025, by = 1))

# Display plots
print(p1)
print(p2)
print(p3)

# Save plots
ggsave("HPSA_Designations_Trend.png", p1, width = 10, height = 6, dpi = 300)
ggsave("HPSA_Population_Trend.png", p2, width = 10, height = 6, dpi = 300)
ggsave("HPSA_Practitioners_Trend.png", p3, width = 10, height = 6, dpi = 300)

# ------------------------------
# 10. EXPORT ALL RESULTS
# ------------------------------

# Save summary tables
write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

# Save aggregated data
write.csv(rural_data, "Rural_aggregated_data.csv", row.names = FALSE)
write.csv(nonrural_data, "NonRural_aggregated_data.csv", row.names = FALSE)
write.csv(partially_rural_data, "PartiallyRural_aggregated_data.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")
cat("  - Rural_aggregated_data.csv\n")
cat("  - NonRural_aggregated_data.csv\n")
cat("  - PartiallyRural_aggregated_data.csv\n")
cat("  - HPSA_Designations_Trend.png\n")
cat("  - HPSA_Population_Trend.png\n")
cat("  - HPSA_Practitioners_Trend.png\n")

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. AGGREGATE DATA BY YEAR AND MAIN CATEGORY
# ------------------------------

# Function to aggregate data for each main category
aggregate_category <- function(data, category_name) {
  data %>%
    filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == category_name) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed to Remove Designations`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_name)
}

# Aggregate for each main category
rural_data <- aggregate_category(Reg2, "Rural")
nonrural_data <- aggregate_category(Reg2, "Non-Rural")
partially_rural_data <- aggregate_category(Reg2, "Partially Rural")

# Also include "Partially Rural(11)" and combine with Partially Rural
partially_rural_data2 <- aggregate_category(Reg2, "Partially Rural(11)")

if(nrow(partially_rural_data2) > 0) {
  partially_rural_data <- bind_rows(partially_rural_data, partially_rural_data2) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed`, na.rm = TRUE),
      Category = "Partially Rural",
      .groups = 'drop'
    )
}

# ------------------------------
# 2. CONVERT POPULATION TO MILLIONS
# ------------------------------

convert_to_millions <- function(data) {
  data %>%
    mutate(
      `Population (Millions)` = `Population of Designated HPSAs` / 1e6
    )
}

rural_data <- convert_to_millions(rural_data)
nonrural_data <- convert_to_millions(nonrural_data)
partially_rural_data <- convert_to_millions(partially_rural_data)

# View aggregated data
cat("=== AGGREGATED DATA (Population in Millions) ===\n")
cat("\nRURAL DATA (by year):\n")
print(rural_data)

cat("\nNON-RURAL DATA (by year):\n")
print(nonrural_data)

cat("\nPARTIALLY RURAL DATA (by year):\n")
print(partially_rural_data)

# ------------------------------
# 3. FUNCTION FOR REGRESSION ANALYSIS (with better error handling)
# ------------------------------

run_regression_analysis <- function(data, category_name, dependent_var = "Number of Designations") {
  if(is.null(data) || nrow(data) < 3) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("REGRESSION ANALYSIS:", category_name, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("❌ ERROR: Insufficient data (need at least 3 observations)\n")
    cat(sprintf("   Only %d observations available\n", ifelse(is.null(data), 0, nrow(data))))
    return(NULL)
  }
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("REGRESSION ANALYSIS:", category_name, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  # Extract variables
  x <- data$Year
  y <- data[[dependent_var]]
  
  # Remove NA values
  valid_idx <- complete.cases(x, y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if(length(x) < 3) {
    cat("❌ Not enough valid data points\n")
    return(NULL)
  }
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Get coefficients
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Equation
  cat("\n📈 REGRESSION EQUATION:\n")
  if(dependent_var == "Population (Millions)") {
    cat(sprintf("Population (Millions) = %.4f × Year + %.2f\n", slope, intercept))
  } else {
    cat(sprintf("%s = %.2f × Year + %.2f\n", dependent_var, slope, intercept))
  }
  
  # R-squared
  cat("\n📊 R-SQUARED:\n")
  cat(sprintf("R² = %.4f (%.1f%% of variance explained)\n", 
              summary_model$r.squared, summary_model$r.squared * 100))
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat("\n🎯 P-VALUE:\n")
  if(p_value < 0.001) {
    cat(sprintf("p < 0.001 (%.2e) - HIGHLY SIGNIFICANT\n", p_value))
  } else if(p_value < 0.05) {
    cat(sprintf("p = %.4f - SIGNIFICANT at 95%% CI\n", p_value))
  } else {
    cat(sprintf("p = %.4f - NOT significant at 95%% CI\n", p_value))
  }
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  cat("\n🔍 95% CONFIDENCE INTERVALS:\n")
  cat(sprintf("Slope: [%.4f , %.4f]\n", ci[2,1], ci[2,2]))
  cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
  
  # Additional statistics
  cat("\n📋 ADDITIONAL STATISTICS:\n")
  cat(sprintf("Standard Error of Slope: %.4f\n", summary_model$coefficients[2,2]))
  cat(sprintf("t-statistic: %.2f\n", summary_model$coefficients[2,3]))
  cat(sprintf("Degrees of Freedom: %d\n", summary_model$df[2]))
  cat(sprintf("Adjusted R²: %.4f\n", summary_model$adj.r.squared))
  
  # Yearly change interpretation
  cat("\n💡 INTERPRETATION:\n")
  if(slope > 0) {
    if(dependent_var == "Population (Millions)") {
      cat(sprintf("Each year, population INCREASES by approximately %.2f million people\n", slope))
    } else if(dependent_var == "Number of Designations") {
      cat(sprintf("Each year, HPSA designations INCREASE by approximately %.1f\n", slope))
    } else {
      cat(sprintf("Each year, %s INCREASES by approximately %.1f\n", dependent_var, slope))
    }
  } else {
    if(dependent_var == "Population (Millions)") {
      cat(sprintf("Each year, population DECREASES by approximately %.2f million people\n", abs(slope)))
    } else if(dependent_var == "Number of Designations") {
      cat(sprintf("Each year, HPSA designations DECREASE by approximately %.1f\n", abs(slope)))
    } else {
      cat(sprintf("Each year, %s DECREASES by approximately %.1f\n", dependent_var, abs(slope)))
    }
  }
  
  # Return model object
  return(list(model = model, summary = summary_model, slope = slope, 
              p_value = p_value, data = data, ci = ci))
}

# ------------------------------
# 4. RUN REGRESSIONS - NUMBER OF DESIGNATIONS
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 1: NUMBER OF DESIGNATIONS                        ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_desig <- run_regression_analysis(rural_data, "RURAL", "Number of Designations")
nonrural_result_desig <- run_regression_analysis(nonrural_data, "NON-RURAL", "Number of Designations")
partially_result_desig <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Number of Designations")

# ------------------------------
# 5. RUN REGRESSIONS - POPULATION (IN MILLIONS)
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 2: POPULATION (IN MILLIONS)                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_pop <- run_regression_analysis(rural_data, "RURAL", "Population (Millions)")
nonrural_result_pop <- run_regression_analysis(nonrural_data, "NON-RURAL", "Population (Millions)")
partially_result_pop <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Population (Millions)")

# ------------------------------
# 6. RUN REGRESSIONS - PRACTITIONERS NEEDED
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 3: PRACTITIONERS NEEDED                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_prac <- run_regression_analysis(rural_data, "RURAL", "Practitioners Needed")
nonrural_result_prac <- run_regression_analysis(nonrural_data, "NON-RURAL", "Practitioners Needed")
partially_result_prac <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Practitioners Needed")

# ------------------------------
# 7. FUNCTION TO CREATE SUMMARY TABLE (WITH SAFE ERROR HANDLING)
# ------------------------------

create_summary_table_safe <- function(result_r, result_nr, result_pr, metric_name) {
  
  # Helper function to safely extract values
  safe_extract <- function(result, extract_func, default = NA) {
    if(is.null(result)) {
      return(default)
    } else {
      tryCatch(extract_func(result), error = function(e) default)
    }
  }
  
  # Extract values safely
  slope_r <- safe_extract(result_r, function(x) x$slope)
  slope_nr <- safe_extract(result_nr, function(x) x$slope)
  slope_pr <- safe_extract(result_pr, function(x) x$slope)
  
  intercept_r <- safe_extract(result_r, function(x) coef(x$model)[1])
  intercept_nr <- safe_extract(result_nr, function(x) coef(x$model)[1])
  intercept_pr <- safe_extract(result_pr, function(x) coef(x$model)[1])
  
  r2_r <- safe_extract(result_r, function(x) x$summary$r.squared)
  r2_nr <- safe_extract(result_nr, function(x) x$summary$r.squared)
  r2_pr <- safe_extract(result_pr, function(x) x$summary$r.squared)
  
  pval_r <- safe_extract(result_r, function(x) x$p_value)
  pval_nr <- safe_extract(result_nr, function(x) x$p_value)
  pval_pr <- safe_extract(result_pr, function(x) x$p_value)
  
  ci_lower_r <- safe_extract(result_r, function(x) x$ci[2,1])
  ci_lower_nr <- safe_extract(result_nr, function(x) x$ci[2,1])
  ci_lower_pr <- safe_extract(result_pr, function(x) x$ci[2,1])
  
  ci_upper_r <- safe_extract(result_r, function(x) x$ci[2,2])
  ci_upper_nr <- safe_extract(result_nr, function(x) x$ci[2,2])
  ci_upper_pr <- safe_extract(result_pr, function(x) x$ci[2,2])
  
  # Create data frame only for non-NULL results
  result_list <- list()
  
  if(!is.null(result_r)) {
    result_list[[1]] <- data.frame(
      Category = "Rural",
      Metric = metric_name,
      Slope = slope_r,
      Intercept = intercept_r,
      R_Squared = r2_r,
      P_Value = pval_r,
      Significant_95 = pval_r < 0.05,
      Trend = ifelse(slope_r > 0, "Increasing", "Decreasing"),
      CI_Lower = ci_lower_r,
      CI_Upper = ci_upper_r,
      Observations = nrow(result_r$data)
    )
  }
  
  if(!is.null(result_nr)) {
    result_list[[length(result_list) + 1]] <- data.frame(
      Category = "Non-Rural",
      Metric = metric_name,
      Slope = slope_nr,
      Intercept = intercept_nr,
      R_Squared = r2_nr,
      P_Value = pval_nr,
      Significant_95 = pval_nr < 0.05,
      Trend = ifelse(slope_nr > 0, "Increasing", "Decreasing"),
      CI_Lower = ci_lower_nr,
      CI_Upper = ci_upper_nr,
      Observations = nrow(result_nr$data)
    )
  }
  
  if(!is.null(result_pr)) {
    result_list[[length(result_list) + 1]] <- data.frame(
      Category = "Partially Rural",
      Metric = metric_name,
      Slope = slope_pr,
      Intercept = intercept_pr,
      R_Squared = r2_pr,
      P_Value = pval_pr,
      Significant_95 = pval_pr < 0.05,
      Trend = ifelse(slope_pr > 0, "Increasing", "Decreasing"),
      CI_Lower = ci_lower_pr,
      CI_Upper = ci_upper_pr,
      Observations = nrow(result_pr$data)
    )
  }
  
  # Combine all data frames
  if(length(result_list) > 0) {
    return(bind_rows(result_list))
  } else {
    return(NULL)
  }
}

# ------------------------------
# 8. CREATE SUMMARY TABLES
# ------------------------------

summary_designations <- create_summary_table_safe(rural_result_desig, nonrural_result_desig, partially_result_desig, "Number of Designations")
summary_population <- create_summary_table_safe(rural_result_pop, nonrural_result_pop, partially_result_pop, "Population (Millions)")
summary_practitioners <- create_summary_table_safe(rural_result_prac, nonrural_result_prac, partially_result_prac, "Practitioners Needed")

# ------------------------------
# 9. DISPLAY SUMMARY TABLES
# ------------------------------

if(!is.null(summary_designations)) {
  cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  print(summary_designations)
}

if(!is.null(summary_population)) {
  cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  print(summary_population)
}

if(!is.null(summary_practitioners)) {
  cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  print(summary_practitioners)
}

# ------------------------------
# 10. COMBINED DATA FOR PLOTTING (ONLY FOR CATEGORIES WITH DATA)
# ------------------------------

all_data_desig <- bind_rows(
  if(!is.null(rural_result_desig)) rural_data %>% select(Year, `Number of Designations`, Category),
  if(!is.null(nonrural_result_desig)) nonrural_data %>% select(Year, `Number of Designations`, Category),
  if(!is.null(partially_result_desig)) partially_rural_data %>% select(Year, `Number of Designations`, Category)
)

all_data_pop <- bind_rows(
  if(!is.null(rural_result_pop)) rural_data %>% select(Year, `Population (Millions)`, Category),
  if(!is.null(nonrural_result_pop)) nonrural_data %>% select(Year, `Population (Millions)`, Category),
  if(!is.null(partially_result_pop)) partially_rural_data %>% select(Year, `Population (Millions)`, Category)
)

all_data_prac <- bind_rows(
  if(!is.null(rural_result_prac)) rural_data %>% select(Year, `Practitioners Needed`, Category),
  if(!is.null(nonrural_result_prac)) nonrural_data %>% select(Year, `Practitioners Needed`, Category),
  if(!is.null(partially_result_prac)) partially_rural_data %>% select(Year, `Practitioners Needed`, Category)
)

# ------------------------------
# 11. VISUALIZATIONS (with safe plotting)
# ------------------------------

# Plot 1: Number of Designations
if(nrow(all_data_desig) > 0) {
  p1 <- ggplot(all_data_desig, aes(x = Year, y = `Number of Designations`, 
                                   color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
    labs(title = "HPSA Designations Trends (2018-2025)",
         x = "Year",
         y = "Number of HPSA Designations",
         caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    scale_color_manual(values = c("Rural" = "blue", 
                                  "Non-Rural" = "darkgreen", 
                                  "Partially Rural" = "purple")) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1))
  
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 10, height = 6, dpi = 300)
}

# Plot 2: Population (in Millions)
if(nrow(all_data_pop) > 0) {
  p2 <- ggplot(all_data_pop, aes(x = Year, y = `Population (Millions)`, 
                                 color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
    labs(title = "Population of Designated HPSAs (in Millions)",
         x = "Year",
         y = "Population (Millions)",
         caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    scale_color_manual(values = c("Rural" = "blue", 
                                  "Non-Rural" = "darkgreen", 
                                  "Partially Rural" = "purple")) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1))
  
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 10, height = 6, dpi = 300)
}

# Plot 3: Practitioners Needed
if(nrow(all_data_prac) > 0) {
  p3 <- ggplot(all_data_prac, aes(x = Year, y = `Practitioners Needed`, 
                                  color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
    labs(title = "Practitioners Needed to Remove Designations",
         x = "Year",
         y = "Number of Practitioners Needed",
         caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    scale_color_manual(values = c("Rural" = "blue", 
                                  "Non-Rural" = "darkgreen", 
                                  "Partially Rural" = "purple")) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1))
  
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 10, height = 6, dpi = 300)
}

# ------------------------------
# 12. EXPORT ALL RESULTS
# ------------------------------

# Save summary tables
if(!is.null(summary_designations)) {
  write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
}
if(!is.null(summary_population)) {
  write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
}
if(!is.null(summary_practitioners)) {
  write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)
}

# Save aggregated data
write.csv(rural_data, "Rural_aggregated_data.csv", row.names = FALSE)
write.csv(nonrural_data, "NonRural_aggregated_data.csv", row.names = FALSE)
if(nrow(partially_rural_data) > 0) {
  write.csv(partially_rural_data, "PartiallyRural_aggregated_data.csv", row.names = FALSE)
}

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
if(!is.null(summary_designations)) cat("  - HPSA_regression_designations.csv\n")
if(!is.null(summary_population)) cat("  - HPSA_regression_population.csv\n")
if(!is.null(summary_practitioners)) cat("  - HPSA_regression_practitioners.csv\n")
cat("  - Rural_aggregated_data.csv\n")
cat("  - NonRural_aggregated_data.csv\n")
if(nrow(partially_rural_data) > 0) cat("  - PartiallyRural_aggregated_data.csv\n")
cat("  - HPSA_Designations_Trend.png\n")
cat("  - HPSA_Population_Trend.png\n")
cat("  - HPSA_Practitioners_Trend.png\n")
# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. AGGREGATE DATA BY YEAR AND MAIN CATEGORY
# ------------------------------

# Function to aggregate data for each main category
aggregate_category <- function(data, category_name) {
  result <- data %>%
    filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == category_name) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed to Remove Designations`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_name)
  
  return(result)
}

# Aggregate for each main category
rural_data <- aggregate_category(Reg2, "Rural")
nonrural_data <- aggregate_category(Reg2, "Non-Rural")
partially_rural_data <- aggregate_category(Reg2, "Partially Rural")

# Also include "Partially Rural(11)" and combine with Partially Rural
partially_rural_data2 <- aggregate_category(Reg2, "Partially Rural(11)")

if(nrow(partially_rural_data2) > 0) {
  partially_rural_data <- bind_rows(partially_rural_data, partially_rural_data2) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed`, na.rm = TRUE),
      Category = "Partially Rural",
      .groups = 'drop'
    )
}

# ------------------------------
# 2. CHECK DATA AVAILABILITY
# ------------------------------

cat("=== DATA AVAILABILITY CHECK ===\n")
cat("\nRural data years:", paste(sort(unique(rural_data$Year)), collapse = ", "))
cat("\nNon-Rural data years:", paste(sort(unique(nonrural_data$Year)), collapse = ", "))
cat("\nPartially Rural data years:", paste(sort(unique(partially_rural_data$Year)), collapse = ", "))
cat("\n")

# ------------------------------
# 3. CONVERT POPULATION TO MILLIONS
# ------------------------------

convert_to_millions <- function(data) {
  if(nrow(data) > 0) {
    data <- data %>%
      mutate(
        `Population (Millions)` = `Population of Designated HPSAs` / 1e6
      )
  }
  return(data)
}

rural_data <- convert_to_millions(rural_data)
nonrural_data <- convert_to_millions(nonrural_data)
partially_rural_data <- convert_to_millions(partially_rural_data)

# View aggregated data
cat("\n=== AGGREGATED DATA (Population in Millions) ===\n")
cat("\nRURAL DATA (by year):\n")
print(rural_data)

cat("\nNON-RURAL DATA (by year):\n")
print(nonrural_data)

cat("\nPARTIALLY RURAL DATA (by year):\n")
print(partially_rural_data)

# ------------------------------
# 4. FUNCTION FOR REGRESSION ANALYSIS (with better error handling)
# ------------------------------

run_regression_analysis <- function(data, category_name, dependent_var = "Number of Designations") {
  if(is.null(data) || nrow(data) < 2) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("REGRESSION ANALYSIS:", category_name, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("❌ ERROR: Insufficient data (need at least 2 observations for regression)\n")
    cat(sprintf("   Only %d observations available\n", ifelse(is.null(data), 0, nrow(data))))
    return(NULL)
  }
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("REGRESSION ANALYSIS:", category_name, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  # Extract variables
  x <- data$Year
  y <- data[[dependent_var]]
  
  # Remove NA values
  valid_idx <- complete.cases(x, y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if(length(x) < 2) {
    cat("❌ Not enough valid data points\n")
    return(NULL)
  }
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Get coefficients
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # Equation
  cat("\n📈 REGRESSION EQUATION:\n")
  if(dependent_var == "Population (Millions)") {
    cat(sprintf("Population (Millions) = %.4f × Year + %.2f\n", slope, intercept))
  } else {
    cat(sprintf("%s = %.2f × Year + %.2f\n", dependent_var, slope, intercept))
  }
  
  # R-squared
  cat("\n📊 R-SQUARED:\n")
  cat(sprintf("R² = %.4f (%.1f%% of variance explained)\n", 
              summary_model$r.squared, summary_model$r.squared * 100))
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  cat("\n🎯 P-VALUE:\n")
  if(p_value < 0.001) {
    cat(sprintf("p < 0.001 (%.2e) - HIGHLY SIGNIFICANT\n", p_value))
  } else if(p_value < 0.05) {
    cat(sprintf("p = %.4f - SIGNIFICANT at 95%% CI\n", p_value))
  } else {
    cat(sprintf("p = %.4f - NOT significant at 95%% CI\n", p_value))
  }
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  cat("\n🔍 95% CONFIDENCE INTERVALS:\n")
  cat(sprintf("Slope: [%.4f , %.4f]\n", ci[2,1], ci[2,2]))
  cat(sprintf("Intercept: [%.2f , %.2f]\n", ci[1,1], ci[1,2]))
  
  # Additional statistics
  cat("\n📋 ADDITIONAL STATISTICS:\n")
  cat(sprintf("Standard Error of Slope: %.4f\n", summary_model$coefficients[2,2]))
  cat(sprintf("t-statistic: %.2f\n", summary_model$coefficients[2,3]))
  cat(sprintf("Degrees of Freedom: %d\n", summary_model$df[2]))
  cat(sprintf("Adjusted R²: %.4f\n", summary_model$adj.r.squared))
  
  # Yearly change interpretation
  cat("\n💡 INTERPRETATION:\n")
  if(slope > 0) {
    if(dependent_var == "Population (Millions)") {
      cat(sprintf("Each year, population INCREASES by approximately %.2f million people\n", slope))
    } else if(dependent_var == "Number of Designations") {
      cat(sprintf("Each year, HPSA designations INCREASE by approximately %.1f\n", slope))
    } else {
      cat(sprintf("Each year, %s INCREASES by approximately %.1f\n", dependent_var, slope))
    }
  } else {
    if(dependent_var == "Population (Millions)") {
      cat(sprintf("Each year, population DECREASES by approximately %.2f million people\n", abs(slope)))
    } else if(dependent_var == "Number of Designations") {
      cat(sprintf("Each year, HPSA designations DECREASE by approximately %.1f\n", abs(slope)))
    } else {
      cat(sprintf("Each year, %s DECREASES by approximately %.1f\n", dependent_var, abs(slope)))
    }
  }
  
  # Return model object
  return(list(model = model, summary = summary_model, slope = slope, 
              p_value = p_value, data = data, ci = ci))
}

# ------------------------------
# 5. RUN REGRESSIONS - NUMBER OF DESIGNATIONS
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 1: NUMBER OF DESIGNATIONS                        ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_desig <- run_regression_analysis(rural_data, "RURAL", "Number of Designations")
nonrural_result_desig <- run_regression_analysis(nonrural_data, "NON-RURAL", "Number of Designations")
partially_result_desig <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Number of Designations")

# ------------------------------
# 6. RUN REGRESSIONS - POPULATION (IN MILLIONS)
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 2: POPULATION (IN MILLIONS)                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_pop <- run_regression_analysis(rural_data, "RURAL", "Population (Millions)")
nonrural_result_pop <- run_regression_analysis(nonrural_data, "NON-RURAL", "Population (Millions)")
partially_result_pop <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Population (Millions)")

# ------------------------------
# 7. RUN REGRESSIONS - PRACTITIONERS NEEDED
# ------------------------------

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ANALYSIS 3: PRACTITIONERS NEEDED                          ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

rural_result_prac <- run_regression_analysis(rural_data, "RURAL", "Practitioners Needed")
nonrural_result_prac <- run_regression_analysis(nonrural_data, "NON-RURAL", "Practitioners Needed")
partially_result_prac <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Practitioners Needed")

# ------------------------------
# 8. FUNCTION TO CREATE SUMMARY TABLE (WITH SAFE ERROR HANDLING)
# ------------------------------

create_summary_table_safe <- function(result_r, result_nr, result_pr, metric_name) {
  
  # Helper function to safely extract values
  safe_extract <- function(result, extract_func, default = NA) {
    if(is.null(result)) {
      return(default)
    } else {
      tryCatch(extract_func(result), error = function(e) default)
    }
  }
  
  # Extract values safely
  slope_r <- safe_extract(result_r, function(x) x$slope)
  slope_nr <- safe_extract(result_nr, function(x) x$slope)
  slope_pr <- safe_extract(result_pr, function(x) x$slope)
  
  intercept_r <- safe_extract(result_r, function(x) coef(x$model)[1])
  intercept_nr <- safe_extract(result_nr, function(x) coef(x$model)[1])
  intercept_pr <- safe_extract(result_pr, function(x) coef(x$model)[1])
  
  r2_r <- safe_extract(result_r, function(x) x$summary$r.squared)
  r2_nr <- safe_extract(result_nr, function(x) x$summary$r.squared)
  r2_pr <- safe_extract(result_pr, function(x) x$summary$r.squared)
  
  pval_r <- safe_extract(result_r, function(x) x$p_value)
  pval_nr <- safe_extract(result_nr, function(x) x$p_value)
  pval_pr <- safe_extract(result_pr, function(x) x$p_value)
  
  ci_lower_r <- safe_extract(result_r, function(x) x$ci[2,1])
  ci_lower_nr <- safe_extract(result_nr, function(x) x$ci[2,1])
  ci_lower_pr <- safe_extract(result_pr, function(x) x$ci[2,1])
  
  ci_upper_r <- safe_extract(result_r, function(x) x$ci[2,2])
  ci_upper_nr <- safe_extract(result_nr, function(x) x$ci[2,2])
  ci_upper_pr <- safe_extract(result_pr, function(x) x$ci[2,2])
  
  obs_r <- safe_extract(result_r, function(x) nrow(x$data))
  obs_nr <- safe_extract(result_nr, function(x) nrow(x$data))
  obs_pr <- safe_extract(result_pr, function(x) nrow(x$data))
  
  # Create data frame for all categories (including NULL ones with NA)
  result_df <- data.frame(
    Category = c("Rural", "Non-Rural", "Partially Rural"),
    Metric = metric_name,
    Slope = c(slope_r, slope_nr, slope_pr),
    Intercept = c(intercept_r, intercept_nr, intercept_pr),
    R_Squared = c(r2_r, r2_nr, r2_pr),
    P_Value = c(pval_r, pval_nr, pval_pr),
    Significant_95 = c(ifelse(!is.na(pval_r) & pval_r < 0.05, "YES", "NO"),
                       ifelse(!is.na(pval_nr) & pval_nr < 0.05, "YES", "NO"),
                       ifelse(!is.na(pval_pr) & pval_pr < 0.05, "YES", "NO")),
    Trend = c(ifelse(!is.na(slope_r) & slope_r > 0, "Increasing", 
                     ifelse(!is.na(slope_r) & slope_r < 0, "Decreasing", "Insufficient Data")),
              ifelse(!is.na(slope_nr) & slope_nr > 0, "Increasing", 
                     ifelse(!is.na(slope_nr) & slope_nr < 0, "Decreasing", "Insufficient Data")),
              ifelse(!is.na(slope_pr) & slope_pr > 0, "Increasing", 
                     ifelse(!is.na(slope_pr) & slope_pr < 0, "Decreasing", "Insufficient Data"))),
    CI_Lower = c(ci_lower_r, ci_lower_nr, ci_lower_pr),
    CI_Upper = c(ci_upper_r, ci_upper_nr, ci_upper_pr),
    Observations = c(obs_r, obs_nr, obs_pr)
  )
  
  return(result_df)
}

# ------------------------------
# 9. CREATE SUMMARY TABLES
# ------------------------------

summary_designations <- create_summary_table_safe(rural_result_desig, nonrural_result_desig, partially_result_desig, "Number of Designations")
summary_population <- create_summary_table_safe(rural_result_pop, nonrural_result_pop, partially_result_pop, "Population (Millions)")
summary_practitioners <- create_summary_table_safe(rural_result_prac, nonrural_result_prac, partially_result_prac, "Practitioners Needed")

# ------------------------------
# 10. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 11. COMBINED DATA FOR PLOTTING (ALL THREE CATEGORIES)
# ------------------------------

# For Designations
all_data_desig <- bind_rows(
  rural_data %>% select(Year, `Number of Designations`, Category),
  nonrural_data %>% select(Year, `Number of Designations`, Category),
  partially_rural_data %>% select(Year, `Number of Designations`, Category)
)

# For Population
all_data_pop <- bind_rows(
  rural_data %>% select(Year, `Population (Millions)`, Category),
  nonrural_data %>% select(Year, `Population (Millions)`, Category),
  partially_rural_data %>% select(Year, `Population (Millions)`, Category)
)

# For Practitioners
all_data_prac <- bind_rows(
  rural_data %>% select(Year, `Practitioners Needed`, Category),
  nonrural_data %>% select(Year, `Practitioners Needed`, Category),
  partially_rural_data %>% select(Year, `Practitioners Needed`, Category)
)

# ------------------------------
# 12. VISUALIZATIONS
# ------------------------------

# Plot 1: Number of Designations
p1 <- ggplot(all_data_desig, aes(x = Year, y = `Number of Designations`, 
                                 color = Category, group = Category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
  labs(title = "HPSA Designations Trends (2018-2025)",
       x = "Year",
       y = "Number of HPSA Designations",
       caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "darkgreen", 
                                "Partially Rural" = "purple")) +
  scale_x_continuous(breaks = seq(2018, 2025, by = 1))

print(p1)
ggsave("HPSA_Designations_Trend.png", p1, width = 10, height = 6, dpi = 300)

# Plot 2: Population (in Millions)
p2 <- ggplot(all_data_pop, aes(x = Year, y = `Population (Millions)`, 
                               color = Category, group = Category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
  labs(title = "Population of Designated HPSAs (in Millions)",
       x = "Year",
       y = "Population (Millions)",
       caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "darkgreen", 
                                "Partially Rural" = "purple")) +
  scale_x_continuous(breaks = seq(2018, 2025, by = 1))

print(p2)
ggsave("HPSA_Population_Trend.png", p2, width = 10, height = 6, dpi = 300)

# Plot 3: Practitioners Needed
p3 <- ggplot(all_data_prac, aes(x = Year, y = `Practitioners Needed`, 
                                color = Category, group = Category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2) +
  labs(title = "Practitioners Needed to Remove Designations",
       x = "Year",
       y = "Number of Practitioners Needed",
       caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  scale_color_manual(values = c("Rural" = "blue", 
                                "Non-Rural" = "darkgreen", 
                                "Partially Rural" = "purple")) +
  scale_x_continuous(breaks = seq(2018, 2025, by = 1))

print(p3)
ggsave("HPSA_Practitioners_Trend.png", p3, width = 10, height = 6, dpi = 300)

# ------------------------------
# 13. EXPORT ALL RESULTS
# ------------------------------

# Save summary tables
write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

# Save aggregated data
write.csv(rural_data, "Rural_aggregated_data.csv", row.names = FALSE)
write.csv(nonrural_data, "NonRural_aggregated_data.csv", row.names = FALSE)
write.csv(partially_rural_data, "PartiallyRural_aggregated_data.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_regression_designations.csv (includes Rural, Non-Rural, Partially Rural)\n")
cat("  - HPSA_regression_population.csv (includes Rural, Non-Rural, Partially Rural)\n")
cat("  - HPSA_regression_practitioners.csv (includes Rural, Non-Rural, Partially Rural)\n")
cat("  - Rural_aggregated_data.csv\n")
cat("  - NonRural_aggregated_data.csv\n")
cat("  - PartiallyRural_aggregated_data.csv\n")
cat("  - HPSA_Designations_Trend.png\n")
cat("  - HPSA_Population_Trend.png\n")
cat("  - HPSA_Practitioners_Trend.png\n")




# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# ------------------------------
# 1. AGGREGATE DATA BY YEAR AND MAIN CATEGORY
# ------------------------------

# Function to aggregate data for each main category
aggregate_category <- function(data, category_name) {
  result <- data %>%
    filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == category_name) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed to Remove Designations`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_name)
  
  return(result)
}

# Aggregate for each main category
rural_data <- aggregate_category(Reg2, "Rural")
nonrural_data <- aggregate_category(Reg2, "Non-Rural")
partially_rural_data <- aggregate_category(Reg2, "Partially Rural")

# Also include "Partially Rural(11)" and combine with Partially Rural
partially_rural_data2 <- aggregate_category(Reg2, "Partially Rural(11)")

if(nrow(partially_rural_data2) > 0) {
  partially_rural_data <- bind_rows(partially_rural_data, partially_rural_data2) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed`, na.rm = TRUE),
      Category = "Partially Rural",
      .groups = 'drop'
    )
}

# ------------------------------
# 2. CONVERT POPULATION TO MILLIONS
# ------------------------------

convert_to_millions <- function(data) {
  if(nrow(data) > 0) {
    data <- data %>%
      mutate(
        `Population (Millions)` = `Population of Designated HPSAs` / 1e6
      )
  }
  return(data)
}

rural_data <- convert_to_millions(rural_data)
nonrural_data <- convert_to_millions(nonrural_data)
partially_rural_data <- convert_to_millions(partially_rural_data)

# ------------------------------
# 3. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression_analysis <- function(data, category_name, dependent_var = "Number of Designations") {
  if(is.null(data) || nrow(data) < 2) {
    return(NULL)
  }
  
  # Extract variables
  x <- data$Year
  y <- data[[dependent_var]]
  
  # Remove NA values
  valid_idx <- complete.cases(x, y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if(length(x) < 2) {
    return(NULL)
  }
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  # Get coefficients
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  
  # P-value
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  
  # 95% Confidence Intervals
  ci <- confint(model, level = 0.95)
  
  # Return model object with equation text
  if(dependent_var == "Population (Millions)") {
    equation_text <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(dependent_var == "Number of Designations") {
    equation_text <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation_text <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  return(list(model = model, summary = summary_model, slope = slope, 
              p_value = p_value, data = data, ci = ci,
              equation = equation_text,
              r2 = summary_model$r.squared,
              p_val_text = ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.4f", p_value))))
}

# ------------------------------
# 4. RUN REGRESSIONS FOR ALL CATEGORIES AND METRICS
# ------------------------------

# Number of Designations
rural_desig <- run_regression_analysis(rural_data, "RURAL", "Number of Designations")
nonrural_desig <- run_regression_analysis(nonrural_data, "NON-RURAL", "Number of Designations")
partially_desig <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Number of Designations")

# Population (Millions)
rural_pop <- run_regression_analysis(rural_data, "RURAL", "Population (Millions)")
nonrural_pop <- run_regression_analysis(nonrural_data, "NON-RURAL", "Population (Millions)")
partially_pop <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Population (Millions)")

# Practitioners Needed
rural_prac <- run_regression_analysis(rural_data, "RURAL", "Practitioners Needed")
nonrural_prac <- run_regression_analysis(nonrural_data, "NON-RURAL", "Practitioners Needed")
partially_prac <- run_regression_analysis(partially_rural_data, "PARTIALLY RURAL", "Practitioners Needed")

# ------------------------------
# 5. FUNCTION TO CREATE PLOT WITH ANNOTATIONS
# ------------------------------

create_plot <- function(data, result_r, result_nr, result_pr, y_var, y_label, title, y_scale_adjust = 1) {
  
  # Combine data
  plot_data <- bind_rows(
    if(!is.null(result_r)) data %>% select(Year, all_of(y_var), Category),
    if(!is.null(result_nr)) data %>% select(Year, all_of(y_var), Category),
    if(!is.null(result_pr)) data %>% select(Year, all_of(y_var), Category)
  )
  
  if(nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # Create color mapping
  colors <- c("Rural" = "blue", "Non-Rural" = "darkgreen", "Partially Rural" = "purple")
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Year, y = get(y_var), color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2, alpha = 0.2) +
    labs(title = title,
         x = "Year",
         y = y_label,
         caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.title = element_text(face = "bold")) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1))
  
  # Adjust y-axis scale based on data range
  y_values <- plot_data[[y_var]]
  y_range <- max(y_values, na.rm = TRUE) - min(y_values, na.rm = TRUE)
  y_min <- min(y_values, na.rm = TRUE) - y_range * 0.1
  y_max <- max(y_values, na.rm = TRUE) + y_range * 0.15
  
  p <- p + coord_cartesian(ylim = c(y_min, y_max))
  
  # Add annotations for each category
  annotation_y_pos <- y_max - (y_range * 0.02)
  step <- y_range * 0.05
  
  if(!is.null(result_r)) {
    annotation_text <- sprintf("%s: %s\nR² = %.3f | %s", 
                               "Rural", result_r$equation, result_r$r2, result_r$p_val_text)
    p <- p + annotate("text", x = 2018.5, y = annotation_y_pos, 
                      label = annotation_text, hjust = 0, size = 3.5, 
                      color = "blue", fontface = "bold")
    annotation_y_pos <- annotation_y_pos - step
  }
  
  if(!is.null(result_nr)) {
    annotation_text <- sprintf("%s: %s\nR² = %.3f | %s", 
                               "Non-Rural", result_nr$equation, result_nr$r2, result_nr$p_val_text)
    p <- p + annotate("text", x = 2018.5, y = annotation_y_pos, 
                      label = annotation_text, hjust = 0, size = 3.5, 
                      color = "darkgreen", fontface = "bold")
    annotation_y_pos <- annotation_y_pos - step
  }
  
  if(!is.null(result_pr)) {
    annotation_text <- sprintf("%s: %s\nR² = %.3f | %s", 
                               "Partially Rural", result_pr$equation, result_pr$r2, result_pr$p_val_text)
    p <- p + annotate("text", x = 2018.5, y = annotation_y_pos, 
                      label = annotation_text, hjust = 0, size = 3.5, 
                      color = "purple", fontface = "bold")
  }
  
  return(p)
}

# ------------------------------
# 6. CREATE ALL PLOTS
# ------------------------------

# Plot 1: Number of Designations
p1 <- create_plot(rural_data, rural_desig, nonrural_desig, partially_desig, 
                  "Number of Designations", 
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)")

# Plot 2: Population (in Millions)
p2 <- create_plot(rural_data, rural_pop, nonrural_pop, partially_pop, 
                  "Population (Millions)", 
                  "Population (Millions)",
                  "Population of Designated HPSAs Trends (2018-2025)")

# Plot 3: Practitioners Needed
p3 <- create_plot(rural_data, rural_prac, nonrural_prac, partially_prac, 
                  "Practitioners Needed", 
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations (2018-2025)")

# Display plots
print(p1)
print(p2)
print(p3)

# Save plots
ggsave("HPSA_Designations_Trend.png", p1, width = 12, height = 7, dpi = 300)
ggsave("HPSA_Population_Trend.png", p2, width = 12, height = 7, dpi = 300)
ggsave("HPSA_Practitioners_Trend.png", p3, width = 12, height = 7, dpi = 300)

# ------------------------------
# 7. CREATE SUMMARY TABLE
# ------------------------------

create_summary_table <- function(result_r, result_nr, result_pr, metric_name) {
  
  safe_value <- function(result, field, subfield = NULL, default = NA) {
    if(is.null(result)) return(default)
    if(!is.null(subfield)) {
      val <- tryCatch(result[[field]][[subfield]], error = function(e) default)
    } else {
      val <- tryCatch(result[[field]], error = function(e) default)
    }
    if(is.null(val) || is.na(val)) return(default)
    return(val)
  }
  
  data.frame(
    Category = c("Rural", "Non-Rural", "Partially Rural"),
    Metric = metric_name,
    Equation = c(safe_value(result_r, "equation"),
                 safe_value(result_nr, "equation"),
                 safe_value(result_pr, "equation")),
    Slope = c(safe_value(result_r, "slope"),
              safe_value(result_nr, "slope"),
              safe_value(result_pr, "slope")),
    R_Squared = c(safe_value(result_r, "r2"),
                  safe_value(result_nr, "r2"),
                  safe_value(result_pr, "r2")),
    P_Value = c(safe_value(result_r, "p_value"),
                safe_value(result_nr, "p_value"),
                safe_value(result_pr, "p_value")),
    Significant_95 = c(ifelse(!is.na(safe_value(result_r, "p_value")) & safe_value(result_r, "p_value") < 0.05, "YES", "NO"),
                       ifelse(!is.na(safe_value(result_nr, "p_value")) & safe_value(result_nr, "p_value") < 0.05, "YES", "NO"),
                       ifelse(!is.na(safe_value(result_pr, "p_value")) & safe_value(result_pr, "p_value") < 0.05, "YES", "NO")),
    CI_Lower = c(safe_value(result_r, "ci", 2, 1),
                 safe_value(result_nr, "ci", 2, 1),
                 safe_value(result_pr, "ci", 2, 1)),
    CI_Upper = c(safe_value(result_r, "ci", 2, 2),
                 safe_value(result_nr, "ci", 2, 2),
                 safe_value(result_pr, "ci", 2, 2)),
    Observations = c(safe_value(result_r, "data", nrow),
                     safe_value(result_nr, "data", nrow),
                     safe_value(result_pr, "data", nrow))
  )
}

# Create summary tables
summary_designations <- create_summary_table(rural_desig, nonrural_desig, partially_desig, "Number of Designations")
summary_population <- create_summary_table(rural_pop, nonrural_pop, partially_pop, "Population (Millions)")
summary_practitioners <- create_summary_table(rural_prac, nonrural_prac, partially_prac, "Practitioners Needed")

# ------------------------------
# 8. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 9. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

# Save aggregated data
write.csv(rural_data, "Rural_aggregated_data.csv", row.names = FALSE)
write.csv(nonrural_data, "NonRural_aggregated_data.csv", row.names = FALSE)
write.csv(partially_rural_data, "PartiallyRural_aggregated_data.csv", row.names = FALSE)

# ------------------------------
# 10. PRINT REGRESSION DETAILS TO CONSOLE
# ------------------------------

print_regression_details <- function(result, name, metric) {
  if(!is.null(result)) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("%s - %s\n", name, metric))
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("Equation: %s\n", result$equation))
    cat(sprintf("R² = %.4f\n", result$r2))
    cat(sprintf("P-value = %.6f %s\n", result$p_value, 
                ifelse(result$p_value < 0.05, "(SIGNIFICANT)", "(NOT significant)")))
    cat(sprintf("95%% CI for Slope: [%.4f , %.4f]\n", result$ci[2,1], result$ci[2,2]))
  }
}

cat("\n\n")
cat("╔═══════════════════════════════════════════════════════════════════════════════╗\n")
cat("║                          REGRESSION DETAILS                                  ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════════════╝\n")

print_regression_details(rural_desig, "RURAL", "Number of Designations")
print_regression_details(nonrural_desig, "NON-RURAL", "Number of Designations")
print_regression_details(partially_desig, "PARTIALLY RURAL", "Number of Designations")

print_regression_details(rural_pop, "RURAL", "Population (Millions)")
print_regression_details(nonrural_pop, "NON-RURAL", "Population (Millions)")
print_regression_details(partially_pop, "PARTIALLY RURAL", "Population (Millions)")

print_regression_details(rural_prac, "RURAL", "Practitioners Needed")
print_regression_details(nonrural_prac, "NON-RURAL", "Practitioners Needed")
print_regression_details(partially_prac, "PARTIALLY RURAL", "Practitioners Needed")

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Population_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Practitioners_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")


# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. CLEAN AND AGGREGATE DATA
# ------------------------------

# Remove rows with "Dental HPSA Totals" and clean up numeric columns
Reg2_clean <- Reg2 %>%
  filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` != "Dental HPSA Totals") %>%
  # Remove commas from numeric columns and convert to numeric
  mutate(
    `Number of Designations` = as.numeric(gsub(",", "", `Number of Designations`)),
    `Population of Designated HPSAs` = as.numeric(gsub(",", "", `Population of Designated HPSAs`)),
    `Practitioners Needed to Remove Designations` = as.numeric(gsub(",", "", `Practitioners Needed to Remove Designations`))
  )

# Function to aggregate data for each main category
aggregate_category <- function(data, category_name) {
  data %>%
    filter(`Health Professional Shortage Areas: Rural/Non-Rural Classification` == category_name) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed to Remove Designations`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_name)
}

# Aggregate for each main category
rural_data <- aggregate_category(Reg2_clean, "Rural")
nonrural_data <- aggregate_category(Reg2_clean, "Non-Rural")
partially_rural_data <- aggregate_category(Reg2_clean, "Partially Rural")

# Also include "Partially Rural(11)" and combine
partially_rural_data2 <- aggregate_category(Reg2_clean, "Partially Rural(11)")

if(nrow(partially_rural_data2) > 0) {
  partially_rural_data <- bind_rows(partially_rural_data, partially_rural_data2) %>%
    group_by(Year) %>%
    summarise(
      `Number of Designations` = sum(`Number of Designations`, na.rm = TRUE),
      `Population of Designated HPSAs` = sum(`Population of Designated HPSAs`, na.rm = TRUE),
      `Practitioners Needed` = sum(`Practitioners Needed`, na.rm = TRUE),
      Category = "Partially Rural",
      .groups = 'drop'
    )
}

# Convert Population to Millions
rural_data <- rural_data %>% mutate(`Population (Millions)` = `Population of Designated HPSAs` / 1e6)
nonrural_data <- nonrural_data %>% mutate(`Population (Millions)` = `Population of Designated HPSAs` / 1e6)
partially_rural_data <- partially_rural_data %>% mutate(`Population (Millions)` = `Population of Designated HPSAs` / 1e6)

# Remove any duplicate years by taking the first occurrence (or you can average)
rural_data <- rural_data %>% distinct(Year, .keep_all = TRUE)
nonrural_data <- nonrural_data %>% distinct(Year, .keep_all = TRUE)
partially_rural_data <- partially_rural_data %>% distinct(Year, .keep_all = TRUE)

# Sort by Year
rural_data <- rural_data %>% arrange(Year)
nonrural_data <- nonrural_data %>% arrange(Year)
partially_rural_data <- partially_rural_data %>% arrange(Year)

# View aggregated data
cat("=== AGGREGATED DATA ===\n")
cat("\nRURAL DATA:\n")
print(rural_data)
cat("\nNON-RURAL DATA:\n")
print(nonrural_data)
cat("\nPARTIALLY RURAL DATA:\n")
print(partially_rural_data)

# ------------------------------
# 2. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression <- function(data, category_name, y_var, y_label) {
  if(is.null(data) || nrow(data) < 2) {
    return(NULL)
  }
  
  x <- data$Year
  y <- data[[y_var]]
  
  # Remove NAs
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  
  if(length(x) < 2) return(NULL)
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r2 <- summary_model$r.squared
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  ci <- confint(model, level = 0.95)
  
  # Create equation text
  if(y_var == "Population (Millions)") {
    equation <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(y_var == "Number of Designations") {
    equation <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  p_text <- ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.4f", p_value))
  
  return(list(
    model = model, slope = slope, intercept = intercept, r2 = r2,
    p_value = p_value, ci = ci, equation = equation, p_text = p_text,
    data = data.frame(Year = x, y = y, Category = category_name)
  ))
}

# ------------------------------
# 3. RUN ALL REGRESSIONS
# ------------------------------

# Number of Designations
rural_desig <- run_regression(rural_data, "Rural", "Number of Designations", "Number of Designations")
nonrural_desig <- run_regression(nonrural_data, "Non-Rural", "Number of Designations", "Number of Designations")
partially_desig <- run_regression(partially_rural_data, "Partially Rural", "Number of Designations", "Number of Designations")

# Population (Millions)
rural_pop <- run_regression(rural_data, "Rural", "Population (Millions)", "Population (Millions)")
nonrural_pop <- run_regression(nonrural_data, "Non-Rural", "Population (Millions)", "Population (Millions)")
partially_pop <- run_regression(partially_rural_data, "Partially Rural", "Population (Millions)", "Population (Millions)")

# Practitioners Needed
rural_prac <- run_regression(rural_data, "Rural", "Practitioners Needed", "Practitioners Needed")
nonrural_prac <- run_regression(nonrural_data, "Non-Rural", "Practitioners Needed", "Practitioners Needed")
partially_prac <- run_regression(partially_rural_data, "Partially Rural", "Practitioners Needed", "Practitioners Needed")

# ------------------------------
# 4. FUNCTION TO CREATE PLOT WITH EQUATION, R2, AND P-VALUE
# ------------------------------

create_plot <- function(result_r, result_nr, result_pr, y_label, title, y_var_name) {
  
  # Collect all data points
  plot_data <- data.frame()
  
  if(!is.null(result_r)) {
    plot_data <- bind_rows(plot_data, result_r$data %>% mutate(Category = "Rural"))
  }
  if(!is.null(result_nr)) {
    plot_data <- bind_rows(plot_data, result_nr$data %>% mutate(Category = "Non-Rural"))
  }
  if(!is.null(result_pr)) {
    plot_data <- bind_rows(plot_data, result_pr$data %>% mutate(Category = "Partially Rural"))
  }
  
  if(nrow(plot_data) == 0) {
    cat("No data available for", title, "\n")
    return(NULL)
  }
  
  # Rename y column for consistent plotting
  colnames(plot_data)[2] <- "y_value"
  
  # Calculate y-axis limits with padding
  y_min <- min(plot_data$y_value, na.rm = TRUE)
  y_max <- max(plot_data$y_value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_min_padded <- y_min - y_range * 0.1
  y_max_padded <- y_max + y_range * 0.25  # Extra space for annotations
  
  # Define colors
  colors <- c("Rural" = "#1f78b4", "Non-Rural" = "#33a02c", "Partially Rural" = "#6a3d9a")
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = Year, y = y_value, color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2, alpha = 0.2) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1)) +
    coord_cartesian(ylim = c(y_min_padded, y_max_padded)) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add annotations for each category
  annotation_y <- y_max_padded - (y_range * 0.02)
  step <- y_range * 0.05
  
  if(!is.null(result_r)) {
    ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s", 
                        "Rural", result_r$equation, result_r$r2, result_r$p_text)
    p <- p + annotate("text", x = 2018.5, y = annotation_y, 
                      label = ann_text, hjust = 0, size = 3.8, 
                      color = colors["Rural"], fontface = "bold", lineheight = 1.2)
    annotation_y <- annotation_y - step
  }
  
  if(!is.null(result_nr)) {
    ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s", 
                        "Non-Rural", result_nr$equation, result_nr$r2, result_nr$p_text)
    p <- p + annotate("text", x = 2018.5, y = annotation_y, 
                      label = ann_text, hjust = 0, size = 3.8, 
                      color = colors["Non-Rural"], fontface = "bold", lineheight = 1.2)
    annotation_y <- annotation_y - step
  }
  
  if(!is.null(result_pr)) {
    ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s", 
                        "Partially Rural", result_pr$equation, result_pr$r2, result_pr$p_text)
    p <- p + annotate("text", x = 2018.5, y = annotation_y, 
                      label = ann_text, hjust = 0, size = 3.8, 
                      color = colors["Partially Rural"], fontface = "bold", lineheight = 1.2)
  }
  
  return(p)
}

# ------------------------------
# 5. CREATE ALL THREE PLOTS
# ------------------------------

p1 <- create_plot(rural_desig, nonrural_desig, partially_desig,
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)",
                  "Number of Designations")

p2 <- create_plot(rural_pop, nonrural_pop, partially_pop,
                  "Population (Millions)",
                  "Population of Designated HPSAs (in Millions)",
                  "Population (Millions)")

p3 <- create_plot(rural_prac, nonrural_prac, partially_prac,
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations",
                  "Practitioners Needed")

# Display plots
if(!is.null(p1)) {
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p2)) {
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p3)) {
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 13, height = 7.5, dpi = 300)
}

# ------------------------------
# 6. CREATE SUMMARY TABLE
# ------------------------------

create_summary <- function(result_r, result_nr, result_pr, metric) {
  data.frame(
    Category = c("Rural", "Non-Rural", "Partially Rural"),
    Metric = metric,
    Equation = c(
      ifelse(is.null(result_r), NA, result_r$equation),
      ifelse(is.null(result_nr), NA, result_nr$equation),
      ifelse(is.null(result_pr), NA, result_pr$equation)
    ),
    Slope = c(
      ifelse(is.null(result_r), NA, round(result_r$slope, 2)),
      ifelse(is.null(result_nr), NA, round(result_nr$slope, 2)),
      ifelse(is.null(result_pr), NA, round(result_pr$slope, 2))
    ),
    R_Squared = c(
      ifelse(is.null(result_r), NA, round(result_r$r2, 4)),
      ifelse(is.null(result_nr), NA, round(result_nr$r2, 4)),
      ifelse(is.null(result_pr), NA, round(result_pr$r2, 4))
    ),
    P_Value = c(
      ifelse(is.null(result_r), NA, format(result_r$p_value, scientific = TRUE, digits = 4)),
      ifelse(is.null(result_nr), NA, format(result_nr$p_value, scientific = TRUE, digits = 4)),
      ifelse(is.null(result_pr), NA, format(result_pr$p_value, scientific = TRUE, digits = 4))
    ),
    Significant_95 = c(
      ifelse(is.null(result_r), NA, ifelse(result_r$p_value < 0.05, "YES", "NO")),
      ifelse(is.null(result_nr), NA, ifelse(result_nr$p_value < 0.05, "YES", "NO")),
      ifelse(is.null(result_pr), NA, ifelse(result_pr$p_value < 0.05, "YES", "NO"))
    ),
    CI_Lower = c(
      ifelse(is.null(result_r), NA, round(result_r$ci[2,1], 2)),
      ifelse(is.null(result_nr), NA, round(result_nr$ci[2,1], 2)),
      ifelse(is.null(result_pr), NA, round(result_pr$ci[2,1], 2))
    ),
    CI_Upper = c(
      ifelse(is.null(result_r), NA, round(result_r$ci[2,2], 2)),
      ifelse(is.null(result_nr), NA, round(result_nr$ci[2,2], 2)),
      ifelse(is.null(result_pr), NA, round(result_pr$ci[2,2], 2))
    ),
    Observations = c(
      ifelse(is.null(result_r), NA, nrow(result_r$data)),
      ifelse(is.null(result_nr), NA, nrow(result_nr$data)),
      ifelse(is.null(result_pr), NA, nrow(result_pr$data))
    )
  )
}

# Create all summary tables
summary_designations <- create_summary(rural_desig, nonrural_desig, partially_desig, "Number of Designations")
summary_population <- create_summary(rural_pop, nonrural_pop, partially_pop, "Population (Millions)")
summary_practitioners <- create_summary(rural_prac, nonrural_prac, partially_prac, "Practitioners Needed")

# ------------------------------
# 7. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 8. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

# Save aggregated data
write.csv(rural_data, "Rural_aggregated_data.csv", row.names = FALSE)
write.csv(nonrural_data, "NonRural_aggregated_data.csv", row.names = FALSE)
write.csv(partially_rural_data, "PartiallyRural_aggregated_data.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Population_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Practitioners_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")
cat("  - Rural_aggregated_data.csv\n")
cat("  - NonRural_aggregated_data.csv\n")
cat("  - PartiallyRural_aggregated_data.csv\n")

names(Reg2)

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. CHECK AND RENAME COLUMNS
# ------------------------------

# Display actual column names
cat("Actual column names:\n")
print(names(Reg2))

# Rename columns for easier access
Reg2_clean <- Reg2 %>%
  rename(
    Year = 1,
    Classification = 2,
    Designations = 3,
    Practitioners_Needed_1 = 4,
    Population = 5,
    Practitioners_Needed_2 = 6
  ) %>%
  # Use the first Practitioners column (remove commas)
  mutate(
    Designations = as.numeric(gsub(",", "", Designations)),
    Population = as.numeric(gsub(",", "", Population)),
    Practitioners_Needed = as.numeric(gsub(",", "", Practitioners_Needed_1))
  ) %>%
  select(Year, Classification, Designations, Population, Practitioners_Needed)

# Remove rows with "Dental HPSA Totals"
Reg2_clean <- Reg2_clean %>%
  filter(Classification != "Dental HPSA Totals")

# View first few rows
cat("\nFirst 10 rows of cleaned data:\n")
print(head(Reg2_clean, 10))

# ------------------------------
# 2. AGGREGATE DATA BY YEAR AND MAIN CATEGORY
# ------------------------------

# Function to aggregate data for each main category
aggregate_category <- function(data, category_name) {
  data %>%
    filter(Classification == category_name) %>%
    group_by(Year) %>%
    summarise(
      Designations = sum(Designations, na.rm = TRUE),
      Population = sum(Population, na.rm = TRUE),
      Practitioners_Needed = sum(Practitioners_Needed, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(Category = category_name)
}

# Aggregate for each main category
rural_data <- aggregate_category(Reg2_clean, "Rural")
nonrural_data <- aggregate_category(Reg2_clean, "Non-Rural")
partially_rural_data <- aggregate_category(Reg2_clean, "Partially Rural")

# Also include "Partially Rural(11)" and combine
partially_rural_data2 <- aggregate_category(Reg2_clean, "Partially Rural(11)")

if(nrow(partially_rural_data2) > 0) {
  partially_rural_data <- bind_rows(partially_rural_data, partially_rural_data2) %>%
    group_by(Year) %>%
    summarise(
      Designations = sum(Designations, na.rm = TRUE),
      Population = sum(Population, na.rm = TRUE),
      Practitioners_Needed = sum(Practitioners_Needed, na.rm = TRUE),
      Category = "Partially Rural",
      .groups = 'drop'
    )
}

# Convert Population to Millions
rural_data <- rural_data %>% mutate(Population_Millions = Population / 1e6)
nonrural_data <- nonrural_data %>% mutate(Population_Millions = Population / 1e6)
partially_rural_data <- partially_rural_data %>% mutate(Population_Millions = Population / 1e6)

# Remove duplicate years (keep first occurrence)
rural_data <- rural_data %>% distinct(Year, .keep_all = TRUE) %>% arrange(Year)
nonrural_data <- nonrural_data %>% distinct(Year, .keep_all = TRUE) %>% arrange(Year)
partially_rural_data <- partially_rural_data %>% distinct(Year, .keep_all = TRUE) %>% arrange(Year)

# View aggregated data
cat("\n=== AGGREGATED DATA ===\n")
cat("\nRURAL DATA:\n")
print(rural_data)
cat("\nNON-RURAL DATA:\n")
print(nonrural_data)
cat("\nPARTIALLY RURAL DATA:\n")
print(partially_rural_data)

# ------------------------------
# 3. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression <- function(data, category_name, y_var, y_label) {
  if(is.null(data) || nrow(data) < 2) {
    return(NULL)
  }
  
  x <- data$Year
  y <- data[[y_var]]
  
  # Remove NAs
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  
  if(length(x) < 2) return(NULL)
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r2 <- summary_model$r.squared
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  ci <- confint(model, level = 0.95)
  
  # Create equation text
  if(y_var == "Population_Millions") {
    equation <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(y_var == "Designations") {
    equation <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  p_text <- ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.4f", p_value))
  
  return(list(
    model = model, slope = slope, intercept = intercept, r2 = r2,
    p_value = p_value, ci = ci, equation = equation, p_text = p_text,
    data = data.frame(Year = x, y = y, Category = category_name)
  ))
}

# ------------------------------
# 4. RUN ALL REGRESSIONS
# ------------------------------

# Number of Designations
rural_desig <- run_regression(rural_data, "Rural", "Designations", "Designations")
nonrural_desig <- run_regression(nonrural_data, "Non-Rural", "Designations", "Designations")
partially_desig <- run_regression(partially_rural_data, "Partially Rural", "Designations", "Designations")

# Population (Millions)
rural_pop <- run_regression(rural_data, "Rural", "Population_Millions", "Population (Millions)")
nonrural_pop <- run_regression(nonrural_data, "Non-Rural", "Population_Millions", "Population (Millions)")
partially_pop <- run_regression(partially_rural_data, "Partially Rural", "Population_Millions", "Population (Millions)")

# Practitioners Needed
rural_prac <- run_regression(rural_data, "Rural", "Practitioners_Needed", "Practitioners Needed")
nonrural_prac <- run_regression(nonrural_data, "Non-Rural", "Practitioners_Needed", "Practitioners Needed")
partially_prac <- run_regression(partially_rural_data, "Partially Rural", "Practitioners_Needed", "Practitioners Needed")

# ------------------------------
# 5. FUNCTION TO CREATE PLOT WITH EQUATION, R2, AND P-VALUE
# ------------------------------

create_plot <- function(result_r, result_nr, result_pr, y_label, title, y_var_name) {
  
  # Collect all data points
  plot_data <- data.frame()
  
  if(!is.null(result_r)) {
    plot_data <- bind_rows(plot_data, result_r$data %>% mutate(Category = "Rural"))
  }
  if(!is.null(result_nr)) {
    plot_data <- bind_rows(plot_data, result_nr$data %>% mutate(Category = "Non-Rural"))
  }
  if(!is.null(result_pr)) {
    plot_data <- bind_rows(plot_data, result_pr$data %>% mutate(Category = "Partially Rural"))
  }
  
  if(nrow(plot_data) == 0) {
    cat("No data available for", title, "\n")
    return(NULL)
  }
  
  # Rename y column for consistent plotting
  colnames(plot_data)[2] <- "y_value"
  
  # Calculate y-axis limits with padding
  y_min <- min(plot_data$y_value, na.rm = TRUE)
  y_max <- max(plot_data$y_value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_min_padded <- y_min - y_range * 0.1
  y_max_padded <- y_max + y_range * 0.28  # Extra space for annotations
  
  # Define colors
  colors <- c("Rural" = "#1f78b4", "Non-Rural" = "#33a02c", "Partially Rural" = "#6a3d9a")
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = Year, y = y_value, color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2, alpha = 0.2) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1)) +
    coord_cartesian(ylim = c(y_min_padded, y_max_padded)) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add annotations for each category
  annotation_y <- y_max_padded - (y_range * 0.02)
  step <- y_range * 0.055
  
  if(!is.null(result_r)) {
    ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s", 
                        "Rural", result_r$equation, result_r$r2, result_r$p_text)
    p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                      label = ann_text, hjust = 0, size = 3.8, 
                      color = colors["Rural"], fontface = "bold", lineheight = 1.2)
    annotation_y <- annotation_y - step
  }
  
  if(!is.null(result_nr)) {
    ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s", 
                        "Non-Rural", result_nr$equation, result_nr$r2, result_nr$p_text)
    p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                      label = ann_text, hjust = 0, size = 3.8, 
                      color = colors["Non-Rural"], fontface = "bold", lineheight = 1.2)
    annotation_y <- annotation_y - step
  }
  
  if(!is.null(result_pr)) {
    ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s", 
                        "Partially Rural", result_pr$equation, result_pr$r2, result_pr$p_text)
    p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                      label = ann_text, hjust = 0, size = 3.8, 
                      color = colors["Partially Rural"], fontface = "bold", lineheight = 1.2)
  }
  
  return(p)
}

# ------------------------------
# 6. CREATE ALL THREE PLOTS
# ------------------------------

p1 <- create_plot(rural_desig, nonrural_desig, partially_desig,
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)",
                  "Designations")

p2 <- create_plot(rural_pop, nonrural_pop, partially_pop,
                  "Population (Millions)",
                  "Population of Designated HPSAs (in Millions)",
                  "Population_Millions")

p3 <- create_plot(rural_prac, nonrural_prac, partially_prac,
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations",
                  "Practitioners_Needed")

# Display plots
if(!is.null(p1)) {
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p2)) {
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p3)) {
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 13, height = 7.5, dpi = 300)
}

# ------------------------------
# 7. CREATE SUMMARY TABLE
# ------------------------------

create_summary <- function(result_r, result_nr, result_pr, metric) {
  data.frame(
    Category = c("Rural", "Non-Rural", "Partially Rural"),
    Metric = metric,
    Equation = c(
      ifelse(is.null(result_r), NA, result_r$equation),
      ifelse(is.null(result_nr), NA, result_nr$equation),
      ifelse(is.null(result_pr), NA, result_pr$equation)
    ),
    Slope = c(
      ifelse(is.null(result_r), NA, round(result_r$slope, 2)),
      ifelse(is.null(result_nr), NA, round(result_nr$slope, 2)),
      ifelse(is.null(result_pr), NA, round(result_pr$slope, 2))
    ),
    R_Squared = c(
      ifelse(is.null(result_r), NA, round(result_r$r2, 4)),
      ifelse(is.null(result_nr), NA, round(result_nr$r2, 4)),
      ifelse(is.null(result_pr), NA, round(result_pr$r2, 4))
    ),
    P_Value = c(
      ifelse(is.null(result_r), NA, format(result_r$p_value, scientific = TRUE, digits = 4)),
      ifelse(is.null(result_nr), NA, format(result_nr$p_value, scientific = TRUE, digits = 4)),
      ifelse(is.null(result_pr), NA, format(result_pr$p_value, scientific = TRUE, digits = 4))
    ),
    Significant_95 = c(
      ifelse(is.null(result_r), NA, ifelse(result_r$p_value < 0.05, "YES", "NO")),
      ifelse(is.null(result_nr), NA, ifelse(result_nr$p_value < 0.05, "YES", "NO")),
      ifelse(is.null(result_pr), NA, ifelse(result_pr$p_value < 0.05, "YES", "NO"))
    ),
    CI_Lower = c(
      ifelse(is.null(result_r), NA, round(result_r$ci[2,1], 2)),
      ifelse(is.null(result_nr), NA, round(result_nr$ci[2,1], 2)),
      ifelse(is.null(result_pr), NA, round(result_pr$ci[2,1], 2))
    ),
    CI_Upper = c(
      ifelse(is.null(result_r), NA, round(result_r$ci[2,2], 2)),
      ifelse(is.null(result_nr), NA, round(result_nr$ci[2,2], 2)),
      ifelse(is.null(result_pr), NA, round(result_pr$ci[2,2], 2))
    ),
    Observations = c(
      ifelse(is.null(result_r), NA, nrow(result_r$data)),
      ifelse(is.null(result_nr), NA, nrow(result_nr$data)),
      ifelse(is.null(result_pr), NA, nrow(result_pr$data))
    )
  )
}

# Create all summary tables
summary_designations <- create_summary(rural_desig, nonrural_desig, partially_desig, "Number of Designations")
summary_population <- create_summary(rural_pop, nonrural_pop, partially_pop, "Population (Millions)")
summary_practitioners <- create_summary(rural_prac, nonrural_prac, partially_prac, "Practitioners Needed")

# ------------------------------
# 8. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 9. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

# Save aggregated data
write.csv(rural_data, "Rural_aggregated_data.csv", row.names = FALSE)
write.csv(nonrural_data, "NonRural_aggregated_data.csv", row.names = FALSE)
write.csv(partially_rural_data, "PartiallyRural_aggregated_data.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Population_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Practitioners_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")
cat("  - Rural_aggregated_data.csv\n")
cat("  - NonRural_aggregated_data.csv\n")
cat("  - PartiallyRural_aggregated_data.csv\n")





# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. CREATE DATA FRAME
# ------------------------------

data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  Category = c(rep("Facility_NonR", 8),
               rep("Non-Rural", 8),
               rep("Partially Rural", 8),
               rep("Rural", 8)),
  Designations = c(1459, 1452, 1493, 1519, 1505, 1505, 1505, 1534,
                   2028, 1976, 2037, 2082, 2071, 2103, 2036, 2023,
                   361, 332, 353, 371, 306, 298, 255, 294,
                   3451, 3659, 4085, 4146, 4608, 4931, 4705, 4742),
  Population_Millions = c(0.87, 0.65, 0.52, 0.55, 0.54, 0.54, 0.51, 0.56,
                          28.42, 25.62, 27.68, 29.16, 31.13, 33.24, 28.40, 25.96,
                          12.90, 11.56, 12.30, 12.97, 13.31, 13.42, 11.03, 11.84,
                          20.11, 18.62, 18.82, 19.11, 22.42, 24.27, 20.70, 19.36),
  Practitioners = c(439, 320, 250, 270, 263, 242, 229, 263,
                    5044, 4686, 5063, 5318, 5562, 5809, 4828, 4452,
                    2176, 2038, 2180, 2283, 2203, 2209, 1777, 1860,
                    3496, 3283, 3252, 3293, 3793, 4063, 3527, 3328)
)

# View data structure
cat("=== DATA SUMMARY ===\n")
print(head(data, 10))
cat("\nUnique categories:", paste(unique(data$Category), collapse = ", "))

# ------------------------------
# 2. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression <- function(data, category_name, y_var, y_label) {
  # Filter data for the category
  category_data <- data %>% filter(Category == category_name)
  
  if(nrow(category_data) < 2) {
    return(NULL)
  }
  
  x <- category_data$Year
  y <- category_data[[y_var]]
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r2 <- summary_model$r.squared
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  ci <- confint(model, level = 0.95)
  
  # Create equation text
  if(y_var == "Population_Millions") {
    equation <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(y_var == "Designations") {
    equation <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  p_text <- ifelse(p_value < 0.001, "p < 0.001", sprintf("p = %.4f", p_value))
  
  # Significance stars
  sig_stars <- ifelse(p_value < 0.001, "***", 
                      ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "ns")))
  
  return(list(
    model = model, slope = slope, intercept = intercept, r2 = r2,
    p_value = p_value, ci = ci, equation = equation, p_text = p_text,
    sig_stars = sig_stars,
    data = data.frame(Year = x, y = y, Category = category_name)
  ))
}

# ------------------------------
# 3. RUN REGRESSIONS FOR ALL CATEGORIES (4 categories now)
# ------------------------------

categories <- c("Rural", "Non-Rural", "Partially Rural", "Facility_NonR")

# Store results in lists
desig_results <- list()
pop_results <- list()
prac_results <- list()

for(cat in categories) {
  desig_results[[cat]] <- run_regression(data, cat, "Designations", "Designations")
  pop_results[[cat]] <- run_regression(data, cat, "Population_Millions", "Population (Millions)")
  prac_results[[cat]] <- run_regression(data, cat, "Practitioners", "Practitioners Needed")
}

# ------------------------------
# 4. FUNCTION TO CREATE PLOT WITH EQUATION, R2, AND P-VALUE
# ------------------------------

create_plot <- function(results_list, y_var, y_label, title, categories_to_plot = categories) {
  
  # Collect all data points
  plot_data <- data.frame()
  
  for(cat in categories_to_plot) {
    if(!is.null(results_list[[cat]])) {
      plot_data <- bind_rows(plot_data, results_list[[cat]]$data %>% mutate(Category = cat))
    }
  }
  
  if(nrow(plot_data) == 0) {
    cat("No data available for", title, "\n")
    return(NULL)
  }
  
  # Rename y column for consistent plotting
  colnames(plot_data)[2] <- "y_value"
  
  # Calculate y-axis limits with padding
  y_min <- min(plot_data$y_value, na.rm = TRUE)
  y_max <- max(plot_data$y_value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_min_padded <- y_min - y_range * 0.1
  y_max_padded <- y_max + y_range * 0.32  # Extra space for annotations
  
  # Define colors for 4 categories
  colors <- c("Rural" = "#1f78b4", 
              "Non-Rural" = "#33a02c", 
              "Partially Rural" = "#6a3d9a",
              "Facility_NonR" = "#e31a23")
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = Year, y = y_value, color = Category, group = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(size = 1, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, size = 1.2, alpha = 0.2) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1)) +
    coord_cartesian(ylim = c(y_min_padded, y_max_padded)) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      caption = "Points: Actual values | Lines: Regression trends | Shaded: 95% CI\nSignificance: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add annotations for each category
  annotation_y <- y_max_padded - (y_range * 0.02)
  step <- y_range * 0.055
  
  for(cat in categories_to_plot) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s %s", 
                          cat, res$equation, res$r2, res$p_text, res$sig_stars)
      p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                        label = ann_text, hjust = 0, size = 3.5, 
                        color = colors[cat], fontface = "bold", lineheight = 1.2)
      annotation_y <- annotation_y - step
    }
  }
  
  return(p)
}

# ------------------------------
# 5. CREATE ALL THREE PLOTS
# ------------------------------

# Plot 1: Number of Designations
p1 <- create_plot(desig_results, "Designations", 
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)")

# Plot 2: Population (in Millions)
p2 <- create_plot(pop_results, "Population_Millions", 
                  "Population (Millions)",
                  "Population of Designated HPSAs (in Millions)")

# Plot 3: Practitioners Needed
p3 <- create_plot(prac_results, "Practitioners", 
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations")

# Display plots
if(!is.null(p1)) {
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 14, height = 8, dpi = 300)
}
if(!is.null(p2)) {
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 14, height = 8, dpi = 300)
}
if(!is.null(p3)) {
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 14, height = 8, dpi = 300)
}

# ------------------------------
# 6. CREATE SUMMARY TABLE
# ------------------------------

create_summary <- function(results_list, metric, categories_list = categories) {
  df <- data.frame()
  for(cat in categories_list) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      df <- bind_rows(df, data.frame(
        Category = cat,
        Metric = metric,
        Equation = res$equation,
        Slope = round(res$slope, 2),
        Intercept = round(res$intercept, 0),
        R_Squared = round(res$r2, 4),
        P_Value = format(res$p_value, scientific = TRUE, digits = 4),
        Significant = ifelse(res$p_value < 0.05, "YES", "NO"),
        Significance_Stars = res$sig_stars,
        CI_Lower = round(res$ci[2,1], 2),
        CI_Upper = round(res$ci[2,2], 2),
        Observations = nrow(res$data)
      ))
    }
  }
  return(df)
}

# Create all summary tables
summary_designations <- create_summary(desig_results, "Number of Designations")
summary_population <- create_summary(pop_results, "Population (Millions)")
summary_practitioners <- create_summary(prac_results, "Practitioners Needed")

# ------------------------------
# 7. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 8. PRINT DETAILED REGRESSION RESULTS TO CONSOLE
# ------------------------------

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("DETAILED REGRESSION RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

for(cat in categories) {
  if(!is.null(desig_results[[cat]])) {
    res <- desig_results[[cat]]
    cat("\n", paste(rep("-", 50), collapse = ""), "\n")
    cat(sprintf("CATEGORY: %s - DESIGNATIONS\n", cat))
    cat(paste(rep("-", 50), collapse = ""), "\n")
    cat(sprintf("Equation: %s\n", res$equation))
    cat(sprintf("R² = %.4f\n", res$r2))
    cat(sprintf("P-value = %.6f %s\n", res$p_value, res$sig_stars))
    cat(sprintf("95%% CI for Slope: [%.2f , %.2f]\n", res$ci[2,1], res$ci[2,2]))
  }
}

# ------------------------------
# 9. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

# Save raw data
write.csv(data, "HPSA_clean_data.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Population_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_Practitioners_Trend.png (with equation, R², and p-value on plot)\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")
cat("  - HPSA_clean_data.csv\n")




# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------
# 1. CREATE DATA FRAME
# ------------------------------

data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  Category = c(rep("Facility_NonR", 8),
               rep("Non-Rural", 8),
               rep("Partially Rural", 8),
               rep("Rural", 8)),
  Designations = c(1459, 1452, 1493, 1519, 1505, 1505, 1505, 1534,
                   2028, 1976, 2037, 2082, 2071, 2103, 2036, 2023,
                   361, 332, 353, 371, 306, 298, 255, 294,
                   3451, 3659, 4085, 4146, 4608, 4931, 4705, 4742),
  Population_Millions = c(0.87, 0.65, 0.52, 0.55, 0.54, 0.54, 0.51, 0.56,
                          28.42, 25.62, 27.68, 29.16, 31.13, 33.24, 28.40, 25.96,
                          12.90, 11.56, 12.30, 12.97, 13.31, 13.42, 11.03, 11.84,
                          20.11, 18.62, 18.82, 19.11, 22.42, 24.27, 20.70, 19.36),
  Practitioners = c(439, 320, 250, 270, 263, 242, 229, 263,
                    5044, 4686, 5063, 5318, 5562, 5809, 4828, 4452,
                    2176, 2038, 2180, 2283, 2203, 2209, 1777, 1860,
                    3496, 3283, 3252, 3293, 3793, 4063, 3527, 3328)
)

# ------------------------------
# 2. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression <- function(data, category_name, y_var) {
  # Filter data for the category
  category_data <- data %>% filter(Category == category_name)
  
  if(nrow(category_data) < 2) {
    return(NULL)
  }
  
  x <- category_data$Year
  y <- category_data[[y_var]]
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r2 <- summary_model$r.squared
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  
  # Create equation text
  if(y_var == "Population_Millions") {
    equation <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(y_var == "Designations") {
    equation <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  p_text <- ifelse(p_value < 0.001, "p < 0.001", 
                   ifelse(p_value < 0.01, sprintf("p = %.4f", p_value),
                          sprintf("p = %.4f", p_value)))
  
  # Significance stars
  sig_stars <- ifelse(p_value < 0.001, "***", 
                      ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "ns")))
  
  return(list(
    model = model, slope = slope, intercept = intercept, r2 = r2,
    p_value = p_value, equation = equation, p_text = p_text,
    sig_stars = sig_stars,
    data = category_data
  ))
}

# ------------------------------
# 3. RUN REGRESSIONS FOR ALL CATEGORIES
# ------------------------------

categories <- c("Rural", "Non-Rural", "Partially Rural", "Facility_NonR")

# Store results in lists
desig_results <- list()
pop_results <- list()
prac_results <- list()

for(cat in categories) {
  desig_results[[cat]] <- run_regression(data, cat, "Designations")
  pop_results[[cat]] <- run_regression(data, cat, "Population_Millions")
  prac_results[[cat]] <- run_regression(data, cat, "Practitioners")
}

# ------------------------------
# 4. FUNCTION TO CREATE PLOT (NO SMOOTHER, ONLY TREND LINE)
# ------------------------------

create_plot <- function(results_list, y_var, y_label, title) {
  
  # Collect all data points and predictions for trend lines
  plot_data <- data.frame()
  trend_data <- data.frame()
  
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      cat_data <- res$data
      cat_data$Category <- cat
      plot_data <- bind_rows(plot_data, cat_data)
      
      # Create trend line predictions
      years <- seq(min(cat_data$Year), max(cat_data$Year), length.out = 100)
      pred_y <- predict(res$model, newdata = data.frame(x = years))
      trend_data <- bind_rows(trend_data, data.frame(
        Year = years,
        y = pred_y,
        Category = cat
      ))
    }
  }
  
  if(nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # Rename y column for consistent plotting
  names(plot_data)[which(names(plot_data) == y_var)] <- "y_value"
  names(trend_data)[2] <- "y_value"
  
  # Calculate y-axis limits with padding
  y_min <- min(plot_data$y_value, na.rm = TRUE)
  y_max <- max(plot_data$y_value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_min_padded <- y_min - y_range * 0.1
  y_max_padded <- y_max + y_range * 0.32
  
  # Define colors
  colors <- c("Rural" = "#1f78b4", 
              "Non-Rural" = "#33a02c", 
              "Partially Rural" = "#6a3d9a",
              "Facility_NonR" = "#e31a23")
  
  # Create base plot - points only
  p <- ggplot(plot_data, aes(x = Year, y = y_value, color = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    # Add trend lines (no smoother, just regression lines)
    geom_line(data = trend_data, aes(x = Year, y = y_value, color = Category), 
              size = 1.2, alpha = 0.9) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1)) +
    coord_cartesian(ylim = c(y_min_padded, y_max_padded)) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      caption = "Points: Actual values | Lines: Linear regression trends\nSignificance: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add ONE annotation per category (not repeated)
  annotation_y <- y_max_padded - (y_range * 0.02)
  step <- y_range * 0.055
  
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s %s", 
                          cat, res$equation, res$r2, res$p_text, res$sig_stars)
      p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                        label = ann_text, hjust = 0, size = 3.8, 
                        color = colors[cat], fontface = "bold", lineheight = 1.2)
      annotation_y <- annotation_y - step
    }
  }
  
  return(p)
}

# ------------------------------
# 5. CREATE ALL THREE PLOTS
# ------------------------------

# Plot 1: Number of Designations
p1 <- create_plot(desig_results, "Designations", 
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)")

# Plot 2: Population (in Millions)
p2 <- create_plot(pop_results, "Population_Millions", 
                  "Population (Millions)",
                  "Population of Designated HPSAs (in Millions)")

# Plot 3: Practitioners Needed
p3 <- create_plot(prac_results, "Practitioners", 
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations")

# Display plots
if(!is.null(p1)) {
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 14, height = 8, dpi = 300)
}
if(!is.null(p2)) {
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 14, height = 8, dpi = 300)
}
if(!is.null(p3)) {
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 14, height = 8, dpi = 300)
}

# ------------------------------
# 6. CREATE SUMMARY TABLE
# ------------------------------

create_summary <- function(results_list, metric) {
  df <- data.frame()
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      df <- bind_rows(df, data.frame(
        Category = cat,
        Metric = metric,
        Equation = res$equation,
        Slope = round(res$slope, 2),
        Intercept = round(res$intercept, 0),
        R_Squared = round(res$r2, 4),
        P_Value = format(res$p_value, scientific = TRUE, digits = 4),
        Significant_95 = ifelse(res$p_value < 0.05, "YES", "NO"),
        Significance = res$sig_stars,
        Observations = nrow(res$data)
      ))
    }
  }
  return(df)
}

# Create all summary tables
summary_designations <- create_summary(desig_results, "Number of Designations")
summary_population <- create_summary(pop_results, "Population (Millions)")
summary_practitioners <- create_summary(prac_results, "Practitioners Needed")

# ------------------------------
# 7. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 8. PRINT DETAILED RESULTS TO CONSOLE
# ------------------------------

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("DETAILED REGRESSION RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

for(cat in categories) {
  if(!is.null(desig_results[[cat]])) {
    res <- desig_results[[cat]]
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat(sprintf("CATEGORY: %s - DESIGNATIONS\n", cat))
    cat(paste(rep("-", 60), collapse = ""), "\n")
    cat(sprintf("Equation: %s\n", res$equation))
    cat(sprintf("R² = %.4f\n", res$r2))
    cat(sprintf("P-value = %.6f %s\n", res$p_value, res$sig_stars))
  }
}

# ------------------------------
# 9. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png\n")
cat("  - HPSA_Population_Trend.png\n")
cat("  - HPSA_Practitioners_Trend.png\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")


# Load required libraries
library(dplyr)
library(ggplot2)

# ------------------------------
# 1. CREATE DATA FRAME
# ------------------------------

data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  Category = c(rep("Non-Rural", 8),
               rep("Partially Rural", 8),
               rep("Rural", 8)),
  Designations = c(2028, 1976, 2037, 2082, 2071, 2103, 2036, 2023,
                   361, 332, 353, 371, 306, 298, 255, 294,
                   3451, 3659, 4085, 4146, 4608, 4931, 4705, 4742),
  Population_Millions = c(28.42, 25.62, 27.68, 29.16, 31.13, 33.24, 28.40, 25.96,
                          12.90, 11.56, 12.30, 12.97, 13.31, 13.42, 11.03, 11.84,
                          20.11, 18.62, 18.82, 19.11, 22.42, 24.27, 20.70, 19.36),
  Practitioners = c(5044, 4686, 5063, 5318, 5562, 5809, 4828, 4452,
                    2176, 2038, 2180, 2283, 2203, 2209, 1777, 1860,
                    3496, 3283, 3252, 3293, 3793, 4063, 3527, 3328)
)

# View data
cat("=== DATA SUMMARY ===\n")
print(head(data, 10))
cat("\nUnique categories:", paste(unique(data$Category), collapse = ", "))

# ------------------------------
# 2. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression <- function(data, category_name, y_var, y_label) {
  # Filter data for the category
  category_data <- data %>% filter(Category == category_name)
  
  if(nrow(category_data) < 2) {
    return(NULL)
  }
  
  x <- category_data$Year
  y <- category_data[[y_var]]
  
  # Run regression
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r2 <- summary_model$r.squared
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  ci <- confint(model, level = 0.95)
  
  # Create equation text
  if(y_var == "Population_Millions") {
    equation <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(y_var == "Designations") {
    equation <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  # P-value text
  if(p_value < 0.001) {
    p_text <- "p < 0.001"
    sig_stars <- "***"
  } else if(p_value < 0.01) {
    p_text <- sprintf("p = %.4f", p_value)
    sig_stars <- "**"
  } else if(p_value < 0.05) {
    p_text <- sprintf("p = %.4f", p_value)
    sig_stars <- "*"
  } else {
    p_text <- sprintf("p = %.4f", p_value)
    sig_stars <- "ns"
  }
  
  return(list(
    model = model, slope = slope, intercept = intercept, r2 = r2,
    p_value = p_value, ci = ci, equation = equation, p_text = p_text,
    sig_stars = sig_stars, data = category_data
  ))
}

# ------------------------------
# 3. RUN REGRESSIONS FOR ALL CATEGORIES
# ------------------------------

categories <- c("Rural", "Non-Rural", "Partially Rural")

# Store results
desig_results <- list()
pop_results <- list()
prac_results <- list()

for(cat in categories) {
  desig_results[[cat]] <- run_regression(data, cat, "Designations", "Designations")
  pop_results[[cat]] <- run_regression(data, cat, "Population_Millions", "Population (Millions)")
  prac_results[[cat]] <- run_regression(data, cat, "Practitioners", "Practitioners")
}

# ------------------------------
# 4. FUNCTION TO CREATE PLOT (TREND LINE ONLY, NO SMOOTHER)
# ------------------------------

create_plot <- function(results_list, y_var, y_label, title) {
  
  # Collect all data points and trend lines
  plot_data <- data.frame()
  trend_data <- data.frame()
  
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      cat_data <- res$data
      cat_data$Category <- cat
      plot_data <- bind_rows(plot_data, cat_data)
      
      # Create trend line (only two points needed for straight line)
      years <- seq(min(cat_data$Year), max(cat_data$Year), length.out = 2)
      pred_y <- predict(res$model, newdata = data.frame(x = years))
      trend_data <- bind_rows(trend_data, data.frame(
        Year = years,
        y_value = pred_y,
        Category = cat
      ))
    }
  }
  
  if(nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # Rename y column
  names(plot_data)[which(names(plot_data) == y_var)] <- "y_value"
  
  # Calculate y-axis limits with padding for annotations
  y_min <- min(plot_data$y_value, na.rm = TRUE)
  y_max <- max(plot_data$y_value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_min_padded <- y_min - y_range * 0.1
  y_max_padded <- y_max + y_range * 0.35  # Extra space for 3 annotations
  
  # Define colors
  colors <- c("Rural" = "#1f78b4", 
              "Non-Rural" = "#33a02c", 
              "Partially Rural" = "#6a3d9a")
  
  # Order categories for consistent legend
  plot_data$Category <- factor(plot_data$Category, levels = categories)
  trend_data$Category <- factor(trend_data$Category, levels = categories)
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Year, y = y_value, color = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(data = trend_data, aes(x = Year, y = y_value, color = Category), 
              size = 1.5, alpha = 0.9) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1)) +
    coord_cartesian(ylim = c(y_min_padded, y_max_padded)) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      caption = "Points: Actual values | Lines: Linear regression trends\nSignificance: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add ONE annotation per category at top left
  annotation_y <- y_max_padded - (y_range * 0.02)
  step <- y_range * 0.07
  
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      ann_text <- sprintf("● %s: %s\n  R² = %.3f | %s %s", 
                          cat, res$equation, res$r2, res$p_text, res$sig_stars)
      p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                        label = ann_text, hjust = 0, size = 3.8, 
                        color = colors[cat], fontface = "bold", lineheight = 1.3)
      annotation_y <- annotation_y - step
    }
  }
  
  return(p)
}

# ------------------------------
# 5. CREATE ALL THREE PLOTS
# ------------------------------

# Plot 1: Number of Designations
p1 <- create_plot(desig_results, "Designations", 
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)")

# Plot 2: Population (in Millions)
p2 <- create_plot(pop_results, "Population_Millions", 
                  "Population (Millions)",
                  "Population of Designated HPSAs (in Millions)")

# Plot 3: Practitioners Needed
p3 <- create_plot(prac_results, "Practitioners", 
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations")

# Display and save plots
if(!is.null(p1)) {
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p2)) {
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p3)) {
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 13, height = 7.5, dpi = 300)
}

# ------------------------------
# 6. CREATE SUMMARY TABLE
# ------------------------------

create_summary <- function(results_list, metric) {
  df <- data.frame()
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      df <- bind_rows(df, data.frame(
        Category = cat,
        Metric = metric,
        Equation = res$equation,
        Slope = round(res$slope, 2),
        Intercept = round(res$intercept, 0),
        R_Squared = round(res$r2, 4),
        P_Value = format(res$p_value, scientific = TRUE, digits = 4),
        Significant_95 = ifelse(res$p_value < 0.05, "YES", "NO"),
        Significance = res$sig_stars,
        CI_Lower = round(res$ci[2,1], 2),
        CI_Upper = round(res$ci[2,2], 2),
        Observations = nrow(res$data)
      ))
    }
  }
  return(df)
}

# Create summary tables
summary_designations <- create_summary(desig_results, "Number of Designations")
summary_population <- create_summary(pop_results, "Population (Millions)")
summary_practitioners <- create_summary(prac_results, "Practitioners Needed")

# ------------------------------
# 7. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 110), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 110), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 8. PRINT DETAILED RESULTS TO CONSOLE
# ------------------------------

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("DETAILED REGRESSION RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

for(cat in categories) {
  if(!is.null(desig_results[[cat]])) {
    res <- desig_results[[cat]]
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat(sprintf("CATEGORY: %s\n", cat))
    cat(paste(rep("-", 60), collapse = ""), "\n")
    cat(sprintf("DESIGNATIONS: %s\n", res$equation))
    cat(sprintf("  R² = %.4f | %s %s\n", res$r2, res$p_text, res$sig_stars))
    cat(sprintf("  95%% CI for Slope: [%.2f , %.2f]\n", res$ci[2,1], res$ci[2,2]))
  }
}

# ------------------------------
# 9. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png\n")
cat("  - HPSA_Population_Trend.png\n")
cat("  - HPSA_Practitioners_Trend.png\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")

# Load required libraries
library(dplyr)
library(ggplot2)

# ------------------------------
# 1. CREATE DATA FRAME
# ------------------------------

data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025,
           2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
  Category = c(rep("Non-Rural", 8),
               rep("Partially Rural", 8),
               rep("Rural", 8)),
  Designations = c(2028, 1976, 2037, 2082, 2071, 2103, 2036, 2023,
                   361, 332, 353, 371, 306, 298, 255, 294,
                   3451, 3659, 4085, 4146, 4608, 4931, 4705, 4742),
  Population_Millions = c(28.42, 25.62, 27.68, 29.16, 31.13, 33.24, 28.40, 25.96,
                          12.90, 11.56, 12.30, 12.97, 13.31, 13.42, 11.03, 11.84,
                          20.11, 18.62, 18.82, 19.11, 22.42, 24.27, 20.70, 19.36),
  Practitioners = c(5044, 4686, 5063, 5318, 5562, 5809, 4828, 4452,
                    2176, 2038, 2180, 2283, 2203, 2209, 1777, 1860,
                    3496, 3283, 3252, 3293, 3793, 4063, 3527, 3328)
)

# ------------------------------
# 2. FUNCTION FOR REGRESSION ANALYSIS
# ------------------------------

run_regression <- function(data, category_name, y_var) {
  category_data <- data %>% filter(Category == category_name)
  
  if(nrow(category_data) < 2) return(NULL)
  
  x <- category_data$Year
  y <- category_data[[y_var]]
  
  model <- lm(y ~ x)
  summary_model <- summary(model)
  
  slope <- coef(model)[2]
  intercept <- coef(model)[1]
  r2 <- summary_model$r.squared
  f <- summary_model$fstatistic
  p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  ci <- confint(model, level = 0.95)
  
  # Create SINGLE LINE equation text
  if(y_var == "Population_Millions") {
    equation <- sprintf("Population = %.2f×Year + %.0f", slope, intercept)
  } else if(y_var == "Designations") {
    equation <- sprintf("Designations = %.1f×Year + %.0f", slope, intercept)
  } else {
    equation <- sprintf("Practitioners = %.1f×Year + %.0f", slope, intercept)
  }
  
  # P-value text
  if(p_value < 0.001) {
    p_text <- "p < 0.001"
    sig_stars <- "***"
  } else if(p_value < 0.01) {
    p_text <- sprintf("p = %.4f", p_value)
    sig_stars <- "**"
  } else if(p_value < 0.05) {
    p_text <- sprintf("p = %.4f", p_value)
    sig_stars <- "*"
  } else {
    p_text <- sprintf("p = %.4f", p_value)
    sig_stars <- "ns"
  }
  
  return(list(
    model = model, slope = slope, r2 = r2, p_value = p_value,
    ci = ci, equation = equation, p_text = p_text, sig_stars = sig_stars,
    data = category_data
  ))
}

# ------------------------------
# 3. RUN REGRESSIONS
# ------------------------------

categories <- c("Rural", "Non-Rural", "Partially Rural")

desig_results <- list()
pop_results <- list()
prac_results <- list()

for(cat in categories) {
  desig_results[[cat]] <- run_regression(data, cat, "Designations")
  pop_results[[cat]] <- run_regression(data, cat, "Population_Millions")
  prac_results[[cat]] <- run_regression(data, cat, "Practitioners")
}

# ------------------------------
# 4. FUNCTION TO CREATE PLOT (SINGLE LINE EQUATIONS, SMALL FONT)
# ------------------------------

create_plot <- function(results_list, y_var, y_label, title) {
  
  plot_data <- data.frame()
  trend_data <- data.frame()
  
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      cat_data <- res$data
      cat_data$Category <- cat
      plot_data <- bind_rows(plot_data, cat_data)
      
      # Trend line (only 2 points)
      years <- seq(min(cat_data$Year), max(cat_data$Year), length.out = 2)
      pred_y <- predict(res$model, newdata = data.frame(x = years))
      trend_data <- bind_rows(trend_data, data.frame(
        Year = years,
        y_value = pred_y,
        Category = cat
      ))
    }
  }
  
  if(nrow(plot_data) == 0) return(NULL)
  
  names(plot_data)[which(names(plot_data) == y_var)] <- "y_value"
  
  # Y-axis limits
  y_min <- min(plot_data$y_value, na.rm = TRUE)
  y_max <- max(plot_data$y_value, na.rm = TRUE)
  y_range <- y_max - y_min
  y_min_padded <- y_min - y_range * 0.1
  y_max_padded <- y_max + y_range * 0.30
  
  colors <- c("Rural" = "#1f78b4", "Non-Rural" = "#33a02c", "Partially Rural" = "#6a3d9a")
  
  plot_data$Category <- factor(plot_data$Category, levels = categories)
  trend_data$Category <- factor(trend_data$Category, levels = categories)
  
  p <- ggplot(plot_data, aes(x = Year, y = y_value, color = Category)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(data = trend_data, aes(x = Year, y = y_value, color = Category), 
              size = 1.5, alpha = 0.9) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(2018, 2025, by = 1)) +
    coord_cartesian(ylim = c(y_min_padded, y_max_padded)) +
    labs(
      title = title,
      x = "Year",
      y = y_label,
      caption = "Points: Actual values | Lines: Linear regression trends"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add SINGLE LINE annotations with SMALLER FONT (size = 3)
  annotation_y <- y_max_padded - (y_range * 0.02)
  step <- y_range * 0.06
  
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      # SINGLE LINE equation format
      ann_text <- sprintf("%s: %s | R² = %.3f | %s %s", 
                          cat, res$equation, res$r2, res$p_text, res$sig_stars)
      p <- p + annotate("text", x = 2018.2, y = annotation_y, 
                        label = ann_text, hjust = 0, size = 3.0,  # SMALLER FONT
                        color = colors[cat], fontface = "bold")
      annotation_y <- annotation_y - step
    }
  }
  
  return(p)
}

# ------------------------------
# 5. CREATE ALL THREE PLOTS
# ------------------------------

p1 <- create_plot(desig_results, "Designations", 
                  "Number of HPSA Designations",
                  "HPSA Designations Trends (2018-2025)")

p2 <- create_plot(pop_results, "Population_Millions", 
                  "Population (Millions)",
                  "Population of Designated HPSAs (in Millions)")

p3 <- create_plot(prac_results, "Practitioners", 
                  "Number of Practitioners Needed",
                  "Practitioners Needed to Remove Designations")

# Display and save
if(!is.null(p1)) {
  print(p1)
  ggsave("HPSA_Designations_Trend.png", p1, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p2)) {
  print(p2)
  ggsave("HPSA_Population_Trend.png", p2, width = 13, height = 7.5, dpi = 300)
}
if(!is.null(p3)) {
  print(p3)
  ggsave("HPSA_Practitioners_Trend.png", p3, width = 13, height = 7.5, dpi = 300)
}

# ------------------------------
# 6. SUMMARY TABLE
# ------------------------------

create_summary <- function(results_list, metric) {
  df <- data.frame()
  for(cat in categories) {
    if(!is.null(results_list[[cat]])) {
      res <- results_list[[cat]]
      # SINGLE LINE equation for table as well
      eq_line <- sprintf("%s | R²=%.3f | %s %s", 
                         res$equation, res$r2, res$p_text, res$sig_stars)
      df <- bind_rows(df, data.frame(
        Category = cat,
        Metric = metric,
        Equation = eq_line,
        Slope = round(res$slope, 2),
        R_Squared = round(res$r2, 4),
        P_Value = format(res$p_value, scientific = TRUE, digits = 4),
        Significance = res$sig_stars,
        Observations = nrow(res$data)
      ))
    }
  }
  return(df)
}

summary_designations <- create_summary(desig_results, "Number of Designations")
summary_population <- create_summary(pop_results, "Population (Millions)")
summary_practitioners <- create_summary(prac_results, "Practitioners Needed")

# ------------------------------
# 7. DISPLAY SUMMARY TABLES
# ------------------------------

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - NUMBER OF DESIGNATIONS\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_designations)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - POPULATION (IN MILLIONS)\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_population)

cat("\n\n", paste(rep("=", 100), collapse = ""), "\n")
cat("SUMMARY TABLE - PRACTITIONERS NEEDED\n")
cat(paste(rep("=", 100), collapse = ""), "\n")
print(summary_practitioners)

# ------------------------------
# 8. EXPORT RESULTS
# ------------------------------

write.csv(summary_designations, "HPSA_regression_designations.csv", row.names = FALSE)
write.csv(summary_population, "HPSA_regression_population.csv", row.names = FALSE)
write.csv(summary_practitioners, "HPSA_regression_practitioners.csv", row.names = FALSE)

cat("\n\n✅ Analysis complete!\n")
cat("\n📁 Files saved:\n")
cat("  - HPSA_Designations_Trend.png\n")
cat("  - HPSA_Population_Trend.png\n")
cat("  - HPSA_Practitioners_Trend.png\n")
cat("  - HPSA_regression_designations.csv\n")
cat("  - HPSA_regression_population.csv\n")
cat("  - HPSA_regression_practitioners.csv\n")































# Load required libraries
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)
library(shapviz)
library(plotly)
library(DT)

# Read the data
data <- read.table(text = "
Height Weight Gender Age_Group Marital_Status Pain_Intensity Injury Numbness Difficulty_in_Movement howmanymealsdoyouhaveeveryday Meals Exercise_Time Exercise_Routine Employment SittingTime_on_Chair Location No_of_Sites
170 85 1 4 2 2 2 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
146 75 2 4 2 2 1 2 1 3 3 4 4 4 3 'L3 & L4 - L5 & L4 - S1' 3
176 92 1 2 2 2 1 1 1 2 2 3 1 2 3 'L4 - L5 & L5' 2
160 80 2 4 2 2 2 1 1 2 2 4 4 4 3 'L3-L4 & L4 - L5 & L5- S1' 3
100 75 1 1 2 1 1 2 2 3 3 2 1 4 2 'L4 - L5' 1
182 79 1 2 1 1 1 1 1 3 3 2 2 2 3 'L4 - L5' 1
145 78 2 4 2 1 2 2 1 2 2 4 4 4 2 'L5- S1' 1
157 72 2 1 1 2 2 2 1 2 2 4 4 4 3 'L4 - L5' 1
171 80 2 2 2 1 1 2 1 2 2 4 4 4 2 'L4 - L5' 1
154 83 1 4 2 2 1 1 1 3 3 4 4 4 3 'L3 -4-5- S1' 2
166 70 2 2 2 2 1 2 1 3 3 2 1 1 3 'L4 - L5' 1
149 80 2 4 2 2 2 2 1 2 2 4 4 4 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
167 49 1 2 1 2 1 1 1 4 4 2 1 4 2 'L4 - L5' 1
152 54 2 2 1 1 1 1 1 3 3 2 2 1 2 'L5- S1' 1
170 108 1 2 2 2 2 2 2 4 4 4 1 4 1 'L4 - L5 & L5- S1' 2
155 60 1 2 2 1 1 2 1 3 3 2 1 2 2 'L4 - L5' 1
174 80 1 2 2 2 2 2 1 2 2 3 2 4 2 'L3 - L4 & L4 - L5' 2
164 61 1 2 2 1 1 1 1 2 2 2 1 1 2 'L4 - L5' 1
182 72 1 2 1 1 1 2 1 2 2 2 1 3 3 'L5- S1' 1
175 73 2 2 1 2 2 1 1 3 3 1 2 2 3 'L4 - L5 & L5- S1' 2
153 89 2 4 2 2 1 2 2 3 3 3 1 3 2 'L4 - L5 & L5' 2
171 94 1 3 2 2 1 1 1 3 3 4 1 4 2 'L3 -4-5- S1' 2
163 81 2 3 2 2 1 2 1 2 2 4 4 4 3 'L4 - L5' 1
153 81 2 3 2 1 2 1 1 3 3 4 4 4 3 'L5- S1' 1
164 93 2 4 2 2 1 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
173 75 1 1 1 2 1 2 1 2 2 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 63 2 2 1 2 1 2 1 3 3 2 1 4 2 'L3 - L4 & L4 - L5' 2
155 110 2 1 1 1 2 2 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
152 75 1 4 2 2 2 2 1 3 3 3 1 1 2 'L4 - L5 & L5- S1' 2
151 91 2 4 2 1 2 1 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
188 92 1 2 2 2 2 1 1 3 3 2 1 1 3 'L4 - L5' 1
161 86 1 2 2 2 1 2 2 2 2 4 1 1 2 'L4 - L5 & L5- S1' 2
160 78 2 3 2 2 2 2 1 3 3 4 4 2 3 'L4 - L5 & L5- S1' 2
173 91 1 3 2 2 1 1 1 2 2 4 4 1 2 'L4 - L5 & L5- S1' 2
170 65 1 2 2 2 1 2 2 2 2 4 1 2 2 'L4 - L5' 1
150 83 2 3 2 1 2 1 1 3 3 4 4 1 2 'L4 - L5 & L5- S1' 2
160 73 1 3 2 2 2 2 1 2 2 3 1 3 2 'L5- S1' 1
162 56 1 3 2 2 1 1 1 4 4 2 1 4 1 'L3-L4 & L4 - L5' 2
170 81 2 3 2 2 1 1 1 3 3 2 2 2 2 'L3-L4 & L4 - L5' 2
171 54 1 2 2 1 1 1 2 3 3 4 1 4 2 'L3-L4 & L4- L5 & L5- S1' 3
150 91 1 2 1 2 2 1 1 2 2 4 1 4 3 'L1 - L2 & L4- L5' 2
176 89 1 2 1 2 2 2 1 2 2 3 1 1 2 'L2-3-4-5-S1' 2
171 77 1 1 1 2 1 2 1 3 3 1 1 4 3 'L2-3-4-5-S1' 2
156 72 1 4 2 2 1 1 1 2 2 4 4 4 3 'L4 - S1' 1
152 82 2 4 2 2 1 2 1 3 3 4 4 4 3 'L4 - L5' 1
166 64 2 1 1 1 1 2 1 3 3 2 2 4 3 'L3-L4 & L4- L5' 2
162 53 2 1 2 1 2 1 1 3 3 2 1 4 2 'L4- L5 & L5- S1' 2
164 87 1 4 2 2 2 1 1 1 1 2 4 4 3 'L4 - L5 & L5- S1' 2
142 53 2 3 2 2 2 2 1 3 3 3 1 4 3 'L4 - L5' 1
156 75 1 3 2 1 1 1 1 2 2 3 1 4 3 'L2 - L3 & L2 - L3' 2
145 85 2 4 2 2 2 1 1 3 3 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
179 115 1 2 2 2 2 1 1 3 3 4 4 3 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
180 86 2 1 2 1 1 2 2 2 2 2 2 2 2 'L4 - L5 & L5- S1' 2
170 72 1 2 2 2 2 1 1 3 3 2 2 2 3 'L4- L5 & L5- S1' 2
167 85 1 2 1 2 1 1 1 2 2 3 1 1 3 'L3 - L4 & L4 - L5' 2
150 55 2 2 2 2 2 1 1 2 2 4 4 4 2 'L4 - L5 & L5- S1' 2
160 74 1 3 2 2 1 2 1 2 2 3 1 3 2 'L3 -4-5- S1' 2
156 77 2 4 2 2 1 1 1 3 3 4 4 4 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 83 1 4 2 2 2 2 1 3 3 4 4 1 2 'L3-L4 & L4 - L5' 2
173 155 1 1 1 2 2 2 1 2 2 4 1 1 2 'L3 - L4 & L4 - L5 & L5- S1' 3
164 78 1 2 2 2 2 2 1 3 3 3 1 3 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
", header = TRUE, sep = " ", quote = "'")

# Data preprocessing
# Convert Pain_Intensity to binary (1 = Mild/Moderate, 2 = Severe)
data$Pain_Binary <- factor(ifelse(data$Pain_Intensity == 1, "Mild", "Severe"))

# Select features
features <- c("Height", "Weight", "Gender", "Age_Group", "Marital_Status", 
              "Injury", "Numbness", "Difficulty_in_Movement", "Meals", 
              "Exercise_Time", "Exercise_Routine", "Employment", 
              "SittingTime_on_Chair", "No_of_Sites")

# Convert categorical variables to factors
categorical_vars <- c("Gender", "Age_Group", "Marital_Status", "Injury", 
                      "Numbness", "Difficulty_in_Movement", "Meals", 
                      "Exercise_Time", "Exercise_Routine", "Employment")

for(var in categorical_vars) {
  if(var %in% colnames(data)) {
    data[[var]] <- as.factor(data[[var]])
  }
}

# Prepare data
X <- data[, features]
y <- data$Pain_Binary

# Remove rows with NAs
complete_cases <- complete.cases(X, y)
X <- X[complete_cases, ]
y <- y[complete_cases]

cat("Dataset dimensions:", dim(X), "\n")
cat("Class distribution:\n")
print(table(y))

# Split data
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

cat("\nTraining set:", nrow(X_train), "observations")
cat("\nTest set:", nrow(X_test), "observations\n")

# Function to calculate metrics
calculate_metrics <- function(cm, probs = NULL, true_labels = NULL) {
  TP <- cm[2,2]; TN <- cm[1,1]; FP <- cm[1,2]; FN <- cm[2,1]
  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  auc <- if(!is.null(probs)) roc(true_labels, probs, quiet = TRUE)$auc else NA
  return(c(Accuracy = accuracy, AUC = auc, Sensitivity = sensitivity, 
           Specificity = specificity, PPV = ppv, NPV = npv))
}

# ==================== RANDOM FOREST ====================
cat("\n", rep("=", 70), "\n")
cat("RANDOM FOREST ANALYSIS\n")
cat(rep("=", 70), "\n")

set.seed(123)
rf_model <- randomForest(x = X_train, y = y_train, ntree = 500, importance = TRUE,
                         mtry = floor(sqrt(ncol(X_train))))

# Predictions
rf_pred_class <- predict(rf_model, X_test)
rf_pred_prob <- predict(rf_model, X_test, type = "prob")[,2]
rf_cm <- table(rf_pred_class, y_test)
rf_metrics <- calculate_metrics(rf_cm, rf_pred_prob, as.numeric(y_test))

# Random Forest Results Table
rf_results <- data.frame(
  Metric = c("Accuracy", "AUC", "Sensitivity", "Specificity", "PPV", "NPV"),
  Value = round(rf_metrics, 4)
)
cat("\nRandom Forest Performance:\n")
print(rf_results)

# Variable Importance
rf_importance <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>% arrange(desc(MeanDecreaseAccuracy))

cat("\nRandom Forest Variable Importance (Top 10 by MDA):\n")
print(head(rf_importance, 10))

# Interactive RF Importance Plot
rf_importance_plot <- ggplot(rf_importance[1:10,], aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest: Mean Decrease Accuracy", x = "Features", y = "MDA") +
  theme_minimal()
ggplotly(rf_importance_plot)

# ==================== XGBOOST ====================
cat("\n", rep("=", 70), "\n")
cat("XGBOOST ANALYSIS\n")
cat(rep("=", 70), "\n")

# Prepare data for XGBoost
X_train_num <- X_train
X_test_num <- X_test
for(col in names(X_train_num)) {
  if(is.factor(X_train_num[[col]])) {
    X_train_num[[col]] <- as.numeric(X_train_num[[col]])
    X_test_num[[col]] <- as.numeric(X_test_num[[col]])
  }
}

X_train_xgb <- scale(as.matrix(X_train_num))
X_test_xgb <- scale(as.matrix(X_test_num), 
                    center = attr(X_train_xgb, "scaled:center"),
                    scale = attr(X_train_xgb, "scaled:scale"))

y_train_num <- as.numeric(y_train) - 1
y_test_num <- as.numeric(y_test) - 1

dtrain <- xgb.DMatrix(X_train_xgb, label = y_train_num)
dtest <- xgb.DMatrix(X_test_xgb, label = y_test_num)

# Train XGBoost
params <- list(objective = "binary:logistic", eval_metric = "auc", 
               max_depth = 3, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8)
set.seed(123)
xgb_model <- xgb.train(params, dtrain, nrounds = 50, verbose = 0)

# Predictions
xgb_pred_prob <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)
xgb_cm <- table(factor(xgb_pred_class, 0:1), factor(y_test_num, 0:1))
xgb_metrics <- calculate_metrics(xgb_cm, xgb_pred_prob, y_test_num)

# XGBoost Results Table
xgb_results <- data.frame(
  Metric = c("Accuracy", "AUC", "Sensitivity", "Specificity", "PPV", "NPV"),
  Value = round(xgb_metrics, 4)
)
cat("\nXGBoost Performance:\n")
print(xgb_results)

# Feature Importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X_train_xgb))
cat("\nXGBoost Feature Importance (Top 10):\n")
print(head(importance_matrix[, c("Feature", "Gain")], 10))

# Interactive XGBoost Importance Plot
xgb_importance_plot <- ggplot(head(importance_matrix, 10), aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "XGBoost: Feature Importance (Gain)", x = "Features", y = "Gain") +
  theme_minimal()
ggplotly(xgb_importance_plot)

# ==================== INTERACTIVE SHAP ANALYSIS ====================
cat("\n", rep("=", 70), "\n")
cat("INTERACTIVE SHAP ANALYSIS\n")
cat(rep("=", 70), "\n")

# Calculate SHAP values
shap_obj <- shapviz(xgb_model, X_pred = X_test_xgb, X = X_train_xgb[1:min(50, nrow(X_train_xgb)), ])

# SHAP Importance Table
shap_importance <- data.frame(
  Feature = names(shap_obj$S$mean),
  Mean_SHAP = as.numeric(shap_obj$S$mean),
  Direction = ifelse(shap_obj$S$mean > 0, "Positive", "Negative")
) %>% arrange(desc(Mean_SHAP))

cat("\nSHAP Feature Importance:\n")
print(head(shap_importance, 15))

# Interactive SHAP Bar Plot
shap_bar <- sv_importance(shap_obj, kind = "bar", max_display = 15) +
  ggtitle("SHAP Feature Importance (Interactive)") +
  theme_minimal()
ggplotly(shap_bar)

# Interactive SHAP Beeswarm Plot
shap_beeswarm <- sv_importance(shap_obj, kind = "both", max_display = 15) +
  ggtitle("SHAP Feature Importance: Bar + Beeswarm")
ggplotly(shap_beeswarm)

# Interactive SHAP Dependence Plots for Top 6 Features
top_features <- shap_importance$Feature[1:min(6, nrow(shap_importance))]

for(feature in top_features) {
  p <- sv_dependence(shap_obj, v = feature, color_var = "auto") +
    ggtitle(paste("SHAP Dependence Plot:", feature)) +
    theme_minimal()
  print(ggplotly(p))
}

# Individual Prediction Explanation (Waterfall Plot)
# Select a random test observation
set.seed(456)
random_idx <- sample(1:nrow(X_test_xgb), 1)
waterfall <- sv_waterfall(shap_obj, row_id = random_idx, max_display = 10) +
  ggtitle(paste("SHAP Waterfall Plot - Patient", random_idx, 
                "(Prediction:", round(xgb_pred_prob[random_idx], 3), ")"))
ggplotly(waterfall)

# ==================== INTERACTIVE COMPARISON DASHBOARD ====================
# Model Comparison Table
comparison_table <- data.frame(
  Metric = c("Accuracy", "AUC", "Sensitivity", "Specificity", "PPV", "NPV"),
  RandomForest = round(rf_metrics, 4),
  XGBoost = round(xgb_metrics, 4)
)

cat("\n", rep("=", 70), "\n")
cat("MODEL COMPARISON\n")
cat(rep("=", 70), "\n")
print(comparison_table)

# Interactive ROC Curves
rf_roc <- roc(y_test_num, rf_pred_prob)
xgb_roc <- roc(y_test_num, xgb_pred_prob)

roc_df <- data.frame(
  FPR = c(1 - rf_roc$specificities, 1 - xgb_roc$specificities),
  TPR = c(rf_roc$sensitivities, xgb_roc$sensitivities),
  Model = rep(c("Random Forest", "XGBoost"), c(length(rf_roc$sensitivities), length(xgb_roc$sensitivities)))
)

roc_plot <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", alpha = 0.5) +
  labs(title = "ROC Curves Comparison",
       subtitle = paste("RF AUC =", round(rf_metrics["AUC"], 3), 
                        "| XGB AUC =", round(xgb_metrics["AUC"], 3))) +
  theme_minimal() +
  scale_color_manual(values = c("Random Forest" = "blue", "XGBoost" = "red"))
ggplotly(roc_plot)

# ==================== CREATE INTERACTIVE HTML REPORT ====================
# Save interactive plots as HTML
htmlwidgets::saveWidget(ggplotly(rf_importance_plot), "RF_Importance.html")
htmlwidgets::saveWidget(ggplotly(xgb_importance_plot), "XGB_Importance.html")
htmlwidgets::saveWidget(ggplotly(shap_bar), "SHAP_Bar_Plot.html")
htmlwidgets::saveWidget(ggplotly(shap_beeswarm), "SHAP_Beeswarm.html")
htmlwidgets::saveWidget(ggplotly(roc_plot), "ROC_Comparison.html")

# Save data tables
library(DT)
rf_table <- datatable(rf_results, options = list(pageLength = 10), 
                      caption = "Random Forest Performance Metrics")
xgb_table <- datatable(xgb_results, options = list(pageLength = 10),
                       caption = "XGBoost Performance Metrics")
shap_table <- datatable(shap_importance, options = list(pageLength = 15),
                        caption = "SHAP Feature Importance")

# Save comparison table
comparison_table_dt <- datatable(comparison_table, options = list(pageLength = 10),
                                 caption = "Model Performance Comparison")

# Save all results
write.csv(rf_importance, "RF_Importance.csv", row.names = FALSE)
































































# Load required libraries
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)
library(shapviz)
library(plotly)

# Read the data
data <- read.table(text = "
Height Weight Gender Age_Group Marital_Status Pain_Intensity Injury Numbness Difficulty_in_Movement howmanymealsdoyouhaveeveryday Meals Exercise_Time Exercise_Routine Employment SittingTime_on_Chair Location No_of_Sites
170 85 1 4 2 2 2 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
146 75 2 4 2 2 1 2 1 3 3 4 4 4 3 'L3 & L4 - L5 & L4 - S1' 3
176 92 1 2 2 2 1 1 1 2 2 3 1 2 3 'L4 - L5 & L5' 2
160 80 2 4 2 2 2 1 1 2 2 4 4 4 3 'L3-L4 & L4 - L5 & L5- S1' 3
100 75 1 1 2 1 1 2 2 3 3 2 1 4 2 'L4 - L5' 1
182 79 1 2 1 1 1 1 1 3 3 2 2 2 3 'L4 - L5' 1
145 78 2 4 2 1 2 2 1 2 2 4 4 4 2 'L5- S1' 1
157 72 2 1 1 2 2 2 1 2 2 4 4 4 3 'L4 - L5' 1
171 80 2 2 2 1 1 2 1 2 2 4 4 4 2 'L4 - L5' 1
154 83 1 4 2 2 1 1 1 3 3 4 4 4 3 'L3 -4-5- S1' 2
166 70 2 2 2 2 1 2 1 3 3 2 1 1 3 'L4 - L5' 1
149 80 2 4 2 2 2 2 1 2 2 4 4 4 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
167 49 1 2 1 2 1 1 1 4 4 2 1 4 2 'L4 - L5' 1
152 54 2 2 1 1 1 1 1 3 3 2 2 1 2 'L5- S1' 1
170 108 1 2 2 2 2 2 2 4 4 4 1 4 1 'L4 - L5 & L5- S1' 2
155 60 1 2 2 1 1 2 1 3 3 2 1 2 2 'L4 - L5' 1
174 80 1 2 2 2 2 2 1 2 2 3 2 4 2 'L3 - L4 & L4 - L5' 2
164 61 1 2 2 1 1 1 1 2 2 2 1 1 2 'L4 - L5' 1
182 72 1 2 1 1 1 2 1 2 2 2 1 3 3 'L5- S1' 1
175 73 2 2 1 2 2 1 1 3 3 1 2 2 3 'L4 - L5 & L5- S1' 2
153 89 2 4 2 2 1 2 2 3 3 3 1 3 2 'L4 - L5 & L5' 2
171 94 1 3 2 2 1 1 1 3 3 4 1 4 2 'L3 -4-5- S1' 2
163 81 2 3 2 2 1 2 1 2 2 4 4 4 3 'L4 - L5' 1
153 81 2 3 2 1 2 1 1 3 3 4 4 4 3 'L5- S1' 1
164 93 2 4 2 2 1 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
173 75 1 1 1 2 1 2 1 2 2 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 63 2 2 1 2 1 2 1 3 3 2 1 4 2 'L3 - L4 & L4 - L5' 2
155 110 2 1 1 1 2 2 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
152 75 1 4 2 2 2 2 1 3 3 3 1 1 2 'L4 - L5 & L5- S1' 2
151 91 2 4 2 1 2 1 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
188 92 1 2 2 2 2 1 1 3 3 2 1 1 3 'L4 - L5' 1
161 86 1 2 2 2 1 2 2 2 2 4 1 1 2 'L4 - L5 & L5- S1' 2
160 78 2 3 2 2 2 2 1 3 3 4 4 2 3 'L4 - L5 & L5- S1' 2
173 91 1 3 2 2 1 1 1 2 2 4 4 1 2 'L4 - L5 & L5- S1' 2
170 65 1 2 2 2 1 2 2 2 2 4 1 2 2 'L4 - L5' 1
150 83 2 3 2 1 2 1 1 3 3 4 4 1 2 'L4 - L5 & L5- S1' 2
160 73 1 3 2 2 2 2 1 2 2 3 1 3 2 'L5- S1' 1
162 56 1 3 2 2 1 1 1 4 4 2 1 4 1 'L3-L4 & L4 - L5' 2
170 81 2 3 2 2 1 1 1 3 3 2 2 2 2 'L3-L4 & L4 - L5' 2
171 54 1 2 2 1 1 1 2 3 3 4 1 4 2 'L3-L4 & L4- L5 & L5- S1' 3
150 91 1 2 1 2 2 1 1 2 2 4 1 4 3 'L1 - L2 & L4- L5' 2
176 89 1 2 1 2 2 2 1 2 2 3 1 1 2 'L2-3-4-5-S1' 2
171 77 1 1 1 2 1 2 1 3 3 1 1 4 3 'L2-3-4-5-S1' 2
156 72 1 4 2 2 1 1 1 2 2 4 4 4 3 'L4 - S1' 1
152 82 2 4 2 2 1 2 1 3 3 4 4 4 3 'L4 - L5' 1
166 64 2 1 1 1 1 2 1 3 3 2 2 4 3 'L3-L4 & L4- L5' 2
162 53 2 1 2 1 2 1 1 3 3 2 1 4 2 'L4- L5 & L5- S1' 2
164 87 1 4 2 2 2 1 1 1 1 2 4 4 3 'L4 - L5 & L5- S1' 2
142 53 2 3 2 2 2 2 1 3 3 3 1 4 3 'L4 - L5' 1
156 75 1 3 2 1 1 1 1 2 2 3 1 4 3 'L2 - L3 & L2 - L3' 2
145 85 2 4 2 2 2 1 1 3 3 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
179 115 1 2 2 2 2 1 1 3 3 4 4 3 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
180 86 2 1 2 1 1 2 2 2 2 2 2 2 2 'L4 - L5 & L5- S1' 2
170 72 1 2 2 2 2 1 1 3 3 2 2 2 3 'L4- L5 & L5- S1' 2
167 85 1 2 1 2 1 1 1 2 2 3 1 1 3 'L3 - L4 & L4 - L5' 2
150 55 2 2 2 2 2 1 1 2 2 4 4 4 2 'L4 - L5 & L5- S1' 2
160 74 1 3 2 2 1 2 1 2 2 3 1 3 2 'L3 -4-5- S1' 2
156 77 2 4 2 2 1 1 1 3 3 4 4 4 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 83 1 4 2 2 2 2 1 3 3 4 4 1 2 'L3-L4 & L4 - L5' 2
173 155 1 1 1 2 2 2 1 2 2 4 1 1 2 'L3 - L4 & L4 - L5 & L5- S1' 3
164 78 1 2 2 2 2 2 1 3 3 3 1 3 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
", header = TRUE, sep = " ", quote = "'")

# Apply value labels based on your definitions
data$Gender <- factor(data$Gender, levels = c(1,2), labels = c("Male", "Female"))
data$Age_Group <- factor(data$Age_Group, levels = c(1,2,3,4), labels = c("20-29", "30-39", "40-49", "50-59"))
data$Marital_Status <- factor(data$Marital_Status, levels = c(1,2), labels = c("Single", "Married"))
data$Pain_Intensity <- factor(data$Pain_Intensity, levels = c(1,2), labels = c("Strong", "Very Strong"))
data$Injury <- factor(data$Injury, levels = c(1,2), labels = c("Yes", "No"))
data$Numbness <- factor(data$Numbness, levels = c(1,2), labels = c("Yes", "No"))
data$Difficulty_in_Movement <- factor(data$Difficulty_in_Movement, levels = c(1,2), labels = c("Yes", "No"))
data$Exercise_Time <- factor(data$Exercise_Time, levels = c(1,2,3,4), 
                             labels = c("Every day", "Once a week", "Three times a week", "I do not exercise"))
data$Exercise_Routine <- factor(data$Exercise_Routine, levels = c(1,2,3), 
                                labels = c("Running", "Walking", "Other"))
data$Employment <- factor(data$Employment, levels = c(1,2,3,4), 
                          labels = c("Education", "Health care", "Physical activity", "Other"))
data$SittingTime_on_Chair <- factor(data$SittingTime_on_Chair, levels = c(1,2,3), 
                                    labels = c("<3 hours", "3-6 hours", "6-9 hours"))

# DV: Pain_Intensity (Strong vs Very Strong)
y <- data$Pain_Intensity

# Select features for modeling
features <- c("Height", "Weight", "Gender", "Age_Group", "Marital_Status", 
              "Injury", "Numbness", "Difficulty_in_Movement", "Meals", 
              "Exercise_Time", "Exercise_Routine", "Employment", 
              "SittingTime_on_Chair", "No_of_Sites")

X <- data[, features]

# Convert all categorical variables to factors properly
for(col in names(X)) {
  if(col %in% c("Gender", "Age_Group", "Marital_Status", "Injury", "Numbness", 
                "Difficulty_in_Movement", "Exercise_Time", "Exercise_Routine", 
                "Employment", "SittingTime_on_Chair")) {
    X[[col]] <- as.factor(as.character(X[[col]]))
  }
}

# Remove rows with NAs
complete_cases <- complete.cases(X, y)
X <- X[complete_cases, ]
y <- y[complete_cases]

cat("Dataset Dimensions:", dim(X), "\n")
cat("\nPain Intensity Distribution (DV):\n")
print(table(y))
cat("\nProportions:\n")
print(prop.table(table(y)))

# Split data
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

cat("\nTraining set:", nrow(X_train), "observations")
cat("\nTest set:", nrow(X_test), "observations\n")

# Function to calculate metrics
calculate_metrics <- function(cm, probs = NULL, true_labels = NULL) {
  TP <- cm[2,2]; TN <- cm[1,1]; FP <- cm[1,2]; FN <- cm[2,1]
  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  auc <- if(!is.null(probs)) roc(true_labels, probs, quiet = TRUE)$auc else NA
  return(c(Accuracy = accuracy, AUC = auc, Sensitivity = sensitivity, 
           Specificity = specificity, PPV = ppv, NPV = npv))
}

# ==================== RANDOM FOREST ====================
cat("\n", rep("=", 70), "\n")
cat("RANDOM FOREST ANALYSIS\n")
cat(rep("=", 70), "\n")

set.seed(123)
rf_model <- randomForest(x = X_train, y = y_train, ntree = 500, importance = TRUE,
                         mtry = floor(sqrt(ncol(X_train))))

# Predictions
rf_pred_class <- predict(rf_model, X_test)
rf_pred_prob <- predict(rf_model, X_test, type = "prob")[,2]
rf_cm <- table(rf_pred_class, y_test)
rf_metrics <- calculate_metrics(rf_cm, rf_pred_prob, as.numeric(y_test))

cat("\nRandom Forest Performance Metrics:\n")
rf_results <- data.frame(Metric = names(rf_metrics), Value = round(rf_metrics, 4))
print(rf_results)

cat("\nConfusion Matrix:\n")
print(rf_cm)

# Variable Importance
rf_importance <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>% arrange(desc(MeanDecreaseAccuracy))

cat("\nRandom Forest Variable Importance (Ranked by Mean Decrease Accuracy):\n")
print(head(rf_importance, 10))

cat("\nRandom Forest Variable Importance (Ranked by Mean Decrease Gini):\n")
print(head(rf_importance %>% arrange(desc(MeanDecreaseGini)), 10))

# Plot RF Importance
par(mfrow = c(1,2), mar = c(5, 10, 4, 2))
varImpPlot(rf_model, main = "Random Forest Variable Importance", cex = 0.7)

# ==================== XGBOOST ====================
cat("\n", rep("=", 70), "\n")
cat("XGBOOST ANALYSIS\n")
cat(rep("=", 70), "\n")

# Prepare data for XGBoost
X_train_num <- X_train
X_test_num <- X_test

for(col in names(X_train_num)) {
  if(is.factor(X_train_num[[col]])) {
    X_train_num[[col]] <- as.numeric(X_train_num[[col]])
    X_test_num[[col]] <- as.numeric(X_test_num[[col]])
  }
}

X_train_xgb <- scale(as.matrix(X_train_num))
X_test_xgb <- scale(as.matrix(X_test_num), 
                    center = attr(X_train_xgb, "scaled:center"),
                    scale = attr(X_train_xgb, "scaled:scale"))

y_train_num <- as.numeric(y_train) - 1
y_test_num <- as.numeric(y_test) - 1

dtrain <- xgb.DMatrix(X_train_xgb, label = y_train_num)
dtest <- xgb.DMatrix(X_test_xgb, label = y_test_num)

# Train XGBoost with cross-validation
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1
)

set.seed(123)
xgb_cv <- xgb.cv(params, dtrain, nrounds = 100, nfold = 5, 
                 early_stopping_rounds = 10, verbose = 0)
best_nrounds <- xgb_cv$best_iteration

xgb_model <- xgb.train(params, dtrain, nrounds = best_nrounds, verbose = 0)

# Predictions
xgb_pred_prob <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)
xgb_cm <- table(factor(xgb_pred_class, 0:1), factor(y_test_num, 0:1))
xgb_metrics <- calculate_metrics(xgb_cm, xgb_pred_prob, y_test_num)

cat("\nXGBoost Performance Metrics:\n")
xgb_results <- data.frame(Metric = names(xgb_metrics), Value = round(xgb_metrics, 4))
print(xgb_results)

cat("\nConfusion Matrix:\n")
print(xgb_cm)

# Feature Importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X_train_xgb))
cat("\nXGBoost Feature Importance (Ranked by Gain):\n")
print(head(importance_matrix[, c("Feature", "Gain", "Cover")], 10))

# ==================== SHAP ANALYSIS ====================
cat("\n", rep("=", 70), "\n")
cat("SHAP ANALYSIS\n")
cat(rep("=", 70), "\n")

# Calculate SHAP values
shap_obj <- shapviz(xgb_model, X_pred = X_test_xgb, X = X_train_xgb[1:min(50, nrow(X_train_xgb)), ])

# SHAP importance
shap_importance <- data.frame(
  Feature = names(shap_obj$S$mean),
  Mean_SHAP = as.numeric(shap_obj$S$mean),
  Direction = ifelse(shap_obj$S$mean > 0, "Positive (towards Very Strong)", "Negative (towards Strong)")
) %>% arrange(desc(Mean_SHAP))

cat("\nVariable-Wise SHAP Analysis (Predictive Contributions to Pain Intensity):\n")
cat(sprintf("%-30s %-20s %-30s\n", "Feature", "Mean SHAP", "Direction"))
cat(rep("-", 80), "\n")
for(i in 1:nrow(shap_importance)) {
  cat(sprintf("%-30s %-20.4f %-30s\n", 
              substr(shap_importance$Feature[i], 1, 30), 
              shap_importance$Mean_SHAP[i], 
              shap_importance$Direction[i]))
}

# SHAP summary bar plot
bar_plot <- sv_importance(shap_obj, kind = "bar", max_display = 15) +
  ggtitle("Figure: SHAP Feature Importance for Pain Intensity") +
  theme_minimal()
print(bar_plot)

# SHAP beeswarm plot
beeswarm_plot <- sv_importance(shap_obj, kind = "both", max_display = 15) +
  ggtitle("SHAP Feature Importance: Bar + Beeswarm")
print(beeswarm_plot)

# SHAP dependence plots for top 6 features
top_features <- shap_importance$Feature[1:min(6, nrow(shap_importance))]

cat("\nGenerating SHAP Dependence Plots for Top 6 Predictors...\n")
for(feature in top_features) {
  dep_plot <- sv_dependence(shap_obj, v = feature, color_var = "auto") +
    ggtitle(paste("SHAP Dependence Plot:", feature)) +
    ylab("SHAP Value (Contribution to Pain Intensity)") +
    theme_minimal()
  print(dep_plot)
}

# Waterfall plot for first observation
waterfall_plot <- sv_waterfall(shap_obj, row_id = 1, max_display = 10) +
  ggtitle(paste("SHAP Waterfall Plot - Patient 1 (Prediction:", 
                round(xgb_pred_prob[1], 3), "->", 
                ifelse(xgb_pred_prob[1] > 0.5, "Very Strong", "Strong"), ")"))
print(waterfall_plot)

# ==================== MODEL COMPARISON ====================
cat("\n", rep("=", 70), "\n")
cat("MODEL COMPARISON\n")
cat(rep("=", 70), "\n")

comparison_df <- data.frame(
  Metric = c("Accuracy", "AUC", "Sensitivity", "Specificity", "PPV", "NPV"),
  RandomForest = round(rf_metrics, 4),
  XGBoost = round(xgb_metrics, 4)
)
print(comparison_df)

# ROC Curves
rf_roc <- roc(y_test_num, rf_pred_prob)
xgb_roc <- roc(y_test_num, xgb_pred_prob)

plot(rf_roc, col = "blue", lwd = 2, 
     main = "ROC Curves: Random Forest vs XGBoost\nPredicting Very Strong Pain",
     xlab = "1 - Specificity", ylab = "Sensitivity")
lines(xgb_roc, col = "red", lwd = 2)
legend("bottomright", 
       legend = c(paste("Random Forest (AUC =", round(rf_metrics["AUC"], 3), ")"),
                  paste("XGBoost (AUC =", round(xgb_metrics["AUC"], 3), ")")),
       col = c("blue", "red"), lwd = 2)

# ==================== SAVE RESULTS ====================
write.csv(rf_importance, "RF_Importance.csv", row.names = FALSE)
write.csv(importance_matrix, "XGB_Importance.csv", row.names = FALSE)
write.csv(shap_importance, "SHAP_Importance.csv", row.names = FALSE)
write.csv(comparison_df, "Model_Comparison.csv", row.names = FALSE)

# Final Summary
cat("\n", rep("=", 70), "\n")
cat("FINAL SUMMARY\n")
cat(rep("=", 70), "\n")

cat("\nDataset:\n")
cat("- Total observations:", nrow(data), "\n")
cat("- Complete cases:", nrow(X), "\n")
cat("- Strong Pain:", sum(y == "Strong"), "\n")
cat("- Very Strong Pain:", sum(y == "Very Strong"), "\n")

cat("\nModel Performance:\n")
cat("- Best model by AUC:", ifelse(rf_metrics["AUC"] > xgb_metrics["AUC"], "Random Forest", "XGBoost"), "\n")
cat("- Best model by Accuracy:", ifelse(rf_metrics["Accuracy"] > xgb_metrics["Accuracy"], "Random Forest", "XGBoost"), "\n")

cat("\nTop 3 Predictors (Random Forest - MDA):\n")
print(head(rf_importance$Feature, 3))

cat("\nTop 3 Predictors (XGBoost - Gain):\n")
print(head(importance_matrix$Feature, 3))

cat("\nTop 3 Predictors (SHAP):\n")
print(head(shap_importance$Feature, 3))

cat("\nResults saved to CSV files.\n")
write.csv(importance_matrix, "XGB_Importance.csv", row.names = FALSE)
write.csv(shap_importance, "SHAP_Importance.csv", row.names = FALSE)
write.csv(comparison_table, "Model_Comparison.csv", row.names = FALSE)

# Create summary report
sink("Analysis_Summary.txt")
cat("===== RANDOM FOREST & XGBOOST ANALYSIS SUMMARY =====\n\n")
cat("Dataset Information:\n")
cat("- Total observations:", nrow(data), "\n")
cat("- Features used:", length(features), "\n")
cat("- Training set:", nrow(X_train), "\n")
cat("- Test set:", nrow(X_test), "\n\n")

cat("Class Distribution:\n")
print(table(y))
cat("\n")

cat("RANDOM FOREST PERFORMANCE:\n")
print(rf_results)
cat("\nTOP 5 RF PREDICTORS (MDA):\n")
print(head(rf_importance, 5))
cat("\n")

cat("XGBOOST PERFORMANCE:\n")
print(xgb_results)
cat("\nTOP 5 XGB PREDICTORS (Gain):\n")
print(head(importance_matrix[, c("Feature", "Gain")], 5))
cat("\n")

cat("TOP 5 SHAP PREDICTORS:\n")
print(head(shap_importance, 5))
cat("\n")

cat("Best Model:", ifelse(rf_metrics["AUC"] > xgb_metrics["AUC"], "Random Forest", "XGBoost"), "\n")
sink()

cat("\n", rep("=", 70), "\n")
cat("ANALYSIS COMPLETE!\n")
cat(rep("=", 70), "\n")
cat("\nOutput files saved:\n")
cat("- RF_Importance.csv / RF_Importance.html\n")
cat("- XGB_Importance.csv / XGB_Importance.html\n")
cat("- SHAP_Importance.csv / SHAP_Bar_Plot.html / SHAP_Beeswarm.html\n")
cat("- Model_Comparison.csv / ROC_Comparison.html\n")
cat("- Analysis_Summary.txt\n")







# Load required libraries
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)
library(shapviz)

# Read the data
data <- read.table(text = "
Height Weight Gender Age_Group Marital_Status Pain_Intensity Injury Numbness Difficulty_in_Movement howmanymealsdoyouhaveeveryday Meals Exercise_Time Exercise_Routine Employment SittingTime_on_Chair Location No_of_Sites
170 85 1 4 2 2 2 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
146 75 2 4 2 2 1 2 1 3 3 4 4 4 3 'L3 & L4 - L5 & L4 - S1' 3
176 92 1 2 2 2 1 1 1 2 2 3 1 2 3 'L4 - L5 & L5' 2
160 80 2 4 2 2 2 1 1 2 2 4 4 4 3 'L3-L4 & L4 - L5 & L5- S1' 3
100 75 1 1 2 1 1 2 2 3 3 2 1 4 2 'L4 - L5' 1
182 79 1 2 1 1 1 1 1 3 3 2 2 2 3 'L4 - L5' 1
145 78 2 4 2 1 2 2 1 2 2 4 4 4 2 'L5- S1' 1
157 72 2 1 1 2 2 2 1 2 2 4 4 4 3 'L4 - L5' 1
171 80 2 2 2 1 1 2 1 2 2 4 4 4 2 'L4 - L5' 1
154 83 1 4 2 2 1 1 1 3 3 4 4 4 3 'L3 -4-5- S1' 2
166 70 2 2 2 2 1 2 1 3 3 2 1 1 3 'L4 - L5' 1
149 80 2 4 2 2 2 2 1 2 2 4 4 4 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
167 49 1 2 1 2 1 1 1 4 4 2 1 4 2 'L4 - L5' 1
152 54 2 2 1 1 1 1 1 3 3 2 2 1 2 'L5- S1' 1
170 108 1 2 2 2 2 2 2 4 4 4 1 4 1 'L4 - L5 & L5- S1' 2
155 60 1 2 2 1 1 2 1 3 3 2 1 2 2 'L4 - L5' 1
174 80 1 2 2 2 2 2 1 2 2 3 2 4 2 'L3 - L4 & L4 - L5' 2
164 61 1 2 2 1 1 1 1 2 2 2 1 1 2 'L4 - L5' 1
182 72 1 2 1 1 1 2 1 2 2 2 1 3 3 'L5- S1' 1
175 73 2 2 1 2 2 1 1 3 3 1 2 2 3 'L4 - L5 & L5- S1' 2
153 89 2 4 2 2 1 2 2 3 3 3 1 3 2 'L4 - L5 & L5' 2
171 94 1 3 2 2 1 1 1 3 3 4 1 4 2 'L3 -4-5- S1' 2
163 81 2 3 2 2 1 2 1 2 2 4 4 4 3 'L4 - L5' 1
153 81 2 3 2 1 2 1 1 3 3 4 4 4 3 'L5- S1' 1
164 93 2 4 2 2 1 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
173 75 1 1 1 2 1 2 1 2 2 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 63 2 2 1 2 1 2 1 3 3 2 1 4 2 'L3 - L4 & L4 - L5' 2
155 110 2 1 1 1 2 2 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
152 75 1 4 2 2 2 2 1 3 3 3 1 1 2 'L4 - L5 & L5- S1' 2
151 91 2 4 2 1 2 1 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
188 92 1 2 2 2 2 1 1 3 3 2 1 1 3 'L4 - L5' 1
161 86 1 2 2 2 1 2 2 2 2 4 1 1 2 'L4 - L5 & L5- S1' 2
160 78 2 3 2 2 2 2 1 3 3 4 4 2 3 'L4 - L5 & L5- S1' 2
173 91 1 3 2 2 1 1 1 2 2 4 4 1 2 'L4 - L5 & L5- S1' 2
170 65 1 2 2 2 1 2 2 2 2 4 1 2 2 'L4 - L5' 1
150 83 2 3 2 1 2 1 1 3 3 4 4 1 2 'L4 - L5 & L5- S1' 2
160 73 1 3 2 2 2 2 1 2 2 3 1 3 2 'L5- S1' 1
162 56 1 3 2 2 1 1 1 4 4 2 1 4 1 'L3-L4 & L4 - L5' 2
170 81 2 3 2 2 1 1 1 3 3 2 2 2 2 'L3-L4 & L4 - L5' 2
171 54 1 2 2 1 1 1 2 3 3 4 1 4 2 'L3-L4 & L4- L5 & L5- S1' 3
150 91 1 2 1 2 2 1 1 2 2 4 1 4 3 'L1 - L2 & L4- L5' 2
176 89 1 2 1 2 2 2 1 2 2 3 1 1 2 'L2-3-4-5-S1' 2
171 77 1 1 1 2 1 2 1 3 3 1 1 4 3 'L2-3-4-5-S1' 2
156 72 1 4 2 2 1 1 1 2 2 4 4 4 3 'L4 - S1' 1
152 82 2 4 2 2 1 2 1 3 3 4 4 4 3 'L4 - L5' 1
166 64 2 1 1 1 1 2 1 3 3 2 2 4 3 'L3-L4 & L4- L5' 2
162 53 2 1 2 1 2 1 1 3 3 2 1 4 2 'L4- L5 & L5- S1' 2
164 87 1 4 2 2 2 1 1 1 1 2 4 4 3 'L4 - L5 & L5- S1' 2
142 53 2 3 2 2 2 2 1 3 3 3 1 4 3 'L4 - L5' 1
156 75 1 3 2 1 1 1 1 2 2 3 1 4 3 'L2 - L3 & L2 - L3' 2
145 85 2 4 2 2 2 1 1 3 3 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
179 115 1 2 2 2 2 1 1 3 3 4 4 3 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
180 86 2 1 2 1 1 2 2 2 2 2 2 2 2 'L4 - L5 & L5- S1' 2
170 72 1 2 2 2 2 1 1 3 3 2 2 2 3 'L4- L5 & L5- S1' 2
167 85 1 2 1 2 1 1 1 2 2 3 1 1 3 'L3 - L4 & L4 - L5' 2
150 55 2 2 2 2 2 1 1 2 2 4 4 4 2 'L4 - L5 & L5- S1' 2
160 74 1 3 2 2 1 2 1 2 2 3 1 3 2 'L3 -4-5- S1' 2
156 77 2 4 2 2 1 1 1 3 3 4 4 4 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 83 1 4 2 2 2 2 1 3 3 4 4 1 2 'L3-L4 & L4 - L5' 2
173 155 1 1 1 2 2 2 1 2 2 4 1 1 2 'L3 - L4 & L4 - L5 & L5- S1' 3
164 78 1 2 2 2 2 2 1 3 3 3 1 3 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
", header = TRUE, sep = " ", quote = "'")

# Apply value labels
data$Gender <- factor(data$Gender, levels = c(1,2), labels = c("Male", "Female"))
data$Age_Group <- factor(data$Age_Group, levels = c(1,2,3,4), labels = c("20-29", "30-39", "40-49", "50-59"))
data$Marital_Status <- factor(data$Marital_Status, levels = c(1,2), labels = c("Single", "Married"))
data$Pain_Intensity <- factor(data$Pain_Intensity, levels = c(1,2), labels = c("Strong", "Very Strong"))
data$Injury <- factor(data$Injury, levels = c(1,2), labels = c("Yes", "No"))
data$Numbness <- factor(data$Numbness, levels = c(1,2), labels = c("Yes", "No"))
data$Difficulty_in_Movement <- factor(data$Difficulty_in_Movement, levels = c(1,2), labels = c("Yes", "No"))
data$Exercise_Time <- factor(data$Exercise_Time, levels = c(1,2,3,4), 
                             labels = c("Every day", "Once a week", "Three times a week", "I do not exercise"))
data$Exercise_Routine <- factor(data$Exercise_Routine, levels = c(1,2,3), 
                                labels = c("Running", "Walking", "Other"))
data$Employment <- factor(data$Employment, levels = c(1,2,3,4), 
                          labels = c("Education", "Health care", "Physical activity", "Other"))
data$SittingTime_on_Chair <- factor(data$SittingTime_on_Chair, levels = c(1,2,3), 
                                    labels = c("<3 hours", "3-6 hours", "6-9 hours"))

# DV: Pain_Intensity (Strong vs Very Strong)
y <- data$Pain_Intensity

# Select features
features <- c("Height", "Weight", "Gender", "Age_Group", "Marital_Status", 
              "Injury", "Numbness", "Difficulty_in_Movement", "Meals", 
              "Exercise_Time", "Exercise_Routine", "Employment", 
              "SittingTime_on_Chair", "No_of_Sites")

X <- data[, features]

# Convert categorical variables to factors properly
for(col in names(X)) {
  if(col %in% c("Gender", "Age_Group", "Marital_Status", "Injury", "Numbness", 
                "Difficulty_in_Movement", "Exercise_Time", "Exercise_Routine", 
                "Employment", "SittingTime_on_Chair")) {
    X[[col]] <- as.factor(as.character(X[[col]]))
  }
}

# Remove rows with NAs
complete_cases <- complete.cases(X, y)
X <- X[complete_cases, ]
y <- y[complete_cases]

cat("Dataset Dimensions:", dim(X), "\n")
cat("\nPain Intensity Distribution:\n")
print(table(y))

# Split data
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

cat("\nTraining set:", nrow(X_train), "observations")
cat("\nTest set:", nrow(X_test), "observations\n")

# Function to calculate metrics
calculate_metrics <- function(cm, probs = NULL, true_labels = NULL) {
  TP <- cm[2,2]; TN <- cm[1,1]; FP <- cm[1,2]; FN <- cm[2,1]
  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  auc <- if(!is.null(probs)) roc(true_labels, probs, quiet = TRUE)$auc else NA
  return(c(Accuracy = accuracy, AUC = auc, Sensitivity = sensitivity, 
           Specificity = specificity, PPV = ppv, NPV = npv))
}

# ==================== RANDOM FOREST ====================
cat("\n", rep("=", 70), "\n")
cat("RANDOM FOREST ANALYSIS\n")
cat(rep("=", 70), "\n")

set.seed(123)
rf_model <- randomForest(x = X_train, y = y_train, ntree = 500, importance = TRUE,
                         mtry = floor(sqrt(ncol(X_train))))

# Predictions
rf_pred_class <- predict(rf_model, X_test)
rf_pred_prob <- predict(rf_model, X_test, type = "prob")[,2]
rf_cm <- table(rf_pred_class, y_test)
rf_metrics <- calculate_metrics(rf_cm, rf_pred_prob, as.numeric(y_test))

cat("\nRandom Forest Performance:\n")
rf_results <- data.frame(Metric = names(rf_metrics), Value = round(rf_metrics, 4))
print(rf_results)

cat("\nConfusion Matrix:\n")
print(rf_cm)

# Variable Importance
rf_importance <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>% arrange(desc(MeanDecreaseAccuracy))

cat("\nRandom Forest Variable Importance (Top 10 by MDA):\n")
print(head(rf_importance, 10))

# ==================== XGBOOST (FIXED) ====================
cat("\n", rep("=", 70), "\n")
cat("XGBOOST ANALYSIS\n")
cat(rep("=", 70), "\n")

# Prepare data for XGBoost
X_train_num <- X_train
X_test_num <- X_test

for(col in names(X_train_num)) {
  if(is.factor(X_train_num[[col]])) {
    X_train_num[[col]] <- as.numeric(X_train_num[[col]])
    X_test_num[[col]] <- as.numeric(X_test_num[[col]])
  }
}

X_train_xgb <- as.matrix(X_train_num)
X_test_xgb <- as.matrix(X_test_num)

# Scale features
X_train_xgb <- scale(X_train_xgb)
X_test_xgb <- scale(X_test_xgb, center = attr(X_train_xgb, "scaled:center"),
                    scale = attr(X_train_xgb, "scaled:scale"))

y_train_num <- as.numeric(y_train) - 1
y_test_num <- as.numeric(y_test) - 1

dtrain <- xgb.DMatrix(X_train_xgb, label = y_train_num)
dtest <- xgb.DMatrix(X_test_xgb, label = y_test_num)

# XGBoost parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1
)

# Train without CV (use fixed nrounds to avoid error)
set.seed(123)
nrounds <- 50
xgb_model <- xgb.train(params, dtrain, nrounds = nrounds, verbose = 0)

# Predictions
xgb_pred_prob <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)
xgb_cm <- table(factor(xgb_pred_class, 0:1), factor(y_test_num, 0:1))
xgb_metrics <- calculate_metrics(xgb_cm, xgb_pred_prob, y_test_num)

cat("\nXGBoost Performance:\n")
xgb_results <- data.frame(Metric = names(xgb_metrics), Value = round(xgb_metrics, 4))
print(xgb_results)

cat("\nConfusion Matrix:\n")
print(xgb_cm)

# Feature Importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X_train_xgb))
cat("\nXGBoost Feature Importance (Ranked by Gain):\n")
print(head(importance_matrix[, c("Feature", "Gain", "Cover")], 10))

# ==================== SHAP ANALYSIS (FIXED) ====================
cat("\n", rep("=", 70), "\n")
cat("SHAP ANALYSIS\n")
cat(rep("=", 70), "\n")

# Calculate SHAP values - ensure dimensions match
# Use a subset of training data as background
set.seed(123)
bg_indices <- sample(1:nrow(X_train_xgb), min(30, nrow(X_train_xgb)))
shap_obj <- shapviz(xgb_model, X_pred = X_test_xgb, X = X_train_xgb[bg_indices, ])

# SHAP importance
shap_importance <- data.frame(
  Feature = names(shap_obj$S$mean),
  Mean_SHAP = as.numeric(shap_obj$S$mean),
  Direction = ifelse(shap_obj$S$mean > 0, "Positive (towards Very Strong)", "Negative (towards Strong)")
) %>% arrange(desc(Mean_SHAP))

cat("\nVariable-Wise SHAP Analysis (Predictive Contributions to Pain Intensity):\n")
cat(sprintf("%-35s %-20s %-35s\n", "Feature", "Mean SHAP", "Direction"))
cat(rep("-", 90), "\n")
for(i in 1:min(15, nrow(shap_importance))) {
  cat(sprintf("%-35s %-20.4f %-35s\n", 
              substr(shap_importance$Feature[i], 1, 35), 
              shap_importance$Mean_SHAP[i], 
              shap_importance$Direction[i]))
}

# SHAP summary bar plot
bar_plot <- sv_importance(shap_obj, kind = "bar", max_display = 15) +
  ggtitle("Figure: SHAP Feature Importance for Pain Intensity") +
  theme_minimal()
print(bar_plot)

# SHAP beeswarm plot
beeswarm_plot <- sv_importance(shap_obj, kind = "both", max_display = 15) +
  ggtitle("SHAP Feature Importance: Bar + Beeswarm")
print(beeswarm_plot)

# SHAP dependence plots for top 6 features
top_features <- shap_importance$Feature[1:min(6, nrow(shap_importance))]

cat("\nGenerating SHAP Dependence Plots for Top 6 Predictors...\n")
for(feature in top_features) {
  if(feature %in% colnames(X_test_xgb)) {
    dep_plot <- sv_dependence(shap_obj, v = feature, color_var = "auto") +
      ggtitle(paste("SHAP Dependence Plot:", feature)) +
      ylab("SHAP Value (Contribution to Pain Intensity)") +
      theme_minimal()
    print(dep_plot)
  }
}

# Waterfall plot for first observation
waterfall_plot <- sv_waterfall(shap_obj, row_id = 1, max_display = 10) +
  ggtitle(paste("SHAP Waterfall Plot - Patient 1\nPrediction:", 
                round(xgb_pred_prob[1], 3), "->", 
                ifelse(xgb_pred_prob[1] > 0.5, "Very Strong", "Strong")))
print(waterfall_plot)

# ==================== MODEL COMPARISON ====================
cat("\n", rep("=", 70), "\n")
cat("MODEL COMPARISON\n")
cat(rep("=", 70), "\n")

comparison_df <- data.frame(
  Metric = c("Accuracy", "AUC", "Sensitivity", "Specificity", "PPV", "NPV"),
  RandomForest = round(rf_metrics, 4),
  XGBoost = round(xgb_metrics, 4)
)
print(comparison_df)

# ROC Curves
if(!is.na(rf_metrics["AUC"]) & !is.na(xgb_metrics["AUC"])) {
  rf_roc <- roc(y_test_num, rf_pred_prob)
  xgb_roc <- roc(y_test_num, xgb_pred_prob)
  
  plot(rf_roc, col = "blue", lwd = 2, 
       main = "ROC Curves: Random Forest vs XGBoost\nPredicting Very Strong Pain",
       xlab = "1 - Specificity", ylab = "Sensitivity")
  lines(xgb_roc, col = "red", lwd = 2)
  legend("bottomright", 
         legend = c(paste("Random Forest (AUC =", round(rf_metrics["AUC"], 3), ")"),
                    paste("XGBoost (AUC =", round(xgb_metrics["AUC"], 3), ")")),
         col = c("blue", "red"), lwd = 2)
}

# ==================== SAVE RESULTS ====================
write.csv(rf_importance, "RF_Importance.csv", row.names = FALSE)
write.csv(importance_matrix, "XGB_Importance.csv", row.names = FALSE)
write.csv(shap_importance, "SHAP_Importance.csv", row.names = FALSE)
write.csv(comparison_df, "Model_Comparison.csv", row.names = FALSE)

# Final Summary
cat("\n", rep("=", 70), "\n")
cat("FINAL SUMMARY\n")
cat(rep("=", 70), "\n")

cat("\nDataset:\n")
cat("- Total observations:", nrow(data), "\n")
cat("- Complete cases:", nrow(X), "\n")
cat("- Strong Pain:", sum(y == "Strong"), "\n")
cat("- Very Strong Pain:", sum(y == "Very Strong"), "\n")

cat("\nModel Performance:\n")
cat("- Random Forest AUC:", round(rf_metrics["AUC"], 4), "\n")
cat("- XGBoost AUC:", round(xgb_metrics["AUC"], 4), "\n")
cat("- Best model by AUC:", ifelse(rf_metrics["AUC"] > xgb_metrics["AUC"], "Random Forest", "XGBoost"), "\n")

cat("\nTop 3 Predictors (Random Forest - MDA):\n")
print(head(rf_importance$Feature, 3))

cat("\nTop 3 Predictors (XGBoost - Gain):\n")
print(head(importance_matrix$Feature, 3))

cat("\nTop 3 Predictors (SHAP):\n")
print(head(shap_importance$Feature, 3))

cat("\nResults saved to CSV files.\n")





































# Load required libraries
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)

# Read the data
data <- read.table(text = "
Height Weight Gender Age_Group Marital_Status Pain_Intensity Injury Numbness Difficulty_in_Movement howmanymealsdoyouhaveeveryday Meals Exercise_Time Exercise_Routine Employment SittingTime_on_Chair Location No_of_Sites
170 85 1 4 2 2 2 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
146 75 2 4 2 2 1 2 1 3 3 4 4 4 3 'L3 & L4 - L5 & L4 - S1' 3
176 92 1 2 2 2 1 1 1 2 2 3 1 2 3 'L4 - L5 & L5' 2
160 80 2 4 2 2 2 1 1 2 2 4 4 4 3 'L3-L4 & L4 - L5 & L5- S1' 3
100 75 1 1 2 1 1 2 2 3 3 2 1 4 2 'L4 - L5' 1
182 79 1 2 1 1 1 1 1 3 3 2 2 2 3 'L4 - L5' 1
145 78 2 4 2 1 2 2 1 2 2 4 4 4 2 'L5- S1' 1
157 72 2 1 1 2 2 2 1 2 2 4 4 4 3 'L4 - L5' 1
171 80 2 2 2 1 1 2 1 2 2 4 4 4 2 'L4 - L5' 1
154 83 1 4 2 2 1 1 1 3 3 4 4 4 3 'L3 -4-5- S1' 2
166 70 2 2 2 2 1 2 1 3 3 2 1 1 3 'L4 - L5' 1
149 80 2 4 2 2 2 2 1 2 2 4 4 4 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
167 49 1 2 1 2 1 1 1 4 4 2 1 4 2 'L4 - L5' 1
152 54 2 2 1 1 1 1 1 3 3 2 2 1 2 'L5- S1' 1
170 108 1 2 2 2 2 2 2 4 4 4 1 4 1 'L4 - L5 & L5- S1' 2
155 60 1 2 2 1 1 2 1 3 3 2 1 2 2 'L4 - L5' 1
174 80 1 2 2 2 2 2 1 2 2 3 2 4 2 'L3 - L4 & L4 - L5' 2
164 61 1 2 2 1 1 1 1 2 2 2 1 1 2 'L4 - L5' 1
182 72 1 2 1 1 1 2 1 2 2 2 1 3 3 'L5- S1' 1
175 73 2 2 1 2 2 1 1 3 3 1 2 2 3 'L4 - L5 & L5- S1' 2
153 89 2 4 2 2 1 2 2 3 3 3 1 3 2 'L4 - L5 & L5' 2
171 94 1 3 2 2 1 1 1 3 3 4 1 4 2 'L3 -4-5- S1' 2
163 81 2 3 2 2 1 2 1 2 2 4 4 4 3 'L4 - L5' 1
153 81 2 3 2 1 2 1 1 3 3 4 4 4 3 'L5- S1' 1
164 93 2 4 2 2 1 1 1 3 3 4 4 4 3 'L4 - L5 & L5- S1' 2
173 75 1 1 1 2 1 2 1 2 2 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 63 2 2 1 2 1 2 1 3 3 2 1 4 2 'L3 - L4 & L4 - L5' 2
155 110 2 1 1 1 2 2 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
152 75 1 4 2 2 2 2 1 3 3 3 1 1 2 'L4 - L5 & L5- S1' 2
151 91 2 4 2 1 2 1 1 2 2 4 4 4 3 'L4 - L5 & L5- S1' 2
188 92 1 2 2 2 2 1 1 3 3 2 1 1 3 'L4 - L5' 1
161 86 1 2 2 2 1 2 2 2 2 4 1 1 2 'L4 - L5 & L5- S1' 2
160 78 2 3 2 2 2 2 1 3 3 4 4 2 3 'L4 - L5 & L5- S1' 2
173 91 1 3 2 2 1 1 1 2 2 4 4 1 2 'L4 - L5 & L5- S1' 2
170 65 1 2 2 2 1 2 2 2 2 4 1 2 2 'L4 - L5' 1
150 83 2 3 2 1 2 1 1 3 3 4 4 1 2 'L4 - L5 & L5- S1' 2
160 73 1 3 2 2 2 2 1 2 2 3 1 3 2 'L5- S1' 1
162 56 1 3 2 2 1 1 1 4 4 2 1 4 1 'L3-L4 & L4 - L5' 2
170 81 2 3 2 2 1 1 1 3 3 2 2 2 2 'L3-L4 & L4 - L5' 2
171 54 1 2 2 1 1 1 2 3 3 4 1 4 2 'L3-L4 & L4- L5 & L5- S1' 3
150 91 1 2 1 2 2 1 1 2 2 4 1 4 3 'L1 - L2 & L4- L5' 2
176 89 1 2 1 2 2 2 1 2 2 3 1 1 2 'L2-3-4-5-S1' 2
171 77 1 1 1 2 1 2 1 3 3 1 1 4 3 'L2-3-4-5-S1' 2
156 72 1 4 2 2 1 1 1 2 2 4 4 4 3 'L4 - S1' 1
152 82 2 4 2 2 1 2 1 3 3 4 4 4 3 'L4 - L5' 1
166 64 2 1 1 1 1 2 1 3 3 2 2 4 3 'L3-L4 & L4- L5' 2
162 53 2 1 2 1 2 1 1 3 3 2 1 4 2 'L4- L5 & L5- S1' 2
164 87 1 4 2 2 2 1 1 1 1 2 4 4 3 'L4 - L5 & L5- S1' 2
142 53 2 3 2 2 2 2 1 3 3 3 1 4 3 'L4 - L5' 1
156 75 1 3 2 1 1 1 1 2 2 3 1 4 3 'L2 - L3 & L2 - L3' 2
145 85 2 4 2 2 2 1 1 3 3 4 4 4 2 'L3 - L4 & L4 - L5 & L5 - S1' 3
179 115 1 2 2 2 2 1 1 3 3 4 4 3 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
180 86 2 1 2 1 1 2 2 2 2 2 2 2 2 'L4 - L5 & L5- S1' 2
170 72 1 2 2 2 2 1 1 3 3 2 2 2 3 'L4- L5 & L5- S1' 2
167 85 1 2 1 2 1 1 1 2 2 3 1 1 3 'L3 - L4 & L4 - L5' 2
150 55 2 2 2 2 2 1 1 2 2 4 4 4 2 'L4 - L5 & L5- S1' 2
160 74 1 3 2 2 1 2 1 2 2 3 1 3 2 'L3 -4-5- S1' 2
156 77 2 4 2 2 1 1 1 3 3 4 4 4 3 'L3 - L4 & L4 - L5 & L5 - S1' 3
154 83 1 4 2 2 2 2 1 3 3 4 4 1 2 'L3-L4 & L4 - L5' 2
173 155 1 1 1 2 2 2 1 2 2 4 1 1 2 'L3 - L4 & L4 - L5 & L5- S1' 3
164 78 1 2 2 2 2 2 1 3 3 3 1 3 2 'L2 - L3 & L3-L4 & L4 - L5 & L5- S1' 4
", header = TRUE, sep = " ", quote = "'")

# Apply value labels
data$Gender <- factor(data$Gender, levels = c(1,2), labels = c("Male", "Female"))
data$Age_Group <- factor(data$Age_Group, levels = c(1,2,3,4), labels = c("20-29", "30-39", "40-49", "50-59"))
data$Marital_Status <- factor(data$Marital_Status, levels = c(1,2), labels = c("Single", "Married"))
data$Pain_Intensity <- factor(data$Pain_Intensity, levels = c(1,2), labels = c("Strong", "Very Strong"))
data$Injury <- factor(data$Injury, levels = c(1,2), labels = c("Yes", "No"))
data$Numbness <- factor(data$Numbness, levels = c(1,2), labels = c("Yes", "No"))
data$Difficulty_in_Movement <- factor(data$Difficulty_in_Movement, levels = c(1,2), labels = c("Yes", "No"))
data$Exercise_Time <- factor(data$Exercise_Time, levels = c(1,2,3,4), 
                             labels = c("Every day", "Once a week", "Three times a week", "I do not exercise"))
data$Exercise_Routine <- factor(data$Exercise_Routine, levels = c(1,2,3), 
                                labels = c("Running", "Walking", "Other"))
data$Employment <- factor(data$Employment, levels = c(1,2,3,4), 
                          labels = c("Education", "Health care", "Physical activity", "Other"))
data$SittingTime_on_Chair <- factor(data$SittingTime_on_Chair, levels = c(1,2,3), 
                                    labels = c("<3 hours", "3-6 hours", "6-9 hours"))

# DV: Pain_Intensity
y <- data$Pain_Intensity

# Select features
features <- c("Height", "Weight", "Gender", "Age_Group", "Marital_Status", 
              "Injury", "Numbness", "Difficulty_in_Movement", "Meals", 
              "Exercise_Time", "Exercise_Routine", "Employment", 
              "SittingTime_on_Chair", "No_of_Sites")

X <- data[, features]

# Convert categorical variables to factors
for(col in names(X)) {
  if(col %in% c("Gender", "Age_Group", "Marital_Status", "Injury", "Numbness", 
                "Difficulty_in_Movement", "Exercise_Time", "Exercise_Routine", 
                "Employment", "SittingTime_on_Chair")) {
    X[[col]] <- as.factor(as.character(X[[col]]))
  }
}

# Remove rows with NAs
complete_cases <- complete.cases(X, y)
X <- X[complete_cases, ]
y <- y[complete_cases]

cat("Dataset Dimensions:", dim(X), "\n")
cat("\nPain Intensity Distribution:\n")
print(table(y))

# Split data
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Function to calculate metrics
calculate_metrics <- function(cm, probs = NULL, true_labels = NULL) {
  TP <- cm[2,2]; TN <- cm[1,1]; FP <- cm[1,2]; FN <- cm[2,1]
  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  ppv <- TP / (TP + FP)
  npv <- TN / (TN + FN)
  auc <- if(!is.null(probs)) roc(true_labels, probs, quiet = TRUE)$auc else NA
  return(c(Accuracy = accuracy, AUC = auc, Sensitivity = sensitivity, 
           Specificity = specificity, PPV = ppv, NPV = npv))
}

# ==================== RANDOM FOREST ====================
cat("\n", rep("=", 70), "\n")
cat("RANDOM FOREST ANALYSIS\n")
cat(rep("=", 70), "\n")

set.seed(123)
rf_model <- randomForest(x = X_train, y = y_train, ntree = 500, importance = TRUE,
                         mtry = floor(sqrt(ncol(X_train))))

rf_pred_class <- predict(rf_model, X_test)
rf_pred_prob <- predict(rf_model, X_test, type = "prob")[,2]
rf_cm <- table(rf_pred_class, y_test)
rf_metrics <- calculate_metrics(rf_cm, rf_pred_prob, as.numeric(y_test))

cat("\nRandom Forest Performance:\n")
rf_results <- data.frame(Metric = names(rf_metrics), Value = round(rf_metrics, 4))
print(rf_results)

# Variable Importance
rf_importance <- data.frame(
  Feature = rownames(importance(rf_model)),
  MeanDecreaseAccuracy = importance(rf_model)[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = importance(rf_model)[, "MeanDecreaseGini"]
) %>% arrange(desc(MeanDecreaseAccuracy))

cat("\nRandom Forest Variable Importance (Top 10 by MDA):\n")
print(head(rf_importance, 10))

# ==================== XGBOOST ====================
cat("\n", rep("=", 70), "\n")
cat("XGBOOST ANALYSIS\n")
cat(rep("=", 70), "\n")

# Prepare data for XGBoost
X_train_num <- X_train
X_test_num <- X_test

for(col in names(X_train_num)) {
  if(is.factor(X_train_num[[col]])) {
    X_train_num[[col]] <- as.numeric(X_train_num[[col]])
    X_test_num[[col]] <- as.numeric(X_test_num[[col]])
  }
}

X_train_xgb <- as.matrix(X_train_num)
X_test_xgb <- as.matrix(X_test_num)

# Scale features
X_train_xgb <- scale(X_train_xgb)
X_test_xgb <- scale(X_test_xgb, center = attr(X_train_xgb, "scaled:center"),
                    scale = attr(X_train_xgb, "scaled:scale"))

y_train_num <- as.numeric(y_train) - 1
y_test_num <- as.numeric(y_test) - 1

dtrain <- xgb.DMatrix(X_train_xgb, label = y_train_num)
dtest <- xgb.DMatrix(X_test_xgb, label = y_test_num)

# Train XGBoost
params <- list(objective = "binary:logistic", eval_metric = "auc", 
               max_depth = 3, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8)
set.seed(123)
xgb_model <- xgb.train(params, dtrain, nrounds = 50, verbose = 0)

# Predictions
xgb_pred_prob <- predict(xgb_model, dtest)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)
xgb_cm <- table(factor(xgb_pred_class, 0:1), factor(y_test_num, 0:1))
xgb_metrics <- calculate_metrics(xgb_cm, xgb_pred_prob, y_test_num)

cat("\nXGBoost Performance:\n")
xgb_results <- data.frame(Metric = names(xgb_metrics), Value = round(xgb_metrics, 4))
print(xgb_results)

# Feature Importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X_train_xgb))
cat("\nXGBoost Feature Importance (Ranked by Gain):\n")
print(head(importance_matrix[, c("Feature", "Gain")], 10))

# ==================== ALTERNATIVE SHAP ANALYSIS (Using xgboost built-in) ====================
cat("\n", rep("=", 70), "\n")
cat("SHAP ANALYSIS (Alternative Method)\n")
cat(rep("=", 70), "\n")

# Use xgboost's built-in SHAP contributions
tryCatch({
  # Calculate SHAP contributions directly
  shap_contrib <- predict(xgb_model, X_test_xgb, predcontrib = TRUE)
  
  # Get SHAP values (excluding the bias term)
  if(is.matrix(shap_contrib)) {
    shap_values <- shap_contrib[, -ncol(shap_contrib)]
    bias_term <- shap_contrib[, ncol(shap_contrib)]
    
    # Calculate mean absolute SHAP values
    shap_importance_alt <- data.frame(
      Feature = colnames(shap_values),
      Mean_SHAP = colMeans(abs(shap_values)),
      Mean_SHAP_Signed = colMeans(shap_values)
    ) %>% arrange(desc(Mean_SHAP))
    
    cat("\nVariable-Wise SHAP Analysis (Predictive Contributions to Pain Intensity):\n")
    cat(sprintf("%-35s %-20s %-20s %-25s\n", "Feature", "Mean |SHAP|", "Mean SHAP", "Direction"))
    cat(rep("-", 100), "\n")
    
    for(i in 1:min(15, nrow(shap_importance_alt))) {
      direction <- ifelse(shap_importance_alt$Mean_SHAP_Signed[i] > 0, 
                          "Positive→Very Strong", "Negative→Strong")
      cat(sprintf("%-35s %-20.4f %-20.4f %-25s\n", 
                  substr(shap_importance_alt$Feature[i], 1, 35), 
                  shap_importance_alt$Mean_SHAP[i],
                  shap_importance_alt$Mean_SHAP_Signed[i],
                  direction))
    }
    
    # Create SHAP summary bar plot
    shap_plot_data <- head(shap_importance_alt, 10)
    shap_bar <- ggplot(shap_plot_data, aes(x = reorder(Feature, Mean_SHAP), y = Mean_SHAP)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Figure: SHAP Feature Importance for Pain Intensity",
           x = "Features", y = "Mean |SHAP value|") +
      theme_minimal()
    print(shap_bar)
    
    # Save SHAP results
    write.csv(shap_importance_alt, "SHAP_Importance.csv", row.names = FALSE)
    
  } else {
    cat("SHAP contribution matrix not available\n")
  }
  
}, error = function(e) {
  cat("SHAP calculation using predcontrib failed:", e$message, "\n")
  
  # Fallback: Use feature importance as proxy
  cat("\nUsing XGBoost feature importance as SHAP proxy:\n")
  print(head(importance_matrix[, c("Feature", "Gain")], 10))
  
  # Create importance plot
  imp_plot <- ggplot(head(importance_matrix, 10), aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "coral") +
    coord_flip() +
    labs(title = "XGBoost Feature Importance (Gain) - SHAP Proxy",
         x = "Features", y = "Gain") +
    theme_minimal()
  print(imp_plot)
})

# ==================== MODEL COMPARISON ====================
cat("\n", rep("=", 70), "\n")
cat("MODEL COMPARISON\n")
cat(rep("=", 70), "\n")

comparison_df <- data.frame(
  Metric = c("Accuracy", "AUC", "Sensitivity", "Specificity", "PPV", "NPV"),
  RandomForest = round(rf_metrics, 4),
  XGBoost = round(xgb_metrics, 4)
)
print(comparison_df)

# ROC Curves
if(!is.na(rf_metrics["AUC"]) & !is.na(xgb_metrics["AUC"])) {
  rf_roc <- roc(y_test_num, rf_pred_prob)
  xgb_roc <- roc(y_test_num, xgb_pred_prob)
  
  # Create ROC plot
  plot(rf_roc, col = "blue", lwd = 2, 
       main = "ROC Curves: Random Forest vs XGBoost\nPredicting Very Strong Pain",
       xlab = "1 - Specificity", ylab = "Sensitivity")
  lines(xgb_roc, col = "red", lwd = 2)
  legend("bottomright", 
         legend = c(paste("Random Forest (AUC =", round(rf_metrics["AUC"], 3), ")"),
                    paste("XGBoost (AUC =", round(xgb_metrics["AUC"], 3), ")")),
         col = c("blue", "red"), lwd = 2)
  
  # Create ggplot version
  roc_df <- data.frame(
    FPR = c(1 - rf_roc$specificities, 1 - xgb_roc$specificities),
    TPR = c(rf_roc$sensitivities, xgb_roc$sensitivities),
    Model = rep(c("Random Forest", "XGBoost"), c(length(rf_roc$sensitivities), length(xgb_roc$sensitivities)))
  )
  
  roc_plot <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
    geom_line(size = 1.2) +
    geom_abline(linetype = "dashed", alpha = 0.5) +
    labs(title = "ROC Curves Comparison",
         subtitle = paste("RF AUC =", round(rf_metrics["AUC"], 3), 
                          "| XGB AUC =", round(xgb_metrics["AUC"], 3))) +
    theme_minimal() +
    scale_color_manual(values = c("Random Forest" = "blue", "XGBoost" = "red"))
  print(roc_plot)
}

# ==================== SAVE RESULTS ====================
write.csv(rf_importance, "RF_Importance.csv", row.names = FALSE)
write.csv(importance_matrix, "XGB_Importance.csv", row.names = FALSE)
write.csv(comparison_df, "Model_Comparison.csv", row.names = FALSE)

# Final Summary
cat("\n", rep("=", 70), "\n")
cat("FINAL SUMMARY\n")
cat(rep("=", 70), "\n")

cat("\nDataset:\n")
cat("- Total observations:", nrow(data), "\n")
cat("- Complete cases:", nrow(X), "\n")
cat("- Strong Pain:", sum(y == "Strong"), "\n")
cat("- Very Strong Pain:", sum(y == "Very Strong"), "\n")
cat("- Proportion Very Strong:", round(sum(y == "Very Strong")/nrow(y), 3), "\n")

cat("\nModel Performance:\n")
cat("- Random Forest Accuracy:", round(rf_metrics["Accuracy"], 4), "\n")
cat("- Random Forest AUC:", round(rf_metrics["AUC"], 4), "\n")
cat("- XGBoost Accuracy:", round(xgb_metrics["Accuracy"], 4), "\n")
cat("- XGBoost AUC:", round(xgb_metrics["AUC"], 4), "\n")
cat("- Best model by AUC:", ifelse(rf_metrics["AUC"] > xgb_metrics["AUC"], "Random Forest", "XGBoost"), "\n")

cat("\nTop 5 Predictors (Random Forest - MDA):\n")
print(head(rf_importance$Feature, 5))

cat("\nTop 5 Predictors (XGBoost - Gain):\n")
print(head(importance_matrix$Feature, 5))

cat("\n\nResults saved to:\n")
cat("- RF_Importance.csv\n")
cat("- XGB_Importance.csv\n")
cat("- Model_Comparison.csv\n")
if(file.exists("SHAP_Importance.csv")) {
  cat("- SHAP_Importance.csv\n")
}








