# ==============================================================================
# MONTE CARLO PERMUTATION TEST (MCPT) - CHOP-AROON STRATEGIE
# Mit vollständigem Pyramiding & Advanced Exit Strategies  
# ==============================================================================

rm(list=ls())
gc()
options(scipen=999)

library(tidyverse)
library(lubridate)
library(TTR)
library(PerformanceAnalytics)
library(tictoc)
library(scales)
library(pracma)
library(data.table)

cat("\n")
cat("########################################################################\n")
cat("#                                                                      #\n")
cat("#      MCPT für Choppiness-Aroon Strategie mit Pyramiding            #\n")
cat("#      Mit Advanced Exit Strategies & Risk Management                 #\n")
cat("#                                                                      #\n")
cat("########################################################################\n\n")

# MCPT-Parameter
N_PERMUTATIONS <- 300
METRIC <- "profit_factor"

# Choppiness-Aroon Parameter
CHOP_PERIOD <- 14
CHOP_THRESHOLD <- 38.2
AROON_PERIOD <- 25
AROON_THRESHOLD <- 50

ATR_PERIOD <- 14
ADX_PERIOD <- 14
CHANDELIER_PERIOD <- 22
STOP_LOSS_ATR_MULT <- 3.0

USE_PYRAMIDING <- TRUE
PYRAMID_METHOD <- "breakout"
MAX_PYRAMID_ORDERS <- 3
PYRAMID_SPACING_ATR <- 0.5
PYRAMID_SIZE_MULTIPLIER <- 0.5
CONSECUTIVE_BARS <- 2
BREAKOUT_LOOKBACK <- 20

EXIT_STRATEGY <- "chandelier"
CHANDELIER_MULTIPLIER <- 3.0
BREAKEVEN_TRIGGER_ATR <- 1.5
BREAKEVEN_OFFSET_ATR <- 0.2
TRAILING_STOP_ATR_MULT <- 2.0
TRAILING_START_ATR_MULT <- 1.5

TP_STRATEGY <- "full"
FULL_TP_ATR_MULT <- 4.5
PARTIAL_TP_1_ATR <- 2.0
PARTIAL_TP_1_SIZE <- 0.33
PARTIAL_TP_2_ATR <- 3.5
PARTIAL_TP_2_SIZE <- 0.33

CLOSE_TIME_HOUR <- 22
MAX_BARS_IN_TRADE <- 100
USE_ADX_FILTER <- FALSE
ADX_THRESHOLD <- 20

TRAIN_START_YEAR <- 2023
TRAIN_END_YEAR <- 2024
TEST_YEAR <- 2025

input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
filename <- paste0(EPIC, "_", INTERVAL, ".csv")

cat("SCHRITT 1: Daten laden\n")
cat("----------------------------------------\n")

epic_data <- read_csv(file.path(input_path, filename), show_col_types = FALSE)
colnames(epic_data) <- c("date", "open", "close", "high", "low", "volume")
epic_data$date <- as.POSIXct(epic_data$date)
epic_data$hour <- hour(epic_data$date)
epic_data <- epic_data %>% distinct(date, .keep_all = TRUE)

train_data <- epic_data %>%
  filter(year(date) >= TRAIN_START_YEAR & year(date) <= TRAIN_END_YEAR) %>%
  arrange(date)

test_data <- epic_data %>%
  filter(year(date) == TEST_YEAR) %>%
  arrange(date)

# Source backtest functions from 03_backtest file
source(file.path("$BASE_DIR/Chop_Aroon", "03_backtest_Chop_Aroon_pyramiding.R"))

run_backtest_mcpt <- function(data) {
  data <- calculate_indicators(data,
    chop_period = CHOP_PERIOD,
    aroon_period = AROON_PERIOD,
    atr_period = ATR_PERIOD,
    adx_period = ADX_PERIOD,
    chandelier_period = CHANDELIER_PERIOD)
  
  data <- generate_signals(data,
    chop_threshold = CHOP_THRESHOLD,
    aroon_threshold = AROON_THRESHOLD,
    use_adx_filter = USE_ADX_FILTER,
    adx_threshold = ADX_THRESHOLD,
    stop_loss_atr_mult = STOP_LOSS_ATR_MULT,
    use_pyramiding = USE_PYRAMIDING,
    pyramid_method = PYRAMID_METHOD,
    max_pyramid_orders = MAX_PYRAMID_ORDERS,
    pyramid_spacing_atr = PYRAMID_SPACING_ATR,
    pyramid_size_multiplier = PYRAMID_SIZE_MULTIPLIER,
    consecutive_bars = CONSECUTIVE_BARS,
    breakout_lookback = BREAKOUT_LOOKBACK,
    exit_strategy = EXIT_STRATEGY,
    chandelier_multiplier = CHANDELIER_MULTIPLIER,
    chandelier_period = CHANDELIER_PERIOD,
    breakeven_trigger_atr = BREAKEVEN_TRIGGER_ATR,
    breakeven_offset_atr = BREAKEVEN_OFFSET_ATR,
    trailing_stop_atr_mult = TRAILING_STOP_ATR_MULT,
    trailing_start_atr_mult = TRAILING_START_ATR_MULT,
    tp_strategy = TP_STRATEGY,
    full_tp_atr_mult = FULL_TP_ATR_MULT,
    partial_tp_1_atr = PARTIAL_TP_1_ATR,
    partial_tp_1_size = PARTIAL_TP_1_SIZE,
    partial_tp_2_atr = PARTIAL_TP_2_ATR,
    partial_tp_2_size = PARTIAL_TP_2_SIZE,
    close_time_hour = CLOSE_TIME_HOUR,
    max_bars_in_trade = MAX_BARS_IN_TRADE)
  
  perf <- calculate_performance(data)
  return(perf)
}

cat("Berechne Training Performance\n")
train_perf <- run_backtest_mcpt(train_data)

if (METRIC == "profit_factor") {
  real_metric_train <- train_perf$profit_factor
} else if (METRIC == "sharpe_ratio") {
  real_metric_train <- train_perf$sharpe_ratio
} else {
  real_metric_train <- train_perf$total_return
}

cat("MONTE CARLO PERMUTATION TEST (TRAINING)\n")
cat(sprintf("Führe %d Permutationen durch...\n\n", N_PERMUTATIONS))

train_data$log_return <- log(train_data$close / lag(train_data$close))
permuted_metrics_train <- numeric(N_PERMUTATIONS)
perm_better_count_train <- 1

pb <- txtProgressBar(min = 0, max = N_PERMUTATIONS, style = 3)

for (perm_i in 1:N_PERMUTATIONS) {
  block_length <- max(5, round(nrow(train_data) / 20))
  n_blocks <- ceiling(nrow(train_data) / block_length)
  block_indices <- sample(1:n_blocks, n_blocks, replace = FALSE)
  
  permuted_returns <- c()
  for (block_idx in block_indices) {
    start_idx <- (block_idx - 1) * block_length + 1
    end_idx <- min(block_idx * block_length, nrow(train_data))
    if (start_idx <= nrow(train_data)) {
      permuted_returns <- c(permuted_returns,
                            train_data$log_return[start_idx:end_idx])
    }
  }
  
  permuted_returns <- permuted_returns[1:nrow(train_data)]
  permuted_returns[is.na(permuted_returns)] <- mean(permuted_returns, na.rm = TRUE)
  
  train_perm <- train_data
  train_perm$log_return <- permuted_returns
  train_perm$close <- train_data$close[1] * exp(cumsum(train_perm$log_return))
  train_perm$open <- train_perm$close * exp(rnorm(nrow(train_data), 0, 0.001))
  train_perm$high <- train_perm$close * (1 + abs(rnorm(nrow(train_data), 0, 0.005)))
  train_perm$low <- train_perm$close * (1 - abs(rnorm(nrow(train_data), 0, 0.005)))
  
  perm_perf <- run_backtest_mcpt(train_perm)
  
  if (METRIC == "profit_factor") {
    perm_metric <- perm_perf$profit_factor
  } else if (METRIC == "sharpe_ratio") {
    perm_metric <- perm_perf$sharpe_ratio
  } else {
    perm_metric <- perm_perf$total_return
  }
  
  permuted_metrics_train[perm_i] <- perm_metric
  
  if (perm_metric >= real_metric_train) {
    perm_better_count_train <- perm_better_count_train + 1
  }
  
  setTxtProgressBar(pb, perm_i)
}

close(pb)

p_value_train <- perm_better_count_train / (N_PERMUTATIONS + 1)

cat("\n\nMCPT ERGEBNISSE - TRAINING\n")
cat(sprintf("Metrik: %s | Echte Performance: %.4f | p-Wert: %.4f\n", 
            METRIC, real_metric_train, p_value_train))

if (nrow(test_data) > 0) {
  test_perf <- run_backtest_mcpt(test_data)
  
  if (METRIC == "profit_factor") {
    real_metric_test <- test_perf$profit_factor
  } else if (METRIC == "sharpe_ratio") {
    real_metric_test <- test_perf$sharpe_ratio
  } else {
    real_metric_test <- test_perf$total_return
  }
  
  test_data$log_return <- log(test_data$close / lag(test_data$close))
  permuted_metrics_test <- numeric(N_PERMUTATIONS)
  perm_better_count_test <- 1
  
  pb <- txtProgressBar(min = 0, max = N_PERMUTATIONS, style = 3)
  
  for (perm_i in 1:N_PERMUTATIONS) {
    block_length <- max(5, round(nrow(test_data) / 20))
    n_blocks <- ceiling(nrow(test_data) / block_length)
    block_indices <- sample(1:n_blocks, n_blocks, replace = FALSE)
    
    permuted_returns <- c()
    for (block_idx in block_indices) {
      start_idx <- (block_idx - 1) * block_length + 1
      end_idx <- min(block_idx * block_length, nrow(test_data))
      if (start_idx <= nrow(test_data)) {
        permuted_returns <- c(permuted_returns,
                              test_data$log_return[start_idx:end_idx])
      }
    }
    
    permuted_returns <- permuted_returns[1:nrow(test_data)]
    permuted_returns[is.na(permuted_returns)] <- mean(permuted_returns, na.rm = TRUE)
    
    test_perm <- test_data
    test_perm$log_return <- permuted_returns
    test_perm$close <- test_data$close[1] * exp(cumsum(test_perm$log_return))
    test_perm$open <- test_perm$close * exp(rnorm(nrow(test_data), 0, 0.001))
    test_perm$high <- test_perm$close * (1 + abs(rnorm(nrow(test_data), 0, 0.005)))
    test_perm$low <- test_perm$close * (1 - abs(rnorm(nrow(test_data), 0, 0.005)))
    
    perm_perf <- run_backtest_mcpt(test_perm)
    
    if (METRIC == "profit_factor") {
      perm_metric <- perm_perf$profit_factor
    } else if (METRIC == "sharpe_ratio") {
      perm_metric <- perm_perf$sharpe_ratio
    } else {
      perm_metric <- perm_perf$total_return
    }
    
    permuted_metrics_test[perm_i] <- perm_metric
    
    if (perm_metric >= real_metric_test) {
      perm_better_count_test <- perm_better_count_test + 1
    }
    
    setTxtProgressBar(pb, perm_i)
  }
  
  close(pb)
  
  p_value_test <- perm_better_count_test / (N_PERMUTATIONS + 1)
  
  cat("\n\nMCPT ERGEBNISSE - TEST\n")
  cat(sprintf("Metrik: %s | Echte Performance: %.4f | p-Wert: %.4f\n",
              METRIC, real_metric_test, p_value_test))
}

cat("\n✓ ANALYSE ABGESCHLOSSEN\n")
