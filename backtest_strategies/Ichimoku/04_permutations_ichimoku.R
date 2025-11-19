# ==============================================================================
# MONTE CARLO PERMUTATION TEST (MCPT) - ERWEITERT
# Ichimoku-Strategie für Gold-Daten mit ATR Risk Management, ADX Filter, RSI
# Mit MCPT für Training UND Test-Daten
# ==============================================================================

rm(list=ls())
gc()
options(scipen=999)

# ==============================================================================
# BIBLIOTHEKEN LADEN
# ==============================================================================

library(tidyverse)
library(lubridate)
library(TTR)
library(PerformanceAnalytics)
library(tictoc)
library(scales)

cat("\n")
cat("########################################################################\n")
cat("#                                                                      #\n")
cat("#      MCPT für Ichimoku Gold-Strategie - ERWEITERTE VERSION          #\n")
cat("#      Mit ATR, ADX, RSI Filtern & Momentum-Tests                     #\n")
cat("#                                                                      #\n")
cat("########################################################################\n\n")

# ==============================================================================
# KONFIGURATION
# ==============================================================================

# MCPT-Parameter
N_PERMUTATIONS <- 200
METRIC <- "profit_factor"  # "sharpe_ratio", "total_return", "profit_factor"

# Ichimoku-Parameter
CONVERSION_PERIOD <- 21      # Tenkan-sen
BASE_PERIOD <- 26            # Kijun-sen
LEADING_SPAN_B <- 70         # Senkou Span B
DISPLACEMENT <- 25           # Verschiebung

# Risk Management Parameter
ATR_PERIOD <- 14
STOP_LOSS_ATR_MULT <- 3.0
TAKE_PROFIT_ATR_MULT <- 4.5
CLOSE_TIME_HOUR <- 22

# ADX Filter
ADX_PERIOD <- 13
ADX_THRESHOLD_LONG <- 19
ADX_THRESHOLD_SHORT <- 21

# RSI Filter
USE_RSI_FILTER <- TRUE
RSI_LONG <- 55
RSI_SHORT <- 45

# Momentum Filter
USE_RSI_MOMENTUM <- FALSE
RSI_MOMENTUM_THRESHOLD <- -2

USE_ADX_MOMENTUM <- TRUE
ADX_MOMENTUM_THRESHOLD <- -1

# Train/Test Jahre
TRAIN_START_YEAR <- 2023
TRAIN_END_YEAR <- 2024
TEST_YEAR <- 2025

# Datenpfade
input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")
EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
filename <- paste0(EPIC, "_", INTERVAL, ".csv")

# ==============================================================================
# SCHRITT 1: DATEN LADEN
# ==============================================================================

cat("SCHRITT 1: Daten laden\n")
cat("----------------------------------------\n")

epic_data <- read_csv(file.path(input_path, filename), show_col_types = FALSE)
#epic_data <- read_csv("Various/GOLD_MINUTE_15.csv")
colnames(epic_data) <- c("date", "open", "close", "high", "low", "volume")

epic_data$date <- as.POSIXct(epic_data$date)
epic_data$hour <- hour(epic_data$date)

epic_data <- epic_data %>%
  distinct(date, .keep_all = TRUE)

cat(sprintf("✓ %d Datenpunkte geladen\n", nrow(epic_data)))
cat(sprintf("  Zeitraum: %s bis %s\n\n", min(epic_data$date), max(epic_data$date)))

# ==============================================================================
# SCHRITT 2: TRAIN/TEST SPLIT
# ==============================================================================

cat("SCHRITT 2: Train/Test Split\n")
cat("----------------------------------------\n")

train_data <- epic_data %>% 
  filter(year(date) >= TRAIN_START_YEAR & year(date) <= TRAIN_END_YEAR) %>%
  arrange(date)

test_data <- epic_data %>% 
  filter(year(date) == TEST_YEAR) %>%
  arrange(date)

cat(sprintf("Training: %d Bars (%s bis %s)\n", 
            nrow(train_data), 
            min(train_data$date), 
            max(train_data$date)))
cat(sprintf("Test: %d Bars (%s bis %s)\n\n", 
            nrow(test_data), 
            min(test_data$date), 
            max(test_data$date)))

# ==============================================================================
# FUNKTION: INDIKATOREN BERECHNEN
# ==============================================================================

calculate_indicators <- function(data, 
                                 conversion_period = CONVERSION_PERIOD,
                                 base_period = BASE_PERIOD,
                                 leading_span_b = LEADING_SPAN_B,
                                 displacement = DISPLACEMENT,
                                 atr_period = ATR_PERIOD,
                                 adx_period = ADX_PERIOD) {
  
  # Ichimoku-Komponenten
  data$tenkan_sen <- (runMax(data$high, conversion_period) + 
                        runMin(data$low, conversion_period)) / 2
  
  data$kijun_sen <- (runMax(data$high, base_period) + 
                       runMin(data$low, base_period)) / 2
  
  data$senkou_span_a <- lag((data$tenkan_sen + data$kijun_sen) / 2, 
                            displacement)
  
  data$senkou_span_b <- lag((runMax(data$high, leading_span_b) + 
                               runMin(data$low, leading_span_b)) / 2,
                            displacement)
  
  data$chikou_span <- lead(data$close, displacement)
  
  # ATR
  data$atr <- ATR(cbind(data$high, data$low, data$close), n = atr_period)[, "atr"]
  
  # ADX
  data$adx <- ADX(cbind(data$high, data$low, data$close), n = adx_period)[, "ADX"]
  
  # RSI
  data$rsi <- RSI(data$close, n = 14)
  
  return(data)
}

# ==============================================================================
# FUNKTION: SIGNALE GENERIEREN
# ==============================================================================

generate_signals <- function(data,
                             stop_loss_atr_mult = STOP_LOSS_ATR_MULT,
                             take_profit_atr_mult = TAKE_PROFIT_ATR_MULT,
                             close_time_hour = CLOSE_TIME_HOUR,
                             adx_threshold_long = ADX_THRESHOLD_LONG,
                             adx_threshold_short = ADX_THRESHOLD_SHORT,
                             use_rsi_filter = USE_RSI_FILTER,
                             rsi_short = RSI_SHORT,
                             rsi_long = RSI_LONG,
                             use_rsi_momentum = USE_RSI_MOMENTUM,
                             rsi_momentum_threshold = RSI_MOMENTUM_THRESHOLD,
                             use_adx_momentum = USE_ADX_MOMENTUM,
                             adx_momentum_threshold = ADX_MOMENTUM_THRESHOLD) {
  
  # Crossover-Signale
  data$tenkan_cross_up <- data$tenkan_sen > data$kijun_sen & 
    lag(data$tenkan_sen) <= lag(data$kijun_sen)
  
  data$tenkan_cross_down <- data$tenkan_sen < data$kijun_sen & 
    lag(data$tenkan_sen) >= lag(data$kijun_sen)
  
  # Cloud-Position
  data$price_above_cloud <- data$close > pmax(data$senkou_span_a, 
                                              data$senkou_span_b, 
                                              na.rm = TRUE)
  
  data$price_below_cloud <- data$close < pmin(data$senkou_span_a, 
                                              data$senkou_span_b, 
                                              na.rm = TRUE)
  
  # ADX-Filter (unterschiedlich für Long/Short)
  data$adx_filter_long <- data$adx >= adx_threshold_long
  data$adx_filter_short <- data$adx >= adx_threshold_short
  
  # ADX Momentum Filter
  if (use_adx_momentum) {
    data$adx_momentum <- data$adx - lag(data$adx, 1)
    data$adx_momentum_filter <- data$adx_momentum > adx_momentum_threshold
  } else {
    data$adx_momentum_filter <- TRUE
  }
  
  # RSI-Filter
  if (use_rsi_filter) {
    data$rsi_filter_long <- data$rsi > rsi_long  
    data$rsi_filter_short <- data$rsi < rsi_short   
  } else {
    data$rsi_filter_long <- TRUE
    data$rsi_filter_short <- TRUE
  }
  
  # RSI Momentum Filter
  if (use_rsi_momentum) {
    data$rsi_momentum <- data$rsi - lag(data$rsi, 1)
    data$rsi_momentum_long <- data$rsi_momentum > rsi_momentum_threshold
    data$rsi_momentum_short <- data$rsi_momentum < -rsi_momentum_threshold
  } else {
    data$rsi_momentum_long <- TRUE
    data$rsi_momentum_short <- TRUE
  }
  
  # Entry-Signale mit allen Filtern
  data$signal_raw <- case_when(
    data$tenkan_cross_up & 
      data$price_above_cloud & 
      data$adx_filter_long & 
      data$adx_momentum_filter &
      data$rsi_filter_long & 
      data$rsi_momentum_long ~ 1,
    
    data$tenkan_cross_down & 
      data$price_below_cloud & 
      data$adx_filter_short & 
      data$adx_momentum_filter &
      data$rsi_filter_short & 
      data$rsi_momentum_short ~ -1,
    
    TRUE ~ 0
  )
  
  # Initialisiere Vektoren
  n <- nrow(data)
  position <- numeric(n)
  entry_price <- rep(NA_real_, n)
  stop_loss <- rep(NA_real_, n)
  take_profit <- rep(NA_real_, n)
  
  # Aktuelle Trade-Variablen
  current_position <- 0
  current_entry <- NA_real_
  current_sl <- NA_real_
  current_tp <- NA_real_
  
  # Optimierte Schleife
  for (i in 2:n) {
    
    # 22:00 Uhr Close Check
    if (!is.na(data$hour[i]) && data$hour[i] == close_time_hour && current_position != 0) {
      current_position <- 0
      current_entry <- NA_real_
      current_sl <- NA_real_
      current_tp <- NA_real_
    }
    # Entry Logic
    else if (current_position == 0 && !is.na(data$signal_raw[i]) && data$signal_raw[i] != 0) {
      
      current_position <- data$signal_raw[i]
      current_entry <- data$close[i]
      
      if (!is.na(data$atr[i])) {
        if (current_position == 1) {
          current_sl <- current_entry - (stop_loss_atr_mult * data$atr[i])
          current_tp <- current_entry + (take_profit_atr_mult * data$atr[i])
        } else {
          current_sl <- current_entry + (stop_loss_atr_mult * data$atr[i])
          current_tp <- current_entry - (take_profit_atr_mult * data$atr[i])
        }
      }
    }
    # Exit Logic
    else if (current_position != 0 && !is.na(current_sl) && !is.na(current_tp)) {
      
      if (current_position == 1) {
        if (data$low[i] <= current_sl || data$high[i] >= current_tp) {
          current_position <- 0
          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
        }
      } else {
        if (data$high[i] >= current_sl || data$low[i] <= current_tp) {
          current_position <- 0
          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
        }
      }
    }
    
    position[i] <- current_position
    entry_price[i] <- current_entry
    stop_loss[i] <- current_sl
    take_profit[i] <- current_tp
  }
  
  data$position <- position
  data$entry_price <- entry_price
  data$stop_loss <- stop_loss
  data$take_profit <- take_profit
  data$signal <- lag(data$position, 1)
  data$signal[is.na(data$signal)] <- 0
  
  return(data)
}

# ==============================================================================
# FUNKTION: PERFORMANCE BERECHNEN
# ==============================================================================

calculate_performance <- function(data) {
  
  # Log-Returns
  data$log_return <- log(data$close / lag(data$close))
  data$forward_return <- lead(data$log_return, 1)
  data$strategy_return <- data$signal * data$forward_return
  
  data_clean <- data %>% filter(!is.na(strategy_return))
  
  # Bestehende Metriken
  total_log_return <- sum(data_clean$strategy_return, na.rm = TRUE)
  mean_log_return <- mean(data_clean$strategy_return, na.rm = TRUE)
  sd_log_return <- sd(data_clean$strategy_return, na.rm = TRUE)
  sharpe_log_ratio <- mean_log_return / sd_log_return * sqrt(252 * 24 * 4)
  
  total_return <- exp(sum(data_clean$strategy_return, na.rm = TRUE)) - 1
  mean_return <- exp(mean(data_clean$strategy_return, na.rm = TRUE)) - 1
  sd_return <- exp(sd(data_clean$strategy_return, na.rm = TRUE)) - 1
  sharpe_ratio <- mean_return / sd_return * sqrt(252 * 24 * 4)
  
  n_trades <- sum(abs(diff(data$position)) > 0, na.rm = TRUE)
  
  # Profit Factor
  gross_profit <- sum(data_clean$strategy_return[data_clean$strategy_return > 0], na.rm = TRUE)
  gross_loss <- abs(sum(data_clean$strategy_return[data_clean$strategy_return < 0], na.rm = TRUE))
  profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)
  
  # Maximum Drawdown
  cumulative_returns <- cumsum(data_clean$strategy_return)
  running_max <- cummax(cumulative_returns)
  drawdown <- cumulative_returns - running_max
  max_drawdown <- min(drawdown, na.rm = TRUE)
  max_drawdown_pct <- (exp(max_drawdown) - 1) * 100
  
  # Win Rate
  winning_trades <- sum(data_clean$strategy_return > 0, na.rm = TRUE)
  losing_trades <- sum(data_clean$strategy_return < 0, na.rm = TRUE)
  total_trades_ret <- winning_trades + losing_trades
  win_rate <- ifelse(total_trades_ret > 0, winning_trades / total_trades_ret, NA)
  
  return(list(
    data = data_clean,
    total_return = total_return,
    mean_return = mean_return,
    sharpe_ratio = sharpe_ratio,
    total_log_return = total_log_return,
    mean_log_return = mean_log_return,
    sharpe_log_ratio = sharpe_log_ratio,
    n_trades = n_trades,
    profit_factor = profit_factor,
    max_drawdown = max_drawdown,
    max_drawdown_pct = max_drawdown_pct,
    win_rate = win_rate,
    winning_trades = winning_trades,
    losing_trades = losing_trades
  ))
}

# ==============================================================================
# FUNKTION: PERFORMANCE DETAILLIERT
# ==============================================================================

calculate_performance_detailed <- function(data, initial_capital = 1000, contracts = 1, 
                                           lot_size = 1.2, spread_pips = 35) {
  
  # Log-Returns
  data$log_return <- log(data$close / lag(data$close))
  data$forward_return <- lead(data$log_return, 1)
  data$strategy_return <- data$signal * data$forward_return
  
  data_clean <- data %>% filter(!is.na(strategy_return))
  
  # Basis-Metriken
  total_log_return <- sum(data_clean$strategy_return, na.rm = TRUE)
  mean_log_return <- mean(data_clean$strategy_return, na.rm = TRUE)
  sd_log_return <- sd(data_clean$strategy_return, na.rm = TRUE)
  sharpe_log_ratio <- mean_log_return / sd_log_return * sqrt(252 * 24 * 4)
  
  total_return <- exp(sum(data_clean$strategy_return, na.rm = TRUE)) - 1
  mean_return <- exp(mean(data_clean$strategy_return, na.rm = TRUE)) - 1
  sd_return <- exp(sd(data_clean$strategy_return, na.rm = TRUE)) - 1
  sharpe_ratio <- mean_return / sd_return * sqrt(252 * 24 * 4)
  
  n_trades <- sum(abs(diff(data$position)) > 0, na.rm = TRUE)
  
  # Profit Factor, Max Drawdown, Win Rate
  gross_profit <- sum(data_clean$strategy_return[data_clean$strategy_return > 0], na.rm = TRUE)
  gross_loss <- abs(sum(data_clean$strategy_return[data_clean$strategy_return < 0], na.rm = TRUE))
  profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)
  
  cumulative_returns <- cumsum(data_clean$strategy_return)
  running_max <- cummax(cumulative_returns)
  drawdown <- cumulative_returns - running_max
  max_drawdown <- min(drawdown, na.rm = TRUE)
  max_drawdown_pct <- (exp(max_drawdown) - 1) * 100
  
  winning_trades <- sum(data_clean$strategy_return > 0, na.rm = TRUE)
  losing_trades <- sum(data_clean$strategy_return < 0, na.rm = TRUE)
  total_trades_ret <- winning_trades + losing_trades
  win_rate <- ifelse(total_trades_ret > 0, winning_trades / total_trades_ret, NA)
  
  # Trade-by-Trade Analyse
  data_clean$position_change <- c(0, diff(data_clean$position))
  trade_starts <- which(data_clean$position_change != 0 & data_clean$position != 0)
  trade_ends <- which(data_clean$position_change != 0 & lag(data_clean$position) != 0)
  
  if(length(trade_starts) > length(trade_ends)) {
    trade_ends <- c(trade_ends, nrow(data_clean))
  }
  
  trades_list <- list()
  current_capital <- initial_capital
  total_spread_costs <- 0
  
  if(length(trade_starts) > 0 && length(trade_starts) == length(trade_ends)) {
    for(i in 1:length(trade_starts)) {
      start_idx <- trade_starts[i]
      end_idx <- trade_ends[i]
      
      trade_direction <- data_clean$position[start_idx]
      entry_price <- data_clean$close[start_idx]
      exit_price <- data_clean$close[end_idx]
      
      sl_price <- data_clean$stop_loss[start_idx]
      tp_price <- data_clean$take_profit[start_idx]
      
      # Exit-Grund bestimmen
      exit_reason <- NA
      if(!is.na(tp_price) && !is.na(sl_price)) {
        if(trade_direction == 1) {
          if(exit_price >= tp_price) {
            exit_reason <- "TP"
          } else if(exit_price <= sl_price) {
            exit_reason <- "SL"
          } else {
            exit_reason <- "Time"
          }
        } else if(trade_direction == -1) {
          if(exit_price <= tp_price) {
            exit_reason <- "TP"
          } else if(exit_price >= sl_price) {
            exit_reason <- "SL"
          } else {
            exit_reason <- "Time"
          }
        }
      } else {
        exit_reason <- "Time"
      }
      
      position_size <- lot_size
      contract_size <- contracts
      pip_value <- 0.01
      
      spread_cost <- spread_pips * pip_value * lot_size * contract_size
      
      price_diff <- ifelse(trade_direction == 1, 
                           exit_price - entry_price,
                           entry_price - exit_price)
      
      gross_pnl <- price_diff * lot_size * contract_size
      net_pnl <- gross_pnl - spread_cost
      trade_return_pct <- net_pnl / current_capital
      
      current_capital <- current_capital + net_pnl
      total_spread_costs <- total_spread_costs + spread_cost
      
      risk_distance <- abs(entry_price - sl_price)
      risk_dollar <- risk_distance * lot_size * contract_size
      risk_percent <- (risk_dollar / (current_capital - net_pnl)) * 100
      
      trades_list[[i]] <- data.frame(
        trade_num = i,
        entry_time = data_clean$date[start_idx],
        exit_time = data_clean$date[end_idx],
        direction = ifelse(trade_direction == 1, "Long", "Short"),
        entry_price = entry_price,
        exit_price = exit_price,
        sl_price = sl_price,
        tp_price = tp_price,
        lot_size = lot_size,
        price_diff = price_diff,
        gross_pnl = gross_pnl,
        spread_cost = spread_cost,
        net_pnl = net_pnl,
        trade_return_pct = trade_return_pct * 100,
        capital_before = current_capital - net_pnl,
        capital_after = current_capital,
        risk_dollar = risk_dollar,
        risk_percent = risk_percent,
        exit_reason = exit_reason,
        duration_bars = end_idx - start_idx + 1
      )
    }
    
    trades_df <- do.call(rbind, trades_list)
    
    winning_trades_net <- sum(trades_df$net_pnl > 0, na.rm = TRUE)
    losing_trades_net <- sum(trades_df$net_pnl < 0, na.rm = TRUE)
    win_rate_net <- winning_trades_net / (winning_trades_net + losing_trades_net)
    
    avg_win <- mean(trades_df$net_pnl[trades_df$net_pnl > 0], na.rm = TRUE)
    avg_loss <- mean(trades_df$net_pnl[trades_df$net_pnl < 0], na.rm = TRUE)
    largest_win <- max(trades_df$net_pnl, na.rm = TRUE)
    largest_loss <- min(trades_df$net_pnl, na.rm = TRUE)
    avg_risk_percent <- mean(trades_df$risk_percent, na.rm = TRUE)
    
    gross_profit_net <- sum(trades_df$net_pnl[trades_df$net_pnl > 0], na.rm = TRUE)
    gross_loss_net <- abs(sum(trades_df$net_pnl[trades_df$net_pnl < 0], na.rm = TRUE))
    profit_factor_net <- ifelse(gross_loss_net > 0, gross_profit_net / gross_loss_net, NA)
    
    exit_stats <- table(trades_df$exit_reason)
    avg_spread_per_trade <- mean(trades_df$spread_cost)
    spread_as_pct_of_avg_win <- (avg_spread_per_trade / avg_win) * 100
    
  } else {
    trades_df <- NULL
    winning_trades_net <- NA
    losing_trades_net <- NA
    win_rate_net <- NA
    avg_win <- NA
    avg_loss <- NA
    largest_win <- NA
    largest_loss <- NA
    avg_risk_percent <- NA
    profit_factor_net <- NA
    exit_stats <- NULL
    total_spread_costs <- NA
    avg_spread_per_trade <- NA
    spread_as_pct_of_avg_win <- NA
  }
  
  return(list(
    data = data_clean,
    total_return = total_return,
    mean_return = mean_return,
    sharpe_ratio = sharpe_ratio,
    total_log_return = total_log_return,
    mean_log_return = mean_log_return,
    sharpe_log_ratio = sharpe_log_ratio,
    n_trades = n_trades,
    profit_factor = profit_factor,
    profit_factor_net = profit_factor_net,
    max_drawdown = max_drawdown,
    max_drawdown_pct = max_drawdown_pct,
    win_rate = win_rate,
    win_rate_net = win_rate_net,
    winning_trades = winning_trades,
    losing_trades = losing_trades,
    winning_trades_net = winning_trades_net,
    losing_trades_net = losing_trades_net,
    trades = trades_df,
    lot_size = lot_size,
    spread_pips = spread_pips,
    initial_capital = initial_capital,
    final_capital = current_capital,
    total_pnl = current_capital - initial_capital,
    total_spread_costs = total_spread_costs,
    avg_spread_per_trade = avg_spread_per_trade,
    spread_as_pct_of_avg_win = spread_as_pct_of_avg_win,
    return_on_capital = ((current_capital - initial_capital) / initial_capital) * 100,
    avg_win = avg_win,
    avg_loss = avg_loss,
    largest_win = largest_win,
    largest_loss = largest_loss,
    avg_risk_percent = avg_risk_percent,
    exit_stats = exit_stats
  ))
}

# ==============================================================================
# SCHRITT 3-5: TRAINING - INDIKATOREN, SIGNALE, PERFORMANCE
# ==============================================================================

cat("SCHRITT 3-5: Berechne Training Performance\n")
cat("----------------------------------------\n")

train_data <- calculate_indicators(train_data)
train_data <- generate_signals(train_data)
train_perf <- calculate_performance(train_data)

cat("✓ Ichimoku-Komponenten berechnet\n")
cat("✓ ATR, ADX und RSI berechnet\n")
cat("✓ Handelssignale mit allen Filtern generiert\n\n")

cat("Echte Training Performance:\n")
cat(sprintf("  Total Return: %.4f\n", train_perf$total_return))
cat(sprintf("  Mean Return: %.6f\n", train_perf$mean_return))
cat(sprintf("  Sharpe Ratio: %.4f\n", train_perf$sharpe_ratio))
cat(sprintf("  Anzahl Trades: %d\n", train_perf$n_trades))
cat(sprintf("  Win Rate: %.4f\n", train_perf$win_rate))
cat(sprintf("  Profit Factor: %.4f\n", train_perf$profit_factor))
cat(sprintf("  Max Drawdown: %.4f%%\n\n", train_perf$max_drawdown_pct))

# Wähle Metrik
if (METRIC == "profit_factor") {
  real_metric_train <- train_perf$profit_factor
} else if (METRIC == "sharpe_ratio") {
  real_metric_train <- train_perf$sharpe_ratio
} else if (METRIC == "total_return") {
  real_metric_train <- train_perf$total_return
}

# ==============================================================================
# SCHRITT 6: MONTE CARLO PERMUTATION TEST - TRAINING
# ==============================================================================

cat("SCHRITT 6: Monte Carlo Permutation Test (TRAINING)\n")
cat("========================================\n")
cat(sprintf("Führe %d Permutationen durch...\n\n", N_PERMUTATIONS))

train_data$log_return <- log(train_data$close / lag(train_data$close))

permuted_metrics_train <- numeric(N_PERMUTATIONS)
perm_better_count_train <- 1
permuted_data_train <- list()

pb <- txtProgressBar(min = 0, max = N_PERMUTATIONS, style = 3)

for (perm_i in 1:N_PERMUTATIONS) {
  
  # Block-Bootstrap
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
  
  # Erstelle permutierte Daten
  train_perm <- train_data
  train_perm$log_return <- permuted_returns
  train_perm$close <- train_data$close[1] * exp(cumsum(train_perm$log_return))
  train_perm$open <- train_perm$close * exp(rnorm(nrow(train_data), 0, 0.001))
  train_perm$high <- train_perm$close * (1 + abs(rnorm(nrow(train_data), 0, 0.005)))
  train_perm$low <- train_perm$close * (1 - abs(rnorm(nrow(train_data), 0, 0.005)))
  
  # Berechne Indikatoren und Signale
  train_perm <- calculate_indicators(train_perm)
  train_perm <- generate_signals(train_perm)
  perm_perf <- calculate_performance(train_perm)
  
  # Speichere Metrik
  if (METRIC == "profit_factor") {
    perm_metric <- perm_perf$profit_factor
  } else if (METRIC == "sharpe_ratio") {
    perm_metric <- perm_perf$sharpe_ratio
  } else if (METRIC == "total_return") {
    perm_metric <- perm_perf$total_return
  }
  
  permuted_metrics_train[perm_i] <- perm_metric
  
  # Speichere erste 100 Permutationen
  if (perm_i <= 100) {
    export_perm <- data.frame(
      date = train_data$date,
      run = perm_i,
      close = train_perm$close
    )
    permuted_data_train[[perm_i]] <- export_perm
  }
  
  if (perm_metric >= real_metric_train) {
    perm_better_count_train <- perm_better_count_train + 1
  }
  
  setTxtProgressBar(pb, perm_i)
}

close(pb)

# P-Wert Training
p_value_train <- perm_better_count_train / (N_PERMUTATIONS + 1)

cat("\n\n")
cat("========================================\n")
cat("MCPT ERGEBNISSE - TRAINING\n")
cat("========================================\n")
cat(sprintf("Metrik: %s\n", METRIC))
cat(sprintf("Echte Performance: %.4f\n", real_metric_train))
cat(sprintf("MCPT p-Wert: %.4f\n", p_value_train))
if (p_value_train < 0.05) {
  cat("✓ SIGNIFIKANT (p < 0.05)\n")
} else {
  cat("✗ NICHT SIGNIFIKANT (p ≥ 0.05)\n")
}
cat("========================================\n\n")

# ==============================================================================
# SCHRITT 7: TEST-DATEN - INDIKATOREN, SIGNALE, PERFORMANCE
# ==============================================================================

if (nrow(test_data) > 0) {
  
  cat("SCHRITT 7: Berechne Test Performance\n")
  cat("----------------------------------------\n")
  
  test_data <- calculate_indicators(test_data)
  test_data <- generate_signals(test_data)
  test_perf <- calculate_performance(test_data)
  
  cat("Out-of-Sample Performance:\n")
  cat(sprintf("  Total Return: %.4f\n", test_perf$total_return))
  cat(sprintf("  Mean Return: %.6f\n", test_perf$mean_return))
  cat(sprintf("  Sharpe Ratio: %.4f\n", test_perf$sharpe_ratio))
  cat(sprintf("  Anzahl Trades: %d\n", test_perf$n_trades))
  cat(sprintf("  Win Rate: %.4f\n", test_perf$win_rate))
  cat(sprintf("  Profit Factor: %.4f\n", test_perf$profit_factor))
  cat(sprintf("  Max Drawdown: %.4f%%\n\n", test_perf$max_drawdown_pct))
  
  # Wähle Metrik
  if (METRIC == "profit_factor") {
    real_metric_test <- test_perf$profit_factor
  } else if (METRIC == "sharpe_ratio") {
    real_metric_test <- test_perf$sharpe_ratio
  } else if (METRIC == "total_return") {
    real_metric_test <- test_perf$total_return
  }
  
  # ==============================================================================
  # SCHRITT 8: MONTE CARLO PERMUTATION TEST - TEST
  # ==============================================================================
  
  cat("SCHRITT 8: Monte Carlo Permutation Test (TEST)\n")
  cat("========================================\n")
  cat(sprintf("Führe %d Permutationen für Test-Daten durch...\n\n", N_PERMUTATIONS))
  
  test_data$log_return <- log(test_data$close / lag(test_data$close))
  
  permuted_metrics_test <- numeric(N_PERMUTATIONS)
  perm_better_count_test <- 1
  permuted_data_test <- list()
  
  pb <- txtProgressBar(min = 0, max = N_PERMUTATIONS, style = 3)
  
  for (perm_i in 1:N_PERMUTATIONS) {
    
    # Block-Bootstrap
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
    
    # Erstelle permutierte Test-Daten
    test_perm <- test_data
    test_perm$log_return <- permuted_returns
    test_perm$close <- test_data$close[1] * exp(cumsum(test_perm$log_return))
    test_perm$open <- test_perm$close * exp(rnorm(nrow(test_data), 0, 0.001))
    test_perm$high <- test_perm$close * (1 + abs(rnorm(nrow(test_data), 0, 0.005)))
    test_perm$low <- test_perm$close * (1 - abs(rnorm(nrow(test_data), 0, 0.005)))
    
    # Berechne Indikatoren und Signale
    test_perm <- calculate_indicators(test_perm)
    test_perm <- generate_signals(test_perm)
    perm_perf <- calculate_performance(test_perm)
    
    # Speichere Metrik
    if (METRIC == "profit_factor") {
      perm_metric <- perm_perf$profit_factor
    } else if (METRIC == "sharpe_ratio") {
      perm_metric <- perm_perf$sharpe_ratio
    } else if (METRIC == "total_return") {
      perm_metric <- perm_perf$total_return
    }
    
    permuted_metrics_test[perm_i] <- perm_metric
    
    # Speichere erste 100 Permutationen
    if (perm_i <= 100) {
      export_perm <- data.frame(
        date = test_data$date,
        run = perm_i,
        close = test_perm$close
      )
      permuted_data_test[[perm_i]] <- export_perm
    }
    
    if (perm_metric >= real_metric_test) {
      perm_better_count_test <- perm_better_count_test + 1
    }
    
    setTxtProgressBar(pb, perm_i)
  }
  
  close(pb)
  
  # P-Wert Test
  p_value_test <- perm_better_count_test / (N_PERMUTATIONS + 1)
  
  cat("\n\n")
  cat("========================================\n")
  cat("MCPT ERGEBNISSE - TEST\n")
  cat("========================================\n")
  cat(sprintf("Metrik: %s\n", METRIC))
  cat(sprintf("Echte Performance: %.4f\n", real_metric_test))
  cat(sprintf("MCPT p-Wert: %.4f\n", p_value_test))
  if (p_value_test < 0.05) {
    cat("✓ SIGNIFIKANT (p < 0.05)\n")
  } else {
    cat("✗ NICHT SIGNIFIKANT (p ≥ 0.05)\n")
  }
  cat("========================================\n\n")
}

# ==============================================================================
# SCHRITT 9: VISUALISIERUNGEN
# ==============================================================================

cat("SCHRITT 9: Erstelle Visualisierungen\n")
cat("----------------------------------------\n")

train_perf_detailed <- calculate_performance_detailed(train_data)
test_perf_detailed <- calculate_performance_detailed(test_data)

train_trades_raw <- train_perf_detailed$trades
test_trades_raw <- test_perf_detailed$trades

# Equity Curve Training
if (!is.null(train_trades_raw)) {
  train_trades_raw <- train_trades_raw %>%
    select(trade_num, exit_time, capital_before, capital_after, net_pnl, trade_return_pct)
  
  start_row <- data.frame(
    trade_num = 0,
    exit_time = min(train_trades_raw$exit_time) - days(1),
    capital_before = 1000,
    capital_after = 1000,
    net_pnl = 0,
    trade_return_pct = 0
  )
  
  train_trades <- rbind(start_row, train_trades_raw)
  
  train_trades <- train_trades %>%
    mutate(
      cumulative_return_pct = ((capital_after / 1000) - 1) * 100,
      cumulative_max = cummax(capital_after),
      drawdown = ((capital_after - cumulative_max) / cumulative_max) * 100,
      month = floor_date(exit_time, "month")
    )
  
  trades_profit <- train_trades %>% filter(capital_after >= 1000)
  trades_loss <- train_trades %>% filter(capital_after < 1000)
  
  p_equity <- ggplot(train_trades, aes(x = exit_time, y = capital_after)) +
    geom_ribbon(data = trades_profit, aes(ymin = 1000, ymax = capital_after), 
                fill = "green", alpha = 0.3) +
    geom_ribbon(data = trades_loss, aes(ymin = capital_after, ymax = 1000), 
                fill = "red", alpha = 0.3) +
    geom_line(color = "#2E86AB", linewidth = 1.2) +
    geom_hline(yintercept = 1000, linetype = "dashed", color = "gray50", alpha = 0.7) +
    scale_y_continuous(labels = dollar_format(prefix = "$")) +
    scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") +
    labs(
      title = "Equity Curve - Training",
      x = NULL,
      y = "Account Balance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  print(p_equity)
}

# 1. MCPT Histogram - Training
df_plot_train <- data.frame(metric = permuted_metrics_train)

p1 <- ggplot(df_plot_train, aes(x = metric)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, 
                 fill = "gray70", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1.2) +
  geom_vline(xintercept = real_metric_train, 
             color = "red", linewidth = 1.5, linetype = "dashed") +
  annotate("text", 
           x = real_metric_train, 
           y = Inf, 
           label = sprintf("Echte Performance\n%.4f", real_metric_train),
           hjust = -0.1, vjust = 2, color = "red", size = 4) +
  labs(
    title = "MCPT: TRAINING-DATEN",
    subtitle = sprintf("p-Wert = %.4f | Metrik: %s | N = %d", 
                       p_value_train, METRIC, N_PERMUTATIONS),
    x = METRIC,
    y = "Dichte"
  ) +
  theme_minimal()

print(p1)

# 2. MCPT Histogram - Test
if (nrow(test_data) > 0) {
  df_plot_test <- data.frame(metric = permuted_metrics_test)
  
  p2 <- ggplot(df_plot_test, aes(x = metric)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, 
                   fill = "gray70", color = "black", alpha = 0.7) +
    geom_density(color = "blue", linewidth = 1.2) +
    geom_vline(xintercept = real_metric_test, 
               color = "red", linewidth = 1.5, linetype = "dashed") +
    annotate("text", 
             x = real_metric_test, 
             y = Inf, 
             label = sprintf("Echte Performance\n%.4f", real_metric_test),
             hjust = -0.1, vjust = 2, color = "red", size = 4) +
    labs(
      title = "MCPT: TEST-DATEN",
      subtitle = sprintf("p-Wert = %.4f | Metrik: %s | N = %d", 
                         p_value_test, METRIC, N_PERMUTATIONS),
      x = METRIC,
      y = "Dichte"
    ) +
    theme_minimal()
  
  print(p2)
}





# 5. Equity Curves (Log-Scale)
train_perf$data$cumulative_return <- cumsum(replace_na(train_perf$data$strategy_return, 0))
train_perf$data$equity_curve <- exp(train_perf$data$cumulative_return)

p5 <- ggplot(train_perf$data, aes(x = date, y = equity_curve)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "In-Sample Equity Curve",
    subtitle = sprintf("Sharpe: %.2f | Return: %.2f%% | Trades: %d | p = %.4f",
                       train_perf$sharpe_ratio,
                       train_perf$total_return * 100,
                       train_perf$n_trades,
                       p_value_train),
    x = "Datum",
    y = "Kumulative Returns"
  ) +
  scale_y_log10() +
  theme_minimal()

print(p5)

if (nrow(test_data) > 0) {
  test_perf$data$cumulative_return <- cumsum(replace_na(test_perf$data$strategy_return, 0))
  test_perf$data$equity_curve <- exp(test_perf$data$cumulative_return)
  
  p6 <- ggplot(test_perf$data, aes(x = date, y = equity_curve)) +
    geom_line(color = "darkblue", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Out-of-Sample Equity Curve",
      subtitle = sprintf("Sharpe: %.2f | Return: %.2f%% | Trades: %d | p = %.4f",
                         test_perf$sharpe_ratio,
                         test_perf$total_return * 100,
                         test_perf$n_trades,
                         p_value_test),
      x = "Datum",
      y = "Kumulative Returns"
    ) +
    scale_y_log10() +
    theme_minimal()
  
  print(p6)
}

cat("✓ Visualisierungen erstellt\n\n")

# ==============================================================================
# FINALE ZUSAMMENFASSUNG
# ==============================================================================

cat("\n")
cat("========================================\n")
cat("FINALE ZUSAMMENFASSUNG\n")
cat("========================================\n\n")

cat("Strategie-Parameter:\n")
cat(sprintf("  Ichimoku: Conv=%d, Base=%d, SpanB=%d, Disp=%d\n", 
            CONVERSION_PERIOD, BASE_PERIOD, LEADING_SPAN_B, DISPLACEMENT))
cat(sprintf("  ATR: Period=%d, SL=%.1fx, TP=%.1fx\n", 
            ATR_PERIOD, STOP_LOSS_ATR_MULT, TAKE_PROFIT_ATR_MULT))
cat(sprintf("  ADX: Period=%d, Long>=%d, Short>=%d\n", 
            ADX_PERIOD, ADX_THRESHOLD_LONG, ADX_THRESHOLD_SHORT))
cat(sprintf("  RSI: Filter=%s, Long>%d, Short<%d\n", 
            USE_RSI_FILTER, RSI_LONG, RSI_SHORT))
cat(sprintf("  RSI Momentum: %s (threshold=%.1f)\n", 
            USE_RSI_MOMENTUM, RSI_MOMENTUM_THRESHOLD))
cat(sprintf("  ADX Momentum: %s (threshold=%.1f)\n", 
            USE_ADX_MOMENTUM, ADX_MOMENTUM_THRESHOLD))
cat(sprintf("  Close Time: %d:00 Uhr\n\n", CLOSE_TIME_HOUR))

cat("TRAINING:\n")
cat(sprintf("  Sharpe Ratio: %.4f\n", train_perf$sharpe_ratio))
cat(sprintf("  Total Return: %.2f%%\n", train_perf$total_return * 100))
cat(sprintf("  Profit Factor: %.4f\n", train_perf$profit_factor))
cat(sprintf("  Win Rate: %.2f%%\n", train_perf$win_rate * 100))
cat(sprintf("  Trades: %d\n", train_perf$n_trades))
cat(sprintf("  MCPT p-Wert: %.4f %s\n\n", 
            p_value_train, 
            ifelse(p_value_train < 0.05, "✓", "✗")))

if (nrow(test_data) > 0) {
  cat("TEST:\n")
  cat(sprintf("  Sharpe Ratio: %.4f (%.1f%% von Training)\n", 
              test_perf$sharpe_ratio,
              test_perf$sharpe_ratio / train_perf$sharpe_ratio * 100))
  cat(sprintf("  Total Return: %.2f%% (%.1f%% von Training)\n", 
              test_perf$total_return * 100,
              test_perf$total_return / train_perf$total_return * 100))
  cat(sprintf("  Profit Factor: %.4f\n", test_perf$profit_factor))
  cat(sprintf("  Win Rate: %.2f%%\n", test_perf$win_rate * 100))
  cat(sprintf("  Trades: %d\n", test_perf$n_trades))
  cat(sprintf("  Drawdown: %.2f%%\n", test_perf$max_drawdown_pct))
  cat(sprintf("  MCPT p-Wert: %.4f %s\n\n", 
              p_value_test, 
              ifelse(p_value_test < 0.05, "✓", "✗")))
}

cat("INTERPRETATION:\n")
if (p_value_train < 0.05 && (nrow(test_data) == 0 || p_value_test < 0.05)) {
  cat("✓ Strategie ist statistisch robust\n")
  cat("  Beide MCPT-Tests sind signifikant\n")
} else if (p_value_train < 0.05 && p_value_test >= 0.05) {
  cat("⚠ In-Sample gut, aber Out-of-Sample fraglich\n")
  cat("  Mögliches Overfitting vorhanden\n")
} else {
  cat("✗ Strategie ist statistisch nicht robust\n")
  cat("  Hohes Risiko für False Positives\n")
}

# ==============================================================================
# Realistische Positionsgrößen und Equity Curve
# ==============================================================================

# Startkapital als ersten Punkt hinzufügen
start_row <- data.frame(
  trade_num = 0,
  exit_time = min(test_trades_raw$exit_time) - days(1),
  capital_before = 1000,
  capital_after = 1000,
  net_pnl = 0,
  trade_return_pct = 0
)

test_trades = test_trades_raw %>%
  select(trade_num, exit_time, capital_before, capital_after, net_pnl, trade_return_pct)

test_trades <- rbind(start_row, test_trades)

# Berechnungen
test_trades <- test_trades %>%
  mutate(
    cumulative_return_pct = ((capital_after / 1000) - 1) * 100,
    cumulative_max = cummax(capital_after),
    drawdown = ((capital_after - cumulative_max) / cumulative_max) * 100,
    month = floor_date(exit_time, "month")
  )

# Statistiken berechnen
total_return <- (tail(test_trades$capital_after, 1) / 1000 - 1) * 100
winning_trades <- sum(test_trades$net_pnl > 0)
losing_trades <- sum(test_trades$net_pnl < 0)
win_rate <- winning_trades / (winning_trades + losing_trades) * 100
avg_win <- mean(test_trades$net_pnl[test_trades$net_pnl > 0])
avg_loss <- mean(test_trades$net_pnl[test_trades$net_pnl < 0])
profit_factor <- abs(sum(test_trades$net_pnl[test_trades$net_pnl > 0]) / 
                       sum(test_trades$net_pnl[test_trades$net_pnl < 0]))
max_drawdown <- min(test_trades$drawdown)
max_profit <- max(test_trades$net_pnl)
max_loss <- min(test_trades$net_pnl)

# Statistiken ausgeben
cat("\n", rep("=", 60), "\n", sep="")
cat("TRADING PERFORMANCE STATISTIKEN\n")
cat(rep("=", 60), "\n\n", sep="")
cat("📊 Allgemeine Statistiken:\n")
cat(sprintf("   Startkapital:        €%.2f\n", 1000))
cat(sprintf("   Endkapital:          €%.2f\n", tail(test_trades$capital_after, 1)))
cat(sprintf("   Total Return:        %.2f%%\n", total_return))
cat(sprintf("   Anzahl Trades:       %d\n", nrow(test_trades)-1))
cat("\n💰 Trade Statistiken:\n")
cat(sprintf("   Winning Trades:      %d (%.1f%%)\n", winning_trades, win_rate))
cat(sprintf("   Losing Trades:       %d (%.1f%%)\n", losing_trades, 100-win_rate))
cat(sprintf("   Avg. Gewinn:         €%.2f\n", avg_win))
cat(sprintf("   Avg. Verlust:        €%.2f\n", avg_loss))
cat(sprintf("   Avg. PnL:            €%.2f\n", avg_win+avg_loss))
cat(sprintf("   Profit Factor:       %.2f\n", profit_factor))
cat("\n📉 Risk Metriken:\n")
cat(sprintf("   Max Drawdown:        %.2f%%\n", max_drawdown))
cat(sprintf("   Größter Gewinn:      €%.2f\n", max_profit))
cat(sprintf("   Größter Verlust:     €%.2f\n", max_loss))
cat(rep("=", 60), "\n\n", sep="")

# Plot 1: Equity Curve
# Separate data für positive und negative Bereiche
trades_profit <- test_trades %>% filter(capital_after >= 1000)
trades_loss <- test_trades %>% filter(capital_after < 1000)

ggplot(test_trades, aes(x = exit_time, y = capital_after)) +
  geom_ribbon(data = trades_profit, aes(ymin = 1000, ymax = capital_after), 
              fill = "green", alpha = 0.3) +
  geom_ribbon(data = trades_loss, aes(ymin = capital_after, ymax = 1000), 
              fill = "red", alpha = 0.3) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  scale_x_datetime(date_labels = "%Y-%m", date_breaks = "1 month") +
  labs(
    title = "Equity Curve",
    x = NULL,
    y = "Account Balance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



cat("\n========================================\n")
cat("✓ ANALYSE ABGESCHLOSSEN\n")
cat("========================================\n\n")