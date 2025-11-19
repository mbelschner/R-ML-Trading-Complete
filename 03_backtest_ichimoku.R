# ==============================================================================
#Backtest
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
library(pracma)

cat("\n")
cat("########################################################################\n")
cat("#                                                                      #\n")
cat("#      Backtest für 15 Minuten Intraday Strategien                    #\n")
cat("#                                                                     #\n")
cat("#                                                                      #\n")
cat("########################################################################\n\n")

# ==============================================================================
# KONFIGURATION
# ==============================================================================

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

#Source Strategy Functions
source(file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "xx_strategies_signals.R"))

# ==============================================================================
# SCHRITT 1: DATEN LADEN
# ==============================================================================

cat("SCHRITT 1: Daten laden\n")
cat("----------------------------------------\n")

epic_data <- read_csv(file.path(input_path, filename), show_col_types = FALSE)
#epic_data <- read_csv("Various/GOLD_MINUTE_15.csv")
colnames(epic_data) <- c("date", "open", "close", "high", "low", "volume")

# Zeitstempel parsen (falls nicht bereits datetime)
epic_data$date <- as.POSIXct(epic_data$date)
epic_data$hour <- hour(epic_data$date)

epic_data = epic_data %>%
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

# ------------------------------------------------------------------------------
# 1. INDIKATOREN BERECHNEN (mit flexiblen Parametern)
# ------------------------------------------------------------------------------

calculate_indicators <- function(data, 
                                 conversion_period = 20,
                                 base_period = 23,
                                 leading_span_b = 44,
                                 displacement = 33,
                                 atr_period = 14,
                                 adx_period = 14) {
  
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
  
  #RSI
  data$rsi <- RSI(data$close, n = 14)
  
  return(data)
}

# ------------------------------------------------------------------------------
# 2. SIGNALE GENERIEREN (mit flexiblen Parametern)
# ------------------------------------------------------------------------------

generate_signals <- function(data,
                             stop_loss_atr_mult = 3.0,
                             take_profit_atr_mult = 4.5,
                             close_time_hour = 22,
                             adx_threshold_long = 19,
                             adx_threshold_short = 22,
                             use_rsi_filter = TRUE,
                             rsi_short = 70,
                             rsi_long = 51,
                             use_rsi_momentum = TRUE,
                             rsi_momentum_threshold = -2,
                             use_adx_momentum = TRUE,
                             adx_momentum_threshold = -1) {
  
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
  
  # ADX Momentum Filter (erste Ableitung) - steigender ADX
  if (use_adx_momentum) {
    data$adx_momentum <- data$adx - lag(data$adx, 1)
    # Für beide Richtungen: steigender ADX = stärkerer Trend
    data$adx_momentum_filter <- data$adx_momentum > adx_momentum_threshold
  } else {
    data$adx_momentum_filter <- TRUE
  }
  
  # RSI-Filter (optional)
  if (use_rsi_filter) {
    data$rsi_filter_long <- data$rsi > rsi_long  
    data$rsi_filter_short <- data$rsi < rsi_short   
  } else {
    data$rsi_filter_long <- TRUE
    data$rsi_filter_short <- TRUE
  }
  
  # RSI Momentum Filter (erste Ableitung)
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

# ------------------------------------------------------------------------------
# 3. PERFORMANCE BERECHNEN
# ------------------------------------------------------------------------------

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
  
  # === NEUE METRIKEN ===
  
  # 1. Profit Factor
  gross_profit <- sum(data_clean$strategy_return[data_clean$strategy_return > 0], na.rm = TRUE)
  gross_loss <- abs(sum(data_clean$strategy_return[data_clean$strategy_return < 0], na.rm = TRUE))
  profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)
  
  # 2. Maximum Drawdown
  cumulative_returns <- cumsum(data_clean$strategy_return)
  running_max <- cummax(cumulative_returns)
  drawdown <- cumulative_returns - running_max
  max_drawdown <- min(drawdown, na.rm = TRUE)
  max_drawdown_pct <- (exp(max_drawdown) - 1) * 100  # In Prozent
  
  # 3. Win Rate
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
# BACKTEST-WRAPPER FUNKTION
# ==============================================================================

run_backtest <- function(data, params) {
  
  # Indikatoren berechnen
  data <- calculate_indicators(
    data,
    conversion_period = params$conversion_period,
    base_period = params$base_period,
    leading_span_b = params$leading_span_b,
    displacement = params$displacement,
    atr_period = params$atr_period,
    adx_period = params$adx_period
  )
  
  # Signale generieren - KORRIGIERT
  data <- generate_signals(
    data,
    stop_loss_atr_mult = params$stop_loss_atr_mult,
    take_profit_atr_mult = params$take_profit_atr_mult,
    close_time_hour = params$close_time_hour,
    adx_threshold_long = params$adx_threshold_long,    
    adx_threshold_short = params$adx_threshold_short,  
    use_rsi_filter = params$use_rsi_filter,            
    rsi_short = params$rsi_short,                      
    rsi_long = params$rsi_long,                        
    use_rsi_momentum = params$use_rsi_momentum,        
    rsi_momentum_threshold = params$rsi_momentum_threshold,  
    use_adx_momentum = params$use_adx_momentum,        
    adx_momentum_threshold = params$adx_momentum_threshold   
  )
  
  # Performance berechnen
  perf <- calculate_performance(data)
  
  return(perf)
}

# ==============================================================================
# PARAMETER-OPTIMIERUNG
# ==============================================================================

# ==============================================================================
# PARAMETER-OPTIMIERUNG (PARALLEL-FIX)
# ==============================================================================

optimize_parameters <- function(data, 
                                param_grid, 
                                metric = "sharpe_ratio",
                                min_trades = 10,
                                parallel = TRUE) {
  
  cat(sprintf("Starte Parameter-Optimierung mit %d Kombinationen...\n", nrow(param_grid)))
  
  if (parallel) {
    library(parallel)
    library(doParallel)
    library(foreach)
    
    n_cores <- detectCores() - 1
    cat(sprintf("Nutze %d CPU-Kerne\n", n_cores))
    
    cl <- makeCluster(n_cores)
    
    clusterExport(cl, 
                  varlist = c("calculate_indicators", 
                              "generate_signals", 
                              "calculate_performance", 
                              "run_backtest",
                              "data"),
                  envir = environment())
    
    clusterEvalQ(cl, {
      library(tidyverse)
      library(TTR)
      library(lubridate)
    })
    
    registerDoParallel(cl)
    
    results_list <- foreach(
      i = 1:nrow(param_grid), 
      .packages = c("tidyverse", "TTR", "lubridate"),
      .errorhandling = "pass",
      .verbose = FALSE
    ) %dopar% {
      
      params <- as.list(param_grid[i, ])
      
      tryCatch({
        perf <- run_backtest(data, params)
        
        if (perf$n_trades >= min_trades && 
            is.finite(perf[[metric]]) && 
            !is.na(perf[[metric]])) {
          
          # KORRIGIERT - alle Parameter aus param_grid extrahieren
          data.frame(
            iteration = i,
            conversion_period = params$conversion_period,
            base_period = params$base_period,
            leading_span_b = params$leading_span_b,
            displacement = params$displacement,
            atr_period = params$atr_period,
            adx_period = params$adx_period,
            stop_loss_atr_mult = params$stop_loss_atr_mult,
            take_profit_atr_mult = params$take_profit_atr_mult,
            close_time_hour = params$close_time_hour,
            adx_threshold_long = params$adx_threshold_long,      # ← GEÄNDERT
            adx_threshold_short = params$adx_threshold_short,    # ← GEÄNDERT
            use_rsi_filter = params$use_rsi_filter,              # ← NEU
            rsi_short = params$rsi_short,                        # ← NEU
            rsi_long = params$rsi_long,                          # ← NEU
            use_rsi_momentum = params$use_rsi_momentum,          # ← NEU
            rsi_momentum_threshold = params$rsi_momentum_threshold,  # ← NEU
            use_adx_momentum = params$use_adx_momentum,          # ← NEU
            adx_momentum_threshold = params$adx_momentum_threshold,  # ← NEU
            sharpe_ratio = perf$sharpe_ratio,
            total_return = perf$total_return,
            n_trades = perf$n_trades,
            win_rate = perf$win_rate,
            profit_factor = perf$profit_factor,
            max_drawdown = perf$max_drawdown,
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      }, error = function(e) {
        list(error = TRUE, message = e$message, iteration = i)
      })
    }
    
    stopCluster(cl)
    
    # Prüfe auf Fehler
    errors <- sapply(results_list, function(x) {
      if (is.list(x) && !is.data.frame(x) && !is.null(x$error)) {
        return(TRUE)
      }
      return(FALSE)
    })
    
    if (any(errors)) {
      error_messages <- results_list[errors]
      cat("\n⚠ Einige Iterationen hatten Fehler:\n")
      for (err in head(error_messages, 3)) {
        cat(sprintf("  Iteration %d: %s\n", err$iteration, err$message))
      }
    }
    
    results_list <- results_list[sapply(results_list, is.data.frame)]
    
    if (length(results_list) > 0) {
      results <- bind_rows(results_list)
    } else {
      results <- NULL
    }
    
  } else {
    # Sequenzielle Verarbeitung - AUCH HIER KORRIGIEREN
    results_list <- list()
    
    pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)
    
    for (i in 1:nrow(param_grid)) {
      params <- as.list(param_grid[i, ])
      
      tryCatch({
        perf <- run_backtest(data, params)
        
        if (perf$n_trades >= min_trades && 
            is.finite(perf[[metric]]) && 
            !is.na(perf[[metric]])) {
          
          results_list[[length(results_list) + 1]] <- data.frame(
            iteration = i,
            conversion_period = params$conversion_period,
            base_period = params$base_period,
            leading_span_b = params$leading_span_b,
            displacement = params$displacement,
            atr_period = params$atr_period,
            adx_period = params$adx_period,
            stop_loss_atr_mult = params$stop_loss_atr_mult,
            take_profit_atr_mult = params$take_profit_atr_mult,
            close_time_hour = params$close_time_hour,
            adx_threshold_long = params$adx_threshold_long,      # ← GEÄNDERT
            adx_threshold_short = params$adx_threshold_short,    # ← GEÄNDERT
            use_rsi_filter = params$use_rsi_filter,              # ← NEU
            rsi_short = params$rsi_short,                        # ← NEU
            rsi_long = params$rsi_long,                          # ← NEU
            use_rsi_momentum = params$use_rsi_momentum,          # ← NEU
            rsi_momentum_threshold = params$rsi_momentum_threshold,  # ← NEU
            use_adx_momentum = params$use_adx_momentum,          # ← NEU
            adx_momentum_threshold = params$adx_momentum_threshold,  # ← NEU
            sharpe_ratio = perf$sharpe_ratio,
            total_return = perf$total_return,
            n_trades = perf$n_trades,
            win_rate = perf$win_rate,
            profit_factor = perf$profit_factor,
            max_drawdown = perf$max_drawdown,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        cat(sprintf("\nFehler bei Iteration %d: %s\n", i, e$message))
      })
      
      setTxtProgressBar(pb, i)
    }
    
    close(pb)
    
    if (length(results_list) > 0) {
      results <- bind_rows(results_list)
    } else {
      results <- NULL
    }
  }
  
  if (is.null(results) || nrow(results) == 0) {
    stop("Keine validen Ergebnisse gefunden! Mögliche Gründe:\n",
         "  - Zu wenige Trades (min_trades = ", min_trades, ")\n",
         "  - Alle Metriken sind NA/Inf\n",
         "  - Fehler in der Berechnung\n",
         "  Versuche: min_trades zu reduzieren oder Parameter-Grid anzupassen")
  }
  
  cat(sprintf("\n✓ %d von %d Kombinationen waren valide\n", 
              nrow(results), nrow(param_grid)))
  
  results <- results %>%
    arrange(desc(.data[[metric]])) %>%
    mutate(rank = row_number())
  
  return(results)
}

# ==============================================================================
# TESTDATEN DETAILLIERTE TRADES
# ==============================================================================

calculate_performance_detailed <- function(data, initial_capital = 300, contracts = 1, lot_size = 1.2, spread_pips = 35) {
  
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
  
  # === TRADE-BY-TRADE ANALYSE ===
  
  # Identifiziere Trade-Starts und -Ends
  data_clean$position_change <- c(0, diff(data_clean$position))
  trade_starts <- which(data_clean$position_change != 0 & data_clean$position != 0)
  trade_ends <- which(data_clean$position_change != 0 & lag(data_clean$position) != 0)
  
  # Falls letzter Trade noch offen ist
  if(length(trade_starts) > length(trade_ends)) {
    trade_ends <- c(trade_ends, nrow(data_clean))
  }
  
  # Trade-Liste erstellen
  trades_list <- list()
  current_capital <- initial_capital
  total_spread_costs <- 0
  
  if(length(trade_starts) > 0 && length(trade_starts) == length(trade_ends)) {
    for(i in 1:length(trade_starts)) {
      start_idx <- trade_starts[i]
      end_idx <- trade_ends[i]
      
      # Trade Informationen
      trade_direction <- data_clean$position[start_idx]
      entry_price <- data_clean$close[start_idx]
      exit_price <- data_clean$close[end_idx]
      
      # Stop Loss und Take Profit aus den Daten
      sl_price <- data_clean$stop_loss[start_idx]
      tp_price <- data_clean$take_profit[start_idx]
      
      # Exit-Grund bestimmen
      exit_reason <- NA
      if(!is.na(tp_price) && !is.na(sl_price)) {
        if(trade_direction == 1) {  # Long
          if(exit_price >= tp_price) {
            exit_reason <- "TP"
          } else if(exit_price <= sl_price) {
            exit_reason <- "SL"
          } else {
            exit_reason <- "Time"
          }
        } else if(trade_direction == -1) {  # Short
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
      
      # Feste Positionsgröße verwenden
      position_size <- lot_size
      
      # Für Gold (XAU/USD): 1 Pip = 0.01 Dollar
      contract_size <- contracts
      pip_value <- 0.01  # Bei Gold ist 1 Pip = 0.01 Dollar
      
      # Spread-Kosten berechnen
      # Spread wird beim Entry UND Exit bezahlt
      spread_cost <- spread_pips * pip_value * lot_size * contract_size
      
      # P&L Berechnung mit fester Lot Size
      price_diff <- ifelse(trade_direction == 1, 
                           exit_price - entry_price,  # Long
                           entry_price - exit_price)  # Short
      
      # Brutto P&L (ohne Spread)
      gross_pnl <- price_diff * lot_size * contract_size
      
      # Netto P&L (nach Spread-Abzug)
      net_pnl <- gross_pnl - spread_cost
      
      # Trade Return relativ zum Kapital
      trade_return_pct <- net_pnl / current_capital
      
      # Kapital Update
      current_capital <- current_capital + net_pnl
      total_spread_costs <- total_spread_costs + spread_cost
      
      # Risiko in Dollar beim Entry
      risk_distance <- abs(entry_price - sl_price)
      risk_dollar <- risk_distance * lot_size * contract_size
      risk_percent <- (risk_dollar / (current_capital - net_pnl)) * 100
      
      # Trade zur Liste hinzufügen
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
        trade_return_pct = trade_return_pct * 100,  # in Prozent
        capital_before = current_capital - net_pnl,
        capital_after = current_capital,
        risk_dollar = risk_dollar,
        risk_percent = risk_percent,
        exit_reason = exit_reason,
        duration_bars = end_idx - start_idx + 1
      )
    }
    
    # Alle Trades in einem DataFrame zusammenfassen
    trades_df <- do.call(rbind, trades_list)
    
    # Zusätzliche Trade-Statistiken (mit Spread)
    winning_trades_net <- sum(trades_df$net_pnl > 0, na.rm = TRUE)
    losing_trades_net <- sum(trades_df$net_pnl < 0, na.rm = TRUE)
    win_rate_net <- winning_trades_net / (winning_trades_net + losing_trades_net)
    
    avg_win <- mean(trades_df$net_pnl[trades_df$net_pnl > 0], na.rm = TRUE)
    avg_loss <- mean(trades_df$net_pnl[trades_df$net_pnl < 0], na.rm = TRUE)
    largest_win <- max(trades_df$net_pnl, na.rm = TRUE)
    largest_loss <- min(trades_df$net_pnl, na.rm = TRUE)
    avg_risk_percent <- mean(trades_df$risk_percent, na.rm = TRUE)
    
    # Profit Factor mit Spread
    gross_profit_net <- sum(trades_df$net_pnl[trades_df$net_pnl > 0], na.rm = TRUE)
    gross_loss_net <- abs(sum(trades_df$net_pnl[trades_df$net_pnl < 0], na.rm = TRUE))
    profit_factor_net <- ifelse(gross_loss_net > 0, gross_profit_net / gross_loss_net, NA)
    
    # Exit-Grund Statistiken
    exit_stats <- table(trades_df$exit_reason)
    
    # Spread Impact Analysis
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
    profit_factor_net = profit_factor_net,  # Nach Spread
    max_drawdown = max_drawdown,
    max_drawdown_pct = max_drawdown_pct,
    win_rate = win_rate,
    win_rate_net = win_rate_net,  # Nach Spread
    winning_trades = winning_trades,
    losing_trades = losing_trades,
    winning_trades_net = winning_trades_net,
    losing_trades_net = losing_trades_net,
    # Trade-by-Trade Details
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
run_backtest_detailed <- function(data, params) {
  
  data <- calculate_indicators(
    data,
    conversion_period = params$conversion_period,
    base_period = params$base_period,
    leading_span_b = params$leading_span_b,
    displacement = params$displacement,
    atr_period = params$atr_period,
    adx_period = params$adx_period
  )
  
  # KORRIGIERT - alle Parameter hinzufügen
  data <- generate_signals(
    data,
    stop_loss_atr_mult = params$stop_loss_atr_mult,
    take_profit_atr_mult = params$take_profit_atr_mult,
    close_time_hour = params$close_time_hour,
    adx_threshold_long = params$adx_threshold_long,
    adx_threshold_short = params$adx_threshold_short,
    use_rsi_filter = params$use_rsi_filter,
    rsi_short = params$rsi_short,
    rsi_long = params$rsi_long,
    use_rsi_momentum = params$use_rsi_momentum,
    rsi_momentum_threshold = params$rsi_momentum_threshold,
    use_adx_momentum = params$use_adx_momentum,
    adx_momentum_threshold = params$adx_momentum_threshold
  )
  
  perf <- calculate_performance_detailed(data)
  
  return(perf)
}

# ==============================================================================
# BEISPIEL-VERWENDUNG
# ==============================================================================

# Parameter-Grid erstellen
param_grid <- expand.grid(
  conversion_period = seq(5,30,4),
  base_period = seq(10,40,4),
  leading_span_b = seq(30,70,5),
  displacement = seq(10,40,5),
  use_rsi_filter = TRUE,
  rsi_long = c(30,55),
  rsi_short = c(45,70),
  use_rsi_momentum = FALSE,
  rsi_momentum_threshold = 0,
  atr_period = 14,
  adx_period = 13,
  stop_loss_atr_mult = 3.0,
  take_profit_atr_mult = 4.5,
  close_time_hour = 22,
  use_adx_momentum = FALSE,
  adx_momentum_threshold = 0,
  adx_threshold_long = 19,
  adx_threshold_short = 21
)

param_grid = param_grid %>%
  filter(conversion_period < base_period,
         displacement < leading_span_b)

cat(sprintf("Parameter-Grid enthält %d Kombinationen\n", nrow(param_grid)))

# ==============================================================================
# TESTEN OB DIE LOOP FUNKTIONIERT
# ==============================================================================

params_test = param_grid[1,]

loop_test = calculate_indicators(test_data)
loop_test = generate_signals(loop_test)
loop_results = calculate_performance(loop_test)

backtest_loop = run_backtest(test_data, params_test)

# ==============================================================================
# OPTIMIERUNG
# ==============================================================================

tic()
# Optimierung durchführen
optimization_results <- optimize_parameters(
  data = train_data,
  param_grid = param_grid,
  metric = "sharpe_ratio",
  min_trades = 20,
  parallel = TRUE
)
toc()

# Beste Parameter anzeigen
cat("\nTop 10 Parameter-Kombinationen:\n")
print(head(optimization_results, 10))

# Beste Parameter extrahieren
best_params <- as.list(optimization_results[1, ])


# Test Daten Validierung --------------------------------------------------

# Validierung auf Test-Daten
cat("\nValidierung auf Test-Daten...\n")
test_perf <- run_backtest_detailed(test_data, best_params)

print(test_perf$total_return)
print(test_perf$profit_factor_net)
print(test_perf$sharpe_ratio)
print(test_perf$win_rate)

test_result_data = test_perf$data
test_result_data = test_result_data %>%
  select(date:low, signal_raw:position_change)
trades_test = test_perf$trades

capital_beginning = head(trades_test$capital_before, 1)
capital_end = tail(trades_test$capital_after, 1)

print(((capital_end-capital_beginning)*100)/capital_beginning)
print(capital_end-capital_beginning)
