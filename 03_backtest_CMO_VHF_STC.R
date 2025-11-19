# ==============================================================================
# Backtest - CMO-VHF-STC Strategy
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
cat("#      Backtest für CMO-VHF-STC Strategie (15 Min Intraday)          #\n")
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

# Source Strategy Functions (für die Indikator-Berechnungen)
source(file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "xx_strategies_signals.R"))

# ==============================================================================
# SCHRITT 1: DATEN LADEN
# ==============================================================================

cat("SCHRITT 1: Daten laden\n")
cat("----------------------------------------\n")

epic_data <- read_csv(file.path(input_path, filename), show_col_types = FALSE)
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
# Indikatoren Funktionen
# ==============================================================================

# Schaff Trend Cycle
calculate_stc <- function(close, fast = 23, slow = 50, cycle = 10, smooth1 = 3, smooth2 = 3) {
  # Calculate MACD
  macd_line <- EMA(close, fast) - EMA(close, slow)
  
  # First stochastic
  macd_min <- runMin(macd_line, cycle)
  macd_max <- runMax(macd_line, cycle)
  stoch1 <- 100 * (macd_line - macd_min) / (macd_max - macd_min)
  stoch1[is.na(stoch1) | is.infinite(stoch1)] <- 50
  stoch1_smooth <- EMA(stoch1, smooth1)
  
  # Second stochastic
  stoch1_min <- runMin(stoch1_smooth, cycle)
  stoch1_max <- runMax(stoch1_smooth, cycle)
  stc <- 100 * (stoch1_smooth - stoch1_min) / (stoch1_max - stoch1_min)
  stc[is.na(stc) | is.infinite(stc)] <- 50
  stc <- EMA(stc, smooth2)
  
  return(as.numeric(stc))
}

# Vertical Horizontal Filter
calculate_vhf <- function(close, n = 28) {
  highest <- runMax(close, n)
  lowest <- runMin(close, n)
  sum_changes <- runSum(abs(diff(c(NA, close))), n)
  
  vhf <- (highest - lowest) / sum_changes
  vhf[is.infinite(vhf)] <- NA
  return(as.numeric(vhf))
}

# Chande Momentum Oscillator
calculate_cmo <- function(close, n = 14) {
  cmo <- CMO(close, n)
  return(as.numeric(cmo))
}



# ==============================================================================
# INDIKATOREN BERECHNEN
# ==============================================================================

calculate_indicators <- function(data, 
                                 cmo_period = 14,
                                 vhf_period = 28,
                                 stc_fast = 23,
                                 stc_slow = 50,
                                 stc_cycle = 10,
                                 atr_period = 14,
                                 adx_period = 14) {
  
  # CMO - Chande Momentum Oscillator
  data$cmo <- calculate_cmo(data$close, n = cmo_period)
  
  # VHF - Vertical Horizontal Filter
  data$vhf <- calculate_vhf(data$close, n = vhf_period)
  
  # STC - Schaff Trend Cycle
  data$stc <- calculate_stc(data$close, fast = stc_fast, slow = stc_slow, cycle = stc_cycle)
  
  # ATR für Stop Loss / Take Profit
  data$atr <- ATR(cbind(data$high, data$low, data$close), n = atr_period)[, "atr"]
  
  # ADX (optional, falls du es als zusätzlichen Filter nutzen willst)
  data$adx <- ADX(cbind(data$high, data$low, data$close), n = adx_period)[, "ADX"]
  
  return(data)
}

# ==============================================================================
# SIGNALE GENERIEREN
# ==============================================================================

generate_signals <- function(data,
                             cmo_threshold = 20,
                             vhf_threshold = 0.35,
                             stc_entry_long = 25,
                             stc_entry_short = 75,
                             stop_loss_atr_mult = 3.0,
                             take_profit_atr_mult = 4.5,
                             close_time_hour = 22,
                             use_price_confirmation = TRUE,
                             use_adx_filter = FALSE,
                             adx_threshold = 20) {
  
  # Initialisiere Signal-Vektoren
  n <- nrow(data)
  data$signal_raw <- rep(0, n)
  
  # Generiere Entry-Signale
  for(i in 2:n) {
    if(is.na(data$cmo[i]) || is.na(data$vhf[i]) || 
       is.na(data$stc[i]) || is.na(data$stc[i-1])) next
    
    # Setup: Market must be trending + CMO shows momentum
    market_trending <- data$vhf[i] > vhf_threshold
    bullish_momentum <- data$cmo[i] > cmo_threshold
    bearish_momentum <- data$cmo[i] < -cmo_threshold
    
    # Optionaler ADX Filter
    adx_ok <- TRUE
    if(use_adx_filter && !is.na(data$adx[i])) {
      adx_ok <- data$adx[i] >= adx_threshold
    }
    
    # Price Confirmation
    price_conf_long <- TRUE
    price_conf_short <- TRUE
    if(use_price_confirmation) {
      price_conf_long <- data$close[i] > data$close[i-1]
      price_conf_short <- data$close[i] < data$close[i-1]
    }
    
    # Long: STC crosses 25 + setup conditions
    if(market_trending && bullish_momentum && adx_ok && price_conf_long &&
       data$stc[i-1] < stc_entry_long && data$stc[i] > stc_entry_long) {
      data$signal_raw[i] <- 1
    }
    
    # Short: STC crosses 75 + setup conditions
    if(market_trending && bearish_momentum && adx_ok && price_conf_short &&
       data$stc[i-1] > stc_entry_short && data$stc[i] < stc_entry_short) {
      data$signal_raw[i] <- -1
    }
  }
  
  # Initialisiere Position-Management Vektoren
  position <- numeric(n)
  entry_price <- rep(NA_real_, n)
  stop_loss <- rep(NA_real_, n)
  take_profit <- rep(NA_real_, n)
  
  # Aktuelle Trade-Variablen
  current_position <- 0
  current_entry <- NA_real_
  current_sl <- NA_real_
  current_tp <- NA_real_
  
  # Position-Management Loop
  for (i in 2:n) {
    
    # 22:00 Uhr Close Check
    if (!is.na(data$hour[i]) && data$hour[i] == close_time_hour && current_position != 0) {
      current_position <- 0
      current_entry <- NA_real_
      current_sl <- NA_real_
      current_tp <- NA_real_
    }
    # Entry Logic
    else if (current_position == 0 && data$signal_raw[i] != 0) {
      
      current_position <- data$signal_raw[i]
      current_entry <- data$close[i]
      
      if (!is.na(data$atr[i])) {
        if (current_position == 1) {  # Long
          current_sl <- current_entry - (stop_loss_atr_mult * data$atr[i])
          current_tp <- current_entry + (take_profit_atr_mult * data$atr[i])
        } else {  # Short
          current_sl <- current_entry + (stop_loss_atr_mult * data$atr[i])
          current_tp <- current_entry - (take_profit_atr_mult * data$atr[i])
        }
      }
    }
    # Exit Logic
    else if (current_position != 0 && !is.na(current_sl) && !is.na(current_tp)) {
      
      if (current_position == 1) {  # Long Exit
        if (data$low[i] <= current_sl || data$high[i] >= current_tp) {
          current_position <- 0
          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
        }
      } else {  # Short Exit
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
# PERFORMANCE BERECHNEN
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
# BACKTEST-WRAPPER FUNKTION
# ==============================================================================

run_backtest <- function(data, params) {
  
  # Indikatoren berechnen
  data <- calculate_indicators(
    data,
    cmo_period = params$cmo_period,
    vhf_period = params$vhf_period,
    stc_fast = params$stc_fast,
    stc_slow = params$stc_slow,
    stc_cycle = params$stc_cycle,
    atr_period = params$atr_period,
    adx_period = params$adx_period
  )
  
  # Signale generieren
  data <- generate_signals(
    data,
    cmo_threshold = params$cmo_threshold,
    vhf_threshold = params$vhf_threshold,
    stc_entry_long = params$stc_entry_long,
    stc_entry_short = params$stc_entry_short,
    stop_loss_atr_mult = params$stop_loss_atr_mult,
    take_profit_atr_mult = params$take_profit_atr_mult,
    close_time_hour = params$close_time_hour,
    use_price_confirmation = params$use_price_confirmation,
    use_adx_filter = params$use_adx_filter,
    adx_threshold = params$adx_threshold
  )
  
  # Performance berechnen
  perf <- calculate_performance(data)
  
  return(perf)
}

# ==============================================================================
# PARAMETER-OPTIMIERUNG
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
                              "data",
                              "calculate_cmo",
                              "calculate_vhf",
                              "calculate_stc"),
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
          
          data.frame(
            iteration = i,
            cmo_period = params$cmo_period,
            vhf_period = params$vhf_period,
            stc_fast = params$stc_fast,
            stc_slow = params$stc_slow,
            stc_cycle = params$stc_cycle,
            cmo_threshold = params$cmo_threshold,
            vhf_threshold = params$vhf_threshold,
            stc_entry_long = params$stc_entry_long,
            stc_entry_short = params$stc_entry_short,
            atr_period = params$atr_period,
            adx_period = params$adx_period,
            stop_loss_atr_mult = params$stop_loss_atr_mult,
            take_profit_atr_mult = params$take_profit_atr_mult,
            close_time_hour = params$close_time_hour,
            use_price_confirmation = params$use_price_confirmation,
            use_adx_filter = params$use_adx_filter,
            adx_threshold = params$adx_threshold,
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
    # Sequenzielle Verarbeitung
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
            cmo_period = params$cmo_period,
            vhf_period = params$vhf_period,
            stc_fast = params$stc_fast,
            stc_slow = params$stc_slow,
            stc_cycle = params$stc_cycle,
            cmo_threshold = params$cmo_threshold,
            vhf_threshold = params$vhf_threshold,
            stc_entry_long = params$stc_entry_long,
            stc_entry_short = params$stc_entry_short,
            atr_period = params$atr_period,
            adx_period = params$adx_period,
            stop_loss_atr_mult = params$stop_loss_atr_mult,
            take_profit_atr_mult = params$take_profit_atr_mult,
            close_time_hour = params$close_time_hour,
            use_price_confirmation = params$use_price_confirmation,
            use_adx_filter = params$use_adx_filter,
            adx_threshold = params$adx_threshold,
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
    stop("Keine validen Ergebnisse gefunden!")
  }
  
  cat(sprintf("\n✓ %d von %d Kombinationen waren valide\n", 
              nrow(results), nrow(param_grid)))
  
  results <- results %>%
    arrange(desc(.data[[metric]])) %>%
    mutate(rank = row_number())
  
  return(results)
}

# ==============================================================================
# DETAILLIERTE PERFORMANCE FÜR TESTDATEN
# ==============================================================================

calculate_performance_detailed <- function(data, initial_capital = 300, 
                                           contracts = 1, lot_size = 1.2, 
                                           spread_pips = 35) {
  
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

run_backtest_detailed <- function(data, params) {
  
  data <- calculate_indicators(
    data,
    cmo_period = params$cmo_period,
    vhf_period = params$vhf_period,
    stc_fast = params$stc_fast,
    stc_slow = params$stc_slow,
    stc_cycle = params$stc_cycle,
    atr_period = params$atr_period,
    adx_period = params$adx_period
  )
  
  data <- generate_signals(
    data,
    cmo_threshold = params$cmo_threshold,
    vhf_threshold = params$vhf_threshold,
    stc_entry_long = params$stc_entry_long,
    stc_entry_short = params$stc_entry_short,
    stop_loss_atr_mult = params$stop_loss_atr_mult,
    take_profit_atr_mult = params$take_profit_atr_mult,
    close_time_hour = params$close_time_hour,
    use_price_confirmation = params$use_price_confirmation,
    use_adx_filter = params$use_adx_filter,
    adx_threshold = params$adx_threshold
  )
  
  perf <- calculate_performance_detailed(data)
  
  return(perf)
}

# ==============================================================================
# BEISPIEL-VERWENDUNG
# ==============================================================================

# Parameter-Grid erstellen
param_grid <- expand.grid(
  cmo_period = c(12,14,16),
  cmo_threshold = c(17,20,23),
  vhf_period = 28,
  vhf_threshold = c(0.2,0.4),
  stc_fast = c(18, 23, 28),
  stc_slow = c(33,36,40),
  stc_cycle = c(8, 10, 12),
  stc_entry_long = 25,
  stc_entry_short = 75,
  atr_period = 14,
  adx_period = 14,
  stop_loss_atr_mult = c(2.5,3),
  take_profit_atr_mult = c(4.0,4.5),
  close_time_hour = 22,
  use_price_confirmation = TRUE,
  use_adx_filter = FALSE,
  adx_threshold = 20
)

# Zusätzliche Filter für das Grid
param_grid <- param_grid %>%
  filter(stc_fast < stc_slow,
         stc_entry_long < stc_entry_short)

cat(sprintf("Parameter-Grid enthält %d Kombinationen\n", nrow(param_grid)))

# ==============================================================================
# TEST OB DIE FUNKTIONEN FUNKTIONIEREN
# ==============================================================================

params_test <- param_grid[1,]

loop_test <- calculate_indicators(test_data)
loop_test <- generate_signals(loop_test)
loop_results <- calculate_performance(loop_test)

backtest_loop <- run_backtest(test_data, params_test)

cat("\nTest-Backtest abgeschlossen:\n")
cat(sprintf("Sharpe Ratio: %.2f\n", backtest_loop$sharpe_ratio))
cat(sprintf("Total Return: %.2f%%\n", backtest_loop$total_return * 100))
cat(sprintf("Trades: %d\n", backtest_loop$n_trades))

# ==============================================================================
# OPTIMIERUNG DURCHFÜHREN
# ==============================================================================

tic()
optimization_results <- optimize_parameters(
  data = train_data,
  param_grid = param_grid,
  metric = "sharpe_ratio",
  min_trades = 20,
  parallel = TRUE
)
toc()

# ==============================================================================
# VALIDIERUNG DER TOP 10 PARAMETER AUF TEST-DATEN
# ==============================================================================
cat("\n========================================\n")
cat("VALIDIERUNG TOP 10 PARAMETER AUF TEST-DATEN\n")
cat("========================================\n")

# Top 10 Parametersätze extrahieren
top_10_params <- head(optimization_results, 10)

# Dataframe für Zusammenfassung erstellen
top_10_summary <- data.frame(
  Rank = integer(),
  Total_Return_Pct = numeric(),
  Profit_Factor = numeric(),
  Sharpe_Ratio = numeric(),
  Win_Rate_Pct = numeric(),
  Max_DD_Pct = numeric(),
  N_Trades = integer(),
  Start_Capital = numeric(),
  End_Capital = numeric(),
  PnL = numeric(),
  stringsAsFactors = FALSE
)

# Liste für detaillierte Ergebnisse
detailed_results <- list()

# Durch alle Top 10 Parameter iterieren
for(i in 1:nrow(top_10_params)) {
  cat(sprintf("\n--- Rang %d ---\n", i))
  
  # Parameter extrahieren
  params <- as.list(top_10_params[i, ])
  
  # Backtest durchführen
  test_perf <- run_backtest_detailed(test_data, params)
  
  # Ergebnisse ausgeben
  cat(sprintf("Total Return: %.2f%%\n", test_perf$total_return * 100))
  cat(sprintf("Profit Factor (netto): %.2f\n", test_perf$profit_factor_net))
  cat(sprintf("Sharpe Ratio: %.2f\n", test_perf$sharpe_ratio))
  cat(sprintf("Win Rate: %.2f%%\n", test_perf$win_rate * 100))
  cat(sprintf("Max Drawdown: %.2f%%\n", test_perf$max_drawdown_pct))
  cat(sprintf("Anzahl Trades: %d\n", test_perf$n_trades))
  
  # Trades extrahieren
  trades_test <- test_perf$trades
  
  if(!is.null(trades_test) && nrow(trades_test) > 0) {
    capital_beginning <- head(trades_test$capital_before, 1)
    capital_end <- tail(trades_test$capital_after, 1)
    pnl <- capital_end - capital_beginning
    
    cat(sprintf("\nStart-Kapital: $%.2f\n", capital_beginning))
    cat(sprintf("End-Kapital: $%.2f\n", capital_end))
    cat(sprintf("Profit/Loss: $%.2f (%.2f%%)\n", 
                pnl, (pnl / capital_beginning) * 100))
    
    cat("\nExit-Statistik:\n")
    print(test_perf$exit_stats)
    
    # Zusammenfassung hinzufügen
    top_10_summary <- rbind(top_10_summary, data.frame(
      Rank = i,
      Total_Return_Pct = test_perf$total_return * 100,
      Profit_Factor = test_perf$profit_factor_net,
      Sharpe_Ratio = test_perf$sharpe_ratio,
      Win_Rate_Pct = test_perf$win_rate * 100,
      Max_DD_Pct = test_perf$max_drawdown_pct,
      N_Trades = test_perf$n_trades,
      Start_Capital = capital_beginning,
      End_Capital = capital_end,
      PnL = pnl
    ))
  } else {
    cat("\nKeine Trades vorhanden.\n")
    
    # Zusammenfassung mit NA-Werten
    top_10_summary <- rbind(top_10_summary, data.frame(
      Rank = i,
      Total_Return_Pct = test_perf$total_return * 100,
      Profit_Factor = test_perf$profit_factor_net,
      Sharpe_Ratio = test_perf$sharpe_ratio,
      Win_Rate_Pct = test_perf$win_rate * 100,
      Max_DD_Pct = test_perf$max_drawdown_pct,
      N_Trades = test_perf$n_trades,
      Start_Capital = NA,
      End_Capital = NA,
      PnL = NA
    ))
  }
  
  # Detaillierte Ergebnisse speichern
  detailed_results[[i]] <- test_perf
  
  cat("\n")
}