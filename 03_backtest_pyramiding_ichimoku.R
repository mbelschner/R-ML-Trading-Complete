# ==============================================================================
# Backtest mit Partiellen Take Profits und Trailing Stop Loss
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

cat("\n")
cat("########################################################################\n")
cat("#                                                                      #\n")
cat("#      Backtest mit Partiellen TPs & Trailing Stop                    #\n")
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
# INDIKATOREN BERECHNEN
# ==============================================================================

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
  
  # RSI
  data$rsi <- RSI(data$close, n = 14)
  
  return(data)
}

# ==============================================================================
# SIGNALE MIT PARTIELLEN TAKE PROFITS UND TRAILING STOP
# ==============================================================================

generate_signals_partial_tp <- function(data,
                                        initial_lot_size = 1.2,      # Gesamte Position Size
                                        tp1_atr_mult = 3.0,          # Erster TP bei 3x ATR
                                        tp1_close_pct = 0.5,         # 50% bei TP1 schließen
                                        tp2_atr_mult = 6.0,          # Zweiter TP bei 6x ATR
                                        tp2_close_pct = 1.0,         # 100% vom Rest bei TP2
                                        trailing_atr_mult = 2.5,     # Trailing Stop bei 2.5x ATR
                                        displacement = 33,           # Displacement für Chikou Span
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
  
  # Entry-Signal Bedingungen
  
  # === ICHIMOKU KUMO LOGIC ===
  # Kumo (Cloud) boundaries - verwende displacement-1 für aktuelle Kumo Position
  kumo_high <- pmax(data$senkou_span_a, data$senkou_span_b, na.rm = TRUE)
  kumo_low <- pmin(data$senkou_span_a, data$senkou_span_b, na.rm = TRUE)
  
  # Preis über/unter Kumo
  data$price_above_kumo <- data$close > kumo_high
  data$price_below_kumo <- data$close < kumo_low
  
  # === CHIKOU SPAN FREE LOGIC ===
  # Chikou Span muss "frei" sein (keine Überschneidung mit historischem Preis)
  # Long: Aktueller Close > High von vor 'displacement' Bars
  # Short: Aktueller Close < Low von vor 'displacement' Bars
  data$chikou_free_long <- data$close > lag(data$high, displacement)
  data$chikou_free_short <- data$close < lag(data$low, displacement)
  
  # === TENKAN/KIJUN CROSSOVER ===
  data$tenkan_above_kijun <- data$tenkan_sen > data$kijun_sen
  data$tenkan_below_kijun <- data$tenkan_sen < data$kijun_sen
  
  data$adx_filter_long <- data$adx >= adx_threshold_long
  data$adx_filter_short <- data$adx >= adx_threshold_short
  
  if (use_adx_momentum) {
    data$adx_momentum <- data$adx - lag(data$adx, 1)
    data$adx_momentum_filter <- data$adx_momentum > adx_momentum_threshold
  } else {
    data$adx_momentum_filter <- TRUE
  }
  
  if (use_rsi_filter) {
    data$rsi_filter_long <- data$rsi > rsi_long  
    data$rsi_filter_short <- data$rsi < rsi_short   
  } else {
    data$rsi_filter_long <- TRUE
    data$rsi_filter_short <- TRUE
  }
  
  if (use_rsi_momentum) {
    data$rsi_momentum <- data$rsi - rsi[1]
    data$rsi_momentum_long <- data$rsi_momentum > rsi_momentum_threshold
    data$rsi_momentum_short <- data$rsi_momentum < -rsi_momentum_threshold
  } else {
    data$rsi_momentum_long <- TRUE
    data$rsi_momentum_short <- TRUE
  }
  
  # === ENTRY SIGNALS ===
  # Long: Tenkan > Kijun UND Preis über Kumo UND Chikou Free
  data$bullish_signal <- data$tenkan_above_kijun & 
    data$price_above_kumo & 
    data$chikou_free_long
  
  # Short: Tenkan < Kijun UND Preis unter Kumo UND Chikou Free
  data$bearish_signal <- data$tenkan_below_kijun & 
    data$price_below_kumo & 
    data$chikou_free_short
  
  # Initial Entry Signals mit allen Filtern
  data$signal_raw <- case_when(
    data$bullish_signal & 
      data$adx_filter_long & 
      data$adx_momentum_filter &
      data$rsi_filter_long & 
      data$rsi_momentum_long ~ 1,
    
    data$bearish_signal & 
      data$adx_filter_short & 
      data$adx_momentum_filter &
      data$rsi_filter_short & 
      data$rsi_momentum_short ~ -1,
    
    TRUE ~ 0
  )
  
  # Initialisierung
  n <- nrow(data)
  
  # Position Tracking
  position_direction <- numeric(n)     # 1 = Long, -1 = Short, 0 = keine Position
  remaining_lot_size <- numeric(n)     # Verbleibende Lot Size
  entry_price <- rep(NA_real_, n)      # Entry Preis
  
  # Take Profit Levels
  tp1_price <- rep(NA_real_, n)
  tp2_price <- rep(NA_real_, n)
  tp1_hit <- logical(n)                # Wurde TP1 erreicht?
  tp2_hit <- logical(n)                # Wurde TP2 erreicht?
  
  # Trailing Stop
  trailing_stop <- rep(NA_real_, n)
  highest_price <- rep(NA_real_, n)    # Für Long
  lowest_price <- rep(NA_real_, n)     # Für Short
  
  # Aktuelle Trade-Variablen
  current_direction <- 0
  current_remaining_size <- 0
  current_entry <- NA_real_
  current_tp1 <- NA_real_
  current_tp2 <- NA_real_
  current_tp1_hit <- FALSE
  current_tp2_hit <- FALSE
  current_trailing <- NA_real_
  current_highest <- NA_real_
  current_lowest <- NA_real_
  
  # Hauptschleife
  for (i in 2:n) {
    
    # === 1. TIME-BASED EXIT (22:00 Uhr) ===
    if (!is.na(data$hour[i]) && data$hour[i] == close_time_hour && current_direction != 0) {
      # Position schließen
      current_direction <- 0
      current_remaining_size <- 0
      current_entry <- NA_real_
      current_tp1 <- NA_real_
      current_tp2 <- NA_real_
      current_tp1_hit <- FALSE
      current_tp2_hit <- FALSE
      current_trailing <- NA_real_
      current_highest <- NA_real_
      current_lowest <- NA_real_
    }
    
    # === 2. INITIAL ENTRY ===
    else if (current_direction == 0 && !is.na(data$signal_raw[i]) && data$signal_raw[i] != 0) {
      
      current_direction <- data$signal_raw[i]
      current_remaining_size <- initial_lot_size
      current_entry <- data$close[i]
      
      if (!is.na(data$atr[i])) {
        if (current_direction == 1) {
          # Long Position
          current_tp1 <- current_entry + (tp1_atr_mult * data$atr[i])
          current_tp2 <- current_entry + (tp2_atr_mult * data$atr[i])
          current_trailing <- current_entry - (trailing_atr_mult * data$atr[i])
          current_highest <- data$high[i]
        } else {
          # Short Position
          current_tp1 <- current_entry - (tp1_atr_mult * data$atr[i])
          current_tp2 <- current_entry - (tp2_atr_mult * data$atr[i])
          current_trailing <- current_entry + (trailing_atr_mult * data$atr[i])
          current_lowest <- data$low[i]
        }
      }
      
      current_tp1_hit <- FALSE
      current_tp2_hit <- FALSE
    }
    
    # === 3. TAKE PROFIT MANAGEMENT ===
    else if (current_direction != 0 && !is.na(current_tp1)) {
      
      if (current_direction == 1) {
        # Long Position
        
        # Check TP1
        if (!current_tp1_hit && data$high[i] >= current_tp1) {
          current_tp1_hit <- TRUE
          # Reduziere Position um tp1_close_pct
          current_remaining_size <- current_remaining_size * (1 - tp1_close_pct)
        }
        
        # Check TP2 (nur wenn TP1 bereits getroffen)
        if (current_tp1_hit && !current_tp2_hit && data$high[i] >= current_tp2) {
          current_tp2_hit <- TRUE
          # Reduziere verbleibende Position um tp2_close_pct
          current_remaining_size <- current_remaining_size * (1 - tp2_close_pct)
        }
        
      } else {
        # Short Position
        
        # Check TP1
        if (!current_tp1_hit && data$low[i] <= current_tp1) {
          current_tp1_hit <- TRUE
          current_remaining_size <- current_remaining_size * (1 - tp1_close_pct)
        }
        
        # Check TP2
        if (current_tp1_hit && !current_tp2_hit && data$low[i] <= current_tp2) {
          current_tp2_hit <- TRUE
          current_remaining_size <- current_remaining_size * (1 - tp2_close_pct)
        }
      }
    }
    
    # === 4. TRAILING STOP UPDATE ===
    if (current_direction == 1 && !is.na(current_trailing)) {
      # Long: Update bei neuem High
      if (data$high[i] > current_highest) {
        current_highest <- data$high[i]
        new_trailing <- current_highest - (trailing_atr_mult * data$atr[i])
        # Trailing Stop kann nur steigen
        if (new_trailing > current_trailing) {
          current_trailing <- new_trailing
        }
      }
    } else if (current_direction == -1 && !is.na(current_trailing)) {
      # Short: Update bei neuem Low
      if (data$low[i] < current_lowest) {
        current_lowest <- data$low[i]
        new_trailing <- current_lowest + (trailing_atr_mult * data$atr[i])
        # Trailing Stop kann nur fallen
        if (new_trailing < current_trailing) {
          current_trailing <- new_trailing
        }
      }
    }
    
    # === 5. TRAILING STOP EXIT ===
    if (current_direction != 0 && !is.na(current_trailing)) {
      
      exit_triggered <- FALSE
      
      if (current_direction == 1) {
        # Long: Exit wenn Low unter Trailing Stop
        if (data$low[i] <= current_trailing) {
          exit_triggered <- TRUE
        }
      } else {
        # Short: Exit wenn High über Trailing Stop
        if (data$high[i] >= current_trailing) {
          exit_triggered <- TRUE
        }
      }
      
      if (exit_triggered) {
        current_direction <- 0
        current_remaining_size <- 0
        current_entry <- NA_real_
        current_tp1 <- NA_real_
        current_tp2 <- NA_real_
        current_tp1_hit <- FALSE
        current_tp2_hit <- FALSE
        current_trailing <- NA_real_
        current_highest <- NA_real_
        current_lowest <- NA_real_
      }
    }
    
    # === 6. STATE SPEICHERN ===
    position_direction[i] <- current_direction
    remaining_lot_size[i] <- current_remaining_size
    entry_price[i] <- current_entry
    tp1_price[i] <- current_tp1
    tp2_price[i] <- current_tp2
    tp1_hit[i] <- current_tp1_hit
    tp2_hit[i] <- current_tp2_hit
    trailing_stop[i] <- current_trailing
    highest_price[i] <- current_highest
    lowest_price[i] <- current_lowest
  }
  
  # Results in DataFrame speichern
  data$position_direction <- position_direction
  data$remaining_lot_size <- remaining_lot_size
  data$entry_price <- entry_price
  data$tp1_price <- tp1_price
  data$tp2_price <- tp2_price
  data$tp1_hit <- tp1_hit
  data$tp2_hit <- tp2_hit
  data$trailing_stop <- trailing_stop
  data$highest_price <- highest_price
  data$lowest_price <- lowest_price
  
  # Signal für Kompatibilität
  data$signal <- lag(data$position_direction, 1)
  data$signal[is.na(data$signal)] <- 0
  
  return(data)
}

# ==============================================================================
# PERFORMANCE BERECHNUNG MIT PARTIELLEN TPS
# ==============================================================================

calculate_performance_partial_tp <- function(data, 
                                             initial_capital = 1000,
                                             initial_lot_size = 1.2,
                                             tp1_close_pct = 0.5,
                                             tp2_close_pct = 0.5,
                                             contracts = 1,
                                             spread_pips = 35,
                                             close_time_hour = 22) {
  
  n <- nrow(data)
  
  # Identifiziere Trades
  data$position_change <- c(0, diff(data$position_direction))
  
  # Trade Starts: Wechsel von 0 zu ±1
  trade_starts <- which(data$position_change != 0 & data$position_direction != 0)
  
  # Trade Ends: Wechsel von ±1 zu 0
  trade_ends <- which(data$position_change != 0 & lag(data$position_direction) != 0)
  
  # Letzter Trade noch offen?
  if (length(trade_starts) > length(trade_ends)) {
    trade_ends <- c(trade_ends, n)
  }
  
  trades_list <- list()
  current_capital <- initial_capital
  total_spread_costs <- 0
  
  pip_value <- 0.01  # Gold: 1 Pip = 0.01 USD
  
  if (length(trade_starts) > 0 && length(trade_starts) == length(trade_ends)) {
    
    for (t in 1:length(trade_starts)) {
      start_idx <- trade_starts[t]
      end_idx <- trade_ends[t]
      
      trade_direction <- data$position_direction[start_idx]
      entry_pr <- data$entry_price[start_idx]
      
      # Sammle Trade-Slice
      trade_slice <- data[start_idx:end_idx, ]
      
      # Finde TP Hits
      tp1_idx <- which(trade_slice$tp1_hit & !lag(trade_slice$tp1_hit, default = FALSE))
      tp2_idx <- which(trade_slice$tp2_hit & !lag(trade_slice$tp2_hit, default = FALSE))
      
      # P&L Tracking
      total_pnl <- 0
      total_spread <- 0
      
      # Initial Spread Kosten
      initial_spread <- spread_pips * pip_value * initial_lot_size * contracts
      total_spread <- total_spread + initial_spread
      
      # Teilschließungen tracken
      tp1_executed <- FALSE
      tp2_executed <- FALSE
      tp1_pnl <- 0
      tp2_pnl <- 0
      final_pnl <- 0
      
      tp1_exit_price <- NA
      tp2_exit_price <- NA
      final_exit_price <- data$close[end_idx]
      
      # TP1 Teilschließung
      if (length(tp1_idx) > 0) {
        tp1_executed <- TRUE
        tp1_bar <- start_idx + tp1_idx[1] - 1
        tp1_exit_price <- data$tp1_price[tp1_bar]
        
        # Lot Size für TP1
        tp1_lot_size <- initial_lot_size * tp1_close_pct
        
        if (trade_direction == 1) {
          price_diff_tp1 <- tp1_exit_price - entry_pr
        } else {
          price_diff_tp1 <- entry_pr - tp1_exit_price
        }
        
        tp1_pnl <- price_diff_tp1 * tp1_lot_size * contracts
        
        # Spread für TP1 Exit
        tp1_spread <- spread_pips * pip_value * tp1_lot_size * contracts
        total_spread <- total_spread + tp1_spread
        
        tp1_pnl <- tp1_pnl - tp1_spread
        total_pnl <- total_pnl + tp1_pnl
      }
      
      # TP2 Teilschließung
      if (length(tp2_idx) > 0) {
        tp2_executed <- TRUE
        tp2_bar <- start_idx + tp2_idx[1] - 1
        tp2_exit_price <- data$tp2_price[tp2_bar]
        
        # Lot Size für TP2 (vom verbleibenden Rest)
        remaining_after_tp1 <- initial_lot_size * (1 - tp1_close_pct)
        tp2_lot_size <- remaining_after_tp1 * tp2_close_pct
        
        if (trade_direction == 1) {
          price_diff_tp2 <- tp2_exit_price - entry_pr
        } else {
          price_diff_tp2 <- entry_pr - tp2_exit_price
        }
        
        tp2_pnl <- price_diff_tp2 * tp2_lot_size * contracts
        
        # Spread für TP2 Exit
        tp2_spread <- spread_pips * pip_value * tp2_lot_size * contracts
        total_spread <- total_spread + tp2_spread
        
        tp2_pnl <- tp2_pnl - tp2_spread
        total_pnl <- total_pnl + tp2_pnl
      }
      
      # Final Exit (Rest der Position)
      final_lot_size <- data$remaining_lot_size[end_idx]
      
      # Falls keine TPs getroffen wurden, ist die komplette Position noch offen
      if (!tp1_executed && !tp2_executed) {
        final_lot_size <- initial_lot_size
      }
      
      if (final_lot_size > 0) {
        if (trade_direction == 1) {
          price_diff_final <- final_exit_price - entry_pr
        } else {
          price_diff_final <- entry_pr - final_exit_price
        }
        
        final_pnl <- price_diff_final * final_lot_size * contracts
        
        # Spread für Final Exit
        final_spread <- spread_pips * pip_value * final_lot_size * contracts
        total_spread <- total_spread + final_spread
        
        final_pnl <- final_pnl - final_spread
        total_pnl <- total_pnl + final_pnl
      }
      
      # Exit Grund
      exit_reason <- NA
      if (tp2_executed) {
        exit_reason <- "TP2"
      } else if (tp1_executed) {
        exit_reason <- "TP1_Trailing"
      } else if (data$hour[end_idx] == close_time_hour) {
        exit_reason <- "Time"
      } else {
        exit_reason <- "Trailing_Stop"
      }
      
      # Capital Update
      trade_return_pct <- total_pnl / current_capital
      current_capital <- current_capital + total_pnl
      total_spread_costs <- total_spread_costs + total_spread
      
      trades_list[[t]] <- data.frame(
        trade_num = t,
        entry_time = data$date[start_idx],
        exit_time = data$date[end_idx],
        direction = ifelse(trade_direction == 1, "Long", "Short"),
        entry_price = entry_pr,
        tp1_executed = tp1_executed,
        tp1_exit_price = tp1_exit_price,
        tp1_pnl = tp1_pnl,
        tp2_executed = tp2_executed,
        tp2_exit_price = tp2_exit_price,
        tp2_pnl = tp2_pnl,
        final_exit_price = final_exit_price,
        final_lot_size = final_lot_size,
        final_pnl = final_pnl,
        total_pnl = total_pnl,
        spread_cost = total_spread,
        trade_return_pct = trade_return_pct * 100,
        capital_before = current_capital - total_pnl,
        capital_after = current_capital,
        exit_reason = exit_reason,
        duration_bars = end_idx - start_idx + 1,
        stringsAsFactors = FALSE
      )
    }
    
    trades_df <- do.call(rbind, trades_list)
    
    # Statistiken
    winning_trades <- sum(trades_df$total_pnl > 0, na.rm = TRUE)
    losing_trades <- sum(trades_df$total_pnl < 0, na.rm = TRUE)
    win_rate <- winning_trades / (winning_trades + losing_trades)
    
    avg_win <- mean(trades_df$total_pnl[trades_df$total_pnl > 0], na.rm = TRUE)
    avg_loss <- mean(trades_df$total_pnl[trades_df$total_pnl < 0], na.rm = TRUE)
    largest_win <- max(trades_df$total_pnl, na.rm = TRUE)
    largest_loss <- min(trades_df$total_pnl, na.rm = TRUE)
    
    gross_profit <- sum(trades_df$total_pnl[trades_df$total_pnl > 0], na.rm = TRUE)
    gross_loss <- abs(sum(trades_df$total_pnl[trades_df$total_pnl < 0], na.rm = TRUE))
    profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)
    
    # Drawdown
    trades_df$cumulative_pnl <- cumsum(trades_df$total_pnl)
    trades_df$running_max <- cummax(trades_df$cumulative_pnl)
    trades_df$drawdown <- trades_df$cumulative_pnl - trades_df$running_max
    max_drawdown <- min(trades_df$drawdown, na.rm = TRUE)
    max_drawdown_pct <- (max_drawdown / initial_capital) * 100
    
    # Sharpe Ratio
    trade_returns <- trades_df$total_pnl / trades_df$capital_before
    sharpe_ratio <- mean(trade_returns) / sd(trade_returns) * sqrt(252 * 24 * 4)
    
    # TP Statistiken
    tp1_hit_count <- sum(trades_df$tp1_executed, na.rm = TRUE)
    tp2_hit_count <- sum(trades_df$tp2_executed, na.rm = TRUE)
    tp1_hit_rate <- tp1_hit_count / nrow(trades_df)
    tp2_hit_rate <- tp2_hit_count / nrow(trades_df)
    
  } else {
    trades_df <- NULL
    winning_trades <- 0
    losing_trades <- 0
    win_rate <- NA
    avg_win <- NA
    avg_loss <- NA
    largest_win <- NA
    largest_loss <- NA
    profit_factor <- NA
    max_drawdown <- NA
    max_drawdown_pct <- NA
    sharpe_ratio <- NA
    tp1_hit_count <- 0
    tp2_hit_count <- 0
    tp1_hit_rate <- 0
    tp2_hit_rate <- 0
  }
  
  return(list(
    trades = trades_df,
    initial_capital = initial_capital,
    final_capital = current_capital,
    total_pnl = current_capital - initial_capital,
    return_on_capital = ((current_capital - initial_capital) / initial_capital) * 100,
    n_trades = nrow(trades_df),
    winning_trades = winning_trades,
    losing_trades = losing_trades,
    win_rate = win_rate,
    avg_win = avg_win,
    avg_loss = avg_loss,
    largest_win = largest_win,
    largest_loss = largest_loss,
    profit_factor = profit_factor,
    max_drawdown = max_drawdown,
    max_drawdown_pct = max_drawdown_pct,
    sharpe_ratio = sharpe_ratio,
    total_spread_costs = total_spread_costs,
    tp1_hit_count = tp1_hit_count,
    tp2_hit_count = tp2_hit_count,
    tp1_hit_rate = tp1_hit_rate,
    tp2_hit_rate = tp2_hit_rate,
    data = data
  ))
}

# ==============================================================================
# BACKTEST-WRAPPER
# ==============================================================================

run_backtest_partial_tp <- function(data, params) {
  
  data <- calculate_indicators(
    data,
    conversion_period = params$conversion_period,
    base_period = params$base_period,
    leading_span_b = params$leading_span_b,
    displacement = params$displacement,
    atr_period = params$atr_period,
    adx_period = params$adx_period
  )
  
  data <- generate_signals_partial_tp(
    data,
    initial_lot_size = params$initial_lot_size,
    tp1_atr_mult = params$tp1_atr_mult,
    tp1_close_pct = params$tp1_close_pct,
    tp2_atr_mult = params$tp2_atr_mult,
    tp2_close_pct = params$tp2_close_pct,
    trailing_atr_mult = params$trailing_atr_mult,
    displacement = params$displacement,
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
  
  perf <- calculate_performance_partial_tp(
    data,
    initial_capital = params$initial_capital,
    initial_lot_size = params$initial_lot_size,
    tp1_close_pct = params$tp1_close_pct,
    tp2_close_pct = params$tp2_close_pct,
    contracts = params$contracts,
    spread_pips = params$spread_pips,
    close_time_hour = params$close_time_hour
  )
  
  return(perf)
}

# ==============================================================================
# BEISPIEL-PARAMETER
# ==============================================================================

params_partial_tp <- list(
  # Ichimoku
  conversion_period = 11,
  base_period = 17,
  leading_span_b = 60,
  displacement = 30,
  atr_period = 14,
  adx_period = 14,
  
  # Partielle Take Profits
  initial_lot_size = 1.2,
  tp1_atr_mult = 4.5,          # Erster TP bei 3x ATR
  tp1_close_pct = 0.5,         # 50% bei TP1 schließen
  tp2_atr_mult = 8.0,          # Zweiter TP bei 6x ATR
  tp2_close_pct = 1.0,         # 100% vom Rest bei TP2 (= restliche 50%)
  
  # Trailing Stop
  trailing_atr_mult = 3.5,     # Trailing Stop bei 2.5x ATR
  
  # Filter
  close_time_hour = 22,
  adx_threshold_long = 19,
  adx_threshold_short = 22,
  use_rsi_filter = FALSE,
  rsi_long = 55,
  rsi_short = 45,
  use_rsi_momentum = FALSE,
  rsi_momentum_threshold = 0,
  use_adx_momentum = TRUE,
  adx_momentum_threshold = 0,
  
  # Kapital
  initial_capital = 1000,
  contracts = 1,
  spread_pips = 0
)

# ==============================================================================
# BACKTEST AUSFÜHREN
# ==============================================================================

cat("\n=== Partial Take Profit Backtest auf Test-Daten ===\n\n")

tic()
result <- run_backtest_partial_tp(test_data, params_partial_tp)
toc()

# ==============================================================================
# ERGEBNISSE
# ==============================================================================

cat("\n=== PERFORMANCE ÜBERSICHT ===\n")
cat(sprintf("Anfangskapital:     $%.2f\n", result$initial_capital))
cat(sprintf("Endkapital:         $%.2f\n", result$final_capital))
cat(sprintf("Total P&L:          $%.2f\n", result$total_pnl))
cat(sprintf("Return on Capital:  %.2f%%\n", result$return_on_capital))
cat(sprintf("Spread Kosten:      $%.2f\n\n", result$total_spread_costs))

cat("=== TRADE STATISTIKEN ===\n")
cat(sprintf("Anzahl Trades:      %d\n", result$n_trades))
cat(sprintf("Winning Trades:     %d\n", result$winning_trades))
cat(sprintf("Losing Trades:      %d\n", result$losing_trades))
cat(sprintf("Win Rate:           %.2f%%\n", result$win_rate * 100))
cat(sprintf("Profit Factor:      %.2f\n\n", result$profit_factor))

cat("=== TAKE PROFIT STATISTIKEN ===\n")
cat(sprintf("TP1 Hit Count:      %d (%.1f%%)\n", result$tp1_hit_count, result$tp1_hit_rate * 100))
cat(sprintf("TP2 Hit Count:      %d (%.1f%%)\n\n", result$tp2_hit_count, result$tp2_hit_rate * 100))

cat("=== RISIKO METRIKEN ===\n")
cat(sprintf("Max Drawdown:       $%.2f (%.2f%%)\n", result$max_drawdown, result$max_drawdown_pct))
cat(sprintf("Sharpe Ratio:       %.2f\n\n", result$sharpe_ratio))

cat("=== TRADE DETAILS ===\n")
cat(sprintf("Avg Win:            $%.2f\n", result$avg_win))
cat(sprintf("Avg Loss:           $%.2f\n", result$avg_loss))
cat(sprintf("Largest Win:        $%.2f\n", result$largest_win))
cat(sprintf("Largest Loss:       $%.2f\n\n", result$largest_loss))

# Trades DataFrame
cat("\n=== ERSTE 10 TRADES ===\n")
print(head(result$trades, 10))


trades = result$trades  
indicators = result$data

# Export
# output_file <- "partial_tp_trades.csv"
# write_csv(result$trades, output_file)
# cat(sprintf("\n✓ Trades exportiert nach: %s\n", output_file))

cat("\n=== BACKTEST ABGESCHLOSSEN ===\n")