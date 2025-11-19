# ==============================================================================
# Backtest - Ichimoku Strategy mit Advanced Pyramiding & Exit Strategies
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
library(data.table)

cat("\n")
cat("########################################################################\n")
cat("#                                                                      #\n")
cat("#      Backtest für Ichimoku Strategie (15 Min Intraday)             #\n")
cat("#      mit Breakout/Consecutive Pyramiding & Advanced Exits           #\n")
cat("#                                                                      #\n")
cat("########################################################################\n\n")

# ==============================================================================
# KONFIGURATION
# ==============================================================================

TRAIN_START_YEAR <- 2023
TRAIN_END_YEAR <- 2024
TEST_YEAR <- 2025

input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")
EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
filename <- paste0(EPIC, "_", INTERVAL, ".csv")

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
# INDIKATOREN BERECHNEN (ICHIMOKU)
# ==============================================================================

calculate_indicators <- function(data,
                                 conversion_period = 20,
                                 base_period = 23,
                                 leading_span_b = 44,
                                 displacement = 33,
                                 atr_period = 14,
                                 adx_period = 14,
                                 chandelier_period = 22) {

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

  # Zusätzliche Indikatoren für Pyramiding/Exits
  data$close_change <- c(NA, diff(data$close))

  # Chandelier Exit Berechnung
  data$highest_high <- runMax(data$high, chandelier_period)
  data$lowest_low <- runMin(data$low, chandelier_period)

  return(data)
}

# ==============================================================================
# SIGNALE GENERIEREN MIT PYRAMIDING & EXITS
# ==============================================================================

generate_signals <- function(data,
                             # Ichimoku Entry Signal Parameter
                             adx_threshold_long = 19,
                             adx_threshold_short = 22,
                             use_rsi_filter = TRUE,
                             rsi_short = 70,
                             rsi_long = 51,
                             use_rsi_momentum = TRUE,
                             rsi_momentum_threshold = -2,
                             use_adx_momentum = TRUE,
                             adx_momentum_threshold = -1,

                             # Initial Stop Loss
                             stop_loss_atr_mult = 3.0,

                             # Pyramiding Parameter
                             use_pyramiding = TRUE,
                             pyramid_method = "breakout",  # "breakout", "consecutive", "none"
                             max_pyramid_orders = 3,
                             pyramid_spacing_atr = 0.5,
                             pyramid_size_multiplier = 0.5,

                             # Pyramiding: Consecutive
                             consecutive_bars = 2,

                             # Pyramiding: Breakout
                             breakout_lookback = 20,

                             # Exit Strategy
                             exit_strategy = "chandelier",  # "chandelier", "breakeven_trailing"

                             # Chandelier Exit
                             chandelier_multiplier = 3.0,
                             chandelier_period = 22,

                             # Breakeven + Trailing
                             breakeven_trigger_atr = 1.5,
                             breakeven_offset_atr = 0.2,
                             trailing_stop_atr_mult = 2.0,
                             trailing_start_atr_mult = 1.5,

                             # Take Profit Strategy
                             tp_strategy = "full",  # "full", "partial"
                             full_tp_atr_mult = 4.5,

                             # Partial TP
                             partial_tp_1_atr = 2.0,
                             partial_tp_1_size = 0.33,
                             partial_tp_2_atr = 3.5,
                             partial_tp_2_size = 0.33,

                             # Time Exit
                             close_time_hour = 22,
                             max_bars_in_trade = 100) {

  dt <- as.data.table(data)
  n <- nrow(dt)

  # ==========================================
  # VEKTORISIERTE SIGNAL-GENERIERUNG (ICHIMOKU)
  # ==========================================

  # Crossover-Signale
  dt[, tenkan_cross_up := tenkan_sen > kijun_sen & shift(tenkan_sen, 1) <= shift(kijun_sen, 1)]
  dt[, tenkan_cross_down := tenkan_sen < kijun_sen & shift(tenkan_sen, 1) >= shift(kijun_sen, 1)]

  # Cloud-Position
  dt[, price_above_cloud := close > pmax(senkou_span_a, senkou_span_b, na.rm = TRUE)]
  dt[, price_below_cloud := close < pmin(senkou_span_a, senkou_span_b, na.rm = TRUE)]

  # ADX-Filter (unterschiedlich für Long/Short)
  dt[, adx_filter_long := adx >= adx_threshold_long]
  dt[, adx_filter_short := adx >= adx_threshold_short]

  # ADX Momentum Filter (erste Ableitung) - steigender ADX
  if (use_adx_momentum) {
    dt[, adx_momentum := adx - shift(adx, 1)]
    dt[, adx_momentum_filter := adx_momentum > adx_momentum_threshold]
  } else {
    dt[, adx_momentum_filter := TRUE]
  }

  # RSI-Filter (optional)
  if (use_rsi_filter) {
    dt[, rsi_filter_long := rsi > rsi_long]
    dt[, rsi_filter_short := rsi < rsi_short]
  } else {
    dt[, rsi_filter_long := TRUE]
    dt[, rsi_filter_short := TRUE]
  }

  # RSI Momentum Filter (erste Ableitung)
  if (use_rsi_momentum) {
    dt[, rsi_momentum := rsi - shift(rsi, 1)]
    dt[, rsi_momentum_long := rsi_momentum > rsi_momentum_threshold]
    dt[, rsi_momentum_short := rsi_momentum < -rsi_momentum_threshold]
  } else {
    dt[, rsi_momentum_long := TRUE]
    dt[, rsi_momentum_short := TRUE]
  }

  # Entry Signals (ICHIMOKU-SPEZIFISCH)
  dt[, signal_raw := 0]
  dt[tenkan_cross_up == TRUE &
       price_above_cloud == TRUE &
       adx_filter_long == TRUE &
       adx_momentum_filter == TRUE &
       rsi_filter_long == TRUE &
       rsi_momentum_long == TRUE,
     signal_raw := 1]

  dt[tenkan_cross_down == TRUE &
       price_below_cloud == TRUE &
       adx_filter_short == TRUE &
       adx_momentum_filter == TRUE &
       rsi_filter_short == TRUE &
       rsi_momentum_short == TRUE,
     signal_raw := -1]

  # Chandelier Exit Berechnung
  dt[, chandelier_long := highest_high - (chandelier_multiplier * atr)]
  dt[, chandelier_short := lowest_low + (chandelier_multiplier * atr)]

  # ==========================================
  # POSITION MANAGEMENT (VON CMO_VHF_STC VORLAGE)
  # ==========================================

  position <- integer(n)
  entry_price <- numeric(n)
  stop_loss <- numeric(n)
  take_profit <- numeric(n)
  position_size <- numeric(n)
  num_orders <- integer(n)
  exit_reason_vec <- character(n)

  entry_price[] <- NA_real_
  stop_loss[] <- NA_real_
  take_profit[] <- NA_real_
  exit_reason_vec[] <- ""

  # Trade State
  current_position <- 0L
  current_entry <- NA_real_
  current_sl <- NA_real_
  current_tp <- NA_real_
  pyramid_count <- 0L
  pyramid_entries <- numeric(0)
  total_position_size <- 0
  highest_price <- -Inf
  lowest_price <- Inf
  entry_bar <- 0L
  breakeven_moved <- FALSE
  trailing_active <- FALSE
  partial_tp_1_done <- FALSE
  partial_tp_2_done <- FALSE

  # Pre-extract vectors
  signal_raw_vec <- dt$signal_raw
  close_vec <- dt$close
  high_vec <- dt$high
  low_vec <- dt$low
  atr_vec <- dt$atr
  hour_vec <- dt$hour
  close_change_vec <- dt$close_change
  chandelier_long_vec <- dt$chandelier_long
  chandelier_short_vec <- dt$chandelier_short

  # ==========================================
  # MAIN LOOP
  # ==========================================

  for (i in 2:n) {

    # ========================================
    # TIME-BASED EXIT (22:00)
    # ========================================
    if (current_position != 0L && !is.na(hour_vec[i]) && hour_vec[i] == close_time_hour) {
      current_position <- 0L
      exit_reason_vec[i] <- "Time_22h"
      current_entry <- NA_real_
      current_sl <- NA_real_
      current_tp <- NA_real_
      pyramid_count <- 0L
      pyramid_entries <- numeric(0)
      total_position_size <- 0
      highest_price <- -Inf
      lowest_price <- Inf
      entry_bar <- 0L
      breakeven_moved <- FALSE
      trailing_active <- FALSE
      partial_tp_1_done <- FALSE
      partial_tp_2_done <- FALSE
    }
    # ========================================
    # ENTRY LOGIC
    # ========================================
    else if (signal_raw_vec[i] != 0L && current_position == 0L) {

      signal_direction <- signal_raw_vec[i]
      current_position <- signal_direction
      current_entry <- close_vec[i]
      pyramid_count <- 1L
      pyramid_entries <- close_vec[i]
      total_position_size <- 1.0
      entry_bar <- i
      breakeven_moved <- FALSE
      trailing_active <- FALSE
      partial_tp_1_done <- FALSE
      partial_tp_2_done <- FALSE

      atr_i <- atr_vec[i]
      if (!is.na(atr_i)) {
        if (current_position == 1L) {  # LONG
          current_sl <- current_entry - (stop_loss_atr_mult * atr_i)

          if (tp_strategy == "full") {
            current_tp <- current_entry + (full_tp_atr_mult * atr_i)
          } else {
            # Partial TP: Höchster TP Level
            current_tp <- current_entry + (max(partial_tp_1_atr, partial_tp_2_atr) * atr_i)
          }

          highest_price <- high_vec[i]
        } else {  # SHORT
          current_sl <- current_entry + (stop_loss_atr_mult * atr_i)

          if (tp_strategy == "full") {
            current_tp <- current_entry - (full_tp_atr_mult * atr_i)
          } else {
            current_tp <- current_entry - (max(partial_tp_1_atr, partial_tp_2_atr) * atr_i)
          }

          lowest_price <- low_vec[i]
        }
      }
    }
    # ========================================
    # PYRAMIDING LOGIC
    # ========================================
    else if (use_pyramiding &&
             current_position != 0L &&
             pyramid_count < max_pyramid_orders &&
             pyramid_method != "none") {

      pyramid_signal <- FALSE
      atr_i <- atr_vec[i]

      if (!is.na(atr_i)) {

        # ========================================
        # PYRAMIDING: BREAKOUT
        # ========================================
        if (pyramid_method == "breakout") {
          if (i > breakout_lookback) {
            if (current_position == 1L) {  # Long
              start_idx <- max(1, i - breakout_lookback)
              recent_high <- max(high_vec[start_idx:(i-1)], na.rm = TRUE)

              if (!is.infinite(recent_high) && high_vec[i] > recent_high) {
                pyramid_signal <- TRUE
              }
            } else {  # Short
              start_idx <- max(1, i - breakout_lookback)
              recent_low <- min(low_vec[start_idx:(i-1)], na.rm = TRUE)

              if (!is.infinite(recent_low) && low_vec[i] < recent_low) {
                pyramid_signal <- TRUE
              }
            }
          }
        }

        # ========================================
        # PYRAMIDING: CONSECUTIVE
        # ========================================
        else if (pyramid_method == "consecutive") {
          if (i >= consecutive_bars) {
            if (current_position == 1L) {  # Long
              start_idx <- max(1, i - consecutive_bars + 1)
              all_bullish <- all(close_change_vec[start_idx:i] > 0, na.rm = TRUE)
              if (all_bullish) {
                pyramid_signal <- TRUE
              }
            } else {  # Short
              start_idx <- max(1, i - consecutive_bars + 1)
              all_bearish <- all(close_change_vec[start_idx:i] < 0, na.rm = TRUE)
              if (all_bearish) {
                pyramid_signal <- TRUE
              }
            }
          }
        }

        # ========================================
        # EXECUTE PYRAMIDING
        # ========================================
        if (pyramid_signal) {
          # Spacing Check
          last_entry <- pyramid_entries[length(pyramid_entries)]
          min_distance <- pyramid_spacing_atr * atr_i

          add_order <- FALSE
          if (current_position == 1L) {
            add_order <- close_vec[i] > last_entry + min_distance
          } else {
            add_order <- close_vec[i] < last_entry - min_distance
          }

          # Add Order
          if (add_order) {
            pyramid_count <- pyramid_count + 1L
            pyramid_entries <- c(pyramid_entries, close_vec[i])
            total_position_size <- total_position_size + pyramid_size_multiplier

            # Adjust Stop Loss (nur bei Chandelier nicht nötig, da dynamisch)
            if (exit_strategy == "breakeven_trailing") {
              avg_entry <- mean(pyramid_entries)
              if (current_position == 1L) {
                current_sl <- max(current_sl, avg_entry - (trailing_stop_atr_mult * atr_i))
              } else {
                current_sl <- min(current_sl, avg_entry + (trailing_stop_atr_mult * atr_i))
              }
            }
          }
        }
      }
    }
    # ========================================
    # EXIT LOGIC
    # ========================================
    else if (current_position != 0L && !is.na(current_sl) && !is.na(current_tp)) {

      atr_i <- atr_vec[i]
      bars_in_trade <- i - entry_bar

      # Max Bars Exit
      if (bars_in_trade >= max_bars_in_trade) {
        current_position <- 0L
        exit_reason_vec[i] <- "Max_Bars"
        current_entry <- NA_real_
        current_sl <- NA_real_
        current_tp <- NA_real_
        pyramid_count <- 0L
        pyramid_entries <- numeric(0)
        total_position_size <- 0
        highest_price <- -Inf
        lowest_price <- Inf
        entry_bar <- 0L
        breakeven_moved <- FALSE
        trailing_active <- FALSE
        partial_tp_1_done <- FALSE
        partial_tp_2_done <- FALSE
        next
      }

      if (current_position == 1L) {  # ========== LONG POSITION ==========

        # Update Highest Price
        if (high_vec[i] > highest_price) {
          highest_price <- high_vec[i]
        }

        current_profit <- highest_price - current_entry

        # ========================================
        # PARTIAL TAKE PROFIT LOGIC
        # ========================================
        if (tp_strategy == "partial" && !is.na(atr_i)) {

          # 1. Partial TP
          if (!partial_tp_1_done && current_profit >= partial_tp_1_atr * atr_i) {
            total_position_size <- total_position_size * (1 - partial_tp_1_size)
            partial_tp_1_done <- TRUE

            # Move to Breakeven after 1st TP
            current_sl <- current_entry + (breakeven_offset_atr * atr_i)
            breakeven_moved <- TRUE

            # Continue if position still open
            if (total_position_size <= 0.01) {
              current_position <- 0L
              exit_reason_vec[i] <- "Partial_TP_Full"
              current_entry <- NA_real_
              current_sl <- NA_real_
              current_tp <- NA_real_
              pyramid_count <- 0L
              pyramid_entries <- numeric(0)
              total_position_size <- 0
              highest_price <- -Inf
              entry_bar <- 0L
              breakeven_moved <- FALSE
              trailing_active <- FALSE
              partial_tp_1_done <- FALSE
              partial_tp_2_done <- FALSE
              next
            }
          }

          # 2. Partial TP
          if (!partial_tp_2_done && current_profit >= partial_tp_2_atr * atr_i) {
            total_position_size <- total_position_size * (1 - partial_tp_2_size)
            partial_tp_2_done <- TRUE

            # Continue if position still open
            if (total_position_size <= 0.01) {
              current_position <- 0L
              exit_reason_vec[i] <- "Partial_TP_Full"
              current_entry <- NA_real_
              current_sl <- NA_real_
              current_tp <- NA_real_
              pyramid_count <- 0L
              pyramid_entries <- numeric(0)
              total_position_size <- 0
              highest_price <- -Inf
              entry_bar <- 0L
              breakeven_moved <- FALSE
              trailing_active <- FALSE
              partial_tp_1_done <- FALSE
              partial_tp_2_done <- FALSE
              next
            }
          }
        }

        # ========================================
        # STOP LOSS STRATEGY
        # ========================================

        if (exit_strategy == "chandelier") {
          # Chandelier Exit
          if (!is.na(chandelier_long_vec[i])) {
            current_sl <- max(current_sl, chandelier_long_vec[i])
            trailing_active <- TRUE
          }
        }

        else if (exit_strategy == "breakeven_trailing") {
          if (!is.na(atr_i)) {
            # Phase 1: Breakeven
            if (!breakeven_moved && current_profit >= breakeven_trigger_atr * atr_i) {
              current_sl <- current_entry + (breakeven_offset_atr * atr_i)
              breakeven_moved <- TRUE
            }

            # Phase 2: Trailing
            if (breakeven_moved) {
              profit_distance <- highest_price - current_entry
              activation_distance <- (breakeven_trigger_atr + trailing_start_atr_mult) * atr_i

              if (profit_distance >= activation_distance) {
                trailing_active <- TRUE
                new_trailing_sl <- highest_price - (trailing_stop_atr_mult * atr_i)
                current_sl <- max(current_sl, new_trailing_sl)
              }
            }
          }
        }

        # ========================================
        # EXIT CHECK
        # ========================================

        # Stop Loss Hit
        if (low_vec[i] <= current_sl) {
          current_position <- 0L

          if (trailing_active) {
            exit_reason_vec[i] <- "Trailing_SL"
          } else if (breakeven_moved) {
            exit_reason_vec[i] <- "Breakeven_SL"
          } else {
            exit_reason_vec[i] <- "SL"
          }

          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
          pyramid_count <- 0L
          pyramid_entries <- numeric(0)
          total_position_size <- 0
          highest_price <- -Inf
          entry_bar <- 0L
          breakeven_moved <- FALSE
          trailing_active <- FALSE
          partial_tp_1_done <- FALSE
          partial_tp_2_done <- FALSE
        }
        # Take Profit Hit
        else if (high_vec[i] >= current_tp) {
          current_position <- 0L

          if (tp_strategy == "partial") {
            exit_reason_vec[i] <- "Partial_TP_Final"
          } else {
            exit_reason_vec[i] <- "TP"
          }

          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
          pyramid_count <- 0L
          pyramid_entries <- numeric(0)
          total_position_size <- 0
          highest_price <- -Inf
          entry_bar <- 0L
          breakeven_moved <- FALSE
          trailing_active <- FALSE
          partial_tp_1_done <- FALSE
          partial_tp_2_done <- FALSE
        }

      } else {  # ========== SHORT POSITION ==========

        # Update Lowest Price
        if (low_vec[i] < lowest_price) {
          lowest_price <- low_vec[i]
        }

        current_profit <- current_entry - lowest_price

        # ========================================
        # PARTIAL TAKE PROFIT LOGIC
        # ========================================
        if (tp_strategy == "partial" && !is.na(atr_i)) {

          # 1. Partial TP
          if (!partial_tp_1_done && current_profit >= partial_tp_1_atr * atr_i) {
            total_position_size <- total_position_size * (1 - partial_tp_1_size)
            partial_tp_1_done <- TRUE

            current_sl <- current_entry - (breakeven_offset_atr * atr_i)
            breakeven_moved <- TRUE

            if (total_position_size <= 0.01) {
              current_position <- 0L
              exit_reason_vec[i] <- "Partial_TP_Full"
              current_entry <- NA_real_
              current_sl <- NA_real_
              current_tp <- NA_real_
              pyramid_count <- 0L
              pyramid_entries <- numeric(0)
              total_position_size <- 0
              lowest_price <- Inf
              entry_bar <- 0L
              breakeven_moved <- FALSE
              trailing_active <- FALSE
              partial_tp_1_done <- FALSE
              partial_tp_2_done <- FALSE
              next
            }
          }

          # 2. Partial TP
          if (!partial_tp_2_done && current_profit >= partial_tp_2_atr * atr_i) {
            total_position_size <- total_position_size * (1 - partial_tp_2_size)
            partial_tp_2_done <- TRUE

            if (total_position_size <= 0.01) {
              current_position <- 0L
              exit_reason_vec[i] <- "Partial_TP_Full"
              current_entry <- NA_real_
              current_sl <- NA_real_
              current_tp <- NA_real_
              pyramid_count <- 0L
              pyramid_entries <- numeric(0)
              total_position_size <- 0
              lowest_price <- Inf
              entry_bar <- 0L
              breakeven_moved <- FALSE
              trailing_active <- FALSE
              partial_tp_1_done <- FALSE
              partial_tp_2_done <- FALSE
              next
            }
          }
        }

        # ========================================
        # STOP LOSS STRATEGY
        # ========================================

        if (exit_strategy == "chandelier") {
          if (!is.na(chandelier_short_vec[i])) {
            current_sl <- min(current_sl, chandelier_short_vec[i])
            trailing_active <- TRUE
          }
        }

        else if (exit_strategy == "breakeven_trailing") {
          if (!is.na(atr_i)) {
            # Phase 1: Breakeven
            if (!breakeven_moved && current_profit >= breakeven_trigger_atr * atr_i) {
              current_sl <- current_entry - (breakeven_offset_atr * atr_i)
              breakeven_moved <- TRUE
            }

            # Phase 2: Trailing
            if (breakeven_moved) {
              profit_distance <- current_entry - lowest_price
              activation_distance <- (breakeven_trigger_atr + trailing_start_atr_mult) * atr_i

              if (profit_distance >= activation_distance) {
                trailing_active <- TRUE
                new_trailing_sl <- lowest_price + (trailing_stop_atr_mult * atr_i)
                current_sl <- min(current_sl, new_trailing_sl)
              }
            }
          }
        }

        # ========================================
        # EXIT CHECK
        # ========================================

        # Stop Loss Hit
        if (high_vec[i] >= current_sl) {
          current_position <- 0L

          if (trailing_active) {
            exit_reason_vec[i] <- "Trailing_SL"
          } else if (breakeven_moved) {
            exit_reason_vec[i] <- "Breakeven_SL"
          } else {
            exit_reason_vec[i] <- "SL"
          }

          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
          pyramid_count <- 0L
          pyramid_entries <- numeric(0)
          total_position_size <- 0
          lowest_price <- Inf
          entry_bar <- 0L
          breakeven_moved <- FALSE
          trailing_active <- FALSE
          partial_tp_1_done <- FALSE
          partial_tp_2_done <- FALSE
        }
        # Take Profit Hit
        else if (low_vec[i] <= current_tp) {
          current_position <- 0L

          if (tp_strategy == "partial") {
            exit_reason_vec[i] <- "Partial_TP_Final"
          } else {
            exit_reason_vec[i] <- "TP"
          }

          current_entry <- NA_real_
          current_sl <- NA_real_
          current_tp <- NA_real_
          pyramid_count <- 0L
          pyramid_entries <- numeric(0)
          total_position_size <- 0
          lowest_price <- Inf
          entry_bar <- 0L
          breakeven_moved <- FALSE
          trailing_active <- FALSE
          partial_tp_1_done <- FALSE
          partial_tp_2_done <- FALSE
        }
      }
    }

    # Save state
    position[i] <- current_position
    entry_price[i] <- current_entry
    stop_loss[i] <- current_sl
    take_profit[i] <- current_tp
    position_size[i] <- total_position_size
    num_orders[i] <- pyramid_count
  }

  # Write back to data.table
  dt[, `:=`(
    position = position,
    entry_price = entry_price,
    stop_loss = stop_loss,
    take_profit = take_profit,
    position_size = position_size,
    num_orders = num_orders,
    exit_reason = exit_reason_vec
  )]

  dt[, signal := shift(position, 1)]
  dt[is.na(signal), signal := 0]

  result <- as.data.frame(dt)

  # Clean up
  result <- result[, !names(result) %in% c("tenkan_cross_up", "tenkan_cross_down",
                                           "price_above_cloud", "price_below_cloud",
                                           "adx_filter_long", "adx_filter_short",
                                           "adx_momentum", "adx_momentum_filter",
                                           "rsi_filter_long", "rsi_filter_short",
                                           "rsi_momentum", "rsi_momentum_long", "rsi_momentum_short",
                                           "chandelier_long", "chandelier_short")]

  return(result)
}

# ==============================================================================
# PERFORMANCE BERECHNEN
# ==============================================================================

calculate_performance <- function(data) {

  dt <- as.data.table(data)

  dt[, log_return := log(close / shift(close))]
  dt[, forward_return := shift(log_return, -1, type = "lead")]

  if("position_size" %in% names(dt)) {
    dt[, strategy_return := signal * forward_return * position_size]
  } else {
    dt[, strategy_return := signal * forward_return]
  }

  dt_clean <- dt[!is.na(strategy_return)]

  total_log_return <- sum(dt_clean$strategy_return, na.rm = TRUE)
  mean_log_return <- mean(dt_clean$strategy_return, na.rm = TRUE)
  sd_log_return <- sd(dt_clean$strategy_return, na.rm = TRUE)
  sharpe_log_ratio <- mean_log_return / sd_log_return * sqrt(252 * 24 * 4)

  total_return <- exp(total_log_return) - 1
  mean_return <- exp(mean_log_return) - 1
  sd_return <- exp(sd_log_return) - 1
  sharpe_ratio <- mean_return / sd_return * sqrt(252 * 24 * 4)

  dt[, position_change := position - shift(position, fill = 0)]
  n_trades <- sum(abs(dt$position_change) > 0, na.rm = TRUE) / 2

  gross_profit <- sum(dt_clean[strategy_return > 0, strategy_return], na.rm = TRUE)
  gross_loss <- abs(sum(dt_clean[strategy_return < 0, strategy_return], na.rm = TRUE))
  profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)

  dt_clean[, cumulative_returns := cumsum(strategy_return)]
  dt_clean[, running_max := cummax(cumulative_returns)]
  dt_clean[, drawdown := cumulative_returns - running_max]
  max_drawdown <- min(dt_clean$drawdown, na.rm = TRUE)
  max_drawdown_pct <- (exp(max_drawdown) - 1) * 100

  winning_trades <- sum(dt_clean$strategy_return > 0, na.rm = TRUE)
  losing_trades <- sum(dt_clean$strategy_return < 0, na.rm = TRUE)
  total_trades_ret <- winning_trades + losing_trades
  win_rate <- ifelse(total_trades_ret > 0, winning_trades / total_trades_ret, NA)

  return(list(
    data = as.data.frame(dt_clean),
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
# BACKTEST-WRAPPER
# ==============================================================================

run_backtest <- function(data, params) {

  data <- calculate_indicators(
    data,
    conversion_period = params$conversion_period,
    base_period = params$base_period,
    leading_span_b = params$leading_span_b,
    displacement = params$displacement,
    atr_period = params$atr_period,
    adx_period = params$adx_period,
    chandelier_period = params$chandelier_period
  )

  data <- generate_signals(
    data,
    adx_threshold_long = params$adx_threshold_long,
    adx_threshold_short = params$adx_threshold_short,
    use_rsi_filter = params$use_rsi_filter,
    rsi_short = params$rsi_short,
    rsi_long = params$rsi_long,
    use_rsi_momentum = params$use_rsi_momentum,
    rsi_momentum_threshold = params$rsi_momentum_threshold,
    use_adx_momentum = params$use_adx_momentum,
    adx_momentum_threshold = params$adx_momentum_threshold,
    stop_loss_atr_mult = params$stop_loss_atr_mult,
    use_pyramiding = params$use_pyramiding,
    pyramid_method = params$pyramid_method,
    max_pyramid_orders = params$max_pyramid_orders,
    pyramid_spacing_atr = params$pyramid_spacing_atr,
    pyramid_size_multiplier = params$pyramid_size_multiplier,
    consecutive_bars = params$consecutive_bars,
    breakout_lookback = params$breakout_lookback,
    exit_strategy = params$exit_strategy,
    chandelier_multiplier = params$chandelier_multiplier,
    chandelier_period = params$chandelier_period,
    breakeven_trigger_atr = params$breakeven_trigger_atr,
    breakeven_offset_atr = params$breakeven_offset_atr,
    trailing_stop_atr_mult = params$trailing_stop_atr_mult,
    trailing_start_atr_mult = params$trailing_start_atr_mult,
    tp_strategy = params$tp_strategy,
    full_tp_atr_mult = params$full_tp_atr_mult,
    partial_tp_1_atr = params$partial_tp_1_atr,
    partial_tp_1_size = params$partial_tp_1_size,
    partial_tp_2_atr = params$partial_tp_2_atr,
    partial_tp_2_size = params$partial_tp_2_size,
    close_time_hour = params$close_time_hour,
    max_bars_in_trade = params$max_bars_in_trade
  )

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
                              "data"),
                  envir = environment())

    clusterEvalQ(cl, {
      library(tidyverse)
      library(TTR)
      library(lubridate)
      library(data.table)
    })

    registerDoParallel(cl)

    results_list <- foreach(
      i = 1:nrow(param_grid),
      .packages = c("tidyverse", "TTR", "lubridate", "data.table"),
      .errorhandling = "pass",
      .verbose = FALSE
    ) %dopar% {

      params <- as.list(param_grid[i, ])

      tryCatch({
        perf <- run_backtest(data, params)

        if (perf$n_trades >= min_trades &&
            is.finite(perf[[metric]]) &&
            !is.na(perf[[metric]])) {

          result_df <- data.frame(
            iteration = i,
            sharpe_ratio = perf$sharpe_ratio,
            total_return = perf$total_return,
            n_trades = perf$n_trades,
            win_rate = perf$win_rate,
            profit_factor = perf$profit_factor,
            max_drawdown = perf$max_drawdown,
            stringsAsFactors = FALSE
          )

          # Add all params
          for (param_name in names(params)) {
            result_df[[param_name]] <- params[[param_name]]
          }

          result_df
        } else {
          NULL
        }
      }, error = function(e) {
        list(error = TRUE, message = e$message, iteration = i)
      })
    }

    stopCluster(cl)

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
    # Sequential processing
    results_list <- list()
    pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

    for (i in 1:nrow(param_grid)) {
      params <- as.list(param_grid[i, ])

      tryCatch({
        perf <- run_backtest(data, params)

        if (perf$n_trades >= min_trades &&
            is.finite(perf[[metric]]) &&
            !is.na(perf[[metric]])) {

          result_df <- data.frame(
            iteration = i,
            sharpe_ratio = perf$sharpe_ratio,
            total_return = perf$total_return,
            n_trades = perf$n_trades,
            win_rate = perf$win_rate,
            profit_factor = perf$profit_factor,
            max_drawdown = perf$max_drawdown,
            stringsAsFactors = FALSE
          )

          for (param_name in names(params)) {
            result_df[[param_name]] <- params[[param_name]]
          }

          results_list[[length(results_list) + 1]] <- result_df
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
# DETAILLIERTE PERFORMANCE
# ==============================================================================

calculate_performance_detailed <- function(data, initial_capital = 300,
                                           contracts = 1, lot_size = 1.2,
                                           spread_pips = 35) {

  dt <- as.data.table(data)

  dt[, log_return := log(close / shift(close))]
  dt[, forward_return := shift(log_return, -1, type = "lead")]

  if("position_size" %in% names(dt)) {
    dt[, strategy_return := signal * forward_return * position_size]
  } else {
    dt[, strategy_return := signal * forward_return]
  }

  dt_clean <- dt[!is.na(strategy_return)]

  total_log_return <- sum(dt_clean$strategy_return, na.rm = TRUE)
  mean_log_return <- mean(dt_clean$strategy_return, na.rm = TRUE)
  sd_log_return <- sd(dt_clean$strategy_return, na.rm = TRUE)
  sharpe_log_ratio <- mean_log_return / sd_log_return * sqrt(252 * 24 * 4)

  total_return <- exp(total_log_return) - 1
  mean_return <- exp(mean_log_return) - 1
  sd_return <- exp(sd_log_return) - 1
  sharpe_ratio <- mean_return / sd_return * sqrt(252 * 24 * 4)

  dt_clean[, position_change := position - shift(position, fill = 0)]
  n_trades <- sum(abs(dt_clean$position_change) > 0, na.rm = TRUE)/2

  gross_profit <- sum(dt_clean[strategy_return > 0, strategy_return], na.rm = TRUE)
  gross_loss <- abs(sum(dt_clean[strategy_return < 0, strategy_return], na.rm = TRUE))
  profit_factor <- ifelse(gross_loss > 0, gross_profit / gross_loss, NA)

  dt_clean[, cumulative_returns := cumsum(strategy_return)]
  dt_clean[, running_max := cummax(cumulative_returns)]
  dt_clean[, drawdown := cumulative_returns - running_max]
  max_drawdown <- min(dt_clean$drawdown, na.rm = TRUE)
  max_drawdown_pct <- (exp(max_drawdown) - 1) * 100

  winning_trades <- sum(dt_clean$strategy_return > 0, na.rm = TRUE)
  losing_trades <- sum(dt_clean$strategy_return < 0, na.rm = TRUE)
  total_trades_ret <- winning_trades + losing_trades
  win_rate <- ifelse(total_trades_ret > 0, winning_trades / total_trades_ret, NA)

  # Trade-by-trade
  trade_starts <- which(dt_clean$position_change != 0 & dt_clean$position != 0)
  trade_ends <- which(dt_clean$position_change != 0 & shift(dt_clean$position, fill = 0) != 0)

  if(length(trade_starts) > length(trade_ends)) {
    trade_ends <- c(trade_ends, nrow(dt_clean))
  }

  trades_list <- list()
  current_capital <- initial_capital
  total_spread_costs <- 0
  total_pyramid_orders <- 0

  if(length(trade_starts) > 0 && length(trade_starts) == length(trade_ends)) {
    for(i in 1:length(trade_starts)) {
      start_idx <- trade_starts[i]
      end_idx <- trade_ends[i]

      trade_direction <- dt_clean$position[start_idx]
      entry_price <- dt_clean$close[start_idx]
      exit_price <- dt_clean$close[end_idx]

      avg_position_size <- mean(dt_clean$position_size[start_idx:end_idx], na.rm = TRUE)
      if(is.na(avg_position_size)) avg_position_size <- 1.0

      max_orders <- max(dt_clean$num_orders[start_idx:end_idx], na.rm = TRUE)
      if(is.na(max_orders)) max_orders <- 1
      total_pyramid_orders <- total_pyramid_orders + max_orders

      exit_reason <- dt_clean$exit_reason[end_idx]
      if(is.na(exit_reason) || exit_reason == "") exit_reason <- "Unknown"

      sl_price <- dt_clean$stop_loss[start_idx]
      tp_price <- dt_clean$take_profit[start_idx]

      position_size_adj <- lot_size * avg_position_size
      contract_size <- contracts
      pip_value <- 0.01

      spread_cost <- spread_pips * pip_value * lot_size * contract_size * max_orders

      price_diff <- ifelse(trade_direction == 1,
                           exit_price - entry_price,
                           entry_price - exit_price)

      gross_pnl <- price_diff * position_size_adj * contract_size
      net_pnl <- gross_pnl - spread_cost
      trade_return_pct <- net_pnl / current_capital

      current_capital <- current_capital + net_pnl
      total_spread_costs <- total_spread_costs + spread_cost

      risk_distance <- abs(entry_price - sl_price)
      risk_dollar <- risk_distance * position_size_adj * contract_size
      risk_percent <- (risk_dollar / (current_capital - net_pnl)) * 100

      trades_list[[i]] <- data.frame(
        trade_num = i,
        entry_time = dt_clean$date[start_idx],
        exit_time = dt_clean$date[end_idx],
        direction = ifelse(trade_direction == 1, "Long", "Short"),
        entry_price = entry_price,
        exit_price = exit_price,
        sl_price = sl_price,
        tp_price = tp_price,
        lot_size = position_size_adj,
        num_pyramid_orders = max_orders,
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
        duration_bars = end_idx - start_idx + 1,
        stringsAsFactors = FALSE
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
    avg_pyramid_orders <- mean(trades_df$num_pyramid_orders, na.rm = TRUE)

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
    avg_pyramid_orders <- NA
    profit_factor_net <- NA
    exit_stats <- NULL
    total_spread_costs <- NA
    avg_spread_per_trade <- NA
    spread_as_pct_of_avg_win <- NA
  }

  return(list(
    data = as.data.frame(dt_clean),
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
    avg_pyramid_orders = avg_pyramid_orders,
    total_pyramid_orders = total_pyramid_orders,
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
    adx_period = params$adx_period,
    chandelier_period = params$chandelier_period
  )

  data <- generate_signals(
    data,
    adx_threshold_long = params$adx_threshold_long,
    adx_threshold_short = params$adx_threshold_short,
    use_rsi_filter = params$use_rsi_filter,
    rsi_short = params$rsi_short,
    rsi_long = params$rsi_long,
    use_rsi_momentum = params$use_rsi_momentum,
    rsi_momentum_threshold = params$rsi_momentum_threshold,
    use_adx_momentum = params$use_adx_momentum,
    adx_momentum_threshold = params$adx_momentum_threshold,
    stop_loss_atr_mult = params$stop_loss_atr_mult,
    use_pyramiding = params$use_pyramiding,
    pyramid_method = params$pyramid_method,
    max_pyramid_orders = params$max_pyramid_orders,
    pyramid_spacing_atr = params$pyramid_spacing_atr,
    pyramid_size_multiplier = params$pyramid_size_multiplier,
    consecutive_bars = params$consecutive_bars,
    breakout_lookback = params$breakout_lookback,
    exit_strategy = params$exit_strategy,
    chandelier_multiplier = params$chandelier_multiplier,
    chandelier_period = params$chandelier_period,
    breakeven_trigger_atr = params$breakeven_trigger_atr,
    breakeven_offset_atr = params$breakeven_offset_atr,
    trailing_stop_atr_mult = params$trailing_stop_atr_mult,
    trailing_start_atr_mult = params$trailing_start_atr_mult,
    tp_strategy = params$tp_strategy,
    full_tp_atr_mult = params$full_tp_atr_mult,
    partial_tp_1_atr = params$partial_tp_1_atr,
    partial_tp_1_size = params$partial_tp_1_size,
    partial_tp_2_atr = params$partial_tp_2_atr,
    partial_tp_2_size = params$partial_tp_2_size,
    close_time_hour = params$close_time_hour,
    max_bars_in_trade = params$max_bars_in_trade
  )

  perf <- calculate_performance_detailed(data)

  return(perf)
}

# ==============================================================================
# PARAMETER-GRID
# ==============================================================================

param_grid <- expand.grid(
  # Ichimoku Parameter
  conversion_period = c(18, 20, 22),
  base_period = c(20, 23, 26),
  leading_span_b = c(40, 44, 48),
  displacement = c(30, 33, 36),
  atr_period = 14,
  adx_period = c(13, 14),

  # ADX/RSI Filter
  adx_threshold_long = c(18, 19, 20),
  adx_threshold_short = c(21, 22),
  use_rsi_filter = TRUE,
  rsi_long = c(50, 55),
  rsi_short = c(45, 50),
  use_rsi_momentum = c(TRUE, FALSE),
  rsi_momentum_threshold = c(-2, 0),
  use_adx_momentum = c(TRUE, FALSE),
  adx_momentum_threshold = c(-1, 0),

  # Stop Loss
  stop_loss_atr_mult = c(2.5, 3.0),

  # Pyramiding
  use_pyramiding = c(TRUE, FALSE),
  pyramid_method = c("breakout", "consecutive"),
  max_pyramid_orders = c(2, 3),
  pyramid_spacing_atr = c(0.5, 1.0),
  pyramid_size_multiplier = c(0.5),
  consecutive_bars = c(2, 3),
  breakout_lookback = c(15, 20),

  # Exit Strategy
  exit_strategy = c("chandelier", "breakeven_trailing"),
  chandelier_multiplier = c(2.5, 3.0),
  chandelier_period = c(22),
  breakeven_trigger_atr = c(1.2, 1.5),
  breakeven_offset_atr = c(0.2),
  trailing_stop_atr_mult = c(1.8, 2.0),
  trailing_start_atr_mult = c(1.5),

  # Take Profit
  tp_strategy = c("full", "partial"),
  full_tp_atr_mult = c(4.0, 4.5),
  partial_tp_1_atr = c(2.0, 2.5),
  partial_tp_1_size = c(0.33),
  partial_tp_2_atr = c(3.5, 4.0),
  partial_tp_2_size = c(0.33),

  # Time
  close_time_hour = 22,
  max_bars_in_trade = c(80, 100)
)

# Filter
param_grid <- param_grid %>%
  filter(conversion_period < base_period,
         displacement < leading_span_b,
         partial_tp_1_atr < partial_tp_2_atr,
         rsi_long < rsi_short)

cat(sprintf("Parameter-Grid enthält %d Kombinationen\n", nrow(param_grid)))

# ==============================================================================
# TEST
# ==============================================================================

cat("\n=== SPEED TEST ===\n")
params_test <- param_grid[1,]

tic("Full Backtest")
backtest_loop <- run_backtest(test_data, params_test)
toc()

cat("\nTest-Backtest abgeschlossen:\n")
cat(sprintf("Sharpe Ratio: %.2f\n", backtest_loop$sharpe_ratio))
cat(sprintf("Total Return: %.2f%%\n", backtest_loop$total_return * 100))
cat(sprintf("Trades: %d\n", backtest_loop$n_trades))

# ==============================================================================
# OPTIMIERUNG
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
# VALIDIERUNG TOP 10
# ==============================================================================

cat("\n========================================\n")
cat("VALIDIERUNG TOP 10 PARAMETER AUF TEST-DATEN\n")
cat("========================================\n")

top_10_params <- head(optimization_results, 10)

for(i in 1:nrow(top_10_params)) {
  cat(sprintf("\n--- Rang %d ---\n", i))
  cat(sprintf("Ichimoku: Conv=%d, Base=%d | Pyramid: %s | Exit: %s | TP: %s\n",
              top_10_params$conversion_period[i],
              top_10_params$base_period[i],
              top_10_params$pyramid_method[i],
              top_10_params$exit_strategy[i],
              top_10_params$tp_strategy[i]))

  params <- as.list(top_10_params[i, ])
  test_perf <- run_backtest_detailed(test_data, params)

  cat(sprintf("Total Return: %.2f%%\n", test_perf$total_return * 100))
  cat(sprintf("Profit Factor: %.2f\n", test_perf$profit_factor_net))
  cat(sprintf("Sharpe Ratio: %.2f\n", test_perf$sharpe_ratio))
  cat(sprintf("Win Rate: %.2f%%\n", test_perf$win_rate * 100))
  cat(sprintf("Max DD: %.2f%%\n", test_perf$max_drawdown_pct))
  cat(sprintf("Trades: %d\n", test_perf$n_trades))

  if(!is.null(test_perf$trades) && nrow(test_perf$trades) > 0) {
    cat(sprintf("Avg Pyramid Orders: %.2f\n", test_perf$avg_pyramid_orders))
    cat("\nExit Reasons:\n")
    print(test_perf$exit_stats)
  }

  cat("\n")
}

cat("\n✅ Optimierung abgeschlossen!\n")
