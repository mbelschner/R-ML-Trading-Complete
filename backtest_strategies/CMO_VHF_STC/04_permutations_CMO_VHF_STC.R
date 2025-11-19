# ==============================================================================
# MONTE CARLO PERMUTATION TEST (MCPT) - CMO-VHF-STC STRATEGIE
# Mit vollständigem Pyramiding & Advanced Exit Strategies
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
library(pracma)
library(data.table)

cat("\n")
cat("########################################################################\n")
cat("#                                                                      #\n")
cat("#      MCPT für CMO-VHF-STC Strategie mit Pyramiding                 #\n")
cat("#      Mit Advanced Exit Strategies & Risk Management                 #\n")
cat("#                                                                      #\n")
cat("########################################################################\n\n")

# ==============================================================================
# KONFIGURATION
# ==============================================================================

# MCPT-Parameter
N_PERMUTATIONS <- 300
METRIC <- "profit_factor"  # "sharpe_ratio", "total_return", "profit_factor"

# CMO-VHF-STC Parameter (Best from Training)
CMO_PERIOD <- 14
CMO_THRESHOLD <- 20
VHF_PERIOD <- 28
VHF_THRESHOLD <- 0.35
STC_FAST <- 23
STC_SLOW <- 50
STC_CYCLE <- 10
STC_ENTRY_LONG <- 25
STC_ENTRY_SHORT <- 75

# Risk Management Parameter
ATR_PERIOD <- 14
ADX_PERIOD <- 14
CHANDELIER_PERIOD <- 22
STOP_LOSS_ATR_MULT <- 3.0

# Pyramiding Parameter
USE_PYRAMIDING <- TRUE
PYRAMID_METHOD <- "breakout"  # "breakout", "consecutive", "none"
MAX_PYRAMID_ORDERS <- 3
PYRAMID_SPACING_ATR <- 0.5
PYRAMID_SIZE_MULTIPLIER <- 0.5
CONSECUTIVE_BARS <- 2
BREAKOUT_LOOKBACK <- 20

# Exit Strategy
EXIT_STRATEGY <- "chandelier"  # "chandelier", "breakeven_trailing"
CHANDELIER_MULTIPLIER <- 3.0
BREAKEVEN_TRIGGER_ATR <- 1.5
BREAKEVEN_OFFSET_ATR <- 0.2
TRAILING_STOP_ATR_MULT <- 2.0
TRAILING_START_ATR_MULT <- 1.5

# Take Profit
TP_STRATEGY <- "full"  # "full", "partial"
FULL_TP_ATR_MULT <- 4.5
PARTIAL_TP_1_ATR <- 2.0
PARTIAL_TP_1_SIZE <- 0.33
PARTIAL_TP_2_ATR <- 3.5
PARTIAL_TP_2_SIZE <- 0.33

# Time Settings
CLOSE_TIME_HOUR <- 22
MAX_BARS_IN_TRADE <- 100

# Optional Filters
USE_PRICE_CONFIRMATION <- TRUE
USE_ADX_FILTER <- FALSE
ADX_THRESHOLD <- 20

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
# INDIKATOREN-FUNKTIONEN
# ==============================================================================

calculate_stc <- function(close, fast = 23, slow = 50, cycle = 10, smooth1 = 3, smooth2 = 3) {
  macd_line <- EMA(close, fast) - EMA(close, slow)

  macd_min <- runMin(macd_line, cycle)
  macd_max <- runMax(macd_line, cycle)
  stoch1 <- 100 * (macd_line - macd_min) / (macd_max - macd_min)
  stoch1[is.na(stoch1) | is.infinite(stoch1)] <- 50
  stoch1_smooth <- EMA(stoch1, smooth1)

  stoch1_min <- runMin(stoch1_smooth, cycle)
  stoch1_max <- runMax(stoch1_smooth, cycle)
  stc <- 100 * (stoch1_smooth - stoch1_min) / (stoch1_max - stoch1_min)
  stc[is.na(stc) | is.infinite(stc)] <- 50
  stc <- EMA(stc, smooth2)

  return(as.numeric(stc))
}

calculate_vhf <- function(close, n = 28) {
  highest <- runMax(close, n)
  lowest <- runMin(close, n)
  sum_changes <- runSum(abs(diff(c(NA, close))), n)

  vhf <- (highest - lowest) / sum_changes
  vhf[is.infinite(vhf)] <- NA
  return(as.numeric(vhf))
}

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
                                 adx_period = 14,
                                 chandelier_period = 22) {

  data$cmo <- calculate_cmo(data$close, n = cmo_period)
  data$vhf <- calculate_vhf(data$close, n = vhf_period)
  data$stc <- calculate_stc(data$close, fast = stc_fast, slow = stc_slow, cycle = stc_cycle)
  data$atr <- ATR(cbind(data$high, data$low, data$close), n = atr_period)[, "atr"]
  data$adx <- ADX(cbind(data$high, data$low, data$close), n = adx_period)[, "ADX"]

  data$close_change <- c(NA, diff(data$close))
  data$highest_high <- runMax(data$high, chandelier_period)
  data$lowest_low <- runMin(data$low, chandelier_period)

  return(data)
}

# ==============================================================================
# SIGNALE GENERIEREN MIT PYRAMIDING & EXITS (VOLLSTÄNDIG AUS 03_BACKTEST)
# ==============================================================================

generate_signals <- function(data,
                             cmo_threshold = 20,
                             vhf_threshold = 0.35,
                             stc_entry_long = 25,
                             stc_entry_short = 75,
                             use_price_confirmation = TRUE,
                             use_adx_filter = FALSE,
                             adx_threshold = 20,
                             stop_loss_atr_mult = 3.0,
                             use_pyramiding = TRUE,
                             pyramid_method = "breakout",
                             max_pyramid_orders = 3,
                             pyramid_spacing_atr = 0.5,
                             pyramid_size_multiplier = 0.5,
                             consecutive_bars = 2,
                             breakout_lookback = 20,
                             exit_strategy = "chandelier",
                             chandelier_multiplier = 3.0,
                             chandelier_period = 22,
                             breakeven_trigger_atr = 1.5,
                             breakeven_offset_atr = 0.2,
                             trailing_stop_atr_mult = 2.0,
                             trailing_start_atr_mult = 1.5,
                             tp_strategy = "full",
                             full_tp_atr_mult = 4.5,
                             partial_tp_1_atr = 2.0,
                             partial_tp_1_size = 0.33,
                             partial_tp_2_atr = 3.5,
                             partial_tp_2_size = 0.33,
                             close_time_hour = 22,
                             max_bars_in_trade = 100) {

  dt <- as.data.table(data)
  n <- nrow(dt)

  # Vektorisierte Signal-Generierung
  dt[, stc_lag := shift(stc, 1)]
  dt[, close_lag := shift(close, 1)]

  dt[, market_trending := vhf > vhf_threshold]
  dt[, bullish_momentum := cmo > cmo_threshold]
  dt[, bearish_momentum := cmo < -cmo_threshold]

  if (use_adx_filter) {
    dt[, adx_ok := adx >= adx_threshold]
  } else {
    dt[, adx_ok := TRUE]
  }

  if (use_price_confirmation) {
    dt[, price_conf_long := close > close_lag]
    dt[, price_conf_short := close < close_lag]
  } else {
    dt[, price_conf_long := TRUE]
    dt[, price_conf_short := TRUE]
  }

  dt[, stc_cross_up := stc_lag < stc_entry_long & stc > stc_entry_long]
  dt[, stc_cross_down := stc_lag > stc_entry_short & stc < stc_entry_short]

  dt[, signal_raw := 0]
  dt[market_trending == TRUE &
       bullish_momentum == TRUE &
       adx_ok == TRUE &
       price_conf_long == TRUE &
       stc_cross_up == TRUE,
     signal_raw := 1]

  dt[market_trending == TRUE &
       bearish_momentum == TRUE &
       adx_ok == TRUE &
       price_conf_short == TRUE &
       stc_cross_down == TRUE,
     signal_raw := -1]

  dt[, chandelier_long := highest_high - (chandelier_multiplier * atr)]
  dt[, chandelier_short := lowest_low + (chandelier_multiplier * atr)]

  # Position Management
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

  signal_raw_vec <- dt$signal_raw
  close_vec <- dt$close
  high_vec <- dt$high
  low_vec <- dt$low
  atr_vec <- dt$atr
  hour_vec <- dt$hour
  close_change_vec <- dt$close_change
  chandelier_long_vec <- dt$chandelier_long
  chandelier_short_vec <- dt$chandelier_short

  for (i in 2:n) {

    # Time-based exit
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
    # Entry logic
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
        if (current_position == 1L) {
          current_sl <- current_entry - (stop_loss_atr_mult * atr_i)

          if (tp_strategy == "full") {
            current_tp <- current_entry + (full_tp_atr_mult * atr_i)
          } else {
            current_tp <- current_entry + (max(partial_tp_1_atr, partial_tp_2_atr) * atr_i)
          }

          highest_price <- high_vec[i]
        } else {
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
    # Pyramiding logic
    else if (use_pyramiding &&
             current_position != 0L &&
             pyramid_count < max_pyramid_orders &&
             pyramid_method != "none") {

      pyramid_signal <- FALSE
      atr_i <- atr_vec[i]

      if (!is.na(atr_i)) {

        if (pyramid_method == "breakout") {
          if (i > breakout_lookback) {
            if (current_position == 1L) {
              start_idx <- max(1, i - breakout_lookback)
              recent_high <- max(high_vec[start_idx:(i-1)], na.rm = TRUE)

              if (!is.infinite(recent_high) && high_vec[i] > recent_high) {
                pyramid_signal <- TRUE
              }
            } else {
              start_idx <- max(1, i - breakout_lookback)
              recent_low <- min(low_vec[start_idx:(i-1)], na.rm = TRUE)

              if (!is.infinite(recent_low) && low_vec[i] < recent_low) {
                pyramid_signal <- TRUE
              }
            }
          }
        }

        else if (pyramid_method == "consecutive") {
          if (i >= consecutive_bars) {
            if (current_position == 1L) {
              start_idx <- max(1, i - consecutive_bars + 1)
              all_bullish <- all(close_change_vec[start_idx:i] > 0, na.rm = TRUE)
              if (all_bullish) {
                pyramid_signal <- TRUE
              }
            } else {
              start_idx <- max(1, i - consecutive_bars + 1)
              all_bearish <- all(close_change_vec[start_idx:i] < 0, na.rm = TRUE)
              if (all_bearish) {
                pyramid_signal <- TRUE
              }
            }
          }
        }

        if (pyramid_signal) {
          last_entry <- pyramid_entries[length(pyramid_entries)]
          min_distance <- pyramid_spacing_atr * atr_i

          add_order <- FALSE
          if (current_position == 1L) {
            add_order <- close_vec[i] > last_entry + min_distance
          } else {
            add_order <- close_vec[i] < last_entry - min_distance
          }

          if (add_order) {
            pyramid_count <- pyramid_count + 1L
            pyramid_entries <- c(pyramid_entries, close_vec[i])
            total_position_size <- total_position_size + pyramid_size_multiplier

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
    # Exit logic
    else if (current_position != 0L && !is.na(current_sl) && !is.na(current_tp)) {

      atr_i <- atr_vec[i]
      bars_in_trade <- i - entry_bar

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

      if (current_position == 1L) {

        if (high_vec[i] > highest_price) {
          highest_price <- high_vec[i]
        }

        current_profit <- highest_price - current_entry

        if (tp_strategy == "partial" && !is.na(atr_i)) {

          if (!partial_tp_1_done && current_profit >= partial_tp_1_atr * atr_i) {
            total_position_size <- total_position_size * (1 - partial_tp_1_size)
            partial_tp_1_done <- TRUE

            current_sl <- current_entry + (breakeven_offset_atr * atr_i)
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
              highest_price <- -Inf
              entry_bar <- 0L
              breakeven_moved <- FALSE
              trailing_active <- FALSE
              partial_tp_1_done <- FALSE
              partial_tp_2_done <- FALSE
              next
            }
          }

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

        if (exit_strategy == "chandelier") {
          if (!is.na(chandelier_long_vec[i])) {
            current_sl <- max(current_sl, chandelier_long_vec[i])
            trailing_active <- TRUE
          }
        }

        else if (exit_strategy == "breakeven_trailing") {
          if (!is.na(atr_i)) {
            if (!breakeven_moved && current_profit >= breakeven_trigger_atr * atr_i) {
              current_sl <- current_entry + (breakeven_offset_atr * atr_i)
              breakeven_moved <- TRUE
            }

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

      } else {

        if (low_vec[i] < lowest_price) {
          lowest_price <- low_vec[i]
        }

        current_profit <- current_entry - lowest_price

        if (tp_strategy == "partial" && !is.na(atr_i)) {

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

        if (exit_strategy == "chandelier") {
          if (!is.na(chandelier_short_vec[i])) {
            current_sl <- min(current_sl, chandelier_short_vec[i])
            trailing_active <- TRUE
          }
        }

        else if (exit_strategy == "breakeven_trailing") {
          if (!is.na(atr_i)) {
            if (!breakeven_moved && current_profit >= breakeven_trigger_atr * atr_i) {
              current_sl <- current_entry - (breakeven_offset_atr * atr_i)
              breakeven_moved <- TRUE
            }

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

    position[i] <- current_position
    entry_price[i] <- current_entry
    stop_loss[i] <- current_sl
    take_profit[i] <- current_tp
    position_size[i] <- total_position_size
    num_orders[i] <- pyramid_count
  }

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

  result <- result[, !names(result) %in% c("stc_lag", "close_lag", "market_trending",
                                           "bullish_momentum", "bearish_momentum",
                                           "adx_ok", "price_conf_long", "price_conf_short",
                                           "stc_cross_up", "stc_cross_down",
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
# BACKTEST WRAPPER (aus 03_backtest angepasst)
# ==============================================================================

run_backtest <- function(data) {

  data <- calculate_indicators(
    data,
    cmo_period = CMO_PERIOD,
    vhf_period = VHF_PERIOD,
    stc_fast = STC_FAST,
    stc_slow = STC_SLOW,
    stc_cycle = STC_CYCLE,
    atr_period = ATR_PERIOD,
    adx_period = ADX_PERIOD,
    chandelier_period = CHANDELIER_PERIOD
  )

  data <- generate_signals(
    data,
    cmo_threshold = CMO_THRESHOLD,
    vhf_threshold = VHF_THRESHOLD,
    stc_entry_long = STC_ENTRY_LONG,
    stc_entry_short = STC_ENTRY_SHORT,
    use_price_confirmation = USE_PRICE_CONFIRMATION,
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
    max_bars_in_trade = MAX_BARS_IN_TRADE
  )

  perf <- calculate_performance(data)

  return(perf)
}

# ==============================================================================
# TRAINING PERFORMANCE
# ==============================================================================

cat("Berechne Training Performance\n")
cat("----------------------------------------\n")

train_perf <- run_backtest(train_data)

cat("✓ Indikatoren berechnet\n")
cat("✓ Handelssignale mit Pyramiding generiert\n\n")

cat("Echte Training Performance:\n")
cat(sprintf("  Total Return: %.4f\n", train_perf$total_return))
cat(sprintf("  Sharpe Ratio: %.4f\n", train_perf$sharpe_ratio))
cat(sprintf("  Profit Factor: %.4f\n", train_perf$profit_factor))
cat(sprintf("  Anzahl Trades: %d\n", train_perf$n_trades))
cat(sprintf("  Win Rate: %.4f\n", train_perf$win_rate))
cat(sprintf("  Max Drawdown: %.4f%%\n\n", train_perf$max_drawdown_pct))

if (METRIC == "profit_factor") {
  real_metric_train <- train_perf$profit_factor
} else if (METRIC == "sharpe_ratio") {
  real_metric_train <- train_perf$sharpe_ratio
} else if (METRIC == "total_return") {
  real_metric_train <- train_perf$total_return
}

# ==============================================================================
# MONTE CARLO PERMUTATION TEST - TRAINING
# ==============================================================================

cat("MONTE CARLO PERMUTATION TEST (TRAINING)\n")
cat("========================================\n")
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

  perm_perf <- run_backtest(train_perm)

  if (METRIC == "profit_factor") {
    perm_metric <- perm_perf$profit_factor
  } else if (METRIC == "sharpe_ratio") {
    perm_metric <- perm_perf$sharpe_ratio
  } else if (METRIC == "total_return") {
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
# TEST-DATEN PERFORMANCE
# ==============================================================================

if (nrow(test_data) > 0) {

  cat("Berechne Test Performance\n")
  cat("----------------------------------------\n")

  test_perf <- run_backtest(test_data)

  cat("Out-of-Sample Performance:\n")
  cat(sprintf("  Total Return: %.4f\n", test_perf$total_return))
  cat(sprintf("  Sharpe Ratio: %.4f\n", test_perf$sharpe_ratio))
  cat(sprintf("  Profit Factor: %.4f\n", test_perf$profit_factor))
  cat(sprintf("  Anzahl Trades: %d\n", test_perf$n_trades))
  cat(sprintf("  Win Rate: %.4f\n", test_perf$win_rate))
  cat(sprintf("  Max Drawdown: %.4f%%\n\n", test_perf$max_drawdown_pct))

  if (METRIC == "profit_factor") {
    real_metric_test <- test_perf$profit_factor
  } else if (METRIC == "sharpe_ratio") {
    real_metric_test <- test_perf$sharpe_ratio
  } else if (METRIC == "total_return") {
    real_metric_test <- test_perf$total_return
  }

  # ==============================================================================
  # MONTE CARLO PERMUTATION TEST - TEST
  # ==============================================================================

  cat("MONTE CARLO PERMUTATION TEST (TEST)\n")
  cat("========================================\n")
  cat(sprintf("Führe %d Permutationen für Test-Daten durch...\n\n", N_PERMUTATIONS))

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

    perm_perf <- run_backtest(test_perm)

    if (METRIC == "profit_factor") {
      perm_metric <- perm_perf$profit_factor
    } else if (METRIC == "sharpe_ratio") {
      perm_metric <- perm_perf$sharpe_ratio
    } else if (METRIC == "total_return") {
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
# VISUALISIERUNGEN
# ==============================================================================

cat("Erstelle Visualisierungen\n")
cat("----------------------------------------\n")

# MCPT Histogram - Training
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
    title = "MCPT: TRAINING-DATEN (CMO-VHF-STC mit Pyramiding)",
    subtitle = sprintf("p-Wert = %.4f | Metrik: %s | N = %d",
                       p_value_train, METRIC, N_PERMUTATIONS),
    x = METRIC,
    y = "Dichte"
  ) +
  theme_minimal()

print(p1)

# MCPT Histogram - Test
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
      title = "MCPT: TEST-DATEN (CMO-VHF-STC mit Pyramiding)",
      subtitle = sprintf("p-Wert = %.4f | Metrik: %s | N = %d",
                         p_value_test, METRIC, N_PERMUTATIONS),
      x = METRIC,
      y = "Dichte"
    ) +
    theme_minimal()

  print(p2)
}

# Equity Curves
train_perf$data$cumulative_return <- cumsum(replace_na(train_perf$data$strategy_return, 0))
train_perf$data$equity_curve <- exp(train_perf$data$cumulative_return)

p3 <- ggplot(train_perf$data, aes(x = date, y = equity_curve)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "In-Sample Equity Curve (CMO-VHF-STC mit Pyramiding)",
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

print(p3)

if (nrow(test_data) > 0) {
  test_perf$data$cumulative_return <- cumsum(replace_na(test_perf$data$strategy_return, 0))
  test_perf$data$equity_curve <- exp(test_perf$data$cumulative_return)

  p4 <- ggplot(test_perf$data, aes(x = date, y = equity_curve)) +
    geom_line(color = "darkblue", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(
      title = "Out-of-Sample Equity Curve (CMO-VHF-STC mit Pyramiding)",
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

  print(p4)
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
cat(sprintf("  CMO: Period=%d, Threshold=%.1f\n", CMO_PERIOD, CMO_THRESHOLD))
cat(sprintf("  VHF: Period=%d, Threshold=%.2f\n", VHF_PERIOD, VHF_THRESHOLD))
cat(sprintf("  STC: Fast=%d, Slow=%d, Cycle=%d\n", STC_FAST, STC_SLOW, STC_CYCLE))
cat(sprintf("  Pyramiding: %s | Method: %s | Max Orders: %d\n",
            USE_PYRAMIDING, PYRAMID_METHOD, MAX_PYRAMID_ORDERS))
cat(sprintf("  Exit Strategy: %s | TP Strategy: %s\n",
            EXIT_STRATEGY, TP_STRATEGY))
cat(sprintf("  Stop Loss: %.1fx ATR | Take Profit: %.1fx ATR\n\n",
            STOP_LOSS_ATR_MULT, FULL_TP_ATR_MULT))

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
  cat(sprintf("  Sharpe Ratio: %.4f\n", test_perf$sharpe_ratio))
  cat(sprintf("  Total Return: %.2f%%\n", test_perf$total_return * 100))
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

cat("\n========================================\n")
cat("✓ ANALYSE ABGESCHLOSSEN\n")
cat("========================================\n\n")
