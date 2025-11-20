# ==============================================================================
# Backtest - Choppiness-Aroon Strategy mit Advanced Pyramiding & Exit Strategies
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
cat("#      Backtest für Choppiness-Aroon Strategie (15 Min Intraday)     #\n")
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
# Indikatoren Funktionen
# ==============================================================================

calculate_choppiness <- function(high, low, close, n = 14) {
  atr_sum <- runSum(ATR(cbind(high, low, close), n = 1)[,2], n)
  high_low_range <- runMax(high, n) - runMin(low, n)

  chop <- 100 * log10(atr_sum / high_low_range) / log10(n)
  return(as.numeric(chop))
}

calculate_aroon <- function(high, low, n = 25) {
  aroon_data <- aroon(cbind(high, low), n = n)
  aroon_osc <- aroon_data[, "oscillator"]
  return(as.numeric(aroon_osc))
}

# ==============================================================================
# INDIKATOREN BERECHNEN
# ==============================================================================

calculate_indicators <- function(data,
                                 chop_period = 14,
                                 aroon_period = 25,
                                 atr_period = 14,
                                 adx_period = 14,
                                 chandelier_period = 22) {

  # Basis-Indikatoren
  data$chop <- calculate_choppiness(data$high, data$low, data$close, n = chop_period)
  data$aroon_osc <- calculate_aroon(data$high, data$low, n = aroon_period)
  data$atr <- ATR(cbind(data$high, data$low, data$close), n = atr_period)[, "atr"]
  data$adx <- ADX(cbind(data$high, data$low, data$close), n = adx_period)[, "ADX"]

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
                             # Entry Signal Parameter
                             chop_threshold = 38.2,
                             aroon_threshold = 50,
                             use_adx_filter = FALSE,
                             adx_threshold = 20,

                             # Initial Stop Loss
                             stop_loss_atr_mult = 3.0,

                             # Pyramiding Parameter
                             use_pyramiding = TRUE,
                             pyramid_method = "breakout",
                             max_pyramid_orders = 3,
                             pyramid_spacing_atr = 0.5,
                             pyramid_size_multiplier = 0.5,

                             # Pyramiding: Consecutive
                             consecutive_bars = 2,

                             # Pyramiding: Breakout
                             breakout_lookback = 20,

                             # Exit Strategy
                             exit_strategy = "chandelier",

                             # Chandelier Exit
                             chandelier_multiplier = 3.0,
                             chandelier_period = 22,

                             # Breakeven + Trailing
                             breakeven_trigger_atr = 1.5,
                             breakeven_offset_atr = 0.2,
                             trailing_stop_atr_mult = 2.0,
                             trailing_start_atr_mult = 1.5,

                             # Take Profit Strategy
                             tp_strategy = "full",
                             full_tp_atr_mult = 4.5,

                             # Partial TP
                             partial_tp_1_atr = 2.0,
                             partial_tp_1_size = 0.33,
                             partial_tp_2_atr = 3.5,
                             partial_tp_2_size = 0.33,

                             # Time Exit
                             close_time_hour = 22) {

  dt <- as.data.table(data)
  n <- nrow(dt)

  # ==========================================
  # VEKTORISIERTE SIGNAL-GENERIERUNG
  # ==========================================

  dt[, not_choppy := chop < chop_threshold]

  if (use_adx_filter) {
    dt[, adx_ok := adx >= adx_threshold]
  } else {
    dt[, adx_ok := TRUE]
  }

  # Entry Signals - Choppiness-Aroon Strategy
  dt[, signal_raw := 0]

  # Long: Not choppy + Strong uptrend
  dt[not_choppy == TRUE &
       aroon_osc > aroon_threshold &
       adx_ok == TRUE,
     signal_raw := 1]

  # Short: Not choppy + Strong downtrend
  dt[not_choppy == TRUE &
       aroon_osc < -aroon_threshold &
       adx_ok == TRUE,
     signal_raw := -1]

  # Chandelier Exit Berechnung
  dt[, chandelier_long := highest_high - (chandelier_multiplier * atr)]
  dt[, chandelier_short := lowest_low + (chandelier_multiplier * atr)]

  # ==========================================
  # POSITION MANAGEMENT (KOMPLETT VON VORLAGE)
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

  current_position <- 0L
  current_entry <- NA_real_
  current_sl <- NA_real_
  current_tp <- NA_real_
  pyramid_count <- 0L
  pyramid_entries <- numeric(0)
  total_position_size <- 0
  highest_price <- -Inf
  lowest_price <- Inf
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
      breakeven_moved <- FALSE
      trailing_active <- FALSE
      partial_tp_1_done <- FALSE
      partial_tp_2_done <- FALSE
    }
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
    else if (current_position != 0L && !is.na(current_sl) && !is.na(current_tp)) {

      atr_i <- atr_vec[i]

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

  result <- result[, !names(result) %in% c("not_choppy", "adx_ok",
                                           "chandelier_long", "chandelier_short")]

  return(result)
}

# ==============================================================================
# PERFORMANCE BERECHNEN (GLEICH WIE VORLAGE)
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
    chop_period = params$chop_period,
    aroon_period = params$aroon_period,
    atr_period = params$atr_period,
    adx_period = params$adx_period,
    chandelier_period = params$chandelier_period
  )

  data <- generate_signals(
    data,
    chop_threshold = params$chop_threshold,
    aroon_threshold = params$aroon_threshold,
    use_adx_filter = params$use_adx_filter,
    adx_threshold = params$adx_threshold,
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
    close_time_hour = params$close_time_hour
  )

  perf <- calculate_performance(data)

  return(perf)
}

# ==============================================================================
# PARAMETER-OPTIMIERUNG (VORLAGE)
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
                              "calculate_choppiness",
                              "calculate_aroon"),
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
# PARAMETER-GRID
# ==============================================================================

param_grid <- expand.grid(
  # Entry Signal Parameter
  chop_period = c(14),
  chop_threshold = c(38.2, 50),
  aroon_period = c(20, 25),
  aroon_threshold = c(40, 50),
  atr_period = 14,
  adx_period = 14,
  use_adx_filter = FALSE,
  adx_threshold = 20,

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
  close_time_hour = 22) {
)

param_grid <- param_grid %>%
  filter(partial_tp_1_atr < partial_tp_2_atr)

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

cat("\n✅ Optimierung abgeschlossen!\n")
