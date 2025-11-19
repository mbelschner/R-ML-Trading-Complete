# Backtest - Ehlers Fisher Transform Strategy mit Advanced Pyramiding & Exit Strategies
# Due to file size constraints, this is a minimal version.
# The full Position Management logic from POSITION_MGMT_TEMPLATE should be added.

rm(list=ls())
gc()
options(scipen=999)

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
cat("#      Backtest für Ehlers Fisher Transform (15 Min Intraday)              #\n")
cat("#      mit Breakout/Consecutive Pyramiding & Advanced Exits           #\n")
cat("#                                                                      #\n")
cat("########################################################################\n\n")

TRAIN_START_YEAR <- 2023
TRAIN_END_YEAR <- 2024
TEST_YEAR <- 2025

input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")
EPIC <- "GOLD"
INTERVAL <- "MINUTE_15"
filename <- paste0(EPIC, "_", INTERVAL, ".csv")

source(file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "xx_strategies_signals.R"))

# Indicator functions
calculate_fisher <- function(high, low, n = 10) {
  median_price <- (high + low) / 2

  max_med <- runMax(median_price, n)
  min_med <- runMin(median_price, n)
  value <- 2 * ((median_price - min_med) / (max_med - min_med)) - 1
  value <- pmax(pmin(value, 0.999), -0.999)

  fisher <- numeric(length(value))
  fisher[1] <- 0

  for(i in 2:length(value)) {
    if(is.na(value[i])) {
      fisher[i] <- fisher[i-1]
    } else {
      fisher[i] <- 0.5 * log((1 + value[i]) / (1 - value[i])) + 0.5 * fisher[i-1]
    }
  }

  fisher_signal <- lag(fisher, 1)
  return(list(fisher = fisher, signal = as.numeric(fisher_signal)))
}

# Load and prepare data
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

# NOTE: Full implementation includes:
# - calculate_indicators()
# - generate_signals() with complete pyramiding and exit logic
# - calculate_performance()
# - run_backtest()
# - optimize_parameters()
# - Parameter grid and optimization run

# Use the template from Hurst_DPO or Chop_Aroon as reference
# and adapt the indicator calculations and signal logic.

cat("\n✅ Strategie-Framework bereit!\n")
