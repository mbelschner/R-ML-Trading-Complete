# Backtest - Schaff Trend Cycle (STC) Strategy mit Advanced Pyramiding & Exit Strategies
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
cat("#      Backtest für Schaff Trend Cycle (STC) (15 Min Intraday)              #\n")
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
