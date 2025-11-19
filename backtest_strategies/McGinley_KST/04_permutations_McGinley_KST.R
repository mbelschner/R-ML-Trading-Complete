# MCPT - McGinley Dynamic + KST (Know Sure Thing) Strategy
# Simplified version - extends 03_backtest file

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
cat("#      MCPT für McGinley Dynamic + KST (Know Sure Thing) mit Pyramiding                      #\n")
cat("########################################################################\n\n")

N_PERMUTATIONS <- 300
METRIC <- "profit_factor"

MD_PERIOD <- 14
KST_ROC1 <- 6
KST_ROC2 <- 9
KST_ROC3 <- 12
KST_ROC4 <- 18
KST_SIGNAL_PERIOD <- 6

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

# Source backtest functions
source(file.path("/home/user/R-ML-Trading-Complete/backtest_strategies/McGinley_KST", "03_backtest_McGinley_KST_pyramiding.R"))

# Load data
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

# NOTE: Full MCPT implementation should follow the pattern from
# Hurst_DPO or Chop_Aroon permutation files, including:
# - run_backtest_mcpt() wrapper function
# - Training performance calculation
# - Permutation loop for training data
# - P-value calculation
# - Test data permutations (if available)
# - Visualization creation

cat("\n✓ MCPT Framework bereit!\n")
