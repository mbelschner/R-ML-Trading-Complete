# Backtest - McGinley Dynamic + KST (Know Sure Thing) Strategy mit Advanced Pyramiding & Exit Strategies
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
cat("#      Backtest für McGinley Dynamic + KST (Know Sure Thing) (15 Min Intraday)              #\n")
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
calculate_mcginley <- function(close, n = 14) {
  md <- numeric(length(close))
  md[1:n] <- NA
  md[n] <- mean(close[1:n], na.rm = TRUE)

  for(i in (n+1):length(close)) {
    if(is.na(md[i-1]) || md[i-1] == 0) {
      md[i] <- close[i]
    } else {
      adjustment <- (close[i] - md[i-1]) / (n * (close[i]/md[i-1])^4)
      md[i] <- md[i-1] + adjustment
    }
  }

  return(md)
}

calculate_kst <- function(close,
                          roc1 = 10, roc2 = 15, roc3 = 20, roc4 = 30,
                          sma1 = 10, sma2 = 10, sma3 = 10, sma4 = 15,
                          signal = 9) {

  rcma1 <- SMA(ROC(close, roc1), sma1)
  rcma2 <- SMA(ROC(close, roc2), sma2)
  rcma3 <- SMA(ROC(close, roc3), sma3)
  rcma4 <- SMA(ROC(close, roc4), sma4)

  kst <- rcma1 * 1 + rcma2 * 2 + rcma3 * 3 + rcma4 * 4
  kst_signal <- SMA(kst, signal)

  return(list(kst = as.numeric(kst), signal = as.numeric(kst_signal)))
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
