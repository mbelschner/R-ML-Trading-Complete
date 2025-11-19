# Backtest - FRAMA + VHF (Vertical Horizontal Filter) Strategy mit Advanced Pyramiding & Exit Strategies
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
cat("#      Backtest für FRAMA + VHF (Vertical Horizontal Filter) (15 Min Intraday)              #\n")
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
calculate_frama <- function(close, n = 16, FC = 1, SC = 300) {
  if(length(close) < n) return(rep(NA, length(close)))

  frama <- numeric(length(close))
  frama[1:n] <- NA
  frama[n] <- close[n]

  for(i in (n+1):length(close)) {
    n1 <- close[(i-n+1):(i-n/2)]
    n2 <- close[(i-n/2+1):i]

    hl1 <- max(n1) - min(n1)
    hl2 <- max(n2) - min(n2)
    hl <- max(close[(i-n+1):i]) - min(close[(i-n+1):i])

    if(hl == 0) {
      alpha <- 0.5
    } else {
      D <- (log(hl1 + hl2) - log(hl)) / log(2)
      alpha <- exp(-4.6 * (D - 1))
      alpha <- max(2/(SC+1), min(alpha, 2/(FC+1)))
    }

    frama[i] <- alpha * close[i] + (1 - alpha) * frama[i-1]
  }

  return(frama)
}

calculate_vhf <- function(close, n = 28) {
  highest <- runMax(close, n)
  lowest <- runMin(close, n)
  sum_changes <- runSum(abs(diff(c(NA, close))), n)

  vhf <- (highest - lowest) / sum_changes
  vhf[is.infinite(vhf)] <- NA
  return(as.numeric(vhf))
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
