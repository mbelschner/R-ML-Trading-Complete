rm(list=ls())
gc()

pacman::p_load(data.table, tidyverse, lubridate, zoo, patchwork)

# =============================================================================
# DATEN LADEN
# =============================================================================

#Konfiguration
EPIC = "GOLD"
INTERVAL = "MINUTE_15"

filename = paste0(EPIC, "_", INTERVAL, ".csv")

# Pfad zu deinen Daten
input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")

reduced_df = fread(file.path(output_path, paste0(EPIC, "_reduced_df.csv")))

source(file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete/R-ML-Trading-Complete/strategy_pattern_analyzer.R"))

# =============================================================================
# CONDUCT ANALYSIS
# =============================================================================

dt = reduced_df %>%
  select(datetime, open, high, low, close, label)

setnames(dt, "datetime", "timestamp")

mean_reversion_dt = detect_mean_reversion(dt)
breakout_dt = detect_breakout(dt)
momentum_dt = detect_momentum(dt)
sessions_dt = detect_session_strategy(dt)
trend_dt = detect_trend_strategy(dt)
volatility_dt = detect_volatility(dt)
candles = identify_candlestick_patterns(dt)

results = analyze_all_strategies(dt)

print(results$strategy_ranking)
