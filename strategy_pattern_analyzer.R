################################################################################
# STRATEGY & PATTERN ANALYZER
# Erkennt: Trend, Mean Reversion, Breakout, Momentum, Volatilität, Session
# Plus: 10 gängige Candlestick Patterns
################################################################################

library(data.table)
library(ggplot2)
library(lubridate)
library(zoo)

################################################################################
# 1. CANDLESTICK PATTERN IDENTIFIER
################################################################################

identify_candlestick_patterns <- function(dt) {
  # dt muss enthalten: open, high, low, close
  # Alle Patterns geben TRUE/FALSE zurück
  
  dt[, `:=`(
    # Body und Wick Berechnungen
    body = abs(close - open),
    upper_wick = high - pmax(open, close),
    lower_wick = pmin(open, close) - low,
    body_middle = (open + close) / 2,
    candle_range = high - low,
    
    # Bullish/Bearish
    is_bullish = close > open,
    is_bearish = close < open
  )]
  
  # Previous candle values
  dt[, `:=`(
    prev_open = shift(open, 1),
    prev_high = shift(high, 1),
    prev_low = shift(low, 1),
    prev_close = shift(close, 1),
    prev_body = shift(body, 1),
    prev_is_bullish = shift(is_bullish, 1),
    prev_is_bearish = shift(is_bearish, 1)
  )]
  
  # Pattern 1: BULLISH ENGULFING
  dt[, bullish_engulfing := 
       prev_is_bearish & is_bullish &
       open < prev_close & close > prev_open &
       body > prev_body]
  
  # Pattern 2: BEARISH ENGULFING
  dt[, bearish_engulfing := 
       prev_is_bullish & is_bearish &
       open > prev_close & close < prev_open &
       body > prev_body]
  
  # Pattern 3: HAMMER (Bullish Reversal)
  dt[, hammer := 
       lower_wick >= 2 * body &
       upper_wick <= 0.1 * candle_range &
       body > 0]
  
  # Pattern 4: SHOOTING STAR (Bearish Reversal)
  dt[, shooting_star := 
       upper_wick >= 2 * body &
       lower_wick <= 0.1 * candle_range &
       body > 0]
  
  # Pattern 5: DOJI (Indecision)
  dt[, doji := body <= 0.1 * candle_range]
  
  # Pattern 6: MORNING STAR (3-candle bullish reversal)
  dt[, `:=`(
    prev2_close = shift(close, 2),
    prev2_open = shift(open, 2),
    prev2_is_bearish = shift(is_bearish, 2)
  )]
  
  dt[, morning_star := 
       prev2_is_bearish &
       shift(body, 1) < prev_body * 0.3 &  # Small middle candle
       is_bullish &
       close > (prev2_open + prev2_close) / 2]
  
  # Pattern 7: EVENING STAR (3-candle bearish reversal)
  dt[, `:=`(
    prev2_is_bullish = shift(is_bullish, 2)
  )]
  
  dt[, evening_star := 
       prev2_is_bullish &
       shift(body, 1) < prev_body * 0.3 &
       is_bearish &
       close < (shift(open, 2) + shift(close, 2)) / 2]
  
  # Pattern 8: PIERCING PATTERN (Bullish Reversal)
  dt[, piercing_pattern := 
       prev_is_bearish & is_bullish &
       open < prev_low &
       close > (prev_open + prev_close) / 2 &
       close < prev_open]
  
  # Pattern 9: DARK CLOUD COVER (Bearish Reversal)
  dt[, dark_cloud_cover := 
       prev_is_bullish & is_bearish &
       open > prev_high &
       close < (prev_open + prev_close) / 2 &
       close > prev_open]
  
  # Pattern 10: THREE WHITE SOLDIERS (Strong Bullish)
  dt[, three_white_soldiers := 
       is_bullish & prev_is_bullish & shift(is_bullish, 2) &
       close > prev_close & prev_close > shift(close, 2) &
       open > prev_open & open < prev_close &
       shift(open, 1) > shift(open, 2) & shift(open, 1) < shift(close, 2)]
  
  # Pattern 11: THREE BLACK CROWS (Strong Bearish)
  dt[, three_black_crows := 
       is_bearish & prev_is_bearish & shift(is_bearish, 2) &
       close < prev_close & prev_close < shift(close, 2) &
       open < prev_open & open > prev_close &
       shift(open, 1) < shift(open, 2) & shift(open, 1) > shift(close, 2)]
  
  return(dt)
}


################################################################################
# 2. STRATEGY DETECTION FUNCTIONS
################################################################################

# 2.1 TREND STRATEGY
detect_trend_strategy <- function(dt, label_col = "label", window = 10) {
  # Labels: 1 = Long Profitable, -1 = Short Profitable, 0 = Unprofitable
  
  # Berechne Trend-Indikatoren
  dt[, sma_20 := frollmean(close, 20)]
  dt[, sma_50 := frollmean(close, 50)]
  dt[, price_change := close - shift(close, window)]
  dt[, trend_strength := (close - shift(close, window)) / shift(close, window)]
  
  # Trend Detection
  dt[, trend_direction := fcase(
    sma_20 > sma_50 & close > sma_20, 1,   # Uptrend
    sma_20 < sma_50 & close < sma_20, -1,  # Downtrend
    default = 0                             # Sideways
  )]
  
  # Trend Strategy Score
  # Hoher Score = Labels aligned mit Trend
  dt[, trend_aligned := get(label_col) * trend_direction]
  
  # Rolling Window Analysis
  dt[, trend_score := frollmean(ifelse(trend_aligned > 0, 1, 0), window)]
  
  # Consecutive same-direction labels (Trend persistence)
  dt[, label_streak := {
    rle_result <- rle(get(label_col))
    rep(abs(rle_result$lengths), rle_result$lengths)
  }]
  
  dt[, avg_trend_streak := frollmean(label_streak, window)]
  
  return(dt[, .(
    strategy = "Trend",
    score = mean(trend_score, na.rm = TRUE),
    avg_streak = mean(avg_trend_streak, na.rm = TRUE),
    alignment = mean(trend_aligned > 0, na.rm = TRUE)
  )])
}


# 2.2 MEAN REVERSION STRATEGY
detect_mean_reversion <- function(dt, label_col = "label", window = 10) {
  
  # Berechne Mean Reversion Indikatoren
  dt[, sma_20 := frollmean(close, 20)]
  dt[, std_20 := frollapply(close, 20, sd)]
  dt[, z_score := (close - sma_20) / std_20]
  
  dt[, rsi := {
    changes <- close - shift(close, 1)
    gains <- ifelse(changes > 0, changes, 0)
    losses <- ifelse(changes < 0, -changes, 0)
    avg_gain <- frollmean(gains, 14)
    avg_loss <- frollmean(losses, 14)
    rs <- avg_gain / avg_loss
    100 - (100 / (1 + rs))
  }
  ]
  
  # Mean Reversion Conditions
  dt[, mr_condition := fcase(
    z_score < -1.5 & rsi < 30, 1,   # Oversold -> Long profitable expected
    z_score > 1.5 & rsi > 70, -1,   # Overbought -> Short profitable expected
    default = 0
  )]
  
  # Check if labels align with mean reversion
  dt[, mr_aligned := get(label_col) * mr_condition]
  dt[, mr_score := frollmean(ifelse(mr_aligned > 0, 1, 0), window)]
  
  # Alternation pattern (typical for mean reversion)
  dt[, label_change := get(label_col) != shift(get(label_col), 1)]
  dt[, alternation_rate := frollmean(as.numeric(label_change), window)]
  
  # Distance from mean when profitable
  dt[, profitable_distance := ifelse(
    abs(get(label_col)) == 1,
    abs(z_score),
    NA_real_
  )]
  
  return(dt[, .(
    strategy = "Mean_Reversion",
    score = mean(mr_score, na.rm = TRUE),
    alternation_rate = mean(alternation_rate, na.rm = TRUE),
    avg_distance = mean(profitable_distance, na.rm = TRUE)
  )])
}


# 2.3 BREAKOUT STRATEGY
detect_breakout <- function(dt, label_col = "label", window = 10, lookback = 20) {
  
  # Berechne Breakout Indikatoren
  dt[, high_20 := frollapply(high, lookback, max)]
  dt[, low_20 := frollapply(low, lookback, min)]
  dt[, range_20 := high_20 - low_20]
  
  # Donchian Channel Breakout
  dt[, upper_break := close > shift(high_20, 1)]
  dt[, lower_break := close < shift(low_20, 1)]
  
  # Volume spike (if available)
  if ("volume" %in% names(dt)) {
    dt[, `:=`(
      avg_volume = frollmean(volume, 20),
      volume_spike = volume > 1.5 * avg_volume
    )]
  } else {
    dt[, volume_spike := FALSE]
  }
  
  # Breakout detection
  dt[, breakout_type := fcase(
    upper_break, 1,   # Bullish breakout
    lower_break, -1,  # Bearish breakout
    default = 0
  )]
  
  # Check if labels occur after breakouts
  dt[, breakout_label_lag := {
    # Check if label appears within 3 candles after breakout
    breakout_idx <- which(breakout_type != 0)
    sapply(seq_len(.N), function(i) {
      recent_breakout <- breakout_idx[breakout_idx < i & breakout_idx >= i - 3]
      if (length(recent_breakout) > 0) {
        breakout_type[max(recent_breakout)]
      } else {
        0
      }
    })
  }]
  
  dt[, breakout_aligned := get(label_col) * breakout_label_lag]
  dt[, breakout_score := frollmean(ifelse(breakout_aligned > 0, 1, 0), window)]
  
  # Breakout continuation (labels continue in breakout direction)
  dt[, breakout_continuation := 
       breakout_type != 0 & 
       get(label_col) == breakout_type]
  
  return(dt[, .(
    strategy = "Breakout",
    score = mean(breakout_score, na.rm = TRUE),
    continuation_rate = mean(breakout_continuation, na.rm = TRUE),
    breakout_frequency = sum(abs(breakout_type) > 0, na.rm = TRUE) / .N
  )])
}


# 2.4 MOMENTUM STRATEGY
detect_momentum <- function(dt, label_col = "label", window = 10) {
  
  # Momentum Indikatoren
  dt[, roc_5 := (close - shift(close, 5)) / shift(close, 5) * 100]
  dt[, roc_10 := (close - shift(close, 10)) / shift(close, 10) * 100]
  
  # MACD - EMA Berechnungen
  alpha_12 <- 2 / (12 + 1)
  alpha_26 <- 2 / (26 + 1)
  
  dt[, ema_12 := {
    ema <- numeric(length(close))
    ema[1] <- close[1]
    for(i in 2:length(close)) {
      ema[i] <- alpha_12 * close[i] + (1 - alpha_12) * ema[i-1]
    }
    ema
  }]
  
  dt[, ema_26 := {
    ema <- numeric(length(close))
    ema[1] <- close[1]
    for(i in 2:length(close)) {
      ema[i] <- alpha_26 * close[i] + (1 - alpha_26) * ema[i-1]
    }
    ema
  }]
  
  dt[, macd := ema_12 - ema_26]
  dt[, macd_signal := frollmean(macd, 9)]
  dt[, macd_hist := macd - macd_signal]
  
  # Momentum direction
  dt[, momentum_direction := fcase(
    roc_10 > 1 & macd_hist > 0, 1,   # Strong upward momentum
    roc_10 < -1 & macd_hist < 0, -1, # Strong downward momentum
    default = 0
  )]
  
  # Check alignment
  dt[, momentum_aligned := get(label_col) * momentum_direction]
  dt[, momentum_score := frollmean(ifelse(momentum_aligned > 0, 1, 0), window)]
  
  # Acceleration (second derivative)
  dt[, momentum_acceleration := roc_5 - shift(roc_5, 5)]
  
  # Labels appear during acceleration
  dt[, accel_label := ifelse(
    abs(get(label_col)) == 1,
    abs(momentum_acceleration),
    NA_real_
  )]
  
  return(dt[, .(
    strategy = "Momentum",
    score = mean(momentum_score, na.rm = TRUE),
    avg_momentum = mean(abs(roc_10), na.rm = TRUE),
    avg_accel_at_signal = mean(accel_label, na.rm = TRUE)
  )])
}


# 2.5 VOLATILITY STRATEGY
detect_volatility <- function(dt, label_col = "label", window = 10) {
  
  # Volatility Indikatoren
  dt[, tr := pmax(
    high - low,
    abs(high - shift(close, 1)),
    abs(low - shift(close, 1)),
    na.rm = TRUE
  )]
  
  dt[, atr := frollmean(tr, 14)]
  
  # Bollinger Bands Width
  dt[, bb_middle := frollmean(close, 20)]
  dt[, bb_std := frollapply(close, 20, sd)]
  dt[, bb_width := (bb_std * 2) / bb_middle * 100]
  
  # Volatility regime
  dt[, atr_ma := frollmean(atr, 20)]
  dt[, atr_ratio := atr / atr_ma]
  dt[, bb_percentile := percent_rank(bb_width)]
  
  dt[, vol_regime := fcase(
    atr_ratio > 1.2, "High",
    atr_ratio < 0.8, "Low",
    default = "Normal"
  )]
  
  # Expansion/Contraction
  dt[, vol_expanding := atr > shift(atr, 1)]
  dt[, vol_contracting := atr < shift(atr, 1)]
  
  # Check if labels appear after volatility changes
  dt[, vol_expansion_label := {
    expansion_idx <- which(vol_expanding & shift(vol_contracting, 1))
    sapply(seq_len(.N), function(i) {
      any(expansion_idx < i & expansion_idx >= i - 3)
    })
  }]
  
  dt[, vol_score := mean(abs(get(label_col)[vol_expansion_label]) == 1, 
                         na.rm = TRUE)]
  
  # Profitable in high vs low volatility
  dt[, `:=`(
    profitable_in_high_vol = mean(abs(get(label_col)[vol_regime == "High"]) == 1, 
                                  na.rm = TRUE),
    profitable_in_low_vol = mean(abs(get(label_col)[vol_regime == "Low"]) == 1, 
                                 na.rm = TRUE)
  )]
  
  return(dt[, .(
    strategy = "Volatility",
    score = first(vol_score),
    high_vol_profit_rate = first(profitable_in_high_vol),
    low_vol_profit_rate = first(profitable_in_low_vol),
    expansion_signal_rate = mean(vol_expansion_label, na.rm = TRUE)
  )])
}


# 2.6 SESSION-BASED STRATEGY
detect_session_strategy <- function(dt, label_col = "label", timezone = "UTC") {
  
  # Stelle sicher, dass timestamp POSIXct ist
  if (!"POSIXct" %in% class(dt$timestamp)) {
    ts_class <- class(dt$timestamp)
    if ("character" %in% ts_class) {
      dt[, timestamp := as.POSIXct(get("timestamp"), tz = "UTC")]
    } else if ("numeric" %in% ts_class || "integer" %in% ts_class) {
      dt[, timestamp := as.POSIXct(get("timestamp"), origin = "1970-01-01", tz = "UTC")]
    } else {
      dt[, timestamp := as.POSIXct(as.character(get("timestamp")), tz = "UTC")]
    }
  }
  
  # Session Definition (für Gold / Forex typisch)
  dt[, hour_utc := hour(with_tz(timestamp, "UTC"))]
  
  dt[, session := fcase(
    hour_utc >= 0 & hour_utc < 8, "Asian",      # 00:00-08:00 UTC
    hour_utc >= 8 & hour_utc < 16, "London",    # 08:00-16:00 UTC
    hour_utc >= 13 & hour_utc < 21, "NY",       # 13:00-21:00 UTC (overlap 13-16)
    hour_utc >= 16 & hour_utc < 21, "NY_only",  # 16:00-21:00 UTC
    default = "After_Hours"
  )]
  
  # Overlap sessions (beste Liquidität)
  dt[, is_overlap := session == "NY" & hour_utc >= 13 & hour_utc < 16]
  
  # Session performance
  session_stats <- dt[, .(
    profit_rate = mean(abs(get(label_col)) == 1, na.rm = TRUE),
    long_rate = mean(get(label_col) == 1, na.rm = TRUE),
    short_rate = mean(get(label_col) == -1, na.rm = TRUE),
    count = .N
  ), by = session]
  
  # Best session
  best_session <- session_stats[which.max(profit_rate)]$session
  
  # Opening hour effect (erste Stunde jeder Session)
  dt[, is_opening_hour := hour_utc %in% c(0, 8, 13)]
  
  opening_performance <- dt[, .(
    opening_profit_rate = mean(abs(get(label_col)[is_opening_hour]) == 1, 
                               na.rm = TRUE),
    normal_profit_rate = mean(abs(get(label_col)[!is_opening_hour]) == 1, 
                              na.rm = TRUE)
  )]
  
  # Overlap performance
  overlap_performance <- dt[, .(
    overlap_profit_rate = mean(abs(get(label_col)[is_overlap]) == 1, 
                               na.rm = TRUE),
    non_overlap_profit_rate = mean(abs(get(label_col)[!is_overlap]) == 1, 
                                   na.rm = TRUE)
  )]
  
  return(list(
    strategy = "Session",
    session_stats = session_stats,
    best_session = best_session,
    opening_effect = opening_performance,
    overlap_effect = overlap_performance,
    overall_score = max(session_stats$profit_rate, na.rm = TRUE)
  ))
}


################################################################################
# 3. MASTER ANALYSIS FUNCTION
################################################################################

analyze_all_strategies <- function(dt, label_col = "label", 
                                   include_patterns = TRUE) {
  
  # Timestamp Fix - ensure POSIXct format
  if ("timestamp" %in% names(dt)) {
    if (!"POSIXct" %in% class(dt$timestamp)) {
      cat("Converting timestamp to POSIXct format...\n")
      ts_class <- class(dt$timestamp)
      if ("character" %in% ts_class) {
        dt[, timestamp := as.POSIXct(get("timestamp"), tz = "UTC")]
      } else if ("numeric" %in% ts_class || "integer" %in% ts_class) {
        dt[, timestamp := as.POSIXct(get("timestamp"), origin = "1970-01-01", tz = "UTC")]
      } else {
        dt[, timestamp := as.POSIXct(as.character(get("timestamp")), tz = "UTC")]
      }
      cat("Timestamp converted successfully.\n")
    }
  }
  
  # Candlestick Patterns hinzufügen
  if (include_patterns) {
    dt <- identify_candlestick_patterns(dt)
  }
  
  # Alle Strategien analysieren
  results <- list(
    trend = detect_trend_strategy(dt, label_col),
    mean_reversion = detect_mean_reversion(dt, label_col),
    breakout = detect_breakout(dt, label_col),
    momentum = detect_momentum(dt, label_col),
    volatility = detect_volatility(dt, label_col),
    session = detect_session_strategy(dt, label_col)
  )
  
  # Kombiniere Ergebnisse
  strategy_scores <- rbindlist(list(
    results$trend,
    results$mean_reversion,
    results$breakout,
    results$momentum,
    results$volatility,
    data.table(strategy = "Session", 
               score = results$session$overall_score)
  ), fill = TRUE)
  
  # Sortiere nach Score
  strategy_scores <- strategy_scores[order(-score)]
  
  # Pattern Frequency (wenn aktiviert)
  if (include_patterns) {
    pattern_cols <- c("bullish_engulfing", "bearish_engulfing", "hammer",
                      "shooting_star", "doji", "morning_star", "evening_star",
                      "piercing_pattern", "dark_cloud_cover", 
                      "three_white_soldiers", "three_black_crows")
    
    pattern_frequency <- dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE) / .N * 100),
                            .SDcols = pattern_cols]
    
    pattern_frequency <- melt(pattern_frequency, 
                              measure.vars = pattern_cols,
                              variable.name = "pattern",
                              value.name = "frequency_pct")
    
    # Pattern Profitability
    pattern_profitability <- lapply(pattern_cols, function(p) {
      dt[get(p) == TRUE, .(
        pattern = p,
        profit_rate = mean(abs(get(label_col)) == 1, na.rm = TRUE),
        long_rate = mean(get(label_col) == 1, na.rm = TRUE),
        short_rate = mean(get(label_col) == -1, na.rm = TRUE),
        count = .N
      )]
    })
    
    pattern_profitability <- rbindlist(pattern_profitability)
    pattern_profitability <- pattern_profitability[order(-profit_rate)]
    
    results$patterns <- list(
      frequency = pattern_frequency,
      profitability = pattern_profitability
    )
  }
  
  return(list(
    strategy_ranking = strategy_scores,
    detailed_results = results,
    enhanced_data = dt
  ))
}


################################################################################
# 4. VISUALIZATION FUNCTIONS
################################################################################

plot_strategy_scores <- function(analysis_results) {
  
  scores <- analysis_results$strategy_ranking
  
  p <- ggplot(scores, aes(x = reorder(strategy, score), y = score)) +
    geom_col(aes(fill = score), show.legend = FALSE) +
    scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                         midpoint = 0.5) +
    coord_flip() +
    labs(
      title = "Strategy Performance Ranking",
      subtitle = "Score basierend auf Label-Alignment",
      x = "Strategy",
      y = "Performance Score"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.major.y = element_blank()
    )
  
  return(p)
}


plot_pattern_analysis <- function(analysis_results) {
  
  if (!"patterns" %in% names(analysis_results$detailed_results)) {
    stop("Pattern analysis not included. Set include_patterns = TRUE")
  }
  
  prof <- analysis_results$detailed_results$patterns$profitability
  prof <- prof[count >= 5]  # Filter rare patterns
  
  p <- ggplot(prof, aes(x = reorder(pattern, profit_rate), y = profit_rate)) +
    geom_col(aes(fill = profit_rate), show.legend = FALSE) +
    geom_text(aes(label = paste0(round(profit_rate * 100, 1), "%")),
              hjust = -0.1, size = 3) +
    scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                         midpoint = 0.5) +
    coord_flip() +
    labs(
      title = "Candlestick Pattern Profitability",
      subtitle = "Profit Rate bei Pattern-Auftritt",
      x = "Pattern",
      y = "Profit Rate"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.major.y = element_blank()
    ) +
    ylim(0, 1)
  
  return(p)
}


plot_session_heatmap <- function(analysis_results) {
  
  session_data <- analysis_results$detailed_results$session$session_stats
  
  p <- ggplot(session_data, aes(x = session, y = 1, fill = profit_rate)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = paste0(round(profit_rate * 100, 1), "%")),
              size = 5, fontface = "bold") +
    scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                         midpoint = 0.5,
                         name = "Profit Rate") +
    labs(
      title = "Session-Based Performance",
      subtitle = "Profitabilität nach Handelszeit",
      x = "Trading Session",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank()
    )
  
  return(p)
}


################################################################################
# 5. EXAMPLE USAGE
################################################################################

# Beispiel-Daten laden und analysieren
# dt <- fread("your_data.csv")
# dt muss enthalten:
#   - timestamp (POSIXct oder character)
#   - open, high, low, close
#   - label (1 = Long Profitable, -1 = Short Profitable, 0 = Unprofitable)

example_analysis <- function(file_path) {
  
  # Daten laden
  dt <- fread(file_path)
  
  # Timestamp konvertieren falls nötig
  if (is.character(dt$timestamp)) {
    dt[, timestamp := as.POSIXct(timestamp)]
  }
  
  # Vollständige Analyse durchführen
  cat("Starting comprehensive strategy analysis...\n")
  results <- analyze_all_strategies(dt, label_col = "label", 
                                    include_patterns = TRUE)
  
  # Ergebnisse ausgeben
  cat("\n=== STRATEGY RANKING ===\n")
  print(results$strategy_ranking)
  
  cat("\n=== SESSION ANALYSIS ===\n")
  print(results$detailed_results$session$session_stats)
  
  cat("\n=== TOP CANDLESTICK PATTERNS ===\n")
  print(head(results$detailed_results$patterns$profitability, 5))
  
  # Plots erstellen
  p1 <- plot_strategy_scores(results)
  p2 <- plot_pattern_analysis(results)
  p3 <- plot_session_heatmap(results)
  
  # Plots speichern
  ggsave("strategy_ranking.png", p1, width = 10, height = 6, dpi = 300)
  ggsave("pattern_profitability.png", p2, width = 10, height = 6, dpi = 300)
  ggsave("session_heatmap.png", p3, width = 10, height = 4, dpi = 300)
  
  cat("\nPlots saved successfully!\n")
  
  return(results)
}

# USAGE:
# results <- example_analysis("directional_labels.csv")
# enhanced_dt <- results$enhanced_data  # Enthält alle Pattern & Indicators