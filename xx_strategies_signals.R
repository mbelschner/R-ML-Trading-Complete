# Hurst Exponent calculation
calculate_hurst <- function(price, n = 100) {
  if(length(price) < n) return(rep(NA, length(price)))
  
  hurst <- rep(NA, length(price))
  for(i in n:length(price)) {
    ts <- price[(i-n+1):i]
    
    # R/S Analysis
    lags <- c(10, 20, 30, 40, 50)
    rs <- numeric(length(lags))
    
    for(j in seq_along(lags)) {
      lag <- lags[j]
      if(lag >= length(ts)) next
      
      # Calculate R/S for this lag
      chunks <- split(ts, ceiling(seq_along(ts) / lag))
      rs_values <- sapply(chunks, function(chunk) {
        if(length(chunk) < 2) return(NA)
        mean_chunk <- mean(chunk)
        cumdev <- cumsum(chunk - mean_chunk)
        R <- max(cumdev) - min(cumdev)
        S <- sd(chunk)
        if(S == 0 || is.na(S)) return(NA)
        return(R / S)
      })
      rs[j] <- mean(rs_values, na.rm = TRUE)
    }
    
    # Linear regression to find Hurst
    valid <- !is.na(rs) & rs > 0
    if(sum(valid) >= 3) {
      fit <- lm(log(rs[valid]) ~ log(lags[valid]))
      hurst[i] <- coef(fit)[2]
    }
  }
  return(hurst)
}

# Detrended Price Oscillator
calculate_dpo <- function(close, n = 20) {
  displaced_ma <- SMA(close, n)
  shift <- floor(n/2) + 1
  dpo <- close - lag(displaced_ma, shift)
  return(as.numeric(dpo))
}

# Choppiness Index
calculate_choppiness <- function(high, low, close, n = 14) {
  atr_sum <- runSum(ATR(cbind(high, low, close), n = 1)[,2], n)
  high_low_range <- runMax(high, n) - runMin(low, n)
  
  chop <- 100 * log10(atr_sum / high_low_range) / log10(n)
  return(as.numeric(chop))
}

# Aroon Oscillator
calculate_aroon <- function(high, low, n = 25) {
  aroon <- aroon(cbind(high, low), n = n)
  aroon_osc <- aroon[, "oscillator"]
  return(as.numeric(aroon_osc))
}

# Schaff Trend Cycle
calculate_stc <- function(close, fast = 23, slow = 50, cycle = 10, smooth1 = 3, smooth2 = 3) {
  # Calculate MACD
  macd_line <- EMA(close, fast) - EMA(close, slow)
  
  # First stochastic
  macd_min <- runMin(macd_line, cycle)
  macd_max <- runMax(macd_line, cycle)
  stoch1 <- 100 * (macd_line - macd_min) / (macd_max - macd_min)
  stoch1[is.na(stoch1) | is.infinite(stoch1)] <- 50
  stoch1_smooth <- EMA(stoch1, smooth1)
  
  # Second stochastic
  stoch1_min <- runMin(stoch1_smooth, cycle)
  stoch1_max <- runMax(stoch1_smooth, cycle)
  stc <- 100 * (stoch1_smooth - stoch1_min) / (stoch1_max - stoch1_min)
  stc[is.na(stc) | is.infinite(stc)] <- 50
  stc <- EMA(stc, smooth2)
  
  return(as.numeric(stc))
}

# Coppock Curve (adapted for intraday)
calculate_coppock <- function(close, roc1 = 11, roc2 = 14, wma = 10) {
  roc_1 <- ROC(close, roc1, type = "discrete") * 100
  roc_2 <- ROC(close, roc2, type = "discrete") * 100
  coppock <- WMA(roc_1 + roc_2, wma)
  return(as.numeric(coppock))
}

# Vertical Horizontal Filter
calculate_vhf <- function(close, n = 28) {
  highest <- runMax(close, n)
  lowest <- runMin(close, n)
  sum_changes <- runSum(abs(diff(c(NA, close))), n)
  
  vhf <- (highest - lowest) / sum_changes
  vhf[is.infinite(vhf)] <- NA
  return(as.numeric(vhf))
}

# Chande Momentum Oscillator
calculate_cmo <- function(close, n = 14) {
  cmo <- CMO(close, n)
  return(as.numeric(cmo))
}

# Fractal Adaptive Moving Average
calculate_frama <- function(close, n = 16, FC = 1, SC = 300) {
  if(length(close) < n) return(rep(NA, length(close)))
  
  frama <- numeric(length(close))
  frama[1:n] <- NA
  frama[n] <- close[n]
  
  for(i in (n+1):length(close)) {
    # Calculate fractal dimension
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

# McGinley Dynamic
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

# Know Sure Thing
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

# Ehlers Fisher Transform
calculate_fisher <- function(high, low, n = 10) {
  median_price <- (high + low) / 2
  
  # Normalize to -1 to +1
  max_med <- runMax(median_price, n)
  min_med <- runMin(median_price, n)
  value <- 2 * ((median_price - min_med) / (max_med - min_med)) - 1
  value <- pmax(pmin(value, 0.999), -0.999)  # Limit to prevent log errors
  
  # Fisher Transform
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


# ============================================================================
# STRATEGY 1: HURST + DPO
# ============================================================================
strategy_hurst_dpo <- function(data, 
                               hurst_period = 100,
                               hurst_threshold = 0.55,
                               dpo_period = 20,
                               lookback_slope = 3) {
  
  # Calculate indicators
  hurst <- calculate_hurst(data$close, n = hurst_period)
  dpo <- calculate_dpo(data$close, n = dpo_period)
  
  # Price slope for confirmation
  price_slope <- (data$close - lag(data$close, lookback_slope)) / lookback_slope
  
  signals <- rep(0, nrow(data))
  
  for(i in 2:nrow(data)) {
    if(is.na(hurst[i]) || is.na(dpo[i]) || is.na(dpo[i-1])) next
    
    # Long: Hurst shows trending + DPO crosses above 0 + price rising
    if(hurst[i] > hurst_threshold && 
       dpo[i-1] < 0 && dpo[i] > 0 && 
       price_slope[i] > 0) {
      signals[i] <- 1
    }
    
    # Short: Hurst shows trending + DPO crosses below 0 + price falling
    if(hurst[i] > hurst_threshold && 
       dpo[i-1] > 0 && dpo[i] < 0 && 
       price_slope[i] < 0) {
      signals[i] <- -1
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 2: CHOPPINESS + AROON
# ============================================================================
strategy_chop_aroon <- function(data,
                                chop_period = 14,
                                chop_threshold = 38.2,
                                aroon_period = 25,
                                aroon_threshold = 50) {
  
  # Calculate indicators
  chop <- calculate_choppiness(data$high, data$low, data$close, n = chop_period)
  aroon_osc <- calculate_aroon(data$high, data$low, n = aroon_period)
  
  signals <- rep(0, nrow(data))
  
  for(i in 2:nrow(data)) {
    if(is.na(chop[i]) || is.na(aroon_osc[i])) next
    
    # Only trade when market is NOT choppy
    if(chop[i] < chop_threshold) {
      
      # Long: Strong uptrend
      if(aroon_osc[i] > aroon_threshold) {
        signals[i] <- 1
      }
      
      # Short: Strong downtrend
      if(aroon_osc[i] < -aroon_threshold) {
        signals[i] <- -1
      }
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 3: SCHAFF TREND CYCLE
# ============================================================================
strategy_stc <- function(data,
                         stc_fast = 23,
                         stc_slow = 50,
                         stc_cycle = 10,
                         entry_level_long = 25,
                         entry_level_short = 75,
                         exit_level_long = 75,
                         exit_level_short = 25) {
  
  # Calculate STC
  stc <- calculate_stc(data$close, fast = stc_fast, slow = stc_slow, cycle = stc_cycle)
  
  signals <- rep(0, nrow(data))
  position <- 0  # Track current position
  
  for(i in 2:nrow(data)) {
    if(is.na(stc[i]) || is.na(stc[i-1])) next
    
    # Long entry: STC crosses above 25
    if(position == 0 && stc[i-1] < entry_level_long && stc[i] > entry_level_long) {
      signals[i] <- 1
      position <- 1
    }
    
    # Long exit: STC crosses above 75
    if(position == 1 && stc[i] > exit_level_long) {
      signals[i] <- 0
      position <- 0
    }
    
    # Short entry: STC crosses below 75
    if(position == 0 && stc[i-1] > entry_level_short && stc[i] < entry_level_short) {
      signals[i] <- -1
      position <- -1
    }
    
    # Short exit: STC crosses below 25
    if(position == -1 && stc[i] < exit_level_short) {
      signals[i] <- 0
      position <- 0
    }
    
    # Carry forward position
    if(signals[i] == 0 && position != 0) {
      signals[i] <- position
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 4: COPPOCK + MULTI-ROC ALIGNMENT
# ============================================================================
strategy_coppock_roc <- function(data,
                                 coppock_roc1 = 7,  # Shortened for intraday
                                 coppock_roc2 = 10,
                                 coppock_wma = 8,
                                 roc_short = 3,
                                 roc_medium = 5,
                                 roc_long = 8) {
  
  # Calculate Coppock
  coppock <- calculate_coppock(data$close, roc1 = coppock_roc1, 
                               roc2 = coppock_roc2, wma = coppock_wma)
  
  # Calculate multiple ROCs
  roc_s <- ROC(data$close, roc_short)
  roc_m <- ROC(data$close, roc_medium)
  roc_l <- ROC(data$close, roc_long)
  
  signals <- rep(0, nrow(data))
  
  for(i in 2:nrow(data)) {
    if(is.na(coppock[i]) || is.na(coppock[i-1]) || 
       is.na(roc_s[i]) || is.na(roc_m[i]) || is.na(roc_l[i])) next
    
    # Long: Coppock turns positive + all ROCs positive
    if(coppock[i-1] < 0 && coppock[i] > 0 &&
       roc_s[i] > 0 && roc_m[i] > 0 && roc_l[i] > 0) {
      signals[i] <- 1
    }
    
    # Short: Coppock turns negative + all ROCs negative
    if(coppock[i-1] > 0 && coppock[i] < 0 &&
       roc_s[i] < 0 && roc_m[i] < 0 && roc_l[i] < 0) {
      signals[i] <- -1
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 5: FISHER TRANSFORM
# ============================================================================
strategy_fisher <- function(data,
                            fisher_period = 10,
                            confirmation_bars = 2) {
  
  # Calculate Fisher Transform
  fisher_data <- calculate_fisher(data$high, data$low, n = fisher_period)
  fisher <- fisher_data$fisher
  fisher_signal <- fisher_data$signal
  
  signals <- rep(0, nrow(data))
  
  for(i in (confirmation_bars + 1):nrow(data)) {
    if(is.na(fisher[i]) || is.na(fisher_signal[i])) next
    
    # Check for sustained cross (confirmation)
    long_cross <- all(fisher[(i-confirmation_bars+1):i] > fisher_signal[(i-confirmation_bars+1):i])
    short_cross <- all(fisher[(i-confirmation_bars+1):i] < fisher_signal[(i-confirmation_bars+1):i])
    
    # Long: Fisher crosses above signal and stays above
    if(long_cross && fisher[i-confirmation_bars] <= fisher_signal[i-confirmation_bars]) {
      signals[i] <- 1
    }
    
    # Short: Fisher crosses below signal and stays below
    if(short_cross && fisher[i-confirmation_bars] >= fisher_signal[i-confirmation_bars]) {
      signals[i] <- -1
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 6: FRAMA + VHF
# ============================================================================
strategy_frama_vhf <- function(data,
                               frama_period = 16,
                               vhf_period = 28,
                               vhf_threshold = 0.35) {
  
  # Calculate indicators
  frama <- calculate_frama(data$close, n = frama_period)
  vhf <- calculate_vhf(data$close, n = vhf_period)
  
  signals <- rep(0, nrow(data))
  
  for(i in 2:nrow(data)) {
    if(is.na(frama[i]) || is.na(frama[i-1]) || is.na(vhf[i])) next
    
    # Only trade when VHF shows trending market
    if(vhf[i] > vhf_threshold) {
      
      # Long: Price crosses above FRAMA + FRAMA rising
      if(data$close[i-1] <= frama[i-1] && data$close[i] > frama[i] &&
         frama[i] > frama[i-1]) {
        signals[i] <- 1
      }
      
      # Short: Price crosses below FRAMA + FRAMA falling
      if(data$close[i-1] >= frama[i-1] && data$close[i] < frama[i] &&
         frama[i] < frama[i-1]) {
        signals[i] <- -1
      }
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 7: MCGINLEY DYNAMIC + KST
# ============================================================================
strategy_mcginley_kst <- function(data,
                                  md_period = 14,
                                  kst_roc1 = 6, kst_roc2 = 9, 
                                  kst_roc3 = 12, kst_roc4 = 18,
                                  kst_signal = 6) {
  
  # Calculate indicators
  md <- calculate_mcginley(data$close, n = md_period)
  kst_data <- calculate_kst(data$close, 
                            roc1 = kst_roc1, roc2 = kst_roc2,
                            roc3 = kst_roc3, roc4 = kst_roc4,
                            signal = kst_signal)
  kst <- kst_data$kst
  kst_sig <- kst_data$signal
  
  signals <- rep(0, nrow(data))
  
  for(i in 2:nrow(data)) {
    if(is.na(md[i]) || is.na(kst[i]) || is.na(kst_sig[i])) next
    
    # Long: Price > MD + KST > 0 + KST > Signal + KST rising
    if(data$close[i] > md[i] &&
       kst[i] > 0 && kst[i] > kst_sig[i] &&
       kst[i] > kst[i-1]) {
      signals[i] <- 1
    }
    
    # Short: Price < MD + KST < 0 + KST < Signal + KST falling
    if(data$close[i] < md[i] &&
       kst[i] < 0 && kst[i] < kst_sig[i] &&
       kst[i] < kst[i-1]) {
      signals[i] <- -1
    }
  }
  
  return(signals)
}


# ============================================================================
# STRATEGY 8: CMO + VHF + STC (The "Wild Card" Combo)
# ============================================================================
strategy_cmo_vhf_stc <- function(data,
                                 cmo_period = 14,
                                 cmo_threshold = 20,
                                 vhf_period = 28,
                                 vhf_threshold = 0.35,
                                 stc_fast = 23,
                                 stc_slow = 50,
                                 stc_cycle = 10,
                                 stc_entry_long = 25,
                                 stc_entry_short = 75) {
  
  # Calculate indicators
  cmo <- calculate_cmo(data$close, n = cmo_period)
  vhf <- calculate_vhf(data$close, n = vhf_period)
  stc <- calculate_stc(data$close, fast = stc_fast, slow = stc_slow, cycle = stc_cycle)
  
  signals <- rep(0, nrow(data))
  
  for(i in 2:nrow(data)) {
    if(is.na(cmo[i]) || is.na(vhf[i]) || is.na(stc[i]) || is.na(stc[i-1])) next
    
    # Setup: Market must be trending + CMO shows momentum
    market_trending <- vhf[i] > vhf_threshold
    bullish_momentum <- cmo[i] > cmo_threshold
    bearish_momentum <- cmo[i] < -cmo_threshold
    
    # Long: STC crosses 25 + setup conditions
    if(market_trending && bullish_momentum &&
       stc[i-1] < stc_entry_long && stc[i] > stc_entry_long &&
       data$close[i] > data$close[i-1]) {  # Price confirmation
      signals[i] <- 1
    }
    
    # Short: STC crosses 75 + setup conditions
    if(market_trending && bearish_momentum &&
       stc[i-1] > stc_entry_short && stc[i] < stc_entry_short &&
       data$close[i] < data$close[i-1]) {  # Price confirmation
      signals[i] <- -1
    }
  }
  
  return(signals)
}


# ============================================================================
# WRAPPER FUNCTION TO TEST ALL STRATEGIES
# ============================================================================
test_all_strategies <- function(data) {
  
  strategies <- list(
    hurst_dpo = strategy_hurst_dpo(data),
    chop_aroon = strategy_chop_aroon(data),
    stc = strategy_stc(data),
    coppock_roc = strategy_coppock_roc(data),
    fisher = strategy_fisher(data),
    frama_vhf = strategy_frama_vhf(data),
    mcginley_kst = strategy_mcginley_kst(data),
    cmo_vhf_stc = strategy_cmo_vhf_stc(data)
  )
  
  return(strategies)
}

