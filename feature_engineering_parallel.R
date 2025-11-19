# ============================================================================
# Feature Engineering mit PARALLEL PROCESSING
# Basierend auf: López de Prado (2018, Chapter 20) und Jansen (2020)
# ============================================================================

# Benötigte Pakete laden
library(tidyverse)      # Datenmanipulation
library(TTR)            # Technische Indikatoren
library(xgboost)        # XGBoost Modell
library(caret)          # Cross-Validation
library(lubridate)      # Datum/Zeit
library(data.table)     # Effiziente Datenverarbeitung
library(foreach)        # Parallel Loops
library(doParallel)     # Parallel Backend
library(parallel)       # Core Detection

# ============================================================================
# PARALLEL PROCESSING SETUP
# ============================================================================

# López de Prado (2018, Chapter 20): Multiprocessing für schnellere Berechnungen
setup_parallel <- function(n_cores = NULL, verbose = TRUE) {
  # Automatische Core-Detection wenn nicht spezifiziert
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1  # Lasse 1 Core für System frei
  }
  
  # Maximum: verwende nicht mehr Cores als verfügbar
  max_cores <- parallel::detectCores()
  n_cores <- min(n_cores, max_cores)
  
  if (verbose) {
    cat("\n=== Parallel Processing Setup ===\n")
    cat("Verfügbare Cores:", max_cores, "\n")
    cat("Verwendete Cores:", n_cores, "\n")
  }
  
  # Erstelle Cluster
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  if (verbose) {
    cat("Parallel Backend registriert\n")
    cat("Cluster erstellt mit", n_cores, "Workers\n\n")
  }
  
  return(cl)
}

stop_parallel <- function(cl, verbose = TRUE) {
  if (verbose) {
    cat("\nStoppe Parallel Processing...\n")
  }
  stopCluster(cl)
  registerDoSEQ()  # Zurück zu Sequential Processing
  if (verbose) {
    cat("Cluster gestoppt\n")
  }
}

# ============================================================================
# MOLEKÜL-BILDUNG (López de Prado, Chapter 20)
# ============================================================================

# Lineare Partitionierung: Teile Daten in gleich große "Moleküle"
create_molecules <- function(n_atoms, n_threads) {
  # n_atoms: Anzahl der atomaren Tasks (z.B. Anzahl Zeilen)
  # n_threads: Anzahl paralleler Threads
  
  # Anzahl der Moleküle = min(n_threads, n_atoms)
  n_molecules <- min(n_threads, n_atoms)
  
  # Partitionsgrenzen berechnen
  molecule_size <- floor(n_atoms / n_molecules)
  remainder <- n_atoms %% n_molecules
  
  molecules <- list()
  start_idx <- 1
  
  for (i in 1:n_molecules) {
    # Verteile Remainder gleichmäßig
    extra <- ifelse(i <= remainder, 1, 0)
    end_idx <- start_idx + molecule_size - 1 + extra
    
    molecules[[i]] <- list(
      id = i,
      start = start_idx,
      end = end_idx,
      size = end_idx - start_idx + 1
    )
    
    start_idx <- end_idx + 1
  }
  
  return(molecules)
}

# ============================================================================
# 1. LOG-PREISE (sequentiell, sehr schnell)
# ============================================================================

create_log_prices <- function(df) {
  df <- df %>%
    arrange(date) %>%
    mutate(log_price = log(price))
  return(df)
}

# ============================================================================
# 2. FEATURE ENGINEERING - PARALLEL VERSION
# ============================================================================

# Haupt-Funktion: Feature Engineering mit Parallel Processing
compute_technical_features_parallel <- function(df, 
                                                adx_period = 14,
                                                rsi_period = 14,
                                                bb_period = 20,
                                                bb_sd = 2,
                                                macd_fast = 12,
                                                macd_slow = 26,
                                                macd_signal = 9,
                                                sma_periods = c(5, 10, 20, 50, 200),
                                                ichimoku_periods = c(9, 26, 52),
                                                n_cores = NULL,
                                                use_parallel = TRUE) {
  
  df <- df %>% arrange(date)
  
  if (!use_parallel) {
    # Fallback auf sequentielle Version
    cat("⚠️  Parallel Processing deaktiviert - verwende sequentielle Berechnung\n")
    return(compute_technical_features_sequential(df, adx_period, rsi_period, 
                                                 bb_period, bb_sd, macd_fast, 
                                                 macd_slow, macd_signal, 
                                                 sma_periods, ichimoku_periods))
  }
  
  # Setup Parallel Processing
  cl <- setup_parallel(n_cores = n_cores)
  
  # Export benötigter Objekte an Cluster
  clusterExport(cl, c("df", "adx_period", "rsi_period", "bb_period", "bb_sd",
                      "macd_fast", "macd_slow", "macd_signal", "sma_periods",
                      "ichimoku_periods"), envir = environment())
  
  # Lade benötigte Pakete auf jedem Worker
  clusterEvalQ(cl, {
    library(TTR)
    library(dplyr)
  })
  
  cat("\n=== Parallel Feature Engineering ===\n")
  start_time <- Sys.time()
  
  # Definiere Feature-Gruppen die parallel berechnet werden können
  # Jede Gruppe ist unabhängig und kann auf einem eigenen Core laufen
  
  # --- PARALLELE BERECHNUNG DER FEATURE-GRUPPEN ---
  
  cat("Berechne Feature-Gruppen parallel...\n")
  
  feature_results <- foreach(
    feature_group = 1:7,
    .packages = c("TTR", "dplyr"),
    .combine = 'c'
  ) %dopar% {
    
    result <- list()
    
    if (feature_group == 1) {
      # Gruppe 1: ADX
      adx_data <- ADX(HLC = cbind(df$price, df$price, df$price), n = adx_period)
      result$ADX <- adx_data[, "ADX"]
      result$DI_plus <- adx_data[, "DIp"]
      result$DI_minus <- adx_data[, "DIn"]
      
    } else if (feature_group == 2) {
      # Gruppe 2: RSI
      result$RSI <- RSI(df$log_price, n = rsi_period)
      result$RSI_overbought <- ifelse(result$RSI > 70, 1, 0)
      result$RSI_oversold <- ifelse(result$RSI < 30, 1, 0)
      
    } else if (feature_group == 3) {
      # Gruppe 3: Bollinger Bands
      bb <- BBands(df$log_price, n = bb_period, sd = bb_sd)
      result$BB_upper <- bb[, "up"]
      result$BB_lower <- bb[, "dn"]
      result$BB_mavg <- bb[, "mavg"]
      result$BB_width <- (result$BB_upper - result$BB_lower) / result$BB_mavg
      result$BB_pctB <- bb[, "pctB"]
      
    } else if (feature_group == 4) {
      # Gruppe 4: MACD
      macd_data <- MACD(df$log_price, 
                        nFast = macd_fast, 
                        nSlow = macd_slow, 
                        nSig = macd_signal)
      result$MACD <- macd_data[, "macd"]
      result$MACD_signal <- macd_data[, "signal"]
      result$MACD_histogram <- result$MACD - result$MACD_signal
      result$MACD_bullish <- ifelse(result$MACD > result$MACD_signal & 
                                      dplyr::lag(result$MACD) <= dplyr::lag(result$MACD_signal), 1, 0)
      result$MACD_bearish <- ifelse(result$MACD < result$MACD_signal & 
                                      dplyr::lag(result$MACD) >= dplyr::lag(result$MACD_signal), 1, 0)
      
    } else if (feature_group == 5) {
      # Gruppe 5: SMAs (parallel über verschiedene Perioden)
      for (period in sma_periods) {
        col_name <- paste0("SMA_", period)
        result[[col_name]] <- SMA(df$log_price, n = period)
        result[[paste0("price_to_SMA_", period)]] <- 
          (df$log_price - result[[col_name]]) / result[[col_name]]
      }
      
      # SMA Cross-over Features
      if ("SMA_50" %in% names(result) && "SMA_200" %in% names(result)) {
        result$SMA_cross_50_200 <- result$SMA_50 - result$SMA_200
        result$SMA_golden_cross <- ifelse(result$SMA_50 > result$SMA_200 & 
                                            dplyr::lag(result$SMA_50) <= dplyr::lag(result$SMA_200), 1, 0)
        result$SMA_death_cross <- ifelse(result$SMA_50 < result$SMA_200 & 
                                           dplyr::lag(result$SMA_50) >= dplyr::lag(result$SMA_200), 1, 0)
      }
      
    } else if (feature_group == 6) {
      # Gruppe 6: Ichimoku
      tenkan_period <- ichimoku_periods[1]
      kijun_period <- ichimoku_periods[2]
      senkou_period <- ichimoku_periods[3]
      
      result$ichimoku_tenkan <- (runMax(df$price, tenkan_period) + 
                                   runMin(df$price, tenkan_period)) / 2
      result$ichimoku_kijun <- (runMax(df$price, kijun_period) + 
                                  runMin(df$price, kijun_period)) / 2
      result$ichimoku_spanA <- (result$ichimoku_tenkan + result$ichimoku_kijun) / 2
      result$ichimoku_spanB <- (runMax(df$price, senkou_period) + 
                                  runMin(df$price, senkou_period)) / 2
      result$ichimoku_chikou <- dplyr::lag(df$price, kijun_period)
      
      result$ichimoku_cloud_top <- pmax(result$ichimoku_spanA, result$ichimoku_spanB)
      result$ichimoku_cloud_bottom <- pmin(result$ichimoku_spanA, result$ichimoku_spanB)
      result$price_above_cloud <- ifelse(df$price > result$ichimoku_cloud_top, 1, 0)
      result$price_below_cloud <- ifelse(df$price < result$ichimoku_cloud_bottom, 1, 0)
      
      result$ichimoku_TK_cross_bull <- ifelse(
        result$ichimoku_tenkan > result$ichimoku_kijun & 
          dplyr::lag(result$ichimoku_tenkan) <= dplyr::lag(result$ichimoku_kijun), 1, 0)
      result$ichimoku_TK_cross_bear <- ifelse(
        result$ichimoku_tenkan < result$ichimoku_kijun & 
          dplyr::lag(result$ichimoku_tenkan) >= dplyr::lag(result$ichimoku_kijun), 1, 0)
      
    } else if (feature_group == 7) {
      # Gruppe 7: Zusätzliche Features (Momentum, Volatilität)
      for (period in c(1, 3, 5, 10, 21, 63)) {
        result[[paste0("return_", period, "d")]] <- 
          (df$log_price - dplyr::lag(df$log_price, period)) / period
      }
      
      result$volatility_21d <- runSD(df$log_price, n = 21)
      result$volatility_63d <- runSD(df$log_price, n = 63)
      
      result$ATR_14 <- ATR(cbind(df$price, df$price, df$price), n = 14)[, "atr"]
      result$NATR_14 <- result$ATR_14 / df$price * 100
    }
    
    return(result)
  }
  
  # Kombiniere alle Feature-Gruppen
  cat("Kombiniere Feature-Gruppen...\n")
  all_features <- do.call(c, feature_results)
  
  # Füge Features zum Original-DataFrame hinzu
  for (feature_name in names(all_features)) {
    df[[feature_name]] <- all_features[[feature_name]]
  }
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("✓ Feature Engineering abgeschlossen in", round(elapsed, 2), "Sekunden\n")
  cat("Anzahl Features erstellt:", length(all_features), "\n")
  
  # Stoppe Parallel Processing
  stop_parallel(cl, verbose = FALSE)
  
  return(df)
}

# Sequentielle Fallback-Version (identisch zum Original)
compute_technical_features_sequential <- function(df, adx_period, rsi_period, 
                                                  bb_period, bb_sd, macd_fast, 
                                                  macd_slow, macd_signal, 
                                                  sma_periods, ichimoku_periods) {
  
  df <- df %>% arrange(date)
  
  cat("Berechne ADX...\n")
  adx_data <- ADX(HLC = cbind(df$price, df$price, df$price), n = adx_period)
  df$ADX <- adx_data[, "ADX"]
  df$DI_plus <- adx_data[, "DIp"]
  df$DI_minus <- adx_data[, "DIn"]
  
  cat("Berechne RSI...\n")
  df$RSI <- RSI(df$log_price, n = rsi_period)
  df$RSI_overbought <- ifelse(df$RSI > 70, 1, 0)
  df$RSI_oversold <- ifelse(df$RSI < 30, 1, 0)
  
  cat("Berechne Bollinger Bands...\n")
  bb <- BBands(df$log_price, n = bb_period, sd = bb_sd)
  df$BB_upper <- bb[, "up"]
  df$BB_lower <- bb[, "dn"]
  df$BB_mavg <- bb[, "mavg"]
  df$BB_width <- (df$BB_upper - df$BB_lower) / df$BB_mavg
  df$BB_pctB <- bb[, "pctB"]
  
  cat("Berechne MACD...\n")
  macd_data <- MACD(df$log_price, nFast = macd_fast, nSlow = macd_slow, nSig = macd_signal)
  df$MACD <- macd_data[, "macd"]
  df$MACD_signal <- macd_data[, "signal"]
  df$MACD_histogram <- df$MACD - df$MACD_signal
  df$MACD_bullish <- ifelse(df$MACD > df$MACD_signal & 
                              lag(df$MACD) <= lag(df$MACD_signal), 1, 0)
  df$MACD_bearish <- ifelse(df$MACD < df$MACD_signal & 
                              lag(df$MACD) >= lag(df$MACD_signal), 1, 0)
  
  cat("Berechne SMAs...\n")
  for (period in sma_periods) {
    col_name <- paste0("SMA_", period)
    df[[col_name]] <- SMA(df$log_price, n = period)
    df[[paste0("price_to_SMA_", period)]] <- 
      (df$log_price - df[[col_name]]) / df[[col_name]]
  }
  
  df$SMA_cross_50_200 <- df$SMA_50 - df$SMA_200
  df$SMA_golden_cross <- ifelse(df$SMA_50 > df$SMA_200 & 
                                  lag(df$SMA_50) <= lag(df$SMA_200), 1, 0)
  df$SMA_death_cross <- ifelse(df$SMA_50 < df$SMA_200 & 
                                 lag(df$SMA_50) >= lag(df$SMA_200), 1, 0)
  
  cat("Berechne Ichimoku...\n")
  tenkan_period <- ichimoku_periods[1]
  kijun_period <- ichimoku_periods[2]
  senkou_period <- ichimoku_periods[3]
  
  df$ichimoku_tenkan <- (runMax(df$price, tenkan_period) + 
                           runMin(df$price, tenkan_period)) / 2
  df$ichimoku_kijun <- (runMax(df$price, kijun_period) + 
                          runMin(df$price, kijun_period)) / 2
  df$ichimoku_spanA <- (df$ichimoku_tenkan + df$ichimoku_kijun) / 2
  df$ichimoku_spanB <- (runMax(df$price, senkou_period) + 
                          runMin(df$price, senkou_period)) / 2
  df$ichimoku_chikou <- lag(df$price, kijun_period)
  
  df$ichimoku_cloud_top <- pmax(df$ichimoku_spanA, df$ichimoku_spanB)
  df$ichimoku_cloud_bottom <- pmin(df$ichimoku_spanA, df$ichimoku_spanB)
  df$price_above_cloud <- ifelse(df$price > df$ichimoku_cloud_top, 1, 0)
  df$price_below_cloud <- ifelse(df$price < df$ichimoku_cloud_bottom, 1, 0)
  
  df$ichimoku_TK_cross_bull <- ifelse(df$ichimoku_tenkan > df$ichimoku_kijun & 
                                        lag(df$ichimoku_tenkan) <= lag(df$ichimoku_kijun), 1, 0)
  df$ichimoku_TK_cross_bear <- ifelse(df$ichimoku_tenkan < df$ichimoku_kijun & 
                                        lag(df$ichimoku_tenkan) >= lag(df$ichimoku_kijun), 1, 0)
  
  cat("Berechne Momentum Features...\n")
  for (period in c(1, 3, 5, 10, 21, 63)) {
    df[[paste0("return_", period, "d")]] <- 
      (df$log_price - lag(df$log_price, period)) / period
  }
  
  df$volatility_21d <- runSD(df$log_price, n = 21)
  df$volatility_63d <- runSD(df$log_price, n = 63)
  
  df$ATR_14 <- ATR(cbind(df$price, df$price, df$price), n = 14)[, "atr"]
  df$NATR_14 <- df$ATR_14 / df$price * 100
  
  return(df)
}

# ============================================================================
# 3. SAMPLE WEIGHTS - PARALLEL VERSION
# ============================================================================

# Berechne Sample Weights parallel für große Datasets
compute_sample_weights_parallel <- function(df, time_decay = 1.0, 
                                            n_cores = NULL,
                                            use_parallel = TRUE) {
  
  n <- nrow(df)
  
  # Für kleine Datasets ist parallel nicht nötig
  if (n < 1000 || !use_parallel) {
    return(compute_sample_weights_sequential(df, time_decay))
  }
  
  cat("\n=== Parallel Sample Weights Berechnung ===\n")
  start_time <- Sys.time()
  
  cl <- setup_parallel(n_cores = n_cores, verbose = FALSE)
  
  # Erstelle Moleküle für Parallelisierung
  n_threads <- getDoParWorkers()
  molecules <- create_molecules(n, n_threads)
  
  cat("Berechne Overlaps für", n, "Beobachtungen mit", 
      length(molecules), "Molekülen...\n")
  
  # Export Daten
  clusterExport(cl, c("df"), envir = environment())
  
  # Parallele Berechnung der Overlaps
  weights <- foreach(
    mol = molecules,
    .combine = 'c',
    .packages = c("dplyr")
  ) %dopar% {
    
    mol_weights <- numeric(mol$size)
    
    for (idx in 1:mol$size) {
      i <- mol$start + idx - 1
      
      if (!is.na(df$t1[i])) {
        overlaps <- sum(df$date < df$t1[i] & df$t1 > df$date[i], na.rm = TRUE)
        mol_weights[idx] <- 1.0 / (overlaps + 1)
      } else {
        mol_weights[idx] <- 1.0
      }
    }
    
    return(mol_weights)
  }
  
  df$sample_weight <- weights
  
  # Time-decay
  if (time_decay > 0) {
    time_weights <- exp(-time_decay * (max(df$date) - df$date) / 
                          as.numeric(max(df$date) - min(df$date)))
    df$sample_weight <- df$sample_weight * time_weights
  }
  
  # Normalisiere
  df$sample_weight <- df$sample_weight / sum(df$sample_weight) * nrow(df)
  
  stop_parallel(cl, verbose = FALSE)
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat("✓ Sample Weights berechnet in", round(elapsed, 2), "Sekunden\n")
  
  return(df)
}

# Sequentielle Version (wie Original)
compute_sample_weights_sequential <- function(df, time_decay = 1.0) {
  df <- df %>% arrange(date)
  df$sample_weight <- 1.0
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$t1[i])) {
      overlaps <- sum(df$date < df$t1[i] & df$t1 > df$date[i], na.rm = TRUE)
      df$sample_weight[i] <- 1.0 / (overlaps + 1)
    }
  }
  
  if (time_decay > 0) {
    time_weights <- exp(-time_decay * (max(df$date) - df$date) / 
                          as.numeric(max(df$date) - min(df$date)))
    df$sample_weight <- df$sample_weight * time_weights
  }
  
  df$sample_weight <- df$sample_weight / sum(df$sample_weight) * nrow(df)
  
  return(df)
}

# ============================================================================
# 4. PURGED K-FOLD CV (unverändert - bereits effizient)
# ============================================================================

create_purged_folds <- function(df, n_folds = 5, embargo_pct = 0.01) {
  df <- df %>% arrange(date)
  n <- nrow(df)
  fold_size <- floor(n / n_folds)
  folds <- list()
  
  for (k in 1:n_folds) {
    test_start <- (k - 1) * fold_size + 1
    test_end <- ifelse(k < n_folds, k * fold_size, n)
    test_idx <- test_start:test_end
    
    train_idx <- setdiff(1:n, test_idx)
    
    # PURGING
    purge_idx <- c()
    for (i in train_idx) {
      if (!is.na(df$t1[i])) {
        if (df$t1[i] >= df$date[test_start] && df$t1[i] <= df$date[test_end]) {
          purge_idx <- c(purge_idx, i)
        }
      }
    }
    train_idx <- setdiff(train_idx, purge_idx)
    
    # EMBARGOING
    embargo_size <- floor(n * embargo_pct)
    if (test_end + embargo_size <= n) {
      embargo_idx <- (test_end + 1):(test_end + embargo_size)
      train_idx <- setdiff(train_idx, embargo_idx)
    } else {
      embargo_idx <- c()
    }
    
    folds[[k]] <- list(
      train = train_idx,
      test = test_idx,
      purged = purge_idx,
      embargoed = embargo_idx
    )
  }
  
  return(folds)
}

# ============================================================================
# 5. XGBOOST MIT PARALLEL PROCESSING
# ============================================================================

train_xgboost_model_parallel <- function(df, features, target = "label",
                                         n_folds = 5,
                                         early_stopping_rounds = 50,
                                         max_depth = 6,
                                         eta = 0.01,
                                         min_child_weight = 1,
                                         subsample = 0.8,
                                         colsample_bytree = 0.8,
                                         gamma = 0,
                                         nrounds = 1000,
                                         n_cores = NULL) {
  
  cat("\n=== Training XGBoost mit Parallel Processing ===\n")
  
  # Entferne NAs
  df_clean <- df %>%
    select(all_of(c("date", "t1", "sample_weight", features, target))) %>%
    drop_na()
  
  cat("Anzahl Beobachtungen:", nrow(df_clean), "\n")
  
  # Erstelle Purged Folds
  folds <- create_purged_folds(df_clean, n_folds = n_folds)
  
  # Setup Parallel für CV Folds
  cl <- setup_parallel(n_cores = n_cores, verbose = FALSE)
  cat("\nTrainiere", n_folds, "Folds parallel...\n\n")
  
  # Export benötigter Objekte
  clusterExport(cl, c("df_clean", "folds", "features", "target",
                      "max_depth", "eta", "min_child_weight", "subsample",
                      "colsample_bytree", "gamma", "nrounds",
                      "early_stopping_rounds"), 
                envir = environment())
  
  # Lade Pakete auf Workers
  clusterEvalQ(cl, {
    library(xgboost)
    library(dplyr)
  })
  
  # PARALLEL: Trainiere alle Folds gleichzeitig
  cv_results <- foreach(
    k = 1:n_folds,
    .packages = c("xgboost", "dplyr")
  ) %dopar% {
    
    # Train/Test Split
    train_data <- df_clean[folds[[k]]$train, ]
    test_data <- df_clean[folds[[k]]$test, ]
    
    # DMatrix erstellen
    dtrain <- xgb.DMatrix(
      data = as.matrix(train_data[, features]),
      label = train_data[[target]],
      weight = train_data$sample_weight
    )
    
    dtest <- xgb.DMatrix(
      data = as.matrix(test_data[, features]),
      label = test_data[[target]],
      weight = test_data$sample_weight
    )
    
    watchlist <- list(train = dtrain, test = dtest)
    
    # XGBoost Parameter
    params <- list(
      objective = "multi:softprob",
      num_class = 3,
      max_depth = max_depth,
      eta = eta,
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      gamma = gamma,
      eval_metric = "mlogloss",
      nthread = 1  # Jeder Fold läuft auf 1 Thread (CV ist bereits parallel)
    )
    
    # Training
    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      watchlist = watchlist,
      early_stopping_rounds = early_stopping_rounds,
      verbose = 0
    )
    
    # Predictions
    preds <- predict(model, dtest, reshape = TRUE)
    pred_labels <- max.col(preds) - 2
    
    # Metriken
    accuracy <- mean(pred_labels == test_data[[target]])
    conf_mat <- table(Predicted = pred_labels, Actual = test_data[[target]])
    
    # Feature Importance
    importance <- xgb.importance(model = model, feature_names = features)
    
    # Return Results
    list(
      model = model,
      predictions = pred_labels,
      actual = test_data[[target]],
      accuracy = accuracy,
      confusion_matrix = conf_mat,
      feature_importance = importance,
      test_indices = folds[[k]]$test,
      best_iteration = model$best_iteration,
      fold = k
    )
  }
  
  stop_parallel(cl, verbose = FALSE)
  
  # Aggregiere Ergebnisse
  cat("\n=== Cross-Validation Ergebnisse ===\n")
  for (k in 1:n_folds) {
    cat("Fold", k, "- Accuracy:", round(cv_results[[k]]$accuracy, 4), 
        "| Best Iteration:", cv_results[[k]]$best_iteration, "\n")
  }
  
  avg_accuracy <- mean(sapply(cv_results, function(x) x$accuracy))
  cat("\nDurchschnittliche Accuracy:", round(avg_accuracy, 4), "\n")
  
  # Feature Importance aggregiert
  all_importance <- bind_rows(
    lapply(cv_results, function(x) x$feature_importance),
    .id = "fold"
  )
  
  avg_importance <- all_importance %>%
    group_by(Feature) %>%
    summarise(
      Gain = mean(Gain),
      Cover = mean(Cover),
      Frequency = mean(Frequency)
    ) %>%
    arrange(desc(Gain))
  
  cat("\n=== Top 10 Features ===\n")
  print(head(avg_importance, 10))
  
  # Finales Modell auf allen Daten
  cat("\n=== Training Final Model ===\n")
  best_nrounds <- median(sapply(cv_results, function(x) x$best_iteration))
  
  dtrain_full <- xgb.DMatrix(
    data = as.matrix(df_clean[, features]),
    label = df_clean[[target]],
    weight = df_clean$sample_weight
  )
  
  # XGBoost mit Multi-Threading für finales Modell
  if (is.null(n_cores)) {
    n_threads <- parallel::detectCores() - 1
  } else {
    n_threads <- n_cores
  }
  
  params_final <- list(
    objective = "multi:softprob",
    num_class = 3,
    max_depth = max_depth,
    eta = eta,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    gamma = gamma,
    eval_metric = "mlogloss",
    nthread = n_threads  # Finales Modell nutzt alle Threads
  )
  
  final_model <- xgb.train(
    params = params_final,
    data = dtrain_full,
    nrounds = best_nrounds,
    verbose = 1
  )
  
  return(list(
    final_model = final_model,
    cv_results = cv_results,
    feature_importance = avg_importance,
    avg_accuracy = avg_accuracy,
    folds = folds
  ))
}

# ============================================================================
# 6. HAUPT-PIPELINE MIT PARALLEL PROCESSING
# ============================================================================

run_full_pipeline_parallel <- function(df_labelled, 
                                       output_dir = "output",
                                       n_cores = NULL,
                                       use_parallel = TRUE) {
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  FEATURE ENGINEERING & XGBOOST - PARALLEL VERSION          ║\n")
  cat("║  Basierend auf López de Prado (2018, Chapter 20)          ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  
  # Zeige System-Informationen
  cat("=== System Informationen ===\n")
  cat("Verfügbare Cores:", parallel::detectCores(), "\n")
  cat("Parallel Processing:", ifelse(use_parallel, "✓ AKTIVIERT", "✗ DEAKTIVIERT"), "\n")
  if (use_parallel) {
    cores_to_use <- ifelse(is.null(n_cores), 
                           parallel::detectCores() - 1, 
                           n_cores)
    cat("Cores für Berechnung:", cores_to_use, "\n")
  }
  cat("\n")
  
  dir.create(output_dir, showWarnings = FALSE)
  
  total_start <- Sys.time()
  
  # Schritt 1: Log-Preise
  cat("Schritt 1/5: Berechne Log-Preise...\n")
  df <- create_log_prices(df_labelled)
  
  # Schritt 2: Feature Engineering (PARALLEL)
  cat("\nSchritt 2/5: Feature Engineering (Parallel)...\n")
  df_features <- compute_technical_features_parallel(
    df,
    n_cores = n_cores,
    use_parallel = use_parallel
  )
  
  # Schritt 3: Sample Weights (PARALLEL)
  cat("\nSchritt 3/5: Sample Weights (Parallel)...\n")
  df_features <- compute_sample_weights_parallel(
    df_features,
    time_decay = 1.0,
    n_cores = n_cores,
    use_parallel = use_parallel
  )
  
  # Schritt 4: Feature Selection
  cat("\nSchritt 4/5: Feature Selection...\n")
  exclude_cols <- c("date", "price", "label", "t1", "return", "sample_weight",
                    "high", "low", "close", "open", "volume", "log_price")
  all_cols <- colnames(df_features)
  feature_cols <- setdiff(all_cols, exclude_cols)
  feature_cols <- feature_cols[sapply(df_features[feature_cols], is.numeric)]
  
  cat("Anzahl Features:", length(feature_cols), "\n")
  
  # Schritt 5: XGBoost Training (PARALLEL CV)
  cat("\nSchritt 5/5: XGBoost Training (Parallel CV)...\n")
  model_results <- train_xgboost_model_parallel(
    df = df_features,
    features = feature_cols,
    target = "label",
    n_folds = 5,
    n_cores = n_cores
  )
  
  # Speichere Ergebnisse
  cat("\n=== Speichere Ergebnisse ===\n")
  write.csv(model_results$feature_importance,
            file.path(output_dir, "feature_importance.csv"),
            row.names = FALSE)
  
  xgb.save(model_results$final_model,
           file.path(output_dir, "xgboost_model.bin"))
  
  saveRDS(df_features, file.path(output_dir, "df_with_features.rds"))
  
  total_end <- Sys.time()
  total_elapsed <- as.numeric(difftime(total_end, total_start, units = "mins"))
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  PIPELINE ERFOLGREICH ABGESCHLOSSEN!                       ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("⏱️  Gesamtzeit:", round(total_elapsed, 2), "Minuten\n")
  cat("📊 CV Accuracy:", round(model_results$avg_accuracy, 4), "\n")
  cat("📁 Ergebnisse in:", output_dir, "\n\n")
  
  return(list(
    df_features = df_features,
    model_results = model_results
  ))
}

# ============================================================================
# 7. BENCHMARK: PARALLEL VS SEQUENTIAL
# ============================================================================

benchmark_parallel_vs_sequential <- function(df_labelled, n_cores = NULL) {
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  BENCHMARK: PARALLEL vs SEQUENTIAL                         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  
  # Test mit Subset der Daten für schnelleren Benchmark
  df_test <- head(df_labelled, min(5000, nrow(df_labelled)))
  df_test <- create_log_prices(df_test)
  
  # Sequential
  cat("=== SEQUENTIAL TEST ===\n")
  start_seq <- Sys.time()
  df_seq <- compute_technical_features_parallel(
    df_test,
    use_parallel = FALSE
  )
  end_seq <- Sys.time()
  time_seq <- as.numeric(difftime(end_seq, start_seq, units = "secs"))
  
  # Parallel
  cat("\n=== PARALLEL TEST ===\n")
  start_par <- Sys.time()
  df_par <- compute_technical_features_parallel(
    df_test,
    n_cores = n_cores,
    use_parallel = TRUE
  )
  end_par <- Sys.time()
  time_par <- as.numeric(difftime(end_par, start_par, units = "secs"))
  
  # Ergebnisse
  speedup <- time_seq / time_par
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  BENCHMARK ERGEBNISSE                                      ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("📊 Anzahl Beobachtungen:", nrow(df_test), "\n")
  cat("⏱️  Sequential Zeit:", round(time_seq, 2), "Sekunden\n")
  cat("⏱️  Parallel Zeit:", round(time_par, 2), "Sekunden\n")
  cat("🚀 Speedup:", round(speedup, 2), "x schneller\n")
  cat("💡 Effizienz:", round((speedup / (n_cores %||% (parallel::detectCores()-1))) * 100, 1), "%\n")
  cat("\n")
  
  return(list(
    time_sequential = time_seq,
    time_parallel = time_par,
    speedup = speedup,
    n_obs = nrow(df_test),
    n_cores = n_cores %||% (parallel::detectCores()-1)
  ))
}

# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# INFO MESSAGE
# ============================================================================

cat("\n✅ Feature Engineering mit PARALLEL PROCESSING geladen!\n")
cat("📚 Basiert auf López de Prado (2018, Chapter 20: Multiprocessing)\n\n")
cat("🚀 Haupt-Funktionen:\n")
cat("   • run_full_pipeline_parallel()  - Komplette Pipeline (parallel)\n")
cat("   • benchmark_parallel_vs_sequential() - Geschwindigkeitstest\n\n")
cat("💡 Verwendung:\n")
cat("   results <- run_full_pipeline_parallel(df_labelled, n_cores = 4)\n")
cat("   benchmark <- benchmark_parallel_vs_sequential(df_labelled)\n\n")