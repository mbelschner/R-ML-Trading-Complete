# ============================================================================
# PARALLEL FEATURE ENGINEERING & XGBOOST - STANDALONE SCRIPT
# Basierend auf: López de Prado (2018, Chapter 20) und Jansen (2020)
# ============================================================================
# 
# VERWENDUNG:
# 1. Passen Sie die Pfade unten an (INPUT_FILE und OUTPUT_DIR)
# 2. Führen Sie das gesamte Script aus: source("parallel_feature_engineering.R")
# 3. Ergebnisse finden Sie im OUTPUT_DIR
#
# timeN-ANFORDERUNGEN:
# - time (POSIXct)
# - log_open, log_high, log_close (numeric)
# - tb_label (integer: -1, 0, 1)
# - tb_exit_time (POSIXct) - Zeitpunkt des ersten Barrier Touch
# ============================================================================

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  PARALLEL FEATURE ENGINEERING & XGBOOST TRAINING           ║\n")
cat("║  Standalone Script Version                                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# KONFIGURATION
# ============================================================================

rm(list=ls())
gc()

EPIC = "GOLD"
INTERVAL = "MINUTE_15"

working_directory <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")
INPUT_FILE <- read.csv(file.path(output_path, paste0("triplebarrier_labelled_", EPIC, "_", INTERVAL, ".csv")))

# Parallel Processing
N_CORES <- NULL  # NULL = automatisch alle Cores - 1, oder Zahl (z.B. 4)

# Feature Engineering Parameter
ADX_PERIOD <- 14
RSI_PERIOD <- 14
BB_PERIOD <- 20
BB_SD <- 2
MACD_FAST <- 12
MACD_SLOW <- 26
MACD_SIGNAL <- 9
SMA_PERIODS <- c(5, 10, 20, 50, 200)
ICHIMOKU_PERIODS <- c(9, 26, 52)

# Sample Weights
TIME_DECAY <- 1.0  # Time-decay factor für Sample Weights

# Cross-Validation
N_FOLDS <- 5
EMBARGO_PCT <- 0.01  # 1% Embargo nach jedem Test-Set

# XGBoost Parameter (Overfitting-Kontrolle)
MAX_DEPTH <- 6
ETA <- 0.01
MIN_CHILD_WEIGHT <- 1
SUBSAMPLE <- 0.8
COLSAMPLE_BYTREE <- 0.8
GAMMA <- 0
NROUNDS <- 1000
EARLY_STOPPING_ROUNDS <- 50

# ============================================================================
# PAKETE LADEN
# ============================================================================

cat("Lade benötigte Pakete...\n")
suppressPackageStartupMessages({
  library(tidyverse)
  library(TTR)
  library(xgboost)
  library(lubridate)
  library(data.table)
  library(foreach)
  library(doParallel)
  library(parallel)
})
cat("✓ Alle Pakete geladen\n\n")

# ============================================================================
# timeN LADEN
# ============================================================================

# timen einlesen
df = INPUT_FILE

# Konvertiere time zu POSIXct falls nötig
if (!inherits(df$time, "POSIXct")) {
  df$time <- as.POSIXct(df$time)
}

# Konvertiere tb_exit_time zu POSIXct falls vorhanden
if ("tb_exit_time" %in% colnames(df) && !inherits(df$tb_exit_time, "POSIXct")) {
  df$tb_exit_time <- as.POSIXct(df$tb_exit_time)
}

# Sortiere nach Datum
df <- df %>% arrange(time)

cat("✓ timen geladen:", nrow(df), "Beobachtungen\n")
cat("  Zeitraum:", as.character(min(df$time)), "bis", as.character(max(df$time)), "\n")
cat("  Label-Verteilung:\n")
print(table(df$tb_label))
cat("\n")

# Erstelle Output-Verzeichnis
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# PARALLEL PROCESSING SETUP
# ============================================================================

cat("=== Parallel Processing Setup ===\n")

# Core-Detection
if (is.null(N_CORES)) {
  N_CORES <- parallel::detectCores() - 1
}
max_cores <- parallel::detectCores()
N_CORES <- min(N_CORES, max_cores)

cat("Verfügbare Cores:", max_cores, "\n")
cat("Verwendete Cores:", N_CORES, "\n")

# Erstelle Cluster
cl <- makeCluster(N_CORES)
registerDoParallel(cl)

cat("✓ Parallel Backend registriert mit", N_CORES, "Workers\n\n")

# Cleanup-Funktion für später
cleanup_parallel <- function() {
  cat("\nStoppe Parallel Processing...\n")
  stopCluster(cl)
  registerDoSEQ()
  cat("✓ Cluster gestoppt\n")
}

# ============================================================================
# SCHRITT 1: FEATURE ENGINEERING (PARALLEL)
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  SCHRITT 1/5: FEATURE ENGINEERING (PARALLEL)               ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

start_time_features <- Sys.time()

# Verwende den mittleren Log-Preis für einfache Indikatoren
df$log_price <- (df$log_close + df$log_open) / 2

# Export timen an Cluster
clusterExport(cl, c("df", "ADX_PERIOD", "RSI_PERIOD", "BB_PERIOD", "BB_SD",
                    "MACD_FAST", "MACD_SLOW", "MACD_SIGNAL", "SMA_PERIODS",
                    "ICHIMOKU_PERIODS"), envir = environment())

# Lade Pakete auf Workern
clusterEvalQ(cl, {
  library(TTR)
  library(dplyr)
})

cat("Berechne technische Indikatoren parallel...\n")

# Parallele Berechnung der Feature-Gruppen
# .combine = 'cbind' macht direkt einen DataFrame mit Features als Spalten
feature_results <- foreach(
  feature_group = 1:7,
  .packages = c("TTR", "dplyr"),
  .combine = 'cbind'
) %dopar% {
  
  result <- list()
  
  if (feature_group == 1) {
    # Gruppe 1: ADX
    # Verwende OHLC für ADX (close, close, close als Proxy wenn H/L fehlt)
    price_matrix <- cbind(exp(df$log_high), exp(df$log_low), exp(df$log_close))
    adx_data <- ADX(HLC = price_matrix, n = ADX_PERIOD)
    result$ADX <- adx_data[, "ADX"]
    result$DI_plus <- adx_data[, "DIp"]
    result$DI_minus <- adx_data[, "DIn"]
    
  } else if (feature_group == 2) {
    # Gruppe 2: RSI
    result$RSI <- RSI(df$log_price, n = RSI_PERIOD)
    result$RSI_overbought <- ifelse(result$RSI > 70, 1, 0)
    result$RSI_oversold <- ifelse(result$RSI < 30, 1, 0)
    
  } else if (feature_group == 3) {
    # Gruppe 3: Bollinger Bands
    bb <- BBands(df$log_price, n = BB_PERIOD, sd = BB_SD)
    result$BB_upper <- bb[, "up"]
    result$BB_lower <- bb[, "dn"]
    result$BB_mavg <- bb[, "mavg"]
    result$BB_width <- (result$BB_upper - result$BB_lower) / result$BB_mavg
    result$BB_pctB <- bb[, "pctB"]
    
  } else if (feature_group == 4) {
    # Gruppe 4: MACD
    macd_data <- MACD(df$log_price, 
                      nFast = MACD_FAST, 
                      nSlow = MACD_SLOW, 
                      nSig = MACD_SIGNAL)
    result$MACD <- macd_data[, "macd"]
    result$MACD_signal <- macd_data[, "signal"]
    result$MACD_histogram <- result$MACD - result$MACD_signal
    result$MACD_bullish <- ifelse(result$MACD > result$MACD_signal & 
                                    dplyr::lag(result$MACD) <= dplyr::lag(result$MACD_signal), 1, 0)
    result$MACD_bearish <- ifelse(result$MACD < result$MACD_signal & 
                                    dplyr::lag(result$MACD) >= dplyr::lag(result$MACD_signal), 1, 0)
    
  } else if (feature_group == 5) {
    # Gruppe 5: SMAs
    for (period in SMA_PERIODS) {
      col_name <- paste0("SMA_", period)
      result[[col_name]] <- SMA(df$log_price, n = period)
      result[[paste0("price_to_SMA_", period)]] <- 
        (df$log_price - result[[col_name]]) / result[[col_name]]
    }
    
    # SMA Cross-over Features
    if (50 %in% SMA_PERIODS && 200 %in% SMA_PERIODS) {
      sma_50 <- SMA(df$log_price, n = 50)
      sma_200 <- SMA(df$log_price, n = 200)
      result$SMA_cross_50_200 <- sma_50 - sma_200
      result$SMA_golden_cross <- ifelse(sma_50 > sma_200 & 
                                          dplyr::lag(sma_50) <= dplyr::lag(sma_200), 1, 0)
      result$SMA_death_cross <- ifelse(sma_50 < sma_200 & 
                                         dplyr::lag(sma_50) >= dplyr::lag(sma_200), 1, 0)
    }
    
  } else if (feature_group == 6) {
    # Gruppe 6: Ichimoku
    tenkan_period <- ICHIMOKU_PERIODS[1]
    kijun_period <- ICHIMOKU_PERIODS[2]
    senkou_period <- ICHIMOKU_PERIODS[3]
    
    price <- exp(df$log_close)
    
    result$ichimoku_tenkan <- (runMax(price, tenkan_period) + 
                                 runMin(price, tenkan_period)) / 2
    result$ichimoku_kijun <- (runMax(price, kijun_period) + 
                                runMin(price, kijun_period)) / 2
    result$ichimoku_spanA <- (result$ichimoku_tenkan + result$ichimoku_kijun) / 2
    result$ichimoku_spanB <- (runMax(price, senkou_period) + 
                                runMin(price, senkou_period)) / 2
    result$ichimoku_chikou <- dplyr::lag(price, kijun_period)
    
    result$ichimoku_cloud_top <- pmax(result$ichimoku_spanA, result$ichimoku_spanB)
    result$ichimoku_cloud_bottom <- pmin(result$ichimoku_spanA, result$ichimoku_spanB)
    result$price_above_cloud <- ifelse(price > result$ichimoku_cloud_top, 1, 0)
    result$price_below_cloud <- ifelse(price < result$ichimoku_cloud_bottom, 1, 0)
    
    result$ichimoku_TK_cross_bull <- ifelse(
      result$ichimoku_tenkan > result$ichimoku_kijun & 
        dplyr::lag(result$ichimoku_tenkan) <= dplyr::lag(result$ichimoku_kijun), 1, 0)
    result$ichimoku_TK_cross_bear <- ifelse(
      result$ichimoku_tenkan < result$ichimoku_kijun & 
        dplyr::lag(result$ichimoku_tenkan) >= dplyr::lag(result$ichimoku_kijun), 1, 0)
    
  } else if (feature_group == 7) {
    # Gruppe 7: Momentum & Volatilität
    for (period in c(1, 3, 5, 10, 21, 63)) {
      result[[paste0("return_", period, "d")]] <- 
        (df$log_price - dplyr::lag(df$log_price, period)) / period
    }
    
    result$volatility_21d <- runSD(df$log_price, n = 21)
    result$volatility_63d <- runSD(df$log_price, n = 63)
    
    # ATR mit High/Low/Close
    price_matrix <- cbind(exp(df$log_high), exp(df$log_low), exp(df$log_close))
    result$ATR_14 <- ATR(price_matrix, n = 14)[, "atr"]
    result$NATR_14 <- result$ATR_14 / exp(df$log_close) * 100
  }
  
  # Return als data.frame statt list
  return(as.data.frame(result))
}

# Kombiniere Features mit Original-DataFrame
cat("Kombiniere Features...\n")
df <- cbind(df, feature_results)

end_time_features <- Sys.time()
elapsed_features <- as.numeric(difftime(end_time_features, start_time_features, units = "secs"))

cat("✓ Feature Engineering abgeschlossen in", round(elapsed_features, 2), "Sekunden\n")
cat("  Anzahl Features erstellt:", ncol(feature_results), "\n\n")

# ============================================================================
# SCHRITT 2: SAMPLE WEIGHTS (PARALLEL)
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  SCHRITT 2/5: SAMPLE WEIGHTS (PARALLEL)                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")
start_time_weights <- Sys.time()

# Prüfe ob tb_exit_time Spalte existiert
if (!"tb_exit_time1" %in% colnames(df)) {
  cat("⚠️  Warnung: Keine 'tb_exit_time' Spalte gefunden - verwende uniform Weights\n")
  df$sample_weight <- 1.0
} else {
  
  n <- nrow(df)
  cat("Berechne Sample Weights für", n, "Beobachtungen...\n")
  
  library(dplyr)
  
  df <- df %>%
    mutate(
      # Berechne Overlaps für jede Zeile
      overlap_count = map_int(row_number(), ~{
        if (is.na(tb_exit_time[.x])) return(0L)
        sum(time < tb_exit_time[.x] & tb_exit_time > time[.x], na.rm = TRUE)
      }),
      # Berechne Sample Weight
      sample_weight = if_else(!is.na(tb_exit_time), 
                              1.0 / (overlap_count + 1), 
                              1.0)
    ) %>%
    select(-overlap_count)
  
  # Time-decay
  if (TIME_DECAY > 0) {
    max_time <- max(df$time)
    min_time <- min(df$time)
    time_range <- as.numeric(max_time - min_time)
    
    df <- df %>%
      mutate(
        sample_weight = sample_weight * 
          exp(-TIME_DECAY * as.numeric(max_time - time) / time_range)
      )
  }
  
  # Normalisiere
  df <- df %>%
    mutate(sample_weight = sample_weight / sum(sample_weight) * n())
}

end_time_weights <- Sys.time()
elapsed_weights <- as.numeric(difftime(end_time_weights, start_time_weights, units = "secs"))
cat("✓ Sample Weights berechnet in", round(elapsed_weights, 2), "Sekunden\n\n")

# ============================================================================
# SCHRITT 3: FEATURE SELECTION & DATA CLEANING
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  SCHRITT 3/5: FEATURE SELECTION & DATA CLEANING            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Identifiziere Feature-Spalten
exclude_cols <- c("time", "log_open", "log_low", "log_high", "log_close", "log_price",
                  "tb_label", "tb_exit_time", "sample_weight", "return",
                  "open", "high", "low", "close", "price", "atr", "tb_barrier_distance","tb_barrier_pct",
                  "tb_upper_barrier","tb_lower_barrier","tb_upper_barrier_log","tb_lower_barrier_log",  
                  "tb_bars_held","tb_return","tb_log_return" )

all_cols <- colnames(df)
feature_cols <- setdiff(all_cols, exclude_cols)
feature_cols <- feature_cols[sapply(df[feature_cols], is.numeric)] #erstmal nur 10 features auswählen

cat("Anzahl Features:", length(feature_cols), "\n")
cat("Feature-Namen (erste 20):\n")
print(head(feature_cols, 20))

# Bereinige timen (entferne Zeilen mit NAs in kritischen Spalten)
cat("\nBereinge timen...\n")
cat("  Vor Bereinigung:", nrow(df), "Zeilen\n")

df_clean <- df %>%
  select(all_of(c("time", "tb_exit_time", "sample_weight", feature_cols, "tb_label"))) %>%
  drop_na()

cat("  Nach Bereinigung:", nrow(df_clean), "Zeilen\n")
cat("  Entfernte Zeilen:", nrow(df) - nrow(df_clean), "\n\n")

# ============================================================================
# SCHRITT 4: PURGED CROSS-VALIDATION FOLDS
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  SCHRITT 4/5: PURGED K-FOLD CV SETUP                       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Erstelle", N_FOLDS, "Purged CV Folds...\n")

n <- nrow(df_clean)
fold_size <- floor(n / N_FOLDS)
folds <- list()

for (k in 1:N_FOLDS) {
  test_start <- (k - 1) * fold_size + 1
  test_end <- ifelse(k < N_FOLDS, k * fold_size, n)
  test_idx <- test_start:test_end
  
  train_idx <- setdiff(1:n, test_idx)
  
  # PURGING
  purge_idx <- c()
  if ("tb_exit_time" %in% colnames(df_clean)) {
    for (i in train_idx) {
      if (!is.na(df_clean$tb_exit_time[i])) {
        if (df_clean$tb_exit_time[i] >= df_clean$time[test_start] && 
            df_clean$tb_exit_time[i] <= df_clean$time[test_end]) {
          purge_idx <- c(purge_idx, i)
        }
      }
    }
    train_idx <- setdiff(train_idx, purge_idx)
  }
  
  # EMBARGOING
  embargo_size <- floor(n * EMBARGO_PCT)
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
  
  cat("  Fold", k, "- Train:", length(train_idx), 
      "| Test:", length(test_idx),
      "| Purged:", length(purge_idx),
      "| Embargoed:", length(embargo_idx), "\n")
}
cat("\n")

# ============================================================================
# SCHRITT 5: XGBOOST TRAINING (PARALLEL CV)
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  SCHRITT 5/5: XGBOOST TRAINING (PARALLEL CV)               ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# WICHTIG: XGBoost braucht Labels 0, 1, 2 (nicht -1, 0, 1)
cat("Transformiere Labels für XGBoost...\n")
cat("  Original Labels: -1, 0, 1\n")
cat("  XGBoost Labels:   0, 1, 2\n\n")

# Mapping: -1 → 0, 0 → 1, 1 → 2
df_clean$tb_label_xgb <- df_clean$tb_label + 1

# Überprüfung
cat("Label-Verteilung:\n")
cat("  Original:\n")
print(table(df_clean$tb_label))
cat("  XGBoost:\n")
print(table(df_clean$tb_label_xgb))
cat("\n")

start_time_xgb <- Sys.time()

cat("Trainiere", N_FOLDS, "Folds parallel...\n\n")

# Export für XGBoost Training
clusterExport(cl, c("df_clean", "folds", "feature_cols",
                    "MAX_DEPTH", "ETA", "MIN_CHILD_WEIGHT", "SUBSAMPLE",
                    "COLSAMPLE_BYTREE", "GAMMA", "NROUNDS",
                    "EARLY_STOPPING_ROUNDS"), 
              envir = environment())

clusterEvalQ(cl, {
  library(xgboost)
  library(dplyr)
})

# Parallel Training aller Folds
cv_results <- foreach(
  k = 1:N_FOLDS,
  .packages = c("xgboost", "dplyr")
) %dopar% {
  
  # Train/Test Split
  train_data <- df_clean[folds[[k]]$train, ]
  test_data <- df_clean[folds[[k]]$test, ]
  
  # DMatrix erstellen mit transformierten Labels (0, 1, 2)
  dtrain <- xgb.DMatrix(
    data = as.matrix(train_data[, feature_cols]),
    label = train_data$tb_label_xgb,  # ← Verwendet transformierte Labels
    weight = train_data$sample_weight
  )
  
  dtest <- xgb.DMatrix(
    data = as.matrix(test_data[, feature_cols]),
    label = test_data$tb_label_xgb,  # ← Verwendet transformierte Labels
    weight = test_data$sample_weight
  )
  
  watchlist <- list(train = dtrain, test = dtest)
  
  # XGBoost Parameter
  params <- list(
    objective = "multi:softprob",
    num_class = 3,
    max_depth = MAX_DEPTH,
    eta = ETA,
    min_child_weight = MIN_CHILD_WEIGHT,
    subsample = SUBSAMPLE,
    colsample_bytree = COLSAMPLE_BYTREE,
    gamma = GAMMA,
    eval_metric = "mlogloss",
    nthread = 1  # Jeder Fold single-threaded (CV ist parallel)
  )
  
  # Training
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = NROUNDS,
    watchlist = watchlist,
    early_stopping_rounds = EARLY_STOPPING_ROUNDS,
    verbose = 0
  )
  
  # Predictions
  preds <- predict(model, dtest, reshape = TRUE)
  pred_labels_xgb <- max.col(preds) - 1  # Konvertiere zu 0, 1, 2
  pred_labels <- pred_labels_xgb - 1     # Konvertiere zurück zu -1, 0, 1
  
  # Metriken (mit Original-Labels)
  accuracy <- mean(pred_labels == test_data$tb_label)
  conf_mat <- table(Predicted = pred_labels, Actual = test_data$tb_label)
  
  # Feature Importance
  importance <- xgb.importance(model = model, feature_names = feature_cols)
  
  # Return
  list(
    model = model,
    predictions = pred_labels,
    actual = test_data$tb_label,
    accuracy = accuracy,
    confusion_matrix = conf_mat,
    feature_importance = importance,
    best_iteration = model$best_iteration,
    fold = k
  )
}

end_time_xgb <- Sys.time()
elapsed_xgb <- as.numeric(difftime(end_time_xgb, start_time_xgb, units = "mins"))

# Aggregiere Ergebnisse
cat("\n=== Cross-Validation Ergebnisse ===\n")
for (k in 1:N_FOLDS) {
  cat("Fold", k, "- Accuracy:", round(cv_results[[k]]$accuracy, 4),
      "| Best Iteration:", cv_results[[k]]$best_iteration, "\n")
  cat("  Confusion Matrix:\n")
  print(cv_results[[k]]$confusion_matrix)
  cat("\n")
}

avg_accuracy <- mean(sapply(cv_results, function(x) x$accuracy))
cat("Durchschnittliche CV Accuracy:", round(avg_accuracy, 4), "\n")
cat("Training Zeit:", round(elapsed_xgb, 2), "Minuten\n\n")

# Feature Importance aggregieren
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

cat("=== Top 20 Features nach Gain ===\n")
print(head(avg_importance, 20))
cat("\n")

# ============================================================================
# FINALES MODELL AUF ALLEN DATEN
# ============================================================================

cat("=== Training Final Model auf allen Daten ===\n")

best_nrounds <- median(sapply(cv_results, function(x) x$best_iteration))
cat("Verwende", best_nrounds, "Iterationen (Median aus CV)\n")

# DMatrix mit transformierten Labels (0, 1, 2)
dtrain_full <- xgb.DMatrix(
  data = as.matrix(df_clean[, feature_cols]),
  label = df_clean$tb_label_xgb,  # ← Transformierte Labels
  weight = df_clean$sample_weight
)

# Nutze Multi-Threading für finales Modell
params_final <- list(
  objective = "multi:softprob",
  num_class = 3,
  max_depth = MAX_DEPTH,
  eta = ETA,
  min_child_weight = MIN_CHILD_WEIGHT,
  subsample = SUBSAMPLE,
  colsample_bytree = COLSAMPLE_BYTREE,
  gamma = GAMMA,
  eval_metric = "mlogloss",
  nthread = N_CORES
)

final_model <- xgb.train(
  params = params_final,
  data = dtrain_full,
  nrounds = best_nrounds,
  verbose = 1
)

cat("✓ Finales Modell trainiert\n\n")

# ============================================================================
# SPEICHERE ERGEBNISSE
# ============================================================================

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  SPEICHERE ERGEBNISSE                                       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Feature Importance
write.csv(avg_importance, 
          file.path(OUTPUT_DIR, "feature_importance.csv"),
          row.names = FALSE)
cat("✓ Feature Importance gespeichert\n")

# Modell
xgb.save(final_model, file.path(OUTPUT_DIR, "xgboost_model.bin"))
cat("✓ XGBoost Modell gespeichert\n")

# DataFrame mit Features
saveRDS(df_clean, file.path(OUTPUT_DIR, "df_with_features.rds"))
cat("✓ DataFrame gespeichert\n")

# CV Ergebnisse
cv_summary <- data.frame(
  Fold = 1:N_FOLDS,
  Accuracy = sapply(cv_results, function(x) x$accuracy),
  Best_Iteration = sapply(cv_results, function(x) x$best_iteration)
)
write.csv(cv_summary, file.path(OUTPUT_DIR, "cv_results.csv"), row.names = FALSE)
cat("✓ CV Ergebnisse gespeichert\n")

# Feature Importance Plot
library(ggplot2)
p <- avg_importance %>%
  head(20) %>%
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Features nach Gain",
    subtitle = paste("XGBoost | CV Accuracy:", round(avg_accuracy, 4)),
    x = "Feature",
    y = "Gain"
  ) +
  theme_minimal()

ggsave(file.path(OUTPUT_DIR, "feature_importance.png"), p,
       width = 10, height = 8, dpi = 300)
cat("✓ Feature Importance Plot gespeichert\n\n")

# ============================================================================
# CLEANUP
# ============================================================================

cleanup_parallel()

# ============================================================================
# ZUSAMMENFASSUNG
# ============================================================================

total_time <- as.numeric(difftime(Sys.time(), start_time_features, units = "mins"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║  ERFOLGREICH ABGESCHLOSSEN!                                 ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("⏱️  ZEITEN:\n")
cat("  Feature Engineering:", round(elapsed_features, 1), "Sekunden\n")
cat("  Sample Weights:", round(elapsed_weights, 1), "Sekunden\n")
cat("  XGBoost Training:", round(elapsed_xgb, 2), "Minuten\n")
cat("  Total:", round(total_time, 2), "Minuten\n\n")

cat("📊 ERGEBNISSE:\n")
cat("  Anzahl Features:", length(feature_cols), "\n")
cat("  CV Accuracy:", round(avg_accuracy, 4), "\n")
cat("  Best Nrounds:", best_nrounds, "\n")
cat("  Verwendete Cores:", N_CORES, "\n\n")

cat("📁 OUTPUT FILES:\n")
cat("  •", file.path(OUTPUT_DIR, "feature_importance.csv"), "\n")
cat("  •", file.path(OUTPUT_DIR, "feature_importance.png"), "\n")
cat("  •", file.path(OUTPUT_DIR, "xgboost_model.bin"), "\n")
cat("  •", file.path(OUTPUT_DIR, "df_with_features.rds"), "\n")
cat("  •", file.path(OUTPUT_DIR, "cv_results.csv"), "\n\n")

cat("✅ Script erfolgreich abgeschlossen!\n\n")

# ============================================================================
# ENDE
# ============================================================================