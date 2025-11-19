# ==============================================================================
# FINANCIAL TIME SERIES LABELING: Triple Barrier & Trend Scanning
# ==============================================================================

rm(list=ls())
gc()
options(scipen=999)

# Pakete laden -----------------------------------------------------------------

pacman::p_load(tidyverse,
               zoo,
               scales, 
               gridExtra,
               TTR)

# Daten laden ------------------------------------------------------------------
input_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/capitalcom_backtesting", "api-data")
output_path <- file.path("C:/Users/maxib/OneDrive/Dokumente/Finance/R ML Trading Complete", "labelled_data")

EPIC = "GOLD"
INTERVAL = "MINUTE_15"

filename = paste0(EPIC, "_", INTERVAL, ".csv")
output_file_name = paste0("triplebarrier_labelled_", EPIC, "_", INTERVAL, ".csv")

# Daten laden und vorbereiten
# Daten laden und vorbereiten
df_raw <- read_csv(file.path(input_path, filename)) %>%
  mutate(time = as.POSIXct(time, format="%Y-%m-%d %H:%M:%S")) %>%
  select(time, open, high, low, close, volume) %>%
  arrange(time) %>%
  # Log-Transformation für alle Preise
  mutate(
    log_open = log(open),
    log_high = log(high),
    log_low = log(low),
    log_close = log(close)
  )

# ==============================================================================
# 1. SYMMETRISCHE TRIPLE BARRIER METHOD
# ==============================================================================

triple_barrier_labeling <- function(data, 
                                    atr_multiplier = 3,        # 3x ATR für beide Barriers
                                    close_time = "22:30",      # Tägliche Schlusszeit
                                    atr_period = 14) {         # ATR Periode
  
  # Berechne ATR mit TTR Package (auf normalen Preisen)
  atr_data <- ATR(data[, c("high", "low", "close")], n = atr_period)
  atr <- atr_data[, "atr"]
  
  n <- nrow(data)
  labels <- rep(NA, n)
  barrier_touched <- rep(NA, n)
  bars_held <- rep(NA, n)
  returns <- rep(NA, n)
  log_returns <- rep(NA, n)  # Neue Spalte für log returns
  entry_time_char <- rep(NA, n)
  exit_time_char <- rep(NA, n)
  barrier_distances <- rep(NA, n)
  upper_barriers <- rep(NA, n)
  lower_barriers <- rep(NA, n)
  upper_barriers_log <- rep(NA, n)  # Log Barriers
  lower_barriers_log <- rep(NA, n)  # Log Barriers
  
  # Konvertiere close_time zu time object für Vergleich
  close_hour <- as.numeric(substr(close_time, 1, 2))
  close_minute <- as.numeric(substr(close_time, 4, 5))
  
  for (i in 1:(n - 1)) {
    # Überspringe wenn ATR noch nicht berechnet werden kann
    if (is.na(atr[i]) || atr[i] == 0) {
      next
    }
    
    entry_price <- data$close[i]
    entry_log_price <- data$log_close[i]
    entry_datetime <- data$time[i]
    entry_time_char[i] <- format(entry_datetime, "%Y-%m-%d %H:%M:%S")
    
    # SYMMETRISCH: Beide Barriers basieren auf ATR
    # Upper Barrier: Entry + 3x ATR (Profit für Long)
    # Lower Barrier: Entry - 3x ATR (Profit für Short)
    barrier_distance <- atr_multiplier * atr[i]
    
    upper_barrier <- entry_price + barrier_distance
    lower_barrier <- entry_price - barrier_distance
    
    # Log-Barriers berechnen
    upper_barrier_log <- log(upper_barrier)
    lower_barrier_log <- log(lower_barrier)
    
    # Speichere die Barrier-Werte
    barrier_distances[i] <- barrier_distance
    upper_barriers[i] <- upper_barrier
    lower_barriers[i] <- lower_barrier
    upper_barriers_log[i] <- upper_barrier_log
    lower_barriers_log[i] <- lower_barrier_log
    
    # Finde den nächsten 22:30 Zeitpunkt
    entry_date <- as.Date(entry_datetime)
    close_datetime <- as.POSIXct(paste(entry_date, close_time), tz = attr(entry_datetime, "tzone"))
    
    # Wenn Entry nach 22:30 ist, nimm 22:30 vom nächsten Tag
    if (entry_datetime >= close_datetime) {
      close_datetime <- close_datetime + days(1)
    }
    
    # Finde alle zukünftigen Zeitpunkte bis zur Schlusszeit
    future_indices <- which(data$time > entry_datetime & data$time <= close_datetime)
    
    if (length(future_indices) == 0) {
      # Keine zukünftigen Daten verfügbar
      next
    }
    
    future_highs <- data$high[future_indices]
    future_lows <- data$low[future_indices]
    future_times <- data$time[future_indices]
    
    # Finde welche Barrier zuerst getroffen wird
    upper_touch <- which(future_highs >= upper_barrier)
    lower_touch <- which(future_lows <= lower_barrier)
    
    if (length(upper_touch) > 0 && length(lower_touch) > 0) {
      # Beide Barriers getroffen - welche zuerst?
      if (upper_touch[1] < lower_touch[1]) {
        labels[i] <- 1  # Upper Barrier zuerst (Long gewinnt)
        barrier_touched[i] <- "upper"
        bars_held[i] <- upper_touch[1]
        returns[i] <- barrier_distance / entry_price
        log_returns[i] <- upper_barrier_log - entry_log_price  # Log Return
        exit_time_char[i] <- format(future_times[upper_touch[1]], "%Y-%m-%d %H:%M:%S")
      } else {
        labels[i] <- -1  # Lower Barrier zuerst (Short gewinnt)
        barrier_touched[i] <- "lower"
        bars_held[i] <- lower_touch[1]
        returns[i] <- -barrier_distance / entry_price
        log_returns[i] <- lower_barrier_log - entry_log_price  # Log Return (negativ)
        exit_time_char[i] <- format(future_times[lower_touch[1]], "%Y-%m-%d %H:%M:%S")
      }
    } else if (length(upper_touch) > 0) {
      labels[i] <- 1  # Nur Upper Barrier getroffen (Long gewinnt)
      barrier_touched[i] <- "upper"
      bars_held[i] <- upper_touch[1]
      returns[i] <- barrier_distance / entry_price
      log_returns[i] <- upper_barrier_log - entry_log_price
      exit_time_char[i] <- format(future_times[upper_touch[1]], "%Y-%m-%d %H:%M:%S")
    } else if (length(lower_touch) > 0) {
      labels[i] <- -1  # Nur Lower Barrier getroffen (Short gewinnt)
      barrier_touched[i] <- "lower"
      bars_held[i] <- lower_touch[1]
      returns[i] <- -barrier_distance / entry_price
      log_returns[i] <- lower_barrier_log - entry_log_price
      exit_time_char[i] <- format(future_times[lower_touch[1]], "%Y-%m-%d %H:%M:%S")
    } else {
      # Time Barrier (22:30 erreicht)
      labels[i] <- 0
      barrier_touched[i] <- "time_2230"
      bars_held[i] <- length(future_indices)
      
      # Exit beim letzten verfügbaren Zeitpunkt vor/bei 22:30
      exit_idx <- future_indices[length(future_indices)]
      exit_price <- data$close[exit_idx]
      exit_log_price <- data$log_close[exit_idx]
      returns[i] <- (exit_price - entry_price) / entry_price
      log_returns[i] <- exit_log_price - entry_log_price  # Log Return
      exit_time_char[i] <- format(data$time[exit_idx], "%Y-%m-%d %H:%M:%S")
    }
  }
  
  data %>%
    mutate(
      atr = as.numeric(atr),
      tb_barrier_distance = barrier_distances,
      tb_barrier_pct = barrier_distances / close,  # Barrier in %
      tb_upper_barrier = upper_barriers,
      tb_lower_barrier = lower_barriers,
      tb_upper_barrier_log = upper_barriers_log,
      tb_lower_barrier_log = lower_barriers_log,
      tb_label = labels,
      tb_barrier = barrier_touched,
      tb_bars_held = bars_held,
      tb_return = returns,           # Einfacher Return
      tb_log_return = log_returns,   # Log Return
      tb_entry_time = entry_time_char,
      tb_exit_time = exit_time_char
    )
}

# Anwenden der Symmetrischen Triple Barrier Methode
df_labeled <- triple_barrier_labeling(df_raw)

# Speichern der gelabelten Daten
write_csv(df_labeled, file.path(output_path, paste0("triplebarrier_labelled_", EPIC, "_", INTERVAL, ".csv")))

# ==============================================================================
# 2. STATISTIKEN
# ==============================================================================

# Triple Barrier Statistiken
tb_stats <- df_labeled %>%
  filter(!is.na(tb_label)) %>%
  group_by(tb_label) %>%
  summarise(
    count = n(),
    percentage = n() / nrow(filter(df_labeled, !is.na(tb_label))) * 100,
    mean_return = mean(tb_return, na.rm = TRUE),
    mean_log_return = mean(tb_log_return, na.rm = TRUE),
    mean_barrier_pct = mean(tb_barrier_pct, na.rm = TRUE) * 100,
    median_bars = median(tb_bars_held, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label_name = case_when(
    tb_label == -1 ~ "Short (Lower Barrier)",
    tb_label == 0 ~ "Neutral (Time)",
    tb_label == 1 ~ "Long (Upper Barrier)"
  ))

print("=== SYMMETRISCHE TRIPLE BARRIER STATISTIKEN ===")
print(tb_stats)

# Log Return Vergleich
log_return_comparison <- df_labeled %>%
  filter(!is.na(tb_log_return)) %>%
  summarise(
    mean_simple_return = mean(tb_return, na.rm = TRUE),
    mean_log_return = mean(tb_log_return, na.rm = TRUE),
    sd_simple_return = sd(tb_return, na.rm = TRUE),
    sd_log_return = sd(tb_log_return, na.rm = TRUE)
  )

print("\n=== RETURN VERGLEICH (Simple vs. Log) ===")
print(log_return_comparison)

# ATR Statistiken
atr_stats <- df_labeled %>%
  filter(!is.na(atr) & atr > 0) %>%
  summarise(
    mean_atr = mean(atr, na.rm = TRUE),
    median_atr = median(atr, na.rm = TRUE),
    min_atr = min(atr, na.rm = TRUE),
    max_atr = max(atr, na.rm = TRUE),
    mean_barrier_pct = mean(tb_barrier_pct, na.rm = TRUE) * 100
  )

print("\n=== ATR STATISTIKEN ===")
print(atr_stats)

# Symmetrie-Check
symmetry_check <- df_labeled %>%
  filter(!is.na(tb_label)) %>%
  count(tb_label) %>%
  mutate(
    label_name = case_when(
      tb_label == -1 ~ "Short",
      tb_label == 0 ~ "Neutral", 
      tb_label == 1 ~ "Long"
    )
  )

print("\n=== SYMMETRIE-CHECK (Label Verteilung) ===")
print(symmetry_check)

# ==============================================================================
# 3. VISUALISIERUNGEN
# ==============================================================================

# Farbpalette definieren
colors_labels <- c("-1" = "#E74C3C", "0" = "#95A5A6", "1" = "#27AE60")

# Plot 1: Triple Barrier Labels über Zeit mit Log Preisen
p1 <- df_labeled %>%
  filter(!is.na(tb_label)) %>%
  ggplot(aes(x = time, y = log_close)) +
  geom_line(color = "gray40", alpha = 0.5) +
  geom_point(aes(color = factor(tb_label)), size = 1.5, alpha = 0.6) +
  scale_color_manual(values = colors_labels,
                     labels = c("Short (-1)", "Neutral (0)", "Long (1)"),
                     name = "Label") +
  labs(title = "Symmetrische Triple Barrier Labeling mit ATR (Log Prices)",
       subtitle = sprintf("Beide Barriers: 3x ATR (symmetrisch) | Täglicher Close: 22:30"),
       x = "Zeit", y = "Log(Gold Preis)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))

# Plot 4: Return Distribution Comparison (Simple vs Log)
p4_simple <- df_labeled %>%
  filter(!is.na(tb_return)) %>%
  ggplot(aes(x = tb_return, fill = factor(tb_label))) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = colors_labels,
                    labels = c("Short (-1)", "Neutral (0)", "Long (1)"),
                    name = "Label") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Simple Returns",
       x = "Simple Return", y = "Häufigkeit") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12))

p4_log <- df_labeled %>%
  filter(!is.na(tb_log_return)) %>%
  ggplot(aes(x = tb_log_return, fill = factor(tb_label))) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = colors_labels,
                    labels = c("Short (-1)", "Neutral (0)", "Long (1)"),
                    name = "Label") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Log Returns",
       x = "Log Return", y = "Häufigkeit") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12))



# Detailansicht: Zoom auf eine Woche (Log Prices)
zoom_start <- df_labeled$time[500]
zoom_end <- zoom_start + days(7)

p6_a <- df_labeled %>%
  filter(time >= zoom_start & time <= zoom_end, !is.na(tb_label)) %>%
  ggplot(aes(x = time, y = log_close)) +
  geom_ribbon(aes(ymin = tb_lower_barrier_log, ymax = tb_upper_barrier_log), 
              fill = "lightblue", alpha = 0.2) +
  geom_line(color = "gray40", size = 0.8) +
  geom_line(aes(y = tb_upper_barrier_log), color = "#27AE60", linetype = "dashed", alpha = 0.5) +
  geom_line(aes(y = tb_lower_barrier_log), color = "#E74C3C", linetype = "dashed", alpha = 0.5) +
  geom_point(aes(color = factor(tb_label)), size = 3, alpha = 0.7) +
  scale_color_manual(values = colors_labels,
                     labels = c("Short (-1)", "Neutral (0)", "Long (1)"),
                     name = "Triple Barrier") +
  labs(title = "Detailansicht: Symmetrische Barriers in Log Space (7 Tage)",
       subtitle = "Grüne Linie = Upper Barrier (log) | Rote Linie = Lower Barrier (log)",
       x = "Zeit", y = "Log(Gold Preis)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12))

# ATR über Zeit
p6_b <- df_labeled %>%
  filter(!is.na(atr) & atr > 0) %>%
  ggplot(aes(x = time, y = atr)) +
  geom_line(color = "#E74C3C", size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "#3498DB", alpha = 0.2) +
  labs(title = "ATR (14) über Zeit",
       subtitle = "Average True Range bestimmt beide Barrier-Abstände",
       x = "Zeit", y = "ATR (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12))