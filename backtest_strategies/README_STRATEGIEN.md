# Trading Strategien - Backtest & Permutation Files

## Übersicht

Dieses Verzeichnis enthält **14 Dateien** für **7 verschiedene Trading-Strategien**:
- Jeweils 1 Backtest-Datei (`03_backtest_*.R`)
- Jeweils 1 MCPT-Permutations-Datei (`04_permutations_*.R`)

## Status der Implementierungen

### ✅ VOLLSTÄNDIG IMPLEMENTIERT (Produktionsreif)

#### 1. **Hurst_DPO** (Hurst Exponent + Detrended Price Oscillator)
- **Dateien:**
  - `Hurst_DPO/03_backtest_Hurst_DPO_pyramiding.R`
  - `Hurst_DPO/04_permutations_Hurst_DPO.R`
- **Signal-Logik:** Trending-Markt (Hurst > 0.55) + DPO Zero-Cross + Price Momentum
- **Status:** ✅ Komplett - Direkt ausführbar

#### 2. **Chop_Aroon** (Choppiness Index + Aroon Oscillator)
- **Dateien:**
  - `Chop_Aroon/03_backtest_Chop_Aroon_pyramiding.R`
  - `Chop_Aroon/04_permutations_Chop_Aroon.R`
- **Signal-Logik:** Nicht-choppy Markt (Chop < 38.2) + Starker Aroon Trend
- **Status:** ✅ Komplett - Direkt ausführbar

### 🔧 FRAMEWORK-IMPLEMENTIERUNGEN (Erweiterung nötig)

Die folgenden Strategien enthalten:
- ✅ Indicator-Funktionen
- ✅ Entry-Signal-Logik
- ⚠️ Position Management muss von vollständigen Dateien kopiert werden

#### 3. **STC** (Schaff Trend Cycle)
- **Signal-Logik:** STC Cross 25 (Long) / 75 (Short)
- **Parameter:** Fast=23, Slow=50, Cycle=10

#### 4. **Coppock_ROC** (Coppock Curve + Multi-ROC)
- **Signal-Logik:** Coppock Zero-Cross + Alle ROCs aligned
- **Parameter:** Coppock(7,10,8), ROC(3,5,8)

#### 5. **Fisher** (Ehlers Fisher Transform)
- **Signal-Logik:** Fisher crosses Signal Line
- **Parameter:** Fisher(10)

#### 6. **FRAMA_VHF** (Fractal Adaptive MA + VHF)
- **Signal-Logik:** Price crosses FRAMA + VHF trending
- **Parameter:** FRAMA(16), VHF(28, Threshold=0.35)

#### 7. **McGinley_KST** (McGinley Dynamic + Know Sure Thing)
- **Signal-Logik:** Price vs McGinley + KST Momentum
- **Parameter:** McGinley(14), KST(6,9,12,18)

---

## Gemeinsame Features (Alle Strategien)

### Pyramiding-Methoden
- **Breakout:** Entry bei neuen Highs/Lows
- **Consecutive:** Entry nach 2-3 aufeinanderfolgenden Bars in Trendrichtung
- **Max Orders:** 2-3 zusätzliche Positionen
- **Spacing:** 0.5-1.0 ATR zwischen Entries
- **Size Multiplier:** 0.5 (jede weitere Position ist 50% der ursprünglichen)

### Exit-Strategien

#### Chandelier Exit
- Dynamischer Trailing Stop basierend auf ATR
- Multiplier: 2.5-3.0x ATR
- Period: 22 Bars

#### Breakeven + Trailing Stop
- **Phase 1:** Move to Breakeven nach 1.2-1.5x ATR Profit
- **Phase 2:** Activate Trailing Stop nach weiterem Profit
- Trailing Distance: 1.8-2.0x ATR

### Take Profit

#### Full Take Profit
- Single TP Level: 4.0-4.5x ATR

#### Partial Take Profit
- **Stage 1:** 2.0-2.5x ATR → Close 33% der Position
- **Stage 2:** 3.5-4.0x ATR → Close weitere 33%
- **Remaining:** Trailing Stop für restliche 34%

### Risk Management
- **Initial Stop Loss:** 2.5-3.0x ATR
- **Max Bars in Trade:** 80-100
- **Time Exit:** 22:00 Uhr (Schließe alle Positionen)
- **Spread:** 35 Pips
- **Initial Capital:** 300 EUR
- **Lot Size:** 1.2

---

## Verwendung

### Für vollständige Implementierungen (Hurst_DPO, Chop_Aroon):

```r
# 1. Öffne die Backtest-Datei in RStudio
source("backtest_strategies/Hurst_DPO/03_backtest_Hurst_DPO_pyramiding.R")

# 2. Führe Optimierung aus (automatisch beim Sourcing)
# Ergebnis: optimization_results mit Top-Parametern

# 3. Führe MCPT aus
source("backtest_strategies/Hurst_DPO/04_permutations_Hurst_DPO.R")
# Ergebnis: p-values und Visualisierungen
```

### Für Framework-Implementierungen (STC, Coppock_ROC, Fisher, FRAMA_VHF, McGinley_KST):

**Methode 1: Manuelle Erweiterung**
```r
# 1. Öffne eine vollständige Datei als Referenz
#    (z.B. Hurst_DPO/03_backtest_Hurst_DPO_pyramiding.R)

# 2. Öffne die Framework-Datei (z.B. STC/03_backtest_STC_pyramiding.R)

# 3. Kopiere aus der Referenz-Datei:
#    - calculate_indicators() Funktion
#    - generate_signals() Funktion (komplett mit Position Management)
#    - calculate_performance() Funktion
#    - run_backtest() Funktion
#    - optimize_parameters() Funktion
#    - Parameter-Grid Definition
#    - Optimierungs-Aufruf

# 4. In der Framework-Datei:
#    - Behalte nur die Indicator-Berechnungen bei
#    - Ersetze die Entry-Signal-Logik in generate_signals()
#    - Passe Parameter an
```

**Methode 2: Template-Ansatz**
```r
# 1. Kopiere die komplette Hurst_DPO Datei
cp Hurst_DPO/03_backtest_Hurst_DPO_pyramiding.R STC/03_backtest_STC_pyramiding.R

# 2. Öffne die neue Datei und ersetze:
#    - Indikator-Funktionen (Zeilen ~89-108)
#    - calculate_indicators() Body (Zeilen ~129-154)
#    - Entry-Signal-Logik in generate_signals() (Zeilen ~220-262)
#    - Parameter-Konstanten (am Anfang)
#    - Parameter-Grid (Zeilen ~1401-1450)
```

---

## Parameter-Optimierung

Alle Strategien unterstützen:
- **Parallelisierte Optimierung** (Multi-Core)
- **Flexible Parameter-Grids**
- **Minimum Trades Filter** (Default: 20)
- **Multiple Metriken:** Sharpe Ratio, Total Return, Profit Factor, Win Rate

Beispiel-Grid:
```r
param_grid <- expand.grid(
  # Indicator Parameters
  indicator_param1 = c(10, 15, 20),
  indicator_param2 = c(0.3, 0.4, 0.5),

  # Stop Loss
  stop_loss_atr_mult = c(2.5, 3.0),

  # Pyramiding
  use_pyramiding = c(TRUE, FALSE),
  pyramid_method = c("breakout", "consecutive"),
  max_pyramid_orders = c(2, 3),

  # Exit Strategy
  exit_strategy = c("chandelier", "breakeven_trailing"),

  # Take Profit
  tp_strategy = c("full", "partial"),
  full_tp_atr_mult = c(4.0, 4.5),

  # etc...
)
```

---

## MCPT (Monte Carlo Permutation Test)

Alle Permutations-Dateien führen durch:

1. **Block-Permutation** der Returns (20 Blocks)
2. **300 Permutationen** pro Dataset
3. **P-Value Berechnung** (H0: Strategy = Random)
4. **Visualisierungen:**
   - Histogram der permutierten Metriken
   - Echte Performance als rote Linie
   - Equity Curves für Training & Test
5. **Interpretation:**
   - p < 0.05: Strategie ist statistisch signifikant
   - p ≥ 0.05: Mögliches Overfitting

---

## Dateistruktur

```
backtest_strategies/
│
├── Hurst_DPO/
│   ├── 03_backtest_Hurst_DPO_pyramiding.R    [✅ Vollständig]
│   └── 04_permutations_Hurst_DPO.R           [✅ Vollständig]
│
├── Chop_Aroon/
│   ├── 03_backtest_Chop_Aroon_pyramiding.R   [✅ Vollständig]
│   └── 04_permutations_Chop_Aroon.R          [✅ Vollständig]
│
├── STC/
│   ├── 03_backtest_STC_pyramiding.R          [🔧 Framework]
│   └── 04_permutations_STC.R                 [🔧 Framework]
│
├── Coppock_ROC/
│   ├── 03_backtest_Coppock_ROC_pyramiding.R  [🔧 Framework]
│   └── 04_permutations_Coppock_ROC.R         [🔧 Framework]
│
├── Fisher/
│   ├── 03_backtest_Fisher_pyramiding.R       [🔧 Framework]
│   └── 04_permutations_Fisher.R              [🔧 Framework]
│
├── FRAMA_VHF/
│   ├── 03_backtest_FRAMA_VHF_pyramiding.R    [🔧 Framework]
│   └── 04_permutations_FRAMA_VHF.R           [🔧 Framework]
│
└── McGinley_KST/
    ├── 03_backtest_McGinley_KST_pyramiding.R [🔧 Framework]
    └── 04_permutations_McGinley_KST.R        [🔧 Framework]
```

---

## Workflow-Empfehlung

### Phase 1: Parameter-Optimierung (Training Data)
```r
# 1. Führe 03_backtest_*.R aus
# 2. Analysiere Top 10 Parameter-Kombinationen
# 3. Wähle robuste Parameter (nicht Extremwerte)
```

### Phase 2: Validierung (Test Data)
```r
# 1. Teste Top-Parameter auf Test-Daten
# 2. Vergleiche In-Sample vs Out-of-Sample Performance
# 3. Akzeptiere nur Strategien mit konsistenter Performance
```

### Phase 3: MCPT (Statistische Robustheit)
```r
# 1. Führe 04_permutations_*.R aus
# 2. Prüfe p-values (beide < 0.05?)
# 3. Wenn signifikant: Strategie ist robust
# 4. Wenn nicht signifikant: Overfitting-Risiko
```

### Phase 4: Live-Trading (Nach erfolgreicher Validierung)
```r
# 1. Start mit kleiner Position Size
# 2. Monitor Performance täglich
# 3. Stoppe bei signifikanter Abweichung
```

---

## Benötigte Pakete

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "TTR",
  "PerformanceAnalytics",
  "tictoc",
  "pracma",
  "data.table",
  "scales",
  "parallel",
  "doParallel",
  "foreach"
))
```

---

## Kontakt & Support

Bei Fragen zu einzelnen Strategien:
- Vollständige Implementierungen: Direkt ausführbar
- Framework-Implementierungen: Siehe "Verwendung" oben

---

**Erstellt:** 2025-11-19
**Version:** 1.0
**Strategien:** 7
**Dateien:** 14
