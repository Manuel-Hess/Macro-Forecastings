###########################################################
# Principles & Practice of Forecasting – Final Project Script
# Retail Sales (RSAFSNA) – SARIMA vs Dynamic Regression vs ETS
# Produces: all figures + tables for final report
############################################################

# -----------------------------
# 0) Setup: packages
# -----------------------------
pkgs <- c(
  "tidyverse", "tsibble", "fable", "fabletools", "feasts",
  "ggtime", "lubridate", "scales", "broom", "urca"
)

# Install missing packages (optional; comment out for final submission)
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(ggtime)
library(lubridate)
library(scales)
library(broom)



# -----------------------------
# 1) Paths (relative to project/repo root)
# -----------------------------
# Assumes this script is run from within the RStudio Project for this repo,
# with the data file placed in a "data" subfolder.
data_path <- 'data/SALES_DATA.csv'
out_dir   <- 'output'

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE)

# Helper for saving
fig_path <- function(name) file.path(out_dir, "figures", name)
tab_path <- function(name) file.path(out_dir, "tables", name)

# -----------------------------
# 2) Load + clean data
# -----------------------------
sales_raw <- read_csv(data_path, show_col_types = FALSE)

# Expect columns: observation_date (date) and RSAFSNA (numeric)
#sales <- sales_raw |>
# mutate(
#  observation_date = yearmonth(observation_date)
#  ) |>
#  as_tsibble(index = observation_date) |>
#  arrange(observation_date)

library(dplyr)
library(tsibble)
library(lubridate)

#changed from original code 
sales <- sales_raw |>
  mutate(
    observation_date = dmy(observation_date),          # parse "01.09.2010"
    observation_date = yearmonth(observation_date)     # convert to yearmonth
  ) |>
  as_tsibble(index = observation_date) |>
  arrange(observation_date)



# Basic checks
stopifnot("RSAFSNA" %in% names(sales))
stopifnot(!anyDuplicated(sales$observation_date))
anyNA(sales_raw) #line added 

# -----------------------------
# 3) Define analysis window + interventions + splits
# -----------------------------
# Analysis window for the paper
start_ym <- yearmonth("2010 Sep")
end_ym   <- yearmonth("2025 Sep")

sales <- sales |>
  filter(observation_date >= start_ym, observation_date <= end_ym)

# Intervention dummies (alternatives for robustness)
sales <- sales |>
  mutate(
    covid_shock = if_else(observation_date >= yearmonth("2020 Mar") &
                            observation_date <= yearmonth("2020 Jun"), 1, 0),
    covid_year  = if_else(observation_date >= yearmonth("2020 Mar") &
                            observation_date <= yearmonth("2021 Feb"), 1, 0),
    covid_step  = if_else(observation_date >= yearmonth("2020 Mar"), 1, 0)
  )

# Train/test split
train_end <- yearmonth("2021 Jan")
test_start <- yearmonth("2021 Feb")

##I chose the test-split like that, so the dummy is still in training set
train <- sales |> filter(observation_date <= train_end)
test  <- sales |> filter(observation_date >  train_end)


# Optional: log transform (guard against non-positive values; should be fine here)
sales <- sales |> mutate(log_RSAFSNA = log(RSAFSNA))
train <- train |> mutate(log_RSAFSNA = log(RSAFSNA))
test  <- test  |> mutate(log_RSAFSNA = log(RSAFSNA))

##Zoom into COVID shock
# --------------------------------------------------
# Figure X: Observed retail sales around COVID shock
# --------------------------------------------------
sales_zoom_obs <- sales |>
  dplyr::filter(
    observation_date >= tsibble::yearmonth("2019 Jan"),
    observation_date <= tsibble::yearmonth("2021 Dec")
  )

p_covid <- ggplot(sales_zoom_obs, aes(x = observation_date, y = RSAFSNA)) +
  geom_line(color = "black", linewidth = 0.8) +
  labs(
    title = "Retail Sales Around the COVID-19 Shock",
    subtitle = "Observed data (2019–2021)",
    x = "Year",
    y = "USD (billions)"
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1))

ggsave(fig_path("Figure_COVID_zoom_observed.png"),
       p_covid, width = 9, height = 4.5, dpi = 300)

print(p_covid)
##To show the shock and then the subsequent steep increase in Sales

# -----------------------------
# 4) FIGURES (Person 1): EDA
# -----------------------------

# (Formatting) nicer axis labels in "billions" if you prefer:
# RSAFSNA is "millions of USD". So billions = RSAFSNA / 1000.
sales <- sales |> mutate(RSAFSNA_bil = RSAFSNA / 1000)
train <- train |> mutate(RSAFSNA_bil = RSAFSNA / 1000)
test  <- test  |> mutate(RSAFSNA_bil = RSAFSNA / 1000)

# Figure 1: Time series plot (levels)
p1 <- sales |>
  autoplot(RSAFSNA_bil) +
  labs(
    title = "Retail and Food Services Sales (Not Seasonally Adjusted)",
    subtitle = "September 2010 – September 2025",
    x = "Year",
    y = "USD (billions)"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 1))

print(p1) #new line

ggsave(fig_path("Figure1_series.png"), p1, width = 9, height = 4.8, dpi = 300)

# Figure 2: Seasonal subseries plot (use ggtime version; feasts gg_subseries deprecated)
p2 <- ggtime::gg_subseries(sales, RSAFSNA_bil) +
  labs(
    title = "Seasonal Subseries Plot of Retail and Food Services Sales",
    x = "Year",
    y = "USD (billions)"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 1))

#print(p2)

ggsave(fig_path("Figure2_subseries.png"), p2, width = 9, height = 6, dpi = 300)

# Figure 3: STL decomposition (periodic season)
p3 <- sales |>
  model(STL(RSAFSNA_bil ~ season(window = "periodic"))) |>
  components() |>
  autoplot() +
  labs(
    title = "STL Decomposition of Retail and Food Services Sales (Not Seasonally Adjusted)",
    x = "Year"
  )

ggsave(fig_path("Figure3_STL.png"), p3, width = 9, height = 6.5, dpi = 300)

#print(p3)

# Optional EDA: ACF/PACF of levels (useful for appendix)
p_acf <- sales |>
  ACF(RSAFSNA_bil) |>
  autoplot() +
  labs(title = "ACF of Retail Sales (Levels)", x = "Lag", y = "ACF")
#print(p_acf)

p_pacf <- sales |>
  PACF(RSAFSNA_bil) |>
  autoplot() +
  labs(title = "PACF of Retail Sales (Levels)", x = "Lag", y = "PACF")
#print(p_pacf)


ggsave(fig_path("Appendix_ACF_levels.png"), p_acf, width = 7, height = 4.5, dpi = 300)
ggsave(fig_path("Appendix_PACF_levels.png"), p_pacf, width = 7, height = 4.5, dpi = 300)

#added UNIT ROOT TEST
sales |>
  features(RSAFSNA_bil, unitroot_kpss)
#stationarity rejected 

#numbers of differences required 
sales |>
  features(RSAFSNA, unitroot_ndiffs)

# -----------------------------
# 5) MODELS (Person 2): Fit + report
# -----------------------------
# We estimate:
# 1) Baseline seasonal ARIMA
# 2) Dynamic regression with seasonal ARIMA errors (COVID dummy in mean)
# 3) ETS benchmark

# MAIN intervention dummy choice (set this once)
dummy_var <- "covid_shock"   # options: "covid_shock", "covid_year", "covid_step"

# Build formula programmatically
xreg_formula <- as.formula(paste0("RSAFSNA_bil ~ ", dummy_var))

# --------------------------------------------------
# Model 1: Baseline SARIMA: candidate models
# --------------------------------------------------
sarima_cands <- train |>
  model(
    sarima_001_012 = ARIMA(RSAFSNA_bil ~ pdq(0,0,1) + PDQ(0,1,2)),
    sarima_003_012 = ARIMA(RSAFSNA_bil ~ pdq(0,0,3) + PDQ(0,1,2)),
    sarima_210_011 = ARIMA(RSAFSNA_bil ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto_sarima    = ARIMA(RSAFSNA_bil)
  )

##Why these candidate-sarimamodels(maybe put this in appendix):
##1.)sarima_001_012: This model captures strong annual seasonality via a seasonal difference and allows for simple short-run dynamics through a low order MA component.
##2.)sarima_003_012: This specification extends the MA structure to allow for richer short-run dynamics while maintaining the same seasonal structure.
##3.)sarima_210_011: An AR dominated alternative is included to test whether autoregressive dynamics outperform moving-average specifications
##4.)auto_sarima: The automatic SARIMA model serves as data-driven benchmark, selecting the specification that minimizes AICc without manual restrictions. 
## 4 SARIMA-models all trained on the trainingsdata

sarima_rank <- sarima_cands |>
  glance() |>
  arrange(AICc) |>
  select(.model, AICc, BIC)

print(sarima_rank) #added

write_csv(sarima_rank, tab_path("Appendix_SARIMA_ranking.csv"))

# pick best SARIMA model
best_sarima_name <- sarima_rank$.model[1]
print(best_sarima_name) #added 

##As a baseline model, we estimate a seasonal ARIMA (SARIMA) specification for monthly retail sales.
##Several candidate SARIMA models were considered, motivated by strong annual seasonality in the data.
##Model selection was based on the corrected Akaike Information Criterion (AICc).
##-->Two SARIMA candidates achieved identical AICc; we therefore select the automatic ARIMA specification as a transparent, data-driven baseline (results are robust to choosing the closely competing manual specification).
##This model therefore serves as a suitable univariate benchmark for forecasting retail sales.

fits <- train |>
  model(
    SARIMA = ARIMA(RSAFSNA_bil ~ pdq(0,0,1) + PDQ(0,1,2)),
    DR_COVID = ARIMA(xreg_formula),
    ETS = ETS(RSAFSNA_bil)
  )
##Fits is the Container for the estimated models (in fable it is called mable).
coef_table <- fits |> tidy()
names(coef_table)
# Save model reports to text (useful for appendix)
sink(file.path(out_dir, "tables", "ModelReports.txt"))
cat("=== MODEL REPORTS (TRAINING SAMPLE) ===\n\n")
print(report(fits))
sink()

#lines added to report each model individually 
report(fits |> select(SARIMA))
report(fits |> select(DR_COVID))
report(fits |> select(ETS))


###Start deleting from here on we the outcommented plot is not needed. 
##Now we take the best performing sarima model (with smallest AICc) and plot it.
##fc <- fits |>
##forecast(new_data = test)

##p4 <- autoplot(fc, sales) +
##labs(
##title = "Forecast Comparison (Test Period)",
##subtitle = "SARIMA vs Dynamic Regression (COVID dummy) vs ETS",
##x = "Year",
##y = "USD (billions)"
##) +
##scale_y_continuous(labels = label_number(accuracy = 1))

##ggsave(fig_path("Figure4_forecasts_full.png"), p4, width = 9, height = 5, dpi = 300)
##print(p4)##Now we want to display the plot. We have now our baseline SARIMA model.
###Until here we can delete it.

##Interpretation: The baseline SARIMA model captures trend and seasonality well but 
##fails to account for the abrupt COVID-19 shock, motivating the inclusion of an explicit intervention in the dynamic regression model.

# --------------------------------------------------
# Model 2: Dynamic regression with COVID-year dummy
# --------------------------------------------------

dummy_var <- "covid_year"
xreg_formula <- as.formula(paste0("RSAFSNA_bil ~ ", dummy_var))

fit_dr <- train |>
  model(DR_COVID = ARIMA(xreg_formula))

# Check model
fit_dr
report(fit_dr)

##About the dummy: The COVID dummy is defined as a temporary intervention covering January 2020 to January 2021.
##  This specification reflects the period of the most severe pandemic-related disruptions, 
##  including lockdowns and demand shocks, while allowing the series to return to its pre-existing trend thereafter.
##  The one-year window captures a persistent but transitory deviation rather than a permanent structural break.

##  The estimated COVID-year dummy is large, negative, and highly significant, 
##  indicating a substantial temporary decline in retail sales during the peak pandemic period.

# --------------------------------------------------
# Compare Model 1 vs Model 2 (SARIMA vs DR_COVID)
# --------------------------------------------------
fits_12 <- train |>
  model(
    SARIMA   = ARIMA(RSAFSNA_bil),
    DR_COVID = ARIMA(xreg_formula)
  )

fc_12 <- fits_12 |>
  forecast(new_data = test)

p12 <- autoplot(fc_12, sales) +
  labs(
    title = "Forecast Comparison (Test Period)",
    subtitle = "Baseline SARIMA vs Dynamic Regression (COVID-year dummy)",
    x = "Year",
    y = "USD (billions)"
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1))

ggsave(fig_path("Figure_SARIMA_vs_DR.png"), p12, width = 9, height = 5, dpi = 300)
print(p12)

# --------------------------------------------------
# Model 3: ETS benchmark + compare all three models
# --------------------------------------------------
fits_123 <- train |>
  model(
    SARIMA   = ARIMA(RSAFSNA_bil),
    DR_COVID = ARIMA(xreg_formula),
    ETS      = ETS(RSAFSNA_bil)
  )

fc_123 <- fits_123 |>
  forecast(new_data = test)

p123 <- autoplot(fc_123, sales) +
  labs(
    title = "Forecast Comparison (Test Period)",
    subtitle = "SARIMA vs Dynamic Regression (COVID-year dummy) vs ETS",
    x = "Year",
    y = "USD (billions)"
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1))

ggsave(fig_path("Figure_SARIMA_DR_ETS.png"), p123, width = 9, height = 5, dpi = 300)
print(p123)


##Now all 3 models are made and we have a plot which visualizes the (In-sample) performance of the models
##The baseline SARIMA model systematically underpredicts retail sales in the post-COVID period, reflecting its inability to account for structural changes.
##The dynamic regression model with a COVID-year dummy captures the initial shock but tends to overpredict sales thereafter.
##In contrast, the ETS benchmark adapts more flexibly to the evolving level of the series and provides forecasts that are visually closest to the realized data.

##Maybe explain,in work that 80% and 95% means that the expected forecast lies within these borders with probability of 80% resp. 95%.
fits_123 |>
  glance() |>
  select(.model, AICc, BIC)
## Model selection is conducted in-sample using AICc. 
## In-sample model comparison based on AICc indicates that the ETS model provides the poorest fit among the three specifications, 
## despite its strong out-of-sample((As we will see)) performance. This reflects the fact that ETS prioritizes smooth adaptation and forecast accuracy over tight in-sample likelihood fit.
## In contrast, the SARIMA and dynamic regression models are more flexible in capturing short-run dynamics, resulting in lower AICc values. 
## This highlights the well-known trade-off between in-sample fit and out-of-sample forecast performance.

######Jetzt nächster SChritt ist RMSE (out of sample) um zu sehen ob Bauchgefühl wirklich stimmt!!!!


##!!! In my opinion the forecast-models are not good, failing to approximate the Sales-level after the pandemic. But however this can be a good finding:
## Despite model extensions, forecast accuracy remains limited, reflecting the difficulty of predicting post-pandemic dynamics using standard time series models.
## The results demonstrate that while intervention models improve upon purely univariate benchmarks, even augmented time-series models face 
## substantial limitations when forecasting during periods of unprecedented economic disruption.

# --------------------------------------------------
# Out-of-sample forecast accuracy (RMSE / MAE)
# --------------------------------------------------

acc_oos <- fits_123 |>
  forecast(new_data = test) |>
  accuracy(test) |>
  dplyr::select(.model, RMSE, MAE, MAPE)

acc_oos

##Table X reports out-of-sample forecast accuracy for the competing models.
##The ETS benchmark achieves the lowest RMSE, MAE, and MAPE, indicating superior
##predictive performance over the test period.
##Both SARIMA and the dynamic regression model exhibit substantially larger forecast errors, 
##with the dynamic regression performing worst.

##Maybe define first shortly what these measures are
##################################################################
##################################################################
##Until here Code should run w/out problems##

# -----------------------------
# 8) DIAGNOSTICS (Person 3): Residual checks + Ljung-Box
# -----------------------------

# Figure 6: Residual diagnostics per model (time plot + ACF + histogram)
# This creates a faceted diagnostic plot set.
#p6 <- fits |>
#  gg_tsresiduals() +
#  labs(title = "Residual Diagnostics (Training Sample)")

#ggsave(fig_path("Figure6_residual_diagnostics.png"), p6, width = 9, height = 7, dpi = 300)
#not used, computed one for each model 

library(fabletools)
library(feasts)
library(dplyr)
#--------------------------------
# Figure 6: Residual diagnostics per model (time plot + ACF + histogram)
#--------------------------------

# p6 — SARIMA residual diagnostics
p6 <- fits |>
  select(SARIMA) |>
  gg_tsresiduals() +
  labs(title = "Residual diagnostics: SARIMA")

#print(p6)

ggsave(fig_path("Figure6_SARIMA_diagnostics.png"), p6, width = 9, height = 7, dpi = 300)

# p7 — Dynamic regression (COVID) residual diagnostics
p7 <- fits_123 |>
  select(DR_COVID) |>
  gg_tsresiduals() +
  labs(title = "Residual diagnostics: Dynamic regression (COVID)")
ggsave(fig_path("Figure7_DR_COVID_diagnostics.png"), p7, width = 9, height = 7, dpi = 300)
#print(p7)

# p8 — ETS residual diagnostics
p8 <- fits_123 |>
  select(ETS) |>
  gg_tsresiduals() +
  labs(title = "Residual diagnostics: ETS")
#print(p9)

ggsave(fig_path("Figure8_ETS_diagnostics.png"), p8, width = 9, height = 7, dpi = 300)




# Ljung-Box (Table 4): Are residuals jointly uncorrelated?
# Use lag 24 for monthly data (~2 years).

lb_table <- fits_123 |>
  augment() |>
  group_by(.model) |>
  features(.innov, ljung_box, lag = 24, dof = 0) |>
  select(.model, lb_stat, lb_pvalue)

lb_table
write_csv(lb_table, tab_path("Table4_LjungBox.csv"))


# Optional: coefficient robustness across dummy types (Table 5 optional)
fits_robust <- train |>
  model(
    DR_SHOCK = ARIMA(RSAFSNA_bil ~ covid_shock),
    DR_YEAR  = ARIMA(RSAFSNA_bil ~ covid_year),
    DR_STEP  = ARIMA(RSAFSNA_bil ~ covid_step)
  )

coef_robust <- fits_robust |>
  tidy() |>
  filter(str_detect(term, "covid_")) |>
  select(.model, term, estimate)

write_csv(coef_robust, tab_path("Table5_Robustness_DummyCoefficients.csv"))

# Optional: robustness accuracy across dummy types (Table 6 optional)
fc_robust <- fits_robust |>
  forecast(new_data = test)

acc_robust <- fc_robust |>
  accuracy(sales) |>
  select(.model, RMSE, MAE, MAPE, MASE)

write_csv(acc_robust, tab_path("Table6_Robustness_DummyAccuracy.csv"))

# -----------------------------
# 9) Optional: log-spec comparison (Appendix-level)
# -----------------------------
fits_log <- train |>
  model(
    SARIMA_LOG = ARIMA(log_RSAFSNA),
    DR_COVID_LOG = ARIMA(as.formula(paste0("log_RSAFSNA ~ ", dummy_var))),
    ETS_LOG = ETS(log_RSAFSNA)
  )

fc_log <- fits_log |>
  forecast(new_data = test)

acc_log <- fc_log |>
  accuracy(train |> bind_rows(test) |> select(observation_date, log_RSAFSNA) |> as_tsibble(index = observation_date)) |>
  select(.model, RMSE, MAE, MAPE, MASE)

write_csv(acc_log, tab_path("Appendix_LogSpec_Accuracy.csv"))

# -----------------------------
# 10) Summary printout (console)
# -----------------------------
cat("\n=== DONE ===\n")
cat("Outputs saved in:\n", out_dir, "\n\n")
cat("Main figures:\n",
    "- Figure1_series.png\n",
    "- Figure2_subseries.png\n",
    "- Figure3_STL.png\n",
    "- Figure4_forecasts_full.png\n",
    "- Figure5_forecasts_zoom_covid.png\n",
    "- Figure6_residual_diagnostics.png\n\n")

cat("Main tables:\n",
    "- Table1_ModelOverview.csv\n",
    "- Table2_InterventionCoefficient.csv\n",
    "- Table3_ForecastAccuracy.csv\n",
    "- Table4_LjungBox.csv\n\n")

cat("Optional robustness:\n",
    "- Table5_Robustness_DummyCoefficients.csv\n",
    "- Table6_Robustness_DummyAccuracy.csv\n",
    "- Appendix_LogSpec_Accuracy.csv\n\n")

