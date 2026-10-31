# =============================================================================
# EC 422 – Problem Set 3
# Diego Carpenter
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# Part 2 – Getting Comfortable with Time-Series Data in R
# -----------------------------------------------------------------------------

# 2.1 Data folder (only needs to happen once)
if (!dir.exists("data")) dir.create("data")
# Download the FRED CSV with RSAFS + RSAFSNA, move it into data/,
# and rename it retail_sales_raw.csv

# 2.2a Read the CSV and let read_csv() guess the column types
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

# 2.2b Re-read with explicit column types (and FRED's "." as missing)
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)
head(retail_raw)

# 2.3a Reshape from wide to long
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# 2.4a Build the tsibble
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

# 2.4b Inspect and check for gaps
head(retail_ts)
has_gaps(retail_ts)

# 2.5a Canadian gas production
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5b Working copy + gap check
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)
head(can_gas)

# 2.6a Filtering by date
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# 2.6b Extracting parts of a date
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6c Date arithmetic
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# 2.6d Monthly -> quarterly (drop the incomplete 2005 Q1)
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# 2.7a Quick plot of retail sales
retail_ts |> autoplot(sales)

# 2.7b Add ggplot2 layers
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# 2.7c Canadian gas production plot
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Monthly Natural Gas Production in Canada",
    subtitle = "January 1960 – February 2005",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 2.7d One extra layer: a smoothed trend line + comma-formatted y-axis
retail_ts |>
  autoplot(sales) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", linewidth = 0.6) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "U.S. Retail Sales with a Smoothed Trend",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()


# Part 3 – Data Transformations

# 3.1b Log of gas production
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Gas Production",
    x = NULL, y = "log(Volume)"
  ) +
  theme_minimal()

# 3.1c Log vs. level for NSA retail sales
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (Level)", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (Log)", x = NULL, y = "log(sales)") +
  theme_minimal()

# 3.2a Growth rates two ways
can_gas <- can_gas |>
  mutate(
    growth_direct  = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gas Production: Month-over-Month Growth (Direct)",
    x = NULL, y = "Growth rate (%)"
  ) +
  theme_minimal()

# Gap between the exact and log-difference growth rates
can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Exact Growth Rate Minus Log-Difference Approximation (Absolute)",
    x = NULL, y = "Percentage points"
  ) +
  theme_minimal()

summary(can_gas$growth_gap)
summary(abs(can_gas$growth_direct))

# 3.3 SA vs. NSA growth rates
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# Standard deviation of each growth series (backs up the "which is noisier" answer)
retail_ts |>
  as_tibble() |>
  group_by(series_id) |>
  summarize(sd_growth = sd(growth_mom, na.rm = TRUE))

# -----------------------------------------------------------------------------
# Part 4 – Seeing, Decomposing, and Modeling Seasonality
# -----------------------------------------------------------------------------

# 4.1a Season plot: NSA retail sales
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1b Season plot: SA retail sales
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1c Season plot: log Canadian gas
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production (Log)",
    y = "log(Volume)"
  ) +
  theme_minimal()

# 4.2 Subseries plot: log Canadian gas
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# 4.3a X-13ARIMA-SEATS on the NSA retail series
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# Average seasonal factor by calendar month (to find the peak month)
retail_nsa_x13 |>
  as_tibble() |>
  mutate(cal_month = month(Month, label = TRUE)) |>
  group_by(cal_month) |>
  summarize(avg_seasonal = mean(seasonal)) |>
  arrange(desc(avg_seasonal))

# 4.3b Compare my SA series to the Census Bureau's
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

# Summary of the differences and the biggest misses
summary(sa_compare$pct_diff)
sa_compare |> arrange(desc(abs(pct_diff))) |> head(10)

# 4.4a Linear trend + seasonal dummies on log gas production
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# 4.4b Fitted vs. actual
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4c Residual diagnostics
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# -----------------------------------------------------------------------------
# Part 5 – A First Forecast Evaluation
# -----------------------------------------------------------------------------

# 5.1a Expanding-window TS-CV, 1-step-ahead forecasts
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# First and last forecast dates that are actually evaluated
gas_cv_fc |>
  as_tibble() |>
  filter(Month <= max(can_gas$Month)) |>
  summarize(first = min(Month), last = max(Month), n_per_model = n() / 2)

# 5.2a Diebold–Mariano test (absolute loss)
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# 5.3a Rolling window of 240 months
gas_cv_fc_roll <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_roll |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
