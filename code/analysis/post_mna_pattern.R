# Load environment
source("housekeeping.R")

# === Load and clean M&A + stock data ===
mna_data <- read.csv(file.path(work_dir, "EQIX_M&A_clean.csv")) %>%
  mutate(
    Announcement.Date = as.Date(Announcement.Date, format = "%d-%b-%y"),
    Announcement.Date = if_else(year(Announcement.Date) < 2000, Announcement.Date + years(100), Announcement.Date),
    Deal.Value = as.numeric(gsub(",", "", `Deal.Value..USD.Million.`))
  )

stock_data <- read.csv(file.path(work_dir, "EQIX_stock_data.csv")) %>%
  mutate(date = as.Date(date))

# === Regression on 5-day post-announcement window ===
event_window <- 25          
forecast_horizon <- 5       

# Initialize lists to store output and model objects
regression_summary <- list()
regression_lines   <- list()
models_list        <- list()  # New list to store model objects and target info

for (i in 1:nrow(mna_data)) {
  event_date <- mna_data$Announcement.Date[i]
  target <- mna_data$Target.Company[i]
  size_class <- mna_data$Size.Class[i]
  
  post_data <- stock_data %>%
    filter(date > event_date, date <= event_date + event_window) %>%
    arrange(date) %>%
    mutate(day = as.numeric(date - event_date))
  
  if (nrow(post_data) >= 3) {
    model <- lm(adjusted ~ day, data = post_data)
    
    # Store the model along with target info for later use in computing R²
    models_list[[i]] <- list(model = model, Target = target)
    
    regression_summary[[i]] <- broom::tidy(model) %>%
      mutate(Target = target, Size.Class = size_class)
    
    regression_lines[[i]] <- broom::augment(model) %>%
      mutate(Target = target, Size.Class = size_class)
  }
}

summary_df <- bind_rows(regression_summary)
lines_df   <- bind_rows(regression_lines)

# === Tag positive vs negative slope ===
lines_with_slope <- lines_df %>%
  left_join(
    summary_df %>%
      filter(term == "day") %>%
      select(Target, slope = estimate) %>%
      mutate(Slope_Direction = ifelse(slope > 0, "Positive", "Negative")),
    by = "Target"
  )

# === Plot regression lines with labels ===
label_df <- lines_with_slope %>%
  group_by(Target) %>%
  filter(day == max(day)) %>%
  ungroup()

plot_labeled <- ggplot(lines_with_slope, aes(x = day, y = adjusted, group = Target, color = Slope_Direction)) +
  geom_line(alpha = 0.8, linewidth = 1) +
  geom_text_repel(data = label_df, aes(label = Target), size = 3, max.overlaps = 50, segment.alpha = 0.3, box.padding = 0.3) +
  facet_wrap(~ Slope_Direction, scales = "free_y") +
  labs(title = "EQIX Stock Price Patterns After M&A – Labeled by Target Company",
       subtitle = "Each line = 5-day regression",
       x = "Days Since Announcement", y = "Adjusted Close Price") +
  theme_minimal() +
  scale_color_manual(values = c("Positive" = "steelblue", "Negative" = "tomato"))

ggsave(file.path(output_dir, "plot_labeled_by_slope.png"), plot_labeled, width = 10, height = 6)

# === Normalize, average, and forecast for the Positive group ===
positive_norm <- lines_with_slope %>%
  filter(Slope_Direction == "Positive") %>%
  group_by(Target) %>%
  mutate(norm_price = adjusted / first(adjusted)) %>%
  ungroup()

avg_pos <- positive_norm %>%
  group_by(day) %>%
  summarise(avg_price = mean(norm_price, na.rm = TRUE)) %>%
  mutate(group = "Positive")

write.csv(avg_pos, file = file.path(work_dir, "avg_pos.csv"), row.names = FALSE)

fit_pos <- auto.arima(ts(avg_pos$avg_price))
forecast_pos <- forecast(fit_pos, h = forecast_horizon)

plot_pos <- autoplot(forecast_pos) +
  ggtitle("Forecast: Normalized Avg EQIX Price After M&A (Positive Events)") +
  ylab("Normalized Price")

ggsave(file.path(output_dir, "forecast_positive.png"), plot_pos, width = 8, height = 5)

# === Normalize, average, and forecast for the Negative group ===
negative_norm <- lines_with_slope %>%
  filter(Slope_Direction == "Negative") %>%
  group_by(Target) %>%
  mutate(norm_price = adjusted / first(adjusted)) %>%
  ungroup()

avg_neg <- negative_norm %>%
  group_by(day) %>%
  summarise(avg_price = mean(norm_price, na.rm = TRUE)) %>%
  mutate(group = "Negative")

write.csv(avg_neg, file = file.path(work_dir, "avg_neg.csv"), row.names = FALSE)

fit_neg <- auto.arima(ts(avg_neg$avg_price))
forecast_neg <- forecast(fit_neg, h = forecast_horizon)

plot_neg <- autoplot(forecast_neg) +
  ggtitle("Forecast: Normalized Avg EQIX Price After M&A (Negative Events)") +
  ylab("Normalized Price")

ggsave(file.path(output_dir, "forecast_negative.png"), plot_neg, width = 8, height = 5)

# === Combined Comparison Plot: Actual vs Forecast ===

# Prepare actual data (days 1–5) with a 'type' indicator
actual_pos <- avg_pos %>% mutate(type = "Actual", group = "Positive")
actual_neg <- avg_neg %>% mutate(type = "Actual", group = "Negative")

# Convert forecast objects to data frames: forecast days will be 6 to 10
forecast_pos_df <- data.frame(
  day       = (event_window + 1):(event_window + forecast_horizon),
  avg_price = as.numeric(forecast_pos$mean),
  lower     = as.numeric(forecast_pos$lower[, 2]),
  upper     = as.numeric(forecast_pos$upper[, 2]),
  type      = "Forecast",
  group     = "Positive"
)

forecast_neg_df <- data.frame(
  day       = (event_window + 1):(event_window + forecast_horizon),
  avg_price = as.numeric(forecast_neg$mean),
  lower     = as.numeric(forecast_neg$lower[, 2]),
  upper     = as.numeric(forecast_neg$upper[, 2]),
  type      = "Forecast",
  group     = "Negative"
)

# Combine actual and forecast data for both groups
combined_all <- bind_rows(actual_pos, forecast_pos_df, actual_neg, forecast_neg_df)

# Create the comparison plot with distinct lines for Actual and Forecast
plot_comparison <- ggplot(combined_all, aes(x = day, y = avg_price, color = type, linetype = type)) +
  geom_line(size = 1.2) +
  geom_ribbon(data = combined_all %>% filter(type == "Forecast"),
              aes(x = day, ymin = lower, ymax = upper, fill = type),
              alpha = 0.2, inherit.aes = FALSE) +
  facet_wrap(~group, scales = "free_y") +
  labs(title = "ARIMA Forecast vs Actual Normalized Avg EQIX Price by Slope Group",
       subtitle = "Solid lines = Actual normalized average, Dashed lines = ARIMA forecast with 95% CI",
       x = "Days Since Announcement", y = "Normalized Price") +
  scale_color_manual(values = c("Actual" = "steelblue", "Forecast" = "tomato")) +
  scale_fill_manual(values = c("Forecast" = "tomato")) +
  theme_minimal()

ggsave(file.path(output_dir, "arima_forecast_vs_actual_by_group.png"), plot_comparison, width = 10, height = 6)

# === Regression Stats and R-squared Calculation ===

# Retrieve regression statistics (slope, p-values, etc.)
regression_stats <- summary_df %>%
  filter(term == "day") %>%
  select(Target, estimate, std.error, statistic, p.value) %>%
  rename(
    Slope     = estimate,
    Std_Error = std.error,
    t_stat    = statistic,
    p_value   = p.value
  )

# *** FIX: Filter out NULL entries from models_list ***
models_list <- models_list[!sapply(models_list, is.null)]

# Use stored model objects to compute R-squared for each event
r2_by_target <- purrr::map_df(models_list, function(x) {
  data.frame(Target = x$Target, R_squared = summary(x$model)$r.squared)
})

# Combine slope and R² statistics
regression_summary_table <- regression_stats %>%
  left_join(r2_by_target, by = "Target") %>%
  arrange(desc(Slope))

print(regression_summary_table)
write.csv(regression_summary_table, file.path(output_dir, "regression_results_by_target.csv"), row.names = FALSE)

# Save the fitted ARIMA models to file
saveRDS(fit_pos, file = file.path(output_dir, "arima_fit_positive.rds"))
saveRDS(fit_neg, file = file.path(output_dir, "arima_fit_negative.rds"))

# *********************************************
# 1. SETTINGS: Define training and forecast horizon
# *********************************************
# For instance, you might choose a training period of 5 days
# and a forecast horizon of 5 days. Then the full period is 10 days.
# (You can change these numbers, and nothing is hard-coded later.)
training_window <- 20       # Use days 1 to 5 to fit ARIMA
forecast_horizon <- 5      # Forecast the next 5 days
full_period <- training_window + forecast_horizon  # Total period for comparison

# *********************************************
# 2. FUNCTION TO PROCESS ONE GROUP (Positive or Negative)
# *********************************************
create_series_for_test <- function(group_data, group_name, training_window, forecast_horizon) {
  # Normalize the data for each event by dividing by its first observed price.
  # group_data is expected to have at least:
  #   - day (numeric: days since announcement)
  #   - adjusted (the stock's adjusted price)
  #   - Target (identifier per event)
  group_data <- group_data %>%
    arrange(day) %>%
    group_by(Target) %>%
    mutate(norm_price = adjusted / first(adjusted)) %>%
    ungroup()
  
  # --- Compute the actual average normalized price for each day ---
  # We use only data up to the full period.
  actual_df <- group_data %>%
    filter(day <= full_period) %>% 
    group_by(day) %>%
    summarise(avg_price = mean(norm_price, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(type = "Actual", group = group_name)
  
  # --- Build the training series from days 1 to training_window ---
  train_df <- group_data %>%
    filter(day <= training_window) %>% 
    group_by(day) %>%
    summarise(avg_price = mean(norm_price, na.rm = TRUE)) %>%
    ungroup()
  
  # Ensure there is enough data for training.
  if(nrow(train_df) < 2){
    stop("Not enough training data for group: ", group_name)
  }
  
  # Fit ARIMA on the training period.
  ts_train <- ts(train_df$avg_price, start = min(train_df$day), frequency = 1)
  fit <- auto.arima(ts_train)
  
  # Forecast for the forecast_horizon days.
  fc <- forecast(fit, h = forecast_horizon)
  
  # Build the forecast data frame:
  forecast_df <- tibble(
    day       = seq(training_window + 1, training_window + forecast_horizon),
    avg_price = as.numeric(fc$mean),
    lower     = as.numeric(fc$lower[,2]),  # 95% CI lower bound
    upper     = as.numeric(fc$upper[,2]),  # 95% CI upper bound
    type      = "Forecast",
    group     = group_name
  )
  
  # Return both series as a list.
  list(actual = actual_df, forecast = forecast_df)
}

# *********************************************
# 3. PREPARE DATA PER GROUP
# *********************************************
# Assume your dataset 'lines_with_slope' is already available and contains:
#    - day, adjusted, Target, and Slope_Direction (with values "Positive" or "Negative")
#
# Filter for each group. (Adjust the variable names if needed.)
positive_data <- lines_with_slope %>%
  filter(Slope_Direction == "Positive")

negative_data <- lines_with_slope %>%
  filter(Slope_Direction == "Negative")

# Build the series for each group.
series_pos <- create_series_for_test(positive_data, "Positive", training_window, forecast_horizon)
series_neg <- create_series_for_test(negative_data, "Negative", training_window, forecast_horizon)

# *********************************************
# 4. COMBINE THE SERIES
# *********************************************
# Actual data: aggregated observations (for days 1 to full_period)
actual_all <- bind_rows(series_pos$actual, series_neg$actual)

# Forecast data: the ARIMA-based forecast for the forecast period
forecast_all <- bind_rows(series_pos$forecast, series_neg$forecast)

# *********************************************
# 5. PLOT THE FOUR LINES
# *********************************************
# The plot will show:
# • Positive–Actual (solid line)
# • Positive–Forecast (dashed line)
# • Negative–Actual (solid line)
# • Negative–Forecast (dashed line)
plot_four_lines <- ggplot() +
  geom_line(data = actual_all,
            aes(x = day, y = avg_price, color = group, group = interaction(group, type)),
            size = 1.2) +
  geom_line(data = forecast_all,
            aes(x = day, y = avg_price, color = group, group = interaction(group, type)),
            size = 1.2, linetype = "dashed") +
  # Optionally, add forecast confidence intervals as ribbons:
  geom_ribbon(data = forecast_all,
              aes(x = day, ymin = lower, ymax = upper, fill = group),
              alpha = 0.2) +
  scale_color_manual(values = c("Positive" = "steelblue", "Negative" = "tomato")) +
  scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "tomato")) +
  labs(
    title = "Normalized Average EQIX Price: Actual vs. Forecast",
    subtitle = paste("Training period =", training_window, "days; Forecast horizon =", forecast_horizon, "days"),
    x = "Days Since Announcement",
    y = "Normalized Price",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()

# Display the plot.
print(plot_four_lines)

# Optionally, save the plot.
ggsave(file.path(output_dir, "four_lines_actual_vs_forecast_test.png"), plot_four_lines, width = 10, height = 6)