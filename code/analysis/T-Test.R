# === Load environment and libraries ===
source("housekeeping.R")

library(tidyverse)
library(broom)

# === Step 1: Load and clean data ===
mna_data <- read.csv(file.path(work_dir, "EQIX_M&A_clean.csv")) %>%
  mutate(
    Announcement.Date = as.Date(Announcement.Date, format = "%d-%b-%y"),
    Announcement.Date = if_else(year(Announcement.Date) < 2000,
                                Announcement.Date + years(100),
                                Announcement.Date),
    Deal.Value = as.numeric(gsub(",", "", `Deal.Value..USD.Million.`))
  )

stock_data <- read.csv(file.path(work_dir, "EQIX_stock_data.csv")) %>%
  mutate(date = as.Date(date))

# === Step 2: Run 60-day regressions for each M&A event ===
event_window <- 60
regression_summary <- list()
regression_lines <- list()

for (i in 1:nrow(mna_data)) {
  event_date <- mna_data$Announcement.Date[i]
  target <- mna_data$Target.Company[i]
  
  post_data <- stock_data %>%
    filter(date > event_date, date <= event_date + event_window) %>%
    arrange(date) %>%
    mutate(day = as.numeric(date - event_date))
  
  if (nrow(post_data) >= 3) {
    model <- lm(adjusted ~ day, data = post_data)
    
    regression_summary[[i]] <- tidy(model) %>%
      mutate(Target = target)
    
    regression_lines[[i]] <- augment(model) %>%
      mutate(Target = target)
  }
}

summary_df <- bind_rows(regression_summary)
lines_df <- bind_rows(regression_lines)

# === Step 3: Tag slope direction ===
slope_info <- summary_df %>%
  filter(term == "day") %>%
  select(Target, slope = estimate)

lines_with_slope <- lines_df %>%
  left_join(slope_info, by = "Target") %>%
  mutate(Slope_Direction = ifelse(slope > 0, "Positive", "Negative"))

# === Step 4: Normalize prices and extract Day 60 values ===
price_day60 <- lines_with_slope %>%
  group_by(Target) %>%
  mutate(norm_price = adjusted / first(adjusted)) %>%
  filter(day == max(day)) %>%
  ungroup() %>%
  select(Target, norm_price, Slope_Direction)

# === Step 5: Run t-test ===
t_test_result <- t.test(norm_price ~ Slope_Direction, data = price_day60)
print(t_test_result)

# === Step 6: Visualize and export ===
library(ggplot2)

plot_day60 <- ggplot(price_day60, aes(x = Slope_Direction, y = norm_price, fill = Slope_Direction)) +
  geom_boxplot(alpha = 0.7, width = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  labs(title = "Normalized EQIX Stock Price at Day 60 by Slope Direction",
       subtitle = paste("T-test p-value:", signif(t_test_result$p.value, 3)),
       x = "Slope Group", y = "Normalized Price (Day 60)") +
  scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "tomato")) +
  theme_minimal()

print(plot_day60)

ggsave(file.path(work_dir, "day60_boxplot_by_slope.png"), plot_day60, width = 7, height = 5)