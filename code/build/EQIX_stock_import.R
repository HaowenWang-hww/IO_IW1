source("housekeeping.R")

# Download EQIX stock price data
eqix_stock <- tq_get("EQIX", from = "2010-01-01", to = Sys.Date())

# View structure
glimpse(eqix_stock)

# Plot
library(ggplot2)
eqix_stock %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(color = "steelblue") +
  labs(title = "Equinix (EQIX) Adjusted Closing Price", x = "Date", y = "Adjusted Price")

write.csv(eqix_stock, file = file.path(work_dir, "EQIX_stock_data.csv"), row.names = FALSE)
