#Load data
mna_data <- read.csv(file.path(work_dir, "EQIX_M&A_clean.csv")) %>%
  mutate(
    Announcement.Date = as.Date(Announcement.Date, format = "%d-%b-%y"),
    Announcement.Date = if_else(year(Announcement.Date) < 2000,
                                Announcement.Date + years(100),
                                Announcement.Date),
    `Deal.Value..USD.Million.` = as.numeric(gsub(",", "", `Deal.Value..USD.Million.`))  # <- This line is key
  )

stock_data <- read.csv(file.path(work_dir, "EQIX_stock_data.csv")) %>%
  mutate(date = as.Date(date))

