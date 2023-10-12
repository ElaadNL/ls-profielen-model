library(dplyr)
library(zoo)


write_to_csv <- function(df, path) {
  write.table(df, file=path, sep=",", row.names = FALSE, quote = FALSE)
}


days_in_seconds <- function(n) {
  return (n * 24 * 3600)
}


create_capacity_fractions_netbewust_laden <- function(
  from,
  to,
  by="15 mins",
  path="capacity_fractions_netbewust_laden.csv"
) {
  floor_window_start = 17
  floor_window_end = 23
  slope_end = 6
  slope_duration = 24 - floor_window_end + slope_end
  
  date_time <- seq(from=from, to=to, by=by)
  df <- tibble(date_time=date_time) %>% mutate(time=format(date_time, format="%H:%M:%S"))

  one_day <- df %>%
    filter(date_time < from + days_in_seconds(3)) %>%
    mutate(hour=lubridate::hour(date_time)) %>%
    mutate(section="normal") %>%
    mutate(section=ifelse(hour >= floor_window_start & hour < floor_window_end, "floor", section)) %>%
    mutate(section=ifelse(hour >= floor_window_end | hour < slope_end, "slope", section)) %>%
    mutate(value=ifelse(section=="floor", 0.0, ifelse(section=="normal", 1.0, NA))) %>%
    mutate(value=na.approx(value, rule=2)) %>%
    filter(date_time >= from + days_in_seconds(1) & date_time < from + days_in_seconds(2)) %>%
    mutate(time=format(date_time, format="%H:%M:%S")) %>%
    select(time, value)
  
  df <- merge(df, one_day, by="time") %>%
    select(date_time, value) %>%
    arrange(date_time)
  
  write_to_csv(df, path)
  return (df)
}
