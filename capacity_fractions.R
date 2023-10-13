library(dplyr)
library(lubridate)
library(rlist)
library(zoo)


write_to_csv <- function(df, path) {
  write.table(df, file=path, sep=",", row.names = FALSE, quote = FALSE)
}


days_in_seconds <- function(n) {
  return (n * 24 * 3600)
}


time_difference_from_string <- function(s) {
  difference_parts <- strsplit(s, " ")
  numeric_value <- as.numeric(difference_parts[[1]][1])
  units <- tolower(difference_parts[[1]][2])
  time_difference <- as.difftime(numeric_value, units=units)
  return (time_difference)
}

hour_to_timestring <- function(hour) {
  datetime <- as.POSIXct(paste(hour), format = "%H", tz = "UTC")
  timestring <- format(datetime, format="%H:%M:%S")
  return (timestring)
}

timestring_difference <- function(a, b) {
  a_datetime <- as.POSIXct(a, format = "%H:%M:%S", tz = "UTC")
  b_datetime <- as.POSIXct(b, format = "%H:%M:%S", tz = "UTC")
  while (b_datetime < a_datetime) b_datetime <- b_datetime + 24 * 3600
  return (difftime(b_datetime,a_datetime))
}

add_difference_to_timestring <- function(ts, diff) {
  dt <- as.POSIXct(ts, format = "%H:%M:%S", tz = "UTC")
  new_dt <- dt + diff
  new_ts <- format(new_dt, format="%H:%M:%S")
  return (new_ts)
}


create_capacity_fractions_netbewust_laden <- function(
  from,
  to,
  by="15 mins",
  path="capacity_fractions_netbewust_laden.csv",
  times=list(list(floor_start=17, floor_end=23, pre_slope=1, post_slope=7)),
  write=FALSE
) {
  date_time <- seq(from=from, to=to, by=by)
  df <- tibble(date_time=date_time) %>% mutate(time=format(date_time, format="%H:%M:%S"))

  one_day <- df %>%
    filter(date_time < from + days_in_seconds(3)) %>%
    mutate(time=format(date_time, format="%H:%M:%S"))
  
  for (floor_times in times) {
    floor_start <- hour_to_timestring(floor_times$floor_start)
    floor_end <- hour_to_timestring(floor_times$floor_end)
    interval_diff <- time_difference_from_string(by)
    
    pre_slope <- floor_times$pre_slope
    post_slope <- floor_times$post_slope
    
    if (is.null(pre_slope)) {
      pre_slope <- add_difference_to_timestring(floor_start, -interval_diff)
    } else {
      pre_slope_difference_string <- sprintf("%s hours", pre_slope)
      diff <- time_difference_from_string(pre_slope_difference_string)
      pre_slope <- add_difference_to_timestring(floor_start, -diff)
    }
    
    if (is.null(post_slope)) {
      post_slope <- add_difference_to_timestring(floor_end, interval_diff)
    } else {
      post_slope_difference_string <- sprintf("%s hours", post_slope)
      diff <- time_difference_from_string(post_slope_difference_string)
      post_slope <- add_difference_to_timestring(floor_end, diff)
    }
    
    one_day[one_day$time == floor_start | one_day$time == floor_end, "value"] <- 0.0
    one_day[one_day$time == pre_slope | one_day$time == post_slope, "value"] <- 1.0
    print(one_day[one_day$time == post_slope,])
  }
  
  one_day <- one_day %>%
    mutate(value=na.approx(value, rule=2)) %>%
    filter(date_time >= from + days_in_seconds(1) & date_time < from + days_in_seconds(2)) %>%
    select(time, value)
  
  df <- merge(df, one_day, by="time") %>%
    select(date_time, value) %>%
    arrange(date_time)
  
  if (write) write_to_csv(df, path)
  return (df)
}
