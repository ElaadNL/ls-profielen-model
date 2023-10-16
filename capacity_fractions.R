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
  ### Convert a an interval string (e.g. "15 mins") to a time difference object
  difference_parts <- strsplit(s, " ")
  numeric_value <- as.numeric(difference_parts[[1]][1])
  units <- tolower(difference_parts[[1]][2])
  time_difference <- as.difftime(numeric_value, units=units)
  return (time_difference)
}

hour_to_timestring <- function(hour) {
  ### Convert an hour (e.g. 3) to a time string (e.g. "03:00:00")
  datetime <- as.POSIXct(paste(hour), format = "%H", tz = "UTC")
  timestring <- format(datetime, format="%H:%M:%S")
  return (timestring)
}

timestring_difference <- function(a, b) {
  ### Determine the difference between two time strings
  #
  # It is assumed that `a` comes before `b`. This allows us to determine the difference between,
  # for instance, 17:00:00 and 03:00:00 the next day.
  a_datetime <- as.POSIXct(a, format = "%H:%M:%S", tz = "UTC")
  b_datetime <- as.POSIXct(b, format = "%H:%M:%S", tz = "UTC")
  if (b_datetime < a_datetime) b_datetime <- b_datetime + 24 * 3600
  return (difftime(b_datetime,a_datetime))
}

add_difference_to_timestring <- function(ts, diff) {
  ### Add a time difference object to a time string
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
  times=list(list(floor_start=17, floor_end=23, pre_slope=NULL, post_slope=7)),
  write=FALSE
) {
  ### Create capacity fractions in line with definitions from "Netbewust laden"
  #
  # The netbewust laden capacity fractions are defined by a list of "floors". Each floor is defined
  # by a start and end time between which the allowed additional capacity is 0. Everywhere else the
  # allowed capacity is 1. A pre slope and post slope can be optionally defined to enable a more
  # gradual decrease/increase of the allowed capacity. The configuration is the repeated for each
  # day between the `from` and `to` date.
  #
  # Args
  #   from (datetime): The start date
  #   to (datetime): The end date
  #   by (string): The interval size
  #   path (string): Path to write the CSV to
  #   write (bool): Whether to write the CSV file
  #
  date_time <- seq(from=from, to=to, by=by)
  df <- tibble(date_time=date_time) %>% mutate(time=format(date_time, format="%H:%M:%S"))

  # Set up a data frame for building the fractions for a single day
  # This data frame will be repeated for each day in the year
  one_day <- df %>%
    # We add some days as padding to we can later cut out a single day
    # This solves issues with extrapolation at the edges of the day
    filter(date_time < from + days_in_seconds(3)) %>%
    mutate(time=format(date_time, format="%H:%M:%S"))
  
  # Loop over each of the floor-times definitions
  for (floor_times in times) {
    floor_start <- hour_to_timestring(floor_times$floor_start)
    floor_end <- hour_to_timestring(floor_times$floor_end)
    interval_diff <- time_difference_from_string(by)
    
    pre_slope <- floor_times$pre_slope
    post_slope <- floor_times$post_slope
    
    # Determine the time at which the pre slope starts
    if (is.null(pre_slope)) {
      # If no pre slope is defined we define the pre slope to be one interval before the floor start
      pre_slope <- add_difference_to_timestring(floor_start, -interval_diff)
    } else {
      pre_slope_difference_string <- sprintf("%s hours", pre_slope)
      diff <- time_difference_from_string(pre_slope_difference_string)
      pre_slope <- add_difference_to_timestring(floor_start, -diff)
    }
    
    # Determine the time at which the post slope ends
    if (is.null(post_slope)) {
      # If the post slope is not defined it is set to end one interval after the floor ends
      post_slope <- add_difference_to_timestring(floor_end, interval_diff)
    } else {
      post_slope_difference_string <- sprintf("%s hours", post_slope)
      diff <- time_difference_from_string(post_slope_difference_string)
      post_slope <- add_difference_to_timestring(floor_end, diff)
    }
    
    # Set only the values at the floor start/end and pre/post slope times
    # The missing values will be interpolated
    one_day[one_day$time == pre_slope | one_day$time == post_slope, "value"] <- 1.0
    one_day[one_day$time == floor_start | one_day$time == floor_end, "value"] <- 0.0
  }
  
  one_day <- one_day %>%
    # Interpolate the values between the pre slope start, floor start, floor end, and post slope end
    mutate(value=na.approx(value, rule=2)) %>%
    # Cut out a single day
    filter(date_time >= from + days_in_seconds(1) & date_time < from + days_in_seconds(2)) %>%
    select(time, value)
  
  # Create a new data frame with the single day repeated through the year
  df <- merge(df, one_day, by="time") %>%
    select(date_time, value) %>%
    arrange(date_time)
  
  if (write) write_to_csv(df, path)
  return (df)
}
