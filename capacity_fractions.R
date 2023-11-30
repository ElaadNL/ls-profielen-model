library(tidyverse)
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
  #   from (datetime): The start date (make sure that is has the correct timezone!)
  #   to (datetime): The end date (make sure that is has the correct timezone!)
  #   by (string): The interval size
  #   path (string): Path to write the CSV to
  #   times (list[list]): A list of named lists containing the floor definitions
  #   write (bool): Whether to write the CSV file
  
  # Create datetimes and add two days on each end as padding
  # This makes sure interpolation happens correctly on the edges
  date_time <- seq(from=from - days_in_seconds(1), to=to + days_in_seconds(1), by=by)
  df <- tibble(date_time=date_time) %>% mutate(time=format(date_time, format="%H:%M:%S"))
  
  # Loop over each of the floor-times definitions
  for (floor_times in times) {
    floor_start <- hour_to_timestring(floor_times$floor_start)
    floor_end <- hour_to_timestring(floor_times$floor_end)
    interval_size <- time_difference_from_string(by)
    intervals_per_hour <- 3600 / as.double(interval_size, unit="secs")
    
    pre_slope <- floor_times$pre_slope
    post_slope <- floor_times$post_slope
    
    # Determine the interval before the floor start at which the pre slope starts
    if (is.null(pre_slope)) pre_slope <- 1
    else pre_slope <- floor(pre_slope * intervals_per_hour)
    
    # Determine the interval after the floor end at which the post slope ends
    if (is.null(post_slope)) post_slope <- 1
    else post_slope <- floor(post_slope * intervals_per_hour)
    
    # Set only the values at the floor start/end and pre/post slope times
    # The missing values will be interpolated
    df[df$time == floor_start | df$time == floor_end, "value"] <- 0.0
    
    # Set the pre slope values which are shifted `pre_slope` intervals to the front
    selectors <- append((df$time == floor_start)[(pre_slope + 1):nrow(df)], rep(FALSE, pre_slope))
    df[selectors, "value"] <- 1.0
    
    # Set the post slope values which are shifted `post_slope` intervals to the back
    selectors <- append(rep(FALSE, post_slope), (df$time == floor_end)[1:(nrow(df) - post_slope)])
    df[selectors, "value"] <- 1.0
  }
  
  df <- df %>%
    # Interpolate the values between the pre slope start, floor start, floor end, and post slope end
    mutate(value=na.approx(value, rule=2)) %>%
    arrange(date_time) %>%
    select(date_time, value) %>%
    # Cut off the extra days added at the ends
    filter(date_time >= from & date_time <= to) %>%
    # Filter out double dates
    group_by(date_time) %>%
    filter(n() == 1) %>%
    ungroup()
  
  if (write) write_to_csv(df, path)
  return (df)
}
