library(data.table)
library(dplyr)
library(lubridate)
library(MALDIquant)

source("capacity_fractions.R")

### Historical session demand aggregated on weekly level ###
# Description
# Depending on whether we simulate sessions for EVs or charging stations (CPs) we aggregate
# on year, actual_week, and card_id or cs_id
#
# Args
#   sessions (dataframe): dataframe containing session data
#   profile_type (string): profile type (Electrical Vehicle or Charging Station)
#
# Returns
#   sessions_week (dataframe): dataframe containing session data aggregated on a weekly level
get_sessions_week <- function(sessions, profile_type) {
  if (!("cs_id" %in% colnames(sessions))) {
    sessions$cs_id <- sessions$cp_id
  }
  
  if (profile_type == "Electric Vehicle") {
    sessions_week <- sessions %>%
      dplyr::group_by(
        year,
        actual_week,
        card_id
      ) %>%
      dplyr::summarise(
        energy_week = sum(energy, na.rm = TRUE)
      )
  } else {
    sessions_week <- sessions %>%
      dplyr::group_by(
        year,
        actual_week,
        cs_id
      ) %>%
      dplyr::summarise(
        energy_week = sum(energy, na.rm = TRUE)
      )
  }
  
  return (sessions_week)
}


### Sample from annual mileage distribution ###
# Description
# This function samples from an annual mileage distribution table, which is based on a table from CBS (2015)
# After creating a cumulative density function (CDF) it corrects for the proportion of business drivers
# in relation to private drivers, which depends on the year. In 2050 we expect the EV population to be similar to
# the total population of drivers in 2015
#
# Args
#   n_runs (integer): number of simulation runs -> number of profiles
#   year (integer): year
#
# Returns
#   mileage_samples (double[]): vector containing annual mileages samples 
sample_annual_mileage <- function(n_runs, year) {
  # Read annual mileage distribution table
  annual_mileage <- read.csv("data/verdeling_jaarkilometrage.csv", sep = ";", dec = ",")
  
  # Calculate cumulative density function (CDF) based on distribution table
  mileage_density <- density(annual_mileage$km)
  mileage_density$y <- mileage_density$y[mileage_density$x >= 0]
  mileage_density$x <- mileage_density$x[mileage_density$x >= 0]
  mileage_cdf <- cumsum(mileage_density$y) * diff(mileage_density$x[1:2])
  
  # Sample annual mileages from the CDF, for the number of simulation runs (n_runs)
  uniform_samples <- runif(n_runs, min = min(mileage_cdf), max = max(mileage_cdf))
  mileage_samples <- approxfun(mileage_cdf, mileage_density$x)(uniform_samples)
  
  # Correct for the proportion of business drivers in relation to private drivers
  mileage_ratios <- read.csv("data/jaar_kms_ratios.csv", sep = ";", dec = ",")
  mileage_samples <- mileage_samples * mileage_ratios[mileage_ratios$year == year,]$ratio
  
  return (mileage_samples)
}


### Sample annual energy demands ###
# Description
# This function samples the annual energy demand on EV-level or CS-level
# First the function calls sample_annual_mileage(), whereafter the annual mileage is being multiplied with
# the energy efficiency and location mix.
# When the annual demand is being calculated on CP level we also multiply with the EV/CS-ratio
#
# Args
#   profile_type (string): profile type (Electrical Vehicle or Charging Station)
#   charging_location (string): charging location type (public, home, work)
#   n_runs (integer): number of simulation runs
#   year (integer): year
#   ev_cs_ratio (integer): EV/CS ratio (default 3)
#
# Returns
#   annual_energy_demand (double[]): vector containing annual energy demand samples 
sample_annual_endemand <- function(profile_type, charging_location, n_runs, year, ev_cs_ratio = 3) {
  # General mobility assumptions
  annual_mileage <- sample_annual_mileage(n_runs, year)
  energy_efficiency <- 0.2
  
  location_mix = c("home" = .19, "public" = .55, "work" = .17, "fast" = .9)
  
  # Multiply annual mileage with the location mix, energy efficiency and EV/CS-ratio (on CS_level)
  if (profile_type == "Electric Vehicle") {
    annual_energy_demand <- annual_mileage * location_mix[charging_location] * energy_efficiency
  } else {
    annual_energy_demand <- annual_mileage * location_mix[charging_location] * energy_efficiency * ev_cs_ratio
  }
  
  return (annual_energy_demand)
}


### Sample annual energy demands for CSs ###
# Description
# Samples annual energy demand by sampling from monthly energy demands and multiplying by 12.
#
# Args
#   sessions (dataframe): The dataset of sessions
#   n_runs (integer): Number of runs to sample annual demand for
#
# Returns
#   annual_energy_demand (double[]): vector containing annual energy demand samples 
sample_annual_endemand_cs <- function(sessions, n_runs) {
  energy_per_month <- sessions %>%
    arrange(desc(start_datetime)) %>%
    # We only take the most recent whole year
    filter(start_datetime > start_datetime[1] - 3600 * 24 * 365) %>%
    mutate(month=month(start_datetime)) %>%
    group_by(month, cs_id) %>%
    summarise(energy_sum=sum(energy)) %>%
    {.$energy_sum}
  
  # Create a distribution of monthly energy sums and a CDF of the distribution
  density <- density(energy_per_month)
  density$y <- density$y[density$x >= 0]
  density$x <- density$x[density$x >= 0]
  cdf <- cumsum(density$y) * diff(density$x[1:2])
  
  # We sample for each run a single month from all monthly energy sums
  uniform_samples <- runif(n_runs, min = min(cdf), max = max(cdf))
  samples <- approxfun(cdf, density$x)(uniform_samples)
  
  # Multiply by 12 to get yearly demand
  return (samples * 12)
}


### Sample from seasonality distribution ###
# Description
# This function samples from a seasonality distribution table, which is based on the following datasets:
# Den_Haag, EVnet, Jedlix and HTC_Eindhoven.
# For each dataset we selected the 100 card IDs per year and week with the highest energy demand.
# After that we normalized the weekly energy demand relative to the annual energy demand.
#
# Args
#   season_dist (dataframe): dataframe containing for each week the Q1 and Q3 values for power demand
#   annual_energy_demand (double[]): vector containing annual energy demand samples
#   n_runs (integer): number of simulation runs
#
# Returns
#   sample (dataframe): dataframe containing the weekly energy demand data
sample_seasonality <- function(season_dist, annual_energy_demand, n_runs) {
  # Random uniform distribution between min and max distribution of weekly energy demand relative to annual energy demand
  season_sample <- runif(nrow(season_dist) * n_runs, season_dist$energy_min, season_dist$energy_max)
  sample <- data.frame(season_sample)
  
  # Add artificial weeks to data frame
  sample$week <- rep(1:53, times = n_runs)
  
  # Add simulation run IDs to data frame
  sample$run_id <- rep(1:n_runs, each = 53)
  
  # Normalize samples
  sample <- sample %>%
    group_by(run_id) %>%
    dplyr::mutate(
      season_sample = season_sample/sum(season_sample)
    )
  
  # Store annual energy demand in data frame
  sample$annual_energy_demand <- rep(annual_energy_demand, each = 53)
  
  # Calculate weekly energy demand based on annual energy demand and sampled weekly coefficient
  sample$energy_week <- sample$season_sample * sample$annual_energy_demand
  
  return (sample)
}


### Sample from historical sessions ###
# Description
# This function matches historical session sequences with a weekly energy demand that is closest to the sampled
# weekly energy demand.
# After that the individual session data is being added, based on year, actual_week and card_id/cs_id
#
# Args
#   sessions (dataframe): dataframe containing session data
#   sessions_week (dataframe): dataframe containing session data aggregated on a weekly level
#   season_sample (dataframe): dataframe containing for `n` runs and for each week the power demand
#   profile_type (string): The type of profile to simulate ("Charging Station" or "Electric Vehicle")
#   n_runs (integer): The number of runs (profiles) to simulate
#
# Returns
#   sample (dataframe): dataframe containing sampled sessions
sample_sessions <- function(sessions, sessions_week, season_sample, profile_type, n_runs) {
  # Sort sessions by weekly energy demand
  sessions_week <- sessions_week %>% arrange(energy_week)
  
  # Match historical session sequences with a weekly energy demand that is closest to the
  # sampled weekly energy demand
  idx <- match.closest(season_sample$energy_week, sessions_week$energy_week)
  sample <- sessions_week[idx,]
  
  # Assign week numbers and simulation run IDs
  sample$week <- rep(1:53, times = n_runs)
  sample$run_id <- rep(1:n_runs, each = 53)
    
  # Join the weekly samples with the individual session data
  if (profile_type == "Electric Vehicle") {
    sample <- base::merge(sample, sessions, by = c("year", "actual_week", "card_id"))
  } else {
    sample <- base::merge(sample, sessions, by = c("year", "actual_week", "cs_id"))
  }
  
  sample <- sample %>%
    mutate(wday = lubridate::wday(start_datetime, week_start = 1))
  
  return (sample)
}

correct_holiday_sessions_cp <- function(sessions, sample, n_runs, year, holidays) {
  # Take a sample of cs_id's 
  cs_id_sample <- sessions %>%
    mutate(date = as.Date(start_datetime)) %>%
    group_by(cs_id) %>%
    filter(
      # Only take the most recent year of a CS
      date > max(date) - 3600 * 24 * 365
      # Enforce that we need to have at least one session in the same month last year;
      # We then have a complete year of sessions to choose from
      & any(year(date) == year(max(date) - 1) & month(date) == month(max(date)))
    ) %>%
    ungroup() %>%
    select(cs_id) %>%
    unique() %>%
    slice_sample(n=n_runs) %>%
    mutate(run_id = row_number()) %>%
    expand_grid(holiday=unique(holidays$holiday))
  
  # Get the holidays for the target year.
  # We need this to put the holidays on the right day of the right week
  holidays_year <- holidays %>%
    filter(year(date) == year) %>%
    select(holiday, week, wday)
  
  # Get the holiday sessions based on the cs_id sample
  holiday_sessions <- sessions %>%
    mutate(date = as.Date(start_datetime)) %>%
    merge(holidays, by="date") %>%
    merge(cs_id_sample, by=c("cs_id", "holiday")) %>%
    arrange(run_id, cs_id) %>%
    select(run_id, cs_id, card_id, start_datetime, end_datetime, date, holiday, energy) %>%
    merge(holidays_year, by="holiday")
  
  # Replace the sessions in the sample
  sample_holidays_fixed <- sample %>%
    select(run_id, cs_id, card_id, start_datetime, end_datetime, week, wday, energy) %>%
    # We merge the holidays on the week assigned during sampling and the week day of the session
    merge(holidays, by=c("week", "wday"), all.x = T) %>%
    # We need to remove all old sessions on the holidays
    filter(is.na(holiday)) %>%
    # Now append the holiday sessions
    rbind(holiday_sessions)
  
  return(sample_holidays_fixed)
}


### Calculate intervals per session ###
# Description
# This function calculates the number of intervals per session based on the session power distribution.
# The power rate starts to drop when the state of charge (SoC) is above 80%/90%
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#   kW (double): maximum power rate for each session
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
calculate_intervals <- function(samples, kW) {
  # Read session power distribution table
  power_dist <- readRDS("data/sessie_vermogens_verdeling.rds")
  power_dist$kW <- power_dist$y * kW
  
  # The max number of intervals is determined by:
  # The sampled session with the highest energy demand, and the area under the curve (AUC) 
  # of the session power distribution.
  # The distribution curve is normalized between 0 and 1, so we have to multiply with the power rate in kW.
  # We also multiply the product with a factor 4 to obtain the number of 15-minute intervals
  max_intervals <- ceiling(max(samples$energy) * (4/(kW * (sum(power_dist$y)/nrow(power_dist)))))
  max_intervals <- max(max_intervals, 5)
  
  # Initialize conversion table
  conversion <- data.frame(matrix(ncol = 2, nrow = 0))
  names(conversion) <- c("n_intervals", "energy")
  
  # For sessions with a maximum of 4 intervals we assume a constant power rate.
  # For longer sessions we calculate the number of intervals and energy based on the session power distribution
  for (n_intervals in c(5:max_intervals)) {
    # Add relative intervals to each session (1:n), and normalize them between 0 and 1
    intervals <- c(1:n_intervals)
    intervals <- intervals/n_intervals
    
    # Determine energy by matching the session power distribution on the normalized interval
    idx <- match.closest(intervals, power_dist$x)
    energy <- sum(power_dist[idx,]$kW)/4
    
    conversion[nrow(conversion) + 1,] <- c(n_intervals, energy)
  }
  
  # Determine number of intervals for each sampled session based on closest match on energy
  samples <- arrange(samples, energy)
  idx <- match.closest(samples$energy, conversion$energy)
  samples$n_intervals <- conversion[idx,]$n_intervals
  
  # For sessions with a maximum of 4 intervals we assume a constant power rate
  # The equation samples$energy <= kW also assumes that each interval has a length of 15 minutes
  samples$n_intervals <- ifelse(samples$energy <= kW, ceiling(samples$energy/(kW/4)), samples$n_intervals)
  
  return (samples)
}


### Convert sampled sessions ###
# Description
# This function applies selections and transformations to the sampled sessions
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#   kW (double): maximum power rate for each session
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
convert_samples <- function(samples, kW, by) {
  samples <- samples %>%
    arrange(week) %>%
    select(
      run_id,
      card_id,
      cs_id,
      start_datetime,
      end_datetime,
      energy,
      week,
      wday,
      n_intervals
    ) %>%
    dplyr::mutate(
      session_id = row_number(),
      start_datetime = round_date(start_datetime, by),
      end_datetime = round_date(end_datetime, by),
      # Limit end times to 24 hours
      end_datetime = pmin(end_datetime, start_datetime + 3600*24),
      # Ensure that the sessions spans at least one interval
      end_datetime = pmax(end_datetime, start_datetime + 60*15),
    )  %>%
    filter(end_datetime > start_datetime)
  
  return (samples)
}


### Flatten sampled sessions ###
# Description
# This function adds intervals between the start- and end datetime for each session
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
flatten_samples <- function(samples, by) {
  # setDT is a flattening function
  samples <- setDT(samples)[, list(
    session_id,
    run_id,
    card_id,
    cs_id,
    date_time = seq(start_datetime, end_datetime, by = by),
    week,
    wday,
    energy,
    n_intervals
  ), by = 1:nrow(samples)]
  
  samples <- samples %>%
    mutate(time = format(date_time, format = "%H:%M")) %>%
    # The week and wday don't change when flattening the sessions so we need to adjust for that
    group_by(session_id) %>%
    arrange(date_time) %>%
    mutate(
      actual_week = lubridate::week(date_time),
      actual_wday = lubridate::wday(date_time, week_start = 1),
      week_modifier = actual_week - min(actual_week),
      wday_modifier = actual_wday - min(actual_wday),
      week = (week + week_modifier) %% 53,
      week = ifelse(week == 0, 53, week),
      wday = (wday + wday_modifier) %% 7,
      wday = ifelse(wday == 0, 7, wday)
    ) %>%
    ungroup() %>%
    select(
      session_id,
      run_id,
      card_id,
      cs_id,
      week,
      wday,
      time,
      energy,
      n_intervals
    )
  
  return (samples)
}


### Calculate power rate ###
# Description
# This function assigns power rates to each individual interval based on the session power distribution and
# the relative interval of each session
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#   kW (double): maximum power rate for each session
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
calculate_power <- function(samples, kW) {
  # Read session power distribution table
  power_dist <- readRDS("data/sessie_vermogens_verdeling.rds")
  power_dist$kW <- power_dist$y * kW
  
  # Calculate relative interval and normalize between 0 and 1
  samples <- samples %>%
    group_by(session_id) %>%
    dplyr::mutate(
      interval = row_number(),
      interval = pmin(interval / n_intervals, 1)
    )
  
  # Match sampled sessions with session power distribution based on normalized intervals
  idx <- match.closest(samples$interval, power_dist$x)
  samples$power <- power_dist[idx,]$kW
  
  # Every interval after the needed amount of intervals to charge the EV can be set to zero power
  samples <- samples %>%
    group_by(session_id) %>%
    mutate(power=ifelse(row_number() <= n_intervals, power, 0))
  
  return (samples)
}


### Adjusted overlapping sessions ###
# Description
# This function adjusts for sessions of the same run_id, card_id, cs_id, week, wday and time that overlap
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
adjust_overlapping_sessions <- function(samples) {
  samples <- samples %>%
    group_by(
      run_id,
      card_id,
      cs_id,
      week,
      wday,
      time
    ) %>%
    dplyr::summarise(
      power = max(power)
    )
  
  return (samples)
}


### Combine simultaneous sessions at CP-level ###
# Description
# This function sums the power of simultaneous sessions occurring at CP-level
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#   profile_type (string): profile type (Electrical Vehicle or Charging Station)
#   kW (double): The maximum power of a connection
#   n_charging_points (integer): The number of charging points per charging station
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
combine_simultaneous_sessions <- function(samples, profile_type, kW, n_charging_points) {
  if (profile_type == "Electric Vehicle") {
    samples <- samples %>%
      select(
        run_id,
        card_id,
        week,
        wday,
        time,
        power
      ) %>% mutate(
        n = 1
      )
  } else {
    samples <- samples %>%
      group_by(
        run_id,
        week,
        wday,
        time
      ) %>%
      dplyr::summarise(
        power = sum(power),
        n = pmin(n(), n_charging_points)
      )
    
    samples[samples$power > n_charging_points * kW,]$power <- n_charging_points * kW
  }
  
  return (samples)
}


### Create charging profiles on an annual level ###
# Description
# This function creates date_time indices for n_runs charging profiles, and performs a join with the sampled data.
# This is done to make sure that every interval in a year is being covered
#
# Args
#   samples (dataframe): dataframe containing sampled sessions
#   n_runs (integer): number of simulation runs -> number of profiles
#
# Returns
#   df_cp (dataframe): dataframe containing charging profiles
create_profile <- function(samples, n_runs, start_date, end_date, by) {
  # Create the data frame spanning from the start to end date
  df_cp <- data.table(date_time = seq(start_date, end_date, by = by))
  
  # Add time related variables
  df_cp <- df_cp %>%
    mutate(
      week = lubridate::week(date_time),
      wday = lubridate::wday(date_time, week_start=1),
      time = format(date_time, format = "%H:%M"),
    )
  nrows_df_cp <- nrow(df_cp)
  
  # Replicate charging profile dataframe n_runs times
  df_cp_list <- replicate(n_runs, df_cp, simplify = FALSE)
  df_cp <- do.call(rbind, df_cp_list)
  df_cp$run_id <- rep(1:n_runs, each = nrows_df_cp)
  
  # Join charging profiles with sampled data
  df_cp <- merge(df_cp, samples, all.x = TRUE, by = c("run_id", "week", "wday", "time"))
  
  df_cp[is.na(df_cp)] <- 0
  
  # Sort charging profiles by date_time
  df_cp <- df_cp %>%
    arrange(run_id, date_time) %>%
    select(
      run_id,
      date_time,
      time,
      power,
      n
    )
}


### Fill in the allowed capacity at each interval ###
# Description
# This function calculates for each interval and charging station the available capacity based on the base capacity,
# the maximum rated capacity of charging stations, and the amount of allowed extra capacity on top of the base capacity.
# The allowed capacity is determined by finding the interpolated capacity fraction at a given timestamp, multiplying
# it with the maximum flex capacity at that point (max_capacity - base_capacity), and adding it to the base capacity.
#
# Args
#   df_cps (dataframe): Dataframe containing n charging profiles
#   max_capacity (double): The maximum capacity of the charging station
#   base_capacity (double): The base capacity which is always available to connected vehicles
#   allowed_capacity_fractions (dataframe): Dataframe containing the allowed capacity fractions per timestamp
#
# Returns
#   df_cps (dataframe): df_cps with for each interval the capacity added
create_capacities_from_fractions <- function(
    df_cps,
    kW,
    max_capacity,
    base_capacity,
    allowed_capacity_fractions
) {
  # Map the capacity fractions to the right date-time index
  df_cps <- df_cps %>%
    group_by(run_id) %>%
    mutate(allowed_capacity_fraction=approx(
      x=allowed_capacity_fractions$date_time,
      y=allowed_capacity_fractions$value,
      xout=date_time,
      rule=2
    )$y) %>%
    ungroup() %>%
    mutate(
      # In all cases the capacity of the charging station cannot exceed that of the occupied CPs
      capacity=pmin(
        # Maximum power power EV
        n * kW,
        # Maximum power allowed by capacity fractions
        n * (base_capacity + allowed_capacity_fraction * (kW - base_capacity)),
        # Maximum CS capacity
        max_capacity
      )
    ) %>%
    select(-c(allowed_capacity_fraction))
  
  return (df_cps)
}


### Distribute the overcapacity to later intervals ###
# Description
# When at a given interval and charging point station power is used than available (overcapacity), that power is
# distributed to later intervals.
#
# Args
#   df_cps (dataframe): dataframe containing `n` charging profiles
#
# Returns
#   A dataframe containing `n` charging profiles with overcapacity distributed to later intervals
distribute_overcapacity <- function(df_cps) {
  # Calculate initial remainders
  df_cps <- df_cps %>%
    group_by(run_id) %>%
    dplyr::mutate(
      # We can only distribute the remainder if the vehicles are still connected
      overcapacity = power - capacity,
      remainder = pmax(power - capacity, 0),
      # Initialize a metric to keep track of remainders that are lost when vehicles leave the CS
      remainder_after_leave = ifelse(n == 0, remainder, 0),
      remainder = ifelse(n > 0, remainder, 0)
    )
  
  while (sum(df_cps$remainder) > 0) {
    # Calculate required power
    df_cps <- df_cps %>%
      group_by(run_id) %>%
      arrange(date_time) %>%
      dplyr::mutate(
        power = pmin(power, capacity),
        # Shift remainders by one interval
        remainder = dplyr::lag(remainder, default = 0),
        # Update power rate by adding the shifted remainders
        power = power + remainder,
        remainder = pmax(power - capacity, 0),
        remainder_after_leave = ifelse(n == 0, remainder_after_leave + remainder, remainder_after_leave),
        remainder = ifelse(n > 0, remainder, 0),
      )
  }
  
  return (df_cps)
}


### Simulation pipeline ###
# Description
# This function is the main function of the model. It contains the simulation pipeline.
#
# Args
#   sessions (dataframe): individual session data
#   sessions_week (dataframe): session data aggregated on weekly level
#   profile_type (string): level on which we calculated the charging profile: "Electric Vehicle" or "Charging Station"
#   charging_location (string): type of charging location: "public", "work" or "home"
#   n_runs (integer): number of simulation runs, default 100
#   year (integer): year for which we calculate the charging profile, default 2023
#   by (string): interval size of the simulated year
#   kW (double): session power rate in kW, default 11
#   ev_cs_ratio (integer): EV/CS ratio when calculating the charging profile on CS-level, default 3
#   regular_profile (boolean): whether we calculate a regular charging profile or 'netbewust' charging profile
#   base_capacity (double): The base capacity of a charging point when Smart Charging
#   max_capacity (double): The maximum capacity of a charging station as a whole
#   n_charging_points (integer): The number of charging points per charging station
#   times (list[list]): The times during which smart charging is enabled. It should be given as a list of lists.
#   capacity_fractions_path (optional, string): Path to CSV containing for each interval the additional capacity fraction
#   capacity_fractions (dataframe): DataFrame (date_time, value) containing capacity fractions for each interval in a year
#   season_dist_path (string): Path to an RDS containing a dataframe with a seasonality distribution
#   seed (integer): Random seed to use during the simulation
#
# Returns
#   A list containing the individual charging profiles and aggregated charging profile
simulate <- function(
    sessions,
    sessions_week,
    profile_type = "Electric Vehicle",
    charging_location = "public",
    n_runs = 100,
    year = 2023,
    by = "15 mins",
    # 11kW is the assumed maximum charging rate of an EV using three-phase AC charging
    kW = 11.0,
    ev_cs_ratio = 3,
    regular_profile = TRUE,
    # 4kW is the base capacity per CP as defined by "Slim laden voor iedereen 2022 - 2025"
    base_capacity = 4,
    # P = U * I, and divide by 1000 to get kW
    # In general it is assumed that CS' are connected by three-phase 25A cables
    max_capacity = (3 * 25 * 230) / 1000,
    # Number of charging points per charging station (2 by default, should be set to 1 for home charging)
    n_charging_points = 2,
    times=list(list(floor_start=17, floor_end=23, pre_slope=NULL, post_slope=7)),
    capacity_fractions_path = NULL,
    capacity_fractions = NULL,
    season_dist_path = "data/Input/seasonality_distribution.rds",
    # A file file containing a table with the holidays that should be considered (2020-2050)
    holidays_path = "data/Input/holidays.csv",
    seed = NULL
) {
  # Currently, changing the interval size is not supported
  by = "15 mins"
  
  if (!is.null(seed)) {
    # Set the random seed
    set.seed(seed)
  }
  
  # Create start and end dates that span the target year
  start_date = as.POSIXct(paste0(year, "-01-01 00:00:00"))
  end_date = as.POSIXct(paste0(year+1, "-01-01 00:00:00"))
  # We put exactly one week before and after the start and end dates respectively to add padding.
  # This ensures that the edges of the data are realistic
  start_date_with_padding = start_date - 3600 * 24 * 7
  end_date_with_padding = end_date + 3600 * 24 * 7
  
  # Check whether sessions have CP or CS
  if (charging_location %in% c("home", "work")) {
    if ("cp_id" %in% colnames(sessions)) {
      # Use `cp_id` as `cs_id`
      sessions$cs_id <- sessions$cp_id
    } else {
      print("WARNING: Need to define `cp_id` when simulating home or work locations! Now using `cs_id`!")
    }
  }
  if (charging_location %in% c("public") & !("cs_id" %in% colnames(sessions))) {
    print("WARNING: Need to define `cs_id` when simulating home or work locations! Now using `cp_id`!")
    sessions$cs_id <- sessions$cp_id
  }
  
  ### Read files
  
  season_dist <- readRDS(season_dist_path)
  
  holidays <- NULL
  if (!is.null(holidays_path)) {
    holidays <- read.csv(holidays_path) %>%
      mutate(
        date = as.Date(date),
        week = lubridate::week(date),
        wday = lubridate::wday(date, week_start = 1)
      )
  }
  
  if (!is.null(capacity_fractions_path)) {
    # Capacity fractions are given so load them in
    capacity_fractions <- read.csv(capacity_fractions_path) %>%
      mutate(date_time = as_datetime(date_time))
  }
  
  ###
  
  if (!is.null(capacity_fractions)) {
    # We may need to adjust the capacity fractions' year to match the selected year.
    # This is not an optimal solution, but it should be up to the user to supply data for the correct year
    if (!(start_date %in% capacity_fractions$date_time) | !(end_date %in% capacity_fractions$date_time)) {
      median_year <- capacity_fractions %>%
        mutate(year=lubridate::year(date_time)) %>%
        group_by(year) %>%
        count() %>%
        arrange(desc(n)) %>%
        {.$year[1]}
      
      year_diff <- year - median_year
      
      if (year_diff != 0) {
        print(paste0(
          "WARNING: Capacity fractions do not span the target year! ",
          "Adjusting by ", year_diff, " years to match the target year ", year, "!"
        ))
        
        # We shift by exact weeks to make sure that the weekdays stay the same
        weeks_shift <- round((52 + 1/7) * year_diff)
        capacity_fractions <- capacity_fractions %>%
          mutate(date_time=date_time+weeks(weeks_shift))
      }
    }
  }
  
  if (!regular_profile & is.null(capacity_fractions)) {
    # Create capacity fractions based on Smart Charging
    capacity_fractions <- create_capacity_fractions_netbewust_laden(
      start_date_with_padding, end_date_with_padding, by, times=times
    )
  }
  
  if (profile_type == "Electric Vehicle") {
    # Sample for each run annual energy demand based on historical EV data and predictions
    annual_energy_demand <- sample_annual_endemand(profile_type, charging_location, n_runs, year, ev_cs_ratio)
  } else {
    # Sample for each run annual energy demand based on the CS monthly energy distribution in the session data
    annual_energy_demand <- sample_annual_endemand_cs(sessions, n_runs)
  }
  
  # Convert annual energy demand into weekly energy demand and apply a seasonality distribution
  season_sample <- sample_seasonality(season_dist, annual_energy_demand, n_runs)
  session_sample <- sample_sessions(sessions, sessions_week, season_sample, profile_type, n_runs)
  
  if (!is.null(holidays)) {
    session_sample <- correct_holiday_sessions_cp(sessions, session_sample, n_runs, year, holidays)
  }
  
  session_sample <- calculate_intervals(session_sample, kW)
  session_sample <- convert_samples(session_sample, kW, by)
  session_sample <- flatten_samples(session_sample, by)
  session_sample <- calculate_power(session_sample, kW)
  session_sample <- adjust_overlapping_sessions(session_sample)
  session_sample <- combine_simultaneous_sessions(session_sample, profile_type, kW, n_charging_points)
  
  df_cps <- create_profile(session_sample, n_runs, start_date_with_padding, end_date_with_padding, by)
  
  # Set default capacities
  df_cps$capacity <- pmin(df_cps$n*kW, max_capacity)
  
  if (!is.null(capacity_fractions)) {
    df_cps <- create_capacities_from_fractions(df_cps, kW, max_capacity, base_capacity, capacity_fractions)
  }
  
  df_cps <- distribute_overcapacity(df_cps)
  
  # Remove padding
  df_cps <- df_cps[df_cps$date_time >= start_date,]
  df_cps <- df_cps[df_cps$date_time < end_date,]
  
  # Create the aggregated profile which sums for each interval the power of all profiles
  df_cp <- df_cps %>%
    dplyr::group_by(date_time) %>%
    dplyr::summarise(
      power = sum(power, na.rm = TRUE),
      n = sum(n, na.rm = TRUE),
      capacity = sum(capacity, na.rm = TRUE)
    )
  
  return (list("individual" = df_cps, "aggregated" = df_cp))
}
