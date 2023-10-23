library(data.table)
library(dplyr)
library(lubridate)
library(MALDIquant)

source("capacity_fractions.R")


### Retrieve historical sessions ###
# Description
# This function retrieves historical sessions from the Postgres DB with the following variables
#
# Args
#   input_data (string): name of input dataset (Den Haag, EVnet, Jedlix, HTC Eindhoven)
#
# Returns
#   sessions (dataframe): dataframe containing the following session data
#     -card_id
#     -cs_id
#     -start_datetime
#     -end_datetime
#     -year
#     -actual_week
#     -energy
get_sessions <- function(input_data) {
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    print("Connecting to Database…")
    connec <- dbConnect(drv,
                        dbname = Sys.getenv("PG_DBNAME"),
                        host = Sys.getenv("PG_HOST"),
                        port = Sys.getenv("PG_PORT"),
                        user = Sys.getenv("PG_USER"),
                        password = Sys.getenv("PG_PASSWORD"))
    print("Database Connected!")
  },
  error=function(cond) {
    print("Unable to connect to Database.")
  })
  
  sessions <- DBI::dbReadTable(connec, c("ls_profielen", paste0("sessions_", input_data)))
  
  return (sessions)
}


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


### Sample from annual energy demand distribution ###
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
  if (profile_type == "EV") {
    annual_energy_demand <- annual_mileage * location_mix[charging_location] * energy_efficiency
  } else {
    annual_energy_demand <- annual_mileage * location_mix[charging_location] * energy_efficiency * ev_cs_ratio
  }
  
  return (annual_energy_demand)
}


### Sample from seasonality distribution ###
# Description
# This function samples from a seasonality distribution table, which is based on the following datasets:
# Den_Haag, EVnet, Jedlix and HTC_Eindhoven.
# For each dataset we selected the 100 card IDs per year and week with the highest energy demand.
# After that we normalized the weekly energy demand relative to the annual energy demand.
#
# Args
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
  
  # Test annual energy demand per simulation run
  sample_sum <- sample %>%
    group_by(run_id) %>%
    dplyr::summarise(
      energy = sum(energy_week, na.rm = TRUE)
    )
  
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
    sample <- merge(sample, sessions, by = c("year", "actual_week", "card_id"))
  } else {
    sample <- merge(sample, sessions, by = c("year", "actual_week", "cs_id"))
  }
  
  # Test annual energy demand per run
  sample_sum <- sample %>%
    group_by(run_id) %>%
    dplyr::summarise(
      energy = sum(energy, na.rm = TRUE)
    )
  
  return (sample)
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
convert_samples <- function(samples, kW) {
  samples <- samples %>%
    arrange(week) %>%
    select(
      run_id,
      card_id,
      cs_id,
      start_datetime,
      energy,
      week,
      n_intervals
    ) %>%
    dplyr::mutate(
      session_id = row_number(),
      start_datetime = round_date(start_datetime, "15 mins"),
      end_datetime = start_datetime + lubridate::minutes(n_intervals * 15),
      end_datetime_min15 = end_datetime - lubridate::minutes(15)
    ) %>%
    filter(
      !is.na(end_datetime_min15),
      end_datetime > start_datetime
    )
  
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
flatten_samples <- function(samples) {
  # setDT is a flattening function
  samples <- setDT(samples)[, list(
    session_id,
    run_id,
    card_id,
    cs_id,
    # When flattening the sessions we need end_datetime_min15, otherwise we add 1 interval too much
    date_time = seq(start_datetime, end_datetime_min15, by = "15 mins"),
    week,
    energy
  ), by = 1:nrow(samples)]
  
  samples <- samples %>%
    mutate(
      wday = lubridate::wday(date_time),
      time = format(date_time, format = "%H:%M")
    ) %>%
    select(
      session_id,
      run_id,
      card_id,
      cs_id,
      week,
      wday,
      time,
      energy
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
      interval = interval/n()
    )
  
  # Match sampled sessions with session power distribution based on normalized intervals
  idx <- match.closest(samples$interval, power_dist$x)
  samples$power <- power_dist[idx,]$kW
  
  samples[samples$energy <= kW,]$power <- kW
  
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
#
# Returns
#   samples (dataframe): dataframe containing sampled sessions
combine_simultaneous_sessions <- function(samples, profile_type, kW) {
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
        cs_id,
        week,
        wday,
        time
      ) %>%
      dplyr::summarise(
        power = sum(power),
        n = n()
      )
    
    samples[samples$power > 2 * kW,]$power <- 2 * kW
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
create_profile <- function(samples, n_runs) {
  # Add one week before 1st of January to avoid starting at 0 kW
  df_cp <- data.table(date_time = seq(
    as.POSIXct("2021-12-25 00:00:00"),
    as.POSIXct("2022-12-31 23:59:59"),
    by = "15 min"
  ))
  
  # Add time related variables
  df_cp <- df_cp %>%
    mutate(
      month = lubridate::month(date_time),
      week = lubridate::week(date_time),
      wday = lubridate::wday(date_time),
      time = format(date_time, format = "%H:%M"),
      hour = lubridate::hour(date_time)
    )
  nrows_df_cp <- nrow(df_cp)
  
  # Replicate charging profile dataframe n_runs times
  df_cp_list <- replicate(n_runs, df_cp, simplify = FALSE)
  df_cp <- do.call(rbind, df_cp_list)
  df_cp$run_id <- rep(1:n_runs, each = nrows_df_cp)
  
  # Join charging profiles with sampled data
  df_cp <- merge(df_cp, samples, all.x = TRUE, by = c("run_id", "week", "wday", "time"))
  
  df_cp[is.na(df_cp)] <- 0
  df_cp <- df_cp[df_cp$date_time >= "2022-01-01 00:00:00",]
  
  # Sort charging profiles by date_time
  df_cp <- df_cp %>%
    arrange(date_time) %>%
    select(
      run_id,
      date_time,
      time,
      power,
      n
    )
  
  return (df_cp)
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
create_capacities_from_fractions <- function(
  df_cps,
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
    )$y) %>%
    ungroup() %>%
    mutate(
      # The flex power is equal to the max minus base capacity (per EV)
      max_flex_capacity=max_capacity - base_capacity * n,
      # Determine the capacities, equal to the flex power plus the base capacity (per EV)
      capacity=allowed_capacity_fraction * max_flex_capacity + base_capacity * n
    ) %>%
    select(-c(allowed_capacity_fraction, max_flex_capacity))
  
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
      remainder = pmax(power - capacity, 0),
    )
  
  while (sum(df_cps$remainder) > 0) {
    df_cps <- df_cps %>%
      group_by(run_id) %>%
      dplyr::mutate(
        power = pmin(power, capacity)
      )
    
    # Shift remainders by one interval
    df_cps <- df_cps %>%
      group_by(run_id) %>%
      arrange(date_time) %>%
      dplyr::mutate(
        remainder = dplyr::lag(remainder, default = 0)
      )
    
    # Update power rate by adding the shifted remainders
    df_cps <- df_cps %>%
      group_by(run_id) %>%
      dplyr::mutate(
        power = power + remainder
      )
    
    # Recalculate remainders
    df_cps <- df_cps %>%
      group_by(run_id) %>%
      dplyr::mutate(
        remainder = pmax(power - capacity, 0)
      )
    
    df_cps <- df_cps[df_cps$date_time <= "2022-12-31 23:59:59",]
  }
  df_cps <- df_cps[,c("run_id", "date_time", "time", "power", "n")]
  
  return (df_cps)
}


### Simulation pipeline ###
# Description
# This function is the main function of the model. It contains the simulation pipeline.
#
# Args
#   sessions = individual session data
#   sessions_week = session data aggregated on weekly level
#   profile_type = level on which we calculated the charging profile: "Electric Vehicle" or "Charging Station"
#   charging_location = type of charging location: "public", "work" or "home"
#   n_runs = number of simulation runs, default 100
#   year = year for which we calculate the charging profile, default 2023
#   kW = session power rate, default 11
#   ev_cs_ratio = EV/CS ratio when calculating the charging profile on CS-level, default 3
#   regular_profile = whether we calculate a regular charging profile or 'netbewust' charging profile
#   dashboard = whether the simulation pipeline is being called in dashboard mode or not
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
  times=list(list(floor_start=17, floor_end=23, pre_slope=NULL, post_slope=7)),
  capacity_fractions_path = NULL,
  season_dist_path = "data/Input/seasonality_distribution.rds"
) {
  # Read files
  season_dist <- readRDS(season_dist_path)
  
  annual_energy_demand <- sample_annual_endemand(profile_type, charging_location, n_runs, year, ev_cs_ratio)
  season_sample <- sample_seasonality(season_dist, annual_energy_demand, n_runs)
  session_sample <- sample_sessions(sessions, sessions_week, season_sample, profile_type, n_runs)
  session_sample <- calculate_intervals(session_sample, kW)
  session_sample <- convert_samples(session_sample, kW)
  session_sample <- flatten_samples(session_sample)
  session_sample <- calculate_power(session_sample, kW)
  session_sample <- adjust_overlapping_sessions(session_sample)
  session_sample <- combine_simultaneous_sessions(session_sample, profile_type, kW)
  df_cps <- create_profile(session_sample, n_runs)

  if (regular_profile) {
    df_cps$capacity <- kW
  } else {
    from <- as_datetime(ISOdate(2021, 12, 31, hour=0, min=0, sec=0), "CET")
    to <- as_datetime(ISOdate(2023, 1, 2, hour=0, min=0, sec=0), "CET")
    capacity_fractions <- create_capacity_fractions_netbewust_laden(from, to, by, times=times)
    df_cps <- create_capacities_from_fractions(df_cps, max_capacity, base_capacity, capacity_fractions)
  }

  df_cps <- distribute_overcapacity(df_cps)

  df_cp <- df_cps %>%
    dplyr::group_by(date_time) %>%
    dplyr::summarise(
      power = sum(power, na.rm = TRUE),
      n = sum(n, na.rm = TRUE)
    )

  return (list("individual" = df_cps, "aggregated" = df_cp))
}
