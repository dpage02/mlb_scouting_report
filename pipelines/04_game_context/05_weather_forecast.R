# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 05_weather_forecast.R
# ============================================================
# PURPOSE:
#   Attach simple game-day weather forecast to schedule_context.
#
# OUTPUT:
#   - weather_context
#
# NOTES:
#   - Uses Open-Meteo API (no key required)
#   - Returns temperature (°F), wind (mph), precipitation (inches)
#   - Safe if coordinates missing
# ============================================================

library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyr)

message("Running 05_weather_forecast.R")

if (!exists("schedule_context")) {
  stop("schedule_context not found. Run 01_schedule.R first.")
}

if (!exists("park_coordinates")) {
  stop("park_coordinates not found. Build static park context first.")
}

# ------------------------------------------------------------
# 1. Join Park Coordinates
# ------------------------------------------------------------

weather_games <- schedule_context %>%
  left_join(
    park_coordinates,
    by = c("venue_name" = "park_name")
  )

# ------------------------------------------------------------
# 2. Weather Pull Function (Game-Day Forecast)
# ------------------------------------------------------------

get_game_weather <- function(lat, lon, game_date) {
  
  if (is.na(lat) || is.na(lon)) {
    return(tibble(
      game_temp_f      = NA_real_,
      wind_speed_mph   = NA_real_,
      precipitation_in = NA_real_
    ))
  }
  
  base_url <- "https://api.open-meteo.com/v1/forecast"
  
  response <- GET(
    base_url,
    query = list(
      latitude = lat,
      longitude = lon,
      daily = "temperature_2m_max,precipitation_sum,windspeed_10m_max",
      start_date = game_date,
      end_date = game_date,
      temperature_unit = "fahrenheit",
      windspeed_unit = "mph",
      precipitation_unit = "inch",
      timezone = "auto"
    )
  )
  
  if (status_code(response) != 200) {
    return(tibble(
      game_temp_f      = NA_real_,
      wind_speed_mph   = NA_real_,
      precipitation_in = NA_real_
    ))
  }
  
  parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  if (is.null(parsed$daily)) {
    return(tibble(
      game_temp_f      = NA_real_,
      wind_speed_mph   = NA_real_,
      precipitation_in = NA_real_
    ))
  }
  
  tibble(
    game_temp_f      = parsed$daily$temperature_2m_max[1],
    wind_speed_mph   = parsed$daily$windspeed_10m_max[1],
    precipitation_in = round(parsed$daily$precipitation_sum[1], 2)
  )
}

# ------------------------------------------------------------
# 3. Pull Weather For Each Game
# ------------------------------------------------------------

weather_context <- weather_games %>%
  rowwise() %>%
  mutate(
    weather = list(
      get_game_weather(
        latitude,
        longitude,
        as.character(game_date)
      )
    )
  ) %>%
  unnest(cols = weather) %>%
  ungroup() %>%
  select(
    game_pk,
    venue_name,
    game_temp_f,
    wind_speed_mph,
    precipitation_in
  )

message("weather_context built: ", nrow(weather_context), " rows")
message("05_weather_forecast.R complete")
