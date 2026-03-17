# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 02_static_context
# SCRIPT: 00_park_coordinates.R
# ============================================================
# PURPOSE:
#   Static coordinate reference table for:
#     - All 30 MLB regular season parks
#     - All 24 Spring Training parks (AZ + FL)
#
# DESIGN:
#   - Hard-coded
#   - Deterministic
#   - No external API dependency
#   - Coordinates are venue-specific
#
# OUTPUT:
#   park_coordinates
# ============================================================

library(dplyr)

message("Building static park coordinate table")

park_coordinates <- tibble::tribble(
  ~park_name, ~latitude, ~longitude, ~park_level,
  
  # =========================
  # MLB REGULAR SEASON (30)
  # =========================
  
  "Fenway Park", 42.3467, -71.0972, "MLB",
  "Yankee Stadium", 40.8296, -73.9262, "MLB",
  "Tropicana Field", 27.7682, -82.6534, "MLB",
  "Rogers Centre", 43.6414, -79.3894, "MLB",
  "Oriole Park at Camden Yards", 39.2839, -76.6217, "MLB",
  "Angel Stadium", 33.8003, -117.8827, "MLB",
  "Globe Life Field", 32.7473, -97.0847, "MLB",
  "Minute Maid Park", 29.7573, -95.3555, "MLB",
  "Oakland Coliseum", 37.7516, -122.2005, "MLB",
  "T-Mobile Park", 47.5914, -122.3325, "MLB",
  "Guaranteed Rate Field", 41.8300, -87.6338, "MLB",
  "Progressive Field", 41.4962, -81.6852, "MLB",
  "Comerica Park", 42.3390, -83.0485, "MLB",
  "Kauffman Stadium", 39.0517, -94.4803, "MLB",
  "Target Field", 44.9817, -93.2775, "MLB",
  "Chase Field", 33.4453, -112.0667, "MLB",
  "Dodger Stadium", 34.0739, -118.2400, "MLB",
  "Coors Field", 39.7559, -104.9942, "MLB",
  "Petco Park", 32.7073, -117.1573, "MLB",
  "Oracle Park", 37.7786, -122.3893, "MLB",
  "Truist Park", 33.8907, -84.4677, "MLB",
  "loanDepot park", 25.7781, -80.2197, "MLB",
  "Nationals Park", 38.8730, -77.0074, "MLB",
  "Citi Field", 40.7571, -73.8458, "MLB",
  "Citizens Bank Park", 39.9057, -75.1665, "MLB",
  "Wrigley Field", 41.9484, -87.6553, "MLB",
  "Great American Ball Park", 39.0979, -84.5086, "MLB",
  "American Family Field", 43.0280, -87.9712, "MLB",
  "PNC Park", 40.4469, -80.0057, "MLB",
  "Busch Stadium", 38.6226, -90.1928, "MLB",
  
  # =========================
  # SPRING TRAINING — ARIZONA (Cactus League)
  # =========================
  
  "Camelback Ranch", 33.4553, -112.3066, "SPRING",
  "Salt River Fields at Talking Stick", 33.5453, -111.8853, "SPRING",
  "Peoria Sports Complex", 33.6319, -112.2336, "SPRING",
  "Surprise Stadium", 33.6272, -112.3756, "SPRING",
  "Goodyear Ballpark", 33.4300, -112.3914, "SPRING",
  "Tempe Diablo Stadium", 33.4143, -111.9695, "SPRING",
  "Scottsdale Stadium", 33.5093, -111.9150, "SPRING",
  "Hohokam Stadium", 33.4362, -111.8412, "SPRING",
  "American Family Fields of Phoenix", 33.5382, -112.0925, "SPRING",
  
  # =========================
  # SPRING TRAINING — FLORIDA (Grapefruit League)
  # =========================
  
  "CACTI Park of the Palm Beaches", 26.7846, -80.1133, "SPRING",
  "Roger Dean Chevrolet Stadium", 26.8906, -80.1073, "SPRING",
  "JetBlue Park", 26.5790, -81.8634, "SPRING",
  "Hammond Stadium", 26.5788, -81.8720, "SPRING",
  "George M. Steinbrenner Field", 28.0108, -82.5063, "SPRING",
  "BayCare Ballpark", 27.9759, -82.7957, "SPRING",
  "TD Ballpark", 28.0122, -82.7867, "SPRING",
  "LECOM Park", 27.4881, -82.5754, "SPRING",
  "CoolToday Park", 27.0494, -82.2271, "SPRING",
  "Charlotte Sports Park", 26.9824, -82.0913, "SPRING",
  "Clover Park", 27.3503, -80.4037, "SPRING",
  "Ed Smith Stadium", 27.3360, -82.5214, "SPRING",
  "Publix Field at Joker Marchant Stadium", 27.9630, -82.0089, "SPRING"
)

park_coordinates <- park_coordinates %>%
  mutate(park_id = row_number()) %>%
  relocate(park_id)

message("Total parks loaded: ", nrow(park_coordinates))
message("00_park_coordinates.R complete.")
