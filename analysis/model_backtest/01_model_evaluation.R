# ============================================================
# MLB Game Prediction Model — Historical Backtest
# Seasons: 2022, 2023, 2024
# Goal: Compare 5 model variations to find optimal constants
#       and formulation for _game_narrative_helpers.R
# ============================================================
# Run time: ~15-25 minutes (rate-limited API calls)
# Output:   analysis/model_backtest/results/
# ============================================================

suppressPackageStartupMessages({
  library(baseballr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(httr)
  library(jsonlite)
  library(broom)
})

SEASONS      <- 2022:2024
OUT_DIR      <- "analysis/model_backtest/results"
CACHE_DIR    <- "analysis/model_backtest/cache"
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

safe_sleep <- function(s = 0.5) Sys.sleep(s)

# ============================================================
# 1. PULL ACTUAL GAME RESULTS  (MLB Stats API)
# ============================================================
# Returns home_score, away_score, winning_side, and team IDs
# for every completed regular-season game.

pull_season_results <- function(season) {
  cache_file <- file.path(CACHE_DIR, paste0("game_results_", season, ".rds"))
  if (file.exists(cache_file)) {
    message("  [cache] game results ", season)
    return(readRDS(cache_file))
  }
  message("  [API] fetching game schedule for ", season, " ...")

  url <- paste0(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1&season=", season,
    "&gameType=R",
    "&fields=dates,date,games,gamePk,status,abstractGameState,",
    "teams,home,away,score,isWinner,team,id,name"
  )
  resp <- httr::GET(url, httr::timeout(60))
  if (httr::http_error(resp)) stop("Stats API failed for season ", season)

  raw  <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)
  dates <- raw$dates

  rows <- purrr::map_dfr(dates, function(d) {
    purrr::map_dfr(d$games, function(g) {
      status <- g$status$abstractGameState %||% ""
      if (status != "Final") return(NULL)
      tibble(
        season        = season,
        game_date     = d$date,
        game_pk       = as.integer(g$gamePk),
        home_team_id  = as.integer(g$teams$home$team$id),
        home_team_name= g$teams$home$team$name,
        away_team_id  = as.integer(g$teams$away$team$id),
        away_team_name= g$teams$away$team$name,
        home_score    = as.integer(g$teams$home$score %||% NA),
        away_score    = as.integer(g$teams$away$score %||% NA)
      )
    })
  })

  rows <- rows %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    mutate(
      total_runs    = home_score + away_score,
      home_win      = as.integer(home_score > away_score)
    )

  saveRDS(rows, cache_file)
  message("    -> ", nrow(rows), " games")
  rows
}

# helper for %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b

game_results <- purrr::map_dfr(SEASONS, pull_season_results)
message("Total games: ", nrow(game_results))

# ============================================================
# 2. FANGRAPHS TEAM BATTING  (wRC+ by team-season)
# ============================================================

pull_fg_team_batting <- function(season) {
  cache_file <- file.path(CACHE_DIR, paste0("fg_team_bat_", season, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))
  message("  [FG] team batting ", season)

  # fg_batter_leaders with qual=0 type=t (team split)
  # baseballr wrapper: fg_team_batter(startseason, endseason)
  d <- tryCatch(
    baseballr::fg_team_batter(startseason = season, endseason = season),
    error = function(e) {
      message("    fg_team_batter failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(d) || nrow(d) == 0) {
    # Fallback: fg_batter_leaders at team-level
    d <- tryCatch(
      baseballr::fg_batter_leaders(
        startseason = season, endseason = season,
        league = "all", qual = 0, ind = 0
      ) %>%
        group_by(team_name) %>%
        summarise(
          fg_team_wRC_plus = weighted.mean(wRC_plus, PA, na.rm = TRUE),
          .groups = "drop"
        ),
      error = function(e) NULL
    )
    if (!is.null(d)) {
      d <- d %>% rename(TeamName = team_name, wRC_plus = fg_team_wRC_plus)
    }
  }
  if (is.null(d)) return(NULL)
  saveRDS(d, cache_file)
  safe_sleep()
  d
}

fg_bat_raw <- purrr::map(SEASONS, function(s) {
  d <- pull_fg_team_batting(s)
  if (!is.null(d)) d$season <- s
  d
}) %>% purrr::compact()

# Normalize: need (season, team_name_fg, team_wRC_plus)
fg_batting <- purrr::map_dfr(fg_bat_raw, function(d) {
  # FanGraphs team batting column names vary by baseballr version
  nm_cols   <- c("TeamName","Team","team_name","teamName")
  wrc_cols  <- c("wRC_plus","wRCplus","wrc_plus","wRC+")
  nm_col    <- nm_cols[nm_cols %in% names(d)][1]
  wrc_col   <- wrc_cols[wrc_cols %in% names(d)][1]
  if (is.na(nm_col) || is.na(wrc_col)) {
    message("  WARNING: can't find team name / wRC+ column in fg batting")
    message("  Available cols: ", paste(names(d), collapse=", "))
    return(NULL)
  }
  tibble(
    season         = d$season,
    fg_team_name   = d[[nm_col]],
    team_wRC_plus  = as.numeric(d[[wrc_col]])
  )
}) %>%
  filter(!is.na(team_wRC_plus)) %>%
  distinct()

message("FG batting rows: ", nrow(fg_batting))

# ============================================================
# 3. FANGRAPHS PITCHER STATS  (FIP, xFIP, SIERA, ERA)
# NOTE: fg_pitcher_leaders() has a known "object 'leaders' not found" bug.
#       Using direct FanGraphs API (httr) as the pipeline does.
# ============================================================

pull_fg_pitchers_api <- function(season) {
  cache_file <- file.path(CACHE_DIR, paste0("fg_pit_", season, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))
  message("  [FG direct API] pitchers ", season)
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/leaders/major-league/data",
      query = list(
        pos       = "all",
        stats     = "pit",
        lg        = "all",
        season    = season,
        season1   = season,
        ind       = "0",
        qual      = "0",
        type      = "8",      # Dashboard: FIP, xFIP, SIERA, ERA, K%, etc.
        pageitems = "2000000",
        pagenum   = "1",
        rost      = "0"
      ),
      httr::timeout(60)
    ),
    error = function(e) { message("    httr error: ", e$message); NULL }
  )
  if (is.null(resp) || httr::http_error(resp)) {
    message("    API HTTP error for season ", season); return(NULL)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !"data" %in% names(parsed)) return(NULL)
  d <- tryCatch(dplyr::as_tibble(parsed$data), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  message("    -> ", nrow(d), " pitcher rows")
  saveRDS(d, cache_file)
  safe_sleep(0.5)
  d
}

# Pull all pitchers once per season (SP + RP together); filter below
fg_pit_raw <- purrr::map_dfr(SEASONS, function(s) {
  d <- pull_fg_pitchers_api(s)
  if (!is.null(d)) dplyr::mutate(d, season = as.integer(s)) else NULL
})

message("FG raw pitcher rows: ", nrow(fg_pit_raw))
if (nrow(fg_pit_raw) > 0) message("  Columns: ", paste(names(fg_pit_raw)[1:min(15, ncol(fg_pit_raw))], collapse=", "))

# Safe column picker
pick_col <- function(df, choices) {
  hit <- choices[choices %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# Resolve column names (FG API naming can vary slightly)
col_nm   <- pick_col(fg_pit_raw, c("PlayerName","Name","player_name","playerName","name"))
col_team <- pick_col(fg_pit_raw, c("Team","team_name","TeamName","teamName"))
col_ip   <- pick_col(fg_pit_raw, c("IP","ip"))
col_gs   <- pick_col(fg_pit_raw, c("GS","gs"))
col_era  <- pick_col(fg_pit_raw, c("ERA","era"))
col_fip  <- pick_col(fg_pit_raw, c("FIP","fip"))
col_xfip <- pick_col(fg_pit_raw, c("xFIP","xfip"))
col_siera<- pick_col(fg_pit_raw, c("SIERA","siera"))

message("  Column map: name=",col_nm," team=",col_team," IP=",col_ip,
        " GS=",col_gs," FIP=",col_fip," xFIP=",col_xfip)

safe_num <- function(df, col) if (!is.na(col)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))

fg_pit_all <- fg_pit_raw %>%
  dplyr::transmute(
    season   = season,
    pit_name = if (!is.na(col_nm))   .data[[col_nm]]   else NA_character_,
    pit_team = if (!is.na(col_team)) .data[[col_team]] else NA_character_,
    pit_IP   = safe_num(., col_ip),
    pit_GS   = safe_num(., col_gs),
    pit_ERA  = safe_num(., col_era),
    pit_FIP  = safe_num(., col_fip),
    pit_xFIP = safe_num(., col_xfip),
    pit_SIERA= safe_num(., col_siera)
  ) %>%
  dplyr::mutate(
    pit_best_fip  = dplyr::coalesce(pit_xFIP, pit_SIERA, pit_FIP, pit_ERA),
    pit_IP_per_GS = dplyr::if_else(!is.na(pit_GS) & pit_GS > 0,
                                   pit_IP / pit_GS, NA_real_),
    gs_rate       = dplyr::if_else(!is.na(pit_IP) & pit_IP > 0,
                                   pit_GS / pit_IP, NA_real_)
  ) %>%
  dplyr::filter(!is.na(pit_IP), pit_IP > 0)

# Split into SP (GS/IP ≥ 0.15 AND IP ≥ 50) and RP (GS/IP < 0.15 AND IP ≥ 10)
fg_sp <- fg_pit_all %>%
  dplyr::filter(gs_rate >= 0.15, pit_IP >= 50, !is.na(pit_best_fip)) %>%
  dplyr::rename(sp_name=pit_name, sp_team=pit_team, sp_IP=pit_IP, sp_GS=pit_GS,
                sp_ERA=pit_ERA, sp_FIP=pit_FIP, sp_xFIP=pit_xFIP, sp_SIERA=pit_SIERA,
                sp_best_fip=pit_best_fip, sp_IP_per_GS=pit_IP_per_GS) %>%
  dplyr::select(-gs_rate)

message("FG SP rows (GS-heavy, ≥50 IP): ", nrow(fg_sp))

# ============================================================
# 4. BULLPEN FIP  (aggregate RP per team-season)
# ============================================================
# Relievers: gs_rate < 0.15, IP ≥ 10

# Aggregate bullpen FIP from the same all-pitcher pull (relievers: gs_rate < 0.15, IP ≥ 10)
fg_bp <- fg_pit_all %>%
  dplyr::filter(gs_rate < 0.15, pit_IP >= 10, !is.na(pit_FIP)) %>%
  dplyr::group_by(season, bp_team = pit_team) %>%
  dplyr::summarise(
    bullpen_FIP = weighted.mean(pit_FIP,  pit_IP, na.rm = TRUE),
    bullpen_ERA = weighted.mean(pit_ERA,  pit_IP, na.rm = TRUE),
    bullpen_IP  = sum(pit_IP),
    .groups     = "drop"
  ) %>%
  dplyr::filter(!is.na(bullpen_FIP))

message("Bullpen team-seasons: ", nrow(fg_bp))

# ============================================================
# 5. PARK FACTORS  (FanGraphs 3-yr averaged)
# ============================================================

pull_park_factors <- function() {
  cache_file <- file.path(CACHE_DIR, "fg_park_factors.rds")
  if (file.exists(cache_file)) return(readRDS(cache_file))
  message("  [FG] park factors")
  d <- tryCatch(
    baseballr::fg_park(2024),
    error = function(e) { message("  ERROR fg_park: ", e$message); NULL }
  )
  saveRDS(d, cache_file)
  d
}

pf_raw <- pull_park_factors()
message("Park factor rows: ", nrow(pf_raw %||% data.frame()))

# Normalize park factor columns
pf_col_team  <- c("team","Team","TeamName","team_name")
pf_col_basic <- c("basic_5yr","basic_3yr","basic","ParkFactor","park_factor","pfr")

if (!is.null(pf_raw) && nrow(pf_raw) > 0) {
  pf_team_col  <- pick_col(pf_raw, pf_col_team)
  pf_basic_col <- pick_col(pf_raw, pf_col_basic)
  message("  park factor cols used: team=", pf_team_col, " factor=", pf_basic_col)
  park_factors <- pf_raw %>%
    transmute(
      pf_team      = .[[pf_team_col]],
      park_factor  = as.numeric(.[[pf_basic_col]]) / 100  # convert 105 -> 1.05
    ) %>%
    filter(!is.na(park_factor))
} else {
  message("  WARNING: park factors unavailable, using 1.00 for all")
  park_factors <- tibble(pf_team = character(), park_factor = numeric())
}

# ============================================================
# 6. TEAM NAME CROSSWALK  (FanGraphs <-> MLB Stats API)
# ============================================================
# MLB Stats API uses full names; FanGraphs uses abbreviations or short names.
# Build from Lahman Teams as the common backbone.

team_xwalk <- tribble(
  ~mlb_name,                      ~fg_name,        ~fg_short,
  "Arizona Diamondbacks",          "Diamondbacks",   "ARI",
  "Atlanta Braves",                "Braves",         "ATL",
  "Baltimore Orioles",             "Orioles",        "BAL",
  "Boston Red Sox",                "Red Sox",        "BOS",
  "Chicago Cubs",                  "Cubs",           "CHC",
  "Chicago White Sox",             "White Sox",      "CWS",
  "Cincinnati Reds",               "Reds",           "CIN",
  "Cleveland Guardians",           "Guardians",      "CLE",
  "Colorado Rockies",              "Rockies",        "COL",
  "Detroit Tigers",                "Tigers",         "DET",
  "Houston Astros",                "Astros",         "HOU",
  "Kansas City Royals",            "Royals",         "KCR",
  "Los Angeles Angels",            "Angels",         "LAA",
  "Los Angeles Dodgers",           "Dodgers",        "LAD",
  "Miami Marlins",                 "Marlins",        "MIA",
  "Milwaukee Brewers",             "Brewers",        "MIL",
  "Minnesota Twins",               "Twins",          "MIN",
  "New York Mets",                 "Mets",           "NYM",
  "New York Yankees",              "Yankees",        "NYY",
  "Oakland Athletics",             "Athletics",      "OAK",
  "Philadelphia Phillies",         "Phillies",       "PHI",
  "Pittsburgh Pirates",            "Pirates",        "PIT",
  "San Diego Padres",              "Padres",         "SDP",
  "San Francisco Giants",          "Giants",         "SFG",
  "Seattle Mariners",              "Mariners",       "SEA",
  "St. Louis Cardinals",           "Cardinals",      "STL",
  "Tampa Bay Rays",                "Rays",           "TBR",
  "Texas Rangers",                 "Rangers",        "TEX",
  "Toronto Blue Jays",             "Blue Jays",      "TOR",
  "Washington Nationals",          "Nationals",      "WSN",
  # Athletics moved 2025; keep both
  "Athletics",                     "Athletics",      "OAK"
)

# Join helper: match FG team name to MLB full name
# FanGraphs names in the data may be abbreviations or partial names
# We'll try both fg_name (long) and fg_short (abbrev) matches.

join_fg_to_mlb <- function(df, fg_col, out_col = "mlb_name_matched") {
  df %>%
    left_join(
      team_xwalk %>% select(fg_name, mlb_name) %>% rename(!!fg_col := fg_name),
      by = fg_col
    ) %>%
    rename(!!out_col := mlb_name) %>%
    # if still NA, try short-name match
    left_join(
      team_xwalk %>% select(fg_short, mlb_name) %>% rename(!!fg_col := fg_short),
      by = fg_col,
      suffix = c("", "_short")
    ) %>%
    mutate(!!out_col := coalesce(.data[[out_col]], mlb_name_short)) %>%
    select(-any_of("mlb_name_short"))
}

# ============================================================
# 7. PULL GAME-LEVEL STARTING PITCHER ASSIGNMENTS
# ============================================================
# The Stats API game feed has boxscore data with starter assignments.
# Pulling all ~7200 games individually is too slow; instead we use the
# season-level probable pitchers endpoint (only works ~1 week out) and
# as a fallback, the Retrosheet/Baseball Reference approach via
# baseballr::retrosheet_game_logs() or mlb_game_logs().
#
# PRACTICAL APPROACH:
# Use the MLB Stats API schedule endpoint which for completed games
# includes the winning/losing pitcher and starting pitcher IDs in the
# decisions field. We then join SP stats from FanGraphs by team-season.
#
# For this backtest we approximate:
#   "team's SP quality for a game" = team's MEDIAN SP FIP across that season
# weighted by IP/GS to represent their rotation's typical starter.
#
# A better approach (game-level SP assignment) is implemented below as
# pull_game_starters(), but is much slower. We default to team-rotation
# median for speed and use game-level as a more accurate alternative.

# --- Fast path: team rotation stats (SP pool median weighted by IP) ---

team_sp_quality <- fg_sp %>%
  group_by(season, sp_team) %>%
  summarise(
    # Weighted median of SP FIP (IP-weighted to upweight more-used starters)
    rotation_FIP       = weighted.mean(sp_FIP,      sp_IP, na.rm = TRUE),
    rotation_xFIP      = weighted.mean(sp_xFIP,     sp_IP, na.rm = TRUE),
    rotation_best_fip  = weighted.mean(sp_best_fip, sp_IP, na.rm = TRUE),
    rotation_IP_per_GS = weighted.mean(sp_IP_per_GS, sp_GS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(fg_team = sp_team)

# ============================================================
# 8. ASSEMBLE GAME-LEVEL DATASET
# ============================================================
# For each game: home_team, away_team, home_score, away_score,
# home SP quality, away SP quality, both lineups wRC+,
# bullpen FIP both sides, park factor.

# First add fg team name to game_results via the crosswalk
game_results2 <- game_results %>%
  left_join(team_xwalk %>% select(fg_short, mlb_name),
            by = c("home_team_name" = "mlb_name")) %>%
  rename(home_fg_abbrev = fg_short) %>%
  left_join(team_xwalk %>% select(fg_short, mlb_name),
            by = c("away_team_name" = "mlb_name")) %>%
  rename(away_fg_abbrev = fg_short) %>%
  left_join(team_xwalk %>% select(fg_name, mlb_name),
            by = c("home_team_name" = "mlb_name")) %>%
  rename(home_fg_name = fg_name) %>%
  left_join(team_xwalk %>% select(fg_name, mlb_name),
            by = c("away_team_name" = "mlb_name")) %>%
  rename(away_fg_name = fg_name)

# Function to join by either abbreviation or long name
join_team_stats <- function(game_df, stats_df, stats_team_col, by_season = TRUE,
                             prefix = "home") {
  fg_col <- paste0(prefix, "_fg_abbrev")
  fg_name_col <- paste0(prefix, "_fg_name")
  mlb_name_col <- paste0(prefix, "_team_name")

  join_keys_abbrev <- if (by_season) {
    setNames(c(stats_team_col, "season"), c(fg_col, "season"))
  } else {
    setNames(stats_team_col, fg_col)
  }

  d <- game_df %>%
    left_join(stats_df, by = join_keys_abbrev, suffix = c("", paste0("_", prefix)))

  # If many NAs, try long name
  na_frac <- mean(is.na(d[[paste0(prefix, "_",
                                   gsub("^.*\\.", "", stats_team_col))]]))
  if (na_frac > 0.3) {
    join_keys_name <- if (by_season) {
      setNames(c(stats_team_col, "season"), c(fg_name_col, "season"))
    } else {
      setNames(stats_team_col, fg_name_col)
    }
    d2 <- game_df %>%
      left_join(stats_df, by = join_keys_name, suffix = c("", paste0("_", prefix)))
    d <- d2
  }
  d
}

# --- Join home-side SP quality ---
home_sp_q <- team_sp_quality %>% rename(home_fg_abbrev = fg_team)
away_sp_q  <- team_sp_quality %>% rename(away_fg_abbrev = fg_team)

# Also try short-name joins (many FG data have 3-letter abbrevs)
# Detect column format used in team_sp_quality
sample_teams <- unique(team_sp_quality$fg_team)[1:5]
message("Sample SP quality team names: ", paste(sample_teams, collapse=", "))

# Direct season join by whatever fg_team contains
game_df <- game_results2 %>%
  # Home SP quality
  left_join(
    team_sp_quality %>%
      rename(home_rotation_FIP      = rotation_FIP,
             home_rotation_xFIP     = rotation_xFIP,
             home_rotation_best_fip = rotation_best_fip,
             home_rotation_IPperGS  = rotation_IP_per_GS),
    by = c("season", "home_fg_abbrev" = "fg_team")
  ) %>%
  # Fallback: try home_fg_name if home_rotation_FIP still NA
  left_join(
    team_sp_quality %>%
      rename(home_rotation_FIP_b      = rotation_FIP,
             home_rotation_xFIP_b     = rotation_xFIP,
             home_rotation_best_fip_b = rotation_best_fip,
             home_rotation_IPperGS_b  = rotation_IP_per_GS),
    by = c("season", "home_fg_name" = "fg_team")
  ) %>%
  mutate(
    home_rotation_FIP      = coalesce(home_rotation_FIP,      home_rotation_FIP_b),
    home_rotation_xFIP     = coalesce(home_rotation_xFIP,     home_rotation_xFIP_b),
    home_rotation_best_fip = coalesce(home_rotation_best_fip, home_rotation_best_fip_b),
    home_rotation_IPperGS  = coalesce(home_rotation_IPperGS,  home_rotation_IPperGS_b)
  ) %>%
  select(-ends_with("_b")) %>%
  # Away SP quality
  left_join(
    team_sp_quality %>%
      rename(away_rotation_FIP      = rotation_FIP,
             away_rotation_xFIP     = rotation_xFIP,
             away_rotation_best_fip = rotation_best_fip,
             away_rotation_IPperGS  = rotation_IP_per_GS),
    by = c("season", "away_fg_abbrev" = "fg_team")
  ) %>%
  left_join(
    team_sp_quality %>%
      rename(away_rotation_FIP_b      = rotation_FIP,
             away_rotation_xFIP_b     = rotation_xFIP,
             away_rotation_best_fip_b = rotation_best_fip,
             away_rotation_IPperGS_b  = rotation_IP_per_GS),
    by = c("season", "away_fg_name" = "fg_team")
  ) %>%
  mutate(
    away_rotation_FIP      = coalesce(away_rotation_FIP,      away_rotation_FIP_b),
    away_rotation_xFIP     = coalesce(away_rotation_xFIP,     away_rotation_xFIP_b),
    away_rotation_best_fip = coalesce(away_rotation_best_fip, away_rotation_best_fip_b),
    away_rotation_IPperGS  = coalesce(away_rotation_IPperGS,  away_rotation_IPperGS_b)
  ) %>%
  select(-ends_with("_b"))

message("After SP join — NAs home_rotation_FIP: ",
        sum(is.na(game_df$home_rotation_FIP)),
        " of ", nrow(game_df))

# --- Join wRC+ ---
bat_h <- fg_batting %>% rename(home_fg_abbrev = fg_team_name, home_wRC_plus = team_wRC_plus)
bat_a <- fg_batting %>% rename(away_fg_abbrev = fg_team_name, away_wRC_plus = team_wRC_plus)

# also long-name versions
bat_h_name <- fg_batting %>% rename(home_fg_name = fg_team_name, home_wRC_plus_n = team_wRC_plus)
bat_a_name <- fg_batting %>% rename(away_fg_name = fg_team_name, away_wRC_plus_n = team_wRC_plus)

game_df <- game_df %>%
  left_join(bat_h, by = c("season", "home_fg_abbrev")) %>%
  left_join(bat_h_name, by = c("season", "home_fg_name")) %>%
  mutate(home_wRC_plus = coalesce(home_wRC_plus, home_wRC_plus_n)) %>%
  select(-home_wRC_plus_n) %>%
  left_join(bat_a, by = c("season", "away_fg_abbrev")) %>%
  left_join(bat_a_name, by = c("season", "away_fg_name")) %>%
  mutate(away_wRC_plus = coalesce(away_wRC_plus, away_wRC_plus_n)) %>%
  select(-away_wRC_plus_n)

message("wRC+ NAs home: ", sum(is.na(game_df$home_wRC_plus)),
        "  away: ", sum(is.na(game_df$away_wRC_plus)))

# --- Join bullpen FIP ---
bp_h <- fg_bp %>% rename(home_fg_abbrev = bp_team, home_bullpen_FIP = bullpen_FIP)
bp_a <- fg_bp %>% rename(away_fg_abbrev = bp_team, away_bullpen_FIP = bullpen_FIP)
bp_h_name <- fg_bp %>% rename(home_fg_name = bp_team, home_bullpen_FIP_n = bullpen_FIP)
bp_a_name <- fg_bp %>% rename(away_fg_name = bp_team, away_bullpen_FIP_n = bullpen_FIP)

game_df <- game_df %>%
  left_join(bp_h, by = c("season", "home_fg_abbrev")) %>%
  left_join(bp_h_name, by = c("season", "home_fg_name")) %>%
  mutate(home_bullpen_FIP = coalesce(home_bullpen_FIP, home_bullpen_FIP_n)) %>%
  select(-any_of("home_bullpen_FIP_n")) %>%
  left_join(bp_a, by = c("season", "away_fg_abbrev")) %>%
  left_join(bp_a_name, by = c("season", "away_fg_name")) %>%
  mutate(away_bullpen_FIP = coalesce(away_bullpen_FIP, away_bullpen_FIP_n)) %>%
  select(-any_of("away_bullpen_FIP_n"))

# --- Join park factors (no season dimension — use latest available) ---
pf_h <- park_factors %>% rename(home_fg_abbrev = pf_team, home_park_factor = park_factor)
pf_a <- park_factors %>% rename(away_fg_abbrev = pf_team, away_park_factor = park_factor)

game_df <- game_df %>%
  left_join(pf_h, by = "home_fg_abbrev") %>%
  left_join(pf_a, by = "away_fg_abbrev") %>%
  mutate(
    home_park_factor = replace_na(home_park_factor, 1.00),
    away_park_factor = replace_na(away_park_factor, 1.00)
  )

# ============================================================
# 9. COMPUTE LEAGUE AVERAGES FROM DATA
# ============================================================

league_avg_runs <- mean(
  c(game_df$home_score, game_df$away_score),
  na.rm = TRUE
)
league_avg_FIP  <- mean(fg_sp$sp_FIP, na.rm = TRUE)
league_avg_xFIP <- mean(fg_sp$sp_xFIP, na.rm = TRUE)
hfa_actual_runs <- mean(game_df$home_score - game_df$away_score, na.rm = TRUE)
home_win_rate   <- mean(game_df$home_win, na.rm = TRUE)

cat("\n========================================\n")
cat("EMPIRICAL LEAGUE CONSTANTS (2022-2024):\n")
cat("========================================\n")
cat(sprintf("  League avg runs/game:  %.3f\n", league_avg_runs))
cat(sprintf("  League avg FIP (SP):   %.3f\n", league_avg_FIP))
cat(sprintf("  League avg xFIP (SP):  %.3f\n", league_avg_xFIP))
cat(sprintf("  Home field adv (runs): %.3f\n", hfa_actual_runs))
cat(sprintf("  Home win rate:         %.3f\n", home_win_rate))
cat("========================================\n\n")

# ============================================================
# 10. FILTER TO COMPLETE CASES & IMPUTE
# ============================================================
# Drop games where any core input is missing.
# Use league averages for remaining NAs.

LG_AVG_RUNS <- league_avg_runs
LG_AVG_FIP  <- league_avg_FIP
LG_AVG_xFIP <- league_avg_xFIP
HFA_RUNS    <- hfa_actual_runs

# Impute missing values with league averages before filtering
game_model <- game_df %>%
  mutate(
    home_rotation_best_fip = replace_na(home_rotation_best_fip, LG_AVG_xFIP),
    away_rotation_best_fip = replace_na(away_rotation_best_fip, LG_AVG_xFIP),
    home_rotation_FIP      = replace_na(home_rotation_FIP,      LG_AVG_FIP),
    away_rotation_FIP      = replace_na(away_rotation_FIP,      LG_AVG_FIP),
    home_rotation_xFIP     = replace_na(home_rotation_xFIP,     LG_AVG_xFIP),
    away_rotation_xFIP     = replace_na(away_rotation_xFIP,     LG_AVG_xFIP),
    home_wRC_plus          = replace_na(home_wRC_plus,          100),
    away_wRC_plus          = replace_na(away_wRC_plus,          100),
    home_bullpen_FIP       = replace_na(home_bullpen_FIP,       LG_AVG_FIP),
    away_bullpen_FIP       = replace_na(away_bullpen_FIP,       LG_AVG_FIP),
    home_rotation_IPperGS  = replace_na(home_rotation_IPperGS,  5.5),
    away_rotation_IPperGS  = replace_na(away_rotation_IPperGS,  5.5),
    # Blended pitcher quality: SP covers ~5.5/9 innings, bullpen covers rest
    # sp_frac = SP innings fraction of 9
    home_sp_frac = pmin(home_rotation_IPperGS / 9, 0.85),
    away_sp_frac = pmin(away_rotation_IPperGS / 9, 0.85),
    home_blended_FIP = home_sp_frac * home_rotation_best_fip +
                       (1 - home_sp_frac) * home_bullpen_FIP,
    away_blended_FIP = away_sp_frac * away_rotation_best_fip +
                       (1 - away_sp_frac) * away_bullpen_FIP
  ) %>%
  filter(!is.na(home_score), !is.na(away_score),
         home_score >= 0, away_score >= 0)

message("Games in model dataset: ", nrow(game_model))

# ============================================================
# 11. POISSON WIN PROBABILITY (vectorized)
# ============================================================

poisson_win_prob_vec <- function(lambda_home, lambda_away, max_r = 25) {
  r <- 0:max_r
  mapply(function(lh, la) {
    lh <- max(lh, 0.01); la <- max(la, 0.01)
    ph <- dpois(r, lh)
    pa <- dpois(r, la)
    # P(home wins) = sum over h of P(H=h) * P(A < h)
    p_home <- sum(ph * c(0, cumsum(pa)[-length(pa)]))  # P(A < h) = CDF at h-1
    p_tie  <- sum(ph * pa)
    p_home + 0.5 * p_tie
  }, lambda_home, lambda_away)
}

# ============================================================
# 12. MODEL DEFINITIONS
# ============================================================

compute_models <- function(df) {
  df %>% mutate(

    # ---- M1: Current model (SP FIP × lineup wRC+ × Poisson) ----
    # Formula from _game_narrative_helpers.R
    m1_home_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (home_wRC_plus / 100) * (away_rotation_best_fip / LG_AVG_xFIP)
      + 0.05  # flat home field bonus
    )),
    m1_away_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (away_wRC_plus / 100) * (home_rotation_best_fip / LG_AVG_xFIP)
    )),
    m1_total     = m1_home_runs + m1_away_runs,
    m1_p_home    = poisson_win_prob_vec(m1_home_runs, m1_away_runs),

    # ---- M2: M1 + park factor (replaces flat home bonus) ----
    # Park factor applied to home runs (both teams scoring in that park)
    m2_home_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (home_wRC_plus / 100) * (away_rotation_best_fip / LG_AVG_xFIP)
      * home_park_factor
      + HFA_RUNS * 0.5   # half of HFA goes to home offense boost
    )),
    m2_away_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (away_wRC_plus / 100) * (home_rotation_best_fip / LG_AVG_xFIP)
      * home_park_factor
    )),
    m2_total     = m2_home_runs + m2_away_runs,
    m2_p_home    = poisson_win_prob_vec(m2_home_runs, m2_away_runs),

    # ---- M3: M2 + blended pitcher quality (SP + bullpen weighted) ----
    m3_home_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (home_wRC_plus / 100) * (away_blended_FIP / LG_AVG_FIP)
      * home_park_factor
      + HFA_RUNS * 0.5
    )),
    m3_away_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (away_wRC_plus / 100) * (home_blended_FIP / LG_AVG_FIP)
      * home_park_factor
    )),
    m3_total     = m3_home_runs + m3_away_runs,
    m3_p_home    = poisson_win_prob_vec(m3_home_runs, m3_away_runs),

    # ---- M4: M3 + full empirical HFA (both home offense and away suppression) ----
    # Split HFA: +0.15 R/game to home offense, +0.12 R/game suppression on away offense
    # (these are starting points; we'll optimize via regression below)
    m4_home_runs = pmax(1.5, pmin(9.5,
      LG_AVG_RUNS * (home_wRC_plus / 100) * (away_blended_FIP / LG_AVG_FIP)
      * home_park_factor
      + HFA_RUNS * 0.6
    )),
    m4_away_runs = pmax(1.5, pmin(9.5,
      (LG_AVG_RUNS - HFA_RUNS * 0.4) * (away_wRC_plus / 100) *
        (home_blended_FIP / LG_AVG_FIP)
      * home_park_factor
    )),
    m4_total     = m4_home_runs + m4_away_runs,
    m4_p_home    = poisson_win_prob_vec(m4_home_runs, m4_away_runs),

    # ---- M5: Log-linear regression (fitted, not formula-based) ----
    # Predict log(runs) as function of wRC+, opposing FIP, park, etc.
    # We'll fit and predict in a separate step below, using cross-validation.
    # Placeholder columns here:
    m5_home_runs = NA_real_,
    m5_away_runs = NA_real_,
    m5_p_home    = NA_real_
  )
}

game_model <- compute_models(game_model)

# ============================================================
# 13. M5: LOG-LINEAR REGRESSION MODEL
# ============================================================
# Fit log(actual runs) ~ log(wRC+) + log(opposing FIP) + log(park factor) + home
# Use leave-one-season-out cross-validation.

fit_log_linear <- function(train_df) {
  # Prepare long format: one row per team per game
  home_long <- train_df %>%
    transmute(
      game_pk,
      side = "home",
      log_runs  = log(pmax(home_score, 0.5)),
      log_wrc   = log(home_wRC_plus / 100),
      log_opp_fip = log(away_blended_FIP),
      log_pf    = log(home_park_factor),
      is_home   = 1L
    )
  away_long <- train_df %>%
    transmute(
      game_pk,
      side = "away",
      log_runs  = log(pmax(away_score, 0.5)),
      log_wrc   = log(away_wRC_plus / 100),
      log_opp_fip = log(home_blended_FIP),
      log_pf    = log(home_park_factor),
      is_home   = 0L
    )
  long_df <- bind_rows(home_long, away_long)
  lm(log_runs ~ log_wrc + log_opp_fip + log_pf + is_home, data = long_df)
}

predict_log_linear <- function(model, test_df) {
  home_pred <- test_df %>%
    transmute(
      game_pk,
      log_wrc     = log(home_wRC_plus / 100),
      log_opp_fip = log(away_blended_FIP),
      log_pf      = log(home_park_factor),
      is_home     = 1L
    )
  away_pred <- test_df %>%
    transmute(
      game_pk,
      log_wrc     = log(away_wRC_plus / 100),
      log_opp_fip = log(home_blended_FIP),
      log_pf      = log(home_park_factor),
      is_home     = 0L
    )
  home_lambda <- exp(predict(model, home_pred))
  away_lambda <- exp(predict(model, away_pred))
  tibble(
    game_pk     = test_df$game_pk,
    m5_home_runs = pmax(1.5, pmin(9.5, home_lambda)),
    m5_away_runs = pmax(1.5, pmin(9.5, away_lambda)),
    m5_p_home   = poisson_win_prob_vec(m5_home_runs, m5_away_runs)
  )
}

# Leave-one-season-out CV
message("Fitting M5 log-linear (LOSO CV)...")
m5_preds <- purrr::map_dfr(SEASONS, function(test_season) {
  train <- game_model %>% filter(season != test_season)
  test  <- game_model %>% filter(season == test_season)
  mod   <- fit_log_linear(train)
  predict_log_linear(mod, test)
})

game_model <- game_model %>%
  select(-m5_home_runs, -m5_away_runs, -m5_p_home) %>%
  left_join(m5_preds, by = "game_pk") %>%
  mutate(m5_total = m5_home_runs + m5_away_runs)

# Also print M5 coefficients (last full-season model)
full_m5 <- fit_log_linear(game_model)
cat("\n=== M5 Log-Linear Coefficients ===\n")
print(summary(full_m5))

# ============================================================
# 14. NEGATIVE BINOMIAL MODEL (M5b)
# ============================================================
# NB is better than Poisson for overdispersed count data (baseball runs are).

if (requireNamespace("MASS", quietly = TRUE)) {
  fit_nb <- function(train_df) {
    home_long <- train_df %>%
      transmute(
        runs    = pmax(home_score, 0L),
        wrc     = home_wRC_plus / 100,
        opp_fip = away_blended_FIP,
        pf      = home_park_factor,
        is_home = 1L
      )
    away_long <- train_df %>%
      transmute(
        runs    = pmax(away_score, 0L),
        wrc     = away_wRC_plus / 100,
        opp_fip = home_blended_FIP,
        pf      = home_park_factor,
        is_home = 0L
      )
    long_df <- bind_rows(home_long, away_long)
    MASS::glm.nb(runs ~ log(wrc) + log(opp_fip) + log(pf) + is_home,
                 data = long_df,
                 control = glm.control(maxit = 50, trace = FALSE))
  }

  predict_nb <- function(model, test_df) {
    home_pred <- test_df %>%
      transmute(game_pk,
                wrc     = home_wRC_plus / 100,
                opp_fip = away_blended_FIP,
                pf      = home_park_factor,
                is_home = 1L)
    away_pred <- test_df %>%
      transmute(game_pk,
                wrc     = away_wRC_plus / 100,
                opp_fip = home_blended_FIP,
                pf      = home_park_factor,
                is_home = 0L)
    home_lambda <- predict(model, home_pred, type = "response")
    away_lambda <- predict(model, away_pred, type = "response")
    tibble(
      game_pk      = test_df$game_pk,
      m5b_home     = pmax(1.5, pmin(9.5, home_lambda)),
      m5b_away     = pmax(1.5, pmin(9.5, away_lambda)),
      m5b_p_home   = poisson_win_prob_vec(m5b_home, m5b_away)
    )
  }

  message("Fitting M5b NB (LOSO CV)...")
  m5b_preds <- purrr::map_dfr(SEASONS, function(test_season) {
    train <- game_model %>% filter(season != test_season)
    test  <- game_model %>% filter(season == test_season)
    mod   <- tryCatch(fit_nb(train), error = function(e) NULL)
    if (is.null(mod)) return(tibble(game_pk = test$game_pk,
                                    m5b_home=NA, m5b_away=NA, m5b_p_home=NA))
    predict_nb(mod, test)
  })
  game_model <- game_model %>% left_join(m5b_preds, by = "game_pk") %>%
    mutate(m5b_total = m5b_home + m5b_away)
} else {
  game_model <- game_model %>%
    mutate(m5b_home=NA_real_, m5b_away=NA_real_, m5b_p_home=NA_real_, m5b_total=NA_real_)
}

# ============================================================
# 15. EVALUATION METRICS
# ============================================================

brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

mae <- function(pred, actual) mean(abs(pred - actual), na.rm = TRUE)

log_loss <- function(p, y, eps = 1e-7) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

# Calibration: bucket win probs into deciles, check actual win rate
calibration_table <- function(p_home, actual_home_win, n_bins = 10) {
  df <- tibble(p = p_home, y = actual_home_win) %>%
    filter(!is.na(p)) %>%
    mutate(bin = cut(p, breaks = seq(0, 1, length.out = n_bins + 1),
                     include.lowest = TRUE, labels = FALSE))
  df %>%
    group_by(bin) %>%
    summarise(
      n           = n(),
      pred_mean   = mean(p),
      actual_rate = mean(y),
      .groups     = "drop"
    ) %>%
    mutate(diff = actual_rate - pred_mean)
}

# Run metrics on complete cases
eval_df <- game_model %>%
  filter(!is.na(home_score), !is.na(away_score))

models <- list(
  M1  = list(p = "m1_p_home", total = "m1_total"),
  M2  = list(p = "m2_p_home", total = "m2_total"),
  M3  = list(p = "m3_p_home", total = "m3_total"),
  M4  = list(p = "m4_p_home", total = "m4_total"),
  M5  = list(p = "m5_p_home", total = "m5_total"),
  M5b = list(p = "m5b_p_home", total = "m5b_total")
)

results <- purrr::imap_dfr(models, function(cols, name) {
  p_col     <- cols$p
  total_col <- cols$total
  if (!p_col %in% names(eval_df)) return(NULL)
  p_val  <- eval_df[[p_col]]
  t_val  <- eval_df[[total_col]]
  valid  <- !is.na(p_val)
  tibble(
    model      = name,
    n_games    = sum(valid),
    brier      = brier_score(p_val, eval_df$home_win),
    log_loss   = log_loss(p_val, eval_df$home_win),
    mae_total  = mae(t_val, eval_df$total_runs),
    mae_home   = mae(eval_df$m1_home_runs,  eval_df$home_score),  # just for M1 diagnostic
    pct_correct= mean(
      (p_val > 0.5) == (eval_df$home_win == 1),
      na.rm = TRUE
    )
  )
})

cat("\n===========================================\n")
cat("MODEL EVALUATION SUMMARY  (2022-2024 backtest)\n")
cat("===========================================\n")
print(results, n = 20)
cat("\n")

# ============================================================
# 16. CALIBRATION TABLES
# ============================================================

cat("\n--- M1 Calibration ---\n")
print(calibration_table(eval_df$m1_p_home, eval_df$home_win))
cat("\n--- M3 Calibration ---\n")
print(calibration_table(eval_df$m3_p_home, eval_df$home_win))
cat("\n--- M5 Calibration ---\n")
print(calibration_table(eval_df$m5_p_home, eval_df$home_win))

# ============================================================
# 17. HOME FIELD ADVANTAGE DEEP DIVE
# ============================================================

hfa_by_season <- eval_df %>%
  group_by(season) %>%
  summarise(
    games      = n(),
    home_win_pct = mean(home_win),
    avg_hfa_runs = mean(home_score - away_score),
    avg_home_runs = mean(home_score),
    avg_away_runs = mean(away_score),
    .groups = "drop"
  )
cat("\n--- Home Field Advantage by Season ---\n")
print(hfa_by_season)

# ============================================================
# 18. OPTIMAL CONSTANTS — REGRESSION APPROACH
# ============================================================
# Regress actual runs on model's predicted runs to find slope/intercept
# corrections. Also find the best LEAGUE_AVG_RUNS to use.

cat("\n--- Regression: actual ~ predicted (M3, home runs) ---\n")
m3_home_lm <- lm(home_score ~ m3_home_runs, data = eval_df)
cat("  Intercept: ", round(coef(m3_home_lm)[1], 3),
    "  Slope: ", round(coef(m3_home_lm)[2], 3), "\n")
cat("  R²: ", round(summary(m3_home_lm)$r.squared, 4), "\n")

cat("\n--- Regression: actual ~ predicted (M3, away runs) ---\n")
m3_away_lm <- lm(away_score ~ m3_away_runs, data = eval_df)
cat("  Intercept: ", round(coef(m3_away_lm)[1], 3),
    "  Slope: ", round(coef(m3_away_lm)[2], 3), "\n")
cat("  R²: ", round(summary(m3_away_lm)$r.squared, 4), "\n")

# ============================================================
# 19. VARIABLE IMPORTANCE
# ============================================================
# Fit a regression of actual home run differential on inputs
# to understand relative importance of each factor.

imp_df <- eval_df %>%
  mutate(
    run_diff   = home_score - away_score,
    wrc_diff   = home_wRC_plus - away_wRC_plus,
    fip_diff   = away_rotation_best_fip - home_rotation_best_fip,
    bp_diff    = away_bullpen_FIP - home_bullpen_FIP,
    park       = home_park_factor
  ) %>%
  filter(!is.na(wrc_diff), !is.na(fip_diff), !is.na(bp_diff))

imp_lm <- lm(run_diff ~ wrc_diff + fip_diff + bp_diff + park, data = imp_df)
cat("\n--- Variable Importance (run differential) ---\n")
print(summary(imp_lm))

# ============================================================
# 20. SAVE RESULTS
# ============================================================

saveRDS(eval_df, file.path(OUT_DIR, "game_model_eval.rds"))
write.csv(results, file.path(OUT_DIR, "model_comparison.csv"), row.names = FALSE)
write.csv(hfa_by_season, file.path(OUT_DIR, "hfa_by_season.csv"), row.names = FALSE)

# Save calibration tables
cal_list <- list(
  M1  = calibration_table(eval_df$m1_p_home, eval_df$home_win),
  M3  = calibration_table(eval_df$m3_p_home, eval_df$home_win),
  M5  = calibration_table(eval_df$m5_p_home, eval_df$home_win)
)
saveRDS(cal_list, file.path(OUT_DIR, "calibration_tables.rds"))

cat("\n========================================\n")
cat("EMPIRICAL CONSTANTS SUMMARY\n")
cat("========================================\n")
cat(sprintf("  Recommended LEAGUE_AVG_RUNS:  %.2f\n", round(LG_AVG_RUNS, 2)))
cat(sprintf("  Recommended LEAGUE_AVG_FIP:   %.2f\n", round(LG_AVG_FIP, 2)))
cat(sprintf("  Recommended LEAGUE_AVG_xFIP:  %.2f\n", round(LG_AVG_xFIP, 2)))
cat(sprintf("  Observed home field (runs):   %.3f\n", HFA_RUNS))
cat(sprintf("  Observed home win rate:       %.3f\n", home_win_rate))
cat("========================================\n\n")

cat("All results saved to: ", OUT_DIR, "\n")
cat("DONE.\n")
