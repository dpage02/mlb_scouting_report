# ============================================================
# FANTASY BASEBALL — Pull Steamer 2026 Projections
# ============================================================
# SOURCE: FanGraphs projections API (Steamer system)
# OUTPUT:
#   steamer_bat  — projected batting stats per player
#   steamer_pit  — projected pitching stats per player
#
# Both tables include fg_id for joining to player_master_ids.
# ============================================================

source("fantasy/00_fantasy_config.R")

# ------------------------------------------------------------
# Helper: fetch one projections endpoint
# ------------------------------------------------------------

fetch_fg_projections <- function(stats_type, proj_type = "steamer") {
  url <- paste0(
    "https://www.fangraphs.com/api/projections",
    "?type=", proj_type,
    "&stats=", stats_type,
    "&pos=all&team=0&players=0&lg=all"
  )

  resp <- tryCatch(
    httr::GET(url, httr::timeout(30)),
    error = function(e) {
      message("FG projections fetch failed: ", e$message)
      NULL
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("Non-200 response from FanGraphs projections API for ", stats_type)
    return(NULL)
  }

  raw <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       flatten = TRUE),
    error = function(e) {
      message("JSON parse failed: ", e$message)
      NULL
    }
  )

  if (is.null(raw) || length(raw) == 0) return(NULL)

  # fromJSON may return a data.frame or a list with a data element
  if (is.data.frame(raw)) return(raw)
  if (is.list(raw) && "data" %in% names(raw) && is.data.frame(raw$data)) return(raw$data)

  message("Unexpected structure from FG projections API for ", stats_type)
  NULL
}

# ------------------------------------------------------------
# Pull batting projections
# ------------------------------------------------------------

message("Pulling Steamer batting projections...")
bat_raw <- fetch_fg_projections("bat", "steamer")

if (is.null(bat_raw) || nrow(bat_raw) == 0) {
  message("Steamer bat failed — trying ATC projections...")
  bat_raw <- fetch_fg_projections("bat", "atc")
}

if (is.null(bat_raw) || nrow(bat_raw) == 0) {
  stop("Could not retrieve batting projections from FanGraphs.")
}

message("Batting projections: ", nrow(bat_raw), " players, ",
        ncol(bat_raw), " columns")

# ------------------------------------------------------------
# Clean batting projections
# ------------------------------------------------------------

# Standardize ID column — FG uses "playerid" or "PlayerId"
id_col_bat <- intersect(c("playerid", "PlayerId", "xMLBAMID"), names(bat_raw))[1]
name_col   <- intersect(c("PlayerName", "Name", "playerName"), names(bat_raw))[1]
team_col   <- intersect(c("Team", "team"), names(bat_raw))[1]
pos_col    <- intersect(c("Positions", "Position", "pos"), names(bat_raw))[1]

steamer_bat <- bat_raw %>%
  dplyr::rename(
    fg_id       = dplyr::all_of(id_col_bat),
    player_name = dplyr::all_of(name_col)
  ) %>%
  dplyr::mutate(fg_id = as.character(fg_id)) %>%
  # Join to get mlbam_id
  dplyr::left_join(
    player_master_ids %>%
      dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
      dplyr::distinct(fg_id, .keep_all = TRUE) %>%
      dplyr::select(fg_id, mlbam_id) %>%
      dplyr::mutate(fg_id = as.character(fg_id)),
    by = "fg_id"
  ) %>%
  dplyr::mutate(
    team_abbr    = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_,
    proj_pos_raw = if (!is.na(pos_col))  as.character(.data[[pos_col]])  else NA_character_
  ) %>%
  # Select and rename core projection columns
  dplyr::transmute(
    fg_id,
    mlbam_id    = as.integer(mlbam_id),
    player_name,
    team_abbr,
    proj_pos_raw,
    # Counting projections
    proj_pa     = suppressWarnings(as.integer(dplyr::coalesce(
                    tryCatch(.data[["PA"]], error = function(e) NA_real_),
                    NA_real_))),
    proj_ab     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["AB"]]),  error = function(e) NA_integer_))),
    proj_h      = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["H"]]),   error = function(e) NA_integer_))),
    proj_2b     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["2B"]]),  error = function(e) NA_integer_))),
    proj_3b     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["3B"]]),  error = function(e) NA_integer_))),
    proj_hr     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["HR"]]),  error = function(e) NA_integer_))),
    proj_r      = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["R"]]),   error = function(e) NA_integer_))),
    proj_rbi    = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["RBI"]]), error = function(e) NA_integer_))),
    proj_sb     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["SB"]]),  error = function(e) NA_integer_))),
    proj_cs     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["CS"]]),  error = function(e) NA_integer_))),
    proj_bb     = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["BB"]]),  error = function(e) NA_integer_))),
    proj_hbp    = suppressWarnings(as.integer(tryCatch(as.numeric(.data[["HBP"]]), error = function(e) NA_integer_))),
    # Rate projections
    proj_avg    = suppressWarnings(as.numeric(tryCatch(.data[["AVG"]], error = function(e) NA_real_))),
    proj_obp    = suppressWarnings(as.numeric(tryCatch(.data[["OBP"]], error = function(e) NA_real_))),
    proj_slg    = suppressWarnings(as.numeric(tryCatch(.data[["SLG"]], error = function(e) NA_real_))),
    proj_woba   = suppressWarnings(as.numeric(tryCatch(.data[["wOBA"]], error = function(e) NA_real_))),
    proj_wrc_plus = suppressWarnings(as.numeric(tryCatch(
                      dplyr::coalesce(.data[["wRC+"]], .data[["wRC."]]),
                      error = function(e) NA_real_)))
  ) %>%
  dplyr::filter(!is.na(player_name))

message("steamer_bat: ", nrow(steamer_bat), " players")

# ------------------------------------------------------------
# Pull pitching projections
# ------------------------------------------------------------

message("Pulling Steamer pitching projections...")
pit_raw <- fetch_fg_projections("pit", "steamer")

if (is.null(pit_raw) || nrow(pit_raw) == 0) {
  message("Steamer pit failed — trying ATC...")
  pit_raw <- fetch_fg_projections("pit", "atc")
}

if (is.null(pit_raw) || nrow(pit_raw) == 0) {
  stop("Could not retrieve pitching projections from FanGraphs.")
}

message("Pitching projections: ", nrow(pit_raw), " players, ",
        ncol(pit_raw), " columns")

# ------------------------------------------------------------
# Clean pitching projections
# ------------------------------------------------------------

id_col_pit  <- intersect(c("playerid", "PlayerId"), names(pit_raw))[1]
name_col_p  <- intersect(c("PlayerName", "Name", "playerName"), names(pit_raw))[1]
team_col_p  <- intersect(c("Team", "team"), names(pit_raw))[1]

# Estimate QS from GS + ERA if not provided directly
# QS% empirical lookup: ERA → estimated QS per GS
est_qs_per_gs <- function(era) {
  dplyr::case_when(
    era < 3.00 ~ 0.65,
    era < 3.50 ~ 0.58,
    era < 4.00 ~ 0.50,
    era < 4.50 ~ 0.42,
    era < 5.00 ~ 0.33,
    TRUE        ~ 0.18
  )
}

steamer_pit <- pit_raw %>%
  dplyr::rename(
    fg_id       = dplyr::all_of(id_col_pit),
    player_name = dplyr::all_of(name_col_p)
  ) %>%
  dplyr::mutate(fg_id = as.character(fg_id)) %>%
  dplyr::left_join(
    player_master_ids %>%
      dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
      dplyr::distinct(fg_id, .keep_all = TRUE) %>%
      dplyr::select(fg_id, mlbam_id) %>%
      dplyr::mutate(fg_id = as.character(fg_id)),
    by = "fg_id"
  ) %>%
  dplyr::mutate(
    team_abbr = if (!is.na(team_col_p)) as.character(.data[[team_col_p]]) else NA_character_
  ) %>%
  dplyr::transmute(
    fg_id,
    mlbam_id    = as.integer(mlbam_id),
    player_name,
    team_abbr,
    # Workload
    proj_ip     = suppressWarnings(as.numeric(tryCatch(.data[["IP"]], error = function(e) NA_real_))),
    proj_gs     = suppressWarnings(as.integer(tryCatch(.data[["GS"]], error = function(e) NA_integer_))),
    proj_g      = suppressWarnings(as.integer(tryCatch(.data[["G"]],  error = function(e) NA_integer_))),
    proj_sv     = suppressWarnings(as.integer(tryCatch(.data[["SV"]], error = function(e) NA_integer_))),
    proj_w      = suppressWarnings(as.integer(tryCatch(.data[["W"]],  error = function(e) NA_integer_))),
    proj_k      = suppressWarnings(as.integer(tryCatch(.data[["SO"]], error = function(e)
                    suppressWarnings(as.integer(tryCatch(.data[["K"]], error = function(e2) NA_integer_)))))),
    proj_bb_pit = suppressWarnings(as.integer(tryCatch(.data[["BB"]], error = function(e) NA_integer_))),
    # Rates
    proj_era    = suppressWarnings(as.numeric(tryCatch(.data[["ERA"]], error = function(e) NA_real_))),
    proj_whip   = suppressWarnings(as.numeric(tryCatch(.data[["WHIP"]], error = function(e) NA_real_))),
    proj_fip    = suppressWarnings(as.numeric(tryCatch(.data[["FIP"]], error = function(e) NA_real_))),
    proj_xfip   = suppressWarnings(as.numeric(tryCatch(.data[["xFIP"]], error = function(e) NA_real_))),
    proj_k9     = suppressWarnings(as.numeric(tryCatch(
                    dplyr::coalesce(.data[["K.9"]], .data[["K9"]], .data[["SO9"]]),
                    error = function(e) NA_real_))),
    # Derived
    proj_er     = round(proj_era * proj_ip / 9, 1),
    # QS: use directly if available, otherwise estimate from ERA × GS
    proj_qs     = suppressWarnings(as.numeric(tryCatch(.data[["QS"]], error = function(e) NA_real_))),
    proj_qs     = dplyr::if_else(
                    !is.na(proj_qs),
                    proj_qs,
                    round(dplyr::coalesce(as.numeric(proj_gs), 0) *
                          est_qs_per_gs(dplyr::coalesce(proj_era, 4.5)))
                  ),
    # Pitcher role: SP if GS > G/2, RP otherwise
    proj_role   = dplyr::case_when(
                    !is.na(proj_gs) & proj_gs >= 5            ~ "SP",
                    !is.na(proj_sv) & proj_sv >= 5            ~ "RP_closer",
                    TRUE                                        ~ "RP"
                  )
  ) %>%
  dplyr::filter(!is.na(player_name))

message("steamer_pit: ", nrow(steamer_pit), " players")
message("01_steamer_projections complete.")
