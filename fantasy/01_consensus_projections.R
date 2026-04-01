# ============================================================
# FANTASY BASEBALL — Consensus Projections (Multi-System Average)
# ============================================================
# Pulls 5 FanGraphs projection systems and averages them.
# Systems: Steamer, ZiPS, ATC, FG Depth Charts, THE BAT
#
# OUTPUT:
#   steamer_bat  — consensus batting projections
#   steamer_pit  — consensus pitching projections
# (named steamer_* for compatibility with 02_blend_projections.R)
# ============================================================

source("fantasy/00_fantasy_config.R")

PROJ_SYSTEMS <- c("steamer", "zips", "atc", "fangraphsdc", "thebat")

# ------------------------------------------------------------
# Helper: fetch one FG projections endpoint
# ------------------------------------------------------------

fetch_fg_proj <- function(stats_type, proj_type) {
  url <- paste0(
    "https://www.fangraphs.com/api/projections",
    "?type=", proj_type,
    "&stats=", stats_type,
    "&pos=all&team=0&players=0&lg=all"
  )
  resp <- tryCatch(
    httr::GET(url, httr::timeout(20)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)

  raw <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw)) return(NULL)
  if (is.data.frame(raw) && nrow(raw) > 0) return(raw)
  if (is.list(raw) && "data" %in% names(raw)) return(raw$data)
  NULL
}

# ------------------------------------------------------------
# Helper: extract one system → named numeric tibble keyed on fg_id
# ------------------------------------------------------------

extract_bat_system <- function(raw, system_name) {
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  id_col   <- intersect(c("playerid", "PlayerId"), names(raw))[1]
  name_col <- intersect(c("PlayerName", "Name", "playerName"), names(raw))[1]
  team_col <- intersect(c("Team", "team"), names(raw))[1]
  pos_col  <- intersect(c("Positions", "Position", "Pos", "pos", "position"), names(raw))[1]

  if (is.na(id_col)) return(NULL)

  n <- nrow(raw)
  # Return NA vector of correct length when column is absent (avoids numeric(0) size mismatch)
  safe <- function(col) {
    if (is.na(col) || !col %in% names(raw)) return(rep(NA_real_, n))
    suppressWarnings(as.numeric(raw[[col]]))
  }
  safe_int <- function(col) {
    if (is.na(col) || !col %in% names(raw)) return(rep(NA_integer_, n))
    suppressWarnings(as.integer(raw[[col]]))
  }
  safe_chr <- function(col) {
    if (is.na(col) || !col %in% names(raw)) return(rep(NA_character_, n))
    as.character(raw[[col]])
  }

  wrc_col <- intersect(c("wRC+", "wRC."), names(raw))
  wrc_col <- if (length(wrc_col) > 0) wrc_col[1] else NA_character_

  dplyr::tibble(
    fg_id        = as.character(raw[[id_col]]),
    player_name  = safe_chr(name_col),
    team_abbr    = safe_chr(team_col),
    proj_pos_raw = safe_chr(pos_col),
    pa   = safe_int("PA"),
    ab   = safe_int("AB"),
    h    = safe_int("H"),
    d2b  = safe_int("2B"),
    d3b  = safe_int("3B"),
    hr   = safe_int("HR"),
    r    = safe_int("R"),
    rbi  = safe_int("RBI"),
    sb   = safe_int("SB"),
    cs   = safe_int("CS"),
    bb   = safe_int("BB"),
    hbp  = safe_int("HBP"),
    avg  = safe("AVG"),
    obp  = safe("OBP"),
    slg  = safe("SLG"),
    woba = safe("wOBA"),
    wrc_plus = safe(wrc_col)
  ) %>%
    dplyr::filter(!is.na(fg_id), !is.na(player_name))
}

extract_pit_system <- function(raw, system_name) {
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  id_col   <- intersect(c("playerid", "PlayerId"), names(raw))[1]
  name_col <- intersect(c("PlayerName", "Name", "playerName"), names(raw))[1]
  team_col <- intersect(c("Team", "team"), names(raw))[1]

  if (is.na(id_col)) return(NULL)

  n <- nrow(raw)
  safe <- function(col) {
    if (is.na(col) || !col %in% names(raw)) return(rep(NA_real_, n))
    suppressWarnings(as.numeric(raw[[col]]))
  }
  safe_int <- function(col) {
    if (is.na(col) || !col %in% names(raw)) return(rep(NA_integer_, n))
    suppressWarnings(as.integer(raw[[col]]))
  }
  safe_chr <- function(col) {
    if (is.na(col) || !col %in% names(raw)) return(rep(NA_character_, n))
    as.character(raw[[col]])
  }

  k_col  <- intersect(c("SO", "K"),          names(raw))
  k9_col <- intersect(c("K.9", "K9", "SO9"), names(raw))
  k_col  <- if (length(k_col)  > 0) k_col[1]  else NA_character_
  k9_col <- if (length(k9_col) > 0) k9_col[1] else NA_character_

  dplyr::tibble(
    fg_id       = as.character(raw[[id_col]]),
    player_name = safe_chr(name_col),
    team_abbr   = safe_chr(team_col),
    ip   = safe("IP"),
    gs   = safe_int("GS"),
    g    = safe_int("G"),
    w    = safe_int("W"),
    sv   = safe_int("SV"),
    k    = safe_int(k_col),
    bb   = safe_int("BB"),
    era  = safe("ERA"),
    whip = safe("WHIP"),
    fip  = safe("FIP"),
    xfip = safe("xFIP"),
    k9   = safe(k9_col)
  ) %>%
    dplyr::filter(!is.na(fg_id), !is.na(player_name))
}

# ------------------------------------------------------------
# Pull all systems
# ------------------------------------------------------------

message("Pulling batting projections from ", length(PROJ_SYSTEMS), " systems...")

bat_systems <- list()
pit_systems <- list()

for (sys in PROJ_SYSTEMS) {
  message("  ", sys, "...")
  bat_raw <- fetch_fg_proj("bat", sys)
  pit_raw <- fetch_fg_proj("pit", sys)

  bat_df <- extract_bat_system(bat_raw, sys)
  pit_df <- extract_pit_system(pit_raw, sys)

  if (!is.null(bat_df)) {
    message("    batters: ", nrow(bat_df))
    bat_systems[[sys]] <- bat_df
  } else {
    message("    batters: FAILED")
  }

  if (!is.null(pit_df)) {
    message("    pitchers: ", nrow(pit_df))
    pit_systems[[sys]] <- pit_df
  } else {
    message("    pitchers: FAILED")
  }

  Sys.sleep(0.5)  # be polite to FG API
}

message("Systems with data — bat: ", length(bat_systems), " | pit: ", length(pit_systems))

# ------------------------------------------------------------
# Average across systems
# ------------------------------------------------------------

average_systems <- function(system_list, id_cols = c("fg_id","player_name","team_abbr","proj_pos_raw")) {
  if (length(system_list) == 0) stop("No projection systems returned data.")

  # Stack all systems, tag with source
  all <- dplyr::bind_rows(
    lapply(names(system_list), function(nm) {
      system_list[[nm]] %>% dplyr::mutate(.system = nm)
    })
  )

  numeric_cols <- setdiff(names(all), c(id_cols, ".system", "proj_pos_raw"))
  keep_id_cols <- intersect(id_cols, names(all))

  # Average numeric cols per player (fg_id), take first non-NA for metadata
  averaged <- all %>%
    dplyr::group_by(fg_id) %>%
    dplyr::summarise(
      # Metadata from whichever system has it
      dplyr::across(dplyr::any_of(setdiff(keep_id_cols, "fg_id")),
                    ~ dplyr::first(na.omit(.x))),
      dplyr::across(dplyr::any_of(setdiff(numeric_cols, keep_id_cols)),
                    ~ round(mean(.x, na.rm = TRUE), 2)),
      n_systems = dplyr::n_distinct(.system),
      .groups = "drop"
    )

  averaged
}

bat_avg <- average_systems(bat_systems,
  id_cols = c("fg_id","player_name","team_abbr","proj_pos_raw"))
pit_avg <- average_systems(pit_systems,
  id_cols = c("fg_id","player_name","team_abbr"))

message("Consensus batting: ", nrow(bat_avg), " players (avg across ",
        max(bat_avg$n_systems, na.rm=TRUE), " systems)")
message("Consensus pitching: ", nrow(pit_avg), " players (avg across ",
        max(pit_avg$n_systems, na.rm=TRUE), " systems)")

# ── Check coverage for known players ─────────────────────────────────────────
# Checks raw per-system data first, then the averaged output.
# This pinpoints exactly where a player disappears in the pipeline.
check_bat <- c("Baldwin", "Acuna", "Rodriguez", "Soto", "Judge")
for (nm in check_bat) {
  # Search raw systems
  sys_hits <- lapply(names(bat_systems), function(sys) {
    rows <- bat_systems[[sys]] %>%
      dplyr::filter(grepl(nm, player_name, ignore.case=TRUE))
    if (nrow(rows) > 0)
      data.frame(sys=sys, name=rows$player_name[1], fg_id=rows$fg_id[1],
                 pa=rows$pa[1], stringsAsFactors=FALSE)
    else NULL
  })
  sys_hits <- dplyr::bind_rows(Filter(Negate(is.null), sys_hits))

  if (nrow(sys_hits) == 0) {
    message("  PROJ ", nm, ": NOT IN ANY system — player missing from FG API entirely")
    message("    >> Manual add needed or check FG player page for correct fg_id")
  } else {
    message("  PROJ ", nm, ": found in ", nrow(sys_hits), "/", length(bat_systems),
            " systems — fg_ids: ", paste(unique(sys_hits$fg_id), collapse=","),
            " | PA: ", paste(round(sys_hits$pa), collapse=","))
    # Check if it made it through averaging
    in_avg <- bat_avg %>% dplyr::filter(grepl(nm, player_name, ignore.case=TRUE))
    if (nrow(in_avg) == 0) {
      message("    >> DROPPED in average_systems — likely fg_id mismatch across systems")
    } else {
      message("    >> In bat_avg: PA=", round(in_avg$pa[1]), " n_sys=", in_avg$n_systems[1])
    }
  }
}

# ------------------------------------------------------------
# Join to mlbam_id + format to match downstream scripts
# ------------------------------------------------------------

id_map <- player_master_ids %>%
  dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
  dplyr::distinct(fg_id, .keep_all = TRUE) %>%
  dplyr::select(fg_id, mlbam_id) %>%
  dplyr::mutate(fg_id = as.character(fg_id))

est_qs_per_gs <- function(era) {
  dplyr::case_when(
    era < 3.00 ~ 0.65, era < 3.50 ~ 0.58, era < 4.00 ~ 0.50,
    era < 4.50 ~ 0.42, era < 5.00 ~ 0.33, TRUE ~ 0.18
  )
}

steamer_bat <- bat_avg %>%
  dplyr::left_join(id_map, by = "fg_id") %>%
  dplyr::transmute(
    fg_id, mlbam_id = as.integer(mlbam_id), player_name, team_abbr, proj_pos_raw,
    proj_pa      = as.integer(round(pa)),
    proj_ab      = as.integer(round(ab)),
    proj_h       = as.integer(round(h)),
    proj_2b      = as.integer(round(d2b)),
    proj_3b      = as.integer(round(d3b)),
    proj_hr      = as.integer(round(hr)),
    proj_r       = as.integer(round(r)),
    proj_rbi     = as.integer(round(rbi)),
    proj_sb      = as.integer(round(sb)),
    proj_cs      = as.integer(round(dplyr::coalesce(cs, 0))),
    proj_bb      = as.integer(round(bb)),
    proj_hbp     = as.integer(round(dplyr::coalesce(hbp, 0))),
    proj_avg     = round(avg, 3),
    proj_obp     = round(obp, 3),
    proj_slg     = round(slg, 3),
    proj_woba    = round(woba, 3),
    proj_wrc_plus= round(wrc_plus),
    n_systems
  )

steamer_pit <- pit_avg %>%
  dplyr::left_join(id_map, by = "fg_id") %>%
  dplyr::mutate(
    proj_er  = round(dplyr::coalesce(era, 4.2) * dplyr::coalesce(ip, 0) / 9, 1),
    proj_qs  = round(dplyr::coalesce(as.numeric(gs), 0) *
                     est_qs_per_gs(dplyr::coalesce(era, 4.5))),
    proj_role = dplyr::case_when(
      !is.na(gs) & gs >= 5     ~ "SP",
      !is.na(sv) & sv >= 5     ~ "RP_closer",
      TRUE                      ~ "RP"
    )
  ) %>%
  dplyr::transmute(
    fg_id, mlbam_id = as.integer(mlbam_id), player_name, team_abbr,
    proj_ip    = round(dplyr::coalesce(ip, 0), 1),
    proj_gs    = as.integer(round(dplyr::coalesce(gs, 0))),
    proj_g     = as.integer(round(dplyr::coalesce(g, 0))),
    proj_w     = as.integer(round(dplyr::coalesce(w, 0))),
    proj_sv    = as.integer(round(dplyr::coalesce(sv, 0))),
    proj_k     = as.integer(round(dplyr::coalesce(k, 0))),
    proj_bb_pit= as.integer(round(dplyr::coalesce(bb, 0))),
    proj_era   = round(dplyr::coalesce(era, 4.2), 2),
    proj_whip  = round(dplyr::coalesce(whip, 1.3), 2),
    proj_fip   = round(fip, 2),
    proj_xfip  = round(xfip, 2),
    proj_k9    = round(k9, 2),
    proj_er, proj_qs, proj_role,
    n_systems
  )

# ── Manual player overrides ───────────────────────────────────────────────────
# Add players here that the FG API misses. Find fg_id on their FanGraphs page:
# fangraphs.com/players/[name]/[fg_id]/stats/bat
# These rows are appended AFTER averaging so they never get overwritten.
manual_bat_path <- "fantasy/manual_players_bat.csv"
if (file.exists(manual_bat_path)) {
  manual_bat <- readr::read_csv(manual_bat_path, show_col_types = FALSE)
  # Only add players not already in steamer_bat
  new_players <- manual_bat %>%
    dplyr::filter(
      !fg_id %in% steamer_bat$fg_id,
      !grepl("^REPLACE", fg_id),   # skip unfilled placeholder rows
      !is.na(fg_id)
    ) %>%
    dplyr::mutate(
      n_systems = 0L,
      mlbam_id  = suppressWarnings(as.integer(mlbam_id)),
      fg_id     = as.character(fg_id)
    )
  if (nrow(new_players) > 0) {
    message("Manual bat overrides added: ",
            paste(new_players$player_name, collapse=", "))
    steamer_bat <- dplyr::bind_rows(steamer_bat, new_players)
  }
}

# ── Automatic gap-fill from last season's actuals ────────────────────────────
# Any batter with 150+ real PA last year who isn't in the projection systems
# gets added using their actual stats × 0.85 (light regression toward mean).
# This catches breakout players, new regulars, and API coverage gaps.
if (exists("offense_master_season") && exists("player_master_ids")) {

  recent_season <- max(offense_master_season$season, na.rm = TRUE)
  message("Gap-fill: checking ", recent_season, " actuals for missing players...")

  # Name lookup — player_master_ids only used for player_name (joined on mlbam_id)
  # fg_id comes from offense_master_season itself (populated by the FG leaderboard
  # pull with qual="0", which covers every player with a PA including 2025 rookies)
  name_lookup <- player_master_ids %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
    dplyr::select(mlbam_id, player_name)

  # Players with meaningful PA last year not already in projection systems
  actual_starters <- offense_master_season %>%
    dplyr::filter(season == recent_season,
                  dplyr::coalesce(mlb_pa, 0L) >= 100,
                  !is.na(fg_id),
                  !as.character(fg_id) %in% steamer_bat$fg_id) %>%
    dplyr::left_join(name_lookup, by = "mlbam_id")

  if (nrow(actual_starters) == 0) {
    message("  Gap-fill: all 150+ PA players already in projections.")
  } else {
    REGRESS <- 0.85   # regress actuals 15% toward the mean for projection
    wrc_col <- intersect(c("fg_wRC_plus","fg_wRC.","fg_wRC_plus."), names(actual_starters))
    # fg_batter_leaders renames "Pos" → "fg_Pos" after the fg_ prefix pass
    pos_col <- intersect(c("fg_Pos","fg_position","fg_Position","position"), names(actual_starters))

    gap_bat <- actual_starters %>%
      dplyr::transmute(
        fg_id         = as.character(fg_id),
        mlbam_id      = as.integer(mlbam_id),
        player_name,
        team_abbr,
        proj_pos_raw  = if (length(pos_col) > 0) .data[[pos_col[1]]] else NA_character_,
        proj_pa       = as.integer(round(dplyr::coalesce(mlb_pa,  0L) * REGRESS)),
        proj_ab       = as.integer(round(dplyr::coalesce(mlb_ab,  0L) * REGRESS)),
        proj_h        = as.integer(round(dplyr::coalesce(mlb_h,   0L) * REGRESS)),
        proj_2b       = as.integer(round(dplyr::coalesce(mlb_2b,  0L) * REGRESS)),
        proj_3b       = as.integer(round(dplyr::coalesce(mlb_3b,  0L) * REGRESS)),
        proj_hr       = as.integer(round(dplyr::coalesce(mlb_hr,  0L) * REGRESS)),
        proj_r        = as.integer(round(dplyr::coalesce(mlb_r,   0L) * REGRESS)),
        proj_rbi      = as.integer(round(dplyr::coalesce(mlb_rbi, 0L) * REGRESS)),
        proj_sb       = as.integer(round(dplyr::coalesce(mlb_sb,  0L) * REGRESS)),
        proj_cs       = as.integer(round(dplyr::coalesce(mlb_cs,  0L) * REGRESS)),
        proj_bb       = as.integer(round(dplyr::coalesce(mlb_bb,  0L) * REGRESS)),
        proj_hbp      = as.integer(round(dplyr::coalesce(mlb_hbp, 0L) * REGRESS)),
        proj_avg      = round(dplyr::coalesce(mlb_avg, 0), 3),
        proj_obp      = round(dplyr::coalesce(mlb_obp, 0), 3),
        proj_slg      = round(dplyr::coalesce(mlb_slg, 0), 3),
        proj_woba     = NA_real_,
        proj_wrc_plus = if (length(wrc_col) > 0) round(.data[[wrc_col[1]]]) else NA_real_,
        n_systems     = -1L   # flag: gap-filled from actuals, not projection system
      )

    message("  Gap-fill: adding ", nrow(gap_bat), " players missing from FG API:")
    message("    ", paste(gap_bat$player_name, collapse = ", "))
    steamer_bat <- dplyr::bind_rows(steamer_bat, gap_bat)
  }
} else {
  message("Gap-fill skipped: offense_master_season or player_master_ids not loaded")
}

message("01_consensus_projections complete — ",
        length(PROJ_SYSTEMS), " systems pulled and averaged.")
