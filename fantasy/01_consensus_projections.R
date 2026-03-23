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
  pos_col  <- intersect(c("Positions", "Position", "pos"), names(raw))[1]

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
check_bat <- c("Baldwin", "Acuna", "Rodriguez", "Soto", "Judge")
for (nm in check_bat) {
  rows <- bat_avg %>% dplyr::filter(grepl(nm, player_name, ignore.case=TRUE))
  if (nrow(rows) > 0) {
    message("  PROJ ", nm, ": ", rows$player_name[1],
            " PA=", round(rows$pa[1]), " n_sys=", rows$n_systems[1])
  } else {
    # Check which individual systems had them
    found_in <- names(bat_systems)[sapply(bat_systems, function(s)
      any(grepl(nm, s$player_name, ignore.case=TRUE)))]
    message("  PROJ ", nm, ": MISSING from bat_avg — found in: ",
            if (length(found_in)) paste(found_in, collapse=",") else "NONE")
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

message("01_consensus_projections complete — ",
        length(PROJ_SYSTEMS), " systems pulled and averaged.")
