# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 07_bbref_pitching_advanced.R
# ============================================================
# PURPOSE:
#   Scrape Baseball Reference standard pitching leaderboard for
#   ERA+, bWAR, and rate stats not available via baseballr APIs.
#
#   bref_daily_pitcher() does not expose ERA+ or WAR.
#   This script fills that gap by scraping the season summary page.
#
# DATA SOURCE:
#   baseball-reference.com
#   URL: /leagues/majors/{year}-standard-pitching.shtml
#
# OUTPUT:
#   player_season_bbref_pitching_advanced
#
# GRAIN:
#   One row per mlbam_id per season
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

season_to_pull <- unique(player_season_mlb_pitching$season)[1]

# ------------------------------------------------------------
# Scrape helper
# BBRef sometimes wraps tables in HTML comments for lazy loading.
# We strip comments before parsing so rvest can find the table.
# ------------------------------------------------------------

scrape_bbref_pitching_standard <- function(season_val) {

  url <- paste0(
    "https://www.baseball-reference.com/leagues/majors/",
    season_val,
    "-standard-pitching.shtml"
  )

  message("Scraping BBRef standard pitching: ", url)
  Sys.sleep(3)  # BBRef rate limit courtesy delay

  page_raw <- tryCatch(
    httr::GET(url, httr::timeout(60)),
    error = function(e) {
      message("BBRef HTTP request failed: ", e$message)
      NULL
    }
  )

  if (is.null(page_raw) || httr::status_code(page_raw) != 200) {
    message("BBRef returned status: ",
            if (is.null(page_raw)) "NULL" else httr::status_code(page_raw))
    return(NULL)
  }

  html_text <- httr::content(page_raw, as = "text", encoding = "UTF-8")

  # Strip HTML comments so rvest can find commented-out tables
  html_text <- gsub("<!--", "", html_text, fixed = TRUE)
  html_text <- gsub("-->",  "", html_text, fixed = TRUE)

  page <- tryCatch(
    rvest::read_html(html_text),
    error = function(e) {
      message("rvest parse failed: ", e$message)
      NULL
    }
  )
  if (is.null(page)) return(NULL)

  tbl_node <- rvest::html_element(page, "#players_standard_pitching")
  if (length(tbl_node) == 0 || is.na(tbl_node)) {
    message("Could not find #players_standard_pitching table for ", season_val)
    return(NULL)
  }

  # Parse the table
  tbl <- tryCatch(
    rvest::html_table(tbl_node, fill = TRUE),
    error = function(e) {
      message("html_table parse failed: ", e$message)
      NULL
    }
  )
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

  # Extract bbref_id from player <a> href links
  # href format: "/players/m/mcclas01.shtml" → extract slug "mcclas01"
  player_links <- tryCatch({
    tbl_node %>%
      rvest::html_elements("td[data-stat='player'] a") %>%
      rvest::html_attr("href")
  }, error = function(e) character(0))

  bbref_ids <- stringr::str_extract(player_links, "[^/]+(?=\\.shtml$)")

  message("BBRef table columns: ", paste(names(tbl), collapse = ", "))

  # BBRef can name the player column "Name" or "Player" depending on the page.
  # Find it dynamically — it's the first non-numeric text column.
  name_col <- names(tbl)[which(names(tbl) %in% c("Name", "Player"))[1]]
  if (is.na(name_col)) name_col <- names(tbl)[2]  # fallback: second column

  rk_col <- names(tbl)[which(names(tbl) == "Rk")[1]]

  # Remove repeated header rows (BBRef inserts header rows every ~20 rows)
  tbl_clean <- tbl %>%
    dplyr::filter(
      !is.na(.data[[name_col]]),
      .data[[name_col]] != "",
      .data[[name_col]] != "Name",
      .data[[name_col]] != "Player"
    )

  if (!is.na(rk_col)) {
    tbl_clean <- tbl_clean %>%
      dplyr::filter(.data[[rk_col]] != "Rk", .data[[rk_col]] != "")
  }

  # Standardize player column to "Name" for downstream code
  if (name_col != "Name") {
    tbl_clean <- dplyr::rename(tbl_clean, Name = dplyr::all_of(name_col))
  }

  # Attach bbref_id — link count should match data row count
  if (length(bbref_ids) == nrow(tbl_clean)) {
    tbl_clean$bbref_id <- bbref_ids
  } else {
    message("bbref_id link count (", length(bbref_ids), ") != ",
            "table row count (", nrow(tbl_clean), "). IDs not attached.")
    tbl_clean$bbref_id <- NA_character_
  }

  tbl_clean$scraped_season <- as.integer(season_val)
  tbl_clean
}

# ------------------------------------------------------------
# Pull — try current season, fall back one year
# ------------------------------------------------------------

bbref_raw <- scrape_bbref_pitching_standard(season_to_pull)

if (is.null(bbref_raw) || nrow(bbref_raw) == 0) {
  message("BBRef scrape empty for ", season_to_pull,
          ". Trying ", season_to_pull - 1, "...")
  season_to_pull <- season_to_pull - 1
  bbref_raw <- scrape_bbref_pitching_standard(season_to_pull)
}

# ------------------------------------------------------------
# Build canonical table
# ------------------------------------------------------------

if (is.null(bbref_raw) || nrow(bbref_raw) == 0) {

  message("BBRef advanced pitching scrape failed entirely. Creating empty table.")
  player_season_bbref_pitching_advanced <- dplyr::tibble(
    mlbam_id = integer(),
    season   = integer()
  )

} else {

  message("BBRef raw columns: ", paste(names(bbref_raw), collapse = ", "))

  # Rename BBRef column names to our standard
  col_map <- c(
    "ERA+"  = "bbref_ERA_plus",
    "FIP"   = "bbref_FIP",
    "H9"    = "bbref_H9",
    "HR9"   = "bbref_HR9",
    "BB9"   = "bbref_BB9",
    "SO9"   = "bbref_SO9",
    "SO/W"  = "bbref_SO_W",
    "WAR"   = "bbref_WAR"
  )

  for (orig in names(col_map)) {
    if (orig %in% names(bbref_raw)) {
      names(bbref_raw)[names(bbref_raw) == orig] <- col_map[[orig]]
    }
  }

  safe_bbref_num <- function(df, col) {
    if (!col %in% names(df)) return(rep(NA_real_, nrow(df)))
    suppressWarnings(as.numeric(df[[col]]))
  }

  bbref_processed <- bbref_raw %>%
    dplyr::mutate(
      bbref_ERA_plus = safe_bbref_num(., "bbref_ERA_plus"),
      bbref_FIP      = safe_bbref_num(., "bbref_FIP"),
      bbref_H9       = safe_bbref_num(., "bbref_H9"),
      bbref_HR9      = safe_bbref_num(., "bbref_HR9"),
      bbref_BB9      = safe_bbref_num(., "bbref_BB9"),
      bbref_SO9      = safe_bbref_num(., "bbref_SO9"),
      bbref_SO_W     = safe_bbref_num(., "bbref_SO_W"),
      bbref_WAR      = safe_bbref_num(., "bbref_WAR"),
      season         = as.integer(scraped_season)
    )

  # Join to mlbam_id via bbref_id
  player_season_bbref_pitching_advanced <- bbref_processed %>%
    dplyr::filter(!is.na(bbref_id)) %>%
    dplyr::left_join(
      player_master_ids %>%
        dplyr::select(bbref_id, mlbam_id) %>%
        dplyr::filter(!is.na(bbref_id), !is.na(mlbam_id)) %>%
        dplyr::distinct(bbref_id, .keep_all = TRUE),
      by = "bbref_id"
    ) %>%
    dplyr::filter(!is.na(mlbam_id)) %>%
    dplyr::mutate(mlbam_id = as.integer(mlbam_id)) %>%
    dplyr::select(
      mlbam_id, season,
      dplyr::any_of(c(
        "bbref_ERA_plus", "bbref_FIP",
        "bbref_H9", "bbref_HR9", "bbref_BB9", "bbref_SO9", "bbref_SO_W",
        "bbref_WAR"
      ))
    ) %>%
    dplyr::distinct(mlbam_id, season, .keep_all = TRUE)

  n_era_plus <- sum(!is.na(player_season_bbref_pitching_advanced$bbref_ERA_plus))
  n_war      <- sum(!is.na(player_season_bbref_pitching_advanced$bbref_WAR))

  message("07_bbref_pitching_advanced complete: ",
          nrow(player_season_bbref_pitching_advanced), " pitchers | season ",
          season_to_pull, " | ERA+: ", n_era_plus, " | bWAR: ", n_war)
}
