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

  # ── Extract by data-stat attributes ───────────────────────────────────────
  # Using data-stat avoids all html_table() column-name issues (ERA+ header
  # parsing, duplicate column names, encoding variants).
  # Player rows are <tr> elements that contain a td[data-stat='player'] with
  # a link; sub-header/separator rows lack that link and are skipped.

  all_rows <- tbl_node %>% rvest::html_elements("tr")
  if (length(all_rows) == 0) {
    message("No rows found in table for ", season_val)
    return(NULL)
  }

  # Identify player rows (have a player link)
  player_rows <- Filter(function(row) {
    link <- rvest::html_element(row, "td[data-stat='player'] a")
    !is.na(xml2::xml_attr(link, "href"))
  }, all_rows)

  if (length(player_rows) == 0) {
    message("No player rows found in BBRef table for ", season_val)
    return(NULL)
  }

  message("BBRef player rows found: ", length(player_rows))

  # Helper: extract text of a single data-stat cell from a row
  cell_text <- function(row, stat) {
    v <- rvest::html_element(row, paste0("td[data-stat='", stat, "']")) %>%
      rvest::html_text(trim = TRUE)
    if (length(v) == 0 || is.na(v) || v == "") NA_character_ else v
  }

  # Helper: safe numeric
  safe_num <- function(x) suppressWarnings(as.numeric(x))

  # WAR data-stat name varies across BBRef page versions — try all known names
  war_cell <- function(row) {
    for (s in c("WAR_total_pitch", "war_total_pitch", "WAR", "war")) {
      v <- cell_text(row, s)
      if (!is.na(v)) return(v)
    }
    NA_character_
  }

  # Build result tibble — one row per player row
  tbl_clean <- dplyr::tibble(
    Name           = purrr::map_chr(player_rows,
                       ~ rvest::html_text(
                           rvest::html_element(.x, "td[data-stat='player']"),
                           trim = TRUE)),
    bbref_id       = purrr::map_chr(player_rows, function(row) {
                       href <- rvest::html_element(row, "td[data-stat='player'] a") %>%
                         xml2::xml_attr("href")
                       if (is.na(href)) NA_character_ else
                         stringr::str_extract(href, "[^/]+(?=\\.shtml$)")
                     }),
    bbref_ERA_plus = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "earned_run_avg_plus"))),
    bbref_FIP      = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "fip"))),
    bbref_WAR      = safe_num(purrr::map_chr(player_rows, war_cell)),
    bbref_H9       = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "hits_per_nine"))),
    bbref_HR9      = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "home_runs_per_nine"))),
    bbref_BB9      = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "bases_on_balls_per_nine"))),
    bbref_SO9      = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "strikeouts_per_nine"))),
    bbref_SO_W     = safe_num(purrr::map_chr(player_rows,
                       ~ cell_text(.x, "strikeouts_per_base_on_balls"))),
    scraped_season = as.integer(season_val)
  ) %>%
    dplyr::filter(!is.na(Name), Name != "", !is.na(bbref_id))

  n_era  <- sum(!is.na(tbl_clean$bbref_ERA_plus))
  n_war  <- sum(!is.na(tbl_clean$bbref_WAR))
  message("BBRef parse complete: ", nrow(tbl_clean), " players | ",
          "ERA+: ", n_era, " | WAR: ", n_war)

  tbl_clean
}

# ------------------------------------------------------------
# Pull — try current season, fall back one year
# ------------------------------------------------------------

bbref_raw <- scrape_bbref_pitching_standard(season_to_pull)

if (is.null(bbref_raw) || nrow(bbref_raw) < 50) {
  message("BBRef scrape insufficient for ", season_to_pull,
          " (", if (is.null(bbref_raw)) "NULL" else nrow(bbref_raw), " rows). ",
          "Trying ", season_to_pull - 1, "...")
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

  # scrape_bbref_pitching_standard() now returns a clean tibble with
  # pre-named numeric columns (bbref_ERA_plus, bbref_WAR, etc.) extracted
  # directly by data-stat attribute — no column renaming needed here.

  player_season_bbref_pitching_advanced <- bbref_raw %>%
    dplyr::mutate(season = as.integer(scraped_season)) %>%
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
