# ============================================================
# FANTASY BASEBALL — ADP & Consensus Rankings
# ============================================================
# Pulls average draft position from:
#   1. FantasyPros MLB consensus (primary)
#   2. Yahoo Baseball ADP (secondary)
#
# OUTPUT:
#   adp_master  — player name, fg_id, adp (avg across sources),
#                 adp_fantasypros, adp_yahoo
# ============================================================

source("fantasy/00_fantasy_config.R")

# ------------------------------------------------------------
# Source 1: FantasyPros Consensus Rankings
# ------------------------------------------------------------

fetch_fantasypros_adp <- function() {
  # FantasyPros overall MLB consensus rankings
  url <- "https://www.fantasypros.com/mlb/adp/overall.php"

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers(`User-Agent` = "Mozilla/5.0"),
              httr::timeout(20)),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  FantasyPros ADP: failed (", httr::status_code(resp), ")")
    return(NULL)
  }

  page <- rvest::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

  tbl <- tryCatch(
    page %>%
      rvest::html_element("table#data") %>%
      rvest::html_table(fill = TRUE),
    error = function(e) NULL
  )

  if (is.null(tbl) || nrow(tbl) == 0) {
    # Try any table
    tbl <- tryCatch(
      page %>% rvest::html_elements("table") %>%
        purrr::map(rvest::html_table) %>%
        purrr::keep(~ nrow(.x) > 20) %>%
        purrr::pluck(1),
      error = function(e) NULL
    )
  }

  if (is.null(tbl) || nrow(tbl) == 0) {
    message("  FantasyPros ADP: could not parse table")
    return(NULL)
  }

  # Find rank and name columns
  rank_col <- intersect(c("Rank", "RK", "#"), names(tbl))[1]
  name_col <- intersect(c("Player", "Name", "PLAYER"), names(tbl))[1]
  pos_col  <- intersect(c("Position", "POS", "Pos"), names(tbl))[1]
  adp_col  <- intersect(c("AVG", "ADP", "Avg"), names(tbl))[1]

  if (is.na(name_col)) {
    message("  FantasyPros ADP: could not identify name column")
    return(NULL)
  }

  result <- tbl %>%
    dplyr::select(
      player_name_fp = dplyr::all_of(name_col),
      dplyr::any_of(setNames(c(rank_col, pos_col, adp_col),
                             c("fp_rank", "fp_pos", "adp_fantasypros")))
    ) %>%
    dplyr::mutate(
      # Strip team from name (FP often has "Name (TEAM)")
      player_name_fp = stringr::str_trim(
        stringr::str_remove(player_name_fp, "\\s*\\([A-Z]{2,3}\\)\\s*$")
      ),
      adp_fantasypros = suppressWarnings(as.numeric(
        dplyr::coalesce(as.character(adp_fantasypros), as.character(fp_rank))
      )),
      fp_rank = suppressWarnings(as.integer(fp_rank))
    ) %>%
    dplyr::filter(!is.na(player_name_fp), nchar(player_name_fp) > 2) %>%
    dplyr::distinct(player_name_fp, .keep_all = TRUE)

  message("  FantasyPros ADP: ", nrow(result), " players")
  result
}

# ------------------------------------------------------------
# Source 2: Yahoo Baseball ADP
# ------------------------------------------------------------

fetch_yahoo_adp <- function() {
  url <- "https://baseball.fantasysports.yahoo.com/b1/adp"

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers(`User-Agent` = "Mozilla/5.0"),
              httr::timeout(20)),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  Yahoo ADP: failed")
    return(NULL)
  }

  page <- rvest::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

  tbl <- tryCatch(
    page %>%
      rvest::html_elements("table") %>%
      purrr::map(rvest::html_table) %>%
      purrr::keep(~ nrow(.x) > 20) %>%
      purrr::pluck(1),
    error = function(e) NULL
  )

  if (is.null(tbl) || nrow(tbl) == 0) {
    message("  Yahoo ADP: could not parse table")
    return(NULL)
  }

  name_col <- intersect(c("Name", "Player", "PLAYER"), names(tbl))[1]
  adp_col  <- intersect(c("ADP", "Avg Pick", "AVG PICK"), names(tbl))[1]

  if (is.na(name_col)) {
    message("  Yahoo ADP: could not identify name column")
    return(NULL)
  }

  result <- tbl %>%
    dplyr::select(
      player_name_yh = dplyr::all_of(name_col),
      dplyr::any_of(setNames(adp_col, "adp_yahoo"))
    ) %>%
    dplyr::mutate(
      player_name_yh = stringr::str_trim(player_name_yh),
      adp_yahoo = suppressWarnings(as.numeric(adp_yahoo))
    ) %>%
    dplyr::filter(!is.na(player_name_yh), nchar(player_name_yh) > 2) %>%
    dplyr::distinct(player_name_yh, .keep_all = TRUE)

  message("  Yahoo ADP: ", nrow(result), " players")
  result
}

# ------------------------------------------------------------
# Fuzzy name match helper
# ------------------------------------------------------------

normalize_name <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[^a-z ]") %>%
    stringr::str_squish()
}

# ------------------------------------------------------------
# Pull both sources
# ------------------------------------------------------------

message("Fetching ADP rankings...")
fp_adp  <- fetch_fantasypros_adp()
yh_adp  <- fetch_yahoo_adp()

# ------------------------------------------------------------
# Build adp_master joined to big_board player names
# ------------------------------------------------------------

# Use big_board player names as the canonical reference
if (!exists("big_board")) {
  message("big_board not found — run 03_big_board.R first. Skipping ADP join.")
  adp_master <- dplyr::tibble(
    player_name = character(), adp_fantasypros = numeric(),
    adp_yahoo = numeric(), adp = numeric()
  )
} else {

  adp_master <- big_board %>%
    dplyr::select(fg_id, mlbam_id, player_name, primary_pos) %>%
    dplyr::mutate(name_key = normalize_name(player_name))

  # Join FantasyPros
  if (!is.null(fp_adp)) {
    fp_adp <- fp_adp %>%
      dplyr::mutate(name_key = normalize_name(player_name_fp))
    adp_master <- adp_master %>%
      dplyr::left_join(fp_adp %>% dplyr::select(name_key, adp_fantasypros, fp_rank),
                       by = "name_key")
  } else {
    adp_master$adp_fantasypros <- NA_real_
    adp_master$fp_rank         <- NA_integer_
  }

  # Join Yahoo
  if (!is.null(yh_adp)) {
    yh_adp <- yh_adp %>%
      dplyr::mutate(name_key = normalize_name(player_name_yh))
    adp_master <- adp_master %>%
      dplyr::left_join(yh_adp %>% dplyr::select(name_key, adp_yahoo),
                       by = "name_key")
  } else {
    adp_master$adp_yahoo <- NA_real_
  }

  # Consensus ADP: average of available sources
  adp_master <- adp_master %>%
    dplyr::mutate(
      adp = dplyr::case_when(
        !is.na(adp_fantasypros) & !is.na(adp_yahoo) ~
          round((adp_fantasypros + adp_yahoo) / 2, 1),
        !is.na(adp_fantasypros) ~ adp_fantasypros,
        !is.na(adp_yahoo)       ~ adp_yahoo,
        TRUE                     ~ NA_real_
      )
    ) %>%
    dplyr::select(fg_id, mlbam_id, player_name, primary_pos,
                  adp, adp_fantasypros, adp_yahoo, fp_rank)

  adp_matched <- sum(!is.na(adp_master$adp))
  message("ADP matched: ", adp_matched, " / ", nrow(adp_master), " players")
}

message("01b_adp_rankings complete.")
