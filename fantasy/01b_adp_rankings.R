# ============================================================
# FANTASY BASEBALL — ADP & Consensus Rankings
# ============================================================
# Pulls average draft position from:
#   1. FantasyPros JSON API (primary — more reliable than HTML scrape)
#   2. FantasyPros HTML table fallback
#   3. Yahoo Baseball ADP (secondary)
#
# OUTPUT:
#   adp_master  — player name, fg_id, adp (avg across sources),
#                 adp_fantasypros, adp_yahoo, fp_rank
# ============================================================

source("fantasy/00_fantasy_config.R")

# ------------------------------------------------------------
# Source 1: FantasyPros — try JSON API first, then HTML
# ------------------------------------------------------------

fetch_fantasypros_adp <- function() {

  # ── Attempt 1: JSON endpoint ──────────────────────────────
  json_url <- paste0(
    "https://www.fantasypros.com/api/v2/json/mlb/",
    format(Sys.Date(), "%Y"),
    "/consensus-rankings?type=overall&scoring=H2H&position=ALL"
  )
  resp_json <- tryCatch(
    httr::GET(json_url,
              httr::add_headers(`User-Agent` = "Mozilla/5.0",
                                `Accept` = "application/json"),
              httr::timeout(15)),
    error = function(e) NULL
  )

  if (!is.null(resp_json) && httr::status_code(resp_json) == 200) {
    raw <- tryCatch(
      jsonlite::fromJSON(httr::content(resp_json, as = "text", encoding = "UTF-8"),
                         flatten = TRUE),
      error = function(e) NULL
    )
    if (!is.null(raw)) {
      players <- if (is.data.frame(raw)) raw else raw$players
      if (!is.null(players) && nrow(players) > 10) {
        name_col <- intersect(c("player_name", "name", "PlayerName"), names(players))[1]
        rank_col <- intersect(c("rank", "overall_rank", "rk"),         names(players))[1]
        adp_col  <- intersect(c("avg", "adp", "average_pick"),         names(players))[1]
        if (!is.na(name_col)) {
          result <- dplyr::tibble(
            player_name_fp  = as.character(players[[name_col]]),
            fp_rank         = if (!is.na(rank_col)) suppressWarnings(as.integer(players[[rank_col]])) else seq_len(nrow(players)),
            adp_fantasypros = if (!is.na(adp_col))  suppressWarnings(as.numeric(players[[adp_col]]))  else as.numeric(seq_len(nrow(players)))
          ) %>%
            dplyr::filter(!is.na(player_name_fp), nchar(player_name_fp) > 2) %>%
            dplyr::distinct(player_name_fp, .keep_all = TRUE)
          message("  FantasyPros ADP (JSON): ", nrow(result), " players")
          return(result)
        }
      }
    }
  }

  # ── Attempt 2: HTML page ──────────────────────────────────
  html_url <- "https://www.fantasypros.com/mlb/adp/overall.php"
  resp <- tryCatch(
    httr::GET(html_url,
              httr::add_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"),
              httr::timeout(20)),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  FantasyPros ADP: HTTP failed (status ",
            if (!is.null(resp)) httr::status_code(resp) else "NA", ")")
    return(NULL)
  }

  page <- rvest::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

  # Try specific table ID first, then any large table
  tbl <- tryCatch(
    page %>% rvest::html_element("table#data") %>% rvest::html_table(fill = TRUE),
    error = function(e) NULL
  )
  if (is.null(tbl) || nrow(tbl) == 0) {
    all_tbls <- tryCatch(
      page %>% rvest::html_elements("table") %>% purrr::map(rvest::html_table),
      error = function(e) list()
    )
    tbl <- purrr::keep(all_tbls, ~ nrow(.x) > 20) %>% purrr::pluck(1)
  }

  if (is.null(tbl) || nrow(tbl) == 0) {
    message("  FantasyPros ADP (HTML): could not parse table")
    return(NULL)
  }

  message("  FantasyPros ADP (HTML): table columns = ", paste(names(tbl), collapse=", "))

  rank_col <- intersect(c("Rank", "RK", "#", "Rk", "rank"),                    names(tbl))[1]
  name_col <- intersect(c("Player", "Name", "PLAYER", "player",
                           "Player (Team)", "Player Name", "PLAYER NAME"),      names(tbl))[1]
  adp_col  <- intersect(c("AVG", "ADP", "Avg", "Average"),                     names(tbl))[1]

  if (is.na(name_col)) {
    # Last resort: pick the character column with longest average string length
    chr_cols <- names(tbl)[sapply(tbl, is.character)]
    if (length(chr_cols) > 0) {
      avg_len <- sapply(chr_cols, function(col) mean(nchar(tbl[[col]]), na.rm=TRUE))
      name_col <- chr_cols[which.max(avg_len)]
      message("  FantasyPros ADP: guessing name column = '", name_col, "'")
    } else {
      message("  FantasyPros ADP: could not identify name column")
      return(NULL)
    }
  }

  result <- dplyr::tibble(
    player_name_fp  = as.character(tbl[[name_col]]),
    fp_rank         = if (!is.na(rank_col)) suppressWarnings(as.integer(tbl[[rank_col]])) else seq_len(nrow(tbl)),
    adp_fantasypros = if (!is.na(adp_col))  suppressWarnings(as.numeric(tbl[[adp_col]]))  else as.numeric(seq_len(nrow(tbl)))
  ) %>%
    dplyr::mutate(
      # Strip team/position info — handles multiple formats:
      #   "Aaron Judge (NYY)"          → "Aaron Judge"
      #   "Aaron Judge (NYY, OF)"      → "Aaron Judge"
      #   "Aaron Judge (OF - NYY)"     → "Aaron Judge"
      player_name_fp = stringr::str_trim(
        stringr::str_remove(player_name_fp, "\\s*\\(.*\\)\\s*$")
      )
    ) %>%
    dplyr::filter(!is.na(player_name_fp), nchar(player_name_fp) > 2) %>%
    dplyr::distinct(player_name_fp, .keep_all = TRUE)

  # Show a sample so we can verify names look right
  message("  Sample names: ", paste(head(result$player_name_fp, 5), collapse=" | "))

  message("  FantasyPros ADP (HTML): ", nrow(result), " players")
  result
}

# ------------------------------------------------------------
# Source 2: Yahoo Baseball ADP
# ------------------------------------------------------------

fetch_yahoo_adp <- function() {
  url <- "https://baseball.fantasysports.yahoo.com/b1/adp"

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers(
                `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
                `Accept` = "text/html,application/xhtml+xml"
              ),
              httr::timeout(20)),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  Yahoo ADP: HTTP failed")
    return(NULL)
  }

  page <- rvest::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

  all_tbls <- tryCatch(
    page %>% rvest::html_elements("table") %>% purrr::map(rvest::html_table),
    error = function(e) list()
  )
  tbl <- purrr::keep(all_tbls, ~ nrow(.x) > 20) %>% purrr::pluck(1)

  if (is.null(tbl) || nrow(tbl) == 0) {
    message("  Yahoo ADP: could not parse table (page may be JS-rendered)")
    return(NULL)
  }

  message("  Yahoo ADP: table columns = ", paste(names(tbl), collapse=", "))

  name_col <- intersect(c("Name", "Player", "PLAYER"), names(tbl))[1]
  adp_col  <- intersect(c("ADP", "Avg Pick", "AVG PICK", "Average"), names(tbl))[1]
  # Yahoo sometimes has a dedicated position column
  pos_col  <- intersect(c("Position", "Pos", "POS", "Positions", "Eligibility"), names(tbl))[1]

  if (is.na(name_col)) {
    message("  Yahoo ADP: could not identify name column")
    return(NULL)
  }

  result <- dplyr::tibble(
    player_name_raw = as.character(tbl[[name_col]]),
    adp_yahoo = if (!is.na(adp_col)) suppressWarnings(as.numeric(tbl[[adp_col]])) else as.numeric(seq_len(nrow(tbl))),
    yahoo_pos_col   = if (!is.na(pos_col)) as.character(tbl[[pos_col]]) else NA_character_
  ) %>%
    dplyr::mutate(
      # Yahoo player cells often look like "Aaron Judge\nOF - NYY" or "Ben Rice\nC,1B - NYY"
      # Extract position from: anything between newline (or start) and " - TEAM" pattern
      yahoo_pos_extracted = stringr::str_extract(
        player_name_raw,
        "(?:\n|^)([A-Z]{1,2}(?:[,/][A-Z]{1,2})*)(?:\\s*[-–]\\s*[A-Z]{2,3})"
      ) %>%
        stringr::str_extract("[A-Z]{1,2}(?:[,/][A-Z]{1,2})*"),

      # Prefer dedicated column, fall back to extraction from name string
      yahoo_pos = dplyr::coalesce(yahoo_pos_col, yahoo_pos_extracted) %>%
        stringr::str_replace_all(",", "/") %>%   # normalise C,1B → C/1B
        stringr::str_trim(),

      # Clean player name: remove everything from first newline or "(" onwards
      player_name_yh = stringr::str_trim(
        stringr::str_remove(player_name_raw, "(?:\n|\\s*\\().*$")
      )
    ) %>%
    dplyr::select(player_name_yh, adp_yahoo, yahoo_pos) %>%
    dplyr::filter(!is.na(player_name_yh), nchar(player_name_yh) > 2) %>%
    dplyr::distinct(player_name_yh, .keep_all = TRUE)

  pos_found <- sum(!is.na(result$yahoo_pos) & nchar(result$yahoo_pos) > 0, na.rm = TRUE)
  message("  Yahoo ADP: ", nrow(result), " players | position data: ", pos_found, " players")
  message("  Sample (name | pos): ",
    paste(head(paste(result$player_name_yh, result$yahoo_pos, sep=" | "), 4), collapse=" // "))
  result
}

# ------------------------------------------------------------
# Pull both sources
# ------------------------------------------------------------

message("Fetching ADP rankings...")
fp_adp <- fetch_fantasypros_adp()
yh_adp <- fetch_yahoo_adp()

# ------------------------------------------------------------
# Build adp_master joined to big_board player names
# ------------------------------------------------------------

if (!exists("big_board")) {
  message("big_board not found — run 03_big_board.R first. Skipping ADP join.")
  adp_master <- dplyr::tibble(
    fg_id = character(), player_name = character(),
    adp = numeric(), adp_fantasypros = numeric(),
    adp_yahoo = numeric(), fp_rank = integer()
  )
} else {

  adp_master <- big_board %>%
    dplyr::select(fg_id, mlbam_id, player_name, primary_pos) %>%
    dplyr::mutate(name_key = normalize_name(player_name))

  # Join FantasyPros
  if (!is.null(fp_adp) && nrow(fp_adp) > 0) {
    fp_adp <- fp_adp %>%
      dplyr::mutate(name_key = normalize_name(player_name_fp))
    adp_master <- adp_master %>%
      dplyr::left_join(
        fp_adp %>% dplyr::select(name_key, adp_fantasypros, fp_rank),
        by = "name_key"
      )
  } else {
    adp_master$adp_fantasypros <- NA_real_
    adp_master$fp_rank         <- NA_integer_
  }

  # Join Yahoo
  if (!is.null(yh_adp) && nrow(yh_adp) > 0) {
    yh_adp <- yh_adp %>%
      dplyr::mutate(name_key = normalize_name(player_name_yh))
    adp_master <- adp_master %>%
      dplyr::left_join(
        yh_adp %>% dplyr::select(name_key, adp_yahoo),
        by = "name_key"
      )
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

  if (adp_matched == 0) {
    message("  NOTE: ADP scraping returned 0 matches.")
    message("  VOR rankings will still be used for draft guidance.")
    message("  If you need ADP, manually download from fantasypros.com/mlb/adp")
  }
}

# ------------------------------------------------------------
# FantasyPros position eligibility
# Scrapes each position-specific ranking page to build a
# complete player → eligible positions map using Yahoo rules.
# Ben Rice appears on both /c.php and /1b.php → "C/1B"
# ------------------------------------------------------------

fetch_fantasypros_positions <- function() {
  pos_pages <- c(
    C  = "https://www.fantasypros.com/mlb/rankings/c.php",
    `1B` = "https://www.fantasypros.com/mlb/rankings/1b.php",
    `2B` = "https://www.fantasypros.com/mlb/rankings/2b.php",
    `3B` = "https://www.fantasypros.com/mlb/rankings/3b.php",
    SS = "https://www.fantasypros.com/mlb/rankings/ss.php",
    OF = "https://www.fantasypros.com/mlb/rankings/of.php",
    SP = "https://www.fantasypros.com/mlb/rankings/sp.php",
    RP = "https://www.fantasypros.com/mlb/rankings/rp.php"
  )

  all_rows <- list()

  for (pos in names(pos_pages)) {
    url  <- pos_pages[[pos]]
    resp <- tryCatch(
      httr::GET(url,
                httr::add_headers(`User-Agent` = "Mozilla/5.0"),
                httr::timeout(20)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::status_code(resp) != 200) {
      message("  FP positions [", pos, "]: HTTP failed")
      next
    }

    page <- rvest::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))

    # Try multiple CSS selectors — FP has changed their markup over the years
    names_raw <- character(0)

    selectors <- c(
      "table#ranking-table td.player-label a.fp-player-name",
      "table.ecrRankings td.player-label a",
      "table td.player-label a",
      "table a.player-name",
      "table a[href*='/mlb/players/']"
    )
    for (sel in selectors) {
      names_raw <- tryCatch(
        page %>% rvest::html_elements(sel) %>% rvest::html_text(trim = TRUE),
        error = function(e) character(0)
      )
      if (length(names_raw) > 5) break
    }

    # Final fallback: parse any large table by column name
    if (length(names_raw) == 0) {
      tbls <- tryCatch(
        page %>% rvest::html_elements("table") %>% purrr::map(rvest::html_table),
        error = function(e) list()
      )
      tbl <- purrr::keep(tbls, ~ nrow(.x) > 10) %>% purrr::pluck(1)
      if (!is.null(tbl)) {
        name_col <- intersect(c("Player","Name","PLAYER"), names(tbl))[1]
        if (!is.na(name_col)) {
          names_raw <- as.character(tbl[[name_col]]) %>%
            stringr::str_trim() %>%
            stringr::str_remove("\\s*\\(.*\\)\\s*$") %>%
            stringr::str_remove("\\s+[A-Z]{2,3}\\s*$")   # strip trailing team abbr
          names_raw <- names_raw[nchar(names_raw) > 2]
        }
      }
    }

    if (length(names_raw) == 0) {
      message("  FP positions [", pos, "]: no players found")
      next
    }

    message("  FP positions [", pos, "]: ", length(names_raw), " players")
    all_rows[[pos]] <- dplyr::tibble(
      player_name_fp = names_raw,
      fp_pos         = pos
    )
    Sys.sleep(0.4)  # polite delay between page requests
  }

  if (length(all_rows) == 0) {
    message("  FP positions: all pages failed")
    return(NULL)
  }

  # Combine: one row per player per position, then collapse to "C/1B" style
  combined <- dplyr::bind_rows(all_rows) %>%
    dplyr::mutate(name_key = normalize_name(player_name_fp)) %>%
    dplyr::filter(!is.na(name_key), nchar(name_key) > 2) %>%
    dplyr::group_by(name_key) %>%
    dplyr::summarise(
      player_name_fp = dplyr::first(player_name_fp),
      fp_eligible    = paste(unique(fp_pos), collapse = "/"),
      .groups        = "drop"
    )

  message("FP position map built: ", nrow(combined), " players with eligibility data")
  message("  Sample: ",
    paste(head(paste(combined$player_name_fp, combined$fp_eligible, sep="="), 5), collapse=" | "))
  combined
}

# ------------------------------------------------------------
# MLB Position Eligibility (Yahoo threshold = 20+ games at position)
# ------------------------------------------------------------
# Yahoo grants eligibility at any position where a player appeared
# in 20+ games the prior season. This mirrors Yahoo's actual rules
# and is more accurate than scraping FP ranking pages.
# One row per mlbam_id with slash-separated eligible positions:
#   Ben Rice → "C/1B"   |   Bobby Witt Jr. → "SS/3B"
# ------------------------------------------------------------

fetch_mlb_position_eligibility <- function(min_games = 20) {
  season_val <- as.integer(format(Sys.Date(), "%Y")) - 1

  raw <- tryCatch(
    baseballr::mlb_stats(
      stat_type   = "season",
      stat_group  = "fielding",
      player_pool = "all",
      season      = season_val,
      sport_id    = 1,
      limit       = 5000
    ),
    error = function(e) { message("  MLB fielding API failed: ", e$message); NULL }
  )

  if (is.null(raw) || nrow(raw) == 0) {
    message("  No MLB fielding data returned")
    return(NULL)
  }

  raw %>%
    dplyr::transmute(
      mlbam_id = as.integer(player_id),
      position = position_abbreviation,
      games    = suppressWarnings(as.integer(games))
    ) %>%
    dplyr::filter(!is.na(mlbam_id), !is.na(position), !is.na(games),
                  games >= min_games) %>%
    dplyr::mutate(
      fantasy_pos = dplyr::case_when(
        position == "C"                         ~ "C",
        position == "1B"                        ~ "1B",
        position == "2B"                        ~ "2B",
        position == "3B"                        ~ "3B",
        position == "SS"                        ~ "SS",
        position %in% c("LF","CF","RF","OF")   ~ "OF",
        position == "DH"                        ~ "DH",
        position %in% c("SP","RP","P","1","0")  ~ NA_character_,
        TRUE                                    ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(fantasy_pos)) %>%
    dplyr::group_by(mlbam_id) %>%
    dplyr::summarise(
      mlb_eligible = paste(unique(fantasy_pos), collapse = "/"),
      n_positions  = dplyr::n_distinct(fantasy_pos),
      .groups      = "drop"
    )
}

message("Fetching MLB position eligibility (", 20, "+ games threshold)...")
mlb_positions <- tryCatch(
  fetch_mlb_position_eligibility(min_games = 20),
  error = function(e) { message("  MLB positions failed: ", e$message); NULL }
)

if (!is.null(mlb_positions) && nrow(mlb_positions) > 0) {
  multi <- sum(mlb_positions$n_positions > 1)
  message("  MLB positions: ", nrow(mlb_positions), " players | ",
          multi, " multi-position")
  message("  Sample multi-pos: ",
    paste(
      head(
        mlb_positions %>%
          dplyr::filter(n_positions > 1) %>%
          dplyr::mutate(lbl = mlb_eligible) %>%
          dplyr::pull(lbl),
        8
      ),
      collapse = " | "
    )
  )
} else {
  message("  MLB position eligibility unavailable")
}

message("Fetching FantasyPros position eligibility...")
fp_positions <- tryCatch(
  fetch_fantasypros_positions(),
  error = function(e) { message("  FP positions failed: ", e$message); NULL }
)

message("01b_adp_rankings complete.")
