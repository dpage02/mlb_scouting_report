# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 08_steamer_projections.R
# ============================================================
# PURPOSE:
#   Pull Steamer projected wRC+ and PA from FanGraphs API.
#   Used as the primary true-talent offensive estimate in the
#   prediction model — superior to Marcel because it uses
#   age curves, proper regression coefficients, and position
#   adjustments rather than simple PA-weighted averages.
#
# OUTPUT:
#   steamer_projections
#
# GRAIN:
#   One row per mlbam_id (current-season projection)
#
# FALLBACK:
#   If FanGraphs blocks (403) or returns insufficient data,
#   steamer_projections is set to an empty tibble.
#   The prediction model then falls back to Marcel blend.
# ============================================================

.pull_steamer_batting <- function() {
  resp <- tryCatch(
    httr::GET(
      "https://www.fangraphs.com/api/projections",
      query = list(
        type    = "steamer",
        stats   = "bat",
        pos     = "all",
        team    = "0",
        players = "0",
        lg      = "all"
      ),
      httr::add_headers(
        `User-Agent` = "Mozilla/5.0",
        `Accept`     = "application/json"
      ),
      httr::timeout(60)
    ),
    error = function(e) {
      message("Steamer projections fetch failed: ", e$message)
      NULL
    }
  )

  if (is.null(resp)) return(NULL)

  if (httr::http_error(resp)) {
    message("Steamer projections HTTP error: ", httr::status_code(resp))
    return(NULL)
  }

  raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")

  # Guard against Cloudflare HTML block
  if (grepl("<!DOCTYPE", raw_text, fixed = TRUE)) {
    message("Steamer projections: Cloudflare block (HTML response)")
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(raw_text, flatten = TRUE),
    error = function(e) {
      message("Steamer projections JSON parse failed: ", e$message)
      NULL
    }
  )

  if (is.null(parsed)) return(NULL)

  # API returns array directly or wrapped in a list
  df <- if (is.data.frame(parsed)) {
    parsed
  } else if ("data" %in% names(parsed) && is.data.frame(parsed$data)) {
    parsed$data
  } else if (is.list(parsed) && length(parsed) > 0) {
    tryCatch(dplyr::bind_rows(parsed), error = function(e) NULL)
  } else NULL

  if (is.null(df) || nrow(df) == 0) {
    message("Steamer projections: empty response")
    return(NULL)
  }

  df
}

message("Pulling Steamer batting projections from FanGraphs...")

steamer_raw <- .pull_steamer_batting()

if (is.null(steamer_raw) || nrow(steamer_raw) < 100) {
  message("Steamer projections unavailable",
          if (!is.null(steamer_raw)) paste0(" (", nrow(steamer_raw), " rows)") else " (NULL)",
          ". Prediction model will use Marcel blend fallback.")
  steamer_projections <- dplyr::tibble(
    mlbam_id        = integer(),
    steamer_wrc_plus = numeric(),
    steamer_pa       = integer()
  )
} else {
  message("Steamer projections raw: ", nrow(steamer_raw), " rows, ",
          ncol(steamer_raw), " columns")

  # Normalize column names: handle %, +, -, spaces
  names(steamer_raw) <- gsub("\\+", "_plus", gsub("%", "_pct",
    gsub("-", "_", names(steamer_raw))))
  names(steamer_raw) <- gsub("[^A-Za-z0-9_]", "_", names(steamer_raw))
  names(steamer_raw) <- gsub("_+", "_", gsub("_$", "", names(steamer_raw)))

  # Locate mlbam_id column
  mlbam_col <- intersect(c("xMLBAMID", "mlbam_id", "MLBAMID"), names(steamer_raw))[1]

  # Locate wRC+ column (various encodings)
  wrc_col <- intersect(
    c("wRC_plus", "wRC.", "wRC_plus_1", "wRCplus"),
    names(steamer_raw)
  )[1]

  # Locate PA column
  pa_col <- intersect(c("PA", "pa", "PA_1"), names(steamer_raw))[1]

  if (is.na(mlbam_col)) {
    # Try joining via FanGraphs playerid
    fg_id_col <- intersect(c("playerid", "PlayerId"), names(steamer_raw))[1]
    if (!is.na(fg_id_col) && exists("player_master_ids")) {
      message("Steamer: no xMLBAMID column — joining via fg_id")
      steamer_raw <- steamer_raw %>%
        dplyr::mutate(fg_id_join = as.character(.data[[fg_id_col]])) %>%
        dplyr::left_join(
          player_master_ids %>%
            dplyr::filter(!is.na(fg_id), !is.na(mlbam_id)) %>%
            dplyr::distinct(fg_id, .keep_all = TRUE) %>%
            dplyr::select(fg_id, mlbam_id),
          by = c("fg_id_join" = "fg_id")
        )
      mlbam_col <- "mlbam_id"
    }
  }

  if (is.na(mlbam_col) || is.na(wrc_col)) {
    message("Steamer projections: missing required columns",
            " (mlbam_col=", mlbam_col, ", wrc_col=", wrc_col, ")",
            ". Available: ", paste(names(steamer_raw), collapse = ", "))
    steamer_projections <- dplyr::tibble(
      mlbam_id        = integer(),
      steamer_wrc_plus = numeric(),
      steamer_pa       = integer()
    )
  } else {
    steamer_projections <- steamer_raw %>%
      dplyr::mutate(
        mlbam_id        = suppressWarnings(as.integer(.data[[mlbam_col]])),
        steamer_wrc_plus = suppressWarnings(as.numeric(.data[[wrc_col]])),
        steamer_pa       = if (!is.na(pa_col))
          suppressWarnings(as.integer(.data[[pa_col]])) else NA_integer_
      ) %>%
      dplyr::filter(!is.na(mlbam_id), mlbam_id > 0, !is.na(steamer_wrc_plus)) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, steamer_wrc_plus, steamer_pa)

    message("Steamer projections complete: ", nrow(steamer_projections),
            " players with projected wRC+ | ",
            "median wRC+ = ", round(median(steamer_projections$steamer_wrc_plus, na.rm = TRUE)))
  }
}
