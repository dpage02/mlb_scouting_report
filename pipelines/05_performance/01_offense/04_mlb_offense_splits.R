# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 05_performance
# SCRIPT: 04_mlb_offense_splits.R
# ============================================================
# PURPOSE:
#   Pull season-level batter splits vs RHP (vr) and vs LHP (vl)
#   from the MLB Stats API statSplits endpoint.
#
# OUTPUT:
#   player_season_mlb_offense_splits
#
# GRAIN:
#   One row per mlbam_id per season per split_code
#   split_code: "vr" = vs RHP, "vl" = vs LHP
# ============================================================

source("pipelines/05_performance/00_schema/00_grain_definition.R")

season_to_pull <- unique(player_season_mlb_offense$season)[1]

# ------------------------------------------------------------
# Function: Pull one split via MLB Stats API statSplits
# ------------------------------------------------------------

pull_batter_split <- function(sit_code, season_val) {

  url <- paste0(
    "https://statsapi.mlb.com/api/v1/stats",
    "?stats=statSplits",
    "&group=hitting",
    "&gameType=R",
    "&season=", season_val,
    "&sitCodes=", sit_code,
    "&playerPool=all",
    "&limit=5000"
  )

  resp <- tryCatch(
    httr::GET(url, httr::timeout(60)),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("Failed to pull split: ", sit_code)
    return(NULL)
  }

  data <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    flatten          = TRUE,
    simplifyDataFrame = TRUE
  )

  if (length(data$stats) == 0 || is.null(data$stats$splits)) return(NULL)

  splits_raw <- data$stats$splits[[1]]

  if (is.null(splits_raw) || nrow(splits_raw) == 0) return(NULL)

  # Map available stat columns defensively
  # statSplits doesn't always return all fields — use what's there
  col_map <- list(
    mlbam_id   = "player.id",
    team_id    = "team.id",
    split_desc = "split.description",
    mlb_pa     = "stat.plateAppearances",
    mlb_ab     = "stat.atBats",
    mlb_h      = "stat.hits",
    mlb_2b     = "stat.doubles",
    mlb_3b     = "stat.triples",
    mlb_hr     = "stat.homeRuns",
    mlb_bb     = "stat.baseOnBalls",
    mlb_so     = "stat.strikeOuts",
    mlb_hbp    = "stat.hitByPitch",
    mlb_sb     = "stat.stolenBases",
    mlb_rbi    = "stat.rbi",
    mlb_avg    = "stat.avg",
    mlb_obp    = "stat.obp",
    mlb_slg    = "stat.slg",
    mlb_ops    = "stat.ops",
    mlb_babip  = "stat.babip"
  )

  available <- names(splits_raw)
  result    <- splits_raw

  for (new_col in names(col_map)) {
    src_col <- col_map[[new_col]]
    if (src_col %in% available) {
      result[[new_col]] <- splits_raw[[src_col]]
    } else {
      result[[new_col]] <- NA
    }
  }

  result %>%
    dplyr::mutate(
      mlbam_id   = as.integer(mlbam_id),
      season     = as.integer(season_val),
      split_code = sit_code,
      split_desc = dplyr::coalesce(split_desc, sit_code),
      team_id    = as.integer(team_id),
      mlb_pa     = as.integer(mlb_pa),
      mlb_ab     = as.integer(mlb_ab),
      mlb_h      = as.integer(mlb_h),
      mlb_2b     = as.integer(mlb_2b),
      mlb_3b     = as.integer(mlb_3b),
      mlb_hr     = as.integer(mlb_hr),
      mlb_bb     = as.integer(mlb_bb),
      mlb_so     = as.integer(mlb_so),
      mlb_hbp    = as.integer(mlb_hbp),
      mlb_sb     = as.integer(mlb_sb),
      mlb_rbi    = as.integer(mlb_rbi),
      mlb_avg    = as.numeric(mlb_avg),
      mlb_obp    = as.numeric(mlb_obp),
      mlb_slg    = as.numeric(mlb_slg),
      mlb_ops    = as.numeric(mlb_ops),
      mlb_babip  = as.numeric(mlb_babip)
    ) %>%
    dplyr::select(
      mlbam_id, season, split_code, split_desc, team_id,
      dplyr::any_of(c(
        "mlb_pa", "mlb_ab", "mlb_h", "mlb_2b", "mlb_3b",
        "mlb_hr", "mlb_bb", "mlb_so", "mlb_hbp", "mlb_sb",
        "mlb_rbi", "mlb_avg", "mlb_obp", "mlb_slg",
        "mlb_ops", "mlb_babip"
      ))
    ) %>%
    dplyr::left_join(
      team_ids %>% dplyr::select(mlbam_team_id, team_abbr),
      by = c("team_id" = "mlbam_team_id")
    ) %>%
    dplyr::mutate(
      team_abbr = dplyr::coalesce(team_abbr, as.character(team_id))
    ) %>%
    dplyr::select(-team_id) %>%
    dplyr::filter(!is.na(mlbam_id))
}

# ------------------------------------------------------------
# Pull both splits
# ------------------------------------------------------------

message("Pulling batter splits vs RHP (vr) for ", season_to_pull, "...")
splits_vr <- pull_batter_split("vr", season_to_pull)

message("Pulling batter splits vs LHP (vl) for ", season_to_pull, "...")
splits_vl <- pull_batter_split("vl", season_to_pull)

# Fallback: if either split returned nothing, try the prior season
if ((is.null(splits_vr) || nrow(splits_vr) == 0) &&
    (is.null(splits_vl) || nrow(splits_vl) == 0)) {
  fallback_season <- season_to_pull - 1L
  message("No split data for ", season_to_pull,
          ". Falling back to ", fallback_season)
  splits_vr <- pull_batter_split("vr", fallback_season)
  splits_vl <- pull_batter_split("vl", fallback_season)
}

player_season_mlb_offense_splits <- dplyr::bind_rows(splits_vr, splits_vl) %>%
  dplyr::filter(!is.na(mlbam_id)) %>%
  dplyr::arrange(mlbam_id, split_code)

message("04_mlb_offense_splits complete: ",
        nrow(player_season_mlb_offense_splits), " rows | ",
        dplyr::n_distinct(player_season_mlb_offense_splits$mlbam_id),
        " players | season ", season_to_pull,
        " | vr: ", sum(player_season_mlb_offense_splits$split_code == "vr"),
        " | vl: ", sum(player_season_mlb_offense_splits$split_code == "vl"))
