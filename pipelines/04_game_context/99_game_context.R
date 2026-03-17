# ============================================================
# mlb_scouting_report
# PIPELINE PHASE — 04_game_context
# SCRIPT: 99_game_context_commit.R
# ============================================================
# PURPOSE:
#   Combine all game context components into a single table.
#
# OUTPUT:
#   - game_context
#
# DESIGN:
#   - One row per game_pk
#   - All joins left_join
#   - Does not error if sub-table empty
# ============================================================

library(dplyr)

message("Running 99_game_context_commit.R")

required_tables <- c(
  "schedule_context",
  "game_meta",
  "game_probables",
  "game_umpires",
  "weather_context",
  "series_context"
)

missing_tables <- required_tables[!required_tables %in% ls()]

if (length(missing_tables) > 0) {
  stop("Missing required tables: ", paste(missing_tables, collapse = ", "))
}

game_context <- schedule_context %>%
  left_join(game_meta, by = "game_pk") %>%
  left_join(game_probables, by = "game_pk") %>%
  left_join(game_umpires, by = "game_pk") %>%
  left_join(weather_context, by = "game_pk") %>%
  left_join(series_context, by = "game_pk")

# ------------------------------------------------------------
# Resolve column conflicts from multiple joins bringing in the
# same columns (game_date, venue_name, team IDs, etc.)
# schedule_context is the spine — .x versions take precedence.
# ------------------------------------------------------------

# Step 1: find .x columns and their bare-name equivalents
x_cols     <- names(game_context)[endsWith(names(game_context), ".x")]
base_names <- sub("\\.x$", "", x_cols)

# Step 2: drop bare-name duplicates introduced by later joins
# (they have no suffix because the .x/.y conflict already consumed the name)
game_context <- game_context %>%
  dplyr::select(-dplyr::any_of(base_names)) %>%
  dplyr::select(-dplyr::ends_with(".y")) %>%
  dplyr::rename_with(~ sub("\\.x$", "", .x), dplyr::ends_with(".x"))

# Step 3: drop any remaining duplicates (keep first = spine column)
game_context <- game_context[, !duplicated(names(game_context))]

# ------------------------------------------------------------
# Integrity Check
# ------------------------------------------------------------

stopifnot(
  nrow(game_context) ==
    n_distinct(game_context$game_pk)
)

message("game_context built: ", nrow(game_context), " rows")
message("04_game_context complete")
