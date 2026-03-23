# ============================================================
# FANTASY BASEBALL — Build Big Board
# ============================================================
# PURPOSE:
#   1. Calculate projected fantasy points (full season)
#   2. Assign primary fantasy position eligibility
#   3. Calculate Value Over Replacement (VOR) at each position
#   4. Produce ranked big_board tibble
#
# OUTPUT:
#   big_board  — all draftable players ranked by VOR
# ============================================================

source("fantasy/00_fantasy_config.R")

# ── Scoring weights (from config) ───────────────────────────
W <- BAT_WEIGHTS
P <- PITCH_WEIGHTS

# ============================================================
# PART 1 — BATTER FANTASY POINTS
# ============================================================

bat_pts <- proj_bat %>%
  dplyr::mutate(

    # Singles = H - 2B - 3B - HR
    final_1b = pmax(0L, final_h - final_2b - final_3b - final_hr),

    # Projected fantasy points (full season)
    proj_fpts = (final_1b  * W$single  +
                 final_2b  * W$double  +
                 final_3b  * W$triple  +
                 final_hr  * W$hr      +
                 final_r   * W$r       +
                 final_rbi * W$rbi     +
                 final_sb  * W$sb      +
                 final_cs  * W$cs      +
                 final_bb  * W$bb      +
                 final_hbp * W$hbp),

    # Per-game rate (for comparison across players with different PA)
    proj_fpts_pg = round(proj_fpts / pmax(final_pa / 3.8, 1), 2),  # ~3.8 PA/game

    player_type = "batter"
  )

# ── Position eligibility ─────────────────────────────────────
# Parse FG position string (e.g. "1B/3B", "SS/2B", "C", "OF")
# and assign primary + list of eligible slots

parse_positions <- function(pos_str) {
  if (is.na(pos_str) || pos_str == "") return("Util")
  # FG uses "/", "-", or ", " as separators
  pos_str <- gsub("[-,]", "/", pos_str)
  positions <- trimws(unlist(strsplit(pos_str, "/")))
  # Map to standard slots
  slot_map <- c(
    "C"  = "C",  "1B" = "1B", "2B" = "2B", "3B" = "3B", "SS" = "SS",
    "LF" = "OF", "CF" = "OF", "RF" = "OF", "OF" = "OF",
    "DH" = "Util", "SP" = "SP", "RP" = "RP", "P" = "SP"
  )
  mapped <- unique(slot_map[positions[positions %in% names(slot_map)]])
  if (length(mapped) == 0) return("Util")
  paste(mapped, collapse = "/")
}

bat_pts <- bat_pts %>%
  dplyr::mutate(
    eligible_positions = vapply(proj_pos_raw, parse_positions, character(1)),
    primary_pos = dplyr::case_when(
      grepl("C",  eligible_positions) ~ "C",
      grepl("SS", eligible_positions) ~ "SS",
      grepl("2B", eligible_positions) ~ "2B",
      grepl("3B", eligible_positions) ~ "3B",
      grepl("1B", eligible_positions) ~ "1B",
      grepl("OF", eligible_positions) ~ "OF",
      TRUE                             ~ "Util"
    )
  )

# ============================================================
# PART 2 — PITCHER FANTASY POINTS
# ============================================================

pit_pts <- proj_pit %>%
  dplyr::mutate(
    # IP * 3 outs/IP * 0.33 pts/out = IP * 0.99
    proj_fpts = (final_ip  * 3 * P$out  +
                 final_k   * P$k        +
                 final_w   * P$win      +
                 final_sv  * P$sv       +
                 final_er  * P$er       +
                 final_qs  * P$qs),

    proj_fpts_pg = round(proj_fpts / pmax(final_g, 1), 2),

    player_type      = "pitcher",
    primary_pos      = proj_role,  # SP, RP, or RP_closer
    eligible_positions = proj_role
  )

# ============================================================
# PART 3 — VALUE OVER REPLACEMENT (VOR)
# ============================================================
# Replacement level = projected fpts of the last startable player
# at each position given league depth (10 teams × slots per pos)
# ============================================================

# Effective roster depth per position (add buffer for bench/IL churn)
depth <- list(
  C    = TOTAL_C_ROSTERED   + REPLACEMENT_BUFFER,
  `1B` = TOTAL_1B_ROSTERED  + REPLACEMENT_BUFFER,
  `2B` = TOTAL_2B_ROSTERED  + REPLACEMENT_BUFFER,
  `3B` = TOTAL_3B_ROSTERED  + REPLACEMENT_BUFFER,
  SS   = TOTAL_SS_ROSTERED  + REPLACEMENT_BUFFER,
  OF   = TOTAL_OF_ROSTERED  + REPLACEMENT_BUFFER,
  SP   = TOTAL_SP_ROSTERED  + REPLACEMENT_BUFFER,
  RP   = TOTAL_RP_ROSTERED  + REPLACEMENT_BUFFER
)

# Helper: nth highest projected fpts among eligible players at position
replacement_level <- function(pts_df, pos_filter, n) {
  eligible <- pts_df %>%
    dplyr::filter(grepl(pos_filter, eligible_positions, fixed = grepl("OF|C|1B|2B|3B|SS|SP|RP", pos_filter))) %>%
    dplyr::arrange(dplyr::desc(proj_fpts))
  if (nrow(eligible) < n) return(0)
  eligible$proj_fpts[n]
}

# Replacement level for each position
repl_c  <- bat_pts %>% dplyr::filter(grepl("C",  eligible_positions)) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$C) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_1b <- bat_pts %>% dplyr::filter(grepl("1B", eligible_positions)) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$`1B`) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_2b <- bat_pts %>% dplyr::filter(grepl("2B", eligible_positions)) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$`2B`) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_3b <- bat_pts %>% dplyr::filter(grepl("3B", eligible_positions)) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$`3B`) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_ss <- bat_pts %>% dplyr::filter(grepl("SS", eligible_positions)) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$SS) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_of <- bat_pts %>% dplyr::filter(grepl("OF", eligible_positions)) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$OF) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_sp <- pit_pts %>% dplyr::filter(primary_pos == "SP") %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$SP) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

repl_rp <- pit_pts %>% dplyr::filter(primary_pos %in% c("RP", "RP_closer")) %>%
  dplyr::arrange(dplyr::desc(proj_fpts)) %>%
  dplyr::slice(depth$RP) %>% dplyr::pull(proj_fpts) %>% dplyr::first()

# Set NA replacement levels to 0
repl_c  <- dplyr::coalesce(repl_c,  0)
repl_1b <- dplyr::coalesce(repl_1b, 0)
repl_2b <- dplyr::coalesce(repl_2b, 0)
repl_3b <- dplyr::coalesce(repl_3b, 0)
repl_ss <- dplyr::coalesce(repl_ss, 0)
repl_of <- dplyr::coalesce(repl_of, 0)
repl_sp <- dplyr::coalesce(repl_sp, 0)
repl_rp <- dplyr::coalesce(repl_rp, 0)

message("Replacement levels — C:", round(repl_c), " 1B:", round(repl_1b),
        " 2B:", round(repl_2b), " 3B:", round(repl_3b), " SS:", round(repl_ss),
        " OF:", round(repl_of), " SP:", round(repl_sp), " RP:", round(repl_rp))

# VOR = projected fpts minus replacement level AT PRIMARY POSITION
bat_pts <- bat_pts %>%
  dplyr::mutate(
    pos_replacement = dplyr::case_when(
      primary_pos == "C"    ~ repl_c,
      primary_pos == "1B"   ~ repl_1b,
      primary_pos == "2B"   ~ repl_2b,
      primary_pos == "3B"   ~ repl_3b,
      primary_pos == "SS"   ~ repl_ss,
      primary_pos == "OF"   ~ repl_of,
      TRUE                  ~ repl_of   # Util defaults to OF replacement
    ),
    vor = round(proj_fpts - pos_replacement, 1)
  )

pit_pts <- pit_pts %>%
  dplyr::mutate(
    pos_replacement = dplyr::case_when(
      primary_pos == "SP"                      ~ repl_sp,
      primary_pos %in% c("RP", "RP_closer")    ~ repl_rp,
      TRUE                                      ~ repl_rp
    ),
    vor = round(proj_fpts - pos_replacement, 1)
  )

# ============================================================
# PART 4 — COMBINE INTO BIG BOARD
# ============================================================

batter_board <- bat_pts %>%
  dplyr::transmute(
    fg_id, mlbam_id, player_name, team_abbr,
    player_type,
    primary_pos,
    eligible_positions,
    proj_fpts  = round(proj_fpts),
    vor,
    # Key projected stats
    proj_pa    = final_pa,
    proj_h     = final_h,
    proj_hr    = final_hr,
    proj_r     = final_r,
    proj_rbi   = final_rbi,
    proj_sb    = final_sb,
    proj_cs    = final_cs,
    proj_bb    = final_bb,
    proj_avg   = round(final_avg, 3),
    proj_wrc_plus,
    # Statcast
    dplyr::any_of(c("sc_brl_percent", "sc_est_woba", "sc_sprint_speed",
                    "sc_avg_hit_speed"))
  )

pitcher_board <- pit_pts %>%
  dplyr::transmute(
    fg_id, mlbam_id, player_name, team_abbr,
    player_type,
    primary_pos,
    eligible_positions,
    proj_fpts  = round(proj_fpts),
    vor,
    # Key projected stats
    proj_ip    = round(final_ip, 1),
    proj_gs    = final_gs,
    proj_w     = final_w,
    proj_sv    = final_sv,
    proj_k     = final_k,
    proj_er    = round(final_er, 1),
    proj_era   = round(final_era, 2),
    proj_qs    = round(final_qs),
    dplyr::any_of(c("fg_FIP", "fg_xFIP", "best_era_estimator"))
  )

big_board <- dplyr::bind_rows(batter_board, pitcher_board) %>%
  dplyr::arrange(dplyr::desc(vor)) %>%
  dplyr::mutate(
    overall_rank = dplyr::row_number(),
    # Positional rank within primary position
    pos_rank = dplyr::ave(
      vor,
      primary_pos,
      FUN = function(x) rank(-x, ties.method = "min")
    )
  ) %>%
  dplyr::relocate(overall_rank, pos_rank, .before = player_name) %>%
  # Drop players with no real projection (< 10 PA or < 1 IP)
  dplyr::filter(
    (player_type == "batter"  & dplyr::coalesce(proj_pa, 0L) >= 10) |
    (player_type == "pitcher" & dplyr::coalesce(proj_ip, 0)  >= 1)
  )

message("Big board complete: ", nrow(big_board), " players ranked")
message("  Top 5:")
print(big_board %>% dplyr::select(overall_rank, player_name, primary_pos,
                                   proj_fpts, vor) %>% head(5))

# Save for Shiny app
if (!dir.exists("fantasy/draft_app/data")) dir.create("fantasy/draft_app/data", recursive = TRUE)
saveRDS(big_board, "fantasy/draft_app/data/big_board.rds")
message("Big board saved to fantasy/draft_app/data/big_board.rds")
