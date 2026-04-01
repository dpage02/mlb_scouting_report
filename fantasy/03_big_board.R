# ============================================================
# FANTASY BASEBALL — Build Big Board
# ============================================================
# Calculates projected fantasy points, VOR, and ADP value.
#
# OUTPUT:
#   big_board  — all draftable players ranked by VOR
#                includes: proj_fpts, vor, adp, value_vs_adp
# ============================================================

source("fantasy/00_fantasy_config.R")

W <- BAT_WEIGHTS
P <- PITCH_WEIGHTS

# ============================================================
# PART 1 — BATTER FANTASY POINTS
# ============================================================

bat_pts <- proj_bat %>%
  dplyr::mutate(
    final_1b  = pmax(0L, final_h - final_2b - final_3b - final_hr),
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
    player_type = "batter"
  )

# ── Position eligibility ─────────────────────────────────────

parse_positions <- function(pos_str) {
  if (is.na(pos_str) || pos_str == "") return("Util")
  pos_str  <- gsub("[-,]", "/", pos_str)
  positions <- trimws(unlist(strsplit(pos_str, "/")))
  slot_map  <- c(
    "C"  = "C",  "1B" = "1B", "2B" = "2B", "3B" = "3B", "SS" = "SS",
    "LF" = "OF", "CF" = "OF", "RF" = "OF", "OF" = "OF",
    "DH" = "Util", "SP" = "SP", "RP" = "RP", "P" = "SP"
  )
  mapped <- unique(slot_map[positions[positions %in% names(slot_map)]])
  if (length(mapped) == 0) return("Util")
  paste(mapped, collapse = "/")
}


# ── Position resolution ───────────────────────────────────────────────────────
# Priority order (each layer fills gaps left by the one above):
#
#   Layer 1 — MLB fielding stats (20+ games threshold)
#     Mirrors Yahoo's actual eligibility rules. One row per player with
#     all positions where they played 20+ games: Ben Rice → "C/1B"
#     Source: mlb_positions built in 01b_adp_rankings.R
#
#   Layer 2 — FanGraphs depth charts
#     Current 2026 roster position. Catches players not in last season's
#     MLB data (IL returnees, prospects debuting in 2026, etc.)
#     Single position only (no multi-pos from this source).
#
#   Layer 3 — Projection API "Pos" column (proj_pos_raw already set)
#     Last resort: whatever the FG projection system returned.

# Layer 1 — MLB fielding eligibility (primary source)
if (exists("mlb_positions") && !is.null(mlb_positions) && nrow(mlb_positions) > 100) {
  proj_bat <- proj_bat %>%
    dplyr::left_join(
      mlb_positions %>% dplyr::select(mlbam_id, mlb_eligible),
      by = "mlbam_id"
    ) %>%
    dplyr::mutate(proj_pos_raw = dplyr::coalesce(mlb_eligible, proj_pos_raw)) %>%
    dplyr::select(-mlb_eligible)
  message("MLB eligibility applied: ", sum(!is.na(proj_bat$proj_pos_raw)),
          " / ", nrow(proj_bat), " players | multi-pos: ",
          sum(grepl("/", proj_bat$proj_pos_raw, fixed = FALSE), na.rm = TRUE))
} else {
  message("mlb_positions not available — skipping to depth_charts")
}

# Layer 2 — depth_charts fallback (catches 2026 rookies / IL returnees)
if (exists("depth_charts") && !is.null(depth_charts) && nrow(depth_charts) > 100) {
  dc_pos <- depth_charts %>%
    dplyr::filter(!is.na(mlbam_id), !is.na(fg_position), nchar(fg_position) >= 1,
                  !fg_position %in% c("SP", "RP", "SP/RP")) %>%
    dplyr::group_by(mlbam_id) %>%
    dplyr::summarise(
      dc_pos = paste(unique(fg_position), collapse = "/"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dc_pos = dplyr::if_else(dc_pos == "", NA_character_, dc_pos))

  proj_bat <- proj_bat %>%
    dplyr::left_join(dc_pos, by = "mlbam_id") %>%
    dplyr::mutate(proj_pos_raw = dplyr::coalesce(proj_pos_raw, dc_pos)) %>%
    dplyr::select(-dc_pos)
  message("Depth chart fallback: ", sum(!is.na(proj_bat$proj_pos_raw)),
          " / ", nrow(proj_bat), " players now have position")
}

# Layer 3 — FP position pages (rarely works due to JS rendering, but kept as hook)
if (exists("fp_positions") && !is.null(fp_positions) && nrow(fp_positions) > 10) {
  fp_map <- fp_positions %>% dplyr::select(name_key, fp_eligible)
  proj_bat <- proj_bat %>%
    dplyr::mutate(name_key = normalize_name(player_name)) %>%
    dplyr::left_join(fp_map, by = "name_key") %>%
    dplyr::mutate(proj_pos_raw = dplyr::coalesce(fp_eligible, proj_pos_raw)) %>%
    dplyr::select(-name_key, -fp_eligible)
  message("FP positions applied: ", sum(!is.na(proj_bat$proj_pos_raw)), " players")
}

# Diagnostic
n_missing <- sum(is.na(proj_bat$proj_pos_raw))
message("Final position coverage: ", nrow(proj_bat) - n_missing, " / ", nrow(proj_bat),
        " — ", n_missing, " will show as Util")
if (n_missing > 0 && n_missing <= 25) {
  message("  No position: ", paste(proj_bat$player_name[is.na(proj_bat$proj_pos_raw)], collapse = ", "))
}
message("  Sample: ", paste(head(na.omit(unique(proj_bat$proj_pos_raw)), 10), collapse = " | "))

bat_pts <- bat_pts %>%
  dplyr::mutate(
    proj_pos_raw       = proj_bat$proj_pos_raw[match(fg_id, proj_bat$fg_id)],
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

util_batters <- bat_pts %>% dplyr::filter(primary_pos == "Util")
if (nrow(util_batters) > 0) {
  message("  Util batters (no position found): ", nrow(util_batters),
          " — ", paste(head(util_batters$player_name, 10), collapse=", "))
} else {
  message("  All batters have a fielding position")
}

message("Position breakdown: ", paste(names(table(bat_pts$primary_pos)),
        table(bat_pts$primary_pos), sep="=", collapse=" | "))

# ============================================================
# PART 2 — PITCHER FANTASY POINTS
# ============================================================

pit_pts <- proj_pit %>%
  dplyr::mutate(
    proj_fpts   = (final_ip  * 3 * P$out  +
                   final_k   * P$k        +
                   final_w   * P$win      +
                   final_sv  * P$sv       +
                   final_er  * P$er       +
                   final_qs  * P$qs),
    player_type        = "pitcher",
    primary_pos        = proj_role,
    eligible_positions = proj_role
  )

# ============================================================
# PART 3 — VALUE OVER REPLACEMENT (VOR)
# ============================================================

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

nth_pts <- function(df, pos_regex, n) {
  eligible <- df %>%
    dplyr::filter(grepl(pos_regex, eligible_positions)) %>%
    dplyr::arrange(dplyr::desc(proj_fpts))
  if (nrow(eligible) < n) return(0)
  dplyr::coalesce(eligible$proj_fpts[n], 0)
}

repl_c  <- nth_pts(bat_pts, "C",  depth$C)
repl_1b <- nth_pts(bat_pts, "1B", depth$`1B`)
repl_2b <- nth_pts(bat_pts, "2B", depth$`2B`)
repl_3b <- nth_pts(bat_pts, "3B", depth$`3B`)
repl_ss <- nth_pts(bat_pts, "SS", depth$SS)
repl_of <- nth_pts(bat_pts, "OF", depth$OF)
repl_sp <- nth_pts(pit_pts %>% dplyr::filter(primary_pos == "SP"), "SP", depth$SP)
repl_rp <- nth_pts(pit_pts %>% dplyr::filter(primary_pos %in% c("RP","RP_closer")), "RP|RP_closer", depth$RP)

# ── Util-adjusted replacement level ──────────────────────────────────────────
# Util slots (4 per team) mean 40 extra hitters get drafted across the league.
# The Util replacement = the Nth best hitter overall, where N covers ALL hitter
# slots (positional + Util + buffer). Every hitter is Util-eligible, so a
# hitter's true VOR = max(positional VOR, Util VOR).
# Effect: scarce positions (C, SS) keep their premium; abundant positions
# (OF, 1B) get a better floor because they can slide into Util.
total_hitter_slots <- TOTAL_C_ROSTERED + TOTAL_1B_ROSTERED + TOTAL_2B_ROSTERED +
                      TOTAL_3B_ROSTERED + TOTAL_SS_ROSTERED + TOTAL_OF_ROSTERED +
                      TOTAL_UTIL_ROSTERED + REPLACEMENT_BUFFER

repl_util <- {
  sorted_batters <- bat_pts %>% dplyr::arrange(dplyr::desc(proj_fpts))
  if (nrow(sorted_batters) >= total_hitter_slots)
    dplyr::coalesce(sorted_batters$proj_fpts[total_hitter_slots], 0)
  else 0
}

message("Replacement levels — C:", round(repl_c), " 1B:", round(repl_1b),
        " 2B:", round(repl_2b), " 3B:", round(repl_3b), " SS:", round(repl_ss),
        " OF:", round(repl_of), " SP:", round(repl_sp), " RP:", round(repl_rp))
message("Util replacement (", total_hitter_slots, "th best hitter): ", round(repl_util))

# Replacement level lookup by position — used for multi-position VOR
repl_by_pos <- c(
  "C"    = repl_c,  "1B"   = repl_1b, "2B" = repl_2b,
  "3B"   = repl_3b, "SS"   = repl_ss, "OF" = repl_of,
  "Util" = repl_util
)

bat_pts <- bat_pts %>%
  dplyr::mutate(
    # Multi-position VOR: find the lowest replacement level across ALL eligible
    # slots — lower replacement = higher VOR = most valuable position to occupy.
    # e.g. SS/2B/OF gets SS replacement if SS is scarcer than 2B or OF.
    # Then take max vs the Util floor (every hitter is Util-eligible).
    best_pos_repl = sapply(eligible_positions, function(ep) {
      pos_list <- unlist(strsplit(ep, "/"))
      pos_list <- pos_list[pos_list %in% names(repl_by_pos)]
      if (length(pos_list) == 0) return(repl_util)
      min(repl_by_pos[pos_list])
    }),
    vor = round(pmax(proj_fpts - best_pos_repl,
                     proj_fpts - repl_util), 1)
  )

pit_pts <- pit_pts %>%
  dplyr::mutate(
    pos_replacement = dplyr::if_else(primary_pos == "SP", repl_sp, repl_rp),
    vor = round(proj_fpts - pos_replacement, 1)
  )

# ============================================================
# PART 4 — COMBINE INTO BIG BOARD
# ============================================================

batter_board <- bat_pts %>%
  dplyr::transmute(
    fg_id, mlbam_id, player_name, team_abbr,
    player_type, primary_pos, eligible_positions,
    proj_fpts  = round(proj_fpts),
    vor,
    proj_pa    = final_pa,  proj_h   = final_h,
    proj_hr    = final_hr,  proj_r   = final_r,
    proj_rbi   = final_rbi, proj_sb  = final_sb,
    proj_cs    = final_cs,  proj_bb  = final_bb,
    proj_avg   = round(final_avg, 3),
    proj_wrc_plus,
    sc_brl_percent  = if ("sc_brl_percent"  %in% names(bat_pts)) sc_brl_percent  else NA_real_,
    sc_est_woba     = if ("sc_est_woba"     %in% names(bat_pts)) sc_est_woba     else NA_real_,
    sc_sprint_speed = if ("sc_sprint_speed" %in% names(bat_pts)) sc_sprint_speed else NA_real_,
    sc_avg_hit_speed= if ("sc_avg_hit_speed"%in% names(bat_pts)) sc_avg_hit_speed else NA_real_,
    n_systems       = if ("n_systems"       %in% names(bat_pts)) n_systems       else NA_integer_
  )

pitcher_board <- pit_pts %>%
  dplyr::transmute(
    fg_id, mlbam_id, player_name, team_abbr,
    player_type, primary_pos, eligible_positions,
    proj_fpts  = round(proj_fpts),
    vor,
    proj_ip    = round(final_ip, 1), proj_gs = final_gs,
    proj_w     = final_w,  proj_sv  = final_sv,
    proj_k     = final_k,  proj_er  = round(final_er, 1),
    proj_era   = round(final_era, 2), proj_qs = round(final_qs),
    fg_FIP     = if ("fg_FIP"  %in% names(pit_pts)) fg_FIP  else NA_real_,
    fg_xFIP    = if ("fg_xFIP" %in% names(pit_pts)) fg_xFIP else NA_real_,
    n_systems  = if ("n_systems" %in% names(pit_pts)) n_systems else NA_integer_
  )

# ── Diagnostic: check for known players that should be on the board ──────────
check_players <- c("Baldwin", "Rice", "Acuna", "Rodriguez", "Ohtani", "Judge", "Ramirez")
for (nm in check_players) {
  found <- bat_pts %>% dplyr::filter(grepl(nm, player_name, ignore.case = TRUE))
  if (nrow(found) > 0) {
    message("  CHECK ", nm, ": found ", nrow(found), " — PA=",
            paste(dplyr::coalesce(found$final_pa, 0L), collapse=","),
            " pos=", paste(found$primary_pos, collapse=","))
  } else {
    message("  CHECK ", nm, ": NOT FOUND in bat_pts (missing from projection systems)")
  }
}

filtered <- dplyr::bind_rows(batter_board, pitcher_board) %>%
  dplyr::filter(
    # Batters: at least 25 projected PA — low enough to catch emerging starters
    # who are only in some projection systems (bringing their average PA down)
    (player_type == "batter"  & dplyr::coalesce(proj_pa, 0L) >= 25) |
    # SP: at least 20 IP; RP: at least 8 IP
    (player_type == "pitcher" & primary_pos == "SP" &
       dplyr::coalesce(proj_ip, 0) >= 20) |
    (player_type == "pitcher" & primary_pos %in% c("RP","RP_closer") &
       dplyr::coalesce(proj_ip, 0) >= 8)
  )

# ── Two-way players (Ohtani): combine batter + pitcher points ──────────────
# In Yahoo H2H points leagues, a two-way player scores BOTH batting and
# pitching stats — so we sum proj_fpts and vor from both sides.
twoway_ids <- filtered %>%
  dplyr::count(fg_id) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(fg_id)

if (length(twoway_ids) > 0) {
  twoway_bat <- filtered %>% dplyr::filter(fg_id %in% twoway_ids, player_type == "batter")
  twoway_pit <- filtered %>% dplyr::filter(fg_id %in% twoway_ids, player_type == "pitcher")

  message("Two-way players (combined bat+pitch): ",
          paste(twoway_bat$player_name, collapse = ", "))

  # Start with batter row, then add pitcher fpts+VOR on top.
  pit_cols <- c("proj_ip","proj_gs","proj_w","proj_sv","proj_k","proj_er","proj_era","proj_qs")

  pit_pts_only <- twoway_pit %>%
    dplyr::select(fg_id, pit_fpts = proj_fpts, pit_vor = vor,
                  dplyr::any_of(pit_cols))

  twoway_combined <- twoway_bat %>%
    dplyr::select(-dplyr::any_of(pit_cols)) %>%
    dplyr::left_join(pit_pts_only, by = "fg_id") %>%
    dplyr::mutate(
      proj_fpts   = proj_fpts + dplyr::coalesce(pit_fpts, 0),
      vor         = vor       + dplyr::coalesce(pit_vor,  0),
      player_type = "two-way"
    ) %>%
    dplyr::select(-pit_fpts, -pit_vor)

  big_board <- dplyr::bind_rows(
    filtered %>% dplyr::filter(!fg_id %in% twoway_ids),
    twoway_combined
  )
} else {
  big_board <- filtered
}

big_board <- big_board %>%
  dplyr::arrange(dplyr::desc(vor)) %>%
  dplyr::mutate(
    overall_rank = dplyr::row_number(),
    pos_rank     = as.integer(ave(
      vor, primary_pos, FUN = function(x) rank(-x, ties.method = "min")
    ))
  )

# ============================================================
# PART 5 — JOIN ADP & CALCULATE VALUE VS ADP
# ============================================================

if (exists("adp_master") && nrow(adp_master) > 0 && "adp" %in% names(adp_master)) {
  adp_clean <- adp_master %>%
    dplyr::filter(!is.na(fg_id)) %>%
    dplyr::arrange(adp) %>%
    dplyr::distinct(fg_id, .keep_all = TRUE) %>%
    dplyr::select(fg_id, adp, adp_fantasypros, adp_yahoo, fp_rank)

  big_board <- big_board %>%
    dplyr::left_join(adp_clean, by = "fg_id") %>%
    dplyr::mutate(
      # value_vs_adp: positive = being drafted LATER than their rank suggests (value)
      # negative = being drafted EARLIER (overdrafted)
      value_vs_adp = dplyr::if_else(
        !is.na(adp),
        round(adp - overall_rank),
        NA_integer_
      )
    )
  message("ADP joined: ", sum(!is.na(big_board$adp)), " players with ADP data")
} else {
  big_board <- big_board %>%
    dplyr::mutate(adp = NA_real_, adp_fantasypros = NA_real_,
                  adp_yahoo = NA_real_, value_vs_adp = NA_integer_)
}

big_board <- big_board %>%
  dplyr::mutate(
    tier = dplyr::case_when(
      vor >  TIER_BREAKS[1] ~ 1L,
      vor >  TIER_BREAKS[2] ~ 2L,
      vor >  TIER_BREAKS[3] ~ 3L,
      vor >  TIER_BREAKS[4] ~ 4L,
      vor >= TIER_BREAKS[5] ~ 5L,
      TRUE                  ~ 6L
    )
  ) %>%
  dplyr::relocate(overall_rank, pos_rank, tier, .before = player_name) %>%
  dplyr::slice_head(n = BOARD_SIZE)

message("Tier breakdown: ", paste(
  paste0("T", 1:6, "=", tabulate(big_board$tier, nbins = 6)),
  collapse = " | "
))

message("Big board: ", nrow(big_board), " players | ",
        sum(big_board$player_type=="batter"), " batters | ",
        sum(big_board$player_type=="pitcher"), " pitchers | ",
        sum(big_board$player_type=="two-way"), " two-way")
message("Top 10:")
print(big_board %>%
  dplyr::select(overall_rank, player_name, primary_pos,
                proj_fpts, vor, adp, value_vs_adp) %>%
  head(10), n = 10)

# Save for Shiny app + printable board
if (!dir.exists("fantasy/draft_app/data")) dir.create("fantasy/draft_app/data", recursive = TRUE)
saveRDS(big_board, "fantasy/draft_app/data/big_board.rds")
message("Saved: fantasy/draft_app/data/big_board.rds")
