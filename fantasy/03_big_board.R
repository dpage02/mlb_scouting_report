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


# Diagnostic: show what position strings look like
pos_sample <- head(na.omit(unique(proj_bat$proj_pos_raw)), 10)
message("Position sample from projections: ", paste(pos_sample, collapse=" | "))
pct_na_pos <- mean(is.na(proj_bat$proj_pos_raw)) * 100
message("proj_pos_raw NA rate: ", round(pct_na_pos), "%")

# If >80% of position strings are NA, fall back to offense_master_season fg_position
if (pct_na_pos > 80) {
  # Try offense_master_season first (broadest coverage — all players who appeared last season)
  pos_source <- NULL
  if (exists("offense_master_season") && "fg_position" %in% names(offense_master_season)) {
    message("  Using offense_master_season fg_position as fallback")
    pos_source <- offense_master_season %>%
      dplyr::arrange(mlbam_id, dplyr::desc(dplyr::coalesce(mlb_pa, 0L))) %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, pos_fallback = fg_position)
  } else if (exists("player_season_fg_offense") && "fg_position" %in% names(player_season_fg_offense)) {
    message("  Using player_season_fg_offense fg_position as fallback")
    pos_source <- player_season_fg_offense %>%
      dplyr::distinct(mlbam_id, .keep_all = TRUE) %>%
      dplyr::select(mlbam_id, pos_fallback = fg_position)
  }
  if (!is.null(pos_source)) {
    proj_bat <- proj_bat %>%
      dplyr::left_join(pos_source, by = "mlbam_id") %>%
      dplyr::mutate(proj_pos_raw = dplyr::coalesce(proj_pos_raw, pos_fallback)) %>%
      dplyr::select(-pos_fallback)
    pct_na_after <- mean(is.na(proj_bat$proj_pos_raw)) * 100
    message("  Position NA rate after fallback: ", round(pct_na_after), "%")
  }
}

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

message("Replacement levels — C:", round(repl_c), " 1B:", round(repl_1b),
        " 2B:", round(repl_2b), " 3B:", round(repl_3b), " SS:", round(repl_ss),
        " OF:", round(repl_of), " SP:", round(repl_sp), " RP:", round(repl_rp))

bat_pts <- bat_pts %>%
  dplyr::mutate(
    pos_replacement = dplyr::case_when(
      primary_pos == "C"  ~ repl_c,  primary_pos == "1B" ~ repl_1b,
      primary_pos == "2B" ~ repl_2b, primary_pos == "3B" ~ repl_3b,
      primary_pos == "SS" ~ repl_ss, primary_pos == "OF" ~ repl_of,
      TRUE                ~ repl_of
    ),
    vor = round(proj_fpts - pos_replacement, 1)
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

filtered <- dplyr::bind_rows(batter_board, pitcher_board) %>%
  dplyr::filter(
    # Batters: at least 50 projected PA (eliminates fringe guys)
    (player_type == "batter"  & dplyr::coalesce(proj_pa, 0L) >= 50) |
    # SP: at least 30 IP; RP: at least 10 IP
    (player_type == "pitcher" & primary_pos == "SP" &
       dplyr::coalesce(proj_ip, 0) >= 30) |
    (player_type == "pitcher" & primary_pos %in% c("RP","RP_closer") &
       dplyr::coalesce(proj_ip, 0) >= 10)
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
  # batter_board has NA for pitcher cols (from bind_rows); drop them before join.
  pit_cols <- c("proj_ip","proj_gs","proj_w","proj_sv","proj_k","proj_er","proj_era","proj_qs")

  pit_pts_only <- twoway_pit %>%
    dplyr::select(fg_id, pit_fpts = proj_fpts, pit_vor = vor,
                  dplyr::any_of(pit_cols))

  twoway_combined <- twoway_bat %>%
    dplyr::select(-dplyr::any_of(pit_cols)) %>%   # drop NA pitcher cols from batter row
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
  dplyr::relocate(overall_rank, pos_rank, .before = player_name)

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
