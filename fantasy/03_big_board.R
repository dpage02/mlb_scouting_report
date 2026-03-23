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
    dplyr::any_of(c("sc_brl_percent","sc_est_woba","sc_sprint_speed",
                    "sc_avg_hit_speed","n_systems"))
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
    dplyr::any_of(c("fg_FIP","fg_xFIP","n_systems"))
  )

big_board <- dplyr::bind_rows(batter_board, pitcher_board) %>%
  dplyr::filter(
    (player_type == "batter"  & dplyr::coalesce(proj_pa, 0L) >= 10) |
    (player_type == "pitcher" & dplyr::coalesce(proj_ip, 0)  >= 1)
  ) %>%
  dplyr::arrange(dplyr::desc(vor)) %>%
  dplyr::mutate(
    overall_rank = dplyr::row_number(),
    pos_rank     = as.integer(dplyr::ave(
      vor, primary_pos, FUN = function(x) rank(-x, ties.method = "min")
    ))
  )

# ============================================================
# PART 5 — JOIN ADP & CALCULATE VALUE VS ADP
# ============================================================

if (exists("adp_master") && nrow(adp_master) > 0) {
  big_board <- big_board %>%
    dplyr::left_join(
      adp_master %>% dplyr::select(fg_id, adp, adp_fantasypros, adp_yahoo, fp_rank),
      by = "fg_id"
    ) %>%
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
        sum(big_board$player_type=="pitcher"), " pitchers")
message("Top 10:")
print(big_board %>%
  dplyr::select(overall_rank, player_name, primary_pos,
                proj_fpts, vor, adp, value_vs_adp) %>%
  head(10), n = 10)

# Save for Shiny app + printable board
if (!dir.exists("fantasy/draft_app/data")) dir.create("fantasy/draft_app/data", recursive = TRUE)
saveRDS(big_board, "fantasy/draft_app/data/big_board.rds")
message("Saved: fantasy/draft_app/data/big_board.rds")
