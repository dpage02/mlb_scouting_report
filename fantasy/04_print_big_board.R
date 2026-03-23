# ============================================================
# FANTASY BASEBALL — Printable Big Board
# ============================================================
# Generates a printer-friendly HTML cheat sheet.
# Open the output file in a browser → Ctrl+P / Cmd+P to print.
#
# OUTPUT:
#   reports/fantasy_big_board_YYYY-MM-DD.html
# ============================================================

source("fantasy/00_fantasy_config.R")

if (!exists("big_board") || nrow(big_board) == 0) {
  board_path <- "fantasy/draft_app/data/big_board.rds"
  if (!file.exists(board_path)) stop("Run fantasy/run_fantasy_pipeline.R first.")
  big_board <- readRDS(board_path)
}

if (!dir.exists("reports")) dir.create("reports")
out_file <- paste0("reports/fantasy_big_board_", Sys.Date(), ".html")

# ── Helpers ──────────────────────────────────────────────────

pos_color <- function(pos) {
  dplyr::case_when(
    pos == "C"                        ~ "#e8d5f5",
    pos == "1B"                       ~ "#d5e8f5",
    pos == "2B"                       ~ "#d5f5e3",
    pos == "3B"                       ~ "#f5e6d5",
    pos == "SS"                       ~ "#f5d5d5",
    pos == "OF"                       ~ "#d5f0f5",
    pos == "SP"                       ~ "#fff3cd",
    pos %in% c("RP","RP_closer")      ~ "#fde8d5",
    TRUE                               ~ "#f5f5f5"
  )
}

fmt_na <- function(x, digits = 0, suffix = "") {
  ifelse(is.na(x), "—",
    paste0(formatC(round(x, digits), format = "f", digits = digits), suffix))
}

# ── Build HTML ───────────────────────────────────────────────

css <- "
<style>
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: Arial, sans-serif; font-size: 10px; color: #222; }
  h1 { font-size: 16px; text-align: center; margin: 8px 0 2px; }
  .meta { text-align:center; font-size:9px; color:#666; margin-bottom:6px; }

  .section-title {
    font-size: 13px; font-weight: bold; padding: 4px 8px;
    background: #2c3e50; color: white; margin: 8px 0 3px;
    page-break-before: auto;
  }

  table { width: 100%; border-collapse: collapse; margin-bottom: 4px; }
  th {
    background: #2c3e50; color: white; padding: 3px 5px;
    text-align: center; font-size: 9px; font-weight: bold;
    position: sticky; top: 0;
  }
  th.left { text-align: left; }
  td { padding: 2px 4px; border-bottom: 1px solid #e0e0e0; vertical-align: middle; }
  td.num { text-align: right; }
  td.ctr { text-align: center; }

  tr:nth-child(even) { background: #f9f9f9; }
  tr:hover { background: #eef3ff; }

  .rank { font-weight: bold; color: #555; width: 28px; text-align: center; }
  .name { font-weight: 600; min-width: 120px; }
  .pos-badge {
    display: inline-block; padding: 1px 5px; border-radius: 3px;
    font-size: 9px; font-weight: bold;
  }
  .vor-pos { color: #27ae60; font-weight: bold; }
  .vor-neg { color: #e74c3c; }
  .adp-value { color: #1a73e8; font-weight: bold; }
  .adp-reach { color: #e74c3c; }

  .legend { display: flex; gap: 8px; margin: 4px 0 8px; flex-wrap: wrap; }
  .legend-item { display: flex; align-items: center; gap: 3px; font-size: 9px; }
  .legend-box { width: 12px; height: 12px; border-radius: 2px; }

  @media print {
    body { font-size: 9px; }
    .no-print { display: none; }
    table { page-break-inside: auto; }
    tr { page-break-inside: avoid; }
    .section-title { page-break-before: auto; }
    .page-break { page-break-before: always; }
  }

  .top-note {
    background: #eaf4fb; border-left: 3px solid #1a73e8;
    padding: 5px 10px; margin: 4px 0 8px; font-size: 9px;
  }
</style>
"

# Column headers for batters
bat_header <- "
<tr>
  <th class='rank'>#</th><th class='rank'>Pos#</th>
  <th class='left'>Player</th><th>Pos</th><th>Team</th>
  <th>Pts</th><th>VOR</th><th>ADP</th><th>Val</th>
  <th>PA</th><th>HR</th><th>R</th><th>RBI</th>
  <th>SB</th><th>BB</th><th>AVG</th><th>wRC+</th>
  <th>Brl%</th><th>xwOBA</th><th>Sys</th>
</tr>"

# Column headers for pitchers
pit_header <- "
<tr>
  <th class='rank'>#</th><th class='rank'>Pos#</th>
  <th class='left'>Player</th><th>Pos</th><th>Team</th>
  <th>Pts</th><th>VOR</th><th>ADP</th><th>Val</th>
  <th>IP</th><th>W</th><th>SV</th><th>K</th>
  <th>ERA</th><th>QS</th><th>FIP</th><th>Sys</th>
</tr>"

build_batter_row <- function(r) {
  vor_cls  <- if (!is.na(r$vor) && r$vor > 0) "vor-pos" else "vor-neg"
  adp_cls  <- if (!is.na(r$value_vs_adp) && r$value_vs_adp > 10) "adp-value" else
               if (!is.na(r$value_vs_adp) && r$value_vs_adp < -10) "adp-reach" else ""
  bg       <- pos_color(r$primary_pos)
  val_disp <- if (!is.na(r$value_vs_adp)) {
    sign_str <- if (r$value_vs_adp > 0) "+" else ""
    paste0(sign_str, r$value_vs_adp)
  } else "—"

  sprintf(
    "<tr style='background:%s;'>
      <td class='rank'>%s</td><td class='rank'>%s</td>
      <td class='name'>%s</td>
      <td class='ctr'><span class='pos-badge' style='background:%s;'>%s</span></td>
      <td class='ctr'>%s</td>
      <td class='num'><b>%s</b></td>
      <td class='num %s'>%s</td>
      <td class='num'>%s</td>
      <td class='num %s'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='ctr'>%s</td>
    </tr>",
    ifelse(as.integer(r$overall_rank) %% 2 == 0, "#f9f9f9", "white"),
    r$overall_rank, r$pos_rank,
    r$player_name,
    bg, r$primary_pos,
    dplyr::coalesce(r$team_abbr, "?"),
    fmt_na(r$proj_fpts),
    vor_cls, fmt_na(r$vor),
    fmt_na(r$adp, 1),
    adp_cls, val_disp,
    fmt_na(r$proj_pa), fmt_na(r$proj_hr), fmt_na(r$proj_r),
    fmt_na(r$proj_rbi), fmt_na(r$proj_sb), fmt_na(r$proj_bb),
    fmt_na(r$proj_avg, 3),
    fmt_na(r$proj_wrc_plus),
    fmt_na(r$sc_brl_percent, 1),
    fmt_na(r$sc_est_woba, 3),
    dplyr::coalesce(as.character(r$n_systems), "?")
  )
}

build_pitcher_row <- function(r) {
  vor_cls <- if (!is.na(r$vor) && r$vor > 0) "vor-pos" else "vor-neg"
  adp_cls  <- if (!is.na(r$value_vs_adp) && r$value_vs_adp > 10) "adp-value" else
               if (!is.na(r$value_vs_adp) && r$value_vs_adp < -10) "adp-reach" else ""
  bg      <- pos_color(r$primary_pos)
  val_disp <- if (!is.na(r$value_vs_adp)) {
    sign_str <- if (r$value_vs_adp > 0) "+" else ""
    paste0(sign_str, r$value_vs_adp)
  } else "—"

  sprintf(
    "<tr style='background:%s;'>
      <td class='rank'>%s</td><td class='rank'>%s</td>
      <td class='name'>%s</td>
      <td class='ctr'><span class='pos-badge' style='background:%s;'>%s</span></td>
      <td class='ctr'>%s</td>
      <td class='num'><b>%s</b></td>
      <td class='num %s'>%s</td>
      <td class='num'>%s</td>
      <td class='num %s'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='num'>%s</td>
      <td class='num'>%s</td><td class='ctr'>%s</td>
    </tr>",
    ifelse(as.integer(r$overall_rank) %% 2 == 0, "#f9f9f9", "white"),
    r$overall_rank, r$pos_rank,
    r$player_name,
    bg, dplyr::if_else(r$primary_pos == "RP_closer", "RP★", r$primary_pos),
    dplyr::coalesce(r$team_abbr, "?"),
    fmt_na(r$proj_fpts),
    vor_cls, fmt_na(r$vor),
    fmt_na(r$adp, 1),
    adp_cls, val_disp,
    fmt_na(r$proj_ip, 1), fmt_na(r$proj_w), fmt_na(r$proj_sv),
    fmt_na(r$proj_k),
    fmt_na(r$proj_era, 2), fmt_na(r$proj_qs),
    fmt_na(r$fg_xFIP, 2),
    dplyr::coalesce(as.character(r$n_systems), "?")
  )
}

# ── Build page sections ───────────────────────────────────────

# Overall top 300 (mixed)
top_overall <- big_board %>% dplyr::slice_head(n = 300)

# By position
positions_to_show <- c("C","1B","2B","3B","SS","OF","SP","RP","RP_closer")

build_section_html <- function(df, header, row_fn, title) {
  rows <- paste0(vapply(seq_len(nrow(df)), function(i) row_fn(df[i,]), character(1)),
                 collapse = "\n")
  paste0(
    sprintf("<div class='section-title'>%s (%d players)</div>\n", title, nrow(df)),
    "<table>", header, "<tbody>", rows, "</tbody></table>\n"
  )
}

# ── Overall mixed board ───────────────────────────────────────
overall_rows <- paste0(
  vapply(seq_len(nrow(top_overall)), function(i) {
    r <- top_overall[i,]
    if (r$player_type == "batter") build_batter_row(r)
    else build_pitcher_row(r)
  }, character(1)),
  collapse = "\n"
)

overall_header <- "
<tr>
  <th class='rank'>#</th><th class='rank'>Pos#</th>
  <th class='left'>Player</th><th>Pos</th><th>Team</th>
  <th>Pts</th><th>VOR</th><th>ADP</th><th>Val</th>
  <th colspan='8'>Key Stats</th><th>Sys</th>
</tr>"

# ── Positional boards ─────────────────────────────────────────
pos_sections <- ""
for (pos in c("C","1B","2B","3B","SS","OF")) {
  df <- big_board %>%
    dplyr::filter(grepl(pos, eligible_positions, fixed = TRUE)) %>%
    dplyr::arrange(dplyr::desc(vor)) %>%
    dplyr::slice_head(n = 30)
  if (nrow(df) == 0) next
  pos_sections <- paste0(pos_sections,
    build_section_html(df, bat_header, build_batter_row,
                       paste0(pos, " Rankings")))
}

# SP and RP
sp_df <- big_board %>% dplyr::filter(primary_pos == "SP") %>%
  dplyr::arrange(dplyr::desc(vor)) %>% dplyr::slice_head(n = 60)
rp_df <- big_board %>% dplyr::filter(primary_pos %in% c("RP","RP_closer")) %>%
  dplyr::arrange(dplyr::desc(vor)) %>% dplyr::slice_head(n = 40)

sp_html <- build_section_html(sp_df, pit_header, build_pitcher_row, "Starting Pitchers")
rp_html <- build_section_html(rp_df, pit_header, build_pitcher_row, "Relief Pitchers (Closers ★)")

# ── Legend ────────────────────────────────────────────────────
legend_html <- paste0(
  "<div class='legend'>",
  paste0(vapply(c("C","1B","2B","3B","SS","OF","SP","RP"), function(p) {
    sprintf("<div class='legend-item'><div class='legend-box' style='background:%s;'></div>%s</div>",
            pos_color(p), p)
  }, character(1)), collapse = ""),
  "<div class='legend-item' style='margin-left:12px;'>",
  "<span class='adp-value'>Blue +##</span> = ADP value (drafted later than rank)</div>",
  "<div class='legend-item'>",
  "<span class='adp-reach'>Red -##</span> = Being overdrafted</div>",
  "</div>"
)

# ── Assemble full page ────────────────────────────────────────
html <- paste0(
  "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
  "<title>Fantasy Big Board — ", Sys.Date(), "</title>",
  css,
  "</head><body>",

  "<h1>⚾ Fantasy Baseball Big Board</h1>",
  "<div class='meta'>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
  " · H2H Points · 10 Teams · Consensus projections (Steamer/ZiPS/ATC/FG DC/THE BAT) + Statcast blend",
  " · VOR = Value Over Replacement · Val = ADP value (+ = undervalued)</div>",

  legend_html,

  "<div class='top-note'>",
  "<b>How to use:</b> VOR = projected fantasy points above replacement level at position. ",
  "Val column = ADP rank minus your board rank — <b>positive values are UNDERVALUED</b> (being drafted later than they should be). ",
  "Pos# = rank at their position. RP★ = projected closer.",
  "</div>",

  "<div class='section-title'>Overall Top 300 — Mixed</div>",
  "<table>", overall_header, "<tbody>", overall_rows, "</tbody></table>",

  "<div class='page-break'></div>",
  "<div class='section-title' style='font-size:15px; padding:6px 8px;'>POSITIONAL RANKINGS</div>",
  pos_sections,
  "<div class='page-break'></div>",
  sp_html,
  rp_html,

  "</body></html>"
)

writeLines(html, out_file)
message("Printable big board saved: ", out_file)
message("Open in browser → Cmd+P (Mac) or Ctrl+P (Windows) to print")
