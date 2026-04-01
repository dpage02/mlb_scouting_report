# ============================================================
# FANTASY BASEBALL — Draft Day Shiny App (Two-Team Edition)
# ============================================================
# USAGE:
#   shiny::runApp("fantasy/draft_app")
#
# DRAFT FLOW:
#   1. Click any player row in the board to select them
#   2. Click "→ Team 1", "→ Team 2", or "Opponent Drafted"
#   3. Or click "✓ Draft" inside either recommendation box
# ============================================================

library(shiny)
library(dplyr)
library(DT)

# ── Load big board ───────────────────────────────────────────
board_path <- "data/big_board.rds"
if (!file.exists(board_path)) board_path <- "fantasy/draft_app/data/big_board.rds"
if (!file.exists(board_path)) stop("Run fantasy/run_fantasy_pipeline.R first.")

BIG_BOARD_ORIG <- readRDS(board_path) %>%
  dplyr::mutate(
    status       = "Available",
    drafted_pick = NA_integer_,
    drafted_by   = NA_character_
  )

# Ensure adp columns exist (handle boards built before ADP scraping)
for (col in c("adp","adp_fantasypros","adp_yahoo","fp_rank","value_vs_adp")) {
  if (!col %in% names(BIG_BOARD_ORIG))
    BIG_BOARD_ORIG[[col]] <- NA_real_
}

# Ensure tier column exists (handle boards built before tier logic)
if (!"tier" %in% names(BIG_BOARD_ORIG)) {
  BIG_BOARD_ORIG <- BIG_BOARD_ORIG %>%
    dplyr::mutate(tier = dplyr::case_when(
      vor >  200 ~ 1L, vor >  150 ~ 2L, vor >  100 ~ 3L,
      vor >   60 ~ 4L, vor >=  20 ~ 5L, TRUE        ~ 6L
    ))
}

# ── League settings ──────────────────────────────────────────
LEAGUE_TEAMS  <- 10
MY_TEAM_1     <- "My Team 1"
MY_TEAM_2     <- "My Team 2"
MY_TEAMS      <- c(MY_TEAM_1, MY_TEAM_2)

ROSTER_REQS <- c(C=1, `1B`=1, `2B`=1, `3B`=1, SS=1, OF=3, SP=5, RP=3)
UTIL_SLOTS  <- 4

pos_label <- function(p) dplyr::case_when(
  p == "C"  ~ "C",  p == "1B" ~ "1B", p == "2B" ~ "2B",
  p == "3B" ~ "3B", p == "SS" ~ "SS", p == "OF" ~ "OF",
  p == "SP" ~ "SP", p %in% c("RP","RP_closer") ~ "RP",
  TRUE ~ p
)

safe_val <- function(x) { v <- tryCatch(x, error=function(e) NULL); if (is.null(v) || length(v)==0) NA else v[1] }

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Roster checker (multi-position aware) ────────────────────
# Fills scarce positions first so a C/1B player satisfies C before 1B.
check_roster <- function(my_players) {
  if (nrow(my_players) == 0) return(list(needed = names(ROSTER_REQS)))
  slot_order <- c("C","SS","2B","3B","1B","OF","SP","RP")
  used_ids <- character(0)
  filled   <- character(0)
  for (slot in slot_order) {
    need <- ROSTER_REQS[[slot]]
    if (is.null(need) || need == 0) next
    elig <- my_players %>%
      dplyr::filter(
        !fg_id %in% used_ids,
        if (slot %in% c("SP","RP"))
          grepl(slot, primary_pos)
        else
          grepl(slot, eligible_positions, fixed = TRUE)
      )
    n_fill <- min(nrow(elig), need)
    if (n_fill > 0) {
      used_ids <- c(used_ids, elig$fg_id[seq_len(n_fill)])
      filled   <- c(filled, rep(slot, n_fill))
    }
  }
  needed <- character(0)
  for (slot in names(ROSTER_REQS)) {
    gap <- ROSTER_REQS[[slot]] - sum(filled == slot)
    if (gap > 0) needed <- c(needed, rep(slot, gap))
  }
  list(needed = needed)
}

# ── Position pool scarcity ────────────────────────────────────
# For each position: how many above-replacement players remain vs.
# how many total need to be drafted across the league?
pos_pool_depth <- function(available) {
  slots <- c(C=10, `1B`=10, `2B`=10, `3B`=10, SS=10, OF=30, SP=50, RP=30)
  rows <- lapply(names(slots), function(pos) {
    total <- slots[[pos]]
    if (pos %in% c("SP","RP")) {
      n <- sum(grepl(pos, available$primary_pos) & !is.na(available$vor) & available$vor > 0)
    } else {
      n <- sum(grepl(pos, available$eligible_positions, fixed=TRUE) & !is.na(available$vor) & available$vor > 0)
    }
    status <- if (n < total * 0.6) "critical" else if (n < total * 1.3) "thinning" else "ok"
    data.frame(pos=pos, avail=n, needed=total, ratio=n/max(total,1),
               status=status, stringsAsFactors=FALSE)
  })
  do.call(rbind, rows)
}

# ── Recommendation engine ─────────────────────────────────────
# Tier-based hybrid strategy:
#   1. Find BPA (highest VOR available) and their tier.
#   2. Collect all available players in the same tier.
#   3. Among same-tier players who fill a positional need, prefer the
#      highest-VOR one — never reach into a lower tier to fill a need.
#   4. Scarcity alerts are shown as secondary context.
#
# Returns a list with:
#   $pick    — recommended player
#   $bpa     — best player available by raw VOR
#   $reason  — explanation string
#   $by_pos  — best available at each unfilled position (any tier, for display)
#   $alerts  — scarcity warning strings
recommend_pick <- function(board, my_players) {
  available <- board %>%
    dplyr::filter(status == "Available") %>%
    dplyr::arrange(dplyr::desc(dplyr::coalesce(vor, 0)))
  if (nrow(available) == 0) return(NULL)

  bpa        <- available %>% dplyr::slice(1)
  needed_pos <- check_roster(my_players)$needed
  pool       <- pos_pool_depth(available)

  # Scarcity alerts — positions going critical or thinning
  alerts <- pool %>%
    dplyr::filter(status != "ok", pos %in% c("C","SS","2B","SP")) %>%
    dplyr::mutate(msg = dplyr::case_when(
      status == "critical" ~ paste0("\u26a0\ufe0f ", pos, ": only ", avail, " left"),
      status == "thinning" ~ paste0("\u23f3 ", pos, ": pool thinning (", avail, " left)"),
      TRUE ~ ""
    )) %>%
    dplyr::pull(msg)

  # Best available at each needed position (any tier — used for display only)
  by_pos <- lapply(unique(needed_pos), function(pos) {
    if (pos %in% c("SP","RP")) {
      cands <- available %>% dplyr::filter(grepl(pos, primary_pos))
    } else {
      cands <- available %>% dplyr::filter(grepl(pos, eligible_positions, fixed=TRUE))
    }
    if (nrow(cands) == 0) return(NULL)
    pool_row <- pool %>% dplyr::filter(pool$pos == pos)
    list(pos    = pos,
         player = cands %>% dplyr::slice(1),
         status = if (nrow(pool_row) > 0) pool_row$status[1] else "ok",
         avail  = if (nrow(pool_row) > 0) pool_row$avail[1] else NA)
  })
  by_pos <- Filter(Negate(is.null), by_pos)

  if (length(needed_pos) == 0) {
    return(list(pick=bpa, bpa=bpa,
                reason="Roster complete \u2014 best player available",
                by_pos=by_pos, alerts=alerts))
  }

  # ── Tier-based pick logic ────────────────────────────────────
  # Only consider players in the same tier as BPA.
  # Among them, pick the highest-VOR player who fills a needed position.
  # If none fill a need, fall back to BPA.
  bpa_tier <- dplyr::coalesce(bpa$tier[[1]], 6L)

  same_tier <- available %>%
    dplyr::filter(dplyr::coalesce(tier, 6L) == bpa_tier)

  # For each needed position, find the best same-tier candidate
  best_pick   <- bpa
  best_vor    <- dplyr::coalesce(bpa$vor[[1]], 0)
  best_reason <- "Best player available (Tier \u00a0\u2014 no same-tier positional fit)"
  found_need  <- FALSE

  for (pos in unique(needed_pos)) {
    if (pos %in% c("SP","RP")) {
      cands <- same_tier %>% dplyr::filter(grepl(pos, primary_pos))
    } else {
      cands <- same_tier %>% dplyr::filter(grepl(pos, eligible_positions, fixed=TRUE))
    }
    if (nrow(cands) == 0) next

    top_cand  <- cands %>% dplyr::slice(1)
    cand_vor  <- dplyr::coalesce(top_cand$vor[[1]], 0)
    vor_cost  <- dplyr::coalesce(bpa$vor[[1]], 0) - cand_vor

    # Among same-tier candidates, prefer the one who fills a need AND
    # has the highest VOR (ties broken by position scarcity via pool order)
    if (!found_need || cand_vor > best_vor) {
      pool_row   <- pool %>% dplyr::filter(pool$pos == pos)
      scarcity   <- if (nrow(pool_row) > 0) pool_row$status[1] else "ok"
      suf <- if (scarcity == "critical") " \u26a0\ufe0f pool critical" else
             if (scarcity == "thinning") " \u23f3 pool thinning" else ""
      cost_str  <- if (vor_cost == 0) "same VOR as BPA" else
                   paste0(round(vor_cost), " VOR below BPA")

      best_pick   <- top_cand
      best_vor    <- cand_vor
      best_reason <- paste0("Tier ", bpa_tier, ": fill ", pos, suf,
                            " (", cost_str, ")")
      found_need  <- TRUE
    }
  }

  # If BPA itself fills a need, it was already the best — update reason
  if (identical(best_pick$fg_id, bpa$fg_id)) {
    best_reason <- paste0("Best player available fills needed position")
  }

  list(pick=best_pick, bpa=bpa, reason=best_reason,
       by_pos=by_pos, alerts=alerts)
}

# ── Roster HTML builder ───────────────────────────────────────
build_roster_html <- function(my_players, team_name) {
  slots <- c("C","1B","2B","3B","SS","OF","OF","OF",
             "Util","Util","Util","Util","SP","SP","SP","SP","SP","RP","RP","RP")
  used_ids <- character(0)
  rows_html <- ""

  for (slot in slots) {
    eligible <- if (slot == "Util") {
      my_players %>% dplyr::filter(player_type %in% c("batter","two-way"), !fg_id %in% used_ids)
    } else if (slot %in% c("SP","RP")) {
      my_players %>% dplyr::filter(pos_label(primary_pos) == slot, !fg_id %in% used_ids)
    } else {
      my_players %>% dplyr::filter(grepl(slot, eligible_positions, fixed=TRUE), !fg_id %in% used_ids)
    }
    if (nrow(eligible) > 0) {
      pick <- eligible %>% dplyr::slice(1)
      used_ids <- c(used_ids, pick$fg_id)
      rows_html <- paste0(rows_html, sprintf(
        '<tr style="background:#eafaea; border-bottom:1px solid #ddd;">
          <td style="padding:2px 6px; font-weight:600; color:#2c3e50; width:40px;">%s</td>
          <td style="padding:2px 6px; font-size:11px;">%s</td>
          <td style="padding:2px 6px; text-align:right; font-size:10px; color:#555;">%s</td>
        </tr>', slot, pick$player_name, round(dplyr::coalesce(pick$proj_fpts, 0))
      ))
    } else {
      rows_html <- paste0(rows_html, sprintf(
        '<tr style="background:#fff3f3; border-bottom:1px solid #ddd;">
          <td style="padding:2px 6px; font-weight:600; color:#aaa; width:40px;">%s</td>
          <td style="padding:2px 6px; color:#bbb; font-size:11px; font-style:italic;">— OPEN —</td>
          <td></td>
        </tr>', slot
      ))
    }
  }

  total_pts <- sum(my_players$proj_fpts, na.rm=TRUE)
  player_ct <- nrow(my_players)
  paste0(
    sprintf('<div style="font-size:11px; font-weight:700; color:#1a3a5c; padding:4px 0;">%s (%d players · %s pts)</div>',
            team_name, player_ct, round(total_pts)),
    '<table style="width:100%; border-collapse:collapse; font-size:11px;">',
    rows_html,
    sprintf('<tr style="background:#2c3e50; color:white;">
      <td colspan="2" style="padding:3px 6px; font-weight:bold;">PROJ TOTAL</td>
      <td style="padding:3px 6px; text-align:right; font-weight:bold;">%s</td>
    </tr>', round(total_pts)),
    '</table>'
  )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  title = "Fantasy Draft — Two Team Manager",
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Arial, sans-serif; background:#f0f2f5; font-size:13px; }
    .header-bar {
      background:linear-gradient(135deg,#1a3a5c,#2980b9); color:white;
      padding:10px 18px; border-radius:6px; margin-bottom:10px;
    }
    .panel { background:white; border-radius:6px; padding:10px;
             box-shadow:0 1px 4px rgba(0,0,0,.1); margin-bottom:8px; }
    .rec-box { background:#eaf4fb; border:2px solid #1a73e8; border-radius:6px; padding:8px; }
    .rec-box-t1 { background:#f0fff4; border:2px solid #27ae60; border-radius:6px; padding:8px; }
    .stat-tag {
      display:inline-block; background:#e8f0fe; color:#1a73e8;
      border-radius:3px; padding:1px 5px; font-size:11px; margin:1px;
    }
    .pick-big { font-size:24px; font-weight:800; color:#1a3a5c; text-align:center; }
    .round-txt { text-align:center; color:#888; font-size:11px; }
    .btn-team1 { background:#27ae60 !important; color:white !important; border:none !important; }
    .btn-team2 { background:#1a73e8 !important; color:white !important; border:none !important; }
    .btn-opp   { background:#95a5a6 !important; color:white !important; border:none !important; }
    .db1 { background:#27ae60; color:white; border:none; border-radius:3px;
           padding:1px 5px; font-size:10px; cursor:pointer; font-weight:600; }
    .db2 { background:#1a73e8; color:white; border:none; border-radius:3px;
           padding:1px 5px; font-size:10px; cursor:pointer; font-weight:600; }
    .dbo { background:#95a5a6; color:white; border:none; border-radius:3px;
           padding:1px 5px; font-size:10px; cursor:pointer; }
    .db1:hover { background:#1e8449; } .db2:hover { background:#1558b0; }
    .dbo:hover { background:#717d7e; }
    .selected-player-box {
      background:#fffde7; border:2px solid #f39c12; border-radius:6px;
      padding:8px; min-height:52px;
    }
    .selected-hint { color:#aaa; font-style:italic; font-size:12px; padding:4px; }
    table.dataTable tbody tr.selected { background-color:#fff9c4 !important; }
    table.dataTable tbody tr:hover { cursor:pointer; }
  "))),

  tags$div(class="header-bar",
    tags$h3(style="margin:0; font-size:20px;", "\u26be Fantasy Draft \u2014 Two Team Manager"),
    tags$div(style="font-size:11px; opacity:.85;",
      "H2H Points \u00b7 10 Teams \u00b7 Click a row to select \u00b7 Team 1 (green) + Team 2 (blue)")
  ),

  fluidRow(
    # ── Left column ───────────────────────────────────────────
    column(3,

      # Pick counter
      tags$div(class="panel",
        tags$div(class="pick-big", textOutput("pick_counter", inline=TRUE)),
        tags$div(class="round-txt", textOutput("round_info", inline=TRUE))
      ),

      # Recommendations
      tags$div(class="panel",
        tags$div(style="font-weight:700; color:#1a3a5c; margin-bottom:6px;", "\U0001f3af Recommendations"),
        tags$div(style="margin-bottom:8px;",
          tags$div(style="font-size:11px; font-weight:600; color:#27ae60;", "TEAM 1"),
          tags$div(class="rec-box-t1", uiOutput("rec_team1"))
        ),
        tags$div(
          tags$div(style="font-size:11px; font-weight:600; color:#1a73e8;", "TEAM 2"),
          tags$div(class="rec-box", uiOutput("rec_team2"))
        )
      ),

      # Selected player + draft buttons
      tags$div(class="panel",
        tags$div(style="font-weight:700; color:#1a3a5c; margin-bottom:4px;", "Selected Player"),
        tags$div(class="selected-player-box", uiOutput("selected_player_ui")),
        tags$div(style="display:flex; gap:6px; margin-top:6px;",
          actionButton("draft_t1", "\u2192 Team 1", class="btn-team1 btn-sm", style="flex:1;"),
          actionButton("draft_t2", "\u2192 Team 2", class="btn-team2 btn-sm", style="flex:1;")
        ),
        tags$div(style="margin-top:4px;",
          actionButton("draft_opp", "Opponent Drafted", class="btn-opp btn-sm",
                       style="width:100%;")
        ),
        tags$div(style="margin-top:6px; display:flex; gap:6px;",
          actionButton("undo_btn", "\u21a9 Undo", class="btn-warning btn-sm", style="flex:1;"),
          actionButton("reset_btn", "\u21ba Reset", class="btn-danger btn-sm", style="flex:1;",
                       onclick="if(!confirm('Reset entire draft?')) event.stopPropagation();")
        )
      ),

      # Rosters
      tags$div(class="panel",
        htmlOutput("roster_t1"),
        tags$hr(style="margin:8px 0;"),
        htmlOutput("roster_t2")
      )
    ),

    # ── Right column: big board ────────────────────────────
    column(9,
      tags$div(class="panel",
        fluidRow(
          column(2, selectInput("filter_pos", "Position:",
                                c("All","C","1B","2B","3B","SS","OF","Util","SP","RP"),
                                selected="All")),
          column(2, selectInput("filter_type", "Type:",
                                c("All","Batters","Pitchers","Two-Way"), selected="All")),
          column(2, selectInput("filter_status", "Status:",
                                c("Available","All","My Teams","Drafted"),
                                selected="Available")),
          column(2, selectInput("filter_team_mlb", "MLB Team:",
                                c("All"), selected="All")),
          column(4, tags$div(style="padding-top:24px;",
            tags$span(style="font-size:11px; color:#888;",
              textOutput("board_summary", inline=TRUE))
          ))
        )
      ),
      tags$p(style="font-size:11px; color:#888; margin:2px 0 4px 4px;",
             "Click T1/T2/Opp to draft instantly \u00b7 or click a row then use buttons on left"),
      DTOutput("board_table"),

      tags$div(class="panel", style="margin-top:8px;",
        tags$div(style="font-weight:700; color:#1a3a5c; margin-bottom:4px;", "\U0001f4dd Draft Log"),
        DTOutput("draft_log_table")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ── State ─────────────────────────────────────────────────
  board      <- reactiveVal(BIG_BOARD_ORIG)
  draft_log  <- reactiveVal(dplyr::tibble(
    pick=integer(), round=integer(), team=character(),
    player_name=character(), pos=character(),
    proj_fpts=numeric(), vor=numeric(), adp=numeric()
  ))
  undo_stack <- reactiveVal(list())

  # ── MLB teams dropdown ────────────────────────────────────
  observe({
    teams <- sort(unique(na.omit(board()$team_abbr)))
    updateSelectInput(session, "filter_team_mlb",
                      choices=c("All", teams), selected="All")
  })

  # ── Per-team rosters ──────────────────────────────────────
  t1_players <- reactive({ board() %>% dplyr::filter(drafted_by == MY_TEAM_1) })
  t2_players <- reactive({ board() %>% dplyr::filter(drafted_by == MY_TEAM_2) })

  # ── Pick counter ──────────────────────────────────────────
  pick_num <- reactive({ nrow(draft_log()) + 1 })

  output$pick_counter <- renderText({ paste0("Pick #", pick_num()) })
  output$round_info   <- renderText({
    pn  <- pick_num()
    rnd <- ceiling(pn / LEAGUE_TEAMS)
    pk  <- ((pn-1) %% LEAGUE_TEAMS) + 1
    paste0("Round ", rnd, " \u00b7 Pick ", pk, " of ", LEAGUE_TEAMS,
           " \u00b7 ", sum(board()$status=="Available"), " left")
  })
  output$board_summary <- renderText({
    b <- board()
    paste0(sum(b$status=="Available"), " available \u00b7 ",
           sum(b$drafted_by==MY_TEAM_1, na.rm=TRUE), " on T1 \u00b7 ",
           sum(b$drafted_by==MY_TEAM_2, na.rm=TRUE), " on T2")
  })

  # ── Filtered board for table ──────────────────────────────
  filtered_board <- reactive({
    b <- board()
    if (input$filter_status == "Available")  b <- b %>% dplyr::filter(status == "Available")
    if (input$filter_status == "My Teams")   b <- b %>% dplyr::filter(status %in% MY_TEAMS)
    if (input$filter_status == "Drafted")    b <- b %>% dplyr::filter(status == "Drafted")
    if (input$filter_pos != "All")
      b <- b %>% dplyr::filter(grepl(input$filter_pos, eligible_positions, fixed = TRUE))
    if (input$filter_type == "Batters")  b <- b %>% dplyr::filter(player_type %in% c("batter","two-way"))
    if (input$filter_type == "Pitchers") b <- b %>% dplyr::filter(player_type == "pitcher")
    if (input$filter_type == "Two-Way")  b <- b %>% dplyr::filter(player_type == "two-way")
    if (input$filter_team_mlb != "All")
      b <- b %>% dplyr::filter(team_abbr == input$filter_team_mlb)
    b
  })

  # ── Selected player from row click ───────────────────────
  selected_player <- reactive({
    sel <- input$board_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    fb <- filtered_board()
    if (sel > nrow(fb)) return(NULL)
    fb[sel, ]
  })

  output$selected_player_ui <- renderUI({
    p <- selected_player()
    if (is.null(p)) {
      tags$div(class="selected-hint", "\u2190 Click a player in the board to select them")
    } else {
      adp_v <- safe_val(p$adp)
      adp_str <- if (isTRUE(!is.na(adp_v))) paste0("  ADP: ", adp_v) else ""
      tags$div(
        tags$div(style="font-size:14px; font-weight:700;", p$player_name),
        tags$div(style="font-size:11px; color:#555;",
                 pos_label(p$primary_pos), "\u00b7",
                 dplyr::coalesce(p$team_abbr, "?"),
                 "\u00b7 Rank #", p$overall_rank, adp_str),
        tags$div(style="margin-top:3px;",
          tags$span(class="stat-tag", paste("Pts:", round(dplyr::coalesce(p$proj_fpts, 0)))),
          tags$span(class="stat-tag", paste("VOR:", round(dplyr::coalesce(p$vor, 0))))
        )
      )
    }
  })

  # ── Recommendations ───────────────────────────────────────
  rec1 <- reactive({ recommend_pick(board(), t1_players()) })
  rec2 <- reactive({ recommend_pick(board(), t2_players()) })

  player_card <- function(p, label = NULL) {
    if (is.null(p) || nrow(p) == 0) return(NULL)
    pos   <- pos_label(p$primary_pos)
    fpts  <- round(dplyr::coalesce(p$proj_fpts, 0))
    vor_v <- round(dplyr::coalesce(p$vor, 0), 1)
    adp_v <- safe_val(p$adp)
    adp_txt <- if (isTRUE(!is.na(adp_v))) paste0(" ADP:", adp_v) else ""
    tags$div(
      if (!is.null(label)) tags$div(style="font-size:10px; font-weight:600; color:#888; margin-bottom:2px;", label),
      tags$div(style="font-size:13px; font-weight:700;", p$player_name),
      tags$div(style="font-size:11px; color:#555;",
               pos, "\u00b7", dplyr::coalesce(p$team_abbr, "?"),
               "\u00b7 #", p$overall_rank, adp_txt),
      tags$div(style="margin:3px 0;",
        tags$span(class="stat-tag", paste("Pts:", fpts)),
        tags$span(class="stat-tag", paste("VOR:", vor_v)),
        if (isTRUE(!is.na(safe_val(p$proj_hr))))  tags$span(class="stat-tag", paste("HR:", safe_val(p$proj_hr)))  else NULL,
        if (isTRUE(!is.na(safe_val(p$proj_sb))))  tags$span(class="stat-tag", paste("SB:", safe_val(p$proj_sb)))  else NULL,
        if (isTRUE(!is.na(safe_val(p$proj_era)))) tags$span(class="stat-tag", paste("ERA:", safe_val(p$proj_era))) else NULL,
        if (isTRUE(!is.na(safe_val(p$proj_k))))   tags$span(class="stat-tag", paste("K:", safe_val(p$proj_k)))    else NULL
      )
    )
  }

  rec_ui <- function(rec, quick_btn_id, btn_class) {
    if (is.null(rec)) return(tags$em("No players available"))
    pick <- rec$pick
    bpa  <- rec$bpa

    # Scarcity alerts (if any)
    alert_ui <- if (length(rec$alerts) > 0) {
      tags$div(style="font-size:10px; color:#c0392b; margin-bottom:4px;",
               paste(rec$alerts, collapse="  "))
    } else NULL

    # Reason tag
    reason_ui <- if (!is.null(rec$reason) && rec$reason != "Best player available") {
      tags$div(style="font-size:10px; color:#7d3c98; font-weight:600; margin-bottom:3px;",
               paste0("\u27a4 ", rec$reason))
    } else NULL

    # Recommended pick card
    pick_card <- player_card(pick, label = "RECOMMENDED")

    # BPA card (only if different from pick)
    bpa_card <- if (isTRUE(nrow(bpa) > 0) && isTRUE(nrow(pick) > 0) &&
                    isTRUE(bpa$fg_id[[1]] != pick$fg_id[[1]])) {
      tags$div(style="margin-top:5px; padding-top:5px; border-top:1px dashed #ccc;",
               player_card(bpa, label = "BPA"))
    } else NULL

    # Best by position (compact list)
    by_pos_ui <- if (length(rec$by_pos) > 0) {
      pos_items <- lapply(rec$by_pos, function(pb) {
        p <- pb$player
        if (is.null(p) || nrow(p) == 0) return(NULL)
        scarcity_icon <- dplyr::case_when(
          pb$status == "critical" ~ "\u26a0\ufe0f",
          pb$status == "thinning" ~ "\u23f3",
          TRUE ~ ""
        )
        tags$div(style="font-size:10px; color:#555; padding:1px 0;",
          tags$b(paste0(pb$pos, ":")), scarcity_icon, p$player_name,
          tags$span(style="color:#888;", paste0(" (VOR:", round(dplyr::coalesce(p$vor,0)), ")"))
        )
      })
      tags$div(style="margin-top:5px; padding-top:5px; border-top:1px dashed #ccc;",
        tags$div(style="font-size:10px; font-weight:600; color:#888; margin-bottom:2px;",
                 "BEST BY POSITION"),
        do.call(tags$div, Filter(Negate(is.null), pos_items))
      )
    } else NULL

    tags$div(
      alert_ui,
      reason_ui,
      pick_card,
      bpa_card,
      by_pos_ui,
      actionButton(quick_btn_id, "\u2713 Draft Recommended",
                   class=paste(btn_class, "btn-sm"), style="width:100%; margin-top:6px;")
    )
  }

  output$rec_team1 <- renderUI({ rec_ui(rec1(), "quick_draft_t1", "btn-team1") })
  output$rec_team2 <- renderUI({ rec_ui(rec2(), "quick_draft_t2", "btn-team2") })

  # ── Draft helpers ─────────────────────────────────────────
  save_undo <- function() {
    stk <- undo_stack()
    stk <- c(stk, list(list(board=board(), log=draft_log())))
    if (length(stk) > 50) stk <- stk[-1]
    undo_stack(stk)
  }

  do_draft <- function(fg_id_val, team_name) {
    b      <- board()
    player <- b %>% dplyr::filter(fg_id == fg_id_val)
    if (nrow(player) == 0 || player$status[1] != "Available") return()
    save_undo()
    new_status <- if (team_name %in% MY_TEAMS) team_name else "Drafted"
    # If this player has a split twin (same name, different type), remove it too
    twin_ids <- b %>%
      dplyr::filter(player_name == player$player_name[1],
                    fg_id != fg_id_val,
                    status == "Available") %>%
      dplyr::pull(fg_id)
    mark_ids <- c(fg_id_val, twin_ids)
    b <- b %>%
      dplyr::mutate(
        status       = dplyr::if_else(fg_id %in% mark_ids, new_status, status),
        drafted_by   = dplyr::if_else(fg_id %in% mark_ids, team_name, drafted_by),
        drafted_pick = dplyr::if_else(fg_id %in% mark_ids, as.integer(pick_num()), drafted_pick)
      )
    board(b)
    draft_log(dplyr::bind_rows(draft_log(), dplyr::tibble(
      pick        = pick_num(),
      round       = ceiling(pick_num() / LEAGUE_TEAMS),
      team        = team_name,
      player_name = player$player_name[1],
      pos         = pos_label(player$primary_pos[1]),
      proj_fpts   = dplyr::coalesce(player$proj_fpts[1], 0),
      vor         = dplyr::coalesce(player$vor[1], 0),
      adp         = dplyr::coalesce(safe_val(player$adp), NA_real_)
    )))
  }

  # Draft via inline table buttons (fastest path — one click)
  observeEvent(input$draft_inline, {
    req(input$draft_inline$id)
    team <- switch(input$draft_inline$t,
      t1  = MY_TEAM_1,
      t2  = MY_TEAM_2,
      opp = "Opponent",
      "Opponent"
    )
    do_draft(input$draft_inline$id, team)
  })

  # Draft via row selection + sidebar buttons (fallback)
  observeEvent(input$draft_t1, {
    p <- selected_player(); req(!is.null(p)); do_draft(p$fg_id, MY_TEAM_1)
  })
  observeEvent(input$draft_t2, {
    p <- selected_player(); req(!is.null(p)); do_draft(p$fg_id, MY_TEAM_2)
  })
  observeEvent(input$draft_opp, {
    p <- selected_player(); req(!is.null(p)); do_draft(p$fg_id, "Opponent")
  })

  # Draft via recommendation quick buttons
  observeEvent(input$quick_draft_t1, {
    r <- rec1(); req(!is.null(r)); do_draft(r$pick$fg_id, MY_TEAM_1)
  })
  observeEvent(input$quick_draft_t2, {
    r <- rec2(); req(!is.null(r)); do_draft(r$pick$fg_id, MY_TEAM_2)
  })

  # Undo
  observeEvent(input$undo_btn, {
    stk <- undo_stack()
    if (length(stk) == 0) return()
    last <- stk[[length(stk)]]
    board(last$board); draft_log(last$log)
    undo_stack(stk[-length(stk)])
  })

  # Reset
  observeEvent(input$reset_btn, {
    save_undo()
    board(BIG_BOARD_ORIG)
    draft_log(dplyr::tibble(
      pick=integer(), round=integer(), team=character(),
      player_name=character(), pos=character(),
      proj_fpts=numeric(), vor=numeric(), adp=numeric()
    ))
  })

  # ── Roster displays ───────────────────────────────────────
  output$roster_t1 <- renderUI({ HTML(build_roster_html(t1_players(), MY_TEAM_1)) })
  output$roster_t2 <- renderUI({ HTML(build_roster_html(t2_players(), MY_TEAM_2)) })

  # ── Big board table ───────────────────────────────────────
  output$board_table <- renderDT({
    b <- filtered_board()

    # Inline draft buttons — one click drafts directly from the row
    draft_btns <- paste0(
      '<div style="white-space:nowrap">',
      '<button class="db1" onclick="Shiny.setInputValue(\'draft_inline\',',
        '{id:\'', b$fg_id, '\',t:\'t1\'},{priority:\'event\'})">T1</button> ',
      '<button class="db2" onclick="Shiny.setInputValue(\'draft_inline\',',
        '{id:\'', b$fg_id, '\',t:\'t2\'},{priority:\'event\'})">T2</button> ',
      '<button class="dbo" onclick="Shiny.setInputValue(\'draft_inline\',',
        '{id:\'', b$fg_id, '\',t:\'opp\'},{priority:\'event\'})">Opp</button>',
      '</div>'
    )

    display <- dplyr::tibble(
      Draft  = draft_btns,
      `#`    = b$overall_rank,
      `P#`   = b$pos_rank,
      Player = b$player_name,
      Pos    = dplyr::coalesce(b$eligible_positions, pos_label(b$primary_pos)),
      Team   = dplyr::coalesce(b$team_abbr, "?"),
      Pts    = b$proj_fpts,
      VOR    = round(b$vor),
      Tier   = dplyr::coalesce(b$tier, 6L),
      ADP    = round(dplyr::coalesce(b$adp, NA_real_), 1),
      Val    = dplyr::coalesce(b$value_vs_adp, NA_real_),
      PA     = dplyr::coalesce(b$proj_pa,  NA_integer_),
      HR     = dplyr::coalesce(b$proj_hr,  NA_integer_),
      R      = dplyr::coalesce(b$proj_r,   NA_integer_),
      RBI    = dplyr::coalesce(b$proj_rbi, NA_integer_),
      SB     = dplyr::coalesce(b$proj_sb,  NA_integer_),
      AVG    = dplyr::coalesce(b$proj_avg, NA_real_),
      `wRC+` = round(dplyr::coalesce(b$proj_wrc_plus, NA_real_)),
      `Brl%` = dplyr::coalesce(b$sc_brl_percent, NA_real_),
      IP     = dplyr::coalesce(b$proj_ip,  NA_real_),
      W      = dplyr::coalesce(b$proj_w,   NA_integer_),
      SV     = dplyr::coalesce(b$proj_sv,  NA_integer_),
      K      = dplyr::coalesce(b$proj_k,   NA_integer_),
      ERA    = dplyr::coalesce(b$proj_era, NA_real_),
      QS     = dplyr::coalesce(b$proj_qs,  NA_real_),
      Status = b$status,
      By     = dplyr::coalesce(b$drafted_by, "")
    )

    datatable(
      display, rownames=FALSE, escape=FALSE, selection="single",
      options=list(
        pageLength=30, scrollX=TRUE,
        order=list(list(7,"desc")),   # sort by VOR col (index 7 after Draft col)
        dom="ftip",
        columnDefs=list(
          list(width="75px",  targets=0),   # Draft buttons
          list(width="120px", targets=3),   # Player name
          list(width="34px",  targets=c(1,2,4,5))
        )
      ),
      class="cell-border stripe compact hover"
    ) %>%
      formatStyle("Status",
        backgroundColor = styleEqual(
          c("Available", MY_TEAM_1, MY_TEAM_2, "Drafted", "Opponent"),
          c("white", "#d5f5e3", "#dbeafe", "#fdecea", "#fdecea")
        )
      ) %>%
      formatStyle("Val",
        color = styleInterval(c(-1, 0), c("#e74c3c", "#888", "#27ae60")),
        fontWeight = "bold"
      ) %>%
      formatStyle("VOR",
        background         = styleColorBar(range(display$VOR, na.rm=TRUE), "#cce5ff"),
        backgroundSize     = "98% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("Tier",
        backgroundColor = styleEqual(
          1:6,
          c("#ffd700", "#c8e6fa", "#c8f5d8", "#ffe0b2", "#eeeeee", "#f5f5f5")
        ),
        color = styleEqual(1:6, c("#7b5800","#1a3a5c","#1a5c2a","#7b3800","#555","#aaa")),
        fontWeight = "bold", textAlign = "center"
      ) %>%
      formatRound("AVG",  digits=3) %>%
      formatRound("ERA",  digits=2) %>%
      formatRound("IP",   digits=1) %>%
      formatRound("ADP",  digits=1) %>%
      formatRound("Brl%", digits=1)
  })

  # ── Draft log ─────────────────────────────────────────────
  output$draft_log_table <- renderDT({
    datatable(
      draft_log() %>% dplyr::arrange(dplyr::desc(pick)),
      rownames=FALSE,
      options=list(pageLength=8, dom="ftp", order=list(list(0,"desc"))),
      class="compact stripe"
    ) %>%
      formatStyle("team",
        backgroundColor = styleEqual(c(MY_TEAM_1, MY_TEAM_2), c("#d5f5e3","#dbeafe"))
      ) %>%
      formatRound("vor", digits=1)
  })
}

shinyApp(ui, server)
