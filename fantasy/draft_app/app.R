# ============================================================
# FANTASY BASEBALL — Draft Day Shiny App
# ============================================================
# USAGE:
#   shiny::runApp("fantasy/draft_app")
#   — or run via fantasy/run_fantasy_pipeline.R
#
# FEATURES:
#   - Big board ranked by VOR — filter by position/name/team
#   - One-click draft: assign player to your team or opponents
#   - Roster tracker: shows open slots and warns on positional need
#   - Auto-recommend next pick based on VOR + roster need
#   - Undo last pick
#   - Draft log with round/pick tracking
# ============================================================

library(shiny)
library(dplyr)
library(DT)

# ── Load big board ───────────────────────────────────────────
board_path <- "data/big_board.rds"
if (!file.exists(board_path)) {
  # Try relative from project root
  board_path <- "fantasy/draft_app/data/big_board.rds"
}
if (!file.exists(board_path)) stop("big_board.rds not found. Run fantasy/run_fantasy_pipeline.R first.")

BIG_BOARD_ORIG <- readRDS(board_path) %>%
  dplyr::mutate(
    status       = "Available",  # Available | My Team | Drafted
    drafted_pick = NA_integer_,
    drafted_by   = NA_character_
  )

# ── League config ────────────────────────────────────────────
LEAGUE_TEAMS <- 10
MY_TEAM_NAME <- "My Team"

ROSTER_REQS <- c(
  C = 1, `1B` = 1, `2B` = 1, `3B` = 1, SS = 1,
  OF = 3, SP = 5, RP = 3
)
UTIL_SLOTS <- 4  # any hitter

# All teams for the picker
all_teams <- c(MY_TEAM_NAME, paste("Team", 2:LEAGUE_TEAMS))

# ── Position display label ───────────────────────────────────
pos_label <- function(p) {
  dplyr::case_when(
    p == "C"         ~ "C",
    p == "1B"        ~ "1B",
    p == "2B"        ~ "2B",
    p == "3B"        ~ "3B",
    p == "SS"        ~ "SS",
    p == "OF"        ~ "OF",
    p == "SP"        ~ "SP",
    p %in% c("RP", "RP_closer") ~ "RP",
    TRUE             ~ p
  )
}

# ── Roster slot fill checker ─────────────────────────────────
check_roster <- function(my_players) {
  if (nrow(my_players) == 0) {
    needs <- names(ROSTER_REQS)
    return(list(filled = character(0), needed = needs, util_used = 0L))
  }
  pos_counts <- table(pos_label(my_players$primary_pos))
  filled <- character(0)
  needed <- character(0)
  for (slot in names(ROSTER_REQS)) {
    have <- as.integer(pos_counts[slot] %||% 0L)
    need <- ROSTER_REQS[slot]
    if (have >= need) filled <- c(filled, slot)
    else              needed <- c(needed, rep(slot, need - have))
  }
  # Util: any hitter
  hitters_on_team <- nrow(my_players %>% dplyr::filter(player_type == "batter"))
  util_used <- max(0, hitters_on_team - sum(ROSTER_REQS[c("C","1B","2B","3B","SS","OF")]))
  list(filled = filled, needed = needed, util_used = min(util_used, UTIL_SLOTS))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Recommendation engine ─────────────────────────────────────
# Suggest best available player considering positional need
recommend_pick <- function(board, my_players) {
  available <- board %>% dplyr::filter(status == "Available")
  if (nrow(available) == 0) return(NULL)

  roster_info <- check_roster(my_players)
  needed_pos  <- unique(roster_info$needed)

  if (length(needed_pos) == 0) {
    # Roster filled — just take best VOR available
    return(available %>% dplyr::slice(1))
  }

  # Score players by VOR + positional need bonus
  pos_short <- names(sort(table(roster_info$needed), decreasing = TRUE)[1:min(3, length(roster_info$needed))])

  available %>%
    dplyr::mutate(
      pos_need_bonus = dplyr::case_when(
        pos_label(primary_pos) %in% pos_short ~ 50,
        TRUE ~ 0
      ),
      score = vor + pos_need_bonus
    ) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice(1)
}

# ── Roster slot display ───────────────────────────────────────
build_roster_html <- function(my_players) {
  slots <- c("C", "1B", "2B", "3B", "SS", "OF", "OF", "OF",
             "Util", "Util", "Util", "Util", "SP", "SP", "SP", "SP", "SP", "RP", "RP", "RP")
  html <- '<table style="width:100%; font-size:13px; border-collapse:collapse;">'
  html <- paste0(html, '<tr style="background:#2c3e50; color:white;">',
                 '<th style="padding:4px 8px; text-align:left;">Slot</th>',
                 '<th style="padding:4px 8px; text-align:left;">Player</th>',
                 '<th style="padding:4px 8px; text-align:right;">Pts</th></tr>')

  # Fill slots greedily
  used_ids   <- character(0)
  slot_fills <- list()

  for (slot in slots) {
    eligible <- if (slot == "Util") {
      my_players %>% dplyr::filter(player_type == "batter", !fg_id %in% used_ids)
    } else if (slot %in% c("SP","RP")) {
      my_players %>% dplyr::filter(pos_label(primary_pos) == slot, !fg_id %in% used_ids)
    } else {
      my_players %>% dplyr::filter(grepl(slot, eligible_positions, fixed = TRUE), !fg_id %in% used_ids)
    }

    if (nrow(eligible) > 0) {
      pick <- eligible %>% dplyr::slice(1)
      used_ids <- c(used_ids, pick$fg_id)
      slot_fills[[length(slot_fills)+1]] <- list(
        slot   = slot,
        player = pick$player_name,
        pts    = pick$proj_fpts,
        filled = TRUE
      )
    } else {
      slot_fills[[length(slot_fills)+1]] <- list(
        slot   = slot,
        player = "— OPEN —",
        pts    = NA,
        filled = FALSE
      )
    }
  }

  for (i in seq_along(slot_fills)) {
    sf  <- slot_fills[[i]]
    bg  <- if (sf$filled) "#eafaea" else "#fff3f3"
    pts <- if (!is.na(sf$pts)) sf$pts else ""
    html <- paste0(html,
      sprintf('<tr style="background:%s; border-bottom:1px solid #dee2e6;">
        <td style="padding:3px 8px; font-weight:600; color:#2c3e50;">%s</td>
        <td style="padding:3px 8px;">%s</td>
        <td style="padding:3px 8px; text-align:right; color:#555;">%s</td>
      </tr>', bg, sf$slot, sf$player, pts)
    )
  }

  total_pts <- sum(my_players$proj_fpts, na.rm = TRUE)
  html <- paste0(html,
    sprintf('<tr style="background:#2c3e50; color:white; font-weight:bold;">
      <td colspan="2" style="padding:5px 8px;">PROJECTED TOTAL</td>
      <td style="padding:5px 8px; text-align:right;">%s</td>
    </tr>', round(total_pts))
  )

  paste0(html, "</table>")
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  title = "Fantasy Draft Board",

  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Arial, sans-serif; background: #f8f9fa; }
    .section-header {
      background: #2c3e50; color: white; padding: 8px 14px;
      border-radius: 4px; margin-bottom: 8px; font-weight: 600; font-size: 15px;
    }
    .recommend-box {
      background: #eaf4fb; border: 2px solid #1a73e8; border-radius: 6px;
      padding: 10px 14px; margin-bottom: 10px;
    }
    .stat-badge {
      display: inline-block; background: #e8f0fe; color: #1a73e8;
      border-radius: 3px; padding: 1px 6px; font-size: 12px; margin: 1px;
    }
    .pick-counter {
      font-size: 22px; font-weight: 700; color: #2c3e50; text-align: center;
    }
    .filter-panel { background: white; padding: 10px; border-radius: 6px;
                    margin-bottom: 8px; box-shadow: 0 1px 3px rgba(0,0,0,.08); }
    .roster-panel { background: white; padding: 10px; border-radius: 6px;
                    box-shadow: 0 1px 3px rgba(0,0,0,.08); }
    #board_table { font-size: 13px; }
    .dataTables_wrapper { font-size: 13px; }
  "))),

  # ── Header ────────────────────────────────────────────────
  fluidRow(
    column(12,
      tags$div(style = "background:#1a3a5c; color:white; padding:12px 20px; border-radius:6px; margin-bottom:12px;",
        tags$h3(style = "margin:0; font-size:22px;", "⚾ Fantasy Draft Board"),
        tags$span(style = "font-size:13px; opacity:0.8;",
          "H2H Points League · 10 Teams · Powered by Steamer + Statcast")
      )
    )
  ),

  fluidRow(
    # ── Left: roster + recommendation ─────────────────────
    column(3,

      # Pick counter
      tags$div(class = "filter-panel",
        tags$div(class = "pick-counter", textOutput("pick_counter", inline = TRUE)),
        tags$div(style = "text-align:center; color:#888; font-size:12px;",
                 textOutput("round_display", inline = TRUE))
      ),

      # Recommendation
      tags$div(class = "recommend-box",
        tags$div(class = "section-header", style = "margin-bottom:6px;",
                 "🎯 Recommended Pick"),
        uiOutput("recommendation")
      ),

      # Draft controls
      tags$div(class = "filter-panel",
        tags$div(class = "section-header", "Draft a Player"),
        selectizeInput("draft_player", label = NULL,
                       choices = NULL, selected = NULL,
                       options = list(placeholder = "Search player name...")),
        selectInput("draft_for_team", label = "Draft for:",
                    choices = all_teams, selected = MY_TEAM_NAME),
        fluidRow(
          column(6, actionButton("draft_btn", "Draft", class = "btn-primary btn-block",
                                 style = "background:#1a73e8; border:none;")),
          column(6, actionButton("undo_btn", "↩ Undo", class = "btn-warning btn-block"))
        )
      ),

      # My roster
      tags$div(class = "roster-panel",
        tags$div(class = "section-header", "📋 My Roster"),
        htmlOutput("my_roster_html")
      )
    ),

    # ── Right: big board ───────────────────────────────────
    column(9,

      tags$div(class = "filter-panel",
        fluidRow(
          column(3, selectInput("filter_pos", "Position:",
                                choices = c("All", "C","1B","2B","3B","SS","OF","SP","RP"),
                                selected = "All")),
          column(3, selectInput("filter_type", "Type:",
                                choices = c("All", "Batters", "Pitchers"),
                                selected = "All")),
          column(3, selectInput("filter_status", "Status:",
                                choices = c("Available", "All", "My Team", "Drafted"),
                                selected = "Available")),
          column(3, selectInput("filter_team", "MLB Team:",
                                choices = c("All"), selected = "All"))
        )
      ),

      DTOutput("board_table"),

      # Draft log
      tags$div(style = "margin-top:12px;",
        tags$div(class = "section-header", "📝 Draft Log"),
        DTOutput("draft_log_table")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # ── Reactive state ─────────────────────────────────────────
  board    <- reactiveVal(BIG_BOARD_ORIG)
  draft_log <- reactiveVal(dplyr::tibble(
    pick = integer(), round = integer(), team = character(),
    player_name = character(), pos = character(),
    proj_fpts = numeric(), vor = numeric()
  ))
  undo_stack <- reactiveVal(list())

  # Populate team selector with all MLB teams
  observe({
    teams <- sort(unique(na.omit(board()$team_abbr)))
    updateSelectInput(session, "filter_team",
                      choices = c("All", teams), selected = "All")
  })

  # Populate player search
  observe({
    avail <- board() %>% dplyr::filter(status == "Available")
    choices <- setNames(avail$fg_id, paste0(avail$player_name, " (", pos_label(avail$primary_pos), " - ", dplyr::coalesce(avail$team_abbr, "?"), ")"))
    updateSelectizeInput(session, "draft_player",
                         choices = choices, server = TRUE)
  })

  # ── Derived: my team players ─────────────────────────────
  my_players <- reactive({
    board() %>% dplyr::filter(status == "My Team")
  })

  # ── Pick counter ──────────────────────────────────────────
  pick_num <- reactive({
    nrow(draft_log()) + 1
  })

  output$pick_counter <- renderText({
    paste0("Pick #", pick_num())
  })

  output$round_display <- renderText({
    pn <- pick_num()
    rnd <- ceiling(pn / LEAGUE_TEAMS)
    pick_in_round <- ((pn - 1) %% LEAGUE_TEAMS) + 1
    paste0("Round ", rnd, " · Pick ", pick_in_round, " of ", LEAGUE_TEAMS)
  })

  # ── Recommendation ────────────────────────────────────────
  output$recommendation <- renderUI({
    rec <- recommend_pick(board(), my_players())
    if (is.null(rec)) return(tags$em("No players available"))

    pos  <- pos_label(rec$primary_pos)
    fpts <- round(rec$proj_fpts)
    vor  <- round(rec$vor, 1)

    tags$div(
      tags$div(style = "font-size:16px; font-weight:700; color:#1a3a5c;",
               rec$player_name),
      tags$div(style = "color:#555; font-size:13px; margin-bottom:4px;",
               pos, "·", dplyr::coalesce(rec$team_abbr, "?"),
               "· Rank #", rec$overall_rank),
      tags$div(
        tags$span(class = "stat-badge", paste("Proj Pts:", fpts)),
        tags$span(class = "stat-badge", paste("VOR:", vor)),
        if (!is.na(rec$proj_hr))  tags$span(class = "stat-badge", paste("HR:", rec$proj_hr))  else NULL,
        if (!is.na(rec$proj_sb))  tags$span(class = "stat-badge", paste("SB:", rec$proj_sb))  else NULL,
        if (!is.na(rec$proj_era)) tags$span(class = "stat-badge", paste("ERA:", rec$proj_era)) else NULL,
        if (!is.na(rec$proj_k))   tags$span(class = "stat-badge", paste("K:", rec$proj_k))    else NULL
      ),
      tags$div(style = "margin-top:6px;",
        actionButton("draft_rec_btn",
                     paste("Draft", rec$player_name),
                     class = "btn-sm",
                     style = "background:#27ae60; color:white; border:none; font-size:12px;",
                     `data-fg-id` = rec$fg_id)
      )
    )
  })

  # ── Draft action: recommended player ─────────────────────
  observeEvent(input$draft_rec_btn, {
    rec <- recommend_pick(board(), my_players())
    if (!is.null(rec)) {
      do_draft(rec$fg_id, MY_TEAM_NAME)
    }
  })

  # ── Draft action: manual ──────────────────────────────────
  observeEvent(input$draft_btn, {
    req(input$draft_player)
    do_draft(input$draft_player, input$draft_for_team)
  })

  do_draft <- function(fg_id_val, team_name) {
    b      <- board()
    player <- b %>% dplyr::filter(fg_id == fg_id_val)
    if (nrow(player) == 0 || player$status[1] != "Available") return()

    # Save undo state
    undo_stack(c(undo_stack(), list(list(board = b, log = draft_log()))))

    # Update board
    new_status <- if (team_name == MY_TEAM_NAME) "My Team" else "Drafted"
    b <- b %>%
      dplyr::mutate(
        status       = dplyr::if_else(fg_id == fg_id_val, new_status,       status),
        drafted_by   = dplyr::if_else(fg_id == fg_id_val, team_name,        drafted_by),
        drafted_pick = dplyr::if_else(fg_id == fg_id_val, as.integer(pick_num()), drafted_pick)
      )
    board(b)

    # Add to log
    log_entry <- dplyr::tibble(
      pick        = pick_num(),
      round       = ceiling(pick_num() / LEAGUE_TEAMS),
      team        = team_name,
      player_name = player$player_name[1],
      pos         = pos_label(player$primary_pos[1]),
      proj_fpts   = player$proj_fpts[1],
      vor         = player$vor[1]
    )
    draft_log(dplyr::bind_rows(draft_log(), log_entry))
  }

  # ── Undo ─────────────────────────────────────────────────
  observeEvent(input$undo_btn, {
    stk <- undo_stack()
    if (length(stk) == 0) return()
    last <- stk[[length(stk)]]
    board(last$board)
    draft_log(last$log)
    undo_stack(stk[-length(stk)])
  })

  # ── Board table ───────────────────────────────────────────
  filtered_board <- reactive({
    b <- board()

    if (input$filter_status != "All") {
      b <- b %>% dplyr::filter(status == input$filter_status)
    }
    if (input$filter_pos != "All") {
      b <- b %>% dplyr::filter(pos_label(primary_pos) == input$filter_pos)
    }
    if (input$filter_type != "All") {
      type_val <- if (input$filter_type == "Batters") "batter" else "pitcher"
      b <- b %>% dplyr::filter(player_type == type_val)
    }
    if (input$filter_team != "All") {
      b <- b %>% dplyr::filter(team_abbr == input$filter_team)
    }

    b
  })

  output$board_table <- renderDT({
    b <- filtered_board()

    # Build display table
    display <- b %>%
      dplyr::transmute(
        `#`    = overall_rank,
        Player = player_name,
        Pos    = pos_label(primary_pos),
        Team   = dplyr::coalesce(team_abbr, "?"),
        `Proj Pts` = proj_fpts,
        VOR    = round(vor, 0),
        # Batter cols
        PA     = dplyr::coalesce(proj_pa,  NA_integer_),
        HR     = dplyr::coalesce(proj_hr,  NA_integer_),
        R      = dplyr::coalesce(proj_r,   NA_integer_),
        RBI    = dplyr::coalesce(proj_rbi, NA_integer_),
        SB     = dplyr::coalesce(proj_sb,  NA_integer_),
        AVG    = dplyr::coalesce(proj_avg, NA_real_),
        `wRC+` = round(dplyr::coalesce(proj_wrc_plus, NA_real_)),
        # Pitcher cols
        IP     = dplyr::coalesce(proj_ip,  NA_real_),
        W      = dplyr::coalesce(proj_w,   NA_integer_),
        SV     = dplyr::coalesce(proj_sv,  NA_integer_),
        K      = dplyr::coalesce(proj_k,   NA_integer_),
        ERA    = dplyr::coalesce(proj_era, NA_real_),
        QS     = dplyr::coalesce(proj_qs,  NA_real_),
        # Status
        Status = status,
        `By`   = dplyr::coalesce(drafted_by, "")
      )

    dt <- datatable(
      display,
      rownames    = FALSE,
      selection   = "none",
      options     = list(
        pageLength  = 25,
        scrollX     = TRUE,
        order       = list(list(4, "desc")),  # sort by Proj Pts
        columnDefs  = list(
          list(width = "140px", targets = 1),   # Player
          list(width = "45px",  targets = c(0, 2, 3))
        ),
        dom         = "ftip"
      ),
      class = "cell-border stripe compact hover"
    ) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          c("Available", "My Team", "Drafted"),
          c("white",     "#d5f5e3", "#fdecea")
        )
      ) %>%
      formatStyle(
        "VOR",
        background = styleColorBar(range(display$VOR, na.rm = TRUE), "#cce5ff"),
        backgroundSize  = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatRound("AVG", digits = 3) %>%
      formatRound("ERA", digits = 2) %>%
      formatRound("IP",  digits = 1)

    dt
  })

  # ── Draft log ─────────────────────────────────────────────
  output$draft_log_table <- renderDT({
    log <- draft_log() %>% dplyr::arrange(dplyr::desc(pick))
    datatable(
      log,
      rownames  = FALSE,
      options   = list(pageLength = 10, dom = "ftp", order = list(list(0, "desc"))),
      class     = "compact stripe"
    ) %>%
      formatStyle(
        "team",
        backgroundColor = styleEqual(MY_TEAM_NAME, "#d5f5e3")
      )
  })

  # ── My roster display ─────────────────────────────────────
  output$my_roster_html <- renderUI({
    HTML(build_roster_html(my_players()))
  })
}

shinyApp(ui, server)
