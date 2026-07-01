library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(DT)
library(htmltools)
library(plotly)
library(shinyjs)


source("R/helpers.R")

# --- UI ---
ui <- page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bg = "#14110F",
    fg = "#F3F3F4",
    primary = "#D9C5B2",
    secondary = "#202020"
  ),
  sidebar = sidebar(
    bg = "#202020",
    h3("World Cup 2026 Dashboard", style = "color: #D9C5B2; font-weight: bold; margin-bottom: 15px; line-height: 1.2; text-align: center;"),
    p("An interactive dashboard to visualize, analyze, and compare FIFA World Cup 2026 predictions against real tournament results. Features live bracket logic, interactive match maps, and an in-app data editor.", 
      style = "font-size: 13px; color: #D9C5B2; line-height: 1.45; margin-bottom: 20px; text-align: center; opacity: 0.85;"),
    hr(style = "border-top: 1px solid #7E7F83;"),
    h5("Prediction Compare", style = "color: #D9C5B2; font-weight: bold; margin-bottom: 15px;"),
    p("Upload a prediction CSV to compare against the real results.", style = "font-size: 13px; color: #A0A0A0;"),
    fileInput("user_prediction", "Load Prediction (.csv):", accept = c(".csv"), buttonLabel = "Browse...")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  navset_card_underline(
    title = "",
    nav_panel("Group Stage Dashboard", 
      div(class = "dashboard-grid",
          div(class = "widget-map", 
              leafletOutput("map", height = "100%")
          ),
          div(class = "stats-accuracy-container",
              uiOutput("stats_ui", style = "display: contents;"),
              div(class = "widget-accuracy", 
                  div(class="stat-title", "Accuracy"),
                  div(class="accuracy-score", textOutput("stat_accuracy", inline=TRUE)),
                  div(class="accuracy-sub", "Correct (W/D/L)")
              )
          ),
          div(class = "widget-radar", 
              selectInput("radar_team", NULL, choices = NULL, width = "100%", selectize = TRUE),
              div(style = "flex-grow: 1; min-height: 0;",
                  plotlyOutput("radar_plot", height = "100%")
              )
          ),
          div(class = "widget-scatter",
              div(style = "flex-grow: 1; min-height: 0;",
                  plotlyOutput("scatter_plot", height = "100%")
              )
          ),
          div(class = "widget-matches", 
              h5("Matches by Stage", style = "margin-top:0; font-weight:bold; color:#D9C5B2; font-size:14px;"),
              uiOutput("matches_ui")
          ),
          div(class = "widget-middle-stats",
              uiOutput("middle_stats_ui", style = "display: contents;")
          ),
          div(class = "widget-timeline",
              plotlyOutput("timeline_plot", height = "100%")
          ),
          div(class = "widget-table",
              h5("Group Table", style = "margin-top:0; margin-bottom:5px; font-weight:bold; color:#D9C5B2; font-size:14px;"),
              selectInput("group_filter", NULL, choices = sort(unique(paste("Group", teams$group_letter))), width = "100%"),
              div(style = "flex-grow: 1; overflow-y: auto;",
                  DTOutput("group_table_ui")
              )
          ),
          div(class = "widget-scorers",
              plotlyOutput("top_scorers_plot", height = "100%")
          ),
          div(class = "widget-goaldiff",
              plotlyOutput("goal_diff_plot", height = "100%")
          )
      )
    ),
    nav_panel("Calendar",
        uiOutput("calendar_ui")
    ),
    nav_panel("Knockout Bracket",
        uiOutput("bracket_ui")
    ),
    nav_panel("Data Editor",
        div(style = "padding: 20px;",
            h4("Edit Predictions and Actual Results", style = "color: #D9C5B2;"),
            p("Changes made here will update the dashboard and save to the respective CSV file.", style="color: #A0A0A0;"),
            fluidRow(
                column(3, selectInput("editor_dataset", "Select Dataset to Edit:", choices = c("Actual Results", "Predictions"))),
                column(3, actionButton("save_editor", "Save Changes to File", class="btn btn-primary", style="margin-top: 32px; background-color: #16549b; color: #F3F3F4; border: none; font-weight: bold;")),
                column(3, actionButton("sync_api", "Sync Missing Results", icon = icon("sync"), class="btn btn-info", style="margin-top: 32px; color: white; border: none; font-weight: bold;"))
            ),
            navset_underline(
                nav_panel("Matches", DTOutput("editor_table")),
                nav_panel("Penalties", DTOutput("penalty_table"))
            )
        )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  tc <- reactive({
    if (isTRUE(input$theme_toggle)) {
      list(accent = "#0F62F2", line = "#ACCAF1", text = "#14110F", subtext = "#4A5568")
    } else {
      list(accent = "#0F79F2", accent_light = "#749FD2", accent_dark = "#16549b", line = "#7E7F83", text = "#D9C5B2", subtext = "#A0A0A0")
    }
  })
  
  # Reactive Data Storage
  # Load initial data and optionally update from API if matches are missing
  initial_real_data <- parse_predictions("predictions/resultados_reales.csv")
  initial_real_data <- update_results_from_api(initial_real_data, "predictions/resultados_reales.csv")
  
  rv <- reactiveValues(
    real_data = initial_real_data,
    ml_preds = parse_predictions("predictions/prediccion_ml.csv"),
    real_penalties = parse_penalties("predictions/penalties_reales.csv"),
    ml_penalties = parse_penalties("predictions/penalties_ml.csv")
  )
  
  # Sync knockout team names from real data to ML predictions
  rv$ml_preds$Team1[rv$ml_preds$MatchID > 72] <- rv$real_data$Team1[rv$real_data$MatchID > 72]
  rv$ml_preds$Team2[rv$ml_preds$MatchID > 72] <- rv$real_data$Team2[rv$real_data$MatchID > 72]
  
  
  standings <- reactive({ calculate_standings(rv$real_data) })
  ml_standings <- reactive({ calculate_standings(rv$ml_preds) })
  
  final_real_data <- reactive({ rv$real_data })
  final_ml_preds <- reactive({ rv$ml_preds })
  
  top_scorer <- reactive({ standings() %>% filter(GF == max(GF)) %>% pull(Team) })
  least_conceded <- reactive({ standings() %>% filter(GA == min(GA)) %>% pull(Team) })
  most_wins <- reactive({ standings() %>% filter(W == max(W)) %>% pull(Team) })
  
  observe({
    d <- final_real_data()
    teams <- sort(unique(c(d$Team1, d$Team2)))
    teams <- teams[teams %in% unname(english_to_spanish)]
    updateSelectInput(session, "radar_team", choices = teams)
  })
  
  output$stats_ui <- renderUI({
    make_stat_box <- function(title, teams) {
      val <- as.character(format_stat(teams))
      class_name <- if (length(teams) > 1) "stat-wide" else "stat-square"
      len <- max(nchar(val), 1)
      cqi_val <- min(150 / len, 32)
      fs <- sprintf("clamp(12px, %fcqi, 48px)", cqi_val)
      div(class = class_name,
          div(class="stat-title", title),
          div(class="stat-value", style=paste0("font-size: ", fs, ";"), val)
      )
    }
    
    tagList(
      make_stat_box("Top Scorer", top_scorer()),
      make_stat_box("Least Conceded", least_conceded()),
      make_stat_box("Most Wins", most_wins())
    )
  })
  
  output$middle_stats_ui <- renderUI({
    d_real <- final_real_data() %>% filter(!is.na(Goals1) & !is.na(Goals2))
    
    # 1. Average Goals per Match
    avg_goals <- if(nrow(d_real) > 0) round(mean(d_real$Goals1 + d_real$Goals2), 2) else 0
    
    # 2. Most Common Scoreline
    if(nrow(d_real) > 0) {
      scorelines <- paste(pmax(d_real$Goals1, d_real$Goals2), pmin(d_real$Goals1, d_real$Goals2), sep="-")
      most_common <- names(sort(table(scorelines), decreasing=TRUE))[1]
    } else {
      most_common <- "-"
    }
    
    # 3. Biggest Blowout
    if(nrow(d_real) > 0) {
      d_real$margin <- abs(d_real$Goals1 - d_real$Goals2)
      max_margin_idx <- which.max(d_real$margin)
      blowout_match <- d_real[max_margin_idx, ]
      biggest_blowout <- paste0(blowout_match$Team1_Abrev, " ", blowout_match$Goals1, "-", blowout_match$Goals2, " ", blowout_match$Team2_Abrev)
    } else {
      biggest_blowout <- "-"
    }
    
    # 4. Average Goal Error
    preds <- current_preds()
    avg_err <- "-"
    if(!is.null(preds) && nrow(d_real) > 0) {
      j <- d_real %>% left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred")) %>%
             filter(!is.na(Goals1_pred) & !is.na(Goals2_pred))
      if(nrow(j) > 0) {
        err <- mean(abs(j$Goals1_real - j$Goals1_pred) + abs(j$Goals2_real - j$Goals2_pred))
        avg_err <- round(err, 2)
      }
    }
    
    make_middle_box <- function(title, val) {
      val <- as.character(val)
      len <- max(nchar(val), 1)
      cqi_val <- min(150 / len, 32)
      fs <- sprintf("clamp(12px, %fcqi, 48px)", cqi_val)
      div(class="stat-box-middle", 
          div(class="stat-title", title), 
          div(class="stat-value", style=paste0("font-size: ", fs, ";"), val)
      )
    }
    
    tagList(
      make_middle_box("Avg Goals/Match", avg_goals),
      make_middle_box("Common Score", most_common),
      make_middle_box("Biggest Blowout", biggest_blowout),
      make_middle_box("Avg Goal Error", avg_err)
    )
  })
  
  output$group_table_ui <- renderDT({
    d <- standings() %>% 
      filter(Group == input$group_filter) %>%
      mutate(`GF:GA` = paste0(GF, ":", GA)) %>%
      select(Team, P, W, D, L, `GF:GA`, GD, Pts)
      
    datatable(d, options = list(dom = 't', paging = FALSE, scrollX = TRUE), rownames = FALSE, style = "bootstrap") %>%
      formatStyle(columns = names(d), color = '#F3F3F4', backgroundColor = '#202020')
  })
  
  output$top_scorers_plot <- renderPlotly({
    cols <- tc()
    st <- standings() %>% arrange(desc(GF)) %>% head(10) %>% mutate(Team_Abrev = sapply(Team, spanish_to_fifa))
    
    # Dynamic blue color gradient based on goals (GF)
    accent_light <- if (!is.null(cols$accent_light)) cols$accent_light else "#ACCAF1"
    accent_dark <- if (!is.null(cols$accent_dark)) cols$accent_dark else "#0F62F2"
    
    min_gf <- min(st$GF)
    max_gf <- max(st$GF)
    
    if (max_gf == min_gf) {
      st$bar_color <- accent_dark
    } else {
      color_func <- colorRampPalette(c(accent_light, accent_dark))
      # Map GF range to a 100-level color gradient
      norm_val <- round(((st$GF - min_gf) / (max_gf - min_gf)) * 99) + 1
      st$bar_color <- color_func(100)[norm_val]
    }
    
    plot_ly(st, x = ~GF, y = ~reorder(Team_Abrev, GF), type = 'bar', orientation = 'h',
            marker = list(color = ~bar_color)) %>%
      layout(
        title = list(text = "Top Scorers (GF)", font = list(color = cols$text, size = 12)),
        xaxis = list(title = "", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE, tickfont = list(size=9)),
        yaxis = list(title = "", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE, tickfont = list(size=10)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l=40, r=10, t=30, b=20)
      )
  })

  output$goal_diff_plot <- renderPlotly({
    cols <- tc()
    d <- standings() %>% filter(Group == input$group_filter) %>% mutate(Team_Abrev = sapply(Team, spanish_to_fifa))
    d$GA_neg <- -d$GA
    
    plot_ly(d, y = ~reorder(Team_Abrev, GD)) %>%
      add_trace(x = ~GF, name = 'GF', type = 'bar', orientation = 'h',
                marker = list(color = cols$accent_dark)) %>%
      add_trace(x = ~GA_neg, name = 'GA', type = 'bar', orientation = 'h',
                marker = list(color = cols$line)) %>%
      layout(
        title = list(text = paste("Goal Diff (GF vs GA)"), font = list(color = cols$text, size = 12)),
        barmode = 'relative',
        xaxis = list(title = "", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zerolinecolor = cols$line, tickfont = list(size=9)),
        yaxis = list(title = "", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE, tickfont = list(size=10)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l=40, r=10, t=30, b=20),
        showlegend = FALSE
      )
  })
  
  current_preds <- reactive({
    if (is.null(input$user_prediction)) {
      rv$ml_preds
    } else {
      parse_predictions(input$user_prediction$datapath)
    }
  })
  
  observeEvent(input$theme_toggle, {
    if(input$theme_toggle) {
      shinyjs::addClass(selector = "body", class = "light-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "light-mode")
    }
  })
  
  output$timeline_plot <- renderPlotly({
    preds <- current_preds()
    
    # Calculate goals over time using the joined real_data
    joined <- final_real_data() %>% 
      left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    
    joined$TotalReal <- rowSums(joined[, c("Goals1_real", "Goals2_real")], na.rm = TRUE)
    joined$TotalPred <- rowSums(joined[, c("Goals1_pred", "Goals2_pred")], na.rm = TRUE)
    
    # Make sure stage factors are properly ordered for time-series flow
    stage_levels <- c("Groups first match", "Groups second match", "Groups third match", 
                      "Round of 32", "Round of 16", "Quarterfinals", "Semifinals", "Final")
    joined$MatchDay_Label_real <- factor(joined$MatchDay_Label_real, levels = stage_levels)
    levels(joined$MatchDay_Label_real) <- c("GRd 1", "GRd 2", "GRd 3", "R32", "R16", "QF", "SF", "Final")
    
    trend <- joined %>%
      group_by(MatchDay_Label_real) %>%
      summarise(
        ActualGoals = sum(TotalReal, na.rm=TRUE),
        PredGoals = sum(TotalPred, na.rm=TRUE),
        .groups='drop'
      ) %>%
      filter(!is.na(MatchDay_Label_real))
    
    cols <- tc()
    p <- plot_ly(trend, x = ~MatchDay_Label_real) %>%
      add_trace(y = ~ActualGoals, name = 'Actual', type = 'scatter', mode = 'lines+markers',
                line = list(color = cols$accent_light, width = 2), marker = list(color = cols$accent_light, size = 6)) %>%
      add_trace(y = ~PredGoals, name = 'Predicted', type = 'scatter', mode = 'lines+markers',
                line = list(color = cols$accent_dark, width = 2, dash = 'dot'), marker = list(color = cols$accent_dark, size = 6)) %>%
      layout(
        title = list(text = "Total Goals by Tournament Stage", font = list(color = cols$text, size = 12)),
        xaxis = list(title = "", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE, 
                     tickangle = -45, tickfont = list(size=9)),
        yaxis = list(title = "Goals Scored", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE,
                     tickfont = list(size=10)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = cols$text, size=10),
        margin = list(l=30, r=10, t=30, b=50),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.05, y = 0.5, font = list(color = cols$text))
      )
      
    # Use shinyjs to conditionally style the plotly if light mode is active? 
    # Actually, Plotly's transparent bg allows the CSS container to shine through!
    # But text colors are hardcoded. We'll leave them beige for now or the user can just enjoy the contrast.
    
    p
  })
  
  output$stat_accuracy <- renderText({
    preds <- current_preds()
    acc <- calculate_accuracy(final_real_data(), preds)
    paste0(acc, "%")
  })
  
  output$radar_plot <- renderPlotly({
    cols <- tc()
    req(input$radar_team)
    team <- input$radar_team
    preds <- current_preds()
    if(is.null(preds)) preds <- final_real_data()[0,]
    
    d <- get_radar_data(team, final_real_data(), preds)
    d$real[is.na(d$real)] <- 0
    d$pred[is.na(d$pred)] <- 0
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = d$real,
        theta = d$categories,
        name = 'Actual',
        fillcolor = 'rgba(116, 159, 210, 0.4)',
        line = list(color = cols$accent_light, width = 2)
      ) %>%
      add_trace(
        r = d$pred,
        theta = d$categories,
        name = 'Predicted',
        fillcolor = 'rgba(22, 84, 155, 0.4)',
        line = list(color = cols$accent_dark, width = 2)
      ) %>%
      layout(
        polar = list(
          bgcolor = '#202020',
          radialaxis = list(visible = TRUE, range = c(0, max(c(d$real, d$pred, 3))), gridcolor=cols$line, linecolor=cols$line, tickfont=list(color=cols$text)),
          angularaxis = list(tickfont = list(color = cols$text), gridcolor=cols$line, linecolor=cols$line)
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = cols$text, size=10),
        showlegend = FALSE,
        margin = list(l=20, r=20, t=20, b=20)
      )
  })
  
  output$scatter_plot <- renderPlotly({
    cols <- tc()
    
    # Group teams by identical GF and GA values to prevent overlapping dots
    grouped_data <- standings() %>%
      mutate(Team_Abrev = sapply(Team, spanish_to_fifa)) %>%
      group_by(GF, GA) %>%
      summarise(
        Teams_List = paste(Team_Abrev, collapse = ", "),
        Teams_Count = n(),
        .groups = 'drop'
      ) %>%
      mutate(
        # Improved hover visuals showing all teams in the group
        Hover_Text = paste0(
          "<b>GF:</b> ", GF, " | <b>GA:</b> ", GA, "<br>",
          "<b>Teams (", Teams_Count, "):</b><br>",
          sapply(Teams_List, function(x) {
            teams <- strsplit(x, ", ")[[1]]
            paste(paste0("• ", teams), collapse = "<br>")
          })
        ),
        # Label: show exact list if <= 2 teams, otherwise show first two with (+N)
        Label = sapply(Teams_List, function(x) {
          teams <- strsplit(x, ", ")[[1]]
          if (length(teams) <= 2) {
            paste(teams, collapse = ", ")
          } else {
            paste0(paste(teams[1:2], collapse = ", "), " (+", length(teams) - 2, ")")
          }
        })
      )
    
    # Calculate colors based on team count (gradient color instead of scaling size)
    accent_light <- if (!is.null(cols$accent_light)) cols$accent_light else "#ACCAF1"
    accent_dark <- if (!is.null(cols$accent_dark)) cols$accent_dark else "#0F62F2"
    
    max_count <- max(grouped_data$Teams_Count)
    if (max_count == 1) {
      grouped_data$color <- accent_light
    } else {
      color_func <- colorRampPalette(c(accent_light, accent_dark))
      # Map Teams_Count from 1 to max_count to 1-100 color levels
      norm_val <- round(((grouped_data$Teams_Count - 1) / (max_count - 1)) * 99) + 1
      grouped_data$color <- color_func(100)[norm_val]
    }
    
    plot_ly(
      data = grouped_data, 
      x = ~GF, 
      y = ~GA, 
      text = ~Label,
      hovertext = ~Hover_Text,
      hoverinfo = 'text',
      type = 'scatter', 
      mode = 'markers+text',
      textposition = 'top center',
      # Constant size, but color gets darker with team count
      marker = list(
        color = ~color, 
        size = 8,
        line = list(color = cols$line, width = 1)
      )
    ) %>%
      layout(
        title = list(text = "Goals For vs Goals Against", font = list(color = cols$text, size = 11)),
        xaxis = list(title = "Goals Made (GF)", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE, dtick = 1),
        yaxis = list(title = "Goals Received (GA)", color = cols$text, showgrid = FALSE, showline = TRUE, linecolor = cols$line, zeroline = FALSE, dtick = 1),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = cols$text, size=10),
        margin = list(l=40, r=20, t=30, b=40)
      )
  })
  
  output$matches_ui <- renderUI({
    preds <- current_preds()
    if(is.null(preds)) return(div("Error loading predictions.", style="color:red;"))
    
    joined <- final_real_data() %>% 
      left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    
    match_divs <- lapply(unique(joined$MatchDay_Label_real), function(g) {
      g_matches <- joined %>% filter(MatchDay_Label_real == g)
      
      rows <- lapply(1:nrow(g_matches), function(i) {
        row <- g_matches[i, ]
        
        # Format scores beautifully (handling empty NA matches)
        goals1_real_txt <- if(is.na(row$Goals1_real)) "-" else as.character(row$Goals1_real)
        goals2_real_txt <- if(is.na(row$Goals2_real)) "-" else as.character(row$Goals2_real)
        goals1_pred_txt <- if(is.na(row$Goals1_pred)) "" else paste0("(", row$Goals1_pred, ")")
        goals2_pred_txt <- if(is.na(row$Goals2_pred)) "" else paste0("(", row$Goals2_pred, ")")
        
        # Format kickoff date/time and venue using _real suffix
        kickoff_info <- if (!is.null(row$Kickoff_Local_real) && !is.na(row$Kickoff_Local_real) && row$Kickoff_Local_real != "") {
          paste0(row$Kickoff_Local_real, " ", row$Kickoff_TZ_real)
        } else {
          ""
        }
        
        venue_info <- if (!is.null(row$city_name_real) && !is.na(row$city_name_real)) {
          row$city_name_real
        } else {
          ""
        }
        
        div(style = "padding: 6px 0; border-bottom: 1px solid #4E4F53;",
            div(class = "match-row", style = "border-bottom: none; padding: 0; align-items: center;",
                span(row$Team1_Abrev_real, style = "width: 40px; text-align: left; font-weight: bold;"),
                span(
                  span(class="actual-score", goals1_real_txt),
                  span(class="pred-score", goals1_pred_txt),
                  " - ",
                  span(class="pred-score", goals2_pred_txt),
                  span(class="actual-score", goals2_real_txt)
                ),
                span(row$Team2_Abrev_real, style = "width: 40px; text-align: right; font-weight: bold;")
            ),
            if (kickoff_info != "" || venue_info != "") {
              div(style = "display: flex; justify-content: space-between; font-size: 10px; color: #A0A0A0; margin-top: 2px;",
                  span(kickoff_info),
                  span(venue_info, title = if(!is.null(row$venue_name_real) && !is.na(row$venue_name_real)) row$venue_name_real else "", style = "text-decoration: underline dotted; cursor: help;")
              )
            } else {
              NULL
            }
        )
      })
      
      div(
        div(class = "group-header", g),
        do.call(tagList, rows)
      )
    })
    do.call(tagList, match_divs)
  })
  
  output$map <- renderLeaflet({
    preds <- current_preds()
    
    if(is.null(preds)) {
      joined <- final_real_data() %>% 
        rename(
          Team1_Abrev_real = Team1_Abrev,
          Team2_Abrev_real = Team2_Abrev,
          city_id_real = city_id,
          MatchDay_Label_real = MatchDay_Label
        ) %>%
        mutate(
          Goals1_real = Goals1,
          Goals2_real = Goals2,
          Goals1_pred = NA_real_,
          Goals2_pred = NA_real_,
          Kickoff_Local_real = Kickoff_Local,
          Kickoff_TZ_real = Kickoff_TZ,
          Group_real = Group
        )
    } else {
      joined <- final_real_data() %>% 
        left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    }
    joined <- joined %>% filter(!is.na(city_id_real))
    
    map_data <- host_cities %>%
      rowwise() %>%
      mutate(
        popup_text = {
          city_matches <- joined %>% filter(city_id_real == id)
          
          # Header HTML
          header_html <- paste0(
            "<div class='city-popup-header'>",
            "  <div class='city-popup-title'>",
            "    <span>", city_name, "</span>",
            "    <span class='city-popup-country'>", country, " (", airport_code, ")</span>",
            "  </div>",
            "  <div class='city-popup-venue'>",
            "    <span class='city-popup-venue-icon'>🏟️</span>", venue_name,
            "  </div>",
            "</div>"
          )
          
          # Matches HTML
          if (nrow(city_matches) > 0) {
            match_rows <- sapply(seq_len(nrow(city_matches)), function(i) {
              m <- city_matches[i, ]
              
              stage_lbl <- if (!is.null(m$Group_real)) as.character(m$Group_real) else as.character(m$Group)
              if (length(stage_lbl) == 0 || is.na(stage_lbl) || stage_lbl == "") {
                stage_lbl <- as.character(m$MatchDay_Label_real)
              }
              
              stage_lbl <- str_replace(stage_lbl, "Stage", "")
              stage_lbl <- str_replace(stage_lbl, "Groups first match", "Group")
              stage_lbl <- str_replace(stage_lbl, "Groups second match", "Group")
              stage_lbl <- str_replace(stage_lbl, "Groups third match", "Group")
              
              score_html <- ""
              if (!is.na(m$Goals1_real) && !is.na(m$Goals2_real)) {
                score_html <- paste0(
                  "<span class='city-popup-match-score city-popup-score-real'>",
                  m$Goals1_real, " - ", m$Goals2_real,
                  "</span>"
                )
              } else if (!is.na(m$Goals1_pred) && !is.na(m$Goals2_pred)) {
                score_html <- paste0(
                  "<span class='city-popup-match-score city-popup-score-pred' title='Predicted Score'>",
                  "P: ", m$Goals1_pred, " - ", m$Goals2_pred,
                  "</span>"
                )
              } else {
                score_html <- "<span class='city-popup-match-score city-popup-score-tbd'>TBD</span>"
              }
              
              kickoff_str <- ""
              if (!is.null(m$Kickoff_Local_real) && !is.na(m$Kickoff_Local_real) && m$Kickoff_Local_real != "") {
                dt <- tryCatch({
                  as.POSIXct(m$Kickoff_Local_real, format = "%Y-%m-%d %H:%M:%S")
                }, error = function(e) NA)
                
                formatted_dt <- if (!is.na(dt)) {
                  format(dt, "%b %d, %H:%M")
                } else {
                  m$Kickoff_Local_real
                }
                
                tz <- if (!is.null(m$Kickoff_TZ_real) && !is.na(m$Kickoff_TZ_real)) paste0(" (", m$Kickoff_TZ_real, ")") else ""
                kickoff_str <- paste0("<span class='city-popup-match-time'>📅 ", formatted_dt, tz, "</span>")
              }
              
              paste0(
                "<div class='city-popup-match-row'>",
                "  <span class='city-popup-match-stage'>", stage_lbl, "</span>",
                "  <div class='city-popup-match-teams'>",
                "    <span>", m$Team1_Abrev_real, " vs ", m$Team2_Abrev_real, "</span>",
                "    ", kickoff_str,
                "  </div>",
                "  ", score_html,
                "</div>"
              )
            })
            
            matches_html <- paste0(
              "<div class='city-popup-matches-title'>Matches</div>",
              paste(match_rows, collapse = "")
            )
          } else {
            matches_html <- "<div class='city-popup-no-matches'>No matches scheduled</div>"
          }
          
          paste0(
            "<div style='min-width: 220px;'>",
            header_html,
            matches_html,
            "</div>"
          )
        }
      )
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 3) %>%
      addCircleMarkers(
        ~lon, ~lat, 
        popup = ~popup_text,
        radius = 8,
        color = "#D9C5B2",
        stroke = FALSE, fillOpacity = 0.8
      )
  })
  
  output$bracket_ui <- renderUI({
    generate_bracket_ui(matches)
  })
  
  # --- Data Editor Logic ---
  
  # Enable sync button only if there are pending past matches
  observe({
    d <- rv$real_data
    current_utc <- as.POSIXct(Sys.time(), tz = "UTC")
    
    needs_sync <- d %>%
      filter(is.na(Goals1) & !is.na(Kickoff_UTC) & Kickoff_UTC != "") %>%
      mutate(
        match_end_time = as.POSIXct(Kickoff_UTC, tz = "UTC") + 7200
      ) %>%
      filter(match_end_time < current_utc)
    
    if (nrow(needs_sync) > 0) {
      shinyjs::enable("sync_api")
    } else {
      shinyjs::disable("sync_api")
    }
  })
  
  observeEvent(input$sync_api, {
    showNotification("Syncing with OpenFootball API...", type = "message")
    updated_data <- update_results_from_api(rv$real_data, "predictions/resultados_reales.csv")
    
    old_played <- sum(!is.na(rv$real_data$Goals1))
    new_played <- sum(!is.na(updated_data$Goals1))
    
    if (new_played > old_played) {
      rv$real_data <- updated_data
      # Also update ML predictions team names!
      rv$ml_preds$Team1[rv$ml_preds$MatchID > 72] <- updated_data$Team1[updated_data$MatchID > 72]
      rv$ml_preds$Team2[rv$ml_preds$MatchID > 72] <- updated_data$Team2[updated_data$MatchID > 72]
      save_predictions(rv$ml_preds, "predictions/prediccion_ml.csv")
      showNotification(sprintf("Successfully imported %d new match results!", new_played - old_played), type = "message", duration = 5)
    } else {
      showNotification("No new matches to sync.", type = "warning", duration = 3)
    }
  })
    editor_data <- reactive({
      if(input$editor_dataset == "Actual Results") {
        final_real_data()
      } else {
        final_ml_preds()
      }
    })
  
  output$editor_table <- renderDT({
    # Re-render only when the selected dataset changes, not when cell values are edited in place
    input$editor_dataset
    
    d <- isolate({
      editor_data() %>%
        mutate(
          Kickoff = if_else(is.na(Kickoff_Local) | Kickoff_Local == "", "", paste0(Kickoff_Local, " (", Kickoff_TZ, ")")),
          Kickoff_UTC = if_else(is.na(Kickoff_UTC), "", Kickoff_UTC)
        ) %>%
        select(MatchID, Kickoff, Group, Team1, Goals1, Goals2, Team2, Kickoff_UTC)
    })
    datatable(d, 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 6, 7))),
              options = list(
                paging = FALSE, 
                scrollX = TRUE, 
                scrollY = "600px", 
                dom = 't',
                columnDefs = list(
                  list(visible = FALSE, targets = 7),
                  list(orderData = 7, targets = 1)
                )
              ),
              rownames = FALSE,
              style = "bootstrap"
    ) %>%
      formatStyle(columns = names(d), color = '#F3F3F4', backgroundColor = '#202020')
  })
  
  observeEvent(input$editor_table_cell_edit, {
    info <- input$editor_table_cell_edit
    i <- info$row
    j <- info$col + 1 # JS is 0-indexed, R is 1-indexed
    v <- info$value
    
    d <- isolate(editor_data())
    # Columns in d_display: MatchID (1), Kickoff (2), Group (3), Team1 (4), Goals1 (5), Goals2 (6), Team2 (7), Kickoff_UTC (8)
    if (j == 5) {
      d$Goals1[i] <- as.numeric(v)
    } else if (j == 6) {
      d$Goals2[i] <- as.numeric(v)
    }
    
    if (input$editor_dataset == "Actual Results") {
      rv$real_data <- d
    } else {
      rv$ml_preds <- d
    }
    
    # Update DT in-place using a proxy to maintain scroll position and selection state
    proxy <- dataTableProxy("editor_table")
    d_display <- d %>%
      mutate(
        Kickoff = if_else(is.na(Kickoff_Local) | Kickoff_Local == "", "", paste0(Kickoff_Local, " (", Kickoff_TZ, ")")),
        Kickoff_UTC = if_else(is.na(Kickoff_UTC), "", Kickoff_UTC)
      ) %>%
      select(MatchID, Kickoff, Group, Team1, Goals1, Goals2, Team2, Kickoff_UTC)
    replaceData(proxy, d_display, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$save_editor, {
    dataset <- if (input$editor_dataset == "Actual Results") final_real_data() else final_ml_preds()
    pens <- if (input$editor_dataset == "Actual Results") rv$real_penalties else rv$ml_penalties
    
    tied_matches <- dataset %>% 
      filter(stage_id > 1 & !is.na(Goals1) & !is.na(Goals2) & Goals1 == Goals2) %>%
      select(MatchID, Team1, Team2)
      
    if (nrow(tied_matches) > 0) {
      new_pens <- tied_matches %>%
        filter(!(MatchID %in% pens$MatchID)) %>%
        mutate(Pen1 = NA_real_, Pen2 = NA_real_)
      
      if (nrow(new_pens) > 0) {
        pens <- bind_rows(pens, new_pens) %>% arrange(MatchID)
      }
    }
    
    if (input$editor_dataset == "Actual Results") {
      save_predictions(final_real_data(), "predictions/resultados_reales.csv")
      rv$real_penalties <- pens
      save_penalties(pens, "predictions/penalties_reales.csv")
      showNotification("Actual Results and Penalties saved to file!", type = "message")
    } else {
      save_predictions(final_ml_preds(), "predictions/prediccion_ml.csv")
      rv$ml_penalties <- pens
      save_penalties(pens, "predictions/penalties_ml.csv")
      showNotification("ML Predictions and Penalties saved to file!", type = "message")
    }
  })
  
  output$penalty_table <- renderDT({
    pens <- if (input$editor_dataset == "Actual Results") rv$real_penalties else rv$ml_penalties
    fd <- if (input$editor_dataset == "Actual Results") final_real_data() else final_ml_preds()
    
    p_disp <- pens %>%
      select(MatchID, Pen1, Pen2) %>%
      left_join(fd %>% select(MatchID, Team1, Team2), by = "MatchID") %>%
      select(MatchID, Team1, Pen1, Pen2, Team2)
      
    datatable(p_disp, 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 4))),
              options = list(paging = FALSE, scrollX = TRUE, dom = 't'),
              rownames = FALSE,
              style = "bootstrap") %>%
      formatStyle(columns = names(p_disp), color = '#F3F3F4', backgroundColor = '#202020')
  })
  
  observeEvent(input$penalty_table_cell_edit, {
    info <- input$penalty_table_cell_edit
    i <- info$row
    j <- info$col + 1
    v <- as.numeric(info$value)
    
    if (input$editor_dataset == "Actual Results") {
      d <- rv$real_penalties
      if (j == 3) {
        d$Pen1[i] <- v
      } else if (j == 4) {
        d$Pen2[i] <- v
      }
      rv$real_penalties <- d
    } else {
      d <- rv$ml_penalties
      if (j == 3) {
        d$Pen1[i] <- v
      } else if (j == 4) {
        d$Pen2[i] <- v
      }
      rv$ml_penalties <- d
    }
  })
  
  output$calendar_ui <- renderUI({
    d_real <- final_real_data()
    preds <- current_preds()
    if(!is.null(preds)) {
      p_subset <- preds %>% select(Team1, Team2, Goals1_pred = Goals1, Goals2_pred = Goals2)
      d <- d_real %>% left_join(p_subset, by = c("Team1", "Team2")) %>%
           rename(Goals1_real = Goals1, Goals2_real = Goals2)
    } else {
      d <- d_real %>% mutate(Goals1_real = Goals1, Goals2_real = Goals2, Goals1_pred = NA, Goals2_pred = NA)
    }
    
    # Extract date from kickoff_at safely
    d$MatchDate <- as.Date(substring(d$kickoff_at, 1, 10))
    
    # 7 weeks: June 1 (Mon) to July 19 (Sun)
    all_dates <- seq(as.Date("2026-06-01"), as.Date("2026-07-19"), by="day")
    
    daily_stages <- sapply(all_dates, function(dt) {
      m_today <- d %>% filter(MatchDate == dt)
      if (nrow(m_today) > 0) max(m_today$stage_id, na.rm=TRUE) else 0
    })
    
    active_stage_per_day <- numeric(length(daily_stages))
    current_stage <- 1
    for (i in seq_along(daily_stages)) {
      if (daily_stages[i] > current_stage) {
        current_stage <- daily_stages[i]
      }
      active_stage_per_day[i] <- current_stage
    }
    
    day_divs <- lapply(seq_along(all_dates), function(idx) {
      dt <- all_dates[idx]
      matches_today <- d %>% filter(MatchDate == dt) %>% arrange(kickoff_at)
      
      if (nrow(matches_today) > 0) {
        match_divs <- lapply(seq_len(nrow(matches_today)), function(i) {
          m <- matches_today[i, ]
          
          is_assigned <- (m$Team1 %in% unname(english_to_spanish)) || (m$Team2 %in% unname(english_to_spanish))
          
          if (is_assigned) {
            teams_str <- paste(m$Team1_Abrev, "-", m$Team2_Abrev)
          } else {
            teams_str <- as.character(m$MatchDay_Label)
          }
          
          has_real <- !is.na(m$Goals1_real) && !is.na(m$Goals2_real)
          if(has_real) {
            score_str <- paste(m$Goals1_real, "-", m$Goals2_real)
            div(class="calendar-match real-result",
                div(class="match-teams", teams_str),
                div(class="match-score", score_str)
            )
          } else {
            has_pred <- !is.na(m$Goals1_pred) && !is.na(m$Goals2_pred)
            if(has_pred) {
              score_str <- paste("P:", m$Goals1_pred, "-", m$Goals2_pred)
            } else {
              score_str <- "TBD"
            }
            div(class="calendar-match",
                div(class="match-teams", teams_str),
                div(class="match-score", score_str)
            )
          }
        })
      } else {
        match_divs <- list()
      }
      
      month_label <- if (as.numeric(format(dt, "%d")) == 1 || dt == as.Date("2026-06-01")) {
        div(class="calendar-month-label", format(dt, "%b"))
      } else NULL
      
      max_stage <- active_stage_per_day[idx]
      bg_col <- ""
      if (max_stage == 2) bg_col <- "background-color: #121c2e; "
      else if (max_stage == 3) bg_col <- "background-color: #142742; "
      else if (max_stage == 4) bg_col <- "background-color: #153357; "
      else if (max_stage == 5 || max_stage == 6) bg_col <- "background-color: #16406b; "
      else if (max_stage == 7) bg_col <- "background-color: #174e80; "
      
      div(class="calendar-day", style=bg_col,
          month_label,
          div(class="calendar-date", as.numeric(format(dt, "%d"))),
          do.call(tagList, match_divs)
      )
    })
    
    div(class="calendar-wrapper",
        div(class="calendar-header",
            div("Mon"), div("Tue"), div("Wed"), div("Thu"), div("Fri"), div("Sat"), div("Sun")
        ),
        div(class="calendar-grid",
            do.call(tagList, day_divs)
        )
    )
  })
}

shinyApp(ui, server)
