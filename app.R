library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(DT)
library(htmltools)
library(plotly)

# --- Data Preparation ---
teams <- read.csv("extra_data/teams.csv", stringsAsFactors = FALSE)
matches <- read.csv("extra_data/matches.csv", stringsAsFactors = FALSE)
host_cities <- read.csv("extra_data/host_cities.csv", stringsAsFactors = FALSE)

# Hardcode coordinates for the 16 cities
coords <- data.frame(
  id = 1:16,
  lat = c(33.7554, 42.0909, 32.7473, 29.6847, 39.0489, 33.9534, 25.9580, 40.8128, 
          39.9008, 37.4032, 47.5952, 43.6332, 49.2768, 20.6817, 19.3029, 25.6644),
  lon = c(-84.4010, -71.2643, -97.0945, -95.4107, -94.4839, -118.3387, -80.2389, -74.0742,
          -75.1675, -121.9698, -122.3316, -79.4186, -123.1120, -103.4626, -99.1505, -100.2443)
)
host_cities <- host_cities %>% left_join(coords, by = "id")

english_to_spanish <- c(
  "Mexico" = "México", "South Africa" = "Sudáfrica", "South Korea" = "Corea del Sur",
  "Winner UEFA Playoff D" = "República Checa", "Canada" = "Canadá", 
  "Winner UEFA Playoff A" = "Bosnia y Herzegovina", "Qatar" = "Catar", 
  "Switzerland" = "Suiza", "Brazil" = "Brasil", "Morocco" = "Marruecos",
  "Haiti" = "Haití", "Scotland" = "Escocia", "USA" = "Estados Unidos",
  "Paraguay" = "Paraguay", "Australia" = "Australia", "Winner UEFA Playoff C" = "Turquía", 
  "Germany" = "Alemania", "Curaçao" = "Curazao", "Côte d'Ivoire" = "Costa de Marfil", 
  "Ecuador" = "Ecuador", "Netherlands" = "Países Bajos", "Japan" = "Japón", 
  "Winner UEFA Playoff B" = "Suecia", "Tunisia" = "Túnez", "Belgium" = "Bélgica", 
  "Egypt" = "Egipto", "IR Iran" = "Irán", "New Zealand" = "Nueva Zelanda", 
  "Spain" = "España", "Cabo Verde" = "Cabo Verde", "Saudi Arabia" = "Arabia Saudita", 
  "Uruguay" = "Uruguay", "France" = "Francia", "Senegal" = "Senegal", 
  "Winner FIFA Playoff 2" = "Irak", "Norway" = "Noruega", "Argentina" = "Argentina", 
  "Algeria" = "Argelia", "Austria" = "Austria", "Jordan" = "Jordania", 
  "Portugal" = "Portugal", "Winner FIFA Playoff 1" = "República Democrática del Congo", 
  "Uzbekistan" = "Uzbekistán", "Colombia" = "Colombia", "England" = "Inglaterra", 
  "Croatia" = "Croacia", "Ghana" = "Ghana", "Panama" = "Panamá"
)
spanish_to_english <- setNames(names(english_to_spanish), english_to_spanish)

spanish_to_fifa <- function(spanish_name) {
  eng_name <- spanish_to_english[spanish_name]
  if(is.na(eng_name)) return(spanish_name)
  code <- teams$fifa_code[teams$team_name == eng_name]
  if(length(code) > 0) return(code[1])
  return(substr(spanish_name, 1, 3))
}

parse_predictions <- function(filepath) {
  if(!file.exists(filepath)) return(NULL)
  lines <- readLines(filepath, encoding = "UTF-8")
  predictor_name <- str_replace_all(lines[2], ";", "")
  if (trimws(predictor_name) == "") {
    predictor_name <- tools::file_path_sans_ext(basename(filepath))
  }
  valid_lines <- lines[!str_detect(lines, "^;+$") & !str_detect(lines, "^;.*") & !str_detect(lines, "^Equipo;Resultado;;Equipo")]
  valid_lines <- valid_lines[valid_lines != ""]
  
  data <- read.csv(text = valid_lines, sep = ";", header = FALSE, stringsAsFactors = FALSE)
  colnames(data) <- c("Team1", "Goals1", "Goals2", "Team2")
  
  data <- data %>%
    mutate(
      Goals1 = as.numeric(Goals1),
      Goals2 = as.numeric(Goals2),
      Predictor = predictor_name,
      MatchID = row_number()
    )
  
  # Join with matches to get correct stage_id, match_label and city_id
  stage_names <- c("Group Stage", "Round of 32", "Round of 16", "Quarter-Finals", "Semi-Finals", "Third Place Playoff", "Final")
  data <- data %>%
    left_join(matches, by = c("MatchID" = "id")) %>%
    mutate(
      Group = if_else(stage_id == 1, match_label, stage_names[stage_id])
    )
  
  data$Team1_Abrev <- sapply(data$Team1, spanish_to_fifa)
  data$Team2_Abrev <- sapply(data$Team2, spanish_to_fifa)
  return(data)
}

# Real Results
real_data <- parse_predictions("predictions/resultados_reales.csv")

calculate_standings <- function(d) {
  # Standings are calculated only for played Group Stage matches
  d1 <- d %>% filter(stage_id == 1 & !is.na(Goals1) & !is.na(Goals2)) %>% select(Team = Team1, GF = Goals1, GA = Goals2, Group)
  d2 <- d %>% filter(stage_id == 1 & !is.na(Goals1) & !is.na(Goals2)) %>% select(Team = Team2, GF = Goals2, GA = Goals1, Group)
  bind_rows(d1, d2) %>%
    group_by(Group, Team) %>%
    summarise(
      P = n(),
      W = sum(GF > GA),
      D = sum(GF == GA),
      L = sum(GF < GA),
      GF = sum(GF),
      GA = sum(GA),
      GD = GF - GA,
      Pts = W * 3 + D,
      .groups = "drop"
    ) %>%
    arrange(Group, desc(Pts), desc(GD), desc(GF))
}
standings <- calculate_standings(real_data)

top_scorer <- standings %>% filter(GF == max(GF)) %>% pull(Team)
least_conceded <- standings %>% filter(GA == min(GA)) %>% pull(Team)
most_wins <- standings %>% filter(W == max(W)) %>% pull(Team)

format_stat <- function(teams) {
  if (length(teams) > 5) return(paste0(paste(head(teams, 5), collapse = ", "), " (+", length(teams)-5, ")"))
  return(paste(teams, collapse = ", "))
}

calculate_accuracy <- function(real, pred) {
  if(is.null(pred) || nrow(pred) == 0) return(0)
  # Only check accuracy for played matches
  joined <- real %>% 
    inner_join(pred, by = c("Team1", "Team2"), suffix = c("_real", "_pred")) %>%
    filter(!is.na(Goals1_real) & !is.na(Goals2_real))
  if(nrow(joined) == 0) return(0)
  
  joined <- joined %>%
    mutate(
      real_outcome = case_when(Goals1_real > Goals2_real ~ "W1", Goals1_real < Goals2_real ~ "W2", TRUE ~ "D"),
      pred_outcome = case_when(Goals1_pred > Goals2_pred ~ "W1", Goals1_pred < Goals2_pred ~ "W2", TRUE ~ "D"),
      correct = real_outcome == pred_outcome
    )
  round(mean(joined$correct) * 100, 1)
}

get_radar_data <- function(team, real, pred) {
  # Exclude unplayed (NA) matches
  real_team <- bind_rows(
    real %>% filter(Team1 == team) %>% select(GF=Goals1, GA=Goals2),
    real %>% filter(Team2 == team) %>% select(GF=Goals2, GA=Goals1)
  ) %>% filter(!is.na(GF) & !is.na(GA))
  
  pred_team <- bind_rows(
    pred %>% filter(Team1 == team) %>% select(GF=Goals1, GA=Goals2),
    pred %>% filter(Team2 == team) %>% select(GF=Goals2, GA=Goals1)
  ) %>% filter(!is.na(GF) & !is.na(GA))
  
  calc_stats <- function(df) {
    if(nrow(df) == 0) return(c(0,0,0))
    c(
      mean(df$GF, na.rm=TRUE),
      mean(df$GA, na.rm=TRUE),
      mean(df$GF > df$GA, na.rm=TRUE) * 3 + mean(df$GF == df$GA, na.rm=TRUE)
    )
  }
  
  list(
    categories = c("Goals For (avg)", "Goals Against (avg)", "Points (avg)"),
    real = calc_stats(real_team),
    pred = calc_stats(pred_team)
  )
}

generate_bracket_ui <- function(matches_df) {
  stages <- c("Round of 32" = 2, "Round of 16" = 3, "Quarter-Finals" = 4, "Semi-Finals" = 5, "Final" = 7)
  cols <- lapply(names(stages), function(s_name) {
    s_id <- stages[[s_name]]
    stage_matches <- matches_df %>% filter(stage_id == s_id)
    
    # Group matches into pairs (matchups)
    matchups <- lapply(seq(1, nrow(stage_matches), by=2), function(i) {
      if(i == nrow(stage_matches)) { # Single match (e.g., Final)
        div(class="bracket-matchup",
            div(class="bracket-match", stage_matches$match_label[i])
        )
      } else {
        div(class="bracket-matchup",
            div(class="bracket-match", stage_matches$match_label[i]),
            div(class="bracket-match", stage_matches$match_label[i+1])
        )
      }
    })
    
    div(class="bracket-col",
        div(style="text-align: center; color: #7E7F83; font-weight: bold; margin-bottom: 15px; text-transform: uppercase; font-size: 12px;", s_name),
        do.call(tagList, matchups)
    )
  })
  div(class="bracket-container", do.call(tagList, cols))
}

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bg = "#14110F",
    fg = "#F3F3F4",
    primary = "#D9C5B2",
    secondary = "#34312D"
  ),
  sidebar = sidebar(
    bg = "#34312D",
    h3("World Cup 2026 Dashboard", style = "color: #D9C5B2; font-weight: bold; margin-bottom: 20px; line-height: 1.2; text-align: center;"),
    hr(style = "border-top: 1px solid #7E7F83;"),
    h5("Prediction Compare", style = "color: #D9C5B2; font-weight: bold; margin-bottom: 15px;"),
    p("Upload a prediction CSV to compare against the real results.", style = "font-size: 13px; color: #A0A0A0;"),
    fileInput("user_prediction", "Load Prediction (.csv):", accept = c(".csv"), buttonLabel = "Browse...")
  ),
  tags$head(
    tags$style(HTML("
      /* Force body and html to take exact viewport height and hide browser scrollbars */
      html, body {
        height: 100vh;
        margin: 0;
        padding: 0;
        overflow: hidden !important;
      }
      .card-body {
        overflow: hidden !important;
        padding: 10px !important;
      }
      
      /* Style file input button and text to contrast with sidebar */
      .input-group .btn-file, .input-group .form-control {
        background-color: #14110F !important;
        color: #D9C5B2 !important;
        border: 1px solid #7E7F83 !important;
      }
      .input-group .btn-file:hover {
        background-color: #2A2723 !important;
      }
      
      /* Dashboard Grid to fit inside card body with safety margin */
      .dashboard-grid {
        display: grid;
        grid-template-columns: repeat(12, minmax(0, 1fr));
        grid-template-rows: repeat(6, minmax(0, 1fr));
        gap: 16px;
        height: calc(100vh - 130px); 
        margin-top: 5px;
      }
      .widget-map {
        grid-column: 1 / 5;
        grid-row: 1 / 7;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        overflow: hidden;
        position: relative;
      }
      .widget-radar {
        grid-column: 5 / 7;
        grid-row: 2 / 4;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }
      .widget-scatter {
        grid-column: 7 / 11;
        grid-row: 2 / 4;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }
      .widget-matches {
        grid-column: 11 / 13;
        grid-row: 1 / 7;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        overflow-y: auto;
        padding: 10px;
      }
      .widget-table {
        grid-column: 5 / 11;
        grid-row: 4 / 7;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 15px;
        overflow-y: auto;
      }
      .stats-accuracy-container {
        grid-column: 5 / 11;
        grid-row: 1 / 2;
        display: flex;
        gap: 16px;
        width: 100%;
        height: 100%;
      }
      .stat-square, .widget-accuracy {
        flex: 1 1 0;
        min-width: 0;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        overflow-y: auto;
      }
      .stat-wide {
        flex: 2 1 0;
        min-width: 0;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        overflow-y: auto;
      }
      
      .match-row { display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #7E7F83; font-size: 13px; align-items: center;}
      .match-row:last-child { border-bottom: none; }
      .actual-score { color: #F1C40F; font-weight: bold; margin: 0 2px;}
      .pred-score { color: #A0A0A0; font-size: 11px; }
      
      .stat-title { font-size: 11px; color: #7E7F83; text-transform: uppercase; font-weight: bold; margin-bottom: 4px; line-height: 1.2; text-align: center;}
      .stat-value { font-size: 14px; font-weight: bold; color: #D9C5B2; line-height: 1.2; text-align: center; word-break: break-word;}
      
      .accuracy-score { font-size: 32px; font-weight: bold; color: #F1C40F; line-height: 1;}
      .accuracy-sub { font-size: 11px; color: #A0A0A0; text-align: center; margin-top: 5px;}
      .group-header { font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #D9C5B2; font-size: 14px; border-bottom: 1px solid #7E7F83;}
      
      .nav-underline .nav-link.active { color: #F1C40F !important; border-bottom-color: #F1C40F !important; }
      .nav-underline .nav-link { color: #D9C5B2; }
      
      /* Knockout Bracket CSS */
      .bracket-container {
        display: flex;
        height: calc(100vh - 130px);
        background-color: #34312D;
        border-radius: 8px;
        border: 1px solid #7E7F83;
        padding: 20px;
        overflow-x: auto;
      }
      .bracket-col {
        display: flex;
        flex-direction: column;
        justify-content: space-around;
        width: 140px;
        margin: 0 20px;
        position: relative;
        flex-shrink: 0;
      }
      .bracket-matchup {
        display: flex;
        flex-direction: column;
        justify-content: space-around;
        align-items: center;
        flex: 1;
        position: relative;
        padding: 10px 0;
      }
      .bracket-match {
        background-color: #14110F; 
        border: 1px solid #7E7F83; 
        border-radius: 6px; 
        padding: 8px; 
        text-align: center; 
        color: #D9C5B2; 
        font-size: 12px; 
        font-weight: bold; 
        width: 100%; 
        position: relative;
        z-index: 2;
        box-shadow: 0 2px 4px rgba(0,0,0,0.3);
      }
      
      /* Lines combining two matches in a matchup */
      .bracket-col:not(:last-child) .bracket-matchup::after {
        content: '';
        position: absolute;
        right: -20px;
        top: 25%;
        height: 50%;
        width: 20px;
        border-right: 2px solid #7E7F83;
        border-top: 2px solid #7E7F83;
        border-bottom: 2px solid #7E7F83;
        z-index: 1;
      }
      /* Final single match does not have a pair to connect vertically */
      .bracket-col:last-child .bracket-matchup::after {
        display: none;
      }
      /* Line coming into the left side of matches in later rounds */
      .bracket-col:not(:first-child) .bracket-match::before {
        content: '';
        position: absolute;
        left: -20px;
        top: 50%;
        width: 20px;
        border-top: 2px solid #7E7F83;
        z-index: 1;
      }
    "))
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
              h5("Group Matches", style = "margin-top:0; font-weight:bold; color:#D9C5B2; font-size:14px;"),
              uiOutput("matches_ui")
          ),
          div(class = "widget-table",
              h5("Group Table", style = "margin-top:0; font-weight:bold; color:#D9C5B2; font-size:14px;"),
              selectInput("group_filter", NULL, choices = unique(standings$Group), width = "100%"),
              DTOutput("group_table_ui")
          )
      )
    ),
    nav_panel("Knockout Bracket",
        uiOutput("bracket_ui")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "radar_team", choices = sort(unique(c(real_data$Team1, real_data$Team2))))
  })
  
  output$stats_ui <- renderUI({
    make_stat_box <- function(title, teams) {
      class_name <- if (length(teams) > 1) "stat-wide" else "stat-square"
      div(class = class_name,
          div(class="stat-title", title),
          div(class="stat-value", format_stat(teams))
      )
    }
    
    tagList(
      make_stat_box("Top Scorer", top_scorer),
      make_stat_box("Least Conceded", least_conceded),
      make_stat_box("Most Wins", most_wins)
    )
  })
  
  output$group_table_ui <- renderDT({
    d <- standings %>% 
      filter(Group == input$group_filter) %>%
      mutate(`GF:GA` = paste0(GF, ":", GA)) %>%
      select(Team, P, W, D, L, `GF:GA`, GD, Pts)
      
    datatable(d, options = list(dom = 't', paging = FALSE), rownames = FALSE, style = "bootstrap") %>%
      formatStyle(columns = names(d), color = '#F3F3F4', backgroundColor = '#34312D')
  })
  
  current_preds <- reactive({
    if (is.null(input$user_prediction)) {
      parse_predictions("predictions/prediccion_ml.csv")
    } else {
      parse_predictions(input$user_prediction$datapath)
    }
  })
  
  output$stat_accuracy <- renderText({
    preds <- current_preds()
    acc <- calculate_accuracy(real_data, preds)
    paste0(acc, "%")
  })
  
  output$radar_plot <- renderPlotly({
    req(input$radar_team)
    team <- input$radar_team
    preds <- current_preds()
    if(is.null(preds)) preds <- real_data[0,]
    
    d <- get_radar_data(team, real_data, preds)
    d$real[is.na(d$real)] <- 0
    d$pred[is.na(d$pred)] <- 0
    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = d$real,
        theta = d$categories,
        name = 'Actual',
        fillcolor = 'rgba(241, 196, 15, 0.4)',
        line = list(color = '#F1C40F')
      ) %>%
      add_trace(
        r = d$pred,
        theta = d$categories,
        name = 'Predicted',
        fillcolor = 'rgba(160, 160, 160, 0.4)',
        line = list(color = '#A0A0A0')
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, max(c(d$real, d$pred, 3))), gridcolor="#7E7F83", linecolor="#7E7F83", tickfont=list(color="#D9C5B2")),
          angularaxis = list(tickfont = list(color = '#D9C5B2'), gridcolor="#7E7F83", linecolor="#7E7F83")
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#D9C5B2', size=10),
        showlegend = FALSE,
        margin = list(l=20, r=20, t=20, b=20)
      )
  })
  
  output$scatter_plot <- renderPlotly({
    plot_ly(
      data = standings, 
      x = ~GF, 
      y = ~GA, 
      text = ~Team,
      type = 'scatter', 
      mode = 'markers+text',
      textposition = 'top center',
      marker = list(color = '#F1C40F', size = 6)
    ) %>%
      layout(
        title = list(text = "Goals For vs Goals Against", font = list(color = '#D9C5B2', size = 11)),
        xaxis = list(title = "Goals Made (GF)", color = '#D9C5B2', gridcolor = '#7E7F83', zerolinecolor = '#7E7F83'),
        yaxis = list(title = "Goals Received (GA)", color = '#D9C5B2', gridcolor = '#7E7F83', zerolinecolor = '#7E7F83'),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#D9C5B2', size=10),
        margin = list(l=40, r=20, t=30, b=40)
      )
  })
  
  output$matches_ui <- renderUI({
    preds <- current_preds()
    if(is.null(preds)) return(div("Error loading predictions.", style="color:red;"))
    
    joined <- real_data %>% 
      left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    
    match_divs <- lapply(unique(joined$Group_real), function(g) {
      g_matches <- joined %>% filter(Group_real == g)
      
      rows <- lapply(1:nrow(g_matches), function(i) {
        row <- g_matches[i, ]
        
        # Format scores beautifully (handling empty NA matches)
        goals1_real_txt <- if(is.na(row$Goals1_real)) "-" else as.character(row$Goals1_real)
        goals2_real_txt <- if(is.na(row$Goals2_real)) "-" else as.character(row$Goals2_real)
        goals1_pred_txt <- if(is.na(row$Goals1_pred)) "" else paste0("(", row$Goals1_pred, ")")
        goals2_pred_txt <- if(is.na(row$Goals2_pred)) "" else paste0("(", row$Goals2_pred, ")")
        
        div(class = "match-row",
            span(row$Team1_Abrev_real, style = "width: 35px; text-align: left;"),
            span(
              span(class="actual-score", goals1_real_txt),
              span(class="pred-score", goals1_pred_txt),
              " - ",
              span(class="pred-score", goals2_pred_txt),
              span(class="actual-score", goals2_real_txt)
            ),
            span(row$Team2_Abrev_real, style = "width: 35px; text-align: right;")
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
      joined <- real_data
    } else {
      joined <- real_data %>% 
        left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    }
    joined <- joined %>% filter(!is.na(city_id_real))
    
    map_data <- host_cities %>%
      rowwise() %>%
      mutate(
        popup_text = {
          city_matches <- joined %>% filter(city_id_real == id)
          if(nrow(city_matches) > 0) {
            match_list <- paste0(city_matches$Team1_Abrev_real, " vs ", city_matches$Team2_Abrev_real, collapse = "<br>")
            paste0("<b>", city_name, "</b><br>", match_list)
          } else {
            paste0("<b>", city_name, "</b><br>No group matches")
          }
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
}

shinyApp(ui, server)
