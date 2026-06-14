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
  "Czech Republic" = "República Checa", "Canada" = "Canadá", 
  "Bosnia and Herzegovina" = "Bosnia y Herzegovina", "Qatar" = "Catar", 
  "Switzerland" = "Suiza", "Brazil" = "Brasil", "Morocco" = "Marruecos",
  "Haiti" = "Haití", "Scotland" = "Escocia", "USA" = "Estados Unidos",
  "Paraguay" = "Paraguay", "Australia" = "Australia", "Turkey" = "Turquía", 
  "Germany" = "Alemania", "Curaçao" = "Curazao", "Côte d'Ivoire" = "Costa de Marfil", 
  "Ecuador" = "Ecuador", "Netherlands" = "Países Bajos", "Japan" = "Japón", 
  "Sweden" = "Suecia", "Tunisia" = "Túnez", "Belgium" = "Bélgica", 
  "Egypt" = "Egipto", "IR Iran" = "Irán", "New Zealand" = "Nueva Zelanda", 
  "Spain" = "España", "Cabo Verde" = "Cabo Verde", "Saudi Arabia" = "Arabia Saudita", 
  "Uruguay" = "Uruguay", "France" = "Francia", "Senegal" = "Senegal", 
  "Iraq" = "Irak", "Norway" = "Noruega", "Argentina" = "Argentina", 
  "Algeria" = "Argelia", "Austria" = "Austria", "Jordan" = "Jordania", 
  "Portugal" = "Portugal", "DR Congo" = "República Democrática del Congo", 
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
  stage_names <- c("Group Stage", "Round of 32", "Round of 16", "Quarter-final", "Semi-final", "Third Place Playoff", "Final")
  data <- data %>%
    left_join(matches, by = c("MatchID" = "id")) %>%
    mutate(
      Group = if_else(stage_id == 1, match_label, stage_names[stage_id]),
      MatchDay_Label = factor(case_when(
        stage_id == 1 & match_number <= 24 ~ "Groups first match",
        stage_id == 1 & match_number <= 48 ~ "Groups second match",
        stage_id == 1 & match_number <= 72 ~ "Groups third match",
        stage_id == 2 ~ "Round of 32",
        stage_id == 3 ~ "Round of 16",
        stage_id == 4 ~ "Quarter-final",
        stage_id == 5 ~ "Semi-final",
        stage_id == 6 ~ "Third Place Playoff",
        stage_id == 7 ~ "Final",
        TRUE ~ "Unknown"
      ), levels = c("Groups first match", "Groups second match", "Groups third match", "Round of 32", "Round of 16", "Quarter-final", "Semi-final", "Third Place Playoff", "Final"))
    )
  
  data$Team1_Abrev <- sapply(data$Team1, spanish_to_fifa)
  data$Team2_Abrev <- sapply(data$Team2, spanish_to_fifa)
  return(data)
}

save_predictions <- function(df, filepath) {
  lines <- readLines(filepath, encoding = "UTF-8")
  valid_lines_idx <- which(!str_detect(lines, "^;+$") & !str_detect(lines, "^;.*") & !str_detect(lines, "^Equipo;Resultado;;Equipo") & lines != "")
  
  for(i in 1:nrow(df)) {
    idx <- df$MatchID[i]
    if(idx <= length(valid_lines_idx)) {
      line_num <- valid_lines_idx[idx]
      g1 <- if(is.na(df$Goals1[i])) "NA" else df$Goals1[i]
      g2 <- if(is.na(df$Goals2[i])) "NA" else df$Goals2[i]
      lines[line_num] <- paste(df$Team1[i], g1, g2, df$Team2[i], sep=";")
    }
  }
  writeLines(lines, filepath, useBytes=TRUE)
}

calculate_standings <- function(d) {
  # Base table with all group stage teams directly from teams.csv
  base <- teams %>%
    mutate(
      Group = paste("Group", group_letter),
      Team = unname(english_to_spanish[team_name]),
      P = 0, W = 0, D = 0, L = 0, GF = 0, GA = 0, GD = 0, Pts = 0
    ) %>%
    select(Group, Team, P, W, D, L, GF, GA, GD, Pts)
    
  d1 <- d %>% filter(stage_id == 1 & !is.na(Goals1) & !is.na(Goals2)) %>% select(Team = Team1, GF = Goals1, GA = Goals2)
  d2 <- d %>% filter(stage_id == 1 & !is.na(Goals1) & !is.na(Goals2)) %>% select(Team = Team2, GF = Goals2, GA = Goals1)
  
  played <- bind_rows(d1, d2) %>%
    group_by(Team) %>%
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
    )
    
  base %>%
    left_join(played, by = "Team", suffix = c("_base", "")) %>%
    mutate(
      P = coalesce(P, 0),
      W = coalesce(W, 0),
      D = coalesce(D, 0),
      L = coalesce(L, 0),
      GF = coalesce(GF, 0),
      GA = coalesce(GA, 0),
      GD = coalesce(GD, 0),
      Pts = coalesce(Pts, 0)
    ) %>%
    select(Group, Team, P, W, D, L, GF, GA, GD, Pts) %>%
    arrange(Group, desc(Pts), desc(GD), desc(GF))
}

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
  
  make_col <- function(s_name, match_ids, side_class, is_first_or_last = FALSE) {
    col_matches <- matches_df %>% filter(id %in% match_ids) %>% arrange(id)
    
    matchups <- lapply(seq(1, nrow(col_matches), by=2), function(i) {
      if(i == nrow(col_matches)) { 
        div(class="bracket-matchup",
            div(class="bracket-match", col_matches$match_label[i])
        )
      } else {
        div(class="bracket-matchup",
            div(class="bracket-match", col_matches$match_label[i]),
            div(class="bracket-match", col_matches$match_label[i+1])
        )
      }
    })
    
    extra_class <- if(is_first_or_last) "first-col" else ""
    if (side_class == "bracket-col-right" && is_first_or_last) extra_class <- "last-col"
    
    div(class=paste("bracket-col", side_class, extra_class),
        div(style="text-align: center; color: #7E7F83; font-weight: bold; margin-bottom: 15px; text-transform: uppercase; font-size: 12px;", s_name),
        do.call(tagList, matchups)
    )
  }
  
  left_cols <- list(
    make_col("Round of 32", 73:80, "bracket-col-left", TRUE),
    make_col("Round of 16", 89:92, "bracket-col-left"),
    make_col("Quarter-Finals", 97:98, "bracket-col-left"),
    make_col("Semi-Finals", 101, "bracket-col-left")
  )
  
  center_col <- div(class="bracket-col bracket-col-center",
      div(style="text-align: center; color: #7E7F83; font-weight: bold; margin-bottom: 15px; text-transform: uppercase; font-size: 12px;", "Finals"),
      div(class="bracket-matchup",
          div(class="bracket-match final-match", matches_df$match_label[matches_df$id == 104]),
          div(class="third-place-wrapper", style="width: 100%; display: flex; flex-direction: column; align-items: center;",
              div(style="text-align: center; color: #7E7F83; font-weight: bold; margin-bottom: 5px; text-transform: uppercase; font-size: 10px;", "Third Place"),
              div(class="bracket-match third-place-match", style="border-color: #555; color: #888; width: 100%;", matches_df$match_label[matches_df$id == 103])
          )
      )
  )
  
  right_cols <- list(
    make_col("Semi-Finals", 102, "bracket-col-right"),
    make_col("Quarter-Finals", 99:100, "bracket-col-right"),
    make_col("Round of 16", 93:96, "bracket-col-right"),
    make_col("Round of 32", 81:88, "bracket-col-right", TRUE)
  )
  
  div(class="bracket-container", 
      do.call(tagList, left_cols), 
      center_col, 
      do.call(tagList, right_cols)
  )
}

# --- UI ---
ui <- page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bg = "#14110F",
    fg = "#F3F3F4",
    primary = "#D9C5B2",
    secondary = "#34312D"
  ),
  sidebar = sidebar(
    bg = "#34312D",
    h3("World Cup 2026 Dashboard", style = "color: #D9C5B2; font-weight: bold; margin-bottom: 15px; line-height: 1.2; text-align: center;"),
    p("An interactive dashboard to visualize, analyze, and compare FIFA World Cup 2026 predictions against real tournament results. Features live bracket logic, interactive match maps, and an in-app data editor.", 
      style = "font-size: 13px; color: #D9C5B2; line-height: 1.45; margin-bottom: 20px; text-align: center; opacity: 0.85;"),
    hr(style = "border-top: 1px solid #7E7F83;"),
    h5("Prediction Compare", style = "color: #D9C5B2; font-weight: bold; margin-bottom: 15px;"),
    p("Upload a prediction CSV to compare against the real results.", style = "font-size: 13px; color: #A0A0A0;"),
    fileInput("user_prediction", "Load Prediction (.csv):", accept = c(".csv"), buttonLabel = "Browse..."),
    hr(style = "border-top: 1px solid #7E7F83; margin-top: 20px;"),
    div(
      style = "display: flex; align-items: center; justify-content: space-between; padding-right: 10px;",
      h5("Light Mode", style = "color: #D9C5B2; font-weight: bold; margin: 0;"),
      checkboxInput("theme_toggle", "", value = FALSE)
    )
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
      .widget-timeline {
        grid-column: 5 / 11;
        grid-row: 4 / 5;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px 10px;
        overflow: hidden;
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
        grid-column: 5 / 9;
        grid-row: 5 / 7;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 15px;
        overflow-y: auto;
      }
      .widget-scorers {
        grid-column: 9 / 11;
        grid-row: 5 / 6;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px;
        overflow: hidden;
      }
      .widget-goaldiff {
        grid-column: 9 / 11;
        grid-row: 6 / 7;
        background-color: #34312D;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px;
        overflow: hidden;
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
        container-type: inline-size;
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
      
      .accuracy-score { font-size: clamp(16px, 15cqi, 48px); font-weight: bold; color: #F1C40F; line-height: 1;}
      .accuracy-sub { font-size: clamp(9px, 8cqi, 12px); color: #A0A0A0; text-align: center; margin-top: 5px;}
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
        justify-content: space-between;
        width: 100%;
      }
      .bracket-col {
        display: flex;
        flex-direction: column;
        justify-content: space-around;
        width: 130px;
        margin: 0 10px;
        position: relative;
        flex-shrink: 0;
      }
      .bracket-col-center {
        width: 150px;
        margin: 0 30px;
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
      
      /* LEFT SIDE */
      .bracket-col-left .bracket-matchup::after {
        content: '';
        position: absolute;
        right: -10px;
        top: 25%;
        height: 50%;
        width: 10px;
        border-right: 2px solid #7E7F83;
        border-top: 2px solid #7E7F83;
        border-bottom: 2px solid #7E7F83;
        z-index: 1;
      }
      .bracket-col-left:not(.first-col) .bracket-match::before {
        content: '';
        position: absolute;
        left: -10px;
        top: 50%;
        width: 10px;
        border-top: 2px solid #7E7F83;
        z-index: 1;
      }
      
      /* RIGHT SIDE */
      .bracket-col-right .bracket-matchup::after {
        content: '';
        position: absolute;
        left: -10px;
        top: 25%;
        height: 50%;
        width: 10px;
        border-left: 2px solid #7E7F83;
        border-top: 2px solid #7E7F83;
        border-bottom: 2px solid #7E7F83;
        z-index: 1;
      }
      .bracket-col-right:not(.last-col) .bracket-match::before {
        content: '';
        position: absolute;
        right: -10px;
        top: 50%;
        width: 10px;
        border-top: 2px solid #7E7F83;
        z-index: 1;
      }

      /* CENTER COLUMN */
      .bracket-col-center .bracket-match::before {
        content: '';
        position: absolute;
        left: -30px;
        top: 50%;
        width: 30px;
        border-top: 2px solid #7E7F83;
        z-index: 1;
      }
      .bracket-col-center .bracket-match::after {
        content: '';
        position: absolute;
        right: -30px;
        top: 50%;
        width: 30px;
        border-top: 2px solid #7E7F83;
        z-index: 1;
      }
      
      /* LIGHT MODE OVERRIDES */
      body.light-mode, body.light-mode html {
        background-color: #F3F3F4 !important;
      }
      body.light-mode .card-body {
        background-color: #F3F3F4 !important;
      }
      body.light-mode .widget-map, 
      body.light-mode .widget-radar, 
      body.light-mode .widget-scatter, 
      body.light-mode .widget-matches, 
      body.light-mode .widget-table, 
      body.light-mode .widget-timeline,
      body.light-mode .widget-scorers,
      body.light-mode .widget-goaldiff,
      body.light-mode .stat-square, 
      body.light-mode .stat-wide, 
      body.light-mode .widget-accuracy,
      body.light-mode .bracket-container {
        background-color: #FFFFFF !important;
        border: 1px solid #E2E8F0 !important;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05);
      }
      body.light-mode .sidebar {
        background-color: #F8F9FA !important;
        border-right: 1px solid #E2E8F0 !important;
      }
      body.light-mode .bracket-match {
        background-color: #FFFFFF !important;
        color: #14110F !important;
        border: 1px solid #E2E8F0 !important;
        box-shadow: 0 1px 2px rgba(0,0,0,0.05);
      }
      body.light-mode .stat-value { color: #14110F !important; }
      body.light-mode h3, body.light-mode h4, body.light-mode h5, body.light-mode p { color: #14110F !important; }
      body.light-mode .group-header { color: #14110F !important; }
      body.light-mode .input-group .btn-file, 
      body.light-mode .input-group .form-control {
        background-color: #FFFFFF !important;
        color: #14110F !important;
        border: 1px solid #E2E8F0 !important;
      }
      body.light-mode .nav-underline .nav-link { color: #34312D; }
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
              h5("Matches by Stage", style = "margin-top:0; font-weight:bold; color:#D9C5B2; font-size:14px;"),
              uiOutput("matches_ui")
          ),
          div(class = "widget-timeline",
              plotlyOutput("timeline_plot", height = "100%")
          ),
          div(class = "widget-table",
              h5("Group Table", style = "margin-top:0; font-weight:bold; color:#D9C5B2; font-size:14px;"),
              selectInput("group_filter", NULL, choices = sort(unique(paste("Group", teams$group_letter))), width = "100%"),
              DTOutput("group_table_ui")
          ),
          div(class = "widget-scorers",
              plotlyOutput("top_scorers_plot", height = "100%")
          ),
          div(class = "widget-goaldiff",
              plotlyOutput("goal_diff_plot", height = "100%")
          )
      )
    ),
    nav_panel("Knockout Bracket",
        uiOutput("bracket_ui")
    ),
    nav_panel("Data Editor",
        div(style = "padding: 20px;",
            h4("Edit Predictions and Actual Results", style = "color: #D9C5B2;"),
            p("Changes made here will update the dashboard and save to the respective CSV file.", style="color: #A0A0A0;"),
            fluidRow(
                column(4, selectInput("editor_dataset", "Select Dataset to Edit:", choices = c("Actual Results", "Predictions"))),
                column(4, actionButton("save_editor", "Save Changes to File", class="btn btn-primary", style="margin-top: 32px; background-color: #F1C40F; color: #14110F; border: none; font-weight: bold;"))
            ),
            DTOutput("editor_table")
        )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive Data Storage
  rv <- reactiveValues(
    real_data = parse_predictions("predictions/resultados_reales.csv"),
    ml_preds = parse_predictions("predictions/prediccion_ml.csv")
  )
  
  standings <- reactive({ calculate_standings(rv$real_data) })
  top_scorer <- reactive({ standings() %>% filter(GF == max(GF)) %>% pull(Team) })
  least_conceded <- reactive({ standings() %>% filter(GA == min(GA)) %>% pull(Team) })
  most_wins <- reactive({ standings() %>% filter(W == max(W)) %>% pull(Team) })
  
  observe({
    teams <- sort(unique(c(rv$real_data$Team1, rv$real_data$Team2)))
    teams <- teams[teams %in% unname(english_to_spanish)]
    updateSelectInput(session, "radar_team", choices = teams)
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
      make_stat_box("Top Scorer", top_scorer()),
      make_stat_box("Least Conceded", least_conceded()),
      make_stat_box("Most Wins", most_wins())
    )
  })
  
  output$group_table_ui <- renderDT({
    d <- standings() %>% 
      filter(Group == input$group_filter) %>%
      mutate(`GF:GA` = paste0(GF, ":", GA)) %>%
      select(Team, P, W, D, L, `GF:GA`, GD, Pts)
      
    datatable(d, options = list(dom = 't', paging = FALSE), rownames = FALSE, style = "bootstrap") %>%
      formatStyle(columns = names(d), color = '#F3F3F4', backgroundColor = '#34312D', fontSize = '11px')
  })
  
  output$top_scorers_plot <- renderPlotly({
    st <- standings() %>% arrange(desc(GF)) %>% head(5)
    
    plot_ly(st, x = ~GF, y = ~reorder(Team, GF), type = 'bar', orientation = 'h',
            marker = list(color = '#D9C5B2')) %>%
      layout(
        title = list(text = "Top Scorers (GF)", font = list(color = '#D9C5B2', size = 12)),
        xaxis = list(title = "", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE, tickfont = list(size=9)),
        yaxis = list(title = "", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE, tickfont = list(size=10)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l=40, r=10, t=30, b=20)
      )
  })

  output$goal_diff_plot <- renderPlotly({
    d <- standings() %>% filter(Group == input$group_filter)
    d$GA_neg <- -d$GA
    
    plot_ly(d, y = ~reorder(Team, GD)) %>%
      add_trace(x = ~GF, name = 'GF', type = 'bar', orientation = 'h',
                marker = list(color = '#F1C40F')) %>%
      add_trace(x = ~GA_neg, name = 'GA', type = 'bar', orientation = 'h',
                marker = list(color = '#7E7F83')) %>%
      layout(
        title = list(text = paste("Goal Diff (GF vs GA)"), font = list(color = '#D9C5B2', size = 12)),
        barmode = 'relative',
        xaxis = list(title = "", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zerolinecolor = '#7E7F83', tickfont = list(size=9)),
        yaxis = list(title = "", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE, tickfont = list(size=10)),
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
    joined <- rv$real_data %>% 
      left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    
    joined$TotalReal <- rowSums(joined[, c("Goals1_real", "Goals2_real")], na.rm = TRUE)
    joined$TotalPred <- rowSums(joined[, c("Goals1_pred", "Goals2_pred")], na.rm = TRUE)
    
    # Make sure stage factors are properly ordered for time-series flow
    stage_levels <- c("Groups first match", "Groups second match", "Groups third match", 
                      "Round of 32", "Round of 16", "Quarterfinals", "Semifinals", "Final")
    joined$MatchDay_Label_real <- factor(joined$MatchDay_Label_real, levels = stage_levels)
    
    trend <- joined %>%
      group_by(MatchDay_Label_real) %>%
      summarise(
        ActualGoals = sum(TotalReal, na.rm=TRUE),
        PredGoals = sum(TotalPred, na.rm=TRUE),
        .groups='drop'
      ) %>%
      filter(!is.na(MatchDay_Label_real))
    
    p <- plot_ly(trend, x = ~MatchDay_Label_real) %>%
      add_trace(y = ~ActualGoals, name = 'Actual', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#F1C40F', width = 2), marker = list(color = '#F1C40F', size = 6)) %>%
      add_trace(y = ~PredGoals, name = 'Predicted', type = 'scatter', mode = 'lines+markers',
                line = list(color = '#A0A0A0', width = 2, dash = 'dot'), marker = list(color = '#A0A0A0', size = 6)) %>%
      layout(
        title = list(text = "Total Goals by Tournament Stage", font = list(color = '#D9C5B2', size = 12)),
        xaxis = list(title = "", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE, 
                     tickangle = 0, tickfont = list(size=9)),
        yaxis = list(title = "Goals Scored", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE,
                     tickfont = list(size=10)),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#D9C5B2', size=10),
        margin = list(l=30, r=10, t=30, b=20),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center")
      )
      
    # Use shinyjs to conditionally style the plotly if light mode is active? 
    # Actually, Plotly's transparent bg allows the CSS container to shine through!
    # But text colors are hardcoded. We'll leave them beige for now or the user can just enjoy the contrast.
    
    p
  })
  
  output$stat_accuracy <- renderText({
    preds <- current_preds()
    acc <- calculate_accuracy(rv$real_data, preds)
    paste0(acc, "%")
  })
  
  output$radar_plot <- renderPlotly({
    req(input$radar_team)
    team <- input$radar_team
    preds <- current_preds()
    if(is.null(preds)) preds <- rv$real_data[0,]
    
    d <- get_radar_data(team, rv$real_data, preds)
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
      data = standings(), 
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
        xaxis = list(title = "Goals Made (GF)", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE),
        yaxis = list(title = "Goals Received (GA)", color = '#D9C5B2', showgrid = FALSE, showline = TRUE, linecolor = '#7E7F83', zeroline = FALSE),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#D9C5B2', size=10),
        margin = list(l=40, r=20, t=30, b=40)
      )
  })
  
  output$matches_ui <- renderUI({
    preds <- current_preds()
    if(is.null(preds)) return(div("Error loading predictions.", style="color:red;"))
    
    joined <- rv$real_data %>% 
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
      joined <- rv$real_data %>% 
        rename(
          Team1_Abrev_real = Team1_Abrev,
          Team2_Abrev_real = Team2_Abrev,
          city_id_real = city_id,
          MatchDay_Label_real = MatchDay_Label
        )
    } else {
      joined <- rv$real_data %>% 
        left_join(preds, by = c("Team1", "Team2"), suffix = c("_real", "_pred"))
    }
    joined <- joined %>% filter(!is.na(city_id_real))
    
    map_data <- host_cities %>%
      rowwise() %>%
      mutate(
        popup_text = {
          city_matches <- joined %>% filter(city_id_real == id)
          if(nrow(city_matches) > 0) {
            match_list <- paste0(as.character(city_matches$MatchDay_Label_real), ": ", city_matches$Team1_Abrev_real, " vs ", city_matches$Team2_Abrev_real, collapse = "<br>")
            paste0("<b>", city_name, "</b><br>", match_list)
          } else {
            paste0("<b>", city_name, "</b><br>No matches")
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
  
  # --- Data Editor Logic ---
  editor_data <- reactive({
    if (input$editor_dataset == "Actual Results") {
      rv$real_data
    } else {
      rv$ml_preds
    }
  })
  
  output$editor_table <- renderDT({
    d <- editor_data() %>%
      select(MatchID, MatchDay_Label, Group, Team1, Goals1, Goals2, Team2)
    datatable(d, 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 7))),
              options = list(paging = FALSE, scrollX = TRUE, scrollY = "600px", dom = 't'),
              rownames = FALSE,
              style = "bootstrap"
    ) %>%
      formatStyle(columns = names(d), color = '#F3F3F4', backgroundColor = '#34312D')
  })
  
  observeEvent(input$editor_table_cell_edit, {
    info <- input$editor_table_cell_edit
    i <- info$row
    j <- info$col + 1 # JS is 0-indexed, R is 1-indexed
    v <- info$value
    
    d <- editor_data()
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
  })
  
  observeEvent(input$save_editor, {
    if (input$editor_dataset == "Actual Results") {
      save_predictions(rv$real_data, "predictions/resultados_reales.csv")
      showNotification("Actual Results saved to file!", type = "message")
    } else {
      save_predictions(rv$ml_preds, "predictions/prediccion_ml.csv")
      showNotification("ML Predictions saved to file!", type = "message")
    }
  })
}

shinyApp(ui, server)
