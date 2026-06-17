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

clean_kickoff_time <- function(kickoff_at) {
  if (is.na(kickoff_at) || kickoff_at == "") return(list(local_time = "", tz_label = "", utc_time = ""))
  
  # Extract local offset
  offset_str <- str_extract(kickoff_at, "[-+][0-9]{2}$")
  local_offset <- if (!is.na(offset_str)) as.numeric(offset_str) else -4
  
  # Extract datetime string without offset
  dt_part <- sub("[-+][0-9]{2}$", "", kickoff_at)
  
  # Parse EDT datetime (represented by the raw string)
  edt_dt <- as.POSIXlt(dt_part, format = "%Y-%m-%d %H:%M:%S")
  
  # Calculate local datetime by adding (local_offset - (-4)) hours
  offset_diff_seconds <- (local_offset + 4) * 3600
  local_dt <- edt_dt + offset_diff_seconds
  
  # Calculate UTC datetime by adding 4 hours to EDT
  utc_dt <- edt_dt + (4 * 3600)
  
  # Format local time nicely
  local_formatted <- format(local_dt, "%a, %d %b %H:%M")
  
  # Get timezone label
  tz_label <- case_when(
    local_offset == -4 ~ "EDT",
    local_offset == -5 ~ "CDT",
    local_offset == -6 ~ "CST",
    local_offset == -7 ~ "PDT",
    TRUE ~ paste0("UTC", offset_str)
  )
  
  list(
    local_time = local_formatted,
    tz_label = tz_label,
    utc_time = format(utc_dt, "%Y-%m-%d %H:%M:%S UTC")
  )
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
  
  data$Group_from_team <- sapply(data$Team1, function(name) {
    eng <- spanish_to_english[name]
    if (is.na(eng)) eng <- name
    grp <- teams$group_letter[teams$team_name == eng]
    if (length(grp) > 0) return(paste0("Group ", grp[1]))
    return(NA_character_)
  })

  # Map prediction row to matches.csv row dynamically
  data$matches_id <- sapply(seq_len(nrow(data)), function(i) {
    if (i > 72) {
      return(i) # For knockout stage, 73 to 104 correspond directly to matches 73 to 104
    }
    
    eng1 <- spanish_to_english[data$Team1[i]]
    if (is.na(eng1)) eng1 <- data$Team1[i]
    
    eng2 <- spanish_to_english[data$Team2[i]]
    if (is.na(eng2)) eng2 <- data$Team2[i]
    
    id1 <- teams$id[teams$team_name == eng1]
    id2 <- teams$id[teams$team_name == eng2]
    
    if (length(id1) > 0 && length(id2) > 0) {
      idx <- which(matches$stage_id == 1 & 
                   ((matches$home_team_id == id1[1] & matches$away_team_id == id2[1]) |
                    (matches$home_team_id == id2[1] & matches$away_team_id == id1[1])))
      if (length(idx) > 0) return(matches$id[idx[1]])
    }
    return(i)
  })

  data <- data %>%
    left_join(matches, by = c("matches_id" = "id")) %>%
    left_join(host_cities, by = c("city_id" = "id")) %>%
    mutate(
      Group = if_else(stage_id == 1, Group_from_team, stage_names[stage_id]),
      MatchDay_Label = factor(case_when(
        stage_id == 1 & (MatchID %% 6 == 1 | MatchID %% 6 == 2) ~ "Groups first match",
        stage_id == 1 & (MatchID %% 6 == 3 | MatchID %% 6 == 4) ~ "Groups second match",
        stage_id == 1 & (MatchID %% 6 == 5 | MatchID %% 6 == 0) ~ "Groups third match",
        stage_id == 2 ~ "Round of 32",
        stage_id == 3 ~ "Round of 16",
        stage_id == 4 ~ "Quarter-final",
        stage_id == 5 ~ "Semi-final",
        stage_id == 6 ~ "Third Place Playoff",
        stage_id == 7 ~ "Final",
        TRUE ~ "Unknown"
      ), levels = c("Groups first match", "Groups second match", "Groups third match", "Round of 32", "Round of 16", "Quarter-final", "Semi-final", "Third Place Playoff", "Final"))
    ) %>%
    select(-Group_from_team)
  
  # Clean kickoff times
  cleaned_list <- lapply(data$kickoff_at, clean_kickoff_time)
  data$Kickoff_Local <- sapply(cleaned_list, function(x) x$local_time)
  data$Kickoff_TZ <- sapply(cleaned_list, function(x) x$tz_label)
  data$Kickoff_UTC <- sapply(cleaned_list, function(x) x$utc_time)
  
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
  if (length(teams) == 0) return("")
  if (length(teams) > 1) return(paste0(teams[1], " +", length(teams) - 1))
  return(teams[1])
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
      .widget-middle-stats {
        grid-column: 5 / 9;
        grid-row: 3 / 4;
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 10px;
      }
      .stat-box-middle {
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        text-align: center;
        container-type: inline-size;
      }
      .widget-timeline {
        grid-column: 5 / 9;
        grid-row: 4 / 5;
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px 10px;
        overflow: hidden;
      }
      .widget-map {
        grid-column: 1 / 5;
        grid-row: 1 / 7;
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        overflow: hidden;
        position: relative;
      }
      .widget-radar {
        display: none;
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        overflow: hidden;
        flex-direction: column;
      }
      .widget-scatter {
        grid-column: 7 / 11;
        grid-row: 1 / 3;
        background-color: #14110F;
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
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        overflow-y: auto;
        padding: 10px;
      }
      /* Custom Scrollbars for all elements */
      *::-webkit-scrollbar {
        width: 6px;
        height: 6px;
      }
      *::-webkit-scrollbar-track {
        background: #202020;
        border-radius: 8px;
      }
      *::-webkit-scrollbar-thumb {
        background: #7E7F83;
        border-radius: 8px;
      }
      *::-webkit-scrollbar-thumb:hover {
        background: #749FD2;
      }
      .widget-table {
        grid-column: 5 / 9;
        grid-row: 5 / 7;
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }
      .widget-table .form-group {
        margin-bottom: 5px;
      }
      .widget-table .selectize-input {
        min-height: 26px !important;
        padding: 2px 8px !important;
        font-size: 11px !important;
        line-height: 1.2 !important;
      }
      .widget-table .table td, .widget-table .table th {
        padding: 4px 6px !important;
        font-size: 11px !important;
        text-align: center !important;
        vertical-align: middle !important;
      }
      .widget-table .table td:first-child, .widget-table .table th:first-child {
        text-align: left !important;
        padding-left: 5px !important;
      }
      .widget-scorers {
        grid-column: 9 / 11;
        grid-row: 3 / 5;
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px;
        overflow: hidden;
      }
      .widget-goaldiff {
        grid-column: 9 / 11;
        grid-row: 5 / 7;
        background-color: #14110F;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 5px;
        overflow: hidden;
      }
      .stats-accuracy-container {
        grid-column: 5 / 7;
        grid-row: 1 / 3;
        display: grid;
        grid-template-columns: 1fr 1fr;
        grid-template-rows: 1fr 1fr;
        gap: 16px;
        width: 100%;
        height: 100%;
      }
      .stat-square, .stat-wide, .widget-accuracy {
        background-color: #14110F;
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
      
      .match-row { display: flex; justify-content: space-between; padding: 8px 0; border-bottom: 1px solid #7E7F83; font-size: 13px; align-items: center;}
      .match-row:last-child { border-bottom: none; }
      .actual-score { color: #749FD2; font-weight: bold; margin: 0 2px;}
      .pred-score { color: #A0A0A0; font-size: 11px; }
      
      .stat-title { font-size: clamp(9px, 8cqi, 14px); color: #7E7F83; text-transform: uppercase; font-weight: bold; margin-bottom: 4px; line-height: 1.2; text-align: center;}
      .stat-value { font-size: clamp(12px, 12cqi, 28px); font-weight: bold; color: #D9C5B2; line-height: 1.2; text-align: center; word-break: break-word;}
      
      .accuracy-score { font-size: clamp(16px, 32cqi, 48px); font-weight: bold; color: #749FD2; line-height: 1;}
      .accuracy-sub { font-size: clamp(9px, 8cqi, 12px); color: #A0A0A0; text-align: center; margin-top: 5px;}
      .group-header { font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #D9C5B2; font-size: 14px; border-bottom: 1px solid #7E7F83;}
      
      .nav-underline .nav-link.active { color: #749FD2 !important; border-bottom-color: #16549b !important; }
      .nav-underline .nav-link { color: #D9C5B2; }
      
      /* Calendar CSS */
      .calendar-wrapper {
        display: flex;
        flex-direction: column;
        height: calc(100vh - 150px);
        background-color: #202020;
        border: 1px solid #7E7F83;
        border-radius: 8px;
        padding: 10px;
        margin: 10px;
      }
      .calendar-header {
        display: grid;
        grid-template-columns: repeat(7, 1fr);
        text-align: center;
        font-weight: bold;
        color: #749FD2;
        margin-bottom: 5px;
        font-size: 14px;
        border-bottom: 1px solid #7E7F83;
        padding-bottom: 5px;
      }
      .calendar-grid {
        display: grid;
        grid-template-columns: repeat(7, 1fr);
        grid-template-rows: repeat(7, 1fr);
        gap: 6px;
        flex-grow: 1;
        min-height: 0;
      }
      .calendar-day {
        background-color: #14110F;
        border: 1px solid #333;
        border-radius: 6px;
        padding: 4px;
        display: flex;
        flex-direction: column;
        position: relative;
        overflow-y: auto;
      }
      .calendar-date {
        font-size: 13px;
        font-weight: bold;
        color: #7E7F83;
        text-align: right;
        margin-bottom: 4px;
      }
      .calendar-month-label {
        position: absolute;
        top: 4px;
        left: 6px;
        font-size: 10px;
        font-weight: bold;
        color: #14110F;
        background-color: #749FD2;
        padding: 2px 6px;
        border-radius: 4px;
        text-transform: uppercase;
        z-index: 10;
      }
      .calendar-match {
        background-color: #2A2723;
        border-left: 3px solid #16549b;
        border-radius: 4px;
        margin-bottom: 4px;
        padding: 4px 6px;
        font-size: 11px;
        display: flex;
        justify-content: space-between;
        color: #A0A0A0;
      }
      .calendar-match.real-result {
        border-left-color: #0F79F2;
        background-color: #202020;
        color: #F3F3F4;
      }
      .match-teams { font-weight: bold; }
      .match-score { font-family: monospace; }
      .real-result .match-score { color: #749FD2; font-weight: bold; }
      
      /* Knockout Bracket CSS */
      .bracket-container {
        display: flex;
        height: calc(100vh - 130px);
        background-color: #202020;
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
        border: 1px solid #ACCAF1 !important;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05);
      }
      body.light-mode .sidebar {
        background-color: #ACCAF1 !important;
        border-right: 1px solid #ACCAF1 !important;
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
      body.light-mode .nav-underline .nav-link { color: #202020; }
      body.light-mode .nav-underline .nav-link.active { color: #0F62F2 !important; border-bottom-color: #0F62F2 !important; }
      body.light-mode .accuracy-sub, body.light-mode .pred-score { color: #14110F !important; }
      body.light-mode p { color: #14110F !important; }
      body.light-mode .card-header {
        background-color: #FFFFFF !important;
        border-bottom: 1px solid #14110F !important;
      }
      body.light-mode .match-row { color: #14110F !important; }
      
      /* Leaflet Popup styling to match premium dark theme */
      .leaflet-popup-content-wrapper {
        background-color: #14110F !important;
        color: #D9C5B2 !important;
        border: 1px solid #7E7F83 !important;
        border-radius: 12px !important;
        padding: 0px !important;
        box-shadow: 0 10px 30px rgba(0,0,0,0.5) !important;
      }
      .leaflet-popup-content {
        margin: 0 !important;
        padding: 14px 16px !important;
        font-family: 'Outfit', 'Inter', sans-serif !important;
      }
      .leaflet-popup-tip {
        background-color: #14110F !important;
        border: 1px solid #7E7F83 !important;
      }
      
      /* Custom inner elements for popup */
      .city-popup-header {
        border-bottom: 1px solid #2D2A28;
        padding-bottom: 8px;
        margin-bottom: 10px;
      }
      .city-popup-title {
        font-size: 16px;
        font-weight: 700;
        color: #F3F3F4;
        margin: 0;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .city-popup-country {
        font-size: 10px;
        text-transform: uppercase;
        color: #749FD2;
        letter-spacing: 1px;
      }
      .city-popup-venue {
        font-size: 11px;
        color: #A0A0A0;
        margin-top: 3px;
        display: flex;
        align-items: center;
      }
      .city-popup-venue-icon {
        margin-right: 4px;
        color: #0F79F2;
      }
      .city-popup-matches-title {
        font-size: 12px;
        font-weight: 600;
        color: #D9C5B2;
        margin-bottom: 6px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .city-popup-match-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 6px 0;
        border-bottom: 1px dashed #2D2A28;
        font-size: 11px;
      }
      .city-popup-match-row:last-child {
        border-bottom: none;
      }
      .city-popup-match-stage {
        color: #A0A0A0;
        font-size: 9px;
        background-color: #201C1A;
        padding: 2px 6px;
        border-radius: 4px;
        margin-right: 6px;
        white-space: nowrap;
      }
      .city-popup-match-teams {
        color: #F3F3F4;
        font-weight: 500;
        flex-grow: 1;
        line-height: 1.2;
      }
      .city-popup-match-score {
        font-family: monospace;
        font-weight: 700;
        margin-left: 10px;
        padding: 2px 6px;
        border-radius: 4px;
        white-space: nowrap;
      }
      .city-popup-score-real {
        background-color: rgba(217, 197, 178, 0.15) !important;
        color: #D9C5B2 !important;
        border: 1px solid rgba(217, 197, 178, 0.3) !important;
      }
      .city-popup-score-pred {
        background-color: rgba(15, 121, 242, 0.15) !important;
        color: #749FD2 !important;
        border: 1px solid rgba(15, 121, 242, 0.3) !important;
      }
      .city-popup-score-tbd {
        background-color: #201C1A !important;
        color: #7E7F83 !important;
      }
      .city-popup-no-matches {
        color: #A0A0A0;
        font-size: 11px;
        font-style: italic;
        padding: 4px 0;
      }
      .city-popup-match-time {
        font-size: 9px;
        color: #7E7F83;
        margin-top: 2px;
        display: block;
      }
      
      /* Light Mode overrides for Leaflet Popups */
      body.light-mode .leaflet-popup-content-wrapper {
        background-color: #FFFFFF !important;
        color: #14110F !important;
        border: 1px solid #ACCAF1 !important;
        box-shadow: 0 10px 30px rgba(0,0,0,0.15) !important;
      }
      body.light-mode .leaflet-popup-tip {
        background-color: #FFFFFF !important;
        border: 1px solid #ACCAF1 !important;
      }
      body.light-mode .city-popup-header {
        border-bottom: 1px solid #E2E8F0;
      }
      body.light-mode .city-popup-title span {
        color: #14110F;
      }
      body.light-mode .city-popup-matches-title {
        color: #14110F;
      }
      body.light-mode .city-popup-match-row {
        border-bottom: 1px dashed #E2E8F0;
      }
      body.light-mode .city-popup-match-stage {
        background-color: #F3F3F4;
        color: #4A5568;
      }
      body.light-mode .city-popup-match-teams {
        color: #14110F;
      }
      body.light-mode .city-popup-score-real {
        background-color: rgba(217, 197, 178, 0.25) !important;
        color: #7E5F43 !important;
        border: 1px solid rgba(217, 197, 178, 0.5) !important;
      }
      body.light-mode .city-popup-score-pred {
        background-color: rgba(15, 98, 242, 0.08) !important;
        color: #0F62F2 !important;
        border: 1px solid rgba(15, 98, 242, 0.2) !important;
      }
      body.light-mode .city-popup-score-tbd {
        background-color: #F3F3F4 !important;
        color: #7E7F83 !important;
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
                column(4, selectInput("editor_dataset", "Select Dataset to Edit:", choices = c("Actual Results", "Predictions"))),
                column(4, actionButton("save_editor", "Save Changes to File", class="btn btn-primary", style="margin-top: 32px; background-color: #16549b; color: #F3F3F4; border: none; font-weight: bold;"))
            ),
            DTOutput("editor_table")
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
    d_real <- rv$real_data %>% filter(!is.na(Goals1) & !is.na(Goals2))
    
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
    joined <- rv$real_data %>% 
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
        legend = list(orientation = "h", x = 0.5, y = 1.1, xanchor = "center", font = list(color = cols$text))
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
    cols <- tc()
    req(input$radar_team)
    team <- input$radar_team
    preds <- current_preds()
    if(is.null(preds)) preds <- rv$real_data[0,]
    
    d <- get_radar_data(team, rv$real_data, preds)
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
      joined <- rv$real_data %>% 
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
      joined <- rv$real_data %>% 
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
  editor_data <- reactive({
    if (input$editor_dataset == "Actual Results") {
      rv$real_data
    } else {
      rv$ml_preds
    }
  })
  
  output$editor_table <- renderDT({
    # Re-render only when the selected dataset changes, not when cell values are edited in place
    input$editor_dataset
    
    d <- isolate({
      editor_data() %>%
        mutate(Kickoff = if_else(is.na(Kickoff_Local) | Kickoff_Local == "", "", paste0(Kickoff_Local, " (", Kickoff_TZ, ")"))) %>%
        select(MatchID, Kickoff, Group, Team1, Goals1, Goals2, Team2)
    })
    datatable(d, 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 6))),
              options = list(paging = FALSE, scrollX = TRUE, scrollY = "600px", dom = 't'),
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
    # Columns in d_display: MatchID (1), Kickoff (2), Group (3), Team1 (4), Goals1 (5), Goals2 (6), Team2 (7)
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
      mutate(Kickoff = if_else(is.na(Kickoff_Local) | Kickoff_Local == "", "", paste0(Kickoff_Local, " (", Kickoff_TZ, ")"))) %>%
      select(MatchID, Kickoff, Group, Team1, Goals1, Goals2, Team2)
    replaceData(proxy, d_display, resetPaging = FALSE, rownames = FALSE)
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
  
  output$calendar_ui <- renderUI({
    d_real <- rv$real_data
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
