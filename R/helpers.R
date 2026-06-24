# --- Data Preparation ---
library(dplyr)
library(stringr)

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
