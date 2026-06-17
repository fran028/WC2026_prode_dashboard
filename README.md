# World Cup 2026 Prediction Dashboard

An interactive Shiny dashboard to analyze, visualize, and compare predictions against actual results for the **FIFA World Cup 2026**. 

## Features

- **Interactive Map**: A geographic overview of host cities. Clicking on a city displays a premium styled popup with stadium venue information and a chronological table of matches showing kickoff times, stages, and color-coded scores (beige for actual results, blue for predictions, and grey for TBD).
- **Interactive Calendar**: A continuous 7-week calendar grid covering June and July 2026. Daily cell backgrounds scale to a brighter blue based on the tournament stage (Group Stage to the Final). Unplayed knockout slots automatically show the stage category until teams are decided, and played/predicted matches are clearly distinguished.
- **Data Editor**: Seamlessly edit your Predictions and Actual Results directly within the application. Changes are saved back to your local CSV files instantly and update the UI reactively.
- **Prediction Comparison**: Upload your own prediction CSV files or use the built-in editor to compare them against the actual results. The app automatically calculates an accuracy score based on correct match outcomes.
- **Dynamic Data Visualizations**: 
  - **Radar Chart**: Compare team categories with beautifully overlaid Actual vs Predicted webs.
  - **Scatter Plot**: Grouped bird's-eye view of Goals For (GF) vs Goals Against (GA). Markers with identical values are collapsed to a single dot with a color gradient representing team count density. Hover tooltips display bulleted team lists for that group.
  - **Top Scorers Bar Chart**: Displays the top 10 teams using a dynamic blue color gradient that scales from the 10th-place team to the top scorer.
  - **Timeline Trend**: Track tournament goal pacing stage-by-stage with abbreviated, diagonally rotated labels (e.g. `GRd 1`, `R32`, `QF`) for clean screen layout.
- **Group Standings**: Automatically calculated group stage tables based on real or predicted results. The tables are strictly responsive and dynamically scale to fit the dashboard grid perfectly without overflowing.
- **Knockout Bracket**: A fully symmetric, two-sided visualization of the knockout stages, from the Round of 32 down to the Final and Third-Place Playoff.
- **Premium UI Mechanics**: Customized WebKit scrollbars embedded globally across data tables and widget cards to seamlessly match the sleek dark theme.
- **Modular Codebase**: Clean, efficient architecture. Data preparation logic and helper functions are modularized into `R/helpers.R` while custom dark/light theme CSS properties are isolated in `www/style.css` for easy maintenance.
## Prerequisites

To run this dashboard locally, you need [R](https://cran.r-project.org/) installed. We also highly recommend using [RStudio](https://posit.co/download/rstudio-desktop/).

You will need the following R packages installed:
```R
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "stringr", "leaflet", "DT", "htmltools", "plotly", "shinyjs"))
```

## How to Download and Run

1. **Clone the repository**:
   Open your terminal or command prompt and run:
   ```bash
   git clone https://github.com/fran028/WC2026_prode_dashboard.git
   ```
2. **Navigate to the project directory**:
   ```bash
   cd WC2026_prode_dashboard
   ```
3. **Run the App**:
   * **Using RStudio**: Open the `world_cup_2026_pre_analysis.Rproj` file. Then, open `app.R` and click the **"Run App"** button in the top right corner of the editor.
   * **Using the Command Line**: Run the following command in your terminal:
     ```bash
     Rscript -e "shiny::runApp()"
     ```

## How to Use

1. **Explore the Group Stage**: When you launch the app, you will land on the Group Stage Dashboard. Select different teams from the dropdown in the Radar chart to analyze their performance stats.
2. **View the Calendar**: Click on the **"Calendar"** tab to view the tournament matches mapped to a continuous 7-week grid.
3. **View the Bracket**: Click on the **"Knockout Bracket"** tab at the top to see the tournament tree and progression.
4. **Edit Data Live**: Head to the **"Data Editor"** tab to modify Actuals and Predictions on the fly.
5. **Upload Predictions**: On the left sidebar, click **"Browse..."** under *Load Prediction* to upload your own CSV predictions. The dashboard will instantly update all charts, tables, and accuracy scores to compare your file against the actual results!