# World Cup 2026 Prediction Dashboard

An interactive Shiny dashboard to analyze, visualize, and compare predictions against actual results for the **FIFA World Cup 2026**. 

## Features

- **Interactive Map**: A geographic overview of host cities, displaying which matches are played at each location.
- **Prediction Comparison**: Upload your own prediction CSV files to compare them against the actual results. The app automatically calculates an accuracy score based on correct match outcomes (Win/Draw/Loss).
- **Radar & Scatter Charts**: Visually compare average Goals For, Goals Against, and Points for each team.
- **Group Standings**: Automatically calculated group stage tables based on real or predicted results.
- **Knockout Bracket**: A fully symmetric, two-sided visualization of the knockout stages, from the Round of 32 down to the Final and Third-Place Playoff.

## Prerequisites

To run this dashboard locally, you need [R](https://cran.r-project.org/) installed. We also highly recommend using [RStudio](https://posit.co/download/rstudio-desktop/).

You will need the following R packages installed:
```R
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "stringr", "leaflet", "DT", "htmltools", "plotly"))
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

1. **Explore the Group Stage**: When you launch the app, you will land on the Group Stage Dashboard. Click on map markers to see the matches hosted in each city. Select different teams from the dropdown in the Radar chart to analyze their performance stats.
2. **View the Bracket**: Click on the **"Knockout Bracket"** tab at the top to see the tournament tree and progression.
3. **Upload Predictions**: On the left sidebar, click **"Browse..."** under *Load Prediction* to upload your own CSV predictions. The dashboard will instantly update all charts, tables, and accuracy scores to compare your file against the actual results!