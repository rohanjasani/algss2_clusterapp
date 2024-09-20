# ALGS Split 2 Playoff Dashboard

## Description

This Shiny app creates an interactive dashboard for visualizing and analyzing player performance data from the Apex Legends Global Series (ALGS) Year 4 Split 2 Playoff. It uses K-means clustering to group players based on various performance metrics.

## Setup and Running the App

1. Ensure you have R installed on your system.
2. Clone this repository or download the `app.R` file.
3. Place the `ALGS_Split_2.csv` file in a `data` subdirectory.
4. Open R or RStudio and set your working directory to where `app.R` is located.
5. Run the entire code in the `app.R` file.

The app includes a package management section at the beginning:

```r
# Set a default CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of required packages
required_packages <- c("dplyr", "ggplot2", "shiny", "plotly", "DT", "shinythemes", "shinydashboard", "shinyWidgets")

# Function to check and install missing packages
install_if_missing <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
}

# Install missing packages
sapply(required_packages, install_if_missing)
```

This code does the following:
- Sets a default CRAN mirror for package downloads.
- Defines a list of required packages for the app.
- Creates a function `install_if_missing` that checks if a package is installed and installs it if it's not.
- Uses `sapply` to apply this function to all required packages, ensuring they are installed.

By running the entire `app.R` file, you'll automatically install any missing packages and launch the Shiny app.

## Features

- Interactive scatter plots with options for "Kills vs Damage Dealt" and "K+A/D vs Damage Difference per Game"
- Adjustable K-means clustering (1-15 clusters)
- Data filtering using dynamic sliders
- Detailed player statistics table with export options
- Responsive design using Shiny Dashboard

## Usage

1. Plot Selection:
   - Use the radio buttons to choose between two plot types:
     - "Kills vs Damage Dealt": Compares player kills to damage dealt
     - "K+A/D vs Damage Difference per Game": Compares Kill+Assist/Death ratio to damage difference per game

2. Data Filtering:
   - Adjust the dynamic sliders to filter player data:
     - For "Kills vs Damage Dealt":
       - Games Played: Set minimum and maximum number of games
       - Kills: Set minimum and maximum number of kills
       - Damage Dealt: Set minimum and maximum damage dealt
     - For "K+A/D vs Damage Difference per Game":
       - K+A/D: Set minimum and maximum Kill+Assist/Death ratio
       - Damage Difference per Game: Set minimum and maximum damage difference

3. Clustering:
   - Use the "Number of clusters" slider to adjust the K-means clustering (1-15 clusters)
   - Toggle "Cluster on Per Game Basis" to normalize metrics by the number of games played

4. Visualization:
   - The scatter plot updates dynamically based on your selections
   - Each point represents a player, colored by their assigned cluster
   - Hover over points to see detailed player information:
     - Player name
     - Cluster number
     - X and Y axis values (depending on the selected plot type)

5. Player Statistics:
   - Use the "Selected Cluster for Tabulation" dropdown to view statistics for:
     - All players
     - Players in a specific cluster
   - The data table below the plot shows detailed player statistics
   - Use the search bar to find specific players
   - Click column headers to sort the data
   - Use pagination controls to navigate through the data
   - Export options (copy, CSV, Excel, PDF, print) are available above the table

6. About Page:
   - Click the "About" menu item in the sidebar to view information about the dashboard and data source

7. Interpreting the Results:
   - Players clustered together have similar performance characteristics
   - The number of clusters can significantly change the groupings, so experiment with different values
   - Consider how the "Cluster on Per Game Basis" toggle affects the results, especially for players with varying numbers of games played

8. Performance Considerations:
   - Adjusting filters and clustering parameters may take a moment to update, especially with larger datasets
   - If the app becomes unresponsive, try reducing the number of clusters or narrowing your data filters

Remember, this dashboard is a tool for exploration. Experiment with different combinations of filters and clustering parameters to uncover insights about player performance in the ALGS Split 2 Playoff.

## Data

The app uses `ALGS_Split_2.csv`, which should be placed in a `data` subdirectory. Ensure this file is present before running the app.

## About

This dashboard visualizes ALGS Year 4 Split 2 Playoff data, allowing users to explore performance metrics and group players using K-means clustering. Use the left-side controls to adjust visualization and clustering parameters.

Data source: [Apex Legends Global Series Year 4 Split 2 Playoffs tables from Liquipedia](https://liquipedia.net/apexlegends/Apex_Legends_Global_Series/2024/Split_2/Playoffs)

## Contact

Rohan Jasani
jasani.rohan@gmail.com