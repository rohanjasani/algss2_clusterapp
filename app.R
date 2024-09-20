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

# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

file_path <- "data/ALGS_Split_2.csv"
data <- read.csv(file_path)

# Rename columns for clarity and select relevant features
colnames(data) <- c('Player', 'Games', 'Kills', 'Assists', 'Knocks', 
                    'Damage_Dealt', 'Damage_Taken', 'Damage_Difference', 
                    'Ring_Damage', 'Revives', 'Respawns', 'K_D', 'KA_D', 'Survival_Time')

# Convert specific columns to integers (keep Damage_Difference as numeric to allow negative values)
data <- data %>%
    mutate(
        Games = as.integer(Games),
        Kills = as.integer(Kills),
        Assists = as.integer(Assists),
        Knocks = as.integer(Knocks),
        Damage_Dealt = as.integer(Damage_Dealt),
        Damage_Taken = as.integer(Damage_Taken),
        Damage_Difference_per_Game = round(Damage_Difference / Games, 2)  # New column for Damage Difference per Game
    )

# Predefined color palette for clusters
color_mapping <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", 
                   "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5")

# Define the UI for the application
ui <- dashboardPage(
    dashboardHeader(title = "ALGS Split 2 Playoff Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("info-circle"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(width = 3,
                            radioButtons("plot_choice", "Select Plot:",
                                         choices = c("Kills vs Damage Dealt", "K+A/D vs Damage Difference per Game"),
                                         selected = "Kills vs Damage Dealt"),
                            uiOutput("dynamic_sliders"),
                            sliderTextInput("clusters", "Number of clusters:", 
                                            choices = seq(1, 15), selected = 5),
                            materialSwitch("per_game", "Cluster on Per Game Basis", FALSE),
                            uiOutput("selectedClusterUI")
                        ),
                        box(width = 9,
                            plotlyOutput("clusterPlot", height = "600px")
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            DTOutput("playerTable")
                        )
                    )
            ),
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12,
                            title = "About This Dashboard",
                            "This dashboard visualizes player performance data from the ALGS Year 4 Split 2 Playoff using K-means clustering. ",
                            "It allows users to explore relationships between different performance metrics and group players based on their statistics.",
                            tags$br(), tags$br(),
                            "Use the controls on the left to adjust the visualization and clustering parameters.",
                            tags$br(), tags$br(),
                            "Data source: Liquipedia"
                        )
                    )
            )
        )
    )
)

# Define the server logic
server <- function(input, output, session) {
    # Reactive expression for slider ranges
    slider_ranges <- reactive({
        list(
            games = c(min(data$Games), max(data$Games)),
            kills = c(1, max(data$Kills)),
            damage_dealt = c(0, max(data$Damage_Dealt)),
            ka_d = c(0, max(data$KA_D)),
            damage_diff = c(min(data$Damage_Difference_per_Game), max(data$Damage_Difference_per_Game))
        )
    })
    
    # Dynamic UI for sliders
    output$dynamic_sliders <- renderUI({
        ranges <- slider_ranges()
        if (input$plot_choice == "Kills vs Damage Dealt") {
            list(
                sliderInput("games", "Games Played:", min = ranges$games[1], max = ranges$games[2], 
                            value = c(24, ranges$games[2]), step = 1),
                sliderInput("kills", "Kills:", min = ranges$kills[1], max = ranges$kills[2], 
                            value = c(20, ranges$kills[2]), step = 1),
                sliderInput("damage_dealt", "Damage Dealt:", min = ranges$damage_dealt[1], max = ranges$damage_dealt[2], 
                            value = c(0, ranges$damage_dealt[2]), step = 1)
            )
        } else {
            list(
                sliderInput("ka_d", "K+A/D:", min = ranges$ka_d[1], max = ranges$ka_d[2], 
                            value = c(2.0, ranges$ka_d[2]), step = 0.01),
                sliderInput("damage_diff", "Damage Difference per Game:", 
                            min = ranges$damage_diff[1], max = ranges$damage_diff[2], 
                            value = c(0, ranges$damage_diff[2]), step = 1)
            )
        }
    })
    
    # Reactive expression for filtered data
    filtered_data <- reactive({
        req(input$plot_choice)
        if (input$plot_choice == "Kills vs Damage Dealt") {
            req(input$kills, input$games, input$damage_dealt)
            data %>%
                filter(
                    Kills >= input$kills[1] & Kills <= input$kills[2],
                    Damage_Dealt >= input$damage_dealt[1] & Damage_Dealt <= input$damage_dealt[2],
                    Games >= input$games[1] & Games <= input$games[2]
                ) %>%
                mutate(
                    Kills_per_Game = round(Kills / Games, 2),
                    Norm_Kills = Kills / max(Kills),
                    Norm_Damage_Dealt = Damage_Dealt / max(Damage_Dealt)
                )
        } else {
            req(input$ka_d, input$damage_diff)
            data %>%
                filter(
                    KA_D >= input$ka_d[1] & KA_D <= input$ka_d[2],
                    Damage_Difference_per_Game >= input$damage_diff[1] & Damage_Difference_per_Game <= input$damage_diff[2]
                ) %>%
                mutate(
                    Norm_KA_D = KA_D / max(KA_D),
                    Norm_Damage_Difference_per_Game = Damage_Difference_per_Game / max(abs(Damage_Difference_per_Game))
                )
        }
    })
    
    # Reactive expression for custom clustering
    clustered_data <- reactive({
        req(filtered_data())
        
        if (input$plot_choice == "Kills vs Damage Dealt") {
            features <- select(filtered_data(), Kills, Damage_Dealt)
        } else {
            features <- select(filtered_data(), KA_D, Damage_Difference_per_Game)
        }
        
        # Normalize features
        normalized_features <- scale(features)
        
        # Calculate distances from origin for each player
        distances <- sqrt(rowSums(normalized_features^2))
        
        # Perform k-means clustering
        km <- kmeans(normalized_features, centers = as.numeric(input$clusters))
        
        # Create a data frame with player info, distances, and initial cluster
        player_data <- data.frame(
            Player = filtered_data()$Player,
            Distance = distances,
            InitialCluster = km$cluster
        )
        
        # Order players by distance (descending)
        player_data <- player_data[order(-player_data$Distance), ]
        
        # Initialize new cluster assignments
        new_clusters <- rep(NA, nrow(player_data))
        
        # Assign new cluster numbers
        next_cluster <- 1
        for (i in 1:nrow(player_data)) {
            if (is.na(new_clusters[i])) {
                new_clusters[i] <- next_cluster
                current_cluster <- player_data$InitialCluster[i]
                new_clusters[player_data$InitialCluster == current_cluster] <- next_cluster
                next_cluster <- next_cluster + 1
            }
        }
        
        # Add new cluster assignments to filtered data
        result <- filtered_data() %>%
            mutate(Cluster = factor(new_clusters[match(Player, player_data$Player)]))
        
        return(result)
    })
    
    # Update the selected cluster input dynamically
    output$selectedClusterUI <- renderUI({
        selectInput("selected_cluster", 
                    "Selected Cluster for Tabulation:", 
                    choices = c("All", levels(clustered_data()$Cluster)), 
                    selected = "All")
    })
    
    # Plot the clustered data
    output$clusterPlot <- renderPlotly({
        plot_data <- clustered_data()
        x_var <- if (input$plot_choice == "Kills vs Damage Dealt") {
            if (input$per_game) quote(Kills / Games) else quote(Kills)
        } else quote(KA_D)
        y_var <- if (input$plot_choice == "Kills vs Damage Dealt") {
            if (input$per_game) quote(Damage_Dealt / Games) else quote(Damage_Dealt)
        } else quote(Damage_Difference_per_Game)
        
        p <- ggplot(plot_data, aes(x = !!x_var, y = !!y_var, color = Cluster, 
                                   text = paste("Player:", Player, "<br>",
                                                "Cluster:", Cluster, "<br>",
                                                deparse(x_var), ":", round(!!x_var, 2), "<br>",
                                                deparse(y_var), ":", round(!!y_var, 2)))) +
            geom_point(size = 3, alpha = 0.7) +
            scale_color_manual(values = color_mapping[1:input$clusters]) +
            labs(title = paste("Custom Clustering:", input$plot_choice), 
                 x = gsub("_", " ", deparse(x_var)), 
                 y = gsub("_", " ", deparse(y_var))) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10),
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 10)
            )
        
        ggplotly(p, tooltip = "text") %>%
            layout(legend = list(x = 1.05, y = 0.5),  # Move legend outside the plot
                   margin = list(r = 150))  # Add right margin for legend
    })
    
    # Display the table of players
    output$playerTable <- renderDT({
        req(input$selected_cluster)
        
        select_fields <- if (input$plot_choice == "Kills vs Damage Dealt") {
            c("Player", "Games", "Kills", "Assists", "Knocks", "Damage_Dealt", "Damage_Taken", "Damage_Difference", "Damage_Difference_per_Game", "K_D", "KA_D")
        } else {
            c("Player", "KA_D", "Damage_Difference_per_Game", "Kills", "Assists", "Knocks", "Damage_Dealt", "Damage_Taken")
        }
        
        table_data <- if (input$selected_cluster == "All") {
            clustered_data() %>% arrange(Player)
        } else {
            clustered_data() %>% filter(Cluster == input$selected_cluster) %>% arrange(desc(Kills))
        }
        
        datatable(table_data %>% select(all_of(select_fields)),
                  options = list(pageLength = 10, 
                                 lengthMenu = c(10, 25, 50), 
                                 scrollX = TRUE,
                                 dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                  extensions = 'Buttons',
                  rownames = FALSE) %>%
            formatStyle(columns = select_fields, 
                        backgroundColor = styleEqual(levels = levels(clustered_data()$Cluster), 
                                                     values = adjustcolor(color_mapping[1:input$clusters], alpha.f = 0.2)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
