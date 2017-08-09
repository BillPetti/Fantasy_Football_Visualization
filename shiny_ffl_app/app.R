
library(dplyr)
library(readr)
library(shiny)
library(ggplot2)
library(DT)
library(feather)
library(shinyURL)

res <- read_csv("data/FF_2017_draft_data_shiny.csv")

inputs <- data_frame(input_values = c("Games (2016)", "2016 Consistency", "Average Fantasy Points Per Week (2016)", "2016 Normalized Consistency", "Normalized Consistency (2013-16)", "Projected Fantasy Points", "Projected Points Per Week", "Average Draft Position (relative to field position)"), var_names = c("Games_2016", "x2016_Consistency", "Average_Fantasy_Points_Per_Week_2016", "x2016_Normalized_Consistency", "Normalized_Consistency_2013_16", "Projected_Fantasy_Points", "Projected_Points_Per_Week", "Average_Draft_Position_relative_to_field_position"))

inputs_list <- with(inputs, split(var_names, input_values))

#res <- read_feather("/Users/williampetti/Box Sync/Default Sync Folder (billpetti@gmail.com)/Baseball Data/Baseball Data/Spray Chart Tool/spray_chart/shiny_spraychart/data/shiny_spray_master")

# tooltip code modified from https://gitlab.com/snippets/16220

source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/tableau_colorblind_palette")

theme_minimal_bp <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 12),
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(face = "bold", size = 14)
    )
}

ui <- fluidPage(
  titlePanel("Fantasy Football Tool"),
  fluidRow(
    column(12,
           p("Draft and Point Projections are current as of 2017-08-08"),
           p("Size of bubble scaled to Projected Fantasy Points for 2017. Projected Points for RB, WR, and TE assume .5 points per reception"),
           sidebarLayout(
             sidebarPanel(
               selectizeInput("player_type", "Select Positions",
                              choices = c("QB", "RB", "WR", "TE", "K"),
                              selected = c("QB", "RB", "WR", "TE", "K"), 
                              multiple = TRUE),
               numericInput("games_filter", "Select a minimum number of games played in 2016",
                              value = 8),
               numericInput("points_filter", "Select a minimum number of projected points for 2017",
                            value = 80),
               selectizeInput("axis_x", "Select Metric for X-Axis",
                              choices = inputs_list,
                              multiple = FALSE),
               selectizeInput("axis_y", "Select Metric for Y-Axis",
                              choices = inputs_list,
                              multiple = FALSE), 
               downloadButton('downloadData', 'Download Tabular Data'),
               
               width = 3
             ),
             mainPanel(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               tabsetPanel(tabPanel("Player Plotting", 
                                    div(style = "position:relative", plotOutput(outputId = "coolplot_players", hover = hoverOpts("plot_hover_players", delay = 100, delayType = "debounce")), 
                                        uiOutput("hover_info_players"))),
                                    tabPanel("About", 
                                             br(), h1("About this tool"), 
                                             p("This tool is maintained by Bill Petti"), br(), h2("Definitions"), p("2016 Consistency: Lower score = more consistent on a game to game basis. Consistency was calculated using Gini coefficients across a player's 2016 games."), br(), p("2016 Normalized Consistency: 2016 Consistency scores were normalized by season and position using z-scores; so, how many standard deviations above or below the league average on a per-player basis. Again, lower scores = more consistent."), br(), p("Normalized Consistency (2013-2016): Average seasonal Consistency from 2013-2016 was calcualted and then normalized by season and position using z-scores; so, how many standard deviations above or below the league average ona per-player basis. Again, lower scores = more consistent."), br(), p("Special thanks to Dennis Erny (http://www.armchairanalysis.com) for the game-by-game data and to FantasyPros.com for the projected points and draft data."), br(), h2("Links"), 
a("My website", href = "https://billpetti.github.io"), br(),
a("Find me on Twitter", href = "https://twitter.com/BillPetti")
                                    )))))))

server <- function(input, output) {
  
  # filter players
  
  filtered <- reactive({
    
    filtered <- res %>%
      filter(Position %in% input$player_type) %>%
      filter(Games_2016 >= input$games_filter) %>%
      filter(Projected_Fantasy_Points >= input$points_filter)
    
    filtered
  })
  
  # build plot
  
  output$coolplot_players <- renderPlot({
    
    outcome_palette <- c("QB" = "#006BA4", "RB" = "#A2CEEC", "WR" = "#FFBC79", "TE" = "#C85200", "K" = "#595959") 
    
    g <- ggplot(filtered(), aes_string(x = input$axis_x, y = input$axis_y)) +
      geom_point(aes(fill = Position, size = Projected_Fantasy_Points), alpha = .75, shape = 21, stroke = .5) +
      scale_size_continuous(range = c(4,12)) +
      scale_color_manual(values = outcome_palette, "Positions") + 
      scale_fill_manual(values = outcome_palette, guide = FALSE) + 
      labs(caption = "Designed and maintained by Bill Petti\nbillpetti.github.io\n") +
      guides(size = FALSE) +
      theme_minimal_bp() + 
      theme(legend.position = "bottom", 
            legend.text = element_text(size = 17),
            legend.title = element_text(size = 17), 
            axis.title = element_blank(),
            panel.background = element_blank())
            #panel.background = element_rect(fill="#F0F0F0", color=NA))
    
    return(g)
  }, height = 525, width = 1000)
  
  output$hover_info_players <- renderUI({
    hover <- input$plot_hover_players
    point <- nearPoints(filtered(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Player: </b>", point$Player, "<br/>",
                    "<b> Position: </b>", point$Position, "<br/>",
                    "<b> Projected Fantasy Points: </b>", point$Projected_Fantasy_Points, "<br/>",
                    "<b> Projected Points Per Week: </b>", point$Projected_Points_Per_Week, "<br/>",
                    "<b> Average Fantasy Points Per Week (2016): </b>", point$Average_Fantasy_Points_Per_Week_2016, "<br/>",
                    "<b> 2016 Consistency: </b>", point$x2016_Consistency, "<br/>",
                    "<b> 2016 Normalized Consistency: </b>", point$x2016_Normalized_Consistency, "<br/>",
                    "<b> Normalized Consistency (2013-16): </b>", point$Normalized_Consistency_2013_16, "<br/>"
      ))))
  })
 
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
   
}

shinyApp(ui = ui, server = server)
