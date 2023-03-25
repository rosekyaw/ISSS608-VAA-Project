pacman::p_load(sf, tmap, shiny, shinythemes, plotly, tidyverse, ggstatsplot, tools)

wp_nga <- read_rds("data/wp_nga.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Overview"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        # Water Point Status
        selectInput(inputId = "status",
                    label = "Water Point Status :",
                    choices = c("Functional Water Points" = "pct_functional",
                                "Non-Functional Water Points" = "pct_nonfunctional",
                                "Unknown Water Points" = "pct_unknown"),
                    selected = "Functional Water Points"),
        
        # Another Parameter
        selectInput(inputId = "parameter",
                    label = "Another Parameter :",
                    choices = c("Nil",
                                "Water Source",
                                "Technology",
                                "Pressure Score",
                                "Crucial Score",
                                "Capacity"),
                    selected = "Nil"),
        
        # Only show this panel if the parameter is Water Source
        conditionalPanel(
          condition = "input.parameter == 'Water Source'",
          selectInput(inputId = "param1",
                      label = "Water Source :",
                      choices = c("Spring" = "pct_spring",
                                  "Well" = "pct_well")
                      )),  
        # Only show this panel if the parameter is Technology
        conditionalPanel(
          condition = "input.parameter == 'Technology'",
          selectInput(inputId = "param2",
                      label = "Technology:",
                      choices = c("Hand Pump" = "pct_handpump",
                                  "Mechanized Pump" = "pct_mechpump")
          )),

        # Only show this panel if the parameter is Pressure Score
        conditionalPanel(
          condition = "input.parameter == 'Pressure Score'",
          selectInput(inputId = "param3",
                      label = "Pressure Score :",
                      choices = c("Acceptable Pressure Score" = "pct_withinpressure",
                                  "High Pressure Score" = "pct_highpressure")
          )),

        # Only show this panel if the parameter is Crucial Score
        conditionalPanel(
          condition = "input.parameter == 'Crucial Score'",
          selectInput(inputId = "param4",
                      label = "Crucial Score :",
                      choices = c("Low Crucial Score" = "pct_lowcrucial",
                                  "High Crucial Score" = "pct_highcrucial")
          )),

        # Only show this panel if the parameter is Capacity
        conditionalPanel(
          condition = "input.parameter == 'Capacity'",
          selectInput(inputId = "param5",
                      label = "Capacity :",
                      choices = c("Low Capacity" = "pct_lowcap",
                                  "High Capacity" = "pct_highcap")
          )),
      ),
        # Show a plot of the generated distribution
        mainPanel(
          tmapOutput(outputId = "ngaMap",
                     width = "100%", 
                     height = 400))
    )
)


# Define server logic required to draw map
server <- function(input, output) {
  output$ngaMap <- renderTmap({
    
    tmap_options(check.and.fix = TRUE) +
      tm_shape(wp_nga) +
      tm_fill(input$status,
              n = 5,
              style = "pretty",
              palette = "YlOrBr") +
              (if('Nil' %in% input$parameter) tm_bubbles(col= 'white', size = 0)
               else tm_bubbles(
                  (if('Water Source' %in% input$parameter) col = input$param1
                  else if('Technology' %in% input$parameter) col = input$param2
                  else if('Pressure Score' %in% input$parameter) col = input$param3
                  else if('Crucial Score' %in% input$parameter) col = input$param4
                  else if('Capacity' %in% input$parameter) col = input$param5),
                 (if('Water Source' %in% input$parameter) size = input$param1
                  else if('Technology' %in% input$parameter) size = input$param2
                  else if('Pressure Score' %in% input$parameter) size = input$param3
                  else if('Crucial Score' %in% input$parameter) size = input$param4
                  else if('Capacity' %in% input$parameter) size = input$param5),
                 n = 5,
                 alpha = 0.7,
                 scale = 0.5,
                 border.col = "black",
                 border.lwd = 0.1,
                 palette = "Dark2")) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(5.5, 10.5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
