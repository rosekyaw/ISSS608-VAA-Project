pacman::p_load(sf, tmap, shiny, shinythemes, plotly, tidyverse, ggstatsplot, tools)

wp_ngaTrim <- read_rds("data/geodata/wp_ngaTrim.rds")


ui <- fluidPage(
    titlePanel("Dashboard"),
    sidebarLayout(
      sidebarPanel(
        #selectInput(inputId = "inputLGA",
        #            label = "District :",
        #            choices = sort(wp_ngaTrim$shapeName),
        #            multiple = TRUE,
        #            selected = "Aba North"),
        
        selectInput(inputId = "attribute",
                    label = "Attribute :",
                    choices = c("Functional Water Points" = "pct_functional",
                                "Non-Functional Water Points" = "pct_nonFunctional",
                                "Unknown Water Points" = "pct_unknown"),
                    selected = "Functional Water Points"),
      
        selectInput(inputId = "classification",
                    label = "Classification method:",
                    choices = list("sd" = "sd", 
                                   "equal" = "equal", 
                                   "pretty" = "pretty", 
                                   "quantile" = "quantile", 
                                   "kmeans" = "kmeans", 
                                   "hclust" = "hclust", 
                                   "bclust" = "bclust", 
                                   "fisher" = "fisher", 
                                   "jenks" = "jenks"),
                    selected = "pretty"),
      
        sliderInput(inputId = "classes",
                    label = "Number of classes",
                    min = 5,
                    max = 12,
                    value = c(5)),
      
        selectInput(inputId = "colour",
                    label = "Colour scheme:",
                    choices = list("blues" = "Blues", 
                                   "reds" = "Reds", 
                                   "greens" = "Greens",
                                   "Yellow-Orange-Red" = "YlOrRd",
                                   "Yellow-Orange-Brown" = "YlOrBr",
                                   "Yellow-Green" = "YlGn",
                                   "Orange-Red" = "OrRd"),
                    selected = "YlOrRd")),
      mainPanel(
        tmapOutput(outputId = "ngaMap",
                   width = "100%", 
                   height = 400))
    ))

server <- function(input, output) {
  
  output$ngaMap <- renderTmap({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(wp_ngaTrim) +
      tm_fill(input$attribute,
              n = input$classes,
              style = input$classification,
              palette = input$colour) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(6, 8)
      )
    })
  }

shinyApp(ui = ui, server = server)
