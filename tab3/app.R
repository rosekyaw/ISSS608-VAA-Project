pacman::p_load(sf, tmap, shiny, shinythemes, plotly, tidyverse, ggstatsplot, 
               tools, cluster, factoextra, heatmaply, stats)

wp_ngaTrim <- read_rds("data/geodata/wp_ngaTrim.rds")
stdMM <- read_rds("data/geodata/stdMM.rds")

ui <- fluidPage (
  tags$head(
    tags$style(type = "text/css",
               "body {
      background-color: #5c5c5c;
    }
    .sidebarPanel {
      margin-top: 30px;
    }"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "attribute",
                         label = "Attribute :",
                         choices = c("Functional Water Points" = "pct_functional",
                                     "Non-Functional Water Points" = "pct_nonFunctional",
                                     "Hand Pump Technology" = "pct_handPump",
                                     "Mechanical Pump Technology" = "pct_mechPump",
                                     "Tap Stand Technology" = "pct_tapStand",
                                     "Usage Capacity 1000" = "pct_uc1000",
                                     "Usage Capacity Under 1000" = "pct_ucN1000",
                                     "Urban Community" = "pct_urban1",
                                     "Non-urban Community" = "pct_urban0"),
                         selected = c("pct_functional", "pct_nonFunctional", "pct_handPump", "pct_uc1000", "pct_urban0")),
      
      radioButtons(inputId = "optKmethod",
                   label = "Method to determine Optimal k value :",
                   choices = c("Elbow Method", 
                               "Gap Statistics", 
                               "Silhouette Score"),
                   selected = "Elbow Method"),
      
      sliderInput(inputId = "clusterInput",
                  label = "Number of cluster",
                  min = 2,
                  max = 12,
                  value = 3),
      
      selectInput(inputId = "colour",
                  label = "Colour scheme:",
                  choices = list("Yellow-Orange-Brown" = "YlOrBr",
                                 "Yellow-Green" = "YlGn"),
                  selected = "YlOrBr"),
      submitButton("Update view", icon("refresh"))),
    
    mainPanel(
      plotOutput(outputId = "testinput", width = "100%", height = "500"),
      plotOutput(outputId = "optimalK", width = "100%", height = "200"),
      plotOutput(outputId = "ngaMap", width = "100%", height = "400"),
      #tmapOutput(outputId = "ngaMap", width = "100%", height = "400"),
      plotOutput(outputId = "heatmap", width = "100%", height = "800")
      ))
  )


server <- function(input, output) {
  wp_ngaTrimInput <- reactive({
    wp_ngaTrim %>%
      select(input$attribute) %>%
      na.omit() %>%
      mutate_all(as.numeric)
  })
  
  stdMMInput <- reactive({
    stdMM %>%
      select(input$attribute)
  })
    
  clusterVarsInput <- reactive({
    cluster_vars %>%
      select(input$attribute) %>%
      na.omit() %>%
      mutate_all(as.numeric)
  })
  
  output$testinput <- renderPlot({
    tmap_options(check.and.fix = TRUE) +
      tm_shape(wp_ngaTrim) +
      tm_fill(input$attribute,
              style = 'pretty',
              palette = input$colour) +
      tm_borders(lwd = 0.1, 
                 alpha = 1) +
      tm_view(set.zoom.limits = c(6, 8))
    })
  
  
  output$optimalK <- renderPlot({
    req(input$optKmethod, stdMMInput())
    if (input$optKmethod == "Elbow Method") {
      fviz_nbclust(stdMMInput(), kmeans, method = "wss", linecolor = "white") +
        theme_dark() + labs(subtitle = "Elbow method")}
    else if (input$optKmethod == "Gap Statistics") {
      fviz_nbclust(stdMMInput(), hcut, nstart = 25, method = "gap_stat", nboot = 50,
                   linecolor = "white") + theme_dark() + labs(subtitle = "Gap statistic method")}
    else {
      fviz_nbclust(stdMMInput(), kmeans, method = "silhouette", linecolor = "white") + 
        theme_dark() + labs(subtitle = "Silhouette method")}
    })
  
  
  #proxMatrix <- reactive({dist(wp_ngaTrimInput, method = 'euclidean')
  #  })
  #groups <- reactive({as.factor(cutree((hclust(proxMatrix, method = 'ward.D')),
  #                                    k = input$clusterInput))
  #  })
  #ngaClust <- reactive({cbind(wp_ngaTrim,
  #                            as.matrix(groups)) %>%
  #    rename(`CLUSTER` = `as.matrix.groups.`)
  #  })
  
  #ngaClustSF <- reactive({st_sf(ngaClust, crs = 26392)})
  
  #ngaClustSF <- reactive({st_sf(
  #  (cbind
  #   (wp_ngaTrim,
  #     as.matrix
  #     (as.factor
  #       (cutree
  #         (hclust
  #           (dist
  #             (clusterVarsInput, 
  #               method = 'euclidean'), 
  #             method = 'ward.D'),
  #           k = input$clusterInput))))), 
  #  crs = 26392)})
  ngaClustSF <- reactive({st_sf(
    (cbind
     (wp_ngaTrim,
       as.matrix
       (as.factor
         (cutree
           (hclust
             (dist
               (clusterVarsInput, 
                 method = 'euclidean'), 
               method = 'ward.D'),
             k = input$clusterInput))))), 
    crs = 26392)})
  
  
  output$ngaMap <- renderPlot({
    tm_shape(wp_ngaTrim) +
    tm_fill(ngaClustSF$as.matrix.as.factor.cutree..hclust..dist.clusterVarsInput..method....euclidean.....,
            title = "testDisplay") +
      tm_borders(alpha = 0.3) +
      tm_style("cobalt") +
      tm_layout(main.title = "Test title",
                frame = TRUE,
                asp = 0)
    })
 
  
  #output$ngaMap <- renderPlot({
  #  qtm(ngaClust, "CLUSTER")
  #})
  
  #output$ngaMap <- renderTmap({
    #tmap_options(check.and.fix = TRUE) +
      #tm_shape(wp_ngaTrim) +
      #tm_fill(col = "CLUSTER",
      #        ) +
      #tm_borders(lwd = 0.1, alpha = 1) +
      #tm_layout(main.title = "Hierarchical Clustering",
      #          frame = TRUE,
      #          asp = 0) +
      #tm_view(set.zoom.limits = c(6, 8))
  #})
  }
  shinyApp(ui = ui, server = server)