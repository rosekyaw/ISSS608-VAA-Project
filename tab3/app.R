pacman::p_load(sf, tmap, shiny, shinythemes, plotly, tidyverse, ggstatsplot, 
               tools, cluster, factoextra, heatmaply, stats, leaflet, shinyHeatmaply)

funcNfuncHPump <- read_rds("data/funcNfuncHPump.rds")
funcNfuncMPump <- read_rds("data/funcNfuncMPump.rds")
funcNfuncX1000 <- read_rds("data/funcNfuncX1000.rds")
funcNfuncUrban0 <- read_rds("data/funcNfuncUrban0.rds")
funcNfuncCS4 <- read_rds("data/funcNfuncCS4.rds")
funcNfuncCS10 <- read_rds("data/funcNfuncCS10.rds")
funcNfuncPS09 <- read_rds("data/funcNfuncPS09.rds")
funcNfuncPS19 <- read_rds("data/funcNfuncPS19.rds")
funcNfuncHMPump <- read_rds("data/funcNfuncHMPump.rds")
funcNfuncHPump1k <- read_rds("data/funcNfuncHPump1k.rds")
funcNfuncHPumpUrban0 <- read_rds("data/funcNfuncHPumpUrban0.rds")
funcNfuncHPumpCS04 <- read_rds("data/funcNfuncHPumpCS04.rds")
funcNfuncHPumpCS10 <- read_rds("data/funcNfuncHPumpCS10.rds")
funcNfuncHPumpPS09 <- read_rds("data/funcNfuncHPumpPS09.rds")
funcNfuncHPumpPS19 <- read_rds("data/funcNfuncHPumpPS19.rds")
funcNfuncHMPump1k <- read_rds("data/funcNfuncHMPump1k.rds")
funcNfuncHMPumpUrban0 <- read_rds("data/funcNfuncHMPumpUrban0.rds")
funcNfuncHMPumpCS04 <- read_rds("data/funcNfuncHMPumpCS04.rds")
funcNfuncHMPumpCS10 <- read_rds("data/funcNfuncHMPumpCS10.rds")
funcNfuncHMPumpPS09 <- read_rds("data/funcNfuncHMPumpPS09.rds")
funcNfuncHMPumpPS19 <- read_rds("data/funcNfuncHMPumpPS19.rds")
funcNfuncHMPump1kUrban0 <- read_rds("data/funcNfuncHMPump1kUrban0.rds")
funcNfuncHMPump1kCS04 <- read_rds("data/funcNfuncHMPump1kCS04.rds")
funcNfuncHMPump1kCS10 <- read_rds("data/funcNfuncHMPump1kCS10.rds")
funcNfuncHMPump1kPS09 <- read_rds("data/funcNfuncHMPump1kPS09.rds")
funcNfuncHMPump1kPS19 <- read_rds("data/funcNfuncHMPump1kPS19.rds")
funcNfuncHMPump1kUrban0CS04 <- read_rds("data/funcNfuncHMPump1kUrban0CS04.rds")
funcNfuncHMPump1kUrban0CS10 <- read_rds("data/funcNfuncHMPump1kUrban0CS10.rds")
funcNfuncHMPump1kUrban0PS09 <- read_rds("data/funcNfuncHMPump1kUrban0PS09.rds")
funcNfuncHMPump1kUrban0PS19 <- read_rds("data/funcNfuncHMPump1kUrban0PS19.rds")
allV <- read_rds("data/allV.rds")

# optimal K
clust_funcNfuncHPump <- read_rds("data/clust_funcNfuncHPump.rds")
clust_funcNfuncMPump <- read_rds("data/clust_funcNfuncMPump.rds")
clust_funcNfuncX1000 <- read_rds("data/clust_funcNfuncX1000.rds")
clust_funcNfuncUrban0 <- read_rds("data/clust_funcNfuncUrban0.rds")
clust_funcNfuncCS4 <- read_rds("data/clust_funcNfuncCS4.rds")
clust_funcNfuncCS10 <- read_rds("data/clust_funcNfuncCS10.rds")
clust_funcNfuncPS09 <- read_rds("data/clust_funcNfuncPS09.rds")
clust_funcNfuncPS19 <- read_rds("data/clust_funcNfuncPS19.rds")
clust_funcNfuncHMPump <- read_rds("data/clust_funcNfuncHMPump.rds")
clust_funcNfuncHPump1k <- read_rds("data/clust_funcNfuncHPump1k.rds")
clust_funcNfuncHPumpUrban0 <- read_rds("data/clust_funcNfuncHPumpUrban0.rds")
clust_funcNfuncHPumpCS04 <- read_rds("data/clust_funcNfuncHPumpCS04.rds")
clust_funcNfuncHPumpCS10 <- read_rds("data/clust_funcNfuncHPumpCS10.rds")
clust_funcNfuncHPumpPS09 <- read_rds("data/clust_funcNfuncHPumpPS09.rds")
clust_funcNfuncHPumpPS19 <- read_rds("data/clust_funcNfuncHPumpPS19.rds")
clust_funcNfuncHMPump1k <- read_rds("data/clust_funcNfuncHMPump1k.rds")
clust_funcNfuncHMPumpUrban0 <- read_rds("data/clust_funcNfuncHMPumpUrban0.rds")
clust_funcNfuncHMPumpCS04 <- read_rds("data/clust_funcNfuncHMPumpCS04.rds")
clust_funcNfuncHMPumpCS10 <- read_rds("data/clust_funcNfuncHMPumpCS10.rds")
clust_funcNfuncHMPumpPS09 <- read_rds("data/clust_funcNfuncHMPumpPS09.rds")
clust_funcNfuncHMPumpPS19 <- read_rds("data/clust_funcNfuncHMPumpPS19.rds")
clust_funcNfuncHMPump1kUrban0 <- read_rds("data/clust_funcNfuncHMPump1kUrban0.rds")
clust_funcNfuncHMPump1kCS04 <- read_rds("data/clust_funcNfuncHMPump1kCS04.rds")
clust_funcNfuncHMPump1kCS10 <- read_rds("data/clust_funcNfuncHMPump1kCS10.rds")
clust_funcNfuncHMPump1kPS09 <- read_rds("data/clust_funcNfuncHMPump1kPS09.rds")
clust_funcNfuncHMPump1kPS19 <- read_rds("data/clust_funcNfuncHMPump1kPS19.rds")
clust_funcNfuncHMPump1kUrban0CS04 <- read_rds("data/clust_funcNfuncHMPump1kUrban0CS04.rds")
clust_funcNfuncHMPump1kUrban0CS10 <- read_rds("data/clust_funcNfuncHMPump1kUrban0CS10.rds")
clust_funcNfuncHMPump1kUrban0PS09 <- read_rds("data/clust_funcNfuncHMPump1kUrban0PS09.rds")
clust_funcNfuncHMPump1kUrban0PS19 <- read_rds("data/clust_funcNfuncHMPump1kUrban0PS19.rds")
clust_allV <- read_rds("data/clust_allV.rds")

# heatmap
clust_funcNfuncHPumpMat <- read_rds("data/clust_funcNfuncHPumpMat.rds")
clust_funcNfuncMPumpMat <- read_rds("data/clust_funcNfuncMPumpMat.rds")
clust_funcNfuncX1000Mat <- read_rds("data/clust_funcNfuncX1000Mat.rds")
clust_funcNfuncUrban0Mat <- read_rds("data/clust_funcNfuncUrban0Mat.rds")
clust_funcNfuncCS4Mat <- read_rds("data/clust_funcNfuncCS4Mat.rds")
clust_funcNfuncCS10Mat <- read_rds("data/clust_funcNfuncCS10Mat.rds")
clust_funcNfuncPS09Mat <- read_rds("data/clust_funcNfuncPS09Mat.rds")
clust_funcNfuncPS19Mat <- read_rds("data/clust_funcNfuncPS19Mat.rds")
clust_funcNfuncHMPumpMat <- read_rds("data/clust_funcNfuncHMPumpMat.rds")
clust_funcNfuncHPump1kMat <- read_rds("data/clust_funcNfuncHPump1kMat.rds")
clust_funcNfuncHPumpUrban0Mat <- read_rds("data/clust_funcNfuncHPumpUrban0Mat.rds")
clust_funcNfuncHPumpCS04Mat <- read_rds("data/clust_funcNfuncHPumpCS04Mat.rds")
clust_funcNfuncHPumpCS10Mat <- read_rds("data/clust_funcNfuncHPumpCS10Mat.rds")
clust_funcNfuncHPumpPS09Mat <- read_rds("data/clust_funcNfuncHPumpPS09Mat.rds")
clust_funcNfuncHPumpPS19Mat <- read_rds("data/clust_funcNfuncHPumpPS19Mat.rds")
clust_funcNfuncHMPump1kMat <- read_rds("data/clust_funcNfuncHMPump1kMat.rds")
clust_funcNfuncHMPumpUrban0Mat <- read_rds("data/clust_funcNfuncHMPumpUrban0Mat.rds")
clust_funcNfuncHMPumpCS04Mat <- read_rds("data/clust_funcNfuncHMPumpCS04Mat.rds")
clust_funcNfuncHMPumpCS10Mat <- read_rds("data/clust_funcNfuncHMPumpCS10Mat.rds")
clust_funcNfuncHMPumpPS09Mat <- read_rds("data/clust_funcNfuncHMPumpPS09Mat.rds")
clust_funcNfuncHMPumpPS19Mat <- read_rds("data/clust_funcNfuncHMPumpPS19Mat.rds")
clust_funcNfuncHMPump1kUrban0Mat <- read_rds("data/clust_funcNfuncHMPump1kUrban0Mat.rds")
clust_funcNfuncHMPump1kCS04Mat <- read_rds("data/clust_funcNfuncHMPump1kCS04Mat.rds")
clust_funcNfuncHMPump1kCS10Mat <- read_rds("data/clust_funcNfuncHMPump1kCS10Mat.rds")
clust_funcNfuncHMPump1kPS09Mat <- read_rds("data/clust_funcNfuncHMPump1kPS09Mat.rds")
clust_funcNfuncHMPump1kPS19Mat <- read_rds("data/clust_funcNfuncHMPump1kPS19Mat.rds")
clust_funcNfuncHMPump1kUrban0CS04Mat <- read_rds("data/clust_funcNfuncHMPump1kUrban0CS04Mat.rds")
clust_funcNfuncHMPump1kUrban0CS10Mat <- read_rds("data/clust_funcNfuncHMPump1kUrban0CS10Mat.rds")
clust_funcNfuncHMPump1kUrban0PS09Mat <- read_rds("data/clust_funcNfuncHMPump1kUrban0PS09Mat.rds")
clust_funcNfuncHMPump1kUrban0PS19Mat <- read_rds("data/clust_funcNfuncHMPump1kUrban0PS19Mat.rds")
clust_allVMat <- read_rds("data/clust_allVMat.rds")

# plot map
clustFuncNfuncHPumpWard <- read_rds("data/clustFuncNfuncHPumpWard.rds")
clustFuncNfuncMPumpWard <- read_rds("data/clustFuncNfuncMPumpWard.rds")
clustFuncNfuncX1000Ward <- read_rds("data/clustFuncNfuncX1000Ward.rds")
clustFuncNfuncUrban0Ward <- read_rds("data/clustFuncNfuncUrban0Ward.rds")
clustFuncNfuncCS4Ward <- read_rds("data/clustFuncNfuncCS4Ward.rds")
clustFuncNfuncCS10Ward <- read_rds("data/clustFuncNfuncCS10Ward.rds")
clustFuncNfuncPS09Ward <- read_rds("data/clustFuncNfuncPS09Ward.rds")
clustFuncNfuncPS19Ward <- read_rds("data/clustFuncNfuncPS19Ward.rds")
clustFuncNfuncHMPumpWard <- read_rds("data/clustFuncNfuncHMPumpWard.rds")
clustFuncNfuncHPump1kWard <- read_rds("data/clustFuncNfuncHPump1kWard.rds")
clustFuncNfuncHPumpUrban0Ward <- read_rds("data/clustFuncNfuncHPumpUrban0Ward.rds")
clustFuncNfuncHPumpCS04Ward <- read_rds("data/clustFuncNfuncHPumpCS04Ward.rds")
clustFuncNfuncHPumpCS10Ward <- read_rds("data/clustFuncNfuncHPumpCS10Ward.rds")
clustFuncNfuncHPumpPS09Ward <- read_rds("data/clustFuncNfuncHPumpPS09Ward.rds")
clustFuncNfuncHPumpPS19Ward <- read_rds("data/clustFuncNfuncHPumpPS19Ward.rds")
clustFuncNfuncHMPump1kWard <- read_rds("data/clustFuncNfuncHMPump1kWard.rds")
clustFuncNfuncHMPumpUrban0Ward <- read_rds("data/clustFuncNfuncHMPumpUrban0Ward.rds")
clustFuncNfuncHMPumpCS04Ward <- read_rds("data/clustFuncNfuncHMPumpCS04Ward.rds")
clustFuncNfuncHMPumpCS10Ward <- read_rds("data/clustFuncNfuncHMPumpCS10Ward.rds")
clustFuncNfuncHMPumpPS09Ward <- read_rds("data/clustFuncNfuncHMPumpPS09Ward.rds")
clustFuncNfuncHMPumpPS19Ward <- read_rds("data/clustFuncNfuncHMPumpPS19Ward.rds")
clustFuncNfuncHMPump1kUrban0Ward <- read_rds("data/clustFuncNfuncHMPump1kUrban0Ward.rds")
clustFuncNfuncHMPump1kCS04Ward <- read_rds("data/clustFuncNfuncHMPump1kCS04Ward.rds")
clustFuncNfuncHMPump1kCS10Ward <- read_rds("data/clustFuncNfuncHMPump1kCS10Ward.rds")
clustFuncNfuncHMPump1kPS09Ward <- read_rds("data/clustFuncNfuncHMPump1kPS09Ward.rds")
clustFuncNfuncHMPump1kPS19Ward <- read_rds("data/clustFuncNfuncHMPump1kPS19Ward.rds")
clustFuncNfuncHMPump1kUrban0CS04Ward <- read_rds("data/clustFuncNfuncHMPump1kUrban0CS04Ward.rds")
clustFuncNfuncHMPump1kUrban0CS10Ward <- read_rds("data/clustFuncNfuncHMPump1kUrban0CS10Ward.rds")
clustFuncNfuncHMPump1kUrban0PS09Ward <- read_rds("data/clustFuncNfuncHMPump1kUrban0PS09Ward.rds")
clustFuncNfuncHMPump1kUrban0PS19Ward <- read_rds("data/clustFuncNfuncHMPump1kUrban0PS19Ward.rds")
clustAllVWard <- read_rds("data/clustAllVWard.rds")

ui <- fluidPage (
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "attribute",
                         label = "Attribute :",
                         choices = c("Functional Water Points" = "pct_functional",
                                     "Non-Functional Water Points" = "pct_nonFunctional",
                                     "Hand Water Pump Deployment" = "pct_handPump",
                                     "Mechanical Water Pump Deployment" = "pct_mechPump",
                                     "Usage Capacity Limit below 1000" = "pct_ucN1000",
                                     "Sited within Non-urban Communities" = "pct_urban0",
                                     "Supporting < 40% population within 1 km" = "pct_cs04",
                                     "Moderate to Over-Supporting within 1 km" = "pct_cs10",
                                     "Within Usage Capacity Limit" = "pct_ps09",
                                     "Over Usage Capacity Limit" = "pct_ps19"),
                         selected = c("pct_functional", "pct_nonFunctional", "pct_handPump")),
      
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
      submitButton("Update view", icon("refresh"))),
    
    mainPanel(
      plotOutput(outputId = "clusterMap", width = "100%", height = "500"),
      plotOutput(outputId = "optimalK", width = "100%", height = "200"),
      plotOutput(outputId = "heatMap", width = "100%", height = "500")
      
    ))
)


server <- function(input, output) {
  
  attributeInput <- reactive({
    switch(
      paste(sort(input$attribute), collapse = ""),
      "pct_functionalpct_handPumppct_nonFunctional" = clustFuncNfuncHPumpWard,
      "pct_functionalpct_mechPumppct_nonFunctional" = clustFuncNfuncMPumpWard,
      "pct_functionalpct_nonFunctionalpct_ucN1000" = clustFuncNfuncX1000Ward,
      "pct_functionalpct_nonFunctionalpct_urban0" = clustFuncNfuncUrban0Ward,
      "pct_cs04pct_functionalpct_nonFunctional" = clustFuncNfuncCS4Ward,
      "pct_cs10pct_functionalpct_nonFunctional" = clustFuncNfuncCS10Ward,
      "pct_functionalpct_nonFunctionalpct_ps09" = clustFuncNfuncPS09Ward,
      "pct_functionalpct_nonFunctionalpct_ps19" = clustFuncNfuncPS19Ward,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clustFuncNfuncHMPumpWard,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ucN1000" = clustFuncNfuncHPump1kWard,
      "pct_functionalpct_handPumppct_nonFunctionalpct_urban0" = clustFuncNfuncHPumpUrban0Ward,
      "pct_cs04pct_functionalpct_handPumppct_nonFunctional" = clustFuncNfuncHPumpCS04Ward,
      "pct_cs10pct_functionalpct_handPumppct_nonFunctional" = clustFuncNfuncHPumpCS10Ward,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps09" = clustFuncNfuncHPumpPS09Ward,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps19" = clustFuncNfuncHPumpPS19Ward,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clustFuncNfuncHMPump1kWard,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_urban0" = clustFuncNfuncHMPumpUrban0Ward,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clustFuncNfuncHMPumpCS04Ward, 
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clustFuncNfuncHMPumpCS10Ward,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09" = clustFuncNfuncHMPumpPS09Ward,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19" = clustFuncNfuncHMPumpPS19Ward,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clustFuncNfuncHMPump1kUrban0Ward,       
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clustFuncNfuncHMPump1kCS04Ward,
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clustFuncNfuncHMPump1kCS10Ward,        
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000" = clustFuncNfuncHMPump1kPS09Ward,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000" = clustFuncNfuncHMPump1kPS19Ward,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clustFuncNfuncHMPump1kUrban0CS04Ward,  
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clustFuncNfuncHMPump1kUrban0CS10Ward,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000pct_urban0" = clustFuncNfuncHMPump1kUrban0PS09Ward,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000pct_urban0" = clustFuncNfuncHMPump1kUrban0PS19Ward,
      "pct_cs04pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ps19pct_ucN1000pct_urban0" = clustAllVWard)
  })
  
  boundaryInput <- reactive({
    boundaryInput <- switch(
      paste(sort(input$attribute), collapse = ""),
      "pct_functionalpct_handPumppct_nonFunctional" = funcNfuncHPump,
      "pct_functionalpct_mechPumppct_nonFunctional" = funcNfuncMPump,
      "pct_functionalpct_nonFunctionalpct_ucN1000" = funcNfuncX1000,
      "pct_functionalpct_nonFunctionalpct_urban0" = funcNfuncUrban0,
      "pct_cs04pct_functionalpct_nonFunctional" = funcNfuncCS4,
      "pct_cs10pct_functionalpct_nonFunctional" = funcNfuncCS10,
      "pct_functionalpct_nonFunctionalpct_ps09" = funcNfuncPS09,
      "pct_functionalpct_nonFunctionalpct_ps19" = funcNfuncPS19,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = funcNfuncHMPump,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ucN1000" = funcNfuncHPump1k,
      "pct_functionalpct_handPumppct_nonFunctionalpct_urban0" = funcNfuncHPumpUrban0,
      "pct_cs04pct_functionalpct_handPumppct_nonFunctional" = funcNfuncHPumpCS04,
      "pct_cs10pct_functionalpct_handPumppct_nonFunctional" = funcNfuncHPumpCS10,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps09" = funcNfuncHPumpPS09,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps19" = funcNfuncHPumpPS19,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = funcNfuncHMPump1k,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_urban0" = funcNfuncHMPumpUrban0,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = funcNfuncHMPumpCS04, 
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = funcNfuncHMPumpCS10,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09" = funcNfuncHMPumpPS09,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19" = funcNfuncHMPumpPS19,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = funcNfuncHMPump1kUrban0,       
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = funcNfuncHMPump1kCS04,
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = funcNfuncHMPump1kCS10,        
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000" = funcNfuncHMPump1kPS09,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000" = funcNfuncHMPump1kPS19,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = funcNfuncHMPump1kUrban0CS04,  
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = funcNfuncHMPump1kUrban0CS10,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000pct_urban0" = funcNfuncHMPump1kUrban0PS09,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000pct_urban0" = funcNfuncHMPump1kUrban0PS19,
      "pct_cs04pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ps19pct_ucN1000pct_urban0" = allV
    )
    boundaryInput %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(26391))
  })
  
  clusterMapInput <- reactive({
    groupsInput <- as.factor(cutree(attributeInput(), k = input$clusterInput))
    cbind(boundaryInput(), groupsInput) %>%
      rename(cluster = groupsInput)
  })
  
  optKInput <- reactive({
    optKInput <- switch(
      paste(sort(input$attribute), collapse = ""),
      "pct_functionalpct_handPumppct_nonFunctional" = clust_funcNfuncHPump,
      "pct_functionalpct_mechPumppct_nonFunctional" = clust_funcNfuncMPump,
      "pct_functionalpct_nonFunctionalpct_ucN1000" = clust_funcNfuncX1000,
      "pct_functionalpct_nonFunctionalpct_urban0" = clust_funcNfuncUrban0,
      "pct_cs04pct_functionalpct_nonFunctional" = clust_funcNfuncCS4,
      "pct_cs10pct_functionalpct_nonFunctional" = clust_funcNfuncCS10,
      "pct_functionalpct_nonFunctionalpct_ps09" = clust_funcNfuncPS09,
      "pct_functionalpct_nonFunctionalpct_ps19" = clust_funcNfuncPS19,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clust_funcNfuncHMPump,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHPump1k,
      "pct_functionalpct_handPumppct_nonFunctionalpct_urban0" = clust_funcNfuncHPumpUrban0,
      "pct_cs04pct_functionalpct_handPumppct_nonFunctional" = clust_funcNfuncHPumpCS04,
      "pct_cs10pct_functionalpct_handPumppct_nonFunctional" = clust_funcNfuncHPumpCS10,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps09" = clust_funcNfuncHPumpPS09,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps19" = clust_funcNfuncHPumpPS19,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHMPump1k,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_urban0" = clust_funcNfuncHMPumpUrban0,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clust_funcNfuncHMPumpCS04, 
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clust_funcNfuncHMPumpCS10,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09" = clust_funcNfuncHMPumpPS09,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19" = clust_funcNfuncHMPumpPS19,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0,       
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHMPump1kCS04,
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHMPump1kCS10,        
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000" = clust_funcNfuncHMPump1kPS09,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000" = clust_funcNfuncHMPump1kPS19,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0CS04,  
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0CS10,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0PS09,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0PS19,
      "pct_cs04pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ps19pct_ucN1000pct_urban0" = clust_allV
    )
  })
  
  heatmapInput <- reactive({
    heatmapInput <- switch(
      paste(sort(input$attribute), collapse = ""),
      "pct_functionalpct_handPumppct_nonFunctional" = clust_funcNfuncHPumpMat,
      "pct_functionalpct_mechPumppct_nonFunctional" = clust_funcNfuncMPumpMat,
      "pct_functionalpct_nonFunctionalpct_ucN1000" = clust_funcNfuncX1000Mat,
      "pct_functionalpct_nonFunctionalpct_urban0" = clust_funcNfuncUrban0Mat,
      "pct_cs04pct_functionalpct_nonFunctional" = clust_funcNfuncCS4Mat,
      "pct_cs10pct_functionalpct_nonFunctional" = clust_funcNfuncCS10Mat,
      "pct_functionalpct_nonFunctionalpct_ps09" = clust_funcNfuncPS09Mat,
      "pct_functionalpct_nonFunctionalpct_ps19" = clust_funcNfuncPS19Mat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clust_funcNfuncHMPumpMat,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHPump1kMat,
      "pct_functionalpct_handPumppct_nonFunctionalpct_urban0" = clust_funcNfuncHPumpUrban0Mat,
      "pct_cs04pct_functionalpct_handPumppct_nonFunctional" = clust_funcNfuncHPumpCS04Mat,
      "pct_cs10pct_functionalpct_handPumppct_nonFunctional" = clust_funcNfuncHPumpCS10Mat,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps09" = clust_funcNfuncHPumpPS09Mat,
      "pct_functionalpct_handPumppct_nonFunctionalpct_ps19" = clust_funcNfuncHPumpPS19Mat,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHMPump1kMat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_urban0" = clust_funcNfuncHMPumpUrban0Mat,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clust_funcNfuncHMPumpCS04Mat, 
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctional" = clust_funcNfuncHMPumpCS10Mat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09" = clust_funcNfuncHMPumpPS09Mat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19" = clust_funcNfuncHMPumpPS19Mat,
      
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0Mat,       
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHMPump1kCS04Mat,
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000" = clust_funcNfuncHMPump1kCS10Mat,        
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000" = clust_funcNfuncHMPump1kPS09Mat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000" = clust_funcNfuncHMPump1kPS19Mat,
      "pct_cs04pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0CS04Mat,  
      "pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0CS10Mat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0PS09Mat,
      "pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps19pct_ucN1000pct_urban0" = clust_funcNfuncHMPump1kUrban0PS19Mat,
      "pct_cs04pct_cs10pct_functionalpct_handPumppct_mechPumppct_nonFunctionalpct_ps09pct_ps19pct_ucN1000pct_urban0" = clust_allVMat
    )
  })
  
  
  output$clusterMap <- renderPlot({
    tm_shape(clusterMapInput()) +
      tm_fill(col = "cluster",
              title = "Test") +
      tm_borders(alpha = 0.3) +
      tm_style("cobalt") +
      tm_layout(main.title = "Hierarchical Clustering",
                main.title.size = 1.1,
                main.title.position = "center",
                legend.height = 0.3,
                legend.width = 0.1, 
                legend.title.size = 1,
                legend.text.size = 1,
                frame = TRUE,
                asp = 0)
  })
  
  output$optimalK <- renderPlot({
    req(input$optKmethod, optKInput())
    if (input$optKmethod == "Elbow Method") {
      fviz_nbclust(optKInput(), kmeans, method = "wss", linecolor = "white") +
        theme_dark() + labs(subtitle = "Elbow method")}
    else if (input$optKmethod == "Gap Statistics") {
      fviz_nbclust(optKInput(), hcut, nstart = 25, method = "gap_stat", nboot = 50,
                   linecolor = "white") + theme_dark() + labs(subtitle = "Gap statistic method")}
    else {
      fviz_nbclust(optKInput(), kmeans, method = "silhouette", linecolor = "white") + 
        theme_dark() + labs(subtitle = "Silhouette method")}
  })
  
  output$heatMap <- renderPlot({
    heatmap(data.matrix(heatmapInput()),
            Colv = NA,
            dist_method = "euclidean",
            hclust_method = "ward.D",
            seriate = "OLO",
            colors = Blues,
            k_row = 3,
            margins = c(5,5),
            fontsize_row = 1,
            fontsize_col = 8,
            main="Cluster Heatmap of x",
            xlab = "Attribute",
            ylab = "Nigeria LGA",
            dend_hoverinfo = TRUE)
  })
  
}

shinyApp(ui = ui, server = server)
