#################################
#
# Install and load packages
#
#################################
# 


library(shinydashboard)
library(dashboardthemes)
library(stringr)
library(readxl)
library(tidyverse)
library(forcats)
library(tidyquant)
library(caTools)
library(forecast)
library(TSA)
library(tseries)
library(quantmod)
library(timeSeries)
library(xts)
library(scales)
library(car)
library(ROCR)
library(stats)
library(treemapify)
library(shinyWidgets)
library(plotly)
library(sjPlot)
library(DT)
library(ggstatsplot)
library(tools)
library(tmap)
library(sf)
library(corrplot)
library(cluster)
library(factoextra)
library(heatmaply)
library(leaflet)
library(shinyHeatmaply)


#################################
#
# Data Preparation
#
#################################

# Load the data
wp_ngaTrim <- read_rds("data/wp_ngaTrim.rds")
wp_ngaTrim_data <- read_rds("data/wp_ngaTrim_data.rds")
wp_nga <- read_rds("data/wp_nga.rds")

# boundary
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

# List of unique states
states <- append(unique(wp_ngaTrim_data[,c('state')]),"All",after=0)

cluster_vars <- wp_ngaTrim %>%
  st_set_geometry(NULL) %>%
  select("shapeName",
         "total_wp",
         "pct_functional", 
         "pct_nonFunctional",
         "pct_handPump",
         "pct_mechPump",
         "pct_tapStand",
         "pct_uc300",
         "pct_uc1000",
         "pct_ucN1000",
         "pct_uc250",
         "pct_urban1",
         "pct_urban0",
         "pct_cs04",
         "pct_cs10",
         "pct_stat1",
         "pct_stat0",
         "pct_ps09",
         "pct_ps19")

row.names(cluster_vars) <- cluster_vars$shapeName
cluster_vars <- cluster_vars %>% select(-shapeName)

attributesOption <- c("Functional waterpoints" = "pct_functional", "Non-functional waterpoints" = "pct_nonFunctional", "Hand pumps" = "pct_handPump",
  "Mechanical pumps" = "pct_mechPump", "Usage capacity under 1000" = "pct_ucN1000", "Rural" = "pct_urban0",
  "Low Crucial score" = "pct_cs04", "High Crucial score" = "pct_cs10", "Acceptable Pressure score" = "pct_ps09",
  "High Pressure score" = "pct_ps19")

#################################
#
# UI
#
#################################

# Ui functions ------------------------------------------------------------
uiChangeThemeDropdown <- function(dropDownLabel = "Change Theme", defaultTheme = "blue_gradient")
{
  changeThemeChoices <- c(
    "Blue gradient" = "blue_gradient",
    "Flat Red" = "flat_red",
    "Grey light" = "grey_light",
    "Grey dark" = "grey_dark",
    "OneNote" = "onenote",
    "Poor man's Flatly" = "poor_mans_flatly",
    "Purple gradient" = "purple_gradient"
  )
  
  dropdown <- tagList(
    selectizeInput(
      inputId = "dbxChangeTheme",
      label = dropDownLabel,
      choices = changeThemeChoices,
      selected = defaultTheme))
  
  return(dropdown)
}

uiChangeThemeOutput <- function()
{
  themeOutput <- uiOutput("uiChangeTheme")
  return(themeOutput)
}

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview" ,tabName = "Intro", icon = icon("dashboard")),
    menuItem("Visual Inferential Analysis",tabName = "Inferential", icon = icon("chart-line")),
    menuItem("Geographical Segmentation", tabName = "Segmentation", icon = icon("image")),
    menuItem("Theme", tabName = "tabThemes", icon = icon("cog"))
  ))

# Overview
overview_tab <- tabItem(
  tabName = "Intro",
  fluidPage(
    titlePanel("Overview"),
    
    # Sidebar
    sidebarLayout(
      sidebarPanel(
        # Water Point Status
        selectInput(inputId = "status",
                    label = "Water Point Status :",
                    choices = c("Functional waterpoints" = "pct_functional",
                                "Non-functional waterpoints" = "pct_nonfunctional",
                                "Unknown waterpoints" = "pct_unknown"),
                    selected = "Functional waterpoints"),
        
        # Another Parameter
        selectInput(inputId = "parameter",
                    label = "Another Parameter :",
                    choices = c("Nil",
                                "Water Source",
                                "Technology",
                                "Pressure Score",
                                "Crucial Score",
                                "Capacity",
                                "Community"),
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
                      choices = c("Hand pump" = "pct_handpump",
                                  "Mechanical pump" = "pct_mechpump")
          )),
        
        # Only show this panel if the parameter is Pressure Score
        conditionalPanel(
          condition = "input.parameter == 'Pressure Score'",
          selectInput(inputId = "param3",
                      label = "Pressure score :",
                      choices = c("Acceptable Pressure Score" = "pct_withinpressure",
                                  "High Pressure Score" = "pct_highpressure")
          )),
        
        # Only show this panel if the parameter is Crucial Score
        conditionalPanel(
          condition = "input.parameter == 'Crucial Score'",
          selectInput(inputId = "param4",
                      label = "Crucial Score :",
                      choices = c("Low Crucial score" = "pct_lowcrucial",
                                  "High Crucial score" = "pct_highcrucial")
          )),
        
        # Only show this panel if the parameter is Capacity
        conditionalPanel(
          condition = "input.parameter == 'Capacity'",
          selectInput(inputId = "param5",
                      label = "Capacity :",
                      choices = c("Low capacity" = "pct_lowcap",
                                  "High capacity" = "pct_highcap")
          )),
        
        # Only show this panel if the parameter is Community
        conditionalPanel(
          condition = "input.parameter == 'Community'",
          selectInput(inputId = "param6",
                      label = "Community :",
                      choices = c("Urban" = "pct_urban",
                                  "Rural" = "pct_rural")
          )),
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tmapOutput(outputId = "ngaMap",
                   width = "100%", 
                   height = 400))
      
    )
  ))

# Visual Inferential Analysis
inferential_tab <- tabItem(
  tabName = "Inferential",
  # Creating Sub-Panels
  fluidPage(
  # Helptext
  fluidRow(
    column(12,helpText("Select the different sub tabs to find out more. Visualisation may take some time to render.", style = "font-size:110%;font-style:italic;" ), )),
  fluidRow(
    column(12,navbarPage(" ",inverse = TRUE,
                         #ANOVA v2
                         tabPanel("ANOVA",icon=icon("directions"),#br()
                                  fluidRow(
                                    column(5,
                                           selectInput("return_ANOVA2", "ANOVA Analysis for Number of:",
                                                       choices = list("Total waterpoints" = "total_wp",
                                                                      "Functional waterpoints" = "wp_functional",
                                                                      "Non-functional waterpoints" = "wp_nonFunctional",
                                                                      "Hand pumps" = "total_handPump",
                                                                      "Mechanical pumps" = "total_mechPump", 
                                                                      "Tap stands" = "total_tapStand", 
                                                                      "Waterpoints with usage capacity 50" = "total_uc50",
                                                                      "Waterpoints with usage capacity 250" = "total_uc250",
                                                                      "Waterpoints with usage capacity 300" = "total_uc300",
                                                                      "Waterpoints with usage capacity 1000" = "total_uc1000",
                                                                      "Waterpoints with usage capacity under 1000" = "total_ucn1000",
                                                                      "Urban areas" = "total_urban1",
                                                                      "Rural areas" ="total_urban0",
                                                                      "Crucial score 04" = "total_cs04",
                                                                      "Crucial score 10" = "total_cs10",
                                                                      "Pressure score 09" = "total_ps09",
                                                                      "Pressure score 19" ="total_ps19",
                                                                      "Pressure score 39" ="total_ps39",
                                                                      "Pressure score 40" ="total_ps40"),
                                                       selected="Total Waterpoints")
                                    ),
                                  ),
                                  fluidRow(
                                    box(width =12,
                                        h4('Water Point information by States'),
                                        #plot output
                                        fluidRow(plotlyOutput(outputId = 'boxplot_anova', height = '500px'))
                                    )
                                  ),
                                  fluidRow(
                                    box(width =12,
                                        h4('Test Summary'),
                                        htmlOutput('anovatext2'),
                                        verbatimTextOutput('anovaresult2'),
                                    )
                                  )
                        
                   
                         ),
                         tabPanel("CORRELATION", icon=icon("directions"),#br()
                                  sidebarLayout(
                                    sidebarPanel(
#                                  fluidRow(
#                                    column(5,
                                           selectInput("return_corr", "Please select Variable 1 (x-axis):",
                                                       choices = list("Total Waterpoints" = "total_wp",
                                                                      "% functional" = "pct_functional", 
                                                                      "% non-functional" = "pct_nonFunctional",
                                                                      "% hand pump" = "pct_handPump",
                                                                      "% mechanical pump" = "pct_mechPump",
                                                                      "% tap stand" = "pct_tapStand",
                                                                      "% usage capacity 300" = "pct_uc300",
                                                                      "% usage capacity 1000" = "pct_uc1000",
                                                                      "% usage capacity under 1000" = "pct_ucN1000",
                                                                      "% usage capacity 250" = "pct_uc250",
                                                                      "% urban" = "pct_urban1",
                                                                      "% non-urban" = "pct_urban0",
                                                                      "% crucial score 04" = "pct_cs04",
                                                                      "% crucial score 10" = "pct_cs10",
                                                                      "% pressure score 09" = "pct_ps09",
                                                                      "% pressure score 19" = "pct_ps19"),
                                                       selected="pct_functionall"),
                                           selectInput("return_corr2", "Please select Variable 2 (y-axis):",
                                                       choices = list("Total Waterpoints" = "total_wp",
                                                                      "% functional" = "pct_functional", 
                                                                      "% non-functional" = "pct_nonFunctional",
                                                                      "% hand pump" = "pct_handPump",
                                                                      "% mechanical pump" = "pct_mechPump",
                                                                      "% tap stand" = "pct_tapStand",
                                                                      "% usage capacity 300" = "pct_uc300",
                                                                      "% usage capacity 1000" = "pct_uc1000",
                                                                      "% usage capacity under 1000" = "pct_ucN1000",
                                                                      "% usage capacity 250" = "pct_uc250",
                                                                      "% urban" = "pct_urban1",
                                                                      "% non-urban" = "pct_urban0",
                                                                      "% crucial score 04" = "pct_cs04",
                                                                      "% crucial score 10" = "pct_cs10",
                                                                      "% pressure score 09" = "pct_ps09",
                                                                      "% pressure score 19" = "pct_ps19"),
                                                       selected="pct_handPum")
                                    ),
                                  
                                  mainPanel(
                                  fluidRow(
                                    box(width = 12,
                                        h4('Scatter Plot of Selected Waterpoint Variables'),
                                        plotlyOutput('scatter'),
                                    )
                                  ),
                                  fluidRow(
                                    box(width = 12,
                                        h4('Correlation Matrix of Waterpoint Variables'),
                                        plotOutput('correlogram'),
                                    )
                                  ))
                                  
                         )
                         
    )
    )))))

# Geographical Segmentation
segmentation_tab<- tabItem(
  tabName = "Segmentation",
  fluidPage(
    titlePanel("Geographical Segmentation"),
    fluidRow(
      column(8, helpText("The heatmap visualisation or any changes may need up to 40 seconds to render.", 
                         style = "font-size:110%;font-style:italic;"))),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "attribute",
                         label = "Attribute :",
                         choices = attributesOption,
                         selected = attributesOption[1:7]),
      
      radioButtons(inputId = "optKmethod",
                   label = "Method to determine Optimal k value :",
                   choices = c("Within Cluster Sum of Squares(Elbow)", 
                               "Gap Statistics", 
                               "Silhouette Score"),
                   selected = "Within Cluster Sum of Squares(Elbow)"),
      
      sliderInput(inputId = "clusterInput",
                  label = "Number of cluster",
                  min = 2,
                  max = 10,
                  value = 3),
      #submitButton("Update view", icon("refresh")),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cluster Map",
                 plotOutput(outputId = "clusterMap", width = "100%", height = "520")
        ),
        tabPanel("Optimal K",
                 plotlyOutput(outputId = "optimalK", width = "100%", height = "400")
        ),
        tabPanel("Heat Map",
                 plotlyOutput(outputId = "heatMap", width = "100%", height = "550")
        ))))))


# Themes
changetheme_tab <- tabItem(
  tabName = "tabThemes",
  fluidRow(
    column(
      width = 12,
      
      # Theme drop-down ---------------------------------------------------------
      uiChangeThemeDropdown()
    )
  )
)


body <- dashboardBody(
  # Custom theme ------------------------------------------------------------
  uiChangeThemeOutput(),
  
  fluidPage(
    tabItems(overview_tab, inferential_tab, segmentation_tab, changetheme_tab)
  ))


ui <- dashboardPage(
  dashboardHeader(title = "Nigeria Waterpoint Analysis"),
  sidebar,
  body)

#################################
#
# Server
#
#################################
server <- function(input, output, session) {
  
  # Change Theme
  observeEvent(
    input$dbxChangeTheme, 
    {
      output$uiChangeTheme <- renderUI({shinyDashboardThemes(theme = input$dbxChangeTheme)})
    })
  
  #Overview
  output$ngaMap <- renderTmap({
    
    tmap_options(check.and.fix = TRUE) +
      tm_shape(wp_nga) +
      tm_fill(input$status,
              n = 5,
              style = "pretty",
              palette = "Blues") +
      (if('Nil' %in% input$parameter) tm_bubbles(col= 'white', size = 0)
       else tm_bubbles(
         (if('Water Source' %in% input$parameter) col = input$param1
          else if('Technology' %in% input$parameter) col = input$param2
          else if('Pressure Score' %in% input$parameter) col = input$param3
          else if('Crucial Score' %in% input$parameter) col = input$param4
          else if('Capacity' %in% input$parameter) col = input$param5
          else if('Community' %in% input$parameter) col = input$param6),
         (if('Water Source' %in% input$parameter) size = input$param1
          else if('Technology' %in% input$parameter) size = input$param2
          else if('Pressure Score' %in% input$parameter) size = input$param3
          else if('Crucial Score' %in% input$parameter) size = input$param4
          else if('Capacity' %in% input$parameter) size = input$param5
          else if('Community' %in% input$parameter) size = input$param6),
         n = 5,
         alpha = 0.6,
         scale = 0.5,
         border.col = "black",
         border.lwd = 0.1,
         palette = "Set3")) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(5.5, 10.5))
  })
  
  
  # ANOVA Test v2
  #output for ANOVA boxplot
  output$boxplot_anova <- renderPlotly({
    
    p1 <- wp_ngaTrim_data  %>%
      plot_ly(y=~.data[[input$return_ANOVA2]], x=~state, type ="box", color=~state, colors="Dark2",
              boxpoints = "all", jitter =0.3, pointpos = -1.8, text=~paste(district,sep=":")) %>%
      layout(yaxis=list(title=input$return_ANOVA2),xaxis=list(title="States"))
    
    return(p1)
    
  })
  
  
  #ANOVA output
  output$anovatext2 <- renderUI({
    res.aov <- aov(wp_ngaTrim_data[[input$return_ANOVA2]] ~ state, data = wp_ngaTrim_data)
    anovap <- summary(res.aov)[[1]][1,5]
    
    anovatext1 <- paste('<b>','H0:', '</b>','Mean values are the same for all states')
    anovatext2 <- paste('<b>','H1:', '</b>','Not all mean values are the same for states')
    anovatext3 <- paste('Test done using ','<b>', 'ANOVA', '</b>')
    if (anovap < 0.05){
      anovatext4 <- paste('<b>','Results:', '</b>', ' We can reject the null hypothesis')
      anovatext5 <- paste("<font color=\"#0000FF\"><b>","p-value < 0.05", "</b></font>")
    } else {
      anovatext4 <- paste('<b>','Results:', '</b>', ' We cannot reject the null hypothesis')
      anovatext5 <- paste("<font color=\"#0000FF\"><b>","p-value > 0.05", "</b></font>")
    }
    
    HTML(paste(anovatext3,' ',anovatext1,anovatext2,' ',anovatext4,anovatext5,sep = '<br/>'))
  })
  
  output$anovaresult2 <- renderPrint ({
    res.aov <- aov(wp_ngaTrim_data[[input$return_ANOVA2]] ~ state, 
                   data = wp_ngaTrim_data)
    anovaprint <- summary(res.aov)
    
    return(anovaprint)
  })
 
  #Scatter plot output
  output$scatter <- renderPlotly({
    scatter1 <- ggplot(data = cluster_vars, 
              aes_string(x = input$return_corr,y = input$return_corr2)) +
      geom_point()
    scatter1 <- ggplotly(scatter1)
    return(scatter1)
  })
  
  #Correlogram output
  output$correlogram <- renderPlot({
    corr1 <- corrplot.mixed((cor(cluster_vars)),
                       upper = "number",
                       lower = "ellipse",
                       tl.col = "black",
                      diag = "l",
                      tl.pos = "lt")
        return(corr1)
  })
  
  # Geographical representation
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
  
  # Cluset Map
  output$clusterMap <- renderPlot({
    tm_shape(clusterMapInput()) +
      tm_fill(col = "cluster",
              title = "Cluster") +
      tm_borders(alpha = 0.3) +
      tm_style("cobalt") +
      tm_layout(main.title = "Hierarchical Clustering for NGA Water Points",
                main.title.size = 1.8,
                main.title.position = "center",
                legend.height = 1.5,
                legend.width = 0.5, 
                legend.title.size = 1.3,
                legend.text.size = 1,
                frame = TRUE,
                asp = 0)
  })
  
  
  # cluster
  output$optimalK <- renderPlotly({
    req(input$optKmethod, optKInput())
    if (input$optKmethod == "Within Cluster Sum of Squares(Elbow)") {
      fviz_nbclust(optKInput(), kmeans, method = "wss", linecolor = "white") +
        theme_dark() +
        ggtitle("Within Cluster Sum of Squares (Elbow)") +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 13),
              plot.margin = unit(c(1, 1, 1, 0.5), "cm"))}
    else if (input$optKmethod == "Gap Statistics") {
      fviz_nbclust(optKInput(), hcut, nstart = 25, method = "gap_stat", nboot = 50,
                   linecolor = "white") + theme_dark()  +
        ggtitle("Gap statistic method") +
        theme(plot.title = element_text(size = 16), 
              axis.title = element_text(size = 13), 
              plot.margin = unit(c(1, 1, 1, 0.5), "cm"))}
    else {
      fviz_nbclust(optKInput(), kmeans, method = "silhouette", linecolor = "white") + 
        theme_dark() + 
        ggtitle("Silhouette method") +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 13),
              plot.margin = unit(c(1, 1, 1, 0.5), "cm"))}
  })
  
  # Heat Map
  output$heatMap <- renderPlotly({
    heatmaply(data.matrix(heatmapInput()),
              Colv = NA,
              dist_method = "euclidean",
              hclust_method = "ward.D",
              seriate = "OLO",
              colors = Blues,
              k_row = input$clusterInput,
              margins = c(5,5),
              fontsize_row = 1,
              fontsize_col = 8,
              main = "Cluster Heatmap",
              xlab = "",
              ylab = "Nigeria LGA",
              dend_hoverinfo = TRUE) %>%
      layout(xaxis = list(tickangle = 20, tickfont = list(size = 10), titlefont = list(size = 14)),
             yaxis = list(title = "Nigeria LGA", titlefont = list(size = 14)),
             margin = list(l = 80, t = 80, b = 110, r = 50))
  })
  
}


shinyApp(ui = ui, server = server)
