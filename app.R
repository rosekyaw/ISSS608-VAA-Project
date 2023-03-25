#pacman::p_load(sf, tmap, shiny, shinythemes, plotly, tidyverse, ggstatsplot, tools)
packages = c('shiny','shinythemes','shinydashboard','dashboardthemes','stringr','readxl','tidyverse','forcats',  #shiny and utilities
             'tidyquant','caTools','forecast','TSA','tseries','quantmod','timeSeries','xts','scales','car','ROCR',  #stats analysis
             'treemapify','shinyWidgets','plotly','sjPlot', 'DT','ggstatsplot', 'tools', 'tmap','sf','corrplot')   #visualization

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

wp_ngaTrim <- read_rds("data/wp_ngaTrim.rds")
wp_ngaTrim_data <- read_rds("data/wp_ngaTrim_data.rds")
wp_nga <- read_rds("data/wp_nga.rds")

# List of unique states
states <- append(unique(wp_ngaTrim_data[,c('state')]),"All",after=0)
#states.sort <- sort(unique(wp_ngaTrim_data[,c('state')]))
#states.multi <- c("All" = "",unique(wp_ngaTrim_data[,c('state')]))

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
cluster_vars <- cluster_vars %>%
  select(-shapeName)

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview" ,tabName = "Intro", icon = icon("dashboard")),
    menuItem("Visual Inferential Analysis",tabName = "Inferential", icon = icon("chart-line")),
    menuItem("Geographical Segmentation", tabName = "Segmentation", icon = icon("image")),
    menuItem("Theme", tabName = "tabThemes", icon = icon("cog"))
  ))

#################################
#
# Module change theme
#
#################################


# Ui functions ------------------------------------------------------------
uiChangeThemeDropdown <- function(dropDownLabel = "Change Theme", defaultTheme = "poor_mans_flatly")
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
  
  ns <- NS("moduleChangeTheme")
  dropdown <- tagList(
    selectizeInput(
      inputId = ns("dbxChangeTheme"),
      label = dropDownLabel,
      choices = changeThemeChoices,
      selected = defaultTheme
    )
  )
  
  return(dropdown)
}

uiChangeThemeOutput <- function()
{
  ns <- NS("moduleChangeTheme")
  themeOutput <- tagList(
    uiOutput(ns("uiChangeTheme"))
  )
  
  return(themeOutput)
}


# Server functions --------------------------------------------------------
serverChangeTheme <- function(input, output, session)
{
  observeEvent(
    input$dbxChangeTheme, 
    {
      output$uiChangeTheme <- renderUI({
        shinyDashboardThemes(theme = input$dbxChangeTheme)
      })
    }
  )
}


#################################
#
# UI
#
#################################


inferential_tab <- tabItem(
  tabName = "Inferential",
  #Creating Sub-Panels
  fluidPage(
  #Helptext
  fluidRow(
    column(12,helpText("Select the different sub tabs to find out more. Visualization may take some time to render.", style = "font-size:110%;font-style:italic;" ), )),
  fluidRow(
    column(12,navbarPage(" ",inverse = TRUE,
                         #ANOVA v2
                         tabPanel("ANOVA",icon=icon("directions"),#br()
                                  fluidRow(
                                    column(5,
                                           selectInput("return_ANOVA2", "ANOVA Analysis for Number of:",
                                                       choices = list("Total Waterpoints" = "total_wp",
                                                                      "Functional waterpoints" = "wp_functional",
                                                                      "Non-functional waterpoints" = "wp_nonFunctional",
                                                                      "Handpumps" = "total_handPump",
                                                                      "Mechanical pumps" = "total_mechPump", 
                                                                      "Tap stands" = "total_tapStand", 
                                                                      "Waterpoints with usage capacity 50" = "total_uc50",
                                                                      "Waterpoints with usage capacity 250" = "total_uc250",
                                                                      "Waterpoints with usage capacity 300" = "total_uc300",
                                                                      "Waterpoints with usage capacity 1000" = "total_uc1000",
                                                                      "Waterpoints with usage capacity under 1000" = "total_ucn1000",
                                                                      "Urban areas" = "total_urban1",
                                                                      "Non-urban areas" ="total_urban0",
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
#                                    ),
#                                  ),
#                                  fluidRow(
#                                    column(10,
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
                                        plotOutput('scatter'),
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
  ))
  

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
    tabItems(overview_tab, inferential_tab, changetheme_tab)
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

#callModule(module = serverChangeTheme, id = "moduleChangeTheme")

server <- function(input, output) {
  
  #Overview (Rose's part)
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
  
  
  # ANOVA Test v2 (Zhi Hao's part)
  #output for ANOVA boxplot
  output$boxplot_anova <- renderPlotly({
    
    p1 <- wp_ngaTrim_data  %>%
      plot_ly(y=~.data[[input$return_ANOVA2]], x=~state, type ="box", color=~state, colors="Dark2",
              boxpoints = "all", jitter =0.3, pointpos = -1.8, text=~paste(district,sep=":")) %>%
      layout(yaxis=list(title=input$return_ANOVA2),xaxis=list(title="States"))
    
    return(p1)
    
  })
  
  
  #ANOVA output (Zhi Hao's part)
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
 
  #Scatter plot output (Zhi Hao's part)
  output$scatter <- renderPlot({
    
    scatter1 <- ggplot(data = cluster_vars, 
              aes_string(x = input$return_corr,y = input$return_corr2)) +
      geom_point() +
      geom_smooth()
    
    return(scatter1)
  })
  
  #Correlogram output (Zhi Hao's part)
  output$correlogram <- renderPlot({
    corr1 <- corrplot.mixed((cor(cluster_vars)),
                       upper = "number",
                       lower = "ellipse",
                       tl.col = "black",
                      diag = "l",
                      tl.pos = "lt")
        return(corr1)
  })
  
  
  }


shinyApp(ui = ui, server = server)
