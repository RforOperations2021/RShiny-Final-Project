library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinyWidgets)
library(tidyverse)
library(bslib)
library(plotly)

require(rgdal)
require(leaflet)
require(leaflet.extras)

require(dplyr)
require(readxl)
require(stringr)

LAPD <- read_csv('LAPD_updated.csv')

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# App header & title -----------------------------------------------
header <- dashboardHeader(title='Arrests by the LAPD')

# Dashboard sidebar ------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Beginning of COVID-19", icon = icon("bar-chart"), tabName = "covid"),
    menuItem("Police Brutality Protests", icon = icon("bar-chart"), tabName = "protest"),
    
    # Select which areas to include ------------------------
    pickerInput(inputId = "selected_hood",
                label = "Select area(s):",
                choices = sort(unique(LAPD$`Area Name`)),
                options = list(`actions-box` = TRUE),
                multiple = TRUE, 
                selected = unique(LAPD$`Area Name`)),
    
    # Select variable to group by ----------------------------------
    selectInput(inputId = "group", 
                label = "Group Arrests by:",
                choices = c("Race and Ethnicity" = "`Descent Code`", 
                            "Sex" = "`Sex Code`", 
                            "Arrest Type" = "`Arrest Type Code`")),
    
    ### Daily vs weekly
    switchInput(inputId = "time", 
                value=TRUE,
                onLabel= c("date" = "Day"), 
                offLabel= c("week"= "Week")), 
    
    # one more menu item
    menuItem("Maps", icon = icon("map-marked-alt"), tabName = "maps")
    
  )
)

body <- dashboardBody(tabItems(
  # Plot page ----------------------------------------------
  tabItem("covid",
          
          #Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("covid_day"),
            valueBoxOutput("covid_val1"),
            valueBoxOutput("covid_val2")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Over time", plotlyOutput("covid_time")),
                   tabPanel("Total", plotlyOutput("covid_group")),
                   tabPanel("By Offense", plotlyOutput("covid_offense")),
                   tabPanel("Table", DT::dataTableOutput("covid_table")))
          )
          
  ),
  
  
  tabItem("protest",
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("protest_day"),
            valueBoxOutput("protest_val1"),
            valueBoxOutput("protest_val2")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Over time", plotlyOutput("protest_time")),
                   tabPanel("Total", plotlyOutput("protest_group")),
                   tabPanel("By Offense", plotlyOutput("protest_offense")),
                   tabPanel("Table",  DT::dataTableOutput("protest_table")))
          )
  ),
  
  tabItem("maps",
          # Input and Value Boxes ----------------------------------------------
          #more input boxes: offense type
          
          # Map ----------------------------------------------
          
          
          fluidRow(
            tabBox(title = "Maps",
                   width = 12,
                   # Using Shiny JS
                   shinyjs::useShinyjs(),
                   # Style the background and change the page
                   tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                              body {background-color: #D4EFDF;}"),
                   # Map Output
                   leafletOutput("leaflet")
                  # tabPanel("Table",  DT::dataTableOutput("protest_table"))
          ))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Basic Map
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      setView(-118.4, 33, 9)
  })
  
  # Create a subset of data filtering for selected title types ------
  LAPD_subset_COVID <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood & week %in% c(9:11))
  })
  
  LAPD_subset_PROTEST <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood & week %in% c(21:23))
  })
  
  LAPD_subset <- reactive({
    req(input$selected_hood) # ensure availablity of value before proceeding
    filter(LAPD, `Area Name` %in% input$selected_hood)
  })
  
  # Covid tab --------------------------------------------------------
  timeval <- reactive({
    ifelse(input$time==T, "date", "week")
  })
  output$covid_time <- renderPlotly({
    ggplot(data = LAPD_subset_COVID(), aes_string(x = timeval(), group=input$group, color=input$group)) +
      geom_point(stat='count') +
      geom_line(stat='count', alpha=0.3) +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=5) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Time',
           y = 'Arrest Count',
           title = "Arrests over time"
      )
  })
  
  output$covid_group <- renderPlotly({
    ggplot(data = LAPD_subset_COVID(), aes_string(x = input$group)) +
      geom_bar() +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=100) +
      labs(x = input$group,
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = "Arrest Count by Grouping"
      )
  })
  
  output$covid_offense <- renderPlotly({
    vals <- LAPD_subset_COVID() %>% group_by(`Charge Group Description`) %>% summarize(count=n())
    fig <- plot_ly(labels = ~vals$`Charge Group Description`, values = vals$count, type = 'pie')
    fig <- fig %>% layout(title = 'Arrests by Offense Type',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
  })
  
  fluidPage(
    box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  
  # COVID info boxes ----------------------------------------------
  output$covid_day <- renderInfoBox({
    infoBox("Date",subtitle="LAPD issues cite and release protocol the first week of March, 2020", icon = icon("calendar-day"), color = "navy")
  })
  
  output$covid_val1 <- renderValueBox({
    val <-LAPD_subset_COVID() %>%filter(week<10)%>% summarize(count=n())
    
    valueBox(val,"Arrests the week before cite and release", icon = icon("siren-on"), color = "maroon")
  })
  
  output$covid_val2 <- renderValueBox({
    val <-LAPD_subset_COVID() %>%filter(week>10)%>% summarize(count=n())
    
    valueBox(val,"Arrests the week after cite and release", icon = icon("viruses"), color = "olive")
  })
  
  # COVID data table  ----------------------------------------------
  output$covid_table <- DT::renderDataTable({
    LAPD_subset_COVID()[,c(5,7,9:11,13:16,28)] 
  })
  
  # Protest tab -------------------------------------------------------
  
  output$protest_time <- renderPlotly({
    ggplot(data = LAPD_subset_PROTEST(), aes_string(x = timeval(), group=input$group, color=input$group)) +
      geom_point(stat='count') +
      geom_line(stat='count', alpha=0.3) +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=5) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = 'Time',
           y = 'Arrest Count',
           title = "Arrests over time"
      )
  })
  
  output$protest_group <- renderPlotly({
    ggplot(data = LAPD_subset_PROTEST(), aes_string(x = input$group)) +
      geom_bar() +
      geom_text(aes(label=stat(count)), stat='count', nudge_y=100) +
      labs(x = input$group,
           y = 'Arrest Count',
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = "Arrest Count by Grouping"
      )
  })
  
  output$protest_offense <- renderPlotly({
    vals <- LAPD_subset_PROTEST() %>% group_by(`Charge Group Description`) %>% summarize(count=n())
    fig <- plot_ly(labels = ~vals$`Charge Group Description`, values = vals$count, type = 'pie')
    fig <- fig %>% layout(title = 'Arrests by Offense Type',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  # Protest data table  ----------------------------------------------
  output$protest_table <- DT::renderDataTable({
    LAPD_subset_PROTEST()[,c(5,7,9:11,13:16,28)] 
  })
  
  # Protest info boxes ----------------------------------------------
  output$protest_day <- renderInfoBox({
    infoBox("Date",subtitle="George Floyd was murdered at the hands of the police on May 25, 2020", icon = icon("calendar-day"), color = "navy")
  })
  
  output$protest_val1 <- renderValueBox({
    val <-LAPD_subset_PROTEST() %>%filter(week>21 & Charge == '463(A)PC')%>% summarize(count=n())
    
    valueBox(val,"Arrests for looting (463(A)PC) in the two weeks following the death of George Floyd", color = "teal")
  })
  
  output$protest_val2 <- renderValueBox({
    val <-LAPD_subset_PROTEST() %>%filter(week>21 & Charge == '8.78LAAC')%>% summarize(count=n())
    
    valueBox(val,"Arrests for curfew violations in the two weeks following the death of George Floyd", color = "olive")
  })
  
  # Map tab ----------------------------------------
  
  # Replace layer with filtered LAPD dataset
  observe({
    subs <- LAPD_subset()
    arrpal <- colorFactor(c("#33cc33", "#ff9933", "#cc99ff"), c("B", "W", "H"))
    # Data is greenInf
    leafletProxy("leaflet", data = subs) %>%
      # In this case either lines 92 or 93 will work
      clearMarkers() %>%
      #clearGroup(group = "greenInf") %>%
      addCircleMarkers(data = subs, lng = ~LON, lat = ~LAT, radius = 0.5, color = ~arrpal(`Descent Code`)) %>%
      addLegend(position = "topright" , pal= arrpal, values = subs$`Descent Code`, title = "Race and Ethnicity")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

