library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(dplyr)

shinyUI(dashboardPage(
  dashboardHeader(title = "Diabetes Studies App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro", tabName = "intro", icon = icon("flag")),
      menuItem("Studies Info", tabName = "studyinfo", icon = icon("ambulance")),
      menuItem("Annual Data", tabName = "time", icon = icon("calendar")),
      menuItem("Sponsor Data", tabName = "data", icon = icon("cog")),
      menuItem("Map", tabName = "map", icon = icon("map"))
    )
  ), #close dashboardSidebar
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "intro", 
              fluidRow(
              box(tags$iframe(src="https://www.youtube.com/embed/wmOW091P2ew", width="100%", height="400px")),
              box(tags$iframe(src="https://www.youtube.com/embed/7Eg14Yp0TMY", width="100%", height="400px"))
              )
      ),
      
      tabItem(tabName = "studyinfo",
              selectizeInput("chosen",
                             "Select category",
                             choice_study_info),
              fluidRow(plotOutput("studyinfo"))
      ),
      
      tabItem(tabName = "map", 
              fluidRow(box(leafletOutput("mymap", height=700), width = 10),
                       box(
                         selectizeInput("choice",
                                        "Select category",
                                        choice_map), width = 2
                       )
              )
              
      ),
      
      tabItem(tabName = "time",
              selectInput(inputId = "sel",
                          label = 'Select category',
                          choices = choice_time,
                          selected = choice_time), #select shows at the top
              
              fluidRow(plotOutput("time")) 
      ),          
      
      tabItem(tabName = "data",
              fluidRow(box(DT::dataTableOutput("table"),
                           width = 12))
      )
      

    ) #close tabItems
    
  ) #close dashboardBody
  
) #close dashboardPage
) #close shinyUI
