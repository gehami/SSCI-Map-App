#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
if(!require(shinydashboard)){
  install.packages("shinydashboard")
  require(shinydashboard)
}
library(shinydashboard)
if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}
if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}
if(!require(DT)){
  install.packages("DT")
  library(DT)
}
ma = readRDS("SSCI_IR_ma_16-18_for_plotting.rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(sidebarLayout(
  
  # Application title
  # titlePanel("Old Faithful Geyser Data"),
  sidebarPanel(
    # h4(htmlOutput("text")),
    h3("Show Incident Levels: "),
    checkboxGroupInput("Levels", "", choices =  c("1", "2", "3"), selected = c("1", "2", "3")),
    h4("Show schools in council districts: "),
    checkboxGroupInput("cd", "", choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), selected = NULL),
    sliderInput("timeframe",
                "Time Range to include",
                min = min(ma$date, na.rm = T),
                max = max(ma$date, na.rm = T),
                value = c(min(ma$date, na.rm = T), max(ma$date, na.rm = T))),
    h3(paste0("Top Schools for selected Levels")),
    dataTableOutput("Top_10")
  ),
  mainPanel(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    box(
      title = "SSCI Incident Reports by school and level, 2016-2018",
      collapsible = F,
      width = "100%",
      height = "900px",
      leafletOutput("map",width="100%",height="600px"),
      box(
        collapsible = F,
        width = "100%",
        height = "300px",
        column(2, radioButtons("time_frame", "Choose Time Scale", choices = c("Daily", "Weekly", "Monthly"), selected = "Weekly"),
               htmlOutput("graph_text")),
        column(10, plotlyOutput("plot", width = "100%", height = "300px"))
      )
    )
    # leafletOutput("map")
    )

)))



###### So things you can add to this map ######
# Click on a school and it's boundary for enrollment pops up 
# on the right a sheet of facts about the school can pop up
#   things like number of kids, number of incidents, how much best funding is at the school
#   and within it's boundary. How much crime occurs within it's boundaries, present gangs
# so leaflet can be interactive with shiny, we have some examples that to work with, and we can have a dataset which marks which layers are on and which are off.
# 
# 
# 
# 
# 
