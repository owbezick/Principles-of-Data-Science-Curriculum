#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shinyWidgets)
library(readr)
library(dplyr)

library(colorspace)  
library(broom)        
library(tidyverse)    
library(tmap)
library(maps)

ui <- dashboardPage(
    # Application Title ----
    dashboardHeader(title = "Test")
    # Application Sidebar -----
    , dashboardSidebar(
        sidebarMenu(
            menuItem(
                tabName = "homepage", text = "Home Page", icon = icon("home")
            )
            , menuItem(
                tabName = "map", text = "Geo-Spatial Data", icon = icon("map")
            )
            , menuItem(
                tabName = "graph", text = "Graph Networks", icon = icon("chart")
            )
        )
    )
    # Application Body ----
    , dashboardBody(
        tabItems(
            tabItem(
                tabName = "homepage"
                , box(title = "Tab One Box", width = 12)
            )
            , tabItem(
                tabName = "map"
                , box(title = "Tab One Box", width = 12
                      , uiOutput("state_picker")
                      , plotOutput("state_graph")
                      )
            )
            , tabItem(
                tabName = "graph"
                , box(title = "Tab One Box", width = 12)
            )
        )
    )
)


server <- function(input, output) {
    
    census_data <- read_csv("census_data.csv")
    
    ls_states <- census_data %>%
        select(state_abbv) %>%
        distinct() %>%
        pull()
    
    output$state_picker <- renderUI({
        pickerInput(
            "state"
            , label = "Select State"
            , choices = ls_states
            , selected = ls_states[1]
        )
    })
 
    
    graph_data <- reactive({
        req(input$state)
        subset(census_data, subset = (state_abbv == input$state))
    })
    
    output$state_graph <- renderPlot({
        state_df <- graph_data()
        ggplot(data = state_df) + 
            geom_point(mapping = aes(x = medianIncome16, y = black))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
