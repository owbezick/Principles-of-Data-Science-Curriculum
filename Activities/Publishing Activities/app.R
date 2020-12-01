library(readr)
library(dplyr)
library(scales)
library(tidyverse)
library(mapproj)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(dplyr)
library(colorspace)  
library(broom)        
library(tmap)
library(maps)
library(viridis)
library(threejs)
library(knitr)



# Define UI for application
ui <- fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    # Application title
    titlePanel(
        h1("Shiny Activity Example", align = "center")
        ),
    fluidRow(
        column(uiOutput("variable_picker"), width = 6, align = "center")
        , column(uiOutput("state_picker"), width = 6, align = "center")
    )
    , fluidRow(
        column(plotOutput("country_graph"), width = 6, align = "center")
        , column(plotOutput("state_graph"), width = 6, align = "center")
    )
)

# Define server
server <- function(input, output) {

    census_data <- read_csv("census_data.csv")
    
    ls_states <- census_data %>%
        select(state) %>%
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
    
    output$variable_picker <- renderUI({
        pickerInput(
            "variable"
            , label = "Select a variable"
            , choices = c("Population" = "popEstimate16", "Unemployment Rate" = "unemployment",
                          "Median Income" = "medianIncome16", "Percent Voted for Trump" = "prcntGOP16")
        )
    })
    
    graph_data <- reactive({
        req(input$state)
        subset(census_data, subset = (state == input$state))
    })
    
    
    output$state_graph <- renderPlot({
        state_df <- graph_data()
        req(input$variable)
        choice <- input$variable
        key_label <- if (choice == "popEstimate16") {"Population Estimate"} 
                        else if (choice == "unemployment") {"Unemployment Rate"}
                        else if (choice == "medianIncome16") {"Median Income ($)"}
                        else {"Percent that Voted for Trump in 2016"}
        ggplot(
            data = state_df
            , aes(
                x = long
                , y = lat
                , group = group
                , fill = get(choice)
            )
        ) +
            geom_polygon(
                color = "gray90"
                , size = 0.05
            ) + 
            coord_map(
                projection = "albers"
                , lat0 = 42
                , lat1 = 45
            ) +
            scale_color_gradient(pretty_breaks(n=5)) + 
            labs(fill = key_label) 
    })
    
    output$country_graph <- renderPlot({
        req(input$variable)
        choice <- input$variable
        key_label <- if (choice == "popEstimate16") {"Population Estimate"} 
                        else if (choice == "unemployment") {"Unemployment Rate"}
                        else if (choice == "medianIncome16") {"Median Income ($)"}
                        else {"Percent that Voted for Trump in 2016"}
        ggplot(
            data = census_data
            , aes(
                x = long
                , y = lat
                , group = group
                , fill = get(choice)
            )
        ) +
            geom_polygon(
                color = "gray90"
                , size = 0.05
            ) + 
            coord_map(
                projection = "albers"
                , lat0 = 42
                , lat1 = 45
            ) +
            scale_color_gradient(pretty_breaks(n=5)) + 
            labs(fill = key_label) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
