#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shinyWidgets)
library(readr)
library(dplyr)
library(scales)
library(colorspace)  
library(broom)        
library(tidyverse)    
library(tmap)
library(maps)
library(viridis)
library(igraph)
library(threejs)
library(knitr)

ui <- dashboardPage(
    # Application Title ----
    dashboardHeader(title = "Pages")
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
                tabName = "graph", text = "Graph Networks", icon = icon("hubspot")
            )
        )
    )
    # Application Body ----
    , dashboardBody(
        tabItems(
            tabItem(
                # First Page ----
                tabName = "homepage"
                , box(title = HTML("<strong><center> Principles of Data Science Curriculum </center></strong>"), 
                     "Authors: Owen Bezick, Rachel Hendricks, Drew Dibble,
                     and Dayton Simenz", status = "primary", width = 12)
                , box(img(src = "Davidson_Logo.png"), align = "center", width = 12)
                , box(textOutput("purpose"), width = 12)
                , box(title = strong("Course Description"), 
                      textOutput("course_description"), width = 12
                )
            )
            # Second Page ----
            , tabItem(
                tabName = "map"
                , box(title = strong("Geo-Spatial Data"), width = 12,
                          textOutput("geo_description"))
                , box(width = 12,
                      fluidRow(
                          column(uiOutput("variable_picker"), width = 6)
                          , column(uiOutput("state_picker"), width = 6)
                      )
                      , fluidRow(
                          column(plotOutput("country_graph"), width = 6)
                          , column(plotOutput("state_graph"), width = 6)
                        )
                    )
            )
            # Third Page ----
            , tabItem(
                tabName = "graph"
                , box(title = HTML("<strong> Analyzing the TV show <em>Friends</em> Using a Graph Network </strong>"), 
                      width = 12, textOutput("network_description"))
                , box(width = 12,
                    fluidRow(
                          column( plotOutput("friends_graph"), width = 4)
                          , column(plotOutput("friends_bar"), width = 6)
                          , column(uiOutput("character_picker"), width = 2)
                        )
                )
            )
        )
    )
)


server <- function(input, output) {
    
    output$purpose <- renderText({
                      "The purpose of this app is to show what is possible with R
                      and Shiny as well as to give you a taste of some of the topics
                      you will learn throughout this course. The description
                      for our repository is below."
    })
    
    output$course_description <- renderText({
                        "The goal of this repository is to build an open-sourced
                        curriculum for learning the principles of data science
                        in the R programming language. A use case for this
                        repository would be the data science minor at Davidson
                        College. The coding sections of various classes within
                        the minor often contain areas of overlap, which creates
                        a lack of development for students that already have
                        experience in the minor, so a repository such as this
                        one would allow students to develop at their own pace.
                        This course will cover the basic skills as well as 
                        additional and intriguing skills of data science. The 
                        point of the course is not to \"reinvent the wheel\" 
                        by reteaching material to students who have already 
                        taken data science courses or may know of skills from
                        other courses, but rather to provide one location with
                        easy access to lessons and exercises so students can 
                        teach themselves at their own pace. These lessons and
                        exercises were designed with advancement in data science
                        studies in mind while also trying to keep the material
                        engaging for anyone who uses them. Therefore, this 
                        course supports flexibility in choosing and ordering
                        topics. Students may follow the order of modules as
                        pre-set by the designers of the course, or may pick and
                        choose topics that are relevant to the student's
                        interests. Activities will be available for each module,
                        with starter code provided, as well as answer keys to
                        check a student's answers."
    })
    
    output$geo_description <- renderText({
        "In activity I of the Data Wrangling and Visualization section, you will learn
         how to create the following visuals and many others based on census data.
         Choose a variable to investigate nationwide and in the state of your choice." 
    })
    
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
            , choices = colnames(census_data)
        )
    })
 
    graph_data <- reactive({
        req(input$state)
        subset(census_data, subset = (state == input$state))
    })
    
    output$state_graph <- renderPlot({
        state_df <- graph_data()
        choice <- input$variable
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
            labs(fill = "Selected Variable") 
    })
    
    #to do: fix fill for state ----
    output$country_graph <- renderPlot({
        req(input$variable)
        choice <- input$variable
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
            labs(fill = "Selected Variable") 
    })
    
    
    
    output$network_description <- renderText({
        "In activity II of the Data Wrangling and Visualization section, you will get to
        investigate the interactions of the characters in the TV show friends. By the end
        of the activity, you will be able to create both of the visualizations, among others.
        The first visual is a graph network that shows the communities of the five main
        characters and how they intersect. The histogram visual dives into the social patterns
        of a specific character. Specifically, it shows the number of times that the chosen
        character has talked to each of his \"friends\", which, in this case, signifies 
        anyone that he/she has spoken to more than fifteen times. Play around with the
        input to investigate the social patterns of each of the main characters in the show."
        
    })
    
    friends_edgelist <- readRDS("edgelist.RDS")
    friends <- c("Phoebe", "Monica", "Rachel", "Joey", "Ross", "Chandler") 
    edgelist_without <- friends_edgelist %>% 
        dplyr::filter(!(from %in% friends & to %in% friends))
    edgelist_matrix <- as.matrix(edgelist_without[ ,c("from", "to")]) 
    friends_graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
        igraph::set.edge.attribute("weight", value = edgelist_without$weight)
    
    # run louvain with edge weights 
    louvain_partition <- igraph::cluster_louvain(friends_graph, weights = E(friends_graph)$weight) 
    # assign communities to graph 
    friends_graph$community <- louvain_partition$membership 
    # see how many communities there are 
    unique(friends_graph$community) 
    communities <- data.frame() 
    for (i in unique(friends_graph$community)) { 
        # create subgraphs for each community 
        subgraph <- induced_subgraph(friends_graph, v = which(friends_graph$community == i)) 
        # get size of each subgraph 
        size <- igraph::gorder(subgraph) 
        # get betweenness centrality 
        btwn <- igraph::betweenness(subgraph) 
        communities <- communities %>% 
            dplyr::bind_rows(data.frame(
                community = i, 
                n_characters = size, 
                most_important = names(which(btwn == max(btwn))) 
            ) 
            ) 
    }
    top_five <- data.frame() 
    for (i in unique(friends_graph$community)) { 
        # create subgraphs for each community 
        subgraph <- induced_subgraph(friends_graph, v = which(friends_graph$community == i)) 
        # for larger communities 
        if (igraph::gorder(subgraph) > 20) { 
            # get degree 
            degree <- igraph::degree(subgraph) 
        } 
    }
    # Scaling by degree and coloring by community
    V(friends_graph)$size <- 3 
    V(friends_graph)$frame.color <- "white" 
    V(friends_graph)$color <- friends_graph$community 
    V(friends_graph)$label <- V(friends_graph)$name 
    V(friends_graph)$label.cex <- 1.5 
    # Coloring by speaker
    edge.start <- ends(friends_graph, es = E(friends_graph), names = F)[,1] 
    E(friends_graph)$color <- V(friends_graph)$color[edge.start] 
    E(friends_graph)$arrow.mode <- 0 # only label central characters 
    v_labels <- which(V(friends_graph)$name %in% friends) 
    for (i in 1:length(V(friends_graph))) { 
        if (!(i %in% v_labels)) { V(friends_graph)$label[i] <- "" } 
    }
    
    l2 <- layout_with_mds(friends_graph)
    
    output$friends_graph <- renderPlot({
        plot(friends_graph, rescale = T, layout = l2, main = "'Friends' Network - All Seasons")
    })
    
    
    output$character_picker <- renderUI({
        pickerInput(
            "character"
            , label = "Select a character"
            , choices = friends
            , selected = friends[1]
        )
    })
    
    specific_friend <- reactive(filter(friends_edgelist, from == input$character) %>%
                                filter(weight > 15) %>% arrange(desc(weight)
                            ))
    
    output$friends_bar <- renderPlot({
        req(input$character)
        character_chosen <- specific_friend()
        ggplot(character_chosen, aes(x = to, y = weight )) + geom_bar(stat = "identity", fill = "Blue") +
            ggtitle(paste(input$character, "'s Friends", sep="")) + xlab("Friend") + ylab("Times Spoken to") + 
            scale_fill_brewer(palette = "Blues")
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
