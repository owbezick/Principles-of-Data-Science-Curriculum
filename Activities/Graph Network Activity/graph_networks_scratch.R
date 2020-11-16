# This activity will introduce you to graph network analysis in R. While the 
# activity will begin with the creation of a basic data set, and the answers 
# will reflect those that correspond with said data set, this activity is 
# meant to be compatible with any data set properly fit for graph netowrks.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First, it is necessary to install and library relevant packages:
#install.packages("igraph")
#install.packages("tidyverse")
#install.packages("threejs")
#install.packages("readxl")
# (readxl will not be used in this activity but will likely be helpful when
#   working with your own data set)
library(igraph)
library(tidyverse)
library(threejs)
library(readxl) 
library(readr)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Working with Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To create and analyze a graph network, you need a data set with a specific set
# of characteristics; these will be shown and explained below. To yield a 
# thorough understanding, this activity will be done both with a simple data set
# created below and a larger, imported data set containing the dialogue 
# relations between characters in the tv show, 'Friends'.

# When creating our own data, there are two requirements: a list of nodes and
# a list of edges. The node list is a simple one column table containing every 
# node in the network. The edge list, however, contains a minimum of two 
# columns, which outline the relationships between nodes. Below is a simple 
# example of these two lists:

node_list <- tibble(id = 0:5)
edge_list <- tibble(root = c(0, 0, 0, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 5),
                    dest = c(1, 2, 4, 2, 5, 0, 1, 5, 2, 4, 2, 3, 1, 2, 4))

# To import data, you need to use commands specific to the type of file being 
# imported (CSV, Excel, URL, etc.); in this case, the data is being imported
# from the UR: below. The commands needed to tidy each data set will very 
# with each one's individual characteristics. Below are the commands necessary
# to tidy this set, which will end up being a list outlining various cases of
# dialogue by listing the speaker and the recipient of the speech.

edgefile_url <- "https://github.com/keithmcnulty/friends_analysis/blob/master/data/friends_full_series_edgelist.RDS?raw=true" 
download.file(edgefile_url, "edgelist.RDS") 
fr_edgelist <- readRDS("edgelist.RDS") 

knitr::kable(edgelist %>% head(10))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Nodes and Edges~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# There are two main characteristics of a graph netowrk: the nodes, which are 
# the individual items that form the netowrk, and the edges, which signify the
# connections between nodes. 

# Once these lists are created, the edge_list is transformed into a graph 
# object using the "graph.data.frame" command as shown below. Directed 
# indicates whether edges have direction (indicating a one-sided relationship)
# or if all edges are reciprocal, meaning directed = FALSE.

graph <- graph.data.frame(edge_list, directed = TRUE)

# For the friends graph, we'll use the dplyr 'filter' command to filter out any 
# communities shared by the main characters; this allows further analysis into 
# the commmunities formed around each character.

friends <- c("Phoebe", "Monica", "Rachel", "Joey", "Ross", "Chandler") 
edgelist_without <- edgelist %>% 
  dplyr::filter(!(from %in% friends & to %in% friends))

edgelist_matrix <- as.matrix(edgelist_without[ ,c("from", "to")]) 
friends_graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
  igraph::set.edge.attribute("weight", value = edgelist_without$weight)

# Once transformed into a graph opject, you can gather information about the 
# network such as the number of edges and vertices, the network's diameter 
# (indicates largest distance between points), and data about individual nodes.
# Find: a) the number of edges and vertices within the network
#       b) the number of nodes (or vertices) in the network
#       c) the number of edges stemming from node 0
#       d) the diameter of the graph network

E(graph) # Indicates 15 edges
E(friends_graph) # Indicates 2961 edges signifying different instances of speech
V(graph) # Indicates 6 nodes
V(friends_graph) # Indicates 650 nodes, signifying 650 different speakers
degree(graph, v = '0') # Indicates that node 0 is connected to five total edges
degree(friends_graph, v = 'Joey') #Indicates that Joey interacted with 348
#                                   characters
get_diameter(graph) # Indicates a diameter of four
get_diameter(friends_graph) # Indicates that the greatest distance between 
#                             any characters is five degrees of separation

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~Visualizing the Network~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now that a basic understanding of the various characteristics of a graph 
# network has been established, it is time to visualize the network!

# First, it is necessary to format the data as an ego graph, using the command 
# make_ego_graph. 

graph_ego <- make_ego_graph(graph, 3, '0', mode = c('all'))[[1]]
fr_graph_ego <- make_ego_graph(friends_graph, 1, 'Joey', mode = c('all'))[[1]]
plot(graph_ego)
plot(fr_graph_ego)

# If you're trying to visualize the level of connectedness between the netowork
# and a specific node, it can be helpful to add colors as representatives of 
# connectedness. To do this, create an item 'colors' that is a group of different
# colors associated with different degrees of connectedness. Note, the 'c' prior 
# to the parentheses is what distinguishes colors as a single item, rather than 
# a list or group of multiple entities. By attaching the vertices of the network
# to colors, and clarifying the relation between the colors and degrees of 
# connectedness, the graph is now color-coded to represent the closeness of all
# nodes to the ego, or center of the network.

colors <- c("red", "orange","dodgerblue", "cyan")

dists <- distances(graph_ego, v = '0')
fr_dists <- distances(fr_graph_ego, v = 'Joey')

V(graph_ego)$color <- colors[dists+1]
V(fr_graph_ego)$color <- colors[fr_dists+1]
plot(graph_ego, edge.arrow.size = .05)
plot(fr_graph_ego, edge.arrow.size = .05)


# As you can see with the custom data set, colors can be a nice way of 
# showing differing levels of connectivity. However, in the much larger 
# Friends data set, the graph is still a jumbled mess as there are simply
# too many points to show in a comprehendable way. In cases like this, it
# can be helpful to use the Louvain algorithm to decompose the netowk into 
# distinct communities. From this point we will be working exclusivly with 
# the Friends network, as these commands are really only necessary with larger,
# more complex networks.

# run louvain with edge weights 
louvain_partition <- igraph::cluster_louvain(friends_graph, weights = E(friends_graph)$weight) 
# assign communities to graph 
friends_graph$community <- louvain_partition$membership 
# see how many communities there are 
unique(friends_graph$community) 

# When running the 'unique' command, the values returned indicate that there
# are eight distinct communities within the network. To learn more, when can
# analyze the size and centrality of each subgraph using community using gorder
# and betweenness commands within the igraph package; we can then learn the 
# members of each group using dplyr commands to sift through the data.

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
knitr::kable(
  communities %>% 
    dplyr::select(community, n_characters, most_important)
)
# This produces a list of each community, the number of community members, and
# the most important or central member within each group. Since a few of 
# these groups seem insignificant given their small membership count and 
# seemingly random group leaders, we'll only focus on the top five groups.
# The commands below will create subgraphs for each community and show the top
# five members within each, to give a better idea of these communities and the 
# commonalities that bind them. By making this action exclusive to groups with
# over 20 members, the smaller, random groups will automatically be sifted out, 
# leaving only data on important, larger groups to be shown.

top_five <- data.frame() 
for (i in unique(friends_graph$community)) { 
  # create subgraphs for each community 
  subgraph <- induced_subgraph(friends_graph, v = which(friends_graph$community == i)) 
  # for larger communities 
  if (igraph::gorder(subgraph) > 20) { 
    # get degree 
    degree <- igraph::degree(subgraph) 
    # get top five degrees 
    top <- names(head(sort(degree, decreasing = TRUE), 5)) 
    result <- data.frame(community = i, rank = 1:5, character = top) 
  } else { 
    result <- data.frame(community = NULL, rank = NULL, character = NULL) 
  } 
  top_five <- top_five %>% 
    dplyr::bind_rows(result) 
} 

# Now, 'top_five' shows the top five members of each group. For fans of the show, 
# it's obvious that these groups each signify social circles unique to the main 
# characters.

# Now that we undestand the composition of each community, 

# give our nodes some properties, incl scaling them by degree and coloring them 
# by community 
V(friends_graph)$size <- 3 
V(friends_graph)$frame.color <- "white" 
V(friends_graph)$color <- friends_graph$community 
V(friends_graph)$label <- V(friends_graph)$name 
V(friends_graph)$label.cex <- 1.5 
# also color edges according to their starting node 
edge.start <- ends(friends_graph, es = E(friends_graph), names = F)[,1] 
E(friends_graph)$color <- V(friends_graph)$color[edge.start] 
E(friends_graph)$arrow.mode <- 0 # only label central characters 
v_labels <- which(V(friends_graph)$name %in% friends) 
for (i in 1:length(V(friends_graph))) { 
  if (!(i %in% v_labels)) { V(friends_graph)$label[i] <- "" } 
}

# Once this work is done, the netowrks can be plotted in a meaningful way that
# shows the central nodes and indicates the connection of each node to its 
# specific group. The first layout is the mot visually pleasing option, but 
# the second is most effective as it weights each point by its grouping,
# showing the most accurate representation of each point's relationship
# with one another.

l1 <- layout_on_sphere(friends_graph)
plot(friends_graph, rescale = T, layout = l1, main = "'Friends' Network - All Seasons")

l2 <- layout_with_mds(friends_graph) 
plot(friends_graph, rescale = T, layout = l2, main = "'Friends' Network - All Seasons")







