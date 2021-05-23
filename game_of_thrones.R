library(igraph)

# CSV File: https://github.com/mathbeveridge/asoiaf/blob/master/data/asoiaf-all-edges.csv

alledges = read.csv('https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv', header = TRUE, sep = ",")
head(alledges)

# Keep only Source, Target and Weight columns
data = subset(alledges, select = c('Source', 'Target', 'weight'))
head(data)
str(data)

# Make the network
network = graph_from_data_frame(data, directed=FALSE)
print(network, e=TRUE, v=TRUE) 
plot(network)

############### Question 2 ############### 

# Number of vertices/nodes
vcount(network)
# Number of edges/links
ecount(network)
# Diameter
diameter(network)

#Triangles
plot(network)
# matrix(triangles(network), nrow=3)

## Adjacenct triangles
atri = count_triangles(network)
plot(network, vertex.label=atri)

## Always true
sum(count_triangles(network))
sum(count_triangles(network))== length(triangles(network))


# Top 10 characters (No weights degree)
degrees_noweights = sort(degree(network, v = V(network),loops = TRUE, normalized = FALSE), decreasing = TRUE)##we sort the nodes with the most lnks 
first_10_degrees = head(degrees_noweights, n=10)
first_10_degrees


# Top 10 characters (withweights)
weighted = strength(network, vids = V(network), loops = TRUE, weights = data$weight)
top_10_weighted= head(weighted, n=10)
top_10_weighted


############### Question 3 ############### 

plot(network, layout=layout_with_kk, edge.color="orange",vertex.color="blue", vertex.label = NA, edge.arrow.width=55, vertex.size=2, asp=0,main="Plotting the entire Games Of Thrones network")




# Subgraph
top_vertices = degrees_noweights >= 10
plot(induced_subgraph(network, top_vertices),layout=layout_with_kk,vertex.label = NA, edge.arrow.width=15, vertex.size=2,main="Subgraph / Not less than 10 connection")



# Edge density
density_whole_network=edge_density(network, loops = FALSE)
density_whole_network

density_subgraph=edge_density(induced_subgraph(network, top_vertices), loops = FALSE)
density_subgraph


############### Question 4 ############### 

# Betweenness centrality 
bet = sort(betweenness(network, v = V(network), directed = TRUE, nobigint = TRUE, normalized = FALSE), decreasing = TRUE)
top_15_betweeness = head(bet, n=15)
top_15_betweeness


# Closeness centrality
closenes = sort(closeness(network, vids = V(network), weights = NULL, normalized = FALSE), decreasing = TRUE)
top_15_closenes = head(closenes, n=15)
top_15_closenes


############### Question 5 ############### 
## We compute the page rank values for our network:

pagerank = page_rank(network, algo="prpack" , directed=FALSE, damping = 0.85,
                      personalized = NULL, weights = NULL, options = NULL)$vector
pagerank

#we concert the results into dataframes
page_rank = as.data.frame(pagerank)
page_rank$names <- rownames(page_rank)

sorted_rank = page_rank[order(-pagerank),]

head(sorted_rank)

# For plotting purposes the page rank of the characters are resized
page_rank_resizing = as.numeric(sorted_rank[,1] * 1000)

plot(network, layout=layout_with_kk, vertex.color="blue", vertex.label = NA, edge.arrow.width=15, vertex.size=page_rank_resizing, main="Graph for Higher Ranking")

