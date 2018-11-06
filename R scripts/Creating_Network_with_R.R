

## Visualizing Network Data

setwd("~/Desktop/github/Rstats/data")
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
str(users)
str(edges)

## Creating a Network

install.packages("igraph")
library(igraph)

g <- graph.data.frame(users, TRUE, edges)
plot(g, vertex.size = 5, vertex.label = NA)
degree(g)

table(degree(g) >= 10)

V(g)$size <- degree(g)/2+2
summary(degree(g))

## Coloring Vertices

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
rglplot(g, vertex.label=NA)
