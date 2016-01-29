library(igraph)
giant_component <- function(g) {
  v <- components(g)
  comp_no <- match(max(components(g)$csize),components(g)$csize)
  m <- (c(which(components(g)$membership == comp_no)))
  return(m)
}
max_degree <- function(g,ns) {
  v <- c(0)
  for (i in giant_component(g))
    v <- append(v,degree(g,c(i)))
  v <- v[2:length(v)]
  names(v) <- giant_component(g)
  
  if (ns == 1)
    max_degree <- as.numeric(names(sort(v,decreasing = TRUE)))[1]
  else
    max_degree <- as.numeric(names(sort(v,decreasing = TRUE)))[1:ns]
  return(max_degree)
}

max_degree_value <- function(g,ns) {
  v <- c(0)
  for (i in giant_component(g))
    v <- append(v,degree(g,c(i)))
  v <- v[2:length(v)]
  names(v) <- giant_component(g)
  
  if (ns == 1)
    max_degree <- as.numeric(sort(v,decreasing = TRUE))[1]
  else
    max_degree <- as.numeric(sort(v,decreasing = TRUE))[1:ns]
  return(max_degree)
}
# 
# infect <- function(g,runs,t,seeds)
# {
#   V(g)$color <- 'white'
#   for (i in seeds)
#     V(g)[match(i,(V(g)))]$color <- 'light blue'
#   infected <- seeds
#   if (runs != 0 && t != 0) {
#     for (p in seq(1:runs)) {
#       n1 <- character(0)
#       n4 <- character(0)
#       for (i in infected) {
#         for (j in neighbors(g,i)) {
#           n1 <- append(n1,j)
#           n2 <- sapply(n1, function(k)
#             neighbors(g,k))
#           n3 <-
#             sapply(n2[1:length(n1)], function(l)
#               length(intersect(l,infected)))
# 
#           for (n in 1:length(n3)) {
#             if (n3[n] >= t)
#               infected <- unique(append(infected,names(n2)[n]))
#           }
#         }
#       }
#     }
#   }
#   for (o in infected)
#     V(g)[match(o,(V(g)))]$color <- 'light blue'
#   return(g)
# }
# 

infected_nodes <- function(g) {
  return(V(g)[V(g)$color == 'light blue'])
}

color_seeds <- function(g,s)
{
  for (i in s)
    V(g)[match(i,(V(g)))]$color <- 'light blue'
  
}

infect <- function(g,runs,t,seeds)
{
  V(g)$color <- 'white'
  color_seeds(g,seeds)
  infected <- seeds
  
  if (runs != 0 && t != 0) {
    for (p in seq(1:runs)) {
      ifelse(p==1,n1 <- sapply(seeds, function(l) neighbors(g,l)),
             n1 <- sapply(n3, function(l) neighbors(g,l)))
      n2 <- sapply(n1, function(k)
        sapply(k, function(p) if(length(intersect(neighbors(g,p),infected)) >= t)return(p)))
      n3 <- unlist(n2)
      infected <- unique(append(infected,n3))
    }
    
  }
  for (i in infected)
    V(g)[match(i,(V(g)))]$color <- 'light blue'
  
  
  
  
  return(g)
  
}