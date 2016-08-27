

rm(list=ls())

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
  if (ns > 1)
    max_degree <- as.numeric(names(sort(v,decreasing = TRUE)))[1:ns]
  if(ns==0)
    max_degree<-'None'
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

infected_nodes <- function(g) {
  infected<-V(g)[V(g)$color == 'light blue']
  return(infected)
}

color_seeds <- function(g,s)
{
  if(!is.null(s)){
    for (i in s)
      V(g)[match(i,(V(g)))]$color <- 'light blue'
  }
}

color_basic<-function(g){
  V(g)$color <- 'white'
}

infect <- function(g,runs,t,seeds)
{

  if(seeds!='None'){
    color_seeds(g,seeds)
  }
  
  infected <- seeds
  
  if (runs != 0 && t != 0 && seeds!='None') {
    for (p in seq(1:runs)) {
      n1 <- unique(sapply(seeds, function(l) neighbors(g,l)))
      seeds<-unlist(n1)
      seeds<-seeds[!seeds%in%infected]
      infected <- unique(append(infected,seeds))
    }
    
  }
  for (i in infected){
    V(g)[match(i,(V(g)))]$color <- 'light blue'
  }
  
  
  
  return(g)
  
}