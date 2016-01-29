library(igraph)
source('E:/SUNYALBANY/Pension_Simulation_Project/Code/simulation_shiny/functions.R')
g<-sample_gnp(100,0.01)
plot(g)
seeds<-c('55','52','46')
t<-1
V(g)$color <- 'white'
color_seeds(g,seeds)
infected <- seeds
runs<-2

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



