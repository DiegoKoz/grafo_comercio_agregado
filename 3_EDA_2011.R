rm(list = ls())
gc()
library(tidyverse)
library(igraph)
library(countrycode)

dataset <- readRDS('Dataset/dataset_COMTRADE_2011.rds')


dataset <- dataset %>% select(pt3ISO,rt3ISO,TradeValue, ptTitle)


dataset2 <- left_join(dataset,countrycode_data 
                                %>% select(pt3ISO=iso3c, continent))




##Threshold of 1% del comercio de dicho país. Chow 2013

dataset2 <- dataset2 %>% 
  group_by(rt3ISO) %>% 
  mutate(L = case_when(TradeValue>sum(TradeValue)*0.01~1,
                       TRUE ~ 0)) %>% 
  na.omit(.)

edges <- dataset2 %>% filter(L==1)

g <- graph_from_data_frame(edges,directed = TRUE)

plot(g,edge.arrow.size = 0.1, size =10, curved=TRUE)
lay <- - layout.fruchterman.reingold(g)
plot(g, layout=lay, color = "continent")


### aristas ponderadas

E(g)$weight <- edges$TradeValue

plot(g, edge.width=E(g)$weight/max(E(g)$weight),
     alpha = E(g)$weight/max(E(g)$weight), vertex.size=10,
     edge.arrow.size = .1, edge.arrow.width = .1, 
     loop.angle = 2, asp = .75)

## threshold 2%
dataset2 <- dataset2 %>% 
  group_by(rt3ISO) %>% 
  mutate(L = case_when(TradeValue>sum(TradeValue)*0.02~1,
                       TRUE ~ 0)) %>% 
  na.omit(.)

edges <- dataset2 %>% filter(L==1)

g <- graph_from_data_frame(edges,directed = TRUE)

plot(g,edge.arrow.size = 0.1, size =10, curved=TRUE)







