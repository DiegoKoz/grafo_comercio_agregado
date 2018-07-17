# Trabajo con el año 2011. PAra en análisis de comunidades utilizamos el cluster no dirigido

rm(list = ls())
gc()
library(tidyverse)
library(ggthemes)
library(ggridges)
library(igraph)
library(countrycode)

 trade_to_graph <- function(edges, threshold_pct = .01){
  edges <- edges %>% 
    group_by(rt3ISO) %>% 
    mutate(L = case_when(TradeValue>sum(TradeValue)*threshold_pct~1,
                         TRUE ~ 0)) %>% 
    na.omit(.)
  
  edges <- edges %>% filter(L==1)
  
  DF_red <- graph_from_data_frame(edges,directed = FALSE)
  
  nombre_pais <- left_join(data_frame(cod = V(DF_red)$name),  countrycode_data %>%
                             select(cod = iso3c, pais = country.name.en))
  
  V(DF_red)$pais <- nombre_pais$pais
  
  return(DF_red)
}

 
 
 
##### Datasets #####
dataset <- readRDS('Dataset/dataset_COMTRADE_2011.rds')
dataset <- dataset %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue)
dataset <- left_join(dataset,countrycode_data 
                      %>% select(rt3ISO=iso3c, continent, country.name.en)) %>% 
  na.omit(.)

grafo <- trade_to_graph(edges = dataset)

#### Red de mundo pequeño ####

grafo_plf <- power.law.fit(degree(grafo,mode = "out"))

grafo_plf$alpha
# 2.721918
# alpha>1 OK
grafo_plf$KS.p
# 0.4752098
# Test Kolmogorov-Smirnov significativo OK
grafo_plf$xmin
# 20
# xmin se corresponden a valores de grado bajos NO
summary(degree(grafo,mode = "out"))

grafo_plf <- power.law.fit(degree(grafo,mode = "out"),xmin = 13)

grafo_plf$alpha
# 2.186869
# alpha>1 OK
grafo_plf$KS.p
# 0.0003278631
# Test Kolmogorov-Smirnov no significativo NO

### Comparación con redes generadas al azar
rg.transitivity.barabasi <- array()
rg.transitivity.erdos <- array()
for(i in 1:1000){
  rg.1 <- barabasi.game(43, power = 2.4, m=8, directed = F)
  rg.2 <- sample_gnm(43, 336)
  rg.transitivity.barabasi[i] <- mean(transitivity(rg.1, "local", isolates="zero"))
  rg.transitivity.erdos[i] <- mean(transitivity(rg.2, "local", isolates="zero"))
}

png(filename = "graficos/Barabassi.png")
red.transitivity <- mean(transitivity(grafo$grafo, "local", isolates="zero"))
table(red.transitivity > rg.transitivity.barabasi)
hist(rg.transitivity.barabasi,xlim = c(0.5,0.8))#, main = "coef. clustering, grafos de Barabasi-Albert")
abline( v = red.transitivity, col ="red", lwd= 2)
dev.off()
png(filename = "graficos/Erdos.png")
table(red.transitivity < rg.transitivity.erdos)
hist(rg.transitivity.erdos, xlim=c(0.34, 0.55))#, main = "coef. clustering, grafos al azar")
abline( v = red.transitivity, col ="red", lwd= 2)
dev.off()

# No se puede afirmar que sea una red de mundo pequeño.

##### Clustering #####

grafo_continente <- function(threshold_pct = 0.01, data = dataset){
  
  grafo <- trade_to_graph(edges = data, threshold_pct = threshold_pct)
  set.seed(12345)
  grafo.cl <- cluster_louvain(graph = grafo)
  V(grafo)$membership <- grafo.cl$membership
  nombres_vertices <- data_frame(rt3ISO=as.vector(V(grafo)$name),
                                 membership = factor(V(grafo)$membership))
  countrycode_data2 <- countrycode::countrycode_data %>% select(rt3ISO = iso3c, continent)
  
  correspondencia <- left_join(nombres_vertices,countrycode_data2) %>% 
    mutate(color = case_when(continent == "Europe" ~"#E41A1C",
                             continent == "Africa" ~"#377EB8",
                             continent == "Asia" ~"#4DAF4A",
                             continent == "Oceania" ~"#984EA3",
                             continent == "Americas" ~"#FF7F00"),
           shape = case_when(membership == 1 ~"circle",
                             membership == 2 ~"square",
                             membership == 3 ~"sphere",
                             membership == 4 ~"vrectangle",
                             membership == 5 ~"rectangle"))
  
  V(grafo)$continente <- correspondencia$continent
  V(grafo)$color <- correspondencia$color
  V(grafo)$shape <- correspondencia$shape
  E(grafo)$edge.color <- "gray80"
  l <-layout.graphopt(grafo)
  png(paste0("graficos/","grafo_2011_comunidades.png"),width=800,height=700,res=72)
  
  plot.igraph(grafo,edge.arrow.size=.2,
              vertex.frame.color="#ffffff",
              vertex.label="",  vertex.size = 10/V(grafo)$membership, 
              layout=l)#, main= "Clustering comunidades. Algoritmo louvain
# Tamaño y forma según comunidad 
# 2011. threshold 1%")
  legend(x=1,  y=.5,unique(unique(correspondencia$continent)),
         pch=21,col="#777777",pt.bg=unique(unique(correspondencia$color)),
         pt.cex=2,cex=.8,bty="n",ncol=1)
  dev.off()
  
  return(list("grafo" =grafo,"grafo.cl" = grafo.cl))
}


grafo <- grafo_continente(threshold_pct = 0.01, data = dataset)

grafo$grafo.cl

#Un array de modularidades al azar
random.membership <- array()
# Valores de modularidad para 1000 agrupamientos al azar
for(i in 1:1000){
  random.membership[i] <- modularity(grafo$grafo, sample(1:5, length(V(grafo$grafo)),
                                                    replace = T))
} 
table( modularity(grafo$grafo, grafo$grafo.cl$membership) > random.membership )
# TRUE 
# 1000
#El clustering es altamente significativo.

