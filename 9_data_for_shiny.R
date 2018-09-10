# Dataset Gleditsch. 1948-2000

library(tidyverse)
library(ggthemes)
library(ggridges)
library(ggrepel)
library(igraph)
library(countrycode)
library(xlsx)


countrycode_data <- codelist

##### Funciones #####

trade_to_graph <- function(edges, threshold_pct = .01) {
  edges <- edges %>% 
    group_by(rt3ISO) %>% 
    mutate(L = case_when(TradeValue>sum(TradeValue)*threshold_pct~1,
                         TRUE ~ 0)) %>% 
    na.omit(.)
  
  edges <- edges %>% filter(L==1)
  
  DF_red <- graph_from_data_frame(edges,directed = TRUE)
  
  nombre_pais <- left_join(data_frame(cod = V(DF_red)$name),  countrycode_data %>%
                             select(cod = cowc, pais = country.name.en))

  V(DF_red)$pais <- nombre_pais$pais
  
  connected_weak <- is_connected(DF_red, mode = 'weak')
  connected_strong <- is_connected(DF_red, mode = 'strong')
  diameter <- diameter(DF_red, directed = TRUE,unconnected = TRUE)
  density <- graph.density(DF_red)
  
  gsize <- gsize(DF_red)
  
  ## Centralidad
  # Centralidad de intermediación
  max_betweenness <- max(betweenness(DF_red, directed = T))
  mean_betweenness <- mean(betweenness(DF_red, directed = T))
  
  # Centralidades de cercanía 
  mean_closeness <- mean(closeness(DF_red, mode = "all"))
  
  #autovalor 
  mean_eigen_centrality <- mean(eigen_centrality(DF_red, directed = F)$vector)
  mean_eigen_centrality_ponderado <- mean(eigen_centrality(DF_red, directed = F,
                                                           weights = E(DF_red)$TradeValue)$vector)
  
  #Grado
  mean_degree <- mean(degree(DF_red))
  #Transitividad
  coef_clustering <- transitivity(DF_red,isolates = 'zero')
  #correlacion de grado (Red selectiva o no selectiva)
  correlacion <- assortativity_degree(DF_red, directed = F)
  
  
  return(list("grafo" =DF_red,
              'connected_weak' = connected_weak, 
              'connected_strong' = connected_strong,
              'diameter' = diameter,
              'density' = density, 
              'naristas'= gsize, 
              "mean_betweenness" = mean_betweenness,
              "max_betweenness" = max_betweenness,
              "mean_closeness" = mean_closeness,
              "mean_degree" = mean_degree,
              "mean_eigen_centrality" = mean_eigen_centrality,
              "mean_eigen_centrality_ponderado" = mean_eigen_centrality_ponderado,
              "coef_clustering" = coef_clustering,
              "correlacion" = correlacion))
}

##### Datasets #####

trade_dd <- read_delim("data/trade_dd.asc",delim = " ")

# Caracterización del dataset
codigos <- countrycode_data  %>%  
  select(rt3ISO=cowc, rtTitle = country.name.en,  continent) 

dataset_impo <- trade_dd %>% 
  select(rt3ISO = acra,pt3ISO = acrb,TradeValue = impab, yr = year) %>% 
  mutate(rt3ISO = case_when(rt3ISO == "FJI" ~ "FIJ",
                            rt3ISO == "RUM" ~ "ROM",
                            TRUE ~ rt3ISO)) %>% 
  left_join(codigos) %>% na.omit(.)
#expo
dataset_expo <- trade_dd %>% 
  select(rt3ISO = acra,pt3ISO = acrb,TradeValue = expab, yr = year) %>% 
  mutate(rt3ISO = case_when(rt3ISO == "FJI" ~ "FIJ",
                            rt3ISO == "RUM" ~ "ROM",
                            TRUE ~ rt3ISO)) %>% 
  left_join(codigos) %>% na.omit(.)

#### loop impo ####
distribuciones_impo <- data_frame()
for (nano  in sort(unique(dataset_impo$yr))) {
  print(paste0("Año: ",nano))
  dataset2 <- dataset_impo %>% filter(yr == nano)
  net <- trade_to_graph(dataset2, threshold_pct = 0.01)
  
  dist <- data.frame("yr" = nano,
                     'cod' = V(net$grafo)$name,
                     'pais' = V(net$grafo)$pais,
                     "betweenness" = betweenness(net$grafo, directed = T),
                     "degree" = degree(net$grafo),
                     "autovalor" = eigen_centrality(net$grafo, directed = F)$vector,
                     "autovalor_pond" = eigen_centrality(net$grafo, directed = F,
                                                         weights = E(net$grafo)$TradeValue)$vector)
  distribuciones_impo <-  bind_rows(distribuciones_impo, dist)
  
}


#### loop expo ####
distribuciones_expo <- data_frame()
for (nano  in sort(unique(dataset_expo$yr))) {
  print(paste0("Año: ",nano))
  dataset2 <- dataset_expo %>% filter(yr == nano)
  net <- trade_to_graph(dataset2, threshold_pct = 0.01)
  
  #distribuciones de intermediacion | grado | autovalor | autovalor ponderado
  
  dist <- data.frame("yr" = nano,
                     'cod' = V(net$grafo)$name,
                     'pais' = V(net$grafo)$pais,
                     "betweenness" = betweenness(net$grafo, directed = T),
                     "degree" = degree(net$grafo),
                     "autovalor" = eigen_centrality(net$grafo, directed = F)$vector,
                     "autovalor_pond" = eigen_centrality(net$grafo, directed = F,
                                                         weights = E(net$grafo)$TradeValue)$vector)
  distribuciones_expo <-  bind_rows(distribuciones_expo, dist)
  
}


distribuciones_impo %>%
  rename(Intermediacion = betweenness,
         Grado = degree,
         Autovalor = autovalor,
         `Autovalor ponderado`= autovalor_pond) %>% 
  mutate(yr = factor(yr)) %>% 
saveRDS(., "distribuciones_impo.RDS")

distribuciones_expo %>% 
rename(Intermediacion = betweenness,
       Grado = degree,
       Autovalor = autovalor,
       `Autovalor ponderado`= autovalor_pond) %>% 
  mutate(yr = factor(yr)) %>% 
saveRDS(., "distribuciones_expo.RDS")

