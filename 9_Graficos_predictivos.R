rm(list = ls())
gc()
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(igraph)
library(countrycode)
library(ggrepel)
library(ggridges)
library(ggbiplot)

countrycode_data <- codelist %>% select(rt3ISO=iso3c, continent, country.name.es=un.name.es)
##### Datasets #####
dataset <- readRDS('Dataset/dataset_COMTRADE_2011.rds')
dataset <- dataset %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue)
dataset <- left_join(dataset,countrycode_data) %>% 
  na.omit(.)

#expos
dataset_expo <- readRDS('Dataset/dataset_COMTRADE_expo_2011.rds')
dataset_expo <- dataset_expo %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue)
dataset_expo <- left_join(dataset_expo,countrycode_data) %>% 
  na.omit(.)



#### Correlación de grafo impo-expo ####

trade_to_graph <- function(edges, threshold_pct = .01) {
  edges <- edges %>% 
    group_by(rt3ISO) %>% 
    mutate(L = case_when(TradeValue>sum(TradeValue)*threshold_pct~1,
                         TRUE ~ 0)) %>% 
    na.omit(.)
  
  edges <- edges %>% filter(L==1)
  
  DF_red <- graph_from_data_frame(edges,directed = TRUE)
  
  nombre_pais <- left_join(data_frame(cod = V(DF_red)$name),  countrycode_data %>%
                             select(cod = rt3ISO, pais = country.name.es))
  
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

grafo_continente <- function(threshold_pct, dataset = dataset2){
  lista_grafo <- trade_to_graph(edges = dataset, threshold_pct = threshold_pct)
  
  grafo <- lista_grafo$grafo
  nombres_vertices <- data_frame(rt3ISO=as.vector(V(grafo)$name))
  countrycode_data2 <- countrycode_data %>% select(rt3ISO, continent)
  correspondencia <- left_join(nombres_vertices,countrycode_data2) %>% 
    mutate(color = case_when(continent == "Europe" ~"#E41A1C",
                             continent == "Africa" ~"#377EB8",
                             continent == "Asia" ~"#4DAF4A",
                             continent == "Oceania" ~"#984EA3",
                             continent == "Americas" ~"#FF7F00") )
  
  V(grafo)$continente <- correspondencia$continent
  V(grafo)$color <- correspondencia$color
  E(grafo)$edge.color <- "gray80"
  l <-layout.graphopt(grafo)
  return(grafo)
  
}



correlacion_grado_impoexpo <- function(threshold_pct = 0.01,
                                       data_expo = dataset_expo, 
                                       data_impo = dataset2, 
                                       save = T,
                                       pearson_note = TRUE,
                                       label = TRUE){
  
  grafo_impos <- grafo_continente(threshold_pct = threshold_pct,dataset = data_impo)
  grafo_expos <- grafo_continente(threshold_pct = threshold_pct,dataset = data_expo)
  
  grafo_impo_DF <- data_frame('nodo' = V(grafo_impos)$name,
                              'continente' = V(grafo_impos)$continente,
                              'grado_impo' = degree(grafo_impos))
  
  grafo_expo_DF <- data_frame('nodo' = V(grafo_expos)$name,
                              'continente' = V(grafo_expos)$continente,
                              'grado_expo' = degree(grafo_expos))
  
  grafo_impoexpo_DF <- left_join(grafo_impo_DF, grafo_expo_DF)
  
  grafo_impoexpo_DF <- left_join(grafo_impoexpo_DF,
                                 countrycode_data %>%
                                   select(nodo = rt3ISO, pais = country.name.es) ) %>% 
    na.omit(.)
  
  r <- round(cor(grafo_impoexpo_DF$grado_impo,grafo_impoexpo_DF$grado_expo),2)
  
  if (pearson_note) {
    plot_label <- sprintf("\"pearson's\" ~ rho == %0.2f", r)
  }else{
    plot_label = ""
  }
  
  
  fit <- lm(grado_expo~grado_impo, data = grafo_impoexpo_DF)
  
  grafo_impoexpo_DF$leverage <- hat(model.matrix(fit))
  
  corte_grafico <- mean(grafo_impoexpo_DF$grado_expo)*1.8
  corte_leverage <- mean(grafo_impoexpo_DF$leverage)*8
  
  
  grafo_impoexpo_DF <- grafo_impoexpo_DF %>% 
    mutate(corte_label = case_when(grado_impo>corte_grafico ~1,
                                   TRUE~0),
           corte_leverage = case_when(leverage>=corte_leverage ~1,
                                      TRUE~0))
  
  grafico <- ggplot(grafo_impoexpo_DF,aes(grado_impo, grado_expo, color = continente))+
    geom_point(data = grafo_impoexpo_DF %>%
                 filter(grado_impo>corte_grafico))+
    geom_smooth(method = "lm", se = FALSE)+
    geom_text_repel(data = grafo_impoexpo_DF %>%
                      filter(corte_label==1, corte_leverage == 0), aes(label = pais))+
    geom_text_repel(data = grafo_impoexpo_DF %>%
                      filter(corte_leverage==1),
                    aes(label = paste0(pais, " \n Leverage: ", round(leverage,2))), 
                    color = "black", min.segment.length = 0)+
    scale_color_gdocs()+
    theme_tufte()+
    labs(x = "Grado grafo importaciones",
         y = "Grado grafo exportaciones")+#,
    #title = "Grado total. multigrafo exportaciones e importaciones",
    #subtitle = paste0("Grafo año 2011, punto de corte de ", threshold_pct*100,"%"))+
    annotate("text", x = corte_grafico, y = corte_grafico*4,
             label = plot_label, color = 'black', parse = TRUE)
  if (label) {
    grafico <- grafico+
      theme(legend.position = "bottom")
    
  }else{
    grafico <- grafico+
      theme(legend.position = "none")
  }
  
  if (save) {
    grafico
    ggsave(paste0("graficos/predictivos/","corr_grados_2011_",threshold_pct*100,"_pcnt.png"), scale = 2,dpi = 300)
  }
  return(grafico)
}

for (pcnt in c(0.05,.2)) {
  print(pcnt)
  correlacion_grado_impoexpo(threshold_pct = pcnt,
                             data_expo = dataset_expo, 
                             data_impo = dataset, save = T)
}


### figuras 5 y 6

##### Datasets #####
dataset <- readRDS('Dataset/dataset_COMTRADE.rds')
dataset <- dataset %>% filter(yr <= 2011,ptCode != 0, TradeValue>100, rgCode == 1)

dataset <- dataset %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue, yr)
dataset <- left_join(dataset,countrycode_data 
                     %>% select(rt3ISO, continent)) %>% 
  na.omit(.)

#expo
dataset_expo <- readRDS('Dataset/dataset_COMTRADE.rds')
dataset_expo <- dataset_expo %>% filter(yr <= 2011,ptCode != 0, TradeValue>100, rgCode == 2)

dataset_expo <- dataset_expo %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue, yr)
dataset_expo <- left_join(dataset_expo,countrycode_data 
                          %>% select(rt3ISO, continent)) %>% 
  na.omit(.)


#### loop ####
caracteristicas <- data_frame()
distribuciones_impo <- data_frame()

for (nano  in sort(unique(dataset$yr))) {
  print(paste0("Año: ",nano))
  dataset2 <- dataset %>% filter(yr == nano)
  net <- trade_to_graph(dataset2, threshold_pct = 0.01)
  
  renglon <- c('yr'= nano,
               'connected_weak' = net$connected_weak,
               'connected_strong' = net$connected_strong, 
               'diameter' = net$diameter,
               'density' = net$density,
               'naristas' = net$naristas, 
               'max_betweenness' = net$max_betweenness,
               'mean_betweenness' = net$mean_betweenness,
               'mean_closeness' = net$mean_closeness, 
               'mean_degree' = net$mean_degree,
               'mean_eigen_centrality' = net$mean_eigen_centrality,
               'mean_eigen_centrality_ponderado' = net$mean_eigen_centrality_ponderado,
               'coef_clustering' = net$coef_clustering, 
               'correlacion' = net$correlacion)
  caracteristicas <- bind_rows(caracteristicas,renglon)
  
  #distribuciones de intermediacion | grado | autovalor | autovalor ponderado
  
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



ggplot(caracteristicas, aes(yr,coef_clustering ))+
  geom_line(size = 1, color = "black")+
  geom_smooth(se = F)+
  # labs(title= 'Coeficiente de clustering de la red',
  #      subtitle = "Importaciones, threshold 1%, según año")+
  scale_x_continuous(breaks = c(seq(1997,2011,1)))+
  theme_tufte()+
  labs(x= "Año", y="Coeficiente de clustering")

ggsave("graficos/predictivos/coef_clustering_x_yr.png", dpi = 300)

ggplot(caracteristicas, aes(yr,correlacion ))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  # labs(title= 'Correlación de grado en grafo no dirigido',
  #      subtitle = "Importaciones, threshold 1%, según año")+
  scale_x_continuous(breaks = c(seq(1997,2011,1)))+
  theme_tufte()+
  labs(x="Año",y="Correlación")

ggsave("graficos/predictivos/correlacion_x_yr.png", dpi = 300)

#### Grafico 8

ggplot(distribuciones_impo , aes(degree, y = factor(yr), fill = factor(yr)))+
  geom_density_ridges(stat = "binline")+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_impo %>%
                    filter(cod %in% c("CHN", "USA")), 
                  aes(label = cod, color =cod),
                  nudge_y = 0.5, fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año", x = "grado")
# title= 'Distribución de autovalor de los nodos',
# subtitle = "Importaciones, threshold 1%, según año. Detalle China y Estados Unidos")

ggsave("graficos/predictivos/impo_densidad_USAvsCHN_grado_x_yr.png", dpi = 300)

