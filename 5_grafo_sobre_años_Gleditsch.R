# Dataset Gleditsch. 1948-2000

rm(list = ls())
gc()
library(tidyverse)
library(ggthemes)
library(ggridges)
library(ggrepel)
library(igraph)
library(countrycode)
library(xlsx)


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

trade_dd <- read_delim("Dataset/Gleditsch/exptradegdpv4.1/trade_dd.asc",delim = " ")

# Caracterización del dataset
trade_dd %>% filter(acra %in% c("USA","CAN"),acrb %in% c("USA","CAN"), year == 1995)
resumen <- trade_dd %>%
  group_by(year) %>% 
  summarise(Cantidad_datos = n(),
            Promedio_impo = mean(impab),
            Promedio_expo = mean(expab)) %>% 
  gather(.,variable, valor, 2:4)

ggplot(resumen, aes(year, valor))+
  geom_col()+
  theme_tufte()+
  labs(title= "Caracteristicas base Gleditsch")+
  facet_grid(variable~., scales =  'free')
ggsave("graficos/1950_2000_Resumen_BdD.png")

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

#### loop ####
caracteristicas_impo <- data_frame()
distribuciones_impo <- data_frame()

for (nano  in sort(unique(dataset_impo$yr))) {
  print(paste0("Año: ",nano))
  dataset2 <- dataset_impo %>% filter(yr == nano)
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
  caracteristicas_impo <- bind_rows(caracteristicas_impo,renglon)
  
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

caracteristicas_impo
distribuciones_impo

table(caracteristicas_impo$connected_weak)
table(caracteristicas_impo$connected_strong)
# Esta debilmente conectado todos los años
# No esta fuertemente conectado ningún año
#### gráficos características ####


ggplot(caracteristicas_impo, aes(yr,diameter))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Diámetro de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_diametro_x_ano.png")

ggplot(caracteristicas_impo, aes(yr,naristas))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Número de aristas de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_naristas_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,mean_betweenness))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Betweenness promedio de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_mean_betweenness_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,max_betweenness))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Máxima betweenness de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_max_betweenness_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,mean_closeness))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Cercanía promedio de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_mean_closeness_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,mean_degree))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Grado medio de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_mean_degree_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,mean_eigen_centrality))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
#  geom_vline(xintercept = c(1971,1973,1979,1980,1987,1994, 1997, 2000), linetype = 'dashed', color = 'red')+
  labs(title= 'Autovalor promedio de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_mean_eigen_centrality_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,mean_eigen_centrality_ponderado))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Autovalor promedio de la red ponderada',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_mean_eigen_centrality_ponderado_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,coef_clustering ))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Coeficiente de clustering de la red',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_coef_clustering_x_yr.png")

ggplot(caracteristicas_impo, aes(yr,correlacion ))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  labs(title= 'Correlación de grado en grafo no dirigido',
       subtitle = "Importaciones, threshold 1%, según año")+
  theme_tufte()

ggsave("graficos/1950_2000_correlacion_x_yr.png")

#### gráficos densidad IMPO####

ggplot(distribuciones_impo %>% 
         filter(betweenness>0), aes(betweenness, y = factor(yr) ))+
  geom_density_ridges()+
  geom_text_repel(data = distribuciones_impo %>%
                    group_by(yr) %>% 
                    top_n(betweenness, n = 3), aes(label = cod), 
                  nudge_y = 0.5)+
  theme_tufte()+
  theme(legend.position = "none")+
  labs(y = "Año",
       title= 'Distribución de intermediación de los nodos',
       subtitle = "Importaciones, threshold 1%, según año")
  

#ggsave("graficos/1950_2000_impo_densidad_betweenness_x_yr.png")


ggplot(distribuciones_impo , aes(degree, y = factor(yr) ))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_impo %>%
                    group_by(yr) %>% 
                    top_n(degree, n = 3),
                  aes(label = cod, color = cod), nudge_y = 0.5)+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_color_gdocs()+
  labs(y = "Año",
       title= 'Distribución de grado de los nodos',
       subtitle = "Importaciones, threshold 1%, según año")

#ggsave("graficos/1950_2000_impo_densidad_degree_x_yr.png")


ggplot(distribuciones_impo , aes(autovalor, y = factor(yr)))+
  geom_density_ridges()+
  geom_text_repel(data = distribuciones_impo %>%
                    group_by(yr) %>% 
                    top_n(autovalor, n = 3), 
                  aes(label = cod, color = cod), nudge_y = 0.5)+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos',
       subtitle = "Importaciones, threshold 1%, según año")


#ggsave("graficos/1950_2000_impo_densidad_autovalor_x_yr.png")

ggplot(distribuciones_impo , aes(autovalor_pond, y = factor(yr)))+
  geom_density_ridges()+
  geom_text_repel(data = distribuciones_impo %>%
                    group_by(yr) %>% 
                    top_n(autovalor_pond, n = 5),
                  aes(label = cod, color = cod), nudge_y = 0.5)+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_colour_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos en grafo ponderado',
       subtitle = "Importaciones, threshold 1%, según año")

#ggsave("graficos/1950_2000_impo_densidad_autovalor_pond_x_yr.png")

# USA vs CHINA
ggplot(distribuciones_impo , aes(autovalor_pond, y = factor(yr)))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_impo %>%
                    filter(cod %in% c("CHN", "USA")), 
                  aes(label = cod, color =cod), 
                  nudge_y = 0.5, fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos en grafo ponderado',
       subtitle = "Importaciones, threshold 1%, según año. Detalle China y Estados Unidos")

#ggsave("graficos/1950_2000_impo_densidad_USAvsCHN_autovalor_pond_x_yr.png")

ggplot(distribuciones_impo , aes(autovalor, y = factor(yr) ))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_impo %>%
                    filter(cod %in% c("CHN", "USA")), 
                  aes(label = cod, color =cod), 
                  nudge_y = 0.5,fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos',
       subtitle = "Importaciones, threshold 1%, según año. Detalle China y Estados Unidos")

#ggsave("graficos/1950_2000_impo_densidad_USAvsCHN_autovalor_x_yr.png")
 
ggplot(distribuciones_impo , aes(degree, y = factor(yr)))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_impo %>%
                    filter(cod %in% c("CHN", "USA")), 
                  aes(label = cod, color =cod),
                  nudge_y = 0.5, fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos',
       subtitle = "Importaciones, threshold 1%, según año. Detalle China y Estados Unidos")

#ggsave("graficos/1950_2000_impo_densidad_USAvsCHN_grado_x_yr.png")


#### densidad EXPO ####

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

#### gráficos densidad EXPO####

ggplot(distribuciones_expo %>% 
         filter(betweenness>0), aes(betweenness, y = factor(yr)))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_expo %>%
                    group_by(yr) %>% 
                    top_n(betweenness, n = 3),
                  aes(label = cod), nudge_y = 0.5)+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_color_gdocs()+
  labs(y = "Año",
       title= 'Distribución de intermediación de los nodos',
       subtitle = "Exportaciones, threshold 1%, según año")

#ggsave("graficos/1950_2000_expo_densidad_betweenness_x_yr.png")


ggplot(distribuciones_expo , aes(degree, y = factor(yr)))+
  geom_density_ridges(alpha = 1)+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de grado de los nodos',
       subtitle = "Exportaciones, threshold 1%, según año")+
  scale_x_continuous(limits = c(0,100))

#ggsave("graficos/1950_2000_expo_densidad_degree_x_yr.png")

ggplot(distribuciones_expo , aes(autovalor, y = factor(yr)))+
  geom_density_ridges()+
  theme_tufte()+
  theme(legend.position = "none")+
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos',
       subtitle = "Exportaciones, threshold 1%, según año")

#ggsave("graficos/1950_2000_expo_densidad_autovalor_x_yr.png")

ggplot(distribuciones_expo , aes(autovalor_pond, y = factor(yr)))+
  geom_density_ridges()+
  geom_text_repel(data = distribuciones_expo %>%
                    group_by(yr) %>% 
                    top_n(autovalor_pond, n = 5),
                  aes(label = cod), nudge_y = 0.5)+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos en grafo ponderado',
       subtitle = "Exportaciones, threshold 1%, según año")

#ggsave("graficos/1950_2000_expo_densidad_autovalor_pond_x_yr.png")

# USA vs CHINA
ggplot(distribuciones_expo , aes(autovalor_pond, y = factor(yr)))+
  geom_density_ridges()+
  geom_text_repel(data = distribuciones_expo %>%
                    filter(cod %in% c("CHN", "USA")),
                  aes(label = cod, color =cod),
                  nudge_y = 0.5, fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos en grafo ponderado',
       subtitle = "Exportaciones, threshold 1%, según año. Detalle China y Estados Unidos")

#ggsave("graficos/1950_2000_expo_densidad_USAvsCHN_autovalor_pond_x_yr.png")

ggplot(distribuciones_expo , aes(autovalor, y = factor(yr)))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_expo %>%
                    filter(cod %in% c("CHN", "USA")),
                  aes(label = cod, color =cod),
                  nudge_y = 0.5,fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_fill_gdocs() +
  scale_color_gdocs() +
  labs(y = "Año",
       title= 'Distribución de autovalor de los nodos',
       subtitle = "Exportaciones, threshold 1%, según año. Detalle China y Estados Unidos")

#ggsave("graficos/1950_2000_expo_densidad_USAvsCHN_autovalor_x_yr.png")

ggplot(distribuciones_expo , aes(degree, y = factor(yr)))+
  geom_density_ridges(alpha = 0.2)+
  geom_text_repel(data = distribuciones_expo %>%
                    filter(cod %in% c("CHN", "USA")),
                  aes(label = cod, color =cod),
                  nudge_y = 0.5, fontface = "bold")+
  theme_tufte()+
  theme(legend.position = "none")+
  scale_color_gdocs() +
  scale_fill_gdocs() +
  labs(y = "Año",
       title= 'Distribución de grado de los nodos',
       subtitle = "Exportaciones, threshold 1%, según año. Detalle China y Estados Unidos")

#ggsave("graficos/1950_2000_expo_densidad_USAvsCHN_grado_x_yr.png")


#### Cuadros de resumen ####

graph_summary <- function(data = dataset_impo, threshold_pct =0.01){
  
  degree_in <- data_frame("orden" = c(paste0(c(1:5),'°')))
  closeness_in <- data_frame("orden" = c(paste0(c(1:5),'°')))
  intermediacion <- data_frame("orden" = c(paste0(c(1:5),'°')))
  autovalor <- data_frame("orden" = c(paste0(c(1:5),'°')))
  autovalor_pond <- data_frame("orden" = c(paste0(c(1:5),'°')))
  
  for (nano  in sort(unique(data$yr))) {
      dataset2 <- data %>% filter(yr == nano)
      grafo <- trade_to_graph(dataset2, threshold_pct = threshold_pct)$grafo
      nombres <- paste0(nano,"_", c('pais','valor'))
      
      grado_in <- sort(degree(grafo, mode = 'in'), decreasing = T)[0:5]
      degree_in[nombres[1]] <- names(grado_in)
      degree_in[nombres[2]] <- grado_in
      
      cercania_in <- sort( closeness(grafo,mode = "in"), decreasing = T)[0:5]
      closeness_in[nombres[1]] <- names(cercania_in)
      closeness_in[nombres[2]] <- cercania_in
      
      inter <- sort( betweenness(grafo,directed=T), decreasing = T)[0:5]
      intermediacion[nombres[1]] <- names(inter)
      intermediacion[nombres[2]] <- inter
      
      atvlr <- sort(eigen_centrality(grafo,directed = F)$vector, decreasing = T)[0:5]
      autovalor[nombres[1]] <- names(atvlr)
      autovalor[nombres[2]] <- atvlr
      
      atvlr_pnd <- sort(eigen_centrality(grafo,directed = F,
                                         weights = E(grafo)$TradeValue)$vector,
                        decreasing = T)[0:5]
      autovalor_pond[nombres[1]] <- names(atvlr_pnd)
      autovalor_pond[nombres[2]] <- atvlr_pnd
  }
  lista <- list("degree_in" = degree_in,
                "closeness_in" = closeness_in,
                "intermediacion" = intermediacion,
                "autovalor" = autovalor,
                "autovalor_pond" = autovalor_pond)
  return(lista)
}

top5_x_yr_impo <-graph_summary(data = dataset_impo, threshold_pct =0.01)

for ( i in c(1:length(names(top5_x_yr_impo)))) {
  nombre <- names(top5_x_yr_impo)[i]
  tabla <- top5_x_yr_impo[[nombre]]
  if (i==1) {
    write.xlsx(x = as.data.frame(tabla),file =  "Resultados/1950_2000_top5_x_yr_impo.xlsx",
               row.names = FALSE,sheetName = nombre, append = FALSE)
  }
  else{
    write.xlsx(x = as.data.frame(tabla),file =  "Resultados/1950_2000_top5_x_yr_impo.xlsx",
               row.names = FALSE,sheetName = nombre, append = TRUE)
  }
} 


#saveRDS(top5_x_yr_impo, file = "Resultados/top5_x_yr_impo.RDS")

top5_x_yr_expo <-graph_summary(data = dataset_expo, threshold_pct =0.01)

#saveRDS(top5_x_yr_expo, file = "Resultados/top5_x_yr_expo.RDS")
for ( i in c(1:length(names(top5_x_yr_expo)))) {
  nombre <- names(top5_x_yr_expo)[i]
  tabla <- top5_x_yr_expo[[nombre]]
  if (i==1) {
    write.xlsx(x = as.data.frame(tabla),file =  "Resultados/1950_2000_top5_x_yr_expo.xlsx",
               row.names = FALSE,sheetName = nombre, append = FALSE)
  }
  else{
    write.xlsx(x = as.data.frame(tabla),file =  "Resultados/1950_2000_top5_x_yr_expo.xlsx",
               row.names = FALSE,sheetName = nombre, append = TRUE)
  }
} 


