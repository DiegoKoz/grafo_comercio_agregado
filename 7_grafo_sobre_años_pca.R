# Trabajo con los años 1997-2011
## limpiar todas las librerias
#require(nothing, quietly = TRUE)

rm(list = ls())
gc()
library(ggthemes)
library(ggridges)
library(ggrepel)
library(igraph)
library(countrycode)
#library(ggbiplot)
library(tidyverse)
#library(plyr); library(dplyr)



##### Funciones #####

trade_to_graph <- function(edges, threshold_pct = .01) {
  
  edges <- edges %>% 
    group_by(rt3ISO) %>% 
    mutate(sumTradeValue = sum(TradeValue),
            L = case_when(TradeValue>sum(TradeValue)*threshold_pct~1,
                         TRUE ~ 0)) %>% 
    na.omit(.)
  
  edges <- edges %>% filter(L==1)
  
  DF_red <- graph_from_data_frame(edges,directed = TRUE)
  
  if (nrow(edges)>0) {
      nombre_pais <- left_join(data.frame(cod = V(DF_red)$name),  countrycode_data %>%
                                 select(cod = iso3c, pais = country.name.en),
                               by = 'cod')
      V(DF_red)$pais <- nombre_pais$pais
  }
  
  
  return(DF_red)
}

##### Datasets #####

dataset <- readRDS('Dataset/dataset_COMTRADE.rds')

dataset <- dataset %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue, yr,rgCode, ptCode)
dataset <- left_join(dataset,countrycode_data 
                     %>% select(rt3ISO=iso3c, continent)) %>% 
  na.omit(.)

dataset_impo <- dataset %>% filter(yr <= 2011,ptCode != 0, TradeValue>100, rgCode == 1) %>% 
  select(-ptCode, -rgCode)
dataset_expo <- dataset %>% filter(yr <= 2011,ptCode != 0, TradeValue>100, rgCode == 2) %>% 
  select(-ptCode, -rgCode)



#### Medidas de centralidad ####

centrality_measures <- function(threshold_pct =0.01,data, type){
  
  lista <- list()
  
  for (nano  in sort(unique(data$yr))) {
      dataset2 <- data %>% filter(yr == nano)
      grafo <- trade_to_graph(edges = dataset2, threshold_pct = threshold_pct)
      
      if (length(V(grafo))> 0) {
        cod <- V(grafo)$name
      } else {
        cod <- as.numeric(c())
      }
      
            
      grado_in <- degree(grafo, mode = 'in')
      cercania_in <- closeness(grafo,mode = "in")
      inter <- betweenness(grafo,directed=T)
      atvlr <- eigen_centrality(grafo,directed = F)$vector
      atvlr_pnd <- eigen_centrality(grafo,directed = F,
                                    weights = E(grafo)$TradeValue)$vector
      
      nombres <- c("cod",
                 paste(type, threshold_pct, "grado_in", sep = "_"),
                 paste(type, threshold_pct, "cercania_in", sep = "_"),
                 paste(type, threshold_pct, "inter", sep = "_"),
                 paste(type, threshold_pct, "atvlr", sep = "_"), 
                 paste(type, threshold_pct, "atvlr_pnd", sep = "_"))
      
      tmp <- data.frame(cod,grado_in,cercania_in,inter,atvlr,atvlr_pnd)
      names(tmp) <- nombres
      
      
      lista[[paste0(nano)]] <- tmp
  }
  return(lista)
}



lista_impo01 <-centrality_measures(data = dataset_impo, threshold_pct =0.01, type = 'impo')
lista_expo01 <-centrality_measures(data = dataset_expo, threshold_pct =0.01, type = 'expo')

lista_impo10 <-centrality_measures(data = dataset_impo, threshold_pct =0.10, type = 'impo')
lista_expo10 <-centrality_measures(data = dataset_expo, threshold_pct =0.10, type = 'expo')

lista_impo20 <-centrality_measures(data = dataset_impo, threshold_pct =0.20, type = 'impo')
lista_expo20 <-centrality_measures(data = dataset_expo, threshold_pct =0.20, type = 'expo')

variantes_2011 <- list(lista_impo01$`2011`,
                       lista_expo01$`2011`,
                       lista_impo10$`2011`,
                       lista_expo10$`2011`,
                       lista_impo20$`2011`,
                       lista_expo20$`2011`)

centralities_2011 <- Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="cod"), variantes_2011) %>% 
  na.omit()

centralities_2011_reduce <- centralities_2011[sapply(centralities_2011, function(x) length(unique(na.omit(x)))) > 1]

pca <- princomp(centralities_2011_reduce %>% select( -cod),cor=T,scores = T)
summary(pca)

#Me quedo con los primeros tres, que explican más del 20% cada uno
loads <- as.data.frame(pca$loadings[,1:2]) %>% 
  mutate(variable = row.names(.)) %>% 
  gather(.,componente,carga,1:2)

loads <- loads %>% 
  group_by(componente) %>% 
  mutate(etiqueta = 1:30)

tabla <- loads %>% ungroup() %>% 
  filter(componente == 'Comp.1' ) %>% 
  select(etiqueta, variable)

knitr::kable(tabla)

#### grafico ####
ggplot(loads,aes(factor(etiqueta), carga, fill= factor(etiqueta), group= variable))+ 
  geom_col()+
#  labs(title="Cargas",
#       subtitle = "primeras dos componentes")+
  facet_grid(componente~., scales = "free_x")+
  theme_tufte()+
  theme(legend.position = "none")+
  labs(x = "variables")
        
ggsave("graficos/PCA_cargas.PNG")

#biplot
scores <- data.frame(as.matrix(pca$scores[,1:2]))
scores$cod <- centralities_2011$cod
nombre_pais <- left_join(scores,  countrycode_data %>%
                           select(cod = iso3c, pais = country.name.en, continent),
                         by = 'cod')


top_scores <- scores %>%
  arrange(Comp.1) %>%
  top_n(-6, wt = Comp.1)

ggbiplot(pca,
         choices = c(1,2),
         obs.scale =1, 
         var.scale = 1,
         circle = FALSE,
         ellipse = TRUE,
         groups = nombre_pais$continent,
         alpha = 1,
         varname.adjust =1,
         varname.size = 2,
         varname.abbrev = FALSE,
         var.axes	= TRUE)+
  geom_text_repel(data = top_scores, aes(x = Comp.1, y = Comp.2, label = cod))+
  #geom_point(data = scores, aes(x = Comp.1, y = Comp.2, label = cod, alpha = -Comp.1), 
  #           color = "blue3")+
#  labs(title="PCA, threshold 1%, 10% y 20%",
#       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.direction = 'horizontal', legend.position = 'top')


ggsave("graficos/PCA_biplot_all.PNG", scale = 2)

# Solo 1 pcnt threshold

variantes_2011_01 <- list(lista_impo01$`2011`,
                          lista_expo01$`2011`)

centralities_2011_01 <- Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="cod"), variantes_2011_01) %>% 
  na.omit()

pca <- princomp(centralities_2011_01 %>% select( -cod),cor=T,scores = T)
summary(pca)

scores <- data.frame(as.matrix(pca$scores[,1:2]))
scores$cod <- centralities_2011_01$cod

nombre_pais <- left_join(scores,  countrycode_data %>%
                           select(cod = iso3c, pais = country.name.en, continent),
                         by = 'cod')

top_scores <- scores %>%
  arrange(Comp.1) %>% 
  top_n(-6, wt = Comp.1)

ggbiplot(pca,
         choices = c(1,2),
         obs.scale =1, 
         var.scale = 1,
         circle = FALSE,
         ellipse = TRUE,
         groups = nombre_pais$continent,
         alpha = 1,
         varname.adjust =1,
         varname.size = 2,
         varname.abbrev = FALSE,
         var.axes	= TRUE)+
  geom_text_repel(data = top_scores, aes(x = Comp.1, y = Comp.2, label = cod))+
  #geom_point(data = scores, aes(x = Comp.1, y = Comp.2, label = cod, alpha = -Comp.1), 
  #           color = "blue3")+
#  labs(title="PCA, threshold 1%",
#       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.direction = 'horizontal', legend.position = 'top')


ggsave("graficos/PCA_biplot_01.PNG", scale = 2)

# Solo 10 pcnt 

variantes_2011_10 <- list(lista_impo10$`2011`,
                          lista_expo10$`2011`)

centralities_2011_10 <- Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="cod"), variantes_2011_10) %>% 
  na.omit()

pca <- princomp(centralities_2011_10 %>% select( -cod),cor=T,scores = T)
summary(pca)


scores <- data.frame(as.matrix(pca$scores[,1:2]))
scores$cod <- centralities_2011_10$cod
nombre_pais <- left_join(scores,  countrycode_data %>%
                           select(cod = iso3c, pais = country.name.en, continent),
                         by = 'cod')

top_scores <- scores %>%
  arrange(Comp.1) %>% 
  top_n(-6, wt = Comp.1)

ggbiplot(pca,
         choices = c(1,2),
         obs.scale =1, 
         var.scale = 1,
         circle = FALSE,
         ellipse = TRUE,
         groups = nombre_pais$continent,
         alpha = 1,
         varname.adjust =1,
         varname.size = 2,
         varname.abbrev = FALSE,
         var.axes	= TRUE)+
  geom_text_repel(data = top_scores, aes(x = Comp.1, y = Comp.2, label = cod))+
  #geom_point(data = scores, aes(x = Comp.1, y = Comp.2, label = cod, alpha = -Comp.1), 
  #           color = "blue3")+
#  labs(title="PCA, threshold 10%",
#       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.direction = 'horizontal', legend.position = 'top')
ggsave("graficos/PCA_biplot_10.PNG", scale = 2)

# Solo 20 pcnt

variantes_2011_20 <- list(lista_impo20$`2011`,
                       lista_expo20$`2011`)

centralities_2011_20 <- Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="cod"), variantes_2011_20) %>% 
  na.omit()


pca <- princomp(centralities_2011_20 %>% select( -cod),cor=T,scores = T)
summary(pca)


scores <- data.frame(as.matrix(pca$scores[,1:2]))
scores$cod <- centralities_2011_20$cod

nombre_pais <- left_join(scores,  countrycode_data %>%
                           select(cod = iso3c, pais = country.name.en, continent),
                         by = 'cod')

top_scores <- scores %>%
  arrange(Comp.1) %>% 
  top_n(-6, wt = Comp.1)


ggbiplot(pca,
         choices = c(1,2),
         obs.scale =1, 
         var.scale = 1,
         circle = FALSE,
         ellipse = TRUE,
         groups = nombre_pais$continent,
         alpha = 1,
         varname.adjust =1,
         varname.size = 2,
         varname.abbrev = FALSE,
         var.axes	= TRUE)+
  geom_text_repel(data = top_scores, aes(x = Comp.1, y = Comp.2, label = cod))+
  #geom_point(data = scores, aes(x = Comp.1, y = Comp.2, label = cod, alpha = -Comp.1), 
  #           color = "blue3")+
#  labs(title="PCA, threshold 20%",
#       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.direction = 'horizontal', legend.position = 'top')

ggsave("graficos/PCA_biplot_20.PNG", scale = 2)
