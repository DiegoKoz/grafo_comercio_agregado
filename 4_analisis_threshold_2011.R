rm(list = ls())
gc()
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(igraph)
library(countrycode)
library(ggrepel)


##### Funciones #####

trade_to_graph <- function(edges, threshold_pct = .01) {
  edges <- edges %>% 
    group_by(rt3ISO) %>% 
    mutate(L = case_when(TradeValue>sum(TradeValue)*threshold_pct~1,
                         TRUE ~ 0)) %>% 
    na.omit(.)
  
  edges <- edges %>% filter(L==1)
  
  DF_red <- graph_from_data_frame(edges,directed = TRUE)
  
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
dataset <- readRDS('Dataset/dataset_COMTRADE_2011.rds')
dataset <- dataset %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue)
dataset2 <- left_join(dataset,countrycode_data 
                      %>% select(rt3ISO=iso3c, continent, country.name.es)) %>% 
  na.omit(.)

#expos
dataset_expo <- readRDS('Dataset/dataset_COMTRADE_expo_2011.rds')
dataset_expo <- dataset_expo %>% select(rt3ISO,pt3ISO,rtTitle,ptTitle,TradeValue)
dataset_expo <- left_join(dataset_expo,countrycode_data 
                      %>% select(rt3ISO=iso3c, continent,country.name.es)) %>% 
  na.omit(.)


##### Gráficos de frecuencia #####

ggplot(dataset2 %>% 
         filter(TradeValue>0),
       aes(TradeValue, fill = continent, color = continent, 
                                  label = paste0(rt3ISO,"-",pt3ISO)))+
  geom_dotplot(binwidth = 2000000000)+
  geom_text_repel(data =dataset2 %>% 
                    filter(TradeValue>150000000000), aes(y= 0.1))+
  geom_rug()+
  theme_tufte()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top")+
  scale_fill_gdocs()+
  scale_color_gdocs()+
  labs(#title = "Frecuencia de las interacciones",
       #subtitle = "Importaciones 2011, según su valor comercial",
       x = "valor comercial",
       y= "" )

ggsave("graficos/2011_freq_interacciones_0.png")


ggplot(dataset2 %>%
         filter(TradeValue>0),
       aes(TradeValue, fill = continent, color = continent, 
           label = paste0(rt3ISO,"-",pt3ISO)))+
  geom_dotplot(binwidth = 200000000)+
  geom_rug()+
  lims(x= c(0,50000000000))+
  theme_tufte()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_fill_gdocs()+
  scale_color_gdocs()+
  labs(#title = "Frecuencia de las interacciones",
       #subtitle = "Importaciones 2011, según su valor comercial",
       x = "valor comercial",
       y= "" )
ggsave("graficos/2011_freq_interacciones_1.png")


trade_to_plot <- function(edges, title = "Frecuencia de interacciones", 
                          subtitle= "Importaciones 2011, según porcentaje de importaciones que representan del país importador" ) {
  edges <- edges %>% 
    group_by(rt3ISO) %>% 
    mutate(pcnt = TradeValue/sum(TradeValue)) %>% 
    na.omit(.)
  
  g <-   ggplot(edges %>% filter(TradeValue>0),
                aes(pcnt, fill = continent, color = continent,
                    label = paste0(rtTitle,"-
",ptTitle)))+
    geom_dotplot(binwidth = 0.005)+
    geom_text_repel(data =edges %>% 
                      filter(pcnt>0.75), aes(y= 0.1), parse = FALSE)+
    geom_rug()+
    theme_tufte()+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none")+
    scale_color_gdocs()+
    scale_fill_gdocs()+
    labs(title = title, subtitle =  subtitle,  x = "porcentaje", y = "")
  
  g
  return(g)
  
}

trade_to_plot(dataset2, title = "", subtitle = "")
ggsave("graficos/2011_freq_interacciones_3.png")


##### Eleccion del threshold #####

#### loop ####
caracteristicas <- data_frame()
for (threshold in seq(0,.5, 0.01)) {
  print(paste0('threshold: ',threshold))
  net <- trade_to_graph(dataset2, threshold_pct = threshold)
  
  renglon <- c('threshold'= threshold,
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
  
}

caracteristicas
#Esta debilmente conectado hasta un threshold del 10%. Nunca esta fuertemente conectado

#### gráficos características ####

ggplot(caracteristicas, aes(threshold,diameter))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
  geom_vline(xintercept = 0.01, size = 0.5, linetype="dashed", color = "red")+
  labs(title= 'Diámetro de la red',
       subtitle = "Importaciones 2011, según punto de corte")+
  scale_x_continuous(breaks = c(0.01,seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()
#nota: el diametro primero crece porque se eliminan aristas, pero luego decrece porque se desconecta
#      el grafo
ggsave("graficos/2011_diametro_x_threshold.png")

ggplot(caracteristicas, aes(threshold,naristas))+
  geom_line(size = 1.25, color = "black")+
  geom_vline(xintercept = 0.01, size = 0.5,linetype = "dashed", color = "red")+
#  labs(title= 'Número de aristas',
#       subtitle= 'Importaciones 2011, según punto de corte')+
  scale_x_continuous(breaks = c(0.01,seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_naristas_x_threshold.png")

ggplot(caracteristicas, aes(threshold,max_betweenness))+
  geom_line(size = 1.25)+
  geom_smooth(se = F)+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Máxima betweenness',
#       subtitle= 'Importaciones 2011, según punto de corte')+
  scale_x_continuous(breaks = c(0.01,seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_max_betweenness_x_threshold.png")

ggplot(caracteristicas, aes(threshold,mean_betweenness))+
  geom_line(size = 1.25)+
  geom_smooth(se = F)+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Betweenness media',
#       subtitle= 'Importaciones 2011, según punto de corte')+
  scale_x_continuous(breaks = c(seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_mean_betweenness_x_threshold.png", scale = .5)

ggplot(caracteristicas, aes(threshold,mean_closeness))+
  geom_line(size = 1.25)+
  geom_smooth(se = F)+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Closeness media',
#       subtitle= 'Importaciones 2011, según punto de corte')+
  scale_x_continuous(breaks = c(0.01,seq(0,.5,0.05)),limits = c(0,0.5))+
  theme_tufte()

ggsave("graficos/2011_mean_closeness_x_threshold.png")

ggplot(caracteristicas, aes(threshold,coef_clustering ))+
  geom_line(size = 1.25)+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Coeficiente de clustering',
#       subtitle = 'Importaciones 2011, según punto de corte')+
  scale_x_continuous(breaks = c(0.01,seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_coef_clustering_x_threshold.png")

ggplot(caracteristicas, aes(threshold,mean_degree ))+
  geom_line(size = 1.25, color = "black")+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Grado medio de la red',
#       subtitle = "Importaciones 2011, según punto de corte")+
  scale_x_continuous(breaks = c(0.01,seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_grado_medio_x_threshold.png")

ggplot(caracteristicas, aes(threshold,mean_eigen_centrality ))+
  geom_line(size = 1.25, color = "black")+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Autovalor medio de la red',
#       subtitle = "Importaciones 2011, según punto de corte")+
  scale_x_continuous(breaks = c(seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_autovalor_medio_x_threshold.png",scale = .5)

ggplot(caracteristicas, aes(threshold,mean_eigen_centrality_ponderado ))+
  geom_line(size = 1.25, color = "black")+
  geom_vline(xintercept = 0.01, size = 0.75,linetype= "dashed", color = "red")+
#  labs(title= 'Autovalor medio de la red ponderada',
#       subtitle = "Importaciones 2011, según punto de corte")+
  scale_x_continuous(breaks = c(seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()

ggsave("graficos/2011_autovalor_medio_pond_x_threshold.png")

ggplot(caracteristicas, aes(threshold,correlacion))+
  geom_line(size = 1.25, color = "black")+
  geom_smooth(se = F)+
#  labs(title= 'Correlación de grado en grafo no dirigido',
#       subtitle = "Importaciones 2011, según punto de corte")+
  scale_x_continuous(breaks = c(seq(0,.25,0.05)),limits = c(0,0.25))+
  theme_tufte()
#nota: el diametro primero crece porque se eliminan aristas, pero luego decrece porque se desconecta
#      el grafo
ggsave("graficos/2011_correlacion_grado_x_threshold.png")

#### Grafo ####

### Punto de corte elegido: 1%
### representación grafo 2011


grafico_grafo <- function(threshold_pct, save = TRUE, datos = dataset2, label = FALSE, layout="NA"){
  
      lista_grafo <- trade_to_graph(edges = datos, threshold_pct = threshold_pct)
      
      grafo <- lista_grafo$grafo
      nombres_vertices <- data_frame(rt3ISO=as.vector(V(grafo)$name))
      countrycode_data2 <- countrycode::countrycode_data %>% 
        select(rt3ISO = iso3c, continent)
      correspondencia <- left_join(nombres_vertices,countrycode_data2) %>% 
        mutate(color = case_when(continent == "Europe" ~"#E41A1C",
                                 continent == "Africa" ~"#377EB8",
                                 continent == "Asia" ~"#4DAF4A",
                                 continent == "Oceania" ~"#984EA3",
                                 continent == "Americas" ~"#FF7F00") )
      
      V(grafo)$continente <- correspondencia$continent
      V(grafo)$color <- correspondencia$color
      E(grafo)$edge.color <- "gray80"
      trade <- E(grafo)$TradeValue
      width <- (trade-min(trade))/(max(trade)-min(trade))*10
      E(grafo)$width <- width
      
      if (layout == "circle") {
        l <-layout_in_circle(grafo, order = order(V(grafo)$continente,V(grafo)))
        V(grafo)$size <- log(degree(grafo)+1)*3
        if (save) {
          if (label) {
            png(paste0("graficos/","grafo_Circ_2011_",threshold_pct*100,"_pcnt.png"))
            plot(grafo,edge.arrow.size=.2,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black", 
                 layout=l,edge.width=E(grafo)$width, edge.curved=.1, main= paste0("\n 2011, Threshold ", threshold_pct*100,"%"))
            legend(x=-1.5,  y=1.5,unique(correspondencia$continent),
                   pch=21,col="#777777",pt.bg=unique(correspondencia$color),
                   pt.cex=2,cex=1,bty="n",ncol=5)
            
          }
          else{
            png(paste0("graficos/","grafoCirc_2011_",threshold_pct*100,"_pcnt.png"))
            plot(grafo,edge.arrow.size=.2,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black", 
                 layout=l,edge.width=E(grafo)$width, main= paste0("2011, Threshold ", threshold_pct*100,"%"))
          }
          dev.off()
        }
        else{
          if (label) {
            plot(grafo,edge.arrow.size=.2,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black", 
                 layout=l,edge.width=E(grafo)$width)#, main= paste0("Grafo año 2011, punto de corte de ", threshold_pct*100,"%"))
            legend(x=-2,  y=1.5,unique(correspondencia$continent),
                   pch=21,col="#777777",pt.bg=unique(correspondencia$color),
                   pt.cex=2,cex=.75,bty="n",ncol=5)
            
          }
          else{
            plot(grafo,edge.arrow.size=.2,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black", 
                 layout=l,edge.width=E(grafo)$width)#, main= paste0("Grafo año 2011, punto de corte de ", 
          }
        }
      }
      else{
        l <-layout_nicely(grafo)
        if (save) {
          if (label) {
            png(paste0("graficos/","grafo_2011_",threshold_pct*100,"_pcnt.png"))
            plot(grafo,edge.arrow.size=.7,vertex.frame.color="#ffffff", edge.size = .5,
                 vertex.label="", vertex.label.color="black", vertex.size = 6, 
                 layout=l)#, main= paste0("Grafo año 2011, punto de corte de ", 
            #              threshold_pct*100,"%"))
            legend(x=-1.5,  y=1.5,unique(unique(correspondencia$continent)),
                   pch=21,col="#777777",pt.bg=unique(unique(correspondencia$color)),
                   pt.cex=2,cex=1.2,bty="n",ncol=5)
            
          }
          else{
            png(paste0("graficos/","grafo_2011_",threshold_pct*100,"_pcnt.png"))
            plot(grafo,edge.arrow.size=.7,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black", vertex.size = 6, 
                 layout=l)#, main= paste0("Grafo año 2011, punto de corte de ", 
          }
          dev.off()
        }
        else{
          if (label) {
            plot(grafo,edge.arrow.size=.2,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black", vertex.size = 6, 
                 layout=l, main= paste0("Grafo año 2011, punto de corte de ", threshold_pct*100,"%"))
            legend(x=-.5,  y=1,unique(unique(correspondencia$continent)),
                   pch=21,col="#777777",pt.bg=unique(unique(correspondencia$color)),
                   pt.cex=2,cex=1.5,bty="n",ncol=1)
            
          }
          else{
            plot(grafo,edge.arrow.size=.2,vertex.frame.color="#ffffff",
                 vertex.label="", vertex.label.color="black",
                 layout=l)#, main= paste0("Grafo año 2011, punto de corte de ", 
          }
        }
      }
}



grafico_grafo(datos = dataset2,threshold_pct = 0.01, save = T, label = T,layout = "circle")
grafico_grafo(datos = dataset2,threshold_pct = 0.05, save = T, label = F,layout = "circle")
grafico_grafo(datos = dataset2,threshold_pct = 0.10, save = T, label = F,layout = "circle")
grafico_grafo(datos = dataset2,threshold_pct = 0.15,save = T, label = F,layout = "circle")


grafico_grafo(datos = dataset2,threshold_pct = 0.01, save = T, label = TRUE)
grafico_grafo(datos = dataset2,threshold_pct = 0.2, save = T, label = TRUE)


for (pcnt in seq(0.05,.25,0.05)) {
  grafico_grafo(threshold_pct = pcnt, save = T, label = FALSE)
}





#### Comparación impos y expos  ####


grafo_continente <- function(threshold_pct, dataset = dataset2){
  lista_grafo <- trade_to_graph(edges = dataset, threshold_pct = threshold_pct)
  
  grafo <- lista_grafo$grafo
  nombres_vertices <- data_frame(rt3ISO=as.vector(V(grafo)$name))
  countrycode_data2 <- countrycode::countrycode_data %>% select(rt3ISO = iso3c, continent)
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
                                   select(nodo = iso3c, pais = country.name.es) ) %>% 
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
                      filter(corte_label==1), aes(label = nodo))+ #corte_leverage == 0
    # geom_text_repel(data = grafo_impoexpo_DF %>%
    #                   filter(corte_leverage==1),
    #                 aes(label = paste0(nodo, " \n Leverage: ", round(leverage,2))), 
    #                 color = "black", min.segment.length = 0)+
    scale_color_gdocs()+
    theme_tufte()+
    labs(x = "G° Impo",
         y = "G° Expo")+#,
         #title = "Grado total. multigrafo exportaciones e importaciones",
         #subtitle = paste0("Grafo año 2011, punto de corte de ", threshold_pct*100,"%"))+
    annotate("text", x = corte_grafico/2, y = corte_grafico*4,
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
    ggsave(paste0("graficos/","corr_grados_2011_",threshold_pct*100,"_pcnt.png"), scale = 1, width = 10, height = 7 )
  }
  return(grafico)
}

correlacion_grado_impoexpo(threshold_pct = 0.01,
                           data_expo = dataset_expo, 
                           data_impo = dataset2, 
                           save = TRUE,
                           pearson_note = FALSE,
                           label = FALSE)

correlacion_grado_impoexpo(threshold_pct = 0.10,
                           data_expo = dataset_expo, 
                           data_impo = dataset2, 
                           save = TRUE,
                           pearson_note = FALSE,
                           label = FALSE)

correlacion_grado_impoexpo(threshold_pct = 0.20,
                           data_expo = dataset_expo, 
                           data_impo = dataset2, 
                           save = TRUE,
                           pearson_note = FALSE,
                           label = FALSE)

for (pcnt in seq(0.05,.25,0.05)) {
  print(pcnt)
  correlacion_grado_impoexpo(threshold_pct = pcnt,
                             data_expo = dataset_expo, 
                             data_impo = dataset2, save = T)
}



#### Distribucion de grado  ####

dist_acumulada_graf <- function(threshold_pct,data_impo = dataset2){
  grafo_impos <- grafo_continente(threshold_pct = threshold_pct,dataset = data_impo)
  
  png(paste0("graficos/","dist_ac_2011_",threshold_pct*100,"_pcnt.png"))
  plot( degree.distribution(grafo_impos, cumulative = T), 
        xlab = "grado", ylab = "proporción de nodos con grado > x",
        #main = paste0("Distribución acumulada de grados \n Grafo año 2011, punto de corte de ",
        #              threshold_pct*100,"%"),
        type = "b")
  dev.off()
}

dist_acumulada_graf(threshold_pct = 0.01)
for (pcnt in seq(0.05,.25,0.05)) {
dist_acumulada_graf(threshold_pct = pcnt)
}

##### IMPOS #####
#### Top 5 centralidades ####

top_five_nodos <- function(grafo){
  
  degree_out <- sort(degree(grafo, mode = 'out'), decreasing = T)[0:5]
  degree_in <- sort(degree(grafo, mode = 'in'), decreasing = T)[0:5]
  degree_tot <- sort(degree(grafo, mode = 'total'), decreasing = T)[0:5]
  
  cercania_out <- sort( closeness(grafo,mode = "out"), decreasing = T)[0:5]
  cercania_in <- sort( closeness(grafo,mode = "in"), decreasing = T)[0:5]
  cercania_tot <- sort( closeness(grafo,mode = "total"), decreasing = T)[0:5]
  
  betweenness_directed <- sort( betweenness(grafo,directed=T), decreasing = T)[0:5]
  betweenness_undirected <- sort( betweenness(grafo,directed=F), decreasing = T)[0:5]

  autovalor <- sort(eigen_centrality(grafo,directed = F)$vector, decreasing = T)[0:5]
  autovalor_pond <- sort(eigen_centrality(grafo,directed = F,weights = E(grafo)$TradeValue)$vector, 
                         decreasing = T)[0:5]
  
  DF <- data_frame(degree_out_name = names(degree_out),  
                   degree_out_value = degree_out,
                   degree_in_name = names(degree_in),  
                   degree_in_value = degree_in,
                   degree_tot_name = names(degree_tot),  
                   degree_tot_value = degree_tot,
                   cercania_out_name = names(cercania_out),  
                   cercania_out_value = cercania_out,
                   cercania_in_name = names(cercania_in),  
                   cercania_in_value = cercania_in,
                   cercania_tot_name = names(cercania_tot),  
                   cercania_tot_value = cercania_tot,
                   betweenness_directed_name = names(betweenness_directed),  
                   betweenness_directed_value = betweenness_directed,
                   betweenness_undirected_name = names(betweenness_undirected),  
                   betweenness_undirected_value = betweenness_undirected,
                   autovalor_name = names(autovalor),
                   autovalor_value = autovalor,
                   autovalor_ponderado_name = names(autovalor_pond),
                   autovalor_ponderado_value = autovalor_pond
                   
                   )
  return(DF)
}


grafo_impos <-  grafo_continente(threshold_pct = 0.01,dataset = dataset2)
grafo_expos <-  grafo_continente(threshold_pct = 0.01,dataset = dataset_expo)

grafo_impos_DF <- top_five_nodos(grafo_impos)
#saveRDS(grafo_impos_DF, "Resultados/tabla_impo.RDS")
write.xlsx(x = as.data.frame(grafo_impos_DF),file =  "Resultados/top5_nodos.xlsx",
           row.names = FALSE,sheetName = "tabla_impo", append = FALSE)

grafo_expos_DF <- top_five_nodos(grafo_expos)
#saveRDS(grafo_expos_DF, "Resultados/tabla_expo.RDS")
write.xlsx(x = as.data.frame(grafo_expos_DF),file =  "Resultados/top5_nodos.xlsx",
           row.names = FALSE,sheetName = "tabla_expo", append = T)


##############importaciones exportaciones
#dirigido      -0.04973885   -0.02976895
#no dirigido   -0.27006093   -0.17769331
#Los países centrales tienen menos dependencia entre sí para sus importaciones que para sus exportaciones.
#Nichos de consumo vs provisión de materias primas???

### Rol como consumidor (grafo expos) y como productor (grafo impos)

expo_vs_impo <- function(data_impo = dataset2, data_expo = dataset_expo, threshold_pct =0.01){
  
  grafo_impo <- trade_to_graph(edges = data_impo, threshold_pct = threshold_pct)$grafo
  grafo_expo <- trade_to_graph(edges = data_expo, threshold_pct = threshold_pct)$grafo
  
  degree_in <- data_frame("orden" = c(paste0(c(1:5),'°')))
  closeness_in <- data_frame("orden" = c(paste0(c(1:5),'°')))
  intermediacion <- data_frame("orden" = c(paste0(c(1:5),'°')))
  autovalor <- data_frame("orden" = c(paste0(c(1:5),'°')))
  autovalor_pond <- data_frame("orden" = c(paste0(c(1:5),'°')))
  
  L = list("impo" = grafo_impo,"expo"=grafo_expo)
  for (tipo in c("impo","expo")) {
    grafo <- L[[tipo]]
    nombres <- paste0(tipo,"_", c('pais','valor'))
    
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

    atvlr_pnd <- sort(eigen_centrality(grafo,directed = F,weights = E(grafo)$TradeValue)$vector, 
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


comparacion_expo_impo <- expo_vs_impo(data_impo = dataset2, data_expo = dataset_expo, threshold_pct =0.01)

for ( i in c(1:length(names(comparacion_expo_impo)))) {
  nombre <- names(comparacion_expo_impo)[i]
  tabla <- comparacion_expo_impo[[nombre]]
  if (i==1) {
    write.xlsx(x = as.data.frame(tabla),file =  "Resultados/comparacion_expo_impo.xlsx",
               row.names = FALSE,sheetName = nombre, append = FALSE)
  }
  else{
    write.xlsx(x = as.data.frame(tabla),file =  "Resultados/comparacion_expo_impo.xlsx",
               row.names = FALSE,sheetName = nombre, append = TRUE)
  }
} 

#saveRDS(comparacion_expo_impo, "Resultados/comparacion_expo_impo.RDS")





