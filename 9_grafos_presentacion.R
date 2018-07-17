rm(list = ls())
gc()
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(igraph)
library(countrycode)
library(ggrepel)


##### Funciones #####

#rep(letters[-1],length(letters[-1]))
toy_df <- data_frame(entrada = letters[-c(1:15)] , salida = "a") %>% 
  bind_rows(., data_frame(entrada = rep("a",length(letters[-c(1:24)])), salida = letters[-c(1:24)]))

toy_df <- toy_df %>% 
  mutate(color = case_when(salida == "a" ~"#E41A1C",
                           entrada == "a" ~"#377EB8") )


DF_red <- graph_from_data_frame(toy_df,directed = TRUE)


V(DF_red)$color <- palette_pander(length(V(DF_red)))
#"gray80"
E(DF_red)$edge.color <- E(DF_red)$  color
l <-layout_nicely(DF_red)

png(paste0("graficos/toy_graph1.png"))

plot(DF_red,edge.arrow.size=.2,vertex.frame.color="#ffffff",
     vertex.label="", vertex.label.color="black", vertex.size = 10, 
     layout=l,edge.curved = -0.45)
dev.off()
