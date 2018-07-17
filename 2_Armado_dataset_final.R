rm(list = ls())
gc()
library(tidyverse)
library(ggridges)

dataset <- readRDS('Dataset/dataset_COMTRADE.rds')

resumen <- dataset %>%
  group_by(yr) %>% 
  summarise(n = n(),
            suma = sum(TradeValue),
            promedio = mean(TradeValue)) %>% 
  gather(.,variable, valor, 2:4)



ggplot(resumen, aes(yr, valor))+
  geom_col()+
  theme_minimal()+
  scale_x_continuous(breaks = seq(1997,2016,1))+
  facet_grid(variable~., scales =  'free')

#No hay datos después de 2011
dataset %>% 
  group_by(yr, rgDesc) %>% 
  summarise(npaises = unique(rtTitle))


tabla <-as.data.frame(table(dataset$yr,dataset$rtTitle))

tabla %>% 
  filter(Freq>0) %>% 
  group_by(Var1) %>% 
  summarise(n())


dataset2 <- dataset %>% filter(yr<= 2011)


dataset_2011 <- dataset %>% filter(yr == 2011,ptCode != 0, TradeValue>100, rgCode == 1)

ggplot(dataset_2011, aes(TradeValue))+
  geom_density(fill = "lightgreen")+
  theme_minimal()

saveRDS(dataset_2011, "Dataset/dataset_COMTRADE_2011.rds")



dataset_2011_expo <- dataset %>% filter(yr == 2011,ptCode != 0, TradeValue>100, rgCode == 2)

ggplot(dataset_2011_expo, aes(TradeValue))+
  geom_density(fill = "lightgreen")+
  theme_minimal()

saveRDS(dataset_2011_expo, "Dataset/dataset_COMTRADE_expo_2011.rds")





