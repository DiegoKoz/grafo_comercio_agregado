library(tidyverse)
library(stringr)


data <- read.csv('Dataset/WB/ARG_AllYears_WITS_Trade_Summary.CSV',header = T )

datos <- data %>% 
  filter(Product.categories == 'All Products',
         !(Partner %in% c('Unspecified', 'World', '...')),
         Indicator.Type == 'Export', Indicator == 'Partner share(%)-Top 5 Export Partner')
unique(data$Indicator)
