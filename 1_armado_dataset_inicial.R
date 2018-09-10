rm(list = ls())
gc()
library(tidyverse)
library(pryr)
filenames <- list.files("Dataset/COMTRADE/")

columnas <- list()
i <- 1
for (filename in filenames) {
    print(i/1014 *100)
    filepath <- paste0("Dataset/COMTRADE/", filename)
    tryCatch({
      file <- read.csv(filepath)
      columnas[[i]] <- file  %>% colnames(.)
      i <- i+1  
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}


nombres <- columnas[1] %>% unlist(.)
for (archivo in columnas) {
  
  print(unique(archivo == nombres))
}

#Unifico las tablas

dataset <- data.frame()

i <- 1
for (filename in filenames) {
  print(i/547 *100)
  filepath <- paste0("Dataset/COMTRADE/", filename)
  tryCatch({
    file <- read.csv(filepath)
    dataset <- bind_rows(dataset,file)
    i <- i+1  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

object_size(dataset)

## Limpieza dataset
dataset <- readRDS('Dataset/dataset_COMTRADE.rds')

str(dataset)

unique(dataset$yr==dataset$period)


unique(dataset$aggrLevel)
unique(dataset$IsLeaf)
unique(dataset$rgCode)
unique(dataset$rgDesc)
unique(dataset$ptCode2)
unique(dataset$ptTitle2)
unique(dataset$pt3ISO2)
unique(dataset$cstCode)
unique(dataset$cstDesc)
unique(dataset$motCode)
unique(dataset$motDesc)
unique(dataset$cmdDescE)

dataset2 <- dataset %>% 
  select(-X,-period, -IsLeaf, -ptCode2,-ptTitle2, -pt3ISO2, -cstCode, -cstDesc, -motCode, -motDesc,
         -qtCode, -qtDesc,-qtAltCode,-qtAltDesc, -AltQuantity, -NetWeight, -GrossWeight, -CIFValue,
         -FOBValue, -estCode, -TradeQuantity) %>%  
  filter(rgCode %in% c(1,2))

saveRDS(dataset2, file = "Dataset/dataset_COMTRADE.rds")



