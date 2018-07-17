hechos <- list.files('Dataset/COMTRADE/')


for (archivo in hechos) {
    tryCatch({
    print(archivo)  

    datos <- read.csv(paste0('Dataset/COMTRADE/',archivo))
    
    filename <- gsub(pattern = "-","--",archivo)
    
    write.csv(datos,file = paste0('Dataset/COMTRADE/',filename),row.names = FALSE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  
}
