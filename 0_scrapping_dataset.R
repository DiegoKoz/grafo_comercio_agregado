rm(list = ls())
library(tidyverse)
library(rjson)
library(qdapRegex)

reporters <- fromJSON(file="https://comtrade.un.org/data/cache/reporterAreas.json")
reporters <- as.data.frame(t(sapply(reporters$results,rbind))) 
reporters <- reporters %>% filter(V1 != 'all') 
reporters$repcode <- unlist(reporters$V1)
reporters$country <- unlist(reporters$V2)
reporters <- reporters%>% select(repcode, country)

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="recent"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc= 'TOTAL,AG1'
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

periodos <- 1997:2016
periods <- c()
for (i in seq(1,length(periodos),by = 5)) {

  ps <- paste(periodos[i:(i+4)], collapse = ', ')
  periods <- c(periods,ps)
    
}



while (TRUE) {
      
    
    iterator <- expand.grid(reporters$repcode,periods) %>% rename("repcode" = "Var1", "periodo"="Var2")
  
    hechos <- data.frame(filename = list.files('Dataset/COMTRADE/'))
    hechos$repcode <- gsub('_.*$','',hechos$filename)
    hechos$periodo <- rm_between(hechos$filename, "--", "--", extract=TRUE)
    hechos$periodo <- unlist(hechos$periodo)
    hechos <- hechos %>% filter(filename != "tmp")
    
    iterator <-   left_join(iterator,hechos,by = c("repcode", "periodo"))
    
    iterator_restan <- iterator %>% filter(is.na(filename))
    
    print("---------------------------")
    print(paste0("FALTAN: ", nrow(iterator_restan)))
    print("---------------------------")
    
    for (repcode in iterator_restan$repcode) {
      print(repcode)  
      tryCatch({    
        periodos_restan <- iterator_restan$periodo[iterator_restan$repcode == repcode]
          for (ps in periodos_restan) {
              
            descarga <- get.Comtrade(r=repcode, p="all", ps = ps)
            data <- descarga$data
            country <-  reporters$country[reporters$repcode ==repcode]
            filename <- paste0(repcode, '_',country,'--',ps,'--','.csv')
            write.csv(data, paste0('Dataset/COMTRADE/',filename))
            
            print(country)
            print(ps)
            print(descarga$validation['status.name'])
            print(descarga$validation['status.description'])
            print(descarga$validation['message'])
            print(descarga$validation['message'])
            print(Sys.time())
            print('---------------------------------------')
        
            Sys.sleep(2)
    }
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
      
  }
    Sys.sleep(1800)
    
}

# 
# ### Cambiar proxy
# library(httr)
# library(rvest)
# 
# content(GET("http://comtrade.un.org/api/get?max=50000&type=C&freq=A&px=HS&ps=now&r=583&p=all&rg=all&cc=TOTAL&fmt=json"), "parsed")
# 
# 
# content(GET("https://ifconfig.co/json", use_proxy("85.26.146.169", 80)), "parsed")
# 
# s <- html_session("http://hadley.nz", use_proxy("51.15.2.43", 3128))
# 
# 


