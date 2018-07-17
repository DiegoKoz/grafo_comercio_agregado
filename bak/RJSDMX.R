##SDMX Connector for R (RJSDMX)## 
##https://github.com/amattioc/SDMX/wiki/SDMX-Connector-for-R-(RJSDMX)##
##https://wits.worldbank.org/witsapiintro.aspx##

require(devtools) 
install_github(repo = "amattioc/SDMX", subdir = "RJSDMX")
library(RJSDMX)

help(RJSDMX)

#Lista de proveedores:
getProviders()

#Lista de datos de un proveedor
getFlows('ECB')

getDimensions(provider = "WITS",dataflow = "DF_WITS_TradeStats_Trade")
#Datos
datos <-getTimeSeries(provider = "WITS",id = "DF_WITS_TradeStats_Trade")
