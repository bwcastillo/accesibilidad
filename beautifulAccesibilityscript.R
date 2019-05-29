install.packages("sf")
install.packages("geojsonio")
install.packages("plotKML")
library(tidyverse); 
library("sf"); 
library(RCurl);
library(glue)
library(rvest)
library(rgdal)
library(geojsonio)
library(plotKML)


isocrona <- function(coords, mode='WALK', viaje.tiempo=10, viaje.velocidad = 1.138, write){
  consulta.url <- paste0('http://146.155.17.41:15080/otp/routers/enero2019/isochroneOld?fromPlace=', 
                         coords, 
                         '&walkTime=', viaje.tiempo, 
                         '&walkSpeed=', viaje.velocidad,
                         '&mode=', mode, 
                         "&time=", "2019-05-21T08:00:00",
                         '&toPlace=-33.5846161,-70.5410151&output=', 
                         "SHED") 
  
 print(consulta.url)
  walkshed.texto <- getURL(consulta.url)
  polygon <- read_sf(walkshed.texto) %>%
    st_transform(4326) %>%
    st_zm("ZM")
#print(class(polygon))
#return(polygon)
  write=geojson_write(input=polygon, file=paste0("C:/Users/usuario/Documents/Rwork/Accesibilidad","total",i,".geojson"))
}

#Bucle que recorre la función
total<-nrow(ptos)
for (i in 1:total){
isocrona(coords=paste(ptos[i,]$lat, ptos[i,]$lon, sep=",") )
}


#backup
total<-nrow(ptos)
for (i in 1:total)
{isocrona(coords=paste(ptos[i,]$lat, ptos[i,]$lon, sep=","))}

#EL paste0 debe ir luego del atributo que deseo rellenar copiar iteradamente

#TEST:
for (i in 1:total)
{print(paste(ptos[i,]$lat, ptos[i,]$lon, sep=","))
}

