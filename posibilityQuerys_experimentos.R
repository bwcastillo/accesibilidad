library(tidyverse); library(sf); library(RCurl); library(glue)

homedir <- "c:/Users/rdgov/Documents/Magister Desarrollo Urbano/Planificacion Urbana/"
input <- glue("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/Shapefiles/feriasrm")

ptos <- read_sf(dsn = input, layer = "rm") %>%
st_set_crs(4326)
View(ptos)
isocrona()
coordinates <- st_coordinates(ptos)[1,]
coordinates <- paste(coordinates[2], coordinates[1], sep = ",")

isocrona <- function(coords, mode, viaje.tiempo, viaje.velocidad = 1.138){
  consulta.url <- paste0('http://146.155.17.41:15080/otp/routers/enero2019/isochroneOld?fromPlace=', 
                         coordinates, 
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
  return(polygon)
}


times <- c(16, 39, 19)
modes <- c("WALK", "WALK,TRANSIT", "CAR")
for (feria in 1:nrow(ptos)){
  coordinates <- st_coordinates(ptos)[feria,]
  coordinates <- paste(coordinates[2], coordinates[1], sep = ",")
  for (mode in 1:length(modes)){
    modo <- modes[mode]
    print(paste(feria, times[mode], modo))
    isocronito <- isocrona(coordinates, modo, viaje.tiempo = times[mode]) %>%
      mutate(id = feria,
             feria = ptos$name[ptos$id == feria],
             coordinates = coordinates,
             tiempo = times[mode],
             modo = modo)
    if (feria == 1 & mode == 1){
      isocronote <- isocronito
    } else {
      isocronote <-  rbind(isocronito, isocronote)
    }
  }
}

st_write(isocronote, dsn = glue("{input}/isocronas"), 
         layer = "ferias_iso", driver = "ESRI Shapefile",
         update = TRUE, delete_layer = T)


total<-nrow(ptos)
for (i in 1:total)
{ coordina<-paste(ptos[i,]$lat, ptos[i,]$lon)
  print(coordina)}



isocrona <- function(ptos, mode='WALK', viaje.tiempo=10, viaje.velocidad = 1.138){

  consulta.url <- paste0('http://146.155.17.41:15080/otp/routers/enero2019/isochroneOld?fromPlace=', 
                         ptos, 
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
  return(polygon)

  
  }



