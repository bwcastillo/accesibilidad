library(rgdal)

for (i in seq(KMZs)){
zz[[i]]<- getKMLcoordinates(paste0("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/8/",KMZs[i]), ignoreAltitude = T)
zz[[i]] <- do.call(rbind, zz[[i]])
colnames(zz[[i]]) <- c("lon", "lat")
if (class(zz[[i]]) == "matrix" | length(zz) == 1) {print(KMZs[i]);next()}
tmp <- do.call(rbind, zz[[i]])
colnameszz[[i]] <- c("lon", "lat")
LonLat <- rbind(LonLat, zz[[i]])
}

tmp <- getKMLcoordinates(paste0("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/8/",KMZs[i]), ignoreAltitude = T)

  tmp <- do.call(rbind, tmp)
  colnames(tmp) <- c("lon", "lat")
  LonLat <- rbind(LonLat, tmp) #LON LAT Aquí a que objeto referencia?


  
  
  
  From_Kmz_to_Shp_Points <- function(KMZs, folder, dest, name_output){
    LonLat <- data.frame()
    for (i in seq(KMZs)){
      tryCatch(tmp <- getKMLcoordinates(paste0("kml/",folder,"/", KMZs[i]), ignoreAltitude = T), error = function(x){print(KMZs[i]); tmp <- NA})
      if (class(tmp) == "matrix" | length(tmp) == 1) {print(KMZs[i]);next()}
      tmp <- do.call(rbind, tmp)
      colnames(tmp) <- c("lon", "lat")
      LonLat <- rbind(LonLat, tmp)
    }
    LonLat <- LonLat[!is.na(LonLat$lon),]
    sp <- SpatialPointsDataFrame(LonLat, LonLat)
    writeOGR(sp, dest, name_output, "ESRI Shapefile", overwrite_layer = T)
    return(data.frame(LonLat, Ciudad = folder))
  }
  
  setwd("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/")
  tabla <- data.frame()
  for (r in seq(list.files("kml"))){
    folder <- list.files("kml")[r]
    KMZs <- list.files(path= paste0("kml/",folder), pattern="*.kmz", full.names=FALSE)
    tablita <-  From_Kmz_to_Shp_Points(KMZs, folder,
                                       dest = "C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/Shapefiles",
                                       paste0("Ferias_", folder))
    tabla <- rbind(tabla, tablita)
  }


