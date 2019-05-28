library(rgdal)
library(maptools)
library(magicfor)


magic_for(silent=FALSE)
for (i in seq(KMZs)) (print(KMZs[i]))


for (i in seq(KMZs))
{tmp <- getKMLcoordinates(paste0("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/1/",KMZs[i]), ignoreAltitude = T)
  put(tmp)}

magic_result_as_vector()


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

