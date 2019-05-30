library(maptools)
library(rgdal)


#El siguiente script lee los kml de las ferias y crea un dataframe llamado tabla con 
#el par de coordenadas y la ciudad a la cual pertenecen, de acuerdo al nombre de
#la región que está en la carpeta donde se almacenan

setwd("C:/Users/usuario/Documents/Rwork/Accesibilidad/accesibilidadscript/Raw_Data")

tabla <- data.frame()
for (r in seq(list.files("kml"))){
  folder <- list.files("kml")[r]
  KMZs <- list.files(path= paste0("kml/",folder), pattern="*.kmz", full.names=FALSE)
  tablita <-  From_Kmz_to_Shp_Points(KMZs, folder,  #Función que convierte de KMZ a shape
                                     dest = "../Shapefiles", #lo guarda en "shapefiles
                                     paste0("Ferias_", folder)) #con Ferias y nombre de la carpeta que lo sacó 
  tabla <- rbind(tabla, tablita) #Crea tabla con datos
}

View(tabla)
#Reproducir script de accesibilidad
accesibilidad_tabla <- data.frame()
for (c in unique(tabla$Ciudad)){
  tabla.ciudad <- tabla[tabla$Ciudad == c,]
  shp.name <- paste0(c, "_Walksheds")
  if (!file.exists(paste("../Output/Walksheds/", shp.name, ".shp",sep = ""))){
    coords <- paste(tabla.ciudad[,2], tabla.ciudad[,1], sep = ",")
    walkshed.shp <- Consultor_otp(coords) #OJO CON ESTA FUNCIÓN, QUIERO EVITAR USARLA
    print("writing shp")
    writeOGR(walkshed.shp, "../Output/Walksheds", shp.name, driver = "ESRI Shapefile", overwrite_layer = T)
    
  } else {
    walkshed.shp <- readOGR("../Output/Walksheds", shp.name)
  }
  
  #LEE la población por manzanas de cada ciudad y crea un shape (supongo que junta todas las ciudades)
  poblacion_manzanas.shp <- readOGR("../Raw_Data/Poblacion_Manzanas_Ciudades", paste0(c, "_Poblacion_Manzanas"), encoding = "UTF-8")
  
  #Luego hacemos que tenga la misma proyección los walkshed con la población manzana
  walkshed.shp <- spTransform(walkshed.shp, proj4string(poblacion_manzanas.shp))
  
  # Los walksheds que tienen contacto con una manzana, simula un intersect
  manzanas.subset <- poblacion_manzanas.shp[walkshed.shp,]
  
  # Identificar las manzanas con acceso seg?n ID, las que no tienen acceso, y exportar a shp
  accesibilidad.shp <- poblacion_manzanas.shp[,c("ID_W", "Pob")]
  accesibilidad.shp@data$Acceso <- "Dummy"
  accesibilidad.shp@data[accesibilidad.shp@data$ID_W %in% 
                           manzanas.subset@data$ID,]$Acceso <- "S?"
  accesibilidad.shp@data[!(accesibilidad.shp@data$ID_W %in% 
                             manzanas.subset@data$ID),]$Acceso <- "No"
  accesibilidad.carpeta <- paste("../Output/Accesibilidad", sep = "")
  writeOGR(as(accesibilidad.shp, "SpatialPolygonsDataFrame" ), 
           accesibilidad.carpeta, 
           paste(c, "Acceso_Ferias", sep = "_"),
           driver = "ESRI Shapefile",overwrite_layer=TRUE)
  

  # Calcular la poblaci?n con acceso y sin acceso
  pob.acceso <- accesibilidad.shp@data$Pob[accesibilidad.shp@data$Acceso == "S?"]
  pob.total <- accesibilidad.shp@data$Pob
  accesibilidad <- data.frame("Ciudad" = c, "Porcentaje Acceso" = round(sum(pob.acceso)/sum(pob.total)*100, 2))
  accesibilidad_tabla <- rbind(accesibilidad_tabla, accesibilidad)
}

write.csv(accesibilidad_tabla, "C:/Users/usuario/Documents/Rwork/Accesibilidad/accesibilidadScript/92._Accesibilidad_a_Ferias.csv", row.names = F)
