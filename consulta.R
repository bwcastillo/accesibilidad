Consulta_OTP()
Consulta_OTP <- function(coordinates, viaje.tiempo = 10, 
                         viaje.velocidad = 1.38,
                         mode = "WALK", 
                         outputgeom = "SHED",
                         otp_rest_url='http://otpv2.cedeus.cl/')
  {
# Para no saturar el servidor, se realiza una consulta cada .33 segundos, o 3 consultas por segundo.
# Sys.sleep(.1)
# Crear la URL para extraer el walkshed
 
   consulta.parametros = paste('otp/routers/chile/isochroneOld?fromPlace=', 
                               '-33.5846161,-70.5410151', 
                               '&walkTime=', 
                              viaje.tiempo, 
                              '&walkSpeed=', 
                              viaje.velocidad,
                              '&mode=', 
                              mode, 
                              '&toPlace=-32.5846161,-70.5410151&output=', 
                              outputgeom, sep = "") 
  
  coordinates<-paste(tabla.ciudad[,2], tabla.ciudad[,1], sep = ",")
  
  consulta.url <- paste(otp_rest_url, consulta.parametros, sep  = "") #L?nea
  
  # Extraer el walkshed, el cual viene en formato GeoJSON
  print(consulta.url)
  walkshed.texto <- getURL(consulta.url)
  
  # Contador que define el n?mero de veces que se debe intentar una consulta cuando se encuentran
  # geometr?as incorrectas.
  counter <- 0
  # Loop "while". Si el walkshed contiene las palabras "LineString" o "504 Gateway Time-out"
  # el request vuelve a ejecutarse hasta 5 veces, con 10 segundos entre cada interaci?n del request.
  while((grepl("LineString", walkshed.texto) |
         grepl("504 Gateway Time-out", walkshed.texto)) &
        counter <= 20){
    print("Resultado inv?lido. Solicitando de nuevo.")
    Sys.sleep(.5)
    walkshed.texto <- getURL(consulta.url)
    counter <- counter + 1
  }
  walkshed.polygon <- readOGR(walkshed.texto, "OGRGeoJSON", verbose = F, p4s = "+proj=longlat +datum=WGS84")
 
   return(walkshed.polygon)
  }


Consultor_otp <- function(data, viaje.tiempo = 10, viaje.velocidad = 1.38, mode = "WALK"){
  lista.tmp <- list()
  for (i in seq(data)){
    print(paste("Request number:", i))
    tmp <- Consulta_OTP(data[i])
    if (class(tmp) == "SpatialLinesDataFrame") { print("linestring"); next() }
    lista.tmp <- append(lista.tmp, tmp)
  }
  shp.output <- do.call(bind,lista.tmp)
  return(shp.output)
}


accesibilidad_tabla <- data.frame()
for (c in unique(tabla$Ciudad)){
  tabla.ciudad <- tabla[tabla$Ciudad == c,]
  shp.name <- paste0(c, "_Walksheds")
  if (!file.exists(paste("../Output/Walksheds/", shp.name, ".shp",sep = ""))){
    coords <- paste(tabla.ciudad[,2], tabla.ciudad[,1], sep = ",")
    walkshed.shp <- Consultor_otp(coords)
    print("writing shp")
    writeOGR(walkshed.shp, "../Output/Walksheds", shp.name, driver = "ESRI Shapefile", overwrite_layer = T)
    
  } else {
    walkshed.shp <- readOGR("../Output/Walksheds", shp.name)
  }
  
  poblacion_manzanas.shp <- readOGR("../Raw_Data/Poblacion_Manzanas_Ciudades", paste0(c, "_Poblacion_Manzanas"), encoding = "UTF-8")
  walkshed.shp <- spTransform(walkshed.shp, proj4string(poblacion_manzanas.shp))
  # Los walksheds que tienen contacto con una manzana
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

write.csv(accesibilidad_tabla, "E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Resultados/92._Accesibilidad_a_Ferias/92._Accesibilidad_a_Ferias.csv", row.names = F)
