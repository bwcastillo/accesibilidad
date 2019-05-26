library(rvest)
library(stringr)
library(RCurl)
library(rgdal)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/")

# Definir carpetas de trabajo
setwd("c:/Users/rdgov/ownCloud/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/")

setwd("Raw_Data")

# Definir Región a trabajar
region <- c(1:11,14:15)
region<-1
# Hacer un scrapping de las ferias para obtener los links de las ferias
# En las comunas de la región

#TEST: 


for (i in region){
html<-read_html(paste0("http://www.asof.cl/apps/mapaferias/regiones/",i,".php"))
urls<- html %>% 
  html_nodes("option") %>% 
  html_attr("value")  %>% 
  na.omit()
print(cat(urls))
}

urls <- list()

?html_attr

 

View(urls)
   urls<- html %>% 
     html_nodes( css= "option") %>% 
     html_attr(name =  "value") %>% 
    na.omit()

?html_attr
print(length(urls[i]))

View(urls)

#TEST:print(paste0("http://www.asof.cl/apps/mapaferias/regiones/",i,".php"))
#print(length(urls[i]))
#print(paste0("http://www.asof.cl/apps/mapaferias/regiones/",i,".php"))


# Un sapplay que hae un scrap de las urls de los mapas de cada comuna
maps <- sapply(urls, function(x) {
  read_html(paste0("http://www.asof.cl/apps/mapaferias/regiones/",x)) %>%
    html_nodes("iframe") %>%
    html_attr("src")
}
)
# Un sapply que extrae los ids de los archivos kml de cada comuna

ids <- sapply(maps, function(x) {
  str_extract(read_html(x) %>% 
                html_nodes(xpath = '//meta[@itemprop="url"]') %>% 
                html_attr('content'), "[-_0-9A-Za-z]+$")
}
)



# Descarga los kml de cada comuna en la region
for (mid in ids) {
  namess <- gsub("/", "_", names(ids[ids == mid]))
  namess <- gsub(".php", "", namess)
  dir.create(paste0("kml/",region))
  download.file(paste0("https://www.google.com/maps/d/u/0/kml?mid=",mid,"&forcekml=1"),paste0("kml/",region,"/",namess, "_", mid,".kmz"))
}


# extract long-lat coordinates from a bunch of kmz files
# first unzips the KMZ-s then reads coordinates from each KML with getKMLcoordinates {maptools}
# (worth checking this as well: https://gist.github.com/holstius/6631918 )

library(maptools)
setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/Raw_Data")
# list the kmz files in a given folder path
KMZs <- list.files(path="kml/rms/", pattern="*.kmz", full.names=FALSE)

# unzip each KMZ file and read coordinates with getKMLcoordinates()
# select only the matrix element of the list returned by getKMLcoordinates(), 
# therefore mention the index [[1]]

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

setwd("Raw_Data/")
tabla <- data.frame()
for (r in seq(list.files("kml"))){
  folder <- list.files("kml")[r]
  KMZs <- list.files(path= paste0("kml/",folder), pattern="*.kmz", full.names=FALSE)
  tablita <-  From_Kmz_to_Shp_Points(KMZs, folder,
                                     dest = "../Shapefiles",
                                     paste0("Ferias_", folder))
  tabla <- rbind(tabla, tablita)
}