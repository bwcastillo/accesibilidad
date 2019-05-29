install.packages("magicfor")

library(rvest)
library(stringr)
library(RCurl)
library(rgdal)
library(httr)
library(magicfor)
library(tidyr)

setwd("E:/Owncloud Cedeus/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/")

# Definir carpetas de trabajo
setwd("c:/Users/rdgov/ownCloud/Indicadores_de_Sustentabilidad_-_Datos/92._Accesibilidad_a_Ferias/")

setwd("Raw_Data")

# Definir Región a trabajar
region <- c(1:11,14:15)
#TESTfor (i in region){print(i)}
rm<-c("rm")
# Hacer un scrapping de las ferias para obtener los links de las ferias
# En las comunas de la región

#TEST: 


magic_for(silent = TRUE)

for (i in region){
html<-read_html(paste0("http://www.asof.cl/apps/mapaferias/regiones/",i,".php"))
urls<- html %>% 
  html_nodes("option") %>% 
  html_attr("value")  %>% 
  na.omit()
put(print(urls))
}

#Guardando los resultados como vector ya que como dataframe fallan
#por el hecho de que la iteraci?n ocurre 13 veces, cre?ndose 13 columnas en un df
#y las respuestas de esta cantidad de iteraciones son 151
results<-magic_result_as_vector()  
urls<-results

#CONSULTA PARA SANTIAGUITO:
for (i in rm){
html<-read_html(paste0("http://www.asof.cl/apps/mapaferias/",i,".php"))
urlsan<- html %>% 
  html_nodes("option") %>% 
  html_attr("value")  %>% 
  na.omit()
put(print(urlsan))
}
#GuardandoResultado para santiago
View(results)

resultsanti<-magic_result_as_vector()  


#TEST para entender el bucle for:
#print(paste0("http://www.asof.cl/apps/mapaferias/regiones/",i,".php"))
#print(length(urls[i]))
#print(paste0("http://www.asof.cl/apps/mapaferias/regiones/",i,".php"))



# Un sapplay que hae un scrap de las urls de los mapas de cada comuna
maps <- sapply(urls, function(x) {
  read_html(paste0("http://www.asof.cl/apps/mapaferias/regiones/",x)) %>%
    html_nodes("iframe") %>%
    html_attr("src")
}
)
# Un sapplay que hae un scrap de las urls de los mapas de cada comuna only Santiago
mapsanti <- sapply(resultsanti, function(x) {
  read_html(paste0("http://www.asof.cl/apps/mapaferias/",x)) %>%
    html_nodes("iframe") %>%
    html_attr("src")
}
)

#Reflexi?n de la funci?n maps: por ser ambicioso t?cnica e intelectualmente
#quise rescatar todas las comunas de todas las regiones (151)
#sin embargo la funci?n maps me est? corriendo un error com?n, 
#que no me puede abrir la p?gina y es probable que es por que el link que va 
#hacia una comuna espec?fica no tenga ferias y por ende no exista.
#Por lo mismo no debo olvidar mi objetivo que me permite hacer las cosas de forma
#m?s rudimentaria, solo tenemos que evaluar la accesibilidad para 3 regiones
#Una forma de abordar este problema es ver con cual valor se est? provocando el bug y omitirlos
#la otra soluci?n es hacer las cosas m?s simple, o sea un plan b.

#TEST para el bucle del testeador de errores:
for (i in seq(urls)) {print(paste0("http://www.asof.cl/apps/mapaferias/regiones/",urls[i]))}
for (i in seq(urls)) {print(http_error(paste0("http://www.asof.cl/apps/mapaferias/regiones/",urls[i])))
 print(paste0(urls[i]))}


#TESTEANDO ERRORES: HASTA EL MOMENTO RESULTA PERO PODR?A SER M?S LINDO >:(, por ejemplo que diga nombre comuna que falla
magic_for(silent = TRUE)
#Ver links http error 404
for (i in seq(urls)){error<-http_error(paste0("http://www.asof.cl/apps/mapaferias/regiones/",urls[i]))
incorrecto<-paste(error,urls[i], sep=",")
put(print(incorrecto))}
errores<-magic_result_as_vector()  
#Hasta aqui me genera un vector respuesta con el nombre de la comuna, todo junto
#solo separado por una coma


#TESTEANDO ERRORES PARA SANTIAGO: HASTA EL MOMENTO RESULTA PERO PODR?A SER M?S LINDO >:(, por ejemplo que diga nombre comuna que falla
#PROCESO INNECESARIO PARA SANTIAGO PORQUE TODAS SUS COMUNAS ME ARROJAN URL Y NINGUNA ERROR TYPE 404
magic_for(silent = TRUE)
#Ver links http error 404
for (i in seq(resultsanti)){errorsanti<-http_error(paste0("http://www.asof.cl/apps/mapaferias/",resultsanti[i]))
incorrectosanti<-paste(errorsanti,resultsanti[i], sep=",")
put(print(incorrectosanti))}
erroresanti<-magic_result_as_vector()  
View(erroresanti)
#Hasta aqui me genera un vector respuesta con el nombre de la comuna, todo junto
#solo separado por una coma

#dejando nuestros resultados hermosos: separando columna seg?n atributo 
testsplit<-as.data.frame(errores)
testsplit<-separate(data=testsplit,col=errores, into=c("condicion", "comuna"), ",")
testsplit<-(as.data.frame(testsplit))
View(testsplit)

#dejando nuestros resultados de Santiago hermosos: separando columna seg?n atributo 
testsplitsanti<-as.data.frame(erroresanti)
testsplitsanti<-separate(data=testsplitsanti,col=erroresanti, into=c("condicion", "comuna"), ",")
testsplitsanti<-(as.data.frame(testsplitsanti))
View(testsplitsanti)

#Eliminador de filas con comunas que marcan error 404
urls<-testsplit[testsplit$condicion==FALSE,]
urls<-urls$comuna

#Eliminador de filas con comunas que marcan error 404
urlsanti<-testsplitsanti[testsplitsanti$condicion==FALSE,]
urlsanti<-urlsanti$comuna
View(urlsanti)
#Ahora me devuelvo a la l?nea 51 a volver a aplicar las urls v?lidas
#Vengo del futuro: result? <3

# Un sapply que extrae los ids de los archivos kml de cada comuna
ids <- sapply(maps, function(x) {
  str_extract(read_html(x) %>% 
                html_nodes(xpath = '//meta[@itemprop="url"]') %>% 
                html_attr('content'), "[-_0-9A-Za-z]+$")
}
)

# Un sapply que extrae los ids de los archivos kml de cada comuna en Santiago
idsanti <- sapply(mapsanti, function(x) {
  str_extract(read_html(x) %>% 
                html_nodes(xpath = '//meta[@itemprop="url"]') %>% 
                html_attr('content'), "[-_0-9A-Za-z]+$")
}
)
View(idsanti)

setwd("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_data/kml")# list the kmz files in a given folder path


for (mid in ids) {print(mid)}

#Creando directorios para almacenar, lo hice desde RawData no desde kml, 
for (i in seq(region)){ dir.create(paste0("kml/",region[i]))}
for (i in region){ print(i)}


# Descarga los kml de cada comuna en la region (CREO QUE EL SEGUNDO FOR DENTRO DEL ORIGINAL ES INNECESARIO)
for (mid in ids) {
  namess <- gsub("/", "_", names(ids[ids == mid]))
  namess <- gsub(".php", "", namess)
 for (i in region){
  download.file(paste0("https://www.google.com/maps/d/u/0/kml?mid=",mid,"&forcekml=1"),paste0("kml/",i,"/",namess, "_", mid,".kmz"))}
}

# Descarga los kml de cada comuna en la REGION METROPOLITANA RM
for (mid in idsanti) {
  namessanti <- gsub("/", "_", names(idsanti[idsanti == mid]))
  namessanti <- gsub(".php", "", namessanti)
  download.file(paste0("https://www.google.com/maps/d/u/0/kml?mid=",mid,"&forcekml=1"),paste0("rm/",namessanti, "_.kmz"))}


#Un problema que se arrastra: el segundo argumento de download file originalmente no  es i sino que regi?n
#como quer?a automatizar todo el proceso cree un for dentro de otro for, pero esto no soluciona el problema
#sino que repite el proceso de descarga de todos los archivos en cada carpeta creada 
#Una soluci?n r?pida,
#hubiera sido haber creado una primera base de datos que me diga a que regi?n pertenece cada comuna
#Para luego decirle al bucle o a una funci?n,"Almacena las comunas de la regi?n X en la carpeta de la regi?n X"


# extract long-lat coordinates from a bunch of kmz files
# first unzips the KMZ-s then reads coordinates from each KML with getKMLcoordinates {maptools}
# (worth checking this as well: https://gist.github.com/holstius/6631918 )


library(maptools)

#Lista de KMZ de todas las ferias de Chile
KMZs <- list.files(path="C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/1", pattern="*.kmz", full.names=FALSE)

#LISTA KMZ de todas las ferias de Santiago
KMZsanti <- list.files(path="C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/rm", pattern="*.kmz", full.names=FALSE)
View(KMZs)

#TEST del bucle for en set de datos:
for (i in 1:length(KMZs)) { print(KMZs[i])}
for (i in 1:length(KMZsanti)) { print(KMZsanti[i])}


#Test del bucle for para generar todas las coordenadas para la elaboración del shape:
LonLat <- data.frame()

for (i in seq(KMZs)) {
  tryCatch(tmp<-getKMLcoordinates(paste0("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/1/",KMZs[i]), ignoreAltitude=T), error = function(x){print(KMZs[i]); tmp <- NA})
  tmp <- do.call(rbind, tmp)
  colnames(tmp) <- c("lon", "lat")
  LonLat <- rbind(LonLat, tmp)
  print(tmp)
} 
sp <- SpatialPointsDataFrame(LonLat, LonLat)

writeOGR(sp, dsn="C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/Shapefiles/feriasChile", layer="feriasChile", driver="ESRI Shapefile", overwrite_layer = T)


#RECORDAR QUE SIRVE HACER ESTE TEST PARA VER CUALES KMZ NO TIENEN OBJETOS Y POR LO TANTO TAMPOCO COORDENADAS
#Me arrojará un error cuando no encuentre el dataframe, como si quisiera entrar a un objeto que no tiene df
#Por ese motivo es el error
#TEST SANTIAGO: Nuevamente no hice la función, porque me costó mucho entender la forma manual, más la función
LonLatSanti <- data.frame()
for( i in seq(KMZsanti)) {
  tryCatch(tmpsanti<-getKMLcoordinates(paste0("C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/kml/rm/",KMZsanti[i]), ignoreAltitude=T), error = function(x){print(KMZs[i]); tmp <- NA})
  tmpsanti <- do.call(rbind, tmpsanti)
  colnames(tmpsanti) <- c("lon", "lat")
  LonLatSanti<-rbind(LonLatSanti, tmpsanti)
  print(tmpsanti)
  }
spSanti <- SpatialPointsDataFrame(LonLatSanti, LonLatSanti)
writeOGR(spSanti, dsn="C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/Shapefiles/feriasRM", layer= "rm", driver="ESRI Shapefile", overwrite_layer = T)


#Eliminando dato formato kmz que no tiene una coordenada
KMZs<-KMZs[(KMZs!="coyhaique_1BmSNET1W2COfTHgKNbAe4935kOM.kmz")]
View(KMZs)

#Eliminando comunas formato kmz que no tiene una coordenadas en santiago
KMZsanti<-KMZsanti[(KMZsanti!="rm_curacavi_.kmz")]
KMZsanti<-KMZsanti[(KMZsanti!="rm_melipilla_.kmz")]
KMZsanti<-KMZsanti[(KMZsanti!="rm_talagante_.kmz")]
View(KMZsanti)

# unzip each KMZ file and read coordinates with getKMLcoordinates()
# select only the matrix element of the list returned by getKMLcoordinates(), 
# therefore mention the index [[1]]


#Hice la funci?n manual desde la 139 hasta la 144.
#Soy tan estúpido que no soy capaz y a veces no se como poner los argumentos
#Que solicita y que le complican a la funci?n, en este caso "FROM_KMZ_TO_SHP_POINTS"
#LO QUE EN TEORÍA HACE ESTA FUNCIÓN ES:


#Toma la lista"KMZs" la cual son los nombres de los archivos formato .kmz contenidos en una carpeta
#Crea el objeto LonLat que se define como un dataframe
#Luego a través de un bucle for automatiza o itera según el largo de los objetos que estamos analizando lo siguiente:
#1.corre este set en la función getKMLcoordinates, donde devuelve el par de coordenadas de cada archivo .kmz almacenándolas en un objeto definido ("tmp") a través de la función tryCatch
#2.Luego junta toda estas listas de coordenadas  del archivo tmp con do.call donde el primer argumento es rbind o sea junta las filas de objeto tmp que almacena las coordenadas
#3.Posteriormente le asignamos nombres a las columnas de nuestro archivo tmp
#4.Finalmente unimos filas con rbind LonLat del dataframe creado inicialmente con las coordenadas almacenadas en el objeto tmp de los kmz
#Luego de hacer lo anterior a través del loop for crea el objeto espacial sp a través de SpatialPointsDataFrame con las coords de LonLat y data Lonlat
#Finalmente guarda el objeto sp creado con writeOGR


From_Kmz_to_Shp_Points <- function(KMZs, folder, dest, name_output){
  LonLat <- data.frame()
  for (i in seq(KMZs)){
    tryCatch(tmp <- getKMLcoordinates(paste0("kml/1/", KMZs[i]), ignoreAltitude = T), error = function(x){print(KMZs[i]); tmp <- NA})
    if (class(tmp) == "matrix" | length(tmp) == 1) {print(KMZs[i]);next()}
    tmp <- do.call(rbind, tmp)
    colnames(tmp) <- c("lon", "lat")
    LonLat <- rbind(LonLat, tmp)
  }

  LonLat <- LonLat[!is.na(LonLat$lon),]
  sp <- SpatialPointsDataFrame(LonLat, LonLat)
  writeOGR(sp, dsn="C:/Users/usuario/Documents/Rwork/Accesibilidad/Raw_Data/Shapefiles/feriasRM", layer= name_output, driver="ESRI Shapefile", overwrite_layer = T)

  return(data.frame(LonLat, Ciudad = folder))
}


#TESTING:
for(i in seq(KMZs)) { print(KMZs[i])}


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