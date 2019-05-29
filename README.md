# accesibilidad: Memo de lo que he realizado por el momento y del flowork.

multipleKMZ.R
  WebScrapping:
     -Consulta por las ferias en la web de asociación de ferias
     -Consigue los KML
     -Crea carpetas para cada región
     -Guarda los KML todos en una sola carpeta (debo optimizar esta parte del código)
       
      -Tareas pendientes para optimizar este script:
      1)La primera es que en el objeto que registra todas las comunastambién aparezca la región a la cual pertenece
      - bucle for de la línea 30
      2)Lograr guardar los archivos kmz de las ferias comunales en la carpeta de la región a las cuales pertenecen
       
       
   KMZ to SHAPE
       -Hecho manualmente a través de un bucle for en la línea 139-140:
       -Lee las coordenadas de cada archivo kmz y las almacena en un objeto
       -Luego con este objeto crea un shape de puntos que resume todas las ferias de Chile que estaban en formato kmz
       -Optimizar y automatizar lo anterior con la función de la línea 160, en teoría es hacer lo mismo que los pasos anteriores.
       
          
BeautifullAccesibilityScript.R
    Crea función "isocrona": 
        -Una función muy bella creada por RV que permite consultar los walkshed 
        -Le agregué un argumento que permite guardar los walkshed en formato geojson
    
    Consulta walkShed:
        -A través de un bucle for que ejecuta la función isocrona
        -Descarga los walkshed según la cantidad de ferias 
        
posibilityQuerys_experimentos.R:
    Resume info que son parámetros de la función BeautifulAccesibility script
    
