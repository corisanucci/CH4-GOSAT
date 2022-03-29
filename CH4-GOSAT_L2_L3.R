# Author: Corina Sanucci

# Date: 15/02/2021

# Objetivo: Generar rasters de CH4 en área de interés con los datos de GOSAT

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

install.packages("BiocManager")
BiocManager::install("rhdf5")
library(rhdf5); library(raster);library(rgdal);library(magrittr)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

path = 'D:/Tesina/GOSAT/SWIRL3CH4_GU_V02.95/Files/'

AOI <- readOGR('D:/Tesina/SCIAMACHY/SCIAMACHY_L3/AOI.shp')

carpeta_salida <- 'D:/Tesina/GOSAT/CH4_meses/'

# Abro un archivo para inspeccionarlo:
h5f <- H5Fopen(paste(path, 'GOSATTFTS2010060120100630_03C02SV0295.h5', sep = ''))
h5f

# Para ver la estructura del archivo y lo que trae
View(h5ls(h5f,all=T))

h5f$Attribute$metadata # mes 4 del año 2016
h5f$Global$MD_Metadata$identificationInfo$MD_DataIdentification$
  extent$geographicElement$EX_GeographicBoundingBox$
  extentReferenceSystem # esta en WGS84

# Defino la CRS: WGS84 en coordenadas geográficas
crs_project = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Data <- h5readAttributes(h5f, '/Data')

# ¿Que hay en Data?
h5f&'Data'

h5f$Data$mixingRatio$XCH4 # CH4 column-average mixing ratio


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Procesamiento de los HDF5:

GOSATfiles <- dir(path, full.names = T, pattern = '.h5$')

for(i in 1:length(GOSATfiles)){
  
  file <- GOSATfiles[i]

  h5f <- H5Fopen(file)

  # Estos archivos tienen dos unidades: me interesa el mixingRatio
  XCH4 <- h5readAttributes(file,"/Data/mixingRatio/XCH4" )
  # Warning: no modificar el archivo hdf5 mientras lo tengo abierto desde R

  # Extraer las coordenadas de la extensión geográfica
  xMin <- h5f$Global$MD_Metadata$identificationInfo$MD_DataIdentification$extent$
    geographicElement$EX_GeographicBoundingBox$westBoundLongitude%>%as.numeric()

  xMax <- h5f$Global$MD_Metadata$identificationInfo$MD_DataIdentification$extent$
    geographicElement$EX_GeographicBoundingBox$eastBoundLongitude%>%as.numeric()

  yMin <- h5f$Global$MD_Metadata$identificationInfo$MD_DataIdentification$extent$
    geographicElement$EX_GeographicBoundingBox$southBoundLatitude%>%as.numeric()

  yMax <- h5f$Global$MD_Metadata$identificationInfo$MD_DataIdentification$extent$
    geographicElement$EX_GeographicBoundingBox$northBoundLatitude%>%as.numeric()


  # Definir la extensión (left, right, bottom, top)
  rasExt <- extent(xMin,xMax,yMin,yMax)

  # Chequear la extensión: son datos globales
  rasExt

  # class      : Extent 
  # xmin       : -180 
  # xmax       : 180 
  # ymin       : -90 
  # ymax       : 90 

  # Extraemos el valor de píxel sin datos para más tarde: -9999
  # Me fijo en la estructura donde encuentro este dato
  myNoDataValue <- as.integer(h5f$Data$mixingRatio$XCH4[4])
  myNoDataValue

  # Extraemos los datos de metano
  XCH4 <- h5read(h5f,"/Data/mixingRatio/XCH4")

  # Transponer los datos para arreglar filas y columnas invertidas 
  # Dependiendo del formato de los datos este paso puede NO ser necesario (1ro plot para ver)
  XCH4 <- t(XCH4)
  
  # Asignar NA A LOS PIXELES SIN DATOS
  XCH4[XCH4 == myNoDataValue] <- NA

  # Generar un raster con los datos de metano y el crs
  r <- raster(XCH4,crs = crs_project)

  # Asignar la extensión al raster
  extent(r) <- rasExt
  
  # Cortar con AOI
  r_AOI <- crop(r, AOI)
  
  # Enmascarar con el AOI
  r_AOI <- mask(r_AOI, AOI)
  
  # Multiplico por mil para pasar de ppm a ppb
  r_AOI <- r_AOI * 1000
  
  # Generar nombre de archivo
  filename <- gsub('D:/Tesina/GOSAT/SWIRL3CH4_GU_V02.95/Files/|.h5', '', file)
  # Primero reemplazo de L2 a CH4 L3:
  filename <- gsub('TFTS', '_', filename)
  # Luego me quedo con la parte que me interesa:
  filename <- substr(filename, 1, 12)

  writeRaster(r_AOI, paste(carpeta_salida, filename, sep=''),format = 'GTiff')
  
  h5closeAll()
  H5close()
  
}
