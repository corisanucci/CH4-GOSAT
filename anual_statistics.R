# Author: Corina Sanucci

# Date: 18/03/2021

# Objetivo: Generar rasters con estadisticos anuales para CH4-GOSAT

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(raster);library(magrittr);library(lubridate)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

path = 'D:/Tesina/GOSAT/CH4_meses/'

# Directorio donde guardar los productos generados:

salida <- 'D:/Tesina/GOSAT/CH4_anuales/'

media <- paste(salida,'media/')
mediana <- paste(salida,'mediana/')
MAD <- paste(salida,'MAD/')
min_folder <- paste(salida,'min/')
max_folder <- paste(salida,'max/')
N <- paste(salida,'N/')

# Cargo lista con todos los rasters L3
lista.r <- list.files(path, full.names = T, pattern = '.tif$')

# Stack con todos los rasters
rstack <- stack(lista.r)

# Me quedo con la parte que me interesa del nombre para generar un indice por años
fechas <- substr(names(rstack), 7, 14)%>%ymd()
indx <- format(fechas, '%Y')%>%as.numeric()

# Asigno el indice como nombre de las capas al stack
names(rstack) <- indx

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Estadisticos:

# Calculamos los niveles medios de CH4 para cada año
rmedia <- stackApply(rstack, indx, fun = mean, na.rm = T) 
# con na.rm = T le pedimos que no use los NA para calcular las medias 

for(i in 1:nlayers(rmedia)){
  
  filename <- substr(names(rmedia[[i]]), 7, 10)
  
  filename <- paste(filename, '_CH4mean', sep = '')
  
  writeRaster(rmedia[[i]], paste(media, filename, sep=''),
              format = 'GTiff')
  
}

rm(rmedia);rm(media)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rmediana <- stackApply(rstack, indx, fun = median, na.rm = T)

for(i in 1:nlayers(rmediana)){
  
  filename <- substr(names(rmediana[[i]]), 7, 10)
  
  filename <- paste(filename, '_CH4median', sep = '')
  
  writeRaster(rmediana[[i]], paste(mediana, filename, sep=''),
              format = 'GTiff')
  
}

rm(rmediana);rm(mediana)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rmin <- stackApply(rstack, indx, fun = min, na.rm = T)

for(i in 1:nlayers(rmin)){
  
  filename <- substr(names(rmin[[i]]), 7, 10)
  
  filename <- paste(filename, '_CH4min', sep = '')
  
  writeRaster(rmin[[i]], paste(min_folder, filename, sep=''),
              format = 'GTiff')
  
}

rm(rmin);rm(min_folder)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

rmax <- stackApply(rstack, indx, fun = max, na.rm = T)

for(i in 1:nlayers(rmax)){
  
  filename <- substr(names(rmax[[i]]), 7, 10)
  
  filename <- paste(filename, '_CH4max', sep = '')
  
  writeRaster(rmax[[i]], paste(max_folder, filename, sep=''),
              format = 'GTiff')
  
}

rm(rmax);rm(max_folder)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

Nfun <- function(x, ...) { sum(!is.na(x)) }
# the function needs to take an na.rm argument,
#                         perhaps via the dots (which it may ignore)

# The function you pass should accept or ignore a na.rm argument
#                         To ignore, include ... in the function arguments

# Para aceptar el argumento externo de na.rm, en lugar de ... va na.rm
# Nfun <-  function(x, na.rm) { sum(!is.na(x), na.rm = na.rm)}

rN <- stackApply(rstack, indx, fun = Nfun, na.rm = F)

for(i in 1:nlayers(rN)){
  
  filename <- substr(names(rN[[i]]), 7, 10)
  
  filename <- paste(filename, '_CH4_N', sep = '')
  
  writeRaster(rN[[i]], paste(N, filename, sep=''),
              format = 'GTiff')
  
}

rm(rN);rm(N)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

rMAD <- stackApply(rstack, indx, fun = mad, na.rm = T)

for(i in 1:nlayers(rMAD)){
  
  filename <- substr(names(rMAD[[i]]), 7, 10)
  
  filename <- paste(filename, '_CH4_MAD', sep = '')
  
  writeRaster(rMAD[[i]], paste(MAD, filename, sep=''),
              format = 'GTiff')
  
}

rm(rMAD);rm(MAD)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #