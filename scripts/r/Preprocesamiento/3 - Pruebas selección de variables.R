rm(list=ls(all=T))

setwd("C:/Users/ghama/Desktop/Minería de Datos/3 - Entregas")

library(dplyr)

#================================================#
#      0. Carga de los datos de temperatura      #
#================================================#

## Datos climáticos imputados
load("5 - Proyecto Procesamiento/0 - Insumos/Datos_preproc.RData")

datos = US_var_selec 
rm(US_var_selec)

#============================================================================#
#        1. Diferencias de medias para analizar importancia de variables     #
#============================================================================#

var = c("Distance(mi)","Temperature(F)","Wind_Chill(F)","Humidity(%)","Pressure(in)",
         "Visibility(mi)","Wind_Speed(mph)","Precipitation(in)","Accident_duration",
         "Road_features", "Traffic_signals" ,"Interest_points")

## Cálculo medias y desviaciones 
medias = aggregate(datos[,var], by = list(datos$Severity), mean, na.rm = T)
desv =  aggregate(datos[,var], by = list(datos$Severity), var, na.rm = T)
conteo = aggregate(!is.na(datos[,var]), by = list(datos$Severity), sum)

medias = as.data.frame(t(medias))
desv = as.data.frame(t(desv))
conteo = as.data.frame(t(conteo))

medias = data.frame( Variable = rownames(medias), apply(medias, 2, as.numeric))
desv = data.frame( Variable = rownames(desv), apply(desv, 2, as.numeric))
conteo = data.frame( Variable = rownames(conteo), apply(conteo, 2, as.numeric))

colnames(medias) = medias[1,]
colnames(desv) = desv[1,]
colnames(conteo) = conteo[1,]

medias = medias[-1,]
desv = desv[-1,]
conteo = conteo[-1,]

colnames(medias)[1] = "Variable"
colnames(desv)[1] = "Variable"
colnames(conteo)[1] = "Variable"

## Cálculo indicador con pares de clases
tabla = NULL

for(i in 2:4){
 for(j in (i+1):5){
  k = data.frame(Variable = medias$Variable)
  k$Indi = abs(medias[,i] - medias[,j])/sqrt( desv[,i]/conteo[,i]  + desv[,j]/conteo[,j] )
  k$Factor = paste0("Clase ", colnames(medias)[i], " y Clase ", colnames(medias)[j])
  
  tabla = rbind(tabla, k)
  rm(k)
  }
}

tabla = tabla %>% 
 data.table::as.data.table() %>% 
 data.table::dcast(Variable ~ Factor, value.var = "Indi") %>% 
 as.data.frame()

xtable::xtable(tabla)

