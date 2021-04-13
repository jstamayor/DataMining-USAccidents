cat("\014")
rm(list=ls(all=T))

library(dplyr)
library(maps)
library(sp)
library(rgdal)


setwd("C:/Users/ghama/Desktop/Minería de Datos/3 - Entregas")

#==================================#
#       0. Entrada de los datos ####
#==================================#
datos = readRDS("0 - Insumos/US_Accidents_Dec20.rds")
datos = as.data.frame(datos)

#===============================================#
#         1. Reverse geocoding              #####
#===============================================#
region_map <- readOGR(dsn = "0 - Insumos/Shapes/Region")
states_map = readOGR("0 - Insumos/Shapes/Estados")
counties_map = readOGR(dsn = "0 - Insumos/Shapes/Condados")
cbsa_map <- readOGR(dsn = "0 - Insumos/Shapes/CBSA")
# division_map = readOGR(dsn = "0 - Insumos/Shapes/divisions")

region_map$REGIONCE = region_map$AFFGEOID = region_map$GEOID = region_map$LSAD = region_map$AWATER = 
  region_map$ALAND = NULL

states_map$STATEFP = states_map$STATENS = states_map$AFFGEOID = states_map$GEOID = states_map$AWATER = 
  states_map$LSAD = states_map$ALAND = NULL

counties_map$STATEFP = counties_map$COUNTYFP = counties_map$COUNTYNS = counties_map$AFFGEOID =
  counties_map$GEOID = counties_map$LSAD = counties_map$ALAND = counties_map$AWATER = NULL

cbsa_map$CSAFP = cbsa_map$CBSAFP = cbsa_map$AFFGEOID = cbsa_map$GEOID = cbsa_map$LSAD = cbsa_map$ALAND =
  cbsa_map$AWATER = NULL

## Función para realizar el geocoding revers
rev_geo <- function(lat,long){
 # To see where the name of the country is stored in the map object, 
 # you need to explore it in R and see the "data" element. 
 # In this case, "NAME" has the information that we want. 
 # The function over returns the name of the country given the coordinates projected 
 # in the countries_map
 
 #First the coordinates are transformed to spatialpoints
 points <- SpatialPoints(matrix(c(long, lat), ncol = 2))
 
 # Region
 proj4string(points) <- proj4string(region_map)
 region <- as.character(over(points, region_map)$NAME)
 
 # States
 #proj4string(points) <- proj4string(states_map)
 state1 <-as.character(over(points, states_map)$STUSPS)
 state2 <-as.character(over(points, states_map)$NAME)
 
 # Counties
 #proj4string(points) <- proj4string(counties_map)
 county <- as.character(over(points, counties_map)$NAME)
 
 #The same for state
 #proj4string(points) <- proj4string(cbsa_map)
 cbsa <- as.character(over(points, cbsa_map)$NAME)
 
 return(list(region = region, state1 = state1, state2 = state2, county = county, cbsa = cbsa) )
}

## Reverse geocoding
coord = datos %>% select(Start_Lat, Start_Lng) %>% distinct()

coord$CAT_ID = as.numeric(as.character(cut(1:nrow(coord), breaks = 1000, labels = c(1:1000) )))
table(coord$CAT_ID)

for(i in 1:length(unique(coord$CAT_ID))){
 startg <- Sys.time()
 
 dd = coord %>% 
  filter(CAT_ID == i) %>% 
  select(CAT_ID, Start_Lat, Start_Lng) %>% 
  mutate(region_rev = rev_geo(lat = Start_Lat, long = Start_Lng)$region
         , state1_rev = rev_geo(lat = Start_Lat, long = Start_Lng)$state1
         , state2_rev = rev_geo(lat = Start_Lat, long = Start_Lng)$state2 
         , county_rev = rev_geo(lat = Start_Lat, long = Start_Lng)$county
         , cbsa_rev = rev_geo(lat = Start_Lat, long = Start_Lng)$cbsa ) %>% 
  as.data.frame()
 
 saveRDS(dd, paste0("0 - Insumos/Arreglo Geo/arreglo_county_", i,".rds"))
 endg <- Sys.time()
 t = round(endg - startg, 2)
 
 print(paste0("Realizado: ", i, " (", t, ")"))
 rm(dd, t, endg, startg)
}

#===========================#
#   Unir todos los archivos #
#===========================#
archi = list.files("0 - Insumos/Arreglo Geo/")

final = NULL
for(i in archi){
  final = rbind(final
                , readRDS(paste0("0 - Insumos/Arreglo Geo/", i)) )
  print(i)
}

saveRDS(final, paste0("0 - Insumos/Arreglo Geo/arreglo_county_final.rds"))
final = readRDS("0 - Insumos/Arreglo Geo/arreglo_county_final.rds")

nrow(final) == nrow(coord)
#========================================================#
#          2. Pegar información a los datos grandes      #
#========================================================#
rm(list=ls()[!ls() %in% c("final","datos") ])

datos_new = datos %>% 
  select(ID, Start_Lat, Start_Lng, State, County, City) %>% 
  left_join(y = final, by = c("Start_Lat","Start_Lng") )

## Analizar calidad de la información
datos_new$state1_rev %>% unique %>% length
datos_new$State %>% unique %>% length

k = datos_new %>% filter(state1_rev != State) 
k[sample(1:nrow(k),5), ]

rm(k)

## Arreglar el problema de los estados
nom = final %>% 
  select(state1_rev, state2_rev) %>% 
  distinct()

datos_new = datos_new %>% 
  select(-state1_rev, -state2_rev) %>% 
  left_join(y = nom, by = c("State" = "state1_rev") )

## Condados
unique(datos_new$county_rev) %>% length
unique(datos_new$County) %>% length

sum(datos_new$county_rev != datos_new$County & !is.na(datos_new$county_rev), na.rm = T)
sum(datos_new$county_rev != datos_new$County, na.rm = T)

sum(is.na(datos_new$County))
sum(is.na(datos_new$county_rev))

k = datos_new %>% filter(county_rev != County)
k[sample(1:nrow(k),10),]

## Análisis de ciudades
datos_city = datos_new %>% select(ID, City)

datos_city$City = tolower(datos_city$City)
datos_city$City = str_remove(datos_city$City, "\\.")
datos_city$City = Hmisc::capitalize(datos_city$City)

## Pegar 
datos_new = datos_new %>% select(-City) %>% 
  left_join(y = datos_city, by = "ID") 

## Arreglar CBSA
datos_new = datos_new %>% 
  mutate(cbsa_rev = trimws(str_match(cbsa_rev, "(.*?)\\,")[,2]) )

def = datos_new %>% 
  select(ID, region_rev, State, state2_rev, County, county_rev, City, cbsa_rev) %>% 
  mutate(region_rev = trimws(region_rev)
         , State = trimws(State)
         , state2_rev = trimws(state2_rev)
         , County = trimws(County)
         , county_rev = trimws(county_rev)
         , City = trimws(City)
         , cbsa_rev = trimws(cbsa_rev) )

saveRDS(def, "0 - Insumos/arreglo_geoinfo.rds")

#=======================================#
#         Unir toda la info             #
#=======================================#
rm(list=ls()[ls() != "def"])

tmc = readr::read_delim("0 - Insumos/datos_tmc.csv", delim = ";") %>% as.data.frame()

ff = left_join(def, tmc, by = "ID")

saveRDS(ff, "0 - Insumos/datos_preprocesamiento.rds")

ff = ff %>% 
  mutate(C = ifelse(is.na(county_rev), County, county_rev)) %>% 
  select(ID, region_rev, State, state2_rev, C, City, Description_TMC) %>% 
  plyr::rename(c(region_rev = "Region", State = "State_Zip", state2_rev = "State_Name", 
                 C = "County_Geo"))

saveRDS(ff, "0 - Insumos/datos_preprocesamiento_geo.rds")

# Problemas: 
# 1. Para los estados se presentan diferencias, explorando aleatoriamente algunos casos, se 
# observa que esto sucede en las fronteras entre estados. Teniendo en cuenta que las diferencias
# no sueceden en muchos casos. Nos quedamos con los condados "originales".
# 2. Para los condados se aprecia el mismo problema, pero también se detectan algunos casos 
# en donde el county_rev sí contiene al punto mientras el county de la base no lo contiene.
# Así mismo, teniendo en cuenta que para corregir estos registros se requeríria mirar manualmente
# fallas en los registros (los cuales son muchos), se opta por usar el county_rev obtenido de los shapes.

