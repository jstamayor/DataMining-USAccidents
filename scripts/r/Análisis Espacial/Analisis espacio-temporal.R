library(leaflet)
library(maptools)
library(geojsonio)

load("~/Tabla_seleccionada.RData")

estados <- read.csv("~/estados.txt", header=T)

states <- 
  geojson_read( 
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

#Arreglo base
df_abrev<-merge(data.frame(states@data),estados,by.x="name",by.y="Estado","all")
df_abrev$Abrev<- gsub(" ", "", df_abrev$Abrev, fixed = TRUE)

acc_x_est<-data.frame(table(US_var_selec$State))
colnames(acc_x_est)<-c("Estado","Cantidad")
acc_x_est$Estado<-as.character(acc_x_est$Estado)

acc_coord_est<-merge(df_abrev,acc_x_est,by.x="Abrev",by.y="Estado")
estad_faltan<-data.frame(rbind(rbind(c("AK","Alaska","02",0,0),c("HI","Hawaii","15",0,0)),c("PR","Puerto Rico","72",0,0)))
colnames(estad_faltan)<-colnames(acc_coord_est)
acc_coord_est_f<-rbind(acc_coord_est,estad_faltan)
acc_coord_est<-acc_coord_est_f[order(acc_coord_est_f$id),]

#Reemplazo de los datos en el spatial point
states@data$density<-as.numeric(acc_coord_est$Cantidad)

## Dividiendo la informacion por años

US_var_selec$Weather_Timestamp <- as.POSIXct(US_var_selec$Weather_Timestamp)
US_var_selec$year<-format(US_var_selec$Weather_Timestamp,"%Y")

#1. 2016
US_var_2016<-subset(US_var_selec,US_var_selec$year==2016)

#Arreglo base
df_abrev<-merge(data.frame(states@data),estados,by.x="name",by.y="Estado","all")
df_abrev$Abrev<- gsub(" ", "", df_abrev$Abrev, fixed = TRUE)


acc_x_est_2016<-data.frame(table(US_var_2016$State))
colnames(acc_x_est_2016)<-c("Estado","Cantidad")
acc_x_est_2016$Estado<-as.character(acc_x_est_2016$Estado)


acc_coord_est_2016<-merge(df_abrev, acc_x_est_2016,by.x="Abrev",by.y="Estado")
estad_faltan<-data.frame(rbind(rbind(c("AK","Alaska","02",0,0),c("HI","Hawaii","15",0,0)),c("PR","Puerto Rico","72",0,0)))
colnames(estad_faltan)<-colnames(acc_coord_est_2016)
acc_coord_est_f_2016<-rbind(acc_coord_est_2016,estad_faltan)
acc_coord_est_2016<-acc_coord_est_f_2016[order(acc_coord_est_f_2016$id),]


#Reemplazo de los datos en el spatial point
states@data$density<-as.numeric(acc_coord_est_2016$Cantidad)
save(states,file="~/states_2016.RData")



#Token para consulta de API leaflet
token="pk.eyJ1IjoibGlsbGlhbmRhbmllbGEiLCJhIjoiY2ttOGpkZHB2MTh0cDJxdXNoazF2eXI3eCJ9.up5VWzpCwu149e5RSbJ5Tw"

#Mapa 2016
m <- leaflet(states_2016) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv(token)))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)


pal <- colorBin("YlOrRd", domain = states$density , bins = bins)
m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)



#2. 2017
US_var_2017<-subset(US_var_selec,US_var_selec$year==2017)

#Arreglo base
df_abrev<-merge(data.frame(states@data),estados,by.x="name",by.y="Estado","all")
df_abrev$Abrev<- gsub(" ", "", df_abrev$Abrev, fixed = TRUE)


acc_x_est_2017<-data.frame(table(US_var_2017$State))
colnames(acc_x_est_2017)<-c("Estado","Cantidad")
acc_x_est_2017$Estado<-as.character(acc_x_est_2017$Estado)


acc_coord_est_2017<-merge(df_abrev, acc_x_est_2017,by.x="Abrev",by.y="Estado")
estad_faltan<-data.frame(rbind(rbind(c("AK","Alaska","02",0,0),c("HI","Hawaii","15",0,0)),c("PR","Puerto Rico","72",0,0)))
colnames(estad_faltan)<-colnames(acc_coord_est_2017)
acc_coord_est_f_2017<-rbind(acc_coord_est_2017,estad_faltan)
acc_coord_est_2017<-acc_coord_est_f_2017[order(acc_coord_est_f_2017$id),]


#Reemplazo de los datos en el spatial point
states@data$density<-as.numeric(acc_coord_est_2017$Cantidad)
save(states,file="~/states_2017.RData")

#Token para consulta de API leaflet
token="pk.eyJ1IjoibGlsbGlhbmRhbmllbGEiLCJhIjoiY2ttOGpkZHB2MTh0cDJxdXNoazF2eXI3eCJ9.up5VWzpCwu149e5RSbJ5Tw"

#Mapa 2017
m <- leaflet(states_2017) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv(token)))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)


pal <- colorBin("YlOrRd", domain = states$density , bins = bins)
m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)





#3. 2018
US_var_2018<-subset(US_var_selec,US_var_selec$year==2018)

#Arreglo base
df_abrev<-merge(data.frame(states@data),estados,by.x="name",by.y="Estado","all")
df_abrev$Abrev<- gsub(" ", "", df_abrev$Abrev, fixed = TRUE)


acc_x_est_2018<-data.frame(table(US_var_2018$State))
colnames(acc_x_est_2018)<-c("Estado","Cantidad")
acc_x_est_2018$Estado<-as.character(acc_x_est_2018$Estado)


acc_coord_est_2018<-merge(df_abrev, acc_x_est_2018,by.x="Abrev",by.y="Estado")
estad_faltan<-data.frame(rbind(rbind(c("AK","Alaska","02",0,0),c("HI","Hawaii","15",0,0)),c("PR","Puerto Rico","72",0,0)))
colnames(estad_faltan)<-colnames(acc_coord_est_2018)
acc_coord_est_f_2018<-rbind(acc_coord_est_2018,estad_faltan)
acc_coord_est_2018<-acc_coord_est_f_2018[order(acc_coord_est_f_2018$id),]


#Reemplazo de los datos en el spatial point
states@data$density<-as.numeric(acc_coord_est_2018$Cantidad)
save(states,file="~/states_2018.RData")

#Token para consulta de API leaflet
token="pk.eyJ1IjoibGlsbGlhbmRhbmllbGEiLCJhIjoiY2ttOGpkZHB2MTh0cDJxdXNoazF2eXI3eCJ9.up5VWzpCwu149e5RSbJ5Tw"

#Mapa 2018
m <- leaflet(states_2018) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv(token)))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)


pal <- colorBin("YlOrRd", domain = states$density , bins = bins)
m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)


#4. 2019
US_var_2019<-subset(US_var_selec,US_var_selec$year==2019)

#Arreglo base
df_abrev<-merge(data.frame(states@data),estados,by.x="name",by.y="Estado","all")
df_abrev$Abrev<- gsub(" ", "", df_abrev$Abrev, fixed = TRUE)


acc_x_est_2019<-data.frame(table(US_var_2019$State))
colnames(acc_x_est_2019)<-c("Estado","Cantidad")
acc_x_est_2019$Estado<-as.character(acc_x_est_2019$Estado)


acc_coord_est_2019<-merge(df_abrev, acc_x_est_2019,by.x="Abrev",by.y="Estado")
estad_faltan<-data.frame(rbind(rbind(c("AK","Alaska","02",0,0),c("HI","Hawaii","15",0,0)),c("PR","Puerto Rico","72",0,0)))
colnames(estad_faltan)<-colnames(acc_coord_est_2019)
acc_coord_est_f_2019<-rbind(acc_coord_est_2019,estad_faltan)
acc_coord_est_2019<-acc_coord_est_f_2019[order(acc_coord_est_f_2019$id),]


#Reemplazo de los datos en el spatial point
states@data$density<-as.numeric(acc_coord_est_2019$Cantidad)
save(states,file="~/states_2019.RData")


#Token para consulta de API leaflet
token="pk.eyJ1IjoibGlsbGlhbmRhbmllbGEiLCJhIjoiY2ttOGpkZHB2MTh0cDJxdXNoazF2eXI3eCJ9.up5VWzpCwu149e5RSbJ5Tw"

#Mapa 2019
m <- leaflet(states_2019) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv(token)))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)


pal <- colorBin("YlOrRd", domain = states$density , bins = bins)
m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)



#5. 2020
US_var_2020<-subset(US_var_selec,US_var_selec$year==2020)

#Arreglo base
df_abrev<-merge(data.frame(states@data),estados,by.x="name",by.y="Estado","all")
df_abrev$Abrev<- gsub(" ", "", df_abrev$Abrev, fixed = TRUE)


acc_x_est_2020<-data.frame(table(US_var_2020$State))
colnames(acc_x_est_2020)<-c("Estado","Cantidad")
acc_x_est_2020$Estado<-as.character(acc_x_est_2020$Estado)


acc_coord_est_2020<-merge(df_abrev, acc_x_est_2020,by.x="Abrev",by.y="Estado")
estad_faltan<-data.frame(rbind(rbind(c("AK","Alaska","02",0,0),c("HI","Hawaii","15",0,0)),c("PR","Puerto Rico","72",0,0)))
colnames(estad_faltan)<-colnames(acc_coord_est_2020)
acc_coord_est_f_2020<-rbind(acc_coord_est_2020,estad_faltan)
acc_coord_est_2020<-acc_coord_est_f_2020[order(acc_coord_est_f_2020$id),]


#Reemplazo de los datos en el spatial point
states@data$density<-as.numeric(acc_coord_est_2020$Cantidad)
save(states,file="~/states_2020.RData")

#Token para consulta de API leaflet
token="pk.eyJ1IjoibGlsbGlhbmRhbmllbGEiLCJhIjoiY2ttOGpkZHB2MTh0cDJxdXNoazF2eXI3eCJ9.up5VWzpCwu149e5RSbJ5Tw"

#Mapa 2019
m <- leaflet(states_2019) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv(token)))

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)


pal <- colorBin("YlOrRd", domain = states$density , bins = bins)
m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)

