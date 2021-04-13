###Cargue de la informacion
load("~/Tabla_seleccionada.RData")

##### Imputación de datos climáticos #####
#1. Temperature
variables<-c("Start_Lat","Start_Lng","Temperature(F)","State")

temp_na<-US_var_selec[is.na(US_var_selec$`Humidity(%)`),]

#Quitando la hora del starttime
temp_na$Start_Time_f<-as.Date(temp_na$Start_Time)
US_var_selec$Start_Time_f<-as.Date(US_var_selec$Start_Time)

#Quitando datos extremos
US_var_selec_t<-subset(US_var_selec,US_var_selec$`Temperature(F)`<=134 & US_var_selec$`Temperature(F)`>=-38.92 & !is.na(US_var_selec$`Temperature(F)`))


for(i in 1:nrow(temp_na)){
  #Hallando los coincidentes por estado y fecha
  subc_puntos<-subset(US_var_selec_t[,variables],
                      US_var_selec_t$Start_Time_f==temp_na$Start_Time_f[i] & US_var_selec_t$State==temp_na$State[i])
  if(nrow(subc_puntos)!=0){
    #Uniendo el valor faltante al subconjunto de puntos
    subc_puntos_u<-rbind(temp_na[i,variables],subc_puntos)
    
    #Matriz de distancia
    distancia_puntos<-as.matrix(dist(subc_puntos[,c("Start_Lat","Start_Lng")],diag=T,upper=T))
    
    #Hallando la menor distancia
    distancias_ord<-sort(distancia_puntos[,1])
    distancias_sminimas<-distancias_ord[2:4]
    
    #Hallando el valor a imputar
    temp_dist_min<-US_var_selec_t[as.numeric(names(distancias_minimas)),]
    media<-mean(temp_dist_min$`Temperature(F)`)
    
    #Imputando el valor
    temp_na$`Temperature(F)`[i]<-round(media,1)
  }else{temp_na$`Temperature(F)`[i]=NA}
  
  print(i)
}

#2. Humidity
variables<-c("Start_Lat","Start_Lng","Humidity(%)","State")

hum_na<-US_var_selec[is.na(US_var_selec$`Humidity(%)`),]

#Quitando la hora del starttime
hum_na$Start_Time_f<-as.Date(hum_na$Start_Time)
US_var_selec$Start_Time_f<-as.Date(US_var_selec$Start_Time)

#Quitando datos extremos
US_var_selec_h<-subset(US_var_selec,!is.na(US_var_selec$`Humidity(%)`))

for(i in 1:nrow(hum_na)){
  #Hallando los coincidentes por estado y fecha
  subc_puntos<-subset(US_var_selec_h[,variables],
                      US_var_selec_h$Start_Time_f==hum_na$Start_Time_f[i] & US_var_selec_h$State==hum_na$State[i])
  if(nrow(subc_puntos)!=0){
    #Uniendo el valor faltante al subconjunto de puntos
    subc_puntos_u<-rbind(hum_na[i,variables],subc_puntos)
    
    #Matriz de distancia
    distancia_puntos<-as.matrix(dist(subc_puntos[,c("Start_Lat","Start_Lng")],diag=T,upper=T))
    
    #Hallando la menor distancia
    distancias_ord<-sort(distancia_puntos[,1])
    distancias_minimas<-distancias_ord[2:4]
    
    #Hallando el valor a imputar
    hum_dist_min<-US_var_selec_h[as.numeric(names(distancias_minimas)),]
    media<-mean(hum_dist_min$`Humidity(%)`)
    
    #Imputando el valor
    hum_na$`Humidity(%)`[i]<-round(media,1)
  }else{hum_na$`Humidity(%)`[i]=NA}
  print(i)
}

save(hum_na,file="humedad_completo1.RData")

#3. Pressure
variables<-c("Start_Lat","Start_Lng","Pressure(in)","State")

pres_na<-US_var_selec[is.na(US_var_selec$`Pressure(in)`)|US_var_selec$`Pressure(in)`==0,]

#Quitando la hora del starttime
pres_na$Start_Time_f<-as.Date(pres_na$Start_Time)
US_var_selec$Start_Time_f<-as.Date(US_var_selec$Start_Time)

#Quitando datos faltantes y cero
US_var_selec_p<-subset(US_var_selec,!is.na(US_var_selec$`Pressure(in)`)&US_var_selec$`Pressure(in)`!=0)

for(i in 1:nrow(pres_na)){
  #Hallando los coincidentes por estado y fecha
  subc_puntos<-subset(US_var_selec_p[,variables],
                      US_var_selec_p$Start_Time_f==pres_na$Start_Time_f[i] & US_var_selec_p$State==pres_na$State[i])
  if(nrow(subc_puntos)!=0){
    #Uniendo el valor faltante al subconjunto de puntos
    subc_puntos_u<-rbind(pres_na[i,variables],subc_puntos)
    
    #Matriz de distancia
    distancia_puntos<-as.matrix(dist(subc_puntos[,c("Start_Lat","Start_Lng")],diag=T,upper=T))
    
    #Hallando la menor distancia
    distancias_ord<-sort(distancia_puntos[,1])
    distancias_minimas<-distancias_ord[2:4]
    
    #Hallando el valor a imputar
    pres_dist_min<-US_var_selec_p[as.numeric(names(distancias_minimas)),]
    media<-mean(pres_dist_min$`Pressure(in)`)
    
    #Imputando el valor
    pres_na$`Pressure(in)`[i]<-round(media,1)
  }else{pres_na$`Pressure(in)`[i]=NA}
  print(i)
}

save(pres_na,file="presion_completo.RData")

#4. Visibility
variables<-c("Start_Lat","Start_Lng","Visibility(mi)","State")

visib_na<-US_var_selec[is.na(US_var_selec$`Visibility(mi)`)|US_var_selec$`Visibility(mi)`==0,]

#Quitando la hora del starttime
visib_na$Start_Time_f<-as.Date(visib_na$Start_Time)
US_var_selec$Start_Time_f<-as.Date(US_var_selec$Start_Time)

#Quitando datos faltantes y cero
US_var_selec_p<-subset(US_var_selec,!is.na(US_var_selec$`Visibility(mi)`)&US_var_selec$`Visibility(mi)`!=0)

for(i in 1:nrow(visib_na)){
  #Hallando los coincidentes por estado y fecha
  subc_puntos<-subset(US_var_selec_p[,variables],
                      US_var_selec_p$Start_Time_f==visib_na$Start_Time_f[i] & US_var_selec_p$State==visib_na$State[i])
  if(nrow(subc_puntos)!=0){
    #Uniendo el valor faltante al subconjunto de puntos
    subc_puntos_u<-rbind(visib_na[i,variables],subc_puntos)
    
    #Matriz de distancia
    distancia_puntos<-as.matrix(dist(subc_puntos[,c("Start_Lat","Start_Lng")],diag=T,upper=T))
    
    #Hallando la menor distancia
    distancias_ord<-sort(distancia_puntos[,1])
    distancias_minimas<-distancias_ord[2:4]
    
    #Hallando el valor a imputar
    visib_dist_min<-US_var_selec_p[as.numeric(names(distancias_minimas)),]
    media<-mean(visib_dist_min$`Visibility(mi)`)
    
    #Imputando el valor
    visib_na$`Visibility(mi)`[i]<-round(media,1)
  }else{visib_na$`Visibility(mi)`[i]=NA}
  print(i)
}

save(visib_na,file="visibilidad_completo.RData")



#5. Wind speed
variables<-c("Start_Lat","Start_Lng","Wind_Speed(mph)","State")

wind_dir_na<-US_var_selec[is.na(US_var_selec$`Wind_Speed(mph)`)|US_var_selec$`Wind_Speed(mph)`==0,]

#Quitando la hora del starttime
wind_dir_na$Start_Time_f<-as.Date(wind_dir_na$Start_Time)
US_var_selec$Start_Time_f<-as.Date(US_var_selec$Start_Time)

#Quitando datos faltantes y cero
US_var_selec_p<-subset(US_var_selec,!is.na(US_var_selec$`Wind_Speed(mph)`)&US_var_selec$`Wind_Speed(mph)`!=0)

for(i in 1:nrow(wind_dir_na)){
  
  #Hallando los coincidentes por estado y fecha
  subc_puntos<-subset(US_var_selec_p[,variables],
                      US_var_selec_p$Start_Time_f==wind_dir_na$Start_Time_f[i] & US_var_selec_p$State==wind_dir_na$State[i])
  if(nrow(subc_puntos)!=0){
    #Uniendo el valor faltante al subconjunto de puntos
    subc_puntos_u<-rbind(wind_dir_na[i,variables],subc_puntos)
    
    #Matriz de distancia
    distancia_puntos<-as.matrix(dist(subc_puntos[,c("Start_Lat","Start_Lng")],diag=T,upper=T))
    
    #Hallando la menor distancia
    distancias_ord<-sort(distancia_puntos[,1])
    distancias_minimas<-distancias_ord[2:4]
    
    #Hallando el valor a imputar
    windd_dist_min<-US_var_selec_p[as.numeric(names(distancias_minimas)),]
    media<-mean(windd_dist_min$`Wind_Speed(mph`)
    
    #Imputando el valor
    wind_dir_na$`Wind_Speed(mph)`[i]<-round(media,1)
  }else{wind_dir_na$`Wind_Speed(mph)`[i]=NA}
  print(i)
}

save(wind_dir_na,file="wind_dir_completo.RData")


#Union de los archivos resultantes del procedimiento anterior

load("~/Tabla_seleccionada.RData")
load("~/temperatura_completa.RData")
load("~/humedad_completo.RData")
load("~/presion_completo.RData")
load("~/visibilidad_completo.RData")
load("~/wind_speed_completo.RData")

#Temperature
temp_compl<-rbind(temp_na,US_var_selec[which(!is.na(US_var_selec$`Temperature(F)`)),])
temp_compl$Id1<-as.numeric(gsub('A-','',temp_compl$ID))
temp_compl<-temp_compl[order(temp_compl$Id1),]
US_var_selec$`Temperature(F)`<-temp_compl$Temperature.F.


#Humidity
hum_compl<-rbind(hum_na,US_var_selec[which(!is.na(US_var_selec$`Humidity(%)`)),])
hum_compl$Id1<-as.numeric(gsub('A-','',hum_compl$ID))
hum_compl<-hum_compl[order(hum_compl$Id1),]
US_var_selec$`Humidity(%)`<-hum_compl$`Humidity(%)`

#Pressure
pres_compl<-rbind(pres_na,US_var_selec[which(!is.na(US_var_selec$`Pressure(in)`)&US_var_selec$`Pressure(in)`!=0),])
pres_compl$Id1<-as.numeric(gsub('A-','',pres_compl$ID))
pres_compl<-pres_compl[order(pres_compl$Id1),]
US_var_selec$`Pressure(in)`<-pres_compl$`Pressure(in)`

#Visibility
visib_compl<-rbind(visib_na,US_var_selec[which(!is.na(US_var_selec$`Visibility(mi)`)&US_var_selec$`Visibility(mi)`!=0),])
visib_compl$Id1<-as.numeric(gsub('A-','',visib_compl$ID))
visib_compl<-visib_compl[order(visib_compl$Id1),]
US_var_selec$`Visibility(mi)` <-visib_compl$`Visibility(mi)`

#wind speed
wind_speed_compl<-rbind(wind_dir_na,US_var_selec[which(!is.na(US_var_selec$`Wind_Speed(mph)`)&US_var_selec$`Wind_Speed(mph)`<=31.9),])
wind_speed_compl$Id1<-as.numeric(gsub('A-','',wind_speed_compl$ID))
wind_speed_compl<-visib_compl[order(wind_speed_compl$Id1),]
US_var_selec$`Wind_Speed(mph)` <-wind_speed_compl$`Wind_Speed(mph)`


######## Imputación de la variable sunrise sunset ###############

#Sunrise-sunset
sunr<-as.numeric(substr(US_var_selec$Start_Time, 12, 13))
sunr_suns<-ifelse(sunr>=6 & sunr<18,"Day","Night")
US_var_selec$Sunr_Suns<-sunr_suns
US_var_selec$Sunrise_Sunset <- ifelse(is.na(US_var_selec$Sunrise_Sunset), US_var_selec$Sunr_Suns,US_var_selec$Sunrise_Sunset)

######### Conformando la base resultante del preprocesamiento#########
US_var_selec<-US_var_selec[,c(1:28)]
save(US_var_selec,file="Datos_preproc.RData")













