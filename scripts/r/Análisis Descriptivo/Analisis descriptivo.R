library(readr)
library(ggplot2)
#1. Lectura de los datos
US_Accidents_Dec20 <- read_csv("~/US_Accidents_Dec20.csv")

#2. Seleccion de las variables de interes
variables<- c("ID","TMC","Severity","Start_Time","End_Time","Start_Lat","Start_Lng",              
              "Distance(mi)","Description","Street","Side","City","County","State",                
              "Weather_Timestamp","Temperature(F)","Wind_Chill(F)","Humidity(%)",          
              "Pressure(in)","Visibility(mi)","Wind_Direction","Wind_Speed(mph)",      
              "Precipitation(in)","Weather_Condition","Amenity","Bump","Crossing","Give_Way",             
              "Junction","No_Exit","Railway","Roundabout","Station","Stop","Traffic_Calming",
              "Traffic_Signal","Turning_Loop","Sunrise_Sunset")
variable_final<-c(4,5,25:37)
US_var_selec<- US_Accidents_Dec20[,variables]

#3. Calculo de variables Accident_Duration, Road_features,Traffic_signals,Interest_Points
US_var_selec$End_Time <- as.POSIXct(US_var_selec$End_Time)
US_var_selec$Start_Time <- as.POSIXct(US_var_selec$Start_Time)
US_var_selec$Accident_duration <- difftime(US_var_selec$End_Time,US_var_selec$Start_Time, units="hours")
US_var_selec$Road_features <- US_var_selec$Bump+US_var_selec$Crossing+US_var_selec$Junction+US_var_selec$Roundabout+US_var_selec$Traffic_Calming+US_var_selec$Turning_Loop
US_var_selec$Traffic_signals <- US_var_selec$Give_Way+US_var_selec$No_Exit+US_var_selec$Stop+US_var_selec$Traffic_Signal
US_var_selec$Interest_points <- US_var_selec$Amenity+US_var_selec$Railway+US_var_selec$Station

#4. Eliminacion de variables usadas para calcular las anteriormente mencionadas
US_var_selec <- US_var_selec[ ,-c(variable_final)]

#Severity,Accident_duration,start_lat,start_lng,distance,
#weather_timestamp,temperature,wind_child,humidity,pressure,
#visibility,wind_speed,precipitation,interest_points,
#road_features,Traffic signals

#-------Carga base con transformacion inicial
load("~/Tabla_seleccionada.RData")

#1. Histograma Accident_Duration

#1.1 Retirando Outliers de la variable
summary(as.numeric(US_var_selec$Accident_duration))
bigote_superior=1.665+3*(1.665-0.496)
Accident_duration1<-subset(US_var_selec,US_var_selec$Accident_duration<=bigote_superior)

#1.2 Revision del % de outliers en Accident_duration
nrow(Accident_duration1)/nrow(US_var_selec) 

#1.3 Histograma y diagrama de densidad
Accident_plot<-ggplot(Accident_duration1, aes(x=as.numeric(Accident_duration))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("DuraciC3n de la afectaciC3n causada por el accidente")+
  scale_x_continuous(name="DuraciC3n del accidente")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Accident_plot.png", units="in", width=10, height=5,dpi=400)

#2. Histograma Distance

#2.1 Retirando Outliers de la variable
summary(as.numeric(US_var_selec$`Distance(mi)`))
bigote_superior=0.0890+3*(0.0890-0)
Distance1<-subset(US_var_selec,US_var_selec$`Distance(mi)`!=0)

#2.2 Revision del % de outliers en Accident_duration
nrow(Distance1)/nrow(US_var_selec) 

#2.3 Histograma y diagrama de densidad
Distance_plot<-ggplot(Distance1, aes(x=as.numeric(`Distance(mi)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("Distancia (mi) de la afectaciC3n causada por el accidente")+
  scale_x_continuous(name="Distancia (mi)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Distance_plot.png", units="in", width=10, height=5,dpi=400)

#3. Histograma Temperature

#3.1 Dejando valores plausibles de la variable
summary(as.numeric(US_var_selec$`Temperature(F)`))
Temperature1<-subset(US_var_selec,US_var_selec$`Temperature(F)`<=134 & US_var_selec$`Temperature(F)`>=-38.92)
#3.2 Revision del % de outliers en Accident_duration
nrow(Temperature1)/nrow(US_var_selec) # Si se retiran los outliers se conserva el 97% de la informaciC3n aprox.

#3.3 Histograma y diagrama de densidad
Temperature_plot<-ggplot(Temperature1, aes(x=as.numeric(`Temperature(F)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("Temperatura en el lugar del accidente")+
  scale_x_continuous(name="Temperatura (B0F)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Temperature_plot.png", units="in", width=10, height=5,dpi=400)

#4. Histograma Wind Chill

#4.1 Dejando valores plausibles de la variable
summary(as.numeric(US_var_selec$`Wind_Chill(F)`))
Wind_chill1<-subset(US_var_selec,US_var_selec$`Wind_Chill(F)`<=134 & US_var_selec$`Wind_Chill(F)`>=-38.92)
#4.2 Revision del % de outliers en Accident_duration
nrow(Wind_chill1)/nrow(US_var_selec) # Si se retiran los outliers se conserva el 56% de la informaciC3n aprox.

#4.3 Histograma y diagrama de densidad
Wind_chill_plot<-ggplot(Wind_chill1, aes(x=as.numeric(`Wind_Chill(F)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("Temperatura de la ventisca en el lugar del accidente")+
  scale_x_continuous(name="Temperatura (??F)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Wind_chill_plot.png", units="in", width=10, height=5,dpi=400)

#5. Histograma Humidity

#5.1 Retirando vacC-os de la variable
summary(as.numeric(US_var_selec$`Humidity(%)`))
Humidity1<-subset(US_var_selec,!is.na(US_var_selec$`Humidity(%)`))

#5.2 Revision del % de outliers en Accident_duration
nrow(Humidity1)/nrow(US_var_selec) 

#5.3 Histograma y diagrama de densidad
Humidity_plot<-ggplot(Humidity1, aes(x=as.numeric(`Humidity(%)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("Humedad en el lugar del accidente")+
  scale_x_continuous(name="Humedad(%)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Humidity_plot.png", units="in", width=10, height=5,dpi=400)


#6. Histograma Pressure

#6.1 Retirando vacC-os de la variable
summary(as.numeric(US_var_selec$`Pressure(in)`))
Pressure1<-subset(US_var_selec,!is.na(US_var_selec$`Pressure(in)`))

#6.2 Revision del % de outliers en Accident_duration
nrow(Pressure1)/nrow(US_var_selec) 

#6.3 Histograma y diagrama de densidad
Pressure_plot<-ggplot(Pressure1, aes(x=as.numeric(`Pressure(in)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("PresiC3n atmosfC)rica en el lugar del accidente")+
  scale_x_continuous(name="PresiC3n(in)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Pressure_plot.png", units="in", width=10, height=5,dpi=400)

#7. Histograma Visibility

#7.1 Retirando vacC-os de la variable y outliers
summary(as.numeric(US_var_selec$`Visibility(mi)`))
Visibility1<-subset(US_var_selec,!is.na(US_var_selec$`Visibility(mi)`)&US_var_selec$`Visibility(mi)`<=30)

#7.2 Revision del % de outliers en Visibility
nrow(Visibility1)/nrow(US_var_selec)

#7.3 Histograma y diagrama de densidad
Visibility_plot<-ggplot(Visibility1, aes(x=as.numeric(`Visibility(mi)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("Visibilidad en el lugar del accidente")+
  scale_x_continuous(name="Visibilidad(mi)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Visibility_plot.png", units="in", width=10, height=5,dpi=400)


#8. Histograma wind_speed

#8.1 Retirando vacC-os de la variable y outliers
summary(as.numeric(US_var_selec$`Wind_Speed(mph)`))
wind_speed1<-subset(US_var_selec,!is.na(US_var_selec$`Wind_Speed(mph)`)&US_var_selec$`Wind_Speed(mph)`<=31.2)

#8.2 Revision del % de outliers en wind_speed
nrow(wind_speed1)/nrow(US_var_selec) 

#8.3 Histograma y diagrama de densidad
wind_speed_plot<-ggplot(wind_speed1, aes(x=as.numeric(`Wind_Speed(mph)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("Velocidad del viento en el lugar del accidente")+
  scale_x_continuous(name="Velocidad del viento(mph)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/wind_speed_plot.png", units="in", width=10, height=5,dpi=400)

#9. Histograma Precipitation

#9.1 Retirando vacC-os de la variable y outliers
summary(as.numeric(US_var_selec$`Precipitation(in)`))
precipitation1<-subset(US_var_selec,!is.na(US_var_selec$`Precipitation(in)`))

#9.2 Revision del % de outliers en wind_speed
nrow(precipitation1)/nrow(US_var_selec) 

#9.3 Histograma y diagrama de densidad
precipitation_plot<-ggplot(precipitation1, aes(x=as.numeric(`Precipitation(in)`))) + 
  geom_histogram(aes(y=..density..), colour="#0D4864", fill="#0D4864")+
  geom_density(alpha=.2, fill="#0d6454",colour="#031e19")+
  ggtitle("PrecipitaciC3n(in) en el lugar del accidente")+
  scale_x_continuous(name="PrecipitaciC3n(in)")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/precipitation_plot.png", units="in", width=10, height=5,dpi=400)





#10. Diagrama de barras Road_features
Road_features1<-data.frame(table(US_var_selec$Road_features))
colnames(Road_features1)<-c("Cantidad","Frecuencia")

#10.1 Diagrama de barras
Road_features_plot<-ggplot(Road_features1, aes(x=Cantidad,y=Frecuencia)) + 
  geom_bar(stat = "identity",color="#0D4864",fill="#0D4864")+
  ggtitle("Cantidad de caracterC-sticas en la vC-a durante el accidente")+
  scale_x_discrete(name="Cantidad de caracterC-sticas")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Road_features_plot.png", units="in", width=10, height=5,dpi=400)

#11. Diagrama de barras Traffic_signals
Traffic_signals1<-data.frame(table(US_var_selec$Traffic_signals))
colnames(Traffic_signals1)<-c("Cantidad","Frecuencia")

#11.1 Diagrama de barras
Traffic_signals_plot<-ggplot(Traffic_signals1, aes(x=Cantidad,y=Frecuencia)) + 
  geom_bar(stat = "identity",color="#0D4864",fill="#0D4864")+
  ggtitle("Cantidad de seC1ales de trC!nsito en la vC-a durante el accidente")+
  scale_x_discrete(name="Cantidad de seC1ales de trC!nsito")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Traffic_signals_plot.png", units="in", width=10, height=5,dpi=400)


#12. Diagrama de barras Interest_points
Interest_points1<-data.frame(table(US_var_selec$Interest_points))
colnames(Interest_points1)<-c("Cantidad","Frecuencia")

#12.1 Diagrama de barras
Interest_points<-ggplot(Interest_points1, aes(x=Cantidad,y=Frecuencia)) + 
  geom_bar(stat = "identity",color="#0D4864",fill="#0D4864")+
  ggtitle("Cantidad de puntos de interC)s en la vC-a durante el accidente")+
  scale_x_discrete(name="Cantidad de puntos de interC)s")+
  theme(axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/Interest_points_plot.png", units="in", width=10, height=5,dpi=400)

#13. Boxplot Accident_duration
box_accident <- ggplot(US_var_selec, aes(x=0, y=as.numeric(Accident_duration))) + 
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la duraciC3n del accidente")+
  scale_y_continuous(name="DuraciC3n del accidente")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_accident.png", units="in", width=10, height=5,dpi=400)

#14. Boxplot Distance
box_distance <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Distance(mi)`))) + 
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la distancia (mi) de afectaciC3n del accidente")+
  scale_y_continuous(name="Distancia (mi)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_distance.png", units="in", width=10, height=5,dpi=400)

#15. Boxplot Temperature
box_temperature <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Temperature(F)`))) + 
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de Temperatura (B0F) en el lugar del accidente")+
  scale_y_continuous(name="Temperatura (B0F)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_temperature.png", units="in", width=10, height=5,dpi=400)

#16. Boxplot wind_chill
box_wind_chill <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Wind_Chill(F)`))) + 
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de Temperatura(B0F) de la ventisca  en el lugar del accidente")+
  scale_y_continuous(name="Temperatura (B0F)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_wind_chill.png", units="in", width=10, height=5,dpi=400)

#17. Boxplot humidity
box_humidity <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Humidity(%)`))) + 
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la humedad(%) en el lugar del accidente")+
  scale_y_continuous(name="Humedad(%)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_humidity.png", units="in", width=10, height=5,dpi=400)


#18. Boxplot pressure
box_pressure <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Pressure(in)`))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la presiC3n atmosfC)rica(in) en el lugar del accidente")+
  scale_y_continuous(name="PresiC3n atmosfC)rica(in)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_pressure.png", units="in", width=10, height=5,dpi=400)

#19. Boxplot visibility
box_visibility <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Visibility(mi)`))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la visibilidad(mi) en el lugar del accidente")+
  scale_y_continuous(name="Visibilidad(mi)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_visibility.png", units="in", width=10, height=5,dpi=400)

#20. Boxplot wind speed
box_wind_speed <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Wind_Speed(mph)`))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la velocidad del viento(mph) en el lugar del accidente")+
  scale_y_continuous(name="Velocidad del viento(mph)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_wind_speed.png", units="in", width=10, height=5,dpi=400)


#21. Boxplot precipitation(in)
box_visibility <- ggplot(US_var_selec, aes(x=0, y=as.numeric(`Precipitation(in)`))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la precipitaciC3n(in) en el lugar del accidente")+
  scale_y_continuous(name="PrecipitaciC3n(in)")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_precipitation.png", units="in", width=10, height=5,dpi=400)

#22. Boxplot Interest_points
box_Interest_points <- ggplot(US_var_selec, aes(x=0, y=as.numeric(Interest_points))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de la cantidad de puntos de interC)s en el lugar del accidente")+
  scale_y_continuous(name="Cantidad de puntos de interC)s")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


ggsave(" ~/box_interest_points.png", units="in", width=10, height=5,dpi=400)

#23. Boxplot Road_Features
box_Road_features <- ggplot(US_var_selec, aes(x=0, y=as.numeric(Road_features))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de las cantidad de caracterC-sticas de la vC-a en el lugar del accidente")+
  scale_y_continuous(name="Cantidad de caracterC-sticas de la vC-a")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/box_road_features.png", units="in", width=10, height=5,dpi=400)

#24. Boxplot Traffic_signals
box_Traffic_signals <- ggplot(US_var_selec, aes(x=0, y=as.numeric(Traffic_signals))) +
  geom_boxplot(fill="#0D4864",outlier.shape=8,outlier.colour="red")+
  ggtitle("Diagrama de caja de las cantidad de seC1ales en la vC-a en el lugar del accidente")+
  scale_y_continuous(name="Cantidad de seC1ales en la vC-a")+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(" ~/box_Traffic_signals.png", units="in", width=10, height=5,dpi=400)


load("Maestria/Semestre 1/Mineria de datos/Proyecto final/Tabla_seleccionada.RData")