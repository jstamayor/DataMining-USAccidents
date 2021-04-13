
#Cargando el conjunto de datos con variables climaticas imputadas
load("C:/Users/Daniela/Documents/Maestria/Semestre 1/Mineria de datos/Proyecto final/Datos_preproc.RData")

#Seleccion las variables requeridas para el muestreo
subs_clim_var<-data.frame(US_var_selec$ID,US_var_selec$`Temperature(F)`,US_var_selec$`Humidity(%)`,
              US_var_selec$`Pressure(in)`,US_var_selec$`Visibility(mi)`,
              US_var_selec$`Wind_Speed(mph)`,US_var_selec$Severity,
              US_var_selec$Start_Time,US_var_selec$State)

#Creación de la variable year
subs_clim_var$year<-substr(subs_clim_var$US_var_selec.Start_Time,1,4)

#Eliminando información faltante
clim_var_lim<-na.omit(subs_clim_var)

#Verificando las proporciones de las variables year y severity respecto a la original
table(clim_var_lim$year)/nrow(clim_var_lim)
table(clim_var_lim$US_var_selec.Severity)/nrow(clim_var_lim)

##### Muestreo proporcional al estado #####

#Proporcion de la informacion por estado

prop_est<-data.frame(prop.table(table(clim_var_lim$US_var_selec.State)))
names(prop_est)<-c("Estado","Probabilidad")

#Eleccion de la muestra por estado
set.seed(123)
sample_accidents<-sample(seq(1:nrow(prop_est)), 20, replace = F, prob = prop_est$Probabilidad)
sample_states<-data.frame(prop_est[sample_accidents,])

#Seleccionando la información de los estados muestreados

US_acc_sample<-clim_var_lim[which(clim_var_lim$US_var_selec.State%in%sample_states$Estado),]

#Muestreando al interior de cada estrato (estado)
set.seed(123)
Stratified_sampling <- splitstackshape::stratified(US_acc_sample, "US_var_selec.State", .7)

#Verificando que las proporciones se mantienen respecto a la poblacion
prop.table(table(Stratified_sampling$US_var_selec.Severity))
prop.table(table(Stratified_sampling$year))

#Archivo final con muestreo y variables climaticas
write.csv(Stratified_sampling, file = "Muestra_variables_climaticas.csv",row.names = F)
