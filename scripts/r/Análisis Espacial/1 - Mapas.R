cat("\014")
dev.off()
rm(list=ls(all=T))

setwd("C:/Users/ghama/Desktop/Minería de Datos/3 - Entregas")

library(stringr)
library(ggplot2)
library(dplyr)
library(sp)
library(sf)
library(rgdal)
library(rgeos)
library(ggspatial)
library(cowplot)

#=============================================#
#        0. Entrada de los datos           ####
#=============================================#

## Datos de accidentes
datos = readRDS("0 - Insumos/US_Accidents_depurado_v1.rds")

# Agrupar info a nivel de estados
agru1 = datos %>% 
 group_by(State) %>% 
 summarise(`Temperature(F)` = mean(`Temperature(F)`, na.rm = T)
           , Severity = mean(Severity, na.rm = T)
           , `Accident_duration` = mean(`Accident_duration`, na.rm = T)
           , `log(Accident_duration)` = mean(log(`Accident_duration`), na.rm = T)
           , `Interest_Points` = mean(`Interest_Points`, na.rm = T)
           , `Road_Features` = mean(`Road_Features`, na.rm = T)
           , `Traffic_Signals` = mean(`Traffic_Signals`, na.rm = T)
           , `Wind_Speed(mph)` = mean(`Wind_Speed(mph)`, na.rm = T)
           , `Precipitation(in)` = mean(`Precipitation(in)`, na.rm = T)
           , `log(Precipitation(in))` = mean(log(`Precipitation(in)`), na.rm = T)
           , `Wind_Chill(F)` = mean(`Wind_Chill(F)`, na.rm = T)
           , `Humidity(%)` = mean(`Humidity(%)`, na.rm = T)
           , `Pressure(in)` = mean(`Pressure(in)`, na.rm = T)
           , `Visibility(mi)` = mean(`Visibility(mi)`, na.rm = T)
           , `Distance(mi)` = mean(`Distance(mi)`, na.rm = T)
           , `log(Distance(mi))` = mean(log(`Distance(mi)`), na.rm = T)
           ) %>% 
 as.data.frame()

# Agrupar a nivel de condado 
datos$County2 = toupper(datos$County)
datos$County2[str_detect(datos$County2, " ANA")] = "DOÑA ANA"

datos$County2 = ifelse(str_detect(datos$County2, "CITY") & !datos$County2 %in% unique(shp.condados$NAME2)
                       , str_remove(datos$County2, " CITY")
                       , datos$County2)

datos$County2 = ifelse(str_detect(datos$County2, "SAINT") & !datos$County2 %in% unique(shp.condados$NAME2)
                       , str_replace(datos$County2, "SAINT", "ST.")
                       , datos$County2)

datos$County2 = ifelse(str_detect(datos$County2, "COUNTY") & !datos$County2 %in% unique(shp.condados$NAME2)
                       , str_remove(datos$County2, " COUNTY")
                       , datos$County2)

datos$County2[datos$County2 == "DE KALB"] = "DEKALB"
datos$County2[datos$County2 == "LA PORTE"] = "LAPORTE"
datos$County2[datos$County2 == "MACON-BIBB"] = "MACON"
datos$County2[datos$County2 == "OBRIEN"] = "O'BRIEN"
datos$County2[datos$County2 == "PRINCE GEORGES"] = "PRINCE GEORGE'S"
datos$County2[datos$County2 == "QUEEN ANNES"] = "QUEEN ANNE'S"
datos$County2[datos$County2 == "ST JOHN THE BAPTIST"] = "ST. JOHN THE BAPTIST"
datos$County2[datos$County2 == "ST JOSEPH"] = "ST. JOSEPH"
datos$County2[datos$County2 == "ST. MARYS"] = "ST. MARY'S"
datos$County2[datos$County2 == "STE GENEVIEVE"] = "STE. GENEVIEVE"

# Agrupar 
agru2 = datos %>% 
        group_by(County2) %>% 
        summarise(`Temperature(F)` = mean(`Temperature(F)`, na.rm = T)
                  , Severity = mean(Severity, na.rm = T)
                  , `Accident_duration` = mean(`Accident_duration`, na.rm = T)
                  , `log(Accident_duration)` = mean(log(`Accident_duration`), na.rm = T)
                  , `Interest_Points` = mean(`Interest_Points`, na.rm = T)
                  , `Road_Features` = mean(`Road_Features`, na.rm = T)
                  , `Traffic_Signals` = mean(`Traffic_Signals`, na.rm = T)
                  , `Wind_Speed(mph)` = mean(`Wind_Speed(mph)`, na.rm = T)
                  , `Precipitation(in)` = mean(`Precipitation(in)`, na.rm = T)
                  , `log(Precipitation(in))` = mean(log(`Precipitation(in)`), na.rm = T)
                  , `Wind_Chill(F)` = mean(`Wind_Chill(F)`, na.rm = T)
                  , `Humidity(%)` = mean(`Humidity(%)`, na.rm = T)
                  , `Pressure(in)` = mean(`Pressure(in)`, na.rm = T)
                  , `Visibility(mi)` = mean(`Visibility(mi)`, na.rm = T)
                  , `Distance(mi)` = mean(`Distance(mi)`, na.rm = T)
                  , `log(Distance(mi))` = mean(log(`Distance(mi)`), na.rm = T)
        ) %>% 
        as.data.frame() %>% 
        plyr::rename(c(County2 = "NAME2"))
 
## Shape estados
shp.states = readOGR("0 - Insumos/Shapes/Estados")
shp.states = st_as_sf(shp.states)

shp.gru = merge(y = agru1 %>% plyr::rename(c(State = "STUSPS")), x = shp.states, by = "STUSPS")
class(shp.gru)

## Shape condados
shp.condados = st_as_sf(readOGR("0 - Insumos/Shapes/Condados"))
shp.condados = shp.condados %>% filter(STATEFP %in% unique(shp.gru$STATEFP))
shp.condados$NAME2 = toupper(shp.condados$NAME)

shp.condados$NAME2[str_detect(shp.condados$NAME2," ANA")] = "DOÑA ANA"

# Unir con información
shp.agru2 = merge(shp.condados, agru2, by = "NAME2")
class(shp.agru2)

#=================================================================#
#                           1. Mapas                        #######
#=================================================================#

## Mapa a nivel de estado
ggplot(shp.gru) +
 geom_sf(aes(fill = `Temperature(F)`, colour = I("gray31")), lwd = 0.1) + 
 scale_fill_gradient(low = "khaki", high = "red") +
 # geom_sf(data = shp.condados, colour = I("gray71"), alpha = 0, lwd = 0.1) +
 annotation_scale(location = "bl", line_width = 0.8) +
 annotation_north_arrow(location = "bl", which_north = "true", 
                        pad_x = unit(0.35, "in"), pad_y = unit(0.2, "in"),
                        style = north_arrow_fancy_orienteering) +
 theme_light()

## Condados
var_continuas = colnames(agru2)[colnames(agru2) != "NAME2"]

for(i in unique(var_continuas)){
        shp.agru2 %>% 
                ggplot() +
                geom_sf(aes(fill = get(i), colour = I("gray60")), lwd = 0.01) +
                geom_sf(data = shp.gru, aes(colour = I("gray31")), lwd = 0.1, alpha = 0, inherit.aes = F ) + 
                scale_fill_gradient(low = "khaki", high = "red" ) +
                guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15)) +
                annotation_scale(location = "bl", line_width = 0.8) +
                annotation_north_arrow(location = "bl", which_north = "true" 
                                       #, pad_x = unit(1, "in")
                                       , pad_y = unit(0.2, "in"),
                                       style = north_arrow_fancy_orienteering) +
                labs(fill = "") + 
                theme_light() 
        if(i == "Humidity(%)"){
                ggsave(paste0("2 - Análisis descriptivo/Mapas/mapa_Humidity.pdf"), height = 8, width = 12, units = "in")
        } else{
                ggsave(paste0("2 - Análisis descriptivo/Mapas/mapa_", i, ".pdf"), height = 8, width = 12, units = "in")
        }
        
        
        print(paste0("Realizado: ", i))
}
# Claro: gray71
# Oscuro: gray31