library(readxl)
library(foreign)
library(haven)

#leer tablas xlsx
tablas <- list.files(pattern = ".xlsx")

for (k in 1:(length(tablas))){
  #cargar tablas de datos crudos
  nombre <- tablas[k]
  tabla <- read_xlsx(nombre, col_names = TRUE)
  nombre<-gsub('.{4}$', '', nombre)
  assign(paste(nombre), tabla)
}


#leer tablas sav
tablas <- list.files(pattern = ".sav")

for (k in 1:(length(tablas))){
  #cargar tablas de datos crudos
  nombre <- tablas[k]
  tabla <- read_sav(nombre)
  nombre<-gsub('.{4}$', '', nombre)
  assign(paste(nombre), tabla)
}


###Limpiando HECHOS
#2009

#hechos09[,] <- sapply(hechos09[,], as.factor)
#sel <- grepl("modelo_vehi$",names(hechos09))
#hechos09[,] <- lapply(hechos09[,], function(x) replace(x,x %in% 9999, NA))

hechos09<-`Hechos de Transito 2009`
hechos10<-`Hechos de Transito 2010`
hechos11<-`Hechos de Transito 2011`
hechos12<-`Hechos de Transito 2012`
hechos13<-`Hechos de Transito 2013`
hechos14<-`Hechos de Transito 2014`
hechos15<-`Hechos de Transito 2015.`
hechos16<-`Hechos de Transito 2016.`
hechos17<-`Hechos de Transito 2017.`


#Observaciones:
  # num_hecho (id): 2010 le hace falta
  # corre_base: 2014 tiene in numero de correlativo base - no en dicc
  # g_hora_5: 2015, 2016, 2017 tiene un grupo adicional de hora - no en dicc
  # municipios: 2009, 2010. no tienen. 2011, 2012, 2013 no en dicc. -se borran.
  # area_geog_ocu: 2017 no posee información
  # _pil: 2016-2017 no tienen informacion de piloto
  # g_edad: 2014-2015 agregan categoria 11 de 55-59
  # g_edad_80ymas: 2015 agrega grupos que llegan hasta 80.
  # edad_quinquenales: 2015 agrega variable - no en dicc
  # marca_veh: 2009, 2010 y 2012 no poseen.
  # modelo_veh: 2012 no posee

####CAMBIOS: 
#2014, 2015, 2016 -> cambiar num_corre a num_hecho
names(hechos14)[names(hechos14) == "num_correlativo"] <- "num_hecho"
names(hechos15)[names(hechos15) == "núm_corre"] <- "num_hecho"
names(hechos16)[names(hechos16) == "núm_corre"] <- "num_hecho"
names(hechos17)[names(hechos17) == "núm_corre"] <- "num_hecho"

#2014:2017 -> quitar tildes de día_ocu y día_sem_ocu
names(hechos14)[names(hechos14) == "día_ocu"] <- "dia_ocu"
names(hechos15)[names(hechos15) == "día_ocu"] <- "dia_ocu"
names(hechos16)[names(hechos16) == "día_ocu"] <- "dia_ocu"
names(hechos17)[names(hechos17) == "día_ocu"] <- "dia_ocu"
names(hechos14)[names(hechos14) == "día_sem_ocu"] <- "dia_sem_ocu"
names(hechos15)[names(hechos15) == "día_sem_ocu"] <- "dia_sem_ocu"
names(hechos16)[names(hechos16) == "día_sem_ocu"] <- "dia_sem_ocu"
names(hechos17)[names(hechos17) == "día_sem_ocu"] <- "dia_sem_ocu"

#2011 -> Borrar info de municipio. No está en diccionario ni es congruente con otros años
hechos11$muni_ocu <- NULL

#2014-2016 -> cambiar area_geo_ocu a areag_ocu
names(hechos14)[names(hechos14) == "área_geo_ocu"] <- "areag_ocu"
names(hechos15)[names(hechos15) == "área_geo_ocu"] <- "areag_ocu"
names(hechos16)[names(hechos16) == "área_geo_ocu"] <- "areag_ocu"

#2014 -> cambiar sexo_con, sexo_per y edad_con, edad_per a sexo_pil y edad_pil
names(hechos14)[names(hechos14) == "sexo_con"] <- "sexo_pil"
names(hechos15)[names(hechos15) == "sexo_per"] <- "sexo_pil"
names(hechos14)[names(hechos14) == "edad_con"] <- "edad_pil"
names(hechos15)[names(hechos15) == "edad_per"] <- "edad_pil"

#2011-2015 -> borrar variables de agrupaciones de edad para generar otra entera más adelante.
hechos11$g_edad_pil <- NULL
hechos12$g_edad <- NULL
hechos13$g_edad_2 <- NULL
hechos14$g_edad <- NULL
hechos15$g_edad_60ymás <- NULL
hechos15$g_edad_80ymás <- NULL


#2009, 2011, 2012 -> cambiar g_edad_pil, edad_m1, edad_ m1 a mayor_menor
names(hechos09)[names(hechos09) == "g_edad_pil"] <- "mayor_menor"
names(hechos11)[names(hechos11) == "edad_m1"] <- "mayor_menor"
names(hechos12)[names(hechos12) == "edad_m1"] <- "mayor_menor"

#2012, 2014, 2015 -> cambiar a estado_pil
names(hechos12)[names(hechos12) == "condicion_pil"] <- "estado_pil"
names(hechos14)[names(hechos14) == "estado_con"] <- "estado_pil"
names(hechos15)[names(hechos15) == "estado_con"] <- "estado_pil"

#2009-2012 -> cambiar a tipo_veh
names(hechos09)[names(hechos09) == "tipo_vehi"] <- "tipo_veh"
names(hechos10)[names(hechos10) == "tipo_v"] <- "tipo_veh"
names(hechos11)[names(hechos11) == "tipo_vehiculo"] <- "tipo_veh"
names(hechos12)[names(hechos12) == "tipo_vehi"] <- "tipo_veh"

#2011 -> cambiar marca_vehi a marca_veh
names(hechos11)[names(hechos11) == "marca_vehi"] <- "marca_veh"

#2009-2012 -> cambiar a color_veh
names(hechos09)[names(hechos09) == "color_vehi"] <- "color_veh"
names(hechos10)[names(hechos10) == "color_v"] <- "color_veh"
names(hechos11)[names(hechos11) == "color_vehi"] <- "color_veh"
names(hechos12)[names(hechos12) == "color_vehi"] <- "color_veh"

#2009, 2010, 2011 -> cambiar a modelo_veh
names(hechos09)[names(hechos09) == "modelo_vehi"] <- "modelo_veh"
names(hechos10)[names(hechos10) == "modelo_v"] <- "modelo_veh"
names(hechos11)[names(hechos11) == "modelo_vehi"] <- "modelo_veh"

#2010, 2014, 2015, 2016, 2017 -> cambiar a causa_acc
names(hechos10)[names(hechos10) == "causa_ac"] <- "causa_acc"
names(hechos14)[names(hechos14) == "tipo_eve"] <- "causa_acc"
names(hechos15)[names(hechos15) == "tipo_eve"] <- "causa_acc"
names(hechos16)[names(hechos16) == "tipo_eve"] <- "causa_acc"
names(hechos17)[names(hechos17) == "tipo_eve"] <- "causa_acc"

#2009, 2010, 2011, 2012, 2013, 2014 -> agregar año_ocu
hechos09$año_ocu<-rep(2009, nrow(hechos09))
hechos10$año_ocu<-rep(2010, nrow(hechos10))
hechos11$año_ocu<-rep(2011, nrow(hechos11))
hechos12$año_ocu<-rep(2012, nrow(hechos12))
hechos13$año_ocu<-rep(2013, nrow(hechos13))
hechos14$año_ocu<-rep(2014, nrow(hechos14))

#2010 -> agregar mayor_menor
mayor_menor10<-matrix(data=NA, nrow=nrow(hechos10), ncol=1)
for (i in c(1:nrow(hechos10))){
  if(hechos10$edad_pil[i]<18) {mayor_menor10[i] <- 2} 
  else {
    if(hechos10$edad_pil[i]==999) {mayor_menor10[i] <- 9} 
    else {mayor_menor10[i] <- 1}
  }
}
hechos10$mayor_menor<-as.vector(mayor_menor10)

#Unir los data frames del año 2009 al año 2012. Las columnas se unen por medio de nombres iguales y en donde no hbían variables se hacen NAs.
hechos09_10<-merge(hechos09, hechos10, all = TRUE)
hechos09_11<-merge(hechos09_10, hechos11, all = TRUE)
hechos09_12<-merge(hechos09_11, hechos12, all = TRUE)

#2009:2012 -> agregar g_hora
g_hora_912<-matrix(data=NA, nrow=nrow(hechos09_12), ncol=1)
for (j in c(1:nrow(hechos09_12))){
  if (is.na(hechos09_12$hora_ocu[j])==TRUE){hechos09_12$hora_ocu[j]<-99}
  if(hechos09_12$hora_ocu[j]<= 5) {g_hora_912[j] <- 1}
  else {
    if(hechos09_12$hora_ocu[j]<=11) {g_hora_912[j] <- 2}
    else {
      if(hechos09_12$hora_ocu[j]<=17) {g_hora_912[j] <- 3}
      else {
        if (hechos09_12$hora_ocu[j]<=23) {g_hora_912[j] <- 4}  
        else {g_hora_912[j] <- NA}
      }
    }
  }
}
hechos09_12$g_hora<-as.vector(g_hora_912)

#Se unen los data frames 13 y 14 al data frame del 09 al 12.
hechos09_13<-merge(hechos09_12, hechos13, all = TRUE)
hechos09_14<-merge(hechos09_13, hechos14, all = TRUE)


#2009:2014 -> agregar g_modelo
g_modelo_914<-matrix(data=NA, nrow=nrow(hechos09_14), ncol=1)
for (k in c(1:nrow(hechos09_14))){
  if (is.na(hechos09_14$modelo_veh[k])==TRUE){hechos09_14$modelo_veh[k]<-9999}
  if(hechos09_14$modelo_veh[k]< 1970) {g_modelo_914[k] <- 6}
  else {
    if(hechos09_14$modelo_veh[k]< 1980) {g_modelo_914[k] <- 1}
    else {
      if(hechos09_14$modelo_veh[k]< 1990) {g_modelo_914[k] <- 2}
      else {
        if (hechos09_14$modelo_veh[k]< 2000) {g_modelo_914[k] <- 3}  
        else {
          if(hechos09_14$modelo_veh[k]< 2010) {g_modelo_914[k] <- 4}
          else{
            if(hechos09_14$modelo_veh[k]< 2020) {g_modelo_914[k] <- 5}
            else {g_modelo_914[k] <- 6}
          }
        }
      }
    }
  }
}
hechos09_14$g_modelo_veh<-as.vector(g_modelo_914)

#Se une los datos del 2015 al conjunto del 2009 hasta el 2014.
hechos09_15<-merge(hechos09_14, hechos15, all = TRUE)


#2009:2015 -> agregar g_edad_pil. Estos grupos son más completos ya que se basan en la plantilla del año 2015 donde hay en intervalos de 5 años hasta 80 años.
g_edad_915<-matrix(data=NA, nrow=nrow(hechos09_15), ncol=1)
for (l in c(1:nrow(hechos09_15))){
  if (is.na(hechos09_15$edad_pil[l])==TRUE){hechos09_15$edad_pil[l]<-999}
  if(hechos09_15$edad_pil[l]==999) {g_edad_915[l] <- 16}
  else{
    if(hechos09_15$edad_pil[l]< 15) {g_edad_915[l] <- 1}
    else {
      if(hechos09_15$edad_pil[l]< 20) {g_edad_915[l] <- 2}
      else {
        if(hechos09_15$edad_pil[l]< 25) {g_edad_915[l] <- 3}
        else {
          if (hechos09_15$edad_pil[l]< 30) {g_edad_915[l] <- 4} 
          else {
            if(hechos09_15$edad_pil[l]< 35) {g_edad_915[l] <- 5}
            else{
              if(hechos09_15$edad_pil[l]< 40) {g_edad_915[l] <- 6}
              else{
                if(hechos09_15$edad_pil[l]< 45) {g_edad_915[l] <- 7}
                else{
                  if(hechos09_15$edad_pil[l]< 50) {g_edad_915[l] <- 8}
                  else{
                    if(hechos09_15$edad_pil[l]< 55) {g_edad_915[l] <- 9}
                    else{
                      if(hechos09_15$edad_pil[l]< 60) {g_edad_915[l] <- 10}
                      else{
                        if(hechos09_15$edad_pil[l]< 65) {g_edad_915[l] <- 11}
                        else{
                          if(hechos09_15$edad_pil[l]< 70) {g_edad_915[l] <- 12}
                          else{
                            if(hechos09_15$edad_pil[l]< 75) {g_edad_915[l] <- 13}
                            else{
                              if(hechos09_15$edad_pil[l]< 80) {g_edad_915[l] <- 14}
                              else{g_edad_915[l] <- 15}
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
hechos09_15$g_edad<-as.vector(g_edad_915)


#Se une los datos del 2016 y el 2017 al conjunto de todos los otros años
hechos09_16<-merge(hechos09_15, hechos16, all = TRUE)
hechos09_17<-merge(hechos09_16, hechos17, all = TRUE)



###DATAFRAME DE HECHOS FINAL
hechos<-hechos09_17