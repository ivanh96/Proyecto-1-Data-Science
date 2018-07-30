#instalación de paquetes
if (!require("readxl"))    {install.packages("readxl");   library("readxl")}
if (!require("haven"))    {install.packages("haven");   library("haven")}

#colocar la dirección de los archivos juntos xlsx y sav
#leer tablas xlsx
setwd("~/Sync/DATOS/DATA/Proyecto1/spex")
tablasx <- list.files(pattern = ".xlsx")

cols<-list()
cn<-1
for (k in 1:(length(tablasx))){
  #cargar tablas de datos crudos
  nombre <- tablasx[k]
  tabla <- read_xlsx(nombre, col_names = TRUE)
  nombre<-gsub('.{5}$', '', nombre)
  assign(paste(nombre), tabla)
  cols[[cn]]<-colnames(tabla)
  cn<-cn+1
}


#leer tablas sav
tablasv <- list.files(pattern = ".sav")

for (k in 1:(length(tablasv))){
  #cargar tablas de datos crudos
  nombre <- tablasv[k]
  tabla <- read_sav(nombre)
  nombre<-gsub('.{4}$', '', nombre)
  assign(paste(nombre), tabla)
  cols[[cn]]<-colnames(tabla)
  cn<-cn+1
}


`Fallecidos y Lesionados 2009`<-merge(`Fallecidos 2009`, `Lesionados 2009`, by="num_hecho", all = T,  suffixes = c(".fallecidos",".lesionados"))
cols[[4]]<-colnames(`Fallecidos y Lesionados 2009`)
cols[10]<-NULL

cnames <- c(tablasx,tablasv)

nms<-c("num_hecho","día","mes","año","hora","día semana","edad","sexo","departamento","municipio","zona","área geográfica",
       "tipo vehículo","marca vehículo","color vehículo","modelo vehículo",
       "tipo evento", "fallecidos o lesionados", "internado")

# Fallecidos y Lesionados 2009
FL09<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2009`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2009`)
  
  #checkear for NAs 
  
  # a1 dia
  if(is.na(x$dia_ocu.lesionados[i])){
    a1<-x$dia_ocu.fallecidos[i]
  }else{
    a1<-x$dia_ocu.lesionados[i]
  }
  # a2 mes
  if(is.na(x$mes_ocu.lesionados[i])){
    a2<-x$mes_ocu.fallecidos[i]
  }else{
    a2<-x$mes_ocu.lesionados[i]
  }
  # a3 dia semana
  if(is.na(x$dia_sem_ocu.lesionados[i])){
    a3<-x$dia_sem_ocu.fallecidos[i]
  }else{
    a3<-x$dia_sem_ocu.lesionados[i]
  }
  # a4 hora
  if(is.na(x$hora_ocu.lesionados[i])){
    a4<-x$hora_ocu.fallecidos[i]
  }else{
    a4<-x$hora_ocu.lesionados[i]
  }
  # a5 sexo
  if(is.na(x$sexo_les[i])){
    a5<-x$sexo_pil[i]
  }else{
    a5<-x$sexo_les[i]
  }
  # a6 edad
  if(is.na(x$edad_les[i])){
    a6<-as.numeric(x$edad_fall[i])
  }else{
    a6<-as.numeric(x$edad_les[i])
  }
  # a7 departamento
  if(is.na(x$depto_ocu.lesionados[i])){
    a7<-x$depto_ocu.fallecidos[i]
  }else{
    a7<-x$depto_ocu.lesionados[i]
  }
  # a13 area geografica
  if(is.na(x$areag_ocu.lesionados[i])){
    a13<-x$areag_ocu.fallecidos[i]
  }else{
    a13<-x$areag_ocu.lesionados[i]
  }
  # a8 tipo vehículo
  if(is.na(x$tipo_vehi.lesionados[i])){
    a8<-as.numericx$tipo_vehi.fallecidos[i]
  }else{
    a8<-x$tipo_vehi.lesionados[i]
  }
  # a9 modelo vehículo
  if(is.na(x$modelo_vehi.lesionados[i])){
    a9<-x$modelo_vehi.fallecidos[i]
  }else{
    a9<-x$modelo_vehi.lesionados[i]
  }
  # a10 color vehículo
  if(is.na(x$color_vehi.lesionados[i])){
    a10<-x$color_vehi.fallecidos[i]
  }else{
    a10<-x$color_vehi.lesionados[i]
  }
  # a11 tipo evento
  if(is.na(x$causa_acc.lesionados[i])){
    a11<-x$causa_acc.fallecidos[i]
  }else{
    a11<-x$causa_acc.lesionados[i]
  }
  # a12 fallecidos o lesionados
  if(is.na(x$sexo_pil[i])){
    a12<-2
  }else{
    a12<-1
  }
  
  
  y<-data.frame(x$num_hecho[i],a1,a2,"2017",a4,a3,a6,a5,a7,NA,NA,a13,
                a8,NA,a9,a10,
                a11,a12,NA)
  FL09<-rbind(FL09,y)
}
names(FL09)<-nms

# Fallecidos y Lesionados 2010
FL10<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2010`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2010`)
  
  y<-data.frame(paste(i,"10",sep = ""),x$dia_ocu[i],x$mes_ocu[i],"2010",x$hora_ocu[i],x$dia_sem_ocu[i],x$edad_fall_les[i],x$sexo_fall_les[i],x$depto_ocu[i],NA,x$zona_ocu[i],x$areag_ocu[i],
                x$tipo_vehi[i],NA,NA,NA,
                x$Causa_acc[i],x$lesio_fall[i],NA)
  FL10<-rbind(FL10,y)
}
names(FL10)<-nms

# Fallecidos y Lesionados 2011
FL11<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2011`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2011`)
  
  y<-data.frame(x$num_hecho[i],x$dia_ocu[i],x$mes_ocu[i],"2011",x$hora_ocu[i],x$dia_sem_ocu[i],x$edad_pil[i],x$sexo_pil[i],x$depto_ocu[i],x$muni_ocu[i],x$zona_ocu[i],x$areag_ocu[i],
                x$tipo_vehi[i],x$marca_vehi[i],x$marca_vehi[i],x$color_vehi[i],
                x$causa_acc[i],x$condicion_pil[i],NA)
  FL11<-rbind(FL11,y)
}
names(FL11)<-nms

# Fallecidos y Lesionados 2012
FL12<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2012`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2012`)
  
  y<-data.frame(x$num_hecho[i],x$dia_ocu[i],x$mes_ocu[i],"2012",x$hora_ocu[i],x$dia_sem_ocu[i],x$edad_fall_les[i],x$sexo_fall_les[i],x$depto_ocu[i],x$mupio_ocu[i],x$zona_ocu[i],x$areag_ocu[i],
                x$tipo_vehi[i],NA,x$color_vehi[i],NA,
                x$casusa_acc[i],x$estado_implicado[i],NA)
  FL12<-rbind(FL12,y)
}
names(FL12)<-nms

# Fallecidos y Lesionados 2013
FL13<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2013`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2013`)
  
  y<-data.frame(x$num_hecho[i],x$dia_ocu[i],x$mes_ocu[i],"2013",x$hora_ocu[i],x$dia_sem_ocu[i],x$sexo_pil[i],x$sexo_pil[i],x$depto_ocu[i],x$mupio_ocu[i],x$zona_ocu[i],x$areag_ocu[i],
                x$tipo_veh[i],x$marca_veh[i],x$color_veh[i],x$color_veh[i],
                x$causa_acc[i],x$Fallecidos_Lesionados[i],NA)
  FL13<-rbind(FL13,y)
}
names(FL13)<-nms

# Fallecidos y Lesionados 2014
FL14<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2014`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2014`)
  
  y<-data.frame(x$corre_base[i],x$día_ocu[i],x$mes_ocu[i],"2014",x$hora_ocu[i],x$día_sem_ocu[i],x$edad_víc[i],x$sexo_víc[i],x$depto_ocu[i],x$mupio_ocu[i],x$zona_ocu[i],x$área_geo_ocu[i],
                x$tipo_veh[i],x$marca_veh[i],x$color_veh[i],x$modelo_veh[i],
                x$tipo_eve[i],x$fall_les[i],NA)
  FL14<-rbind(FL14,y)
}
names(FL14)<-nms

# Fallecidos y Lesionados 2015
FL15<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2015`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2015`)
  
  y<-data.frame(x$núm_corre[i],x$día_ocu[i],x$mes_ocu[i],x$año_ocu[i],x$hora_ocu[i],x$día_sem_ocu[i],x$edad_per[i],x$sexo_per[i],x$depto_ocu[i],x$mupio_ocu[i],x$zona_ocu[i],x$área_geo_ocu[i],
                x$tipo_veh[i],x$marca_veh[i],x$color_veh[i],x$modelo_veh[i],
                x$tipo_eve[i],x$fall_les[i],x$int_o_noint[i])
  FL15<-rbind(FL15,y)
}
names(FL15)<-nms


# Fallecidos y Lesionados 2016
FL16<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2016`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2016`)
  
  y<-data.frame(x$núm_corre[i],x$día_ocu[i],x$mes_ocu[i],x$año_ocu[i],x$hora_ocu[i],x$día_sem_ocu[i],x$edad_per[i],x$sexo_per[i],x$depto_ocu[i],x$mupio_ocu[i],x$zona_ocu[i],x$área_geo_ocu[i],
                x$tipo_veh[i],x$marca_veh[i],x$color_veh[i],x$modelo_veh[i],
                x$tipo_eve[i],x$fall_les[i],x$int_o_noint[i])
  FL16<-rbind(FL16,y)
}
names(FL16)<-nms

# Fallecidos y Lesionados 2017
FL17<-data.frame()
for(i in 1:nrow(`Fallecidos y Lesionados 2017`)){
  x<-as.data.frame(`Fallecidos y Lesionados 2017`)
  
  y<-data.frame(x$núm_corre[i],x$día_ocu[i],x$mes_ocu[i],x$año_ocu[i],x$hora_ocu[i],x$día_sem_ocu[i],x$edad_per[i],x$sexo_per[i],x$depto_ocu[i],x$mupio_ocu[i],x$zona_ocu[i],NA,
                x$tipo_veh[i],x$marca_veh[i],x$color_veh[i],x$modelo_veh[i],
                x$tipo_eve[i],x$fall_les[i],x$int_o_noint[i])
  FL17<-rbind(FL17,y)
}
names(FL17)<-nms


# Unión
FL<-rbind(FL09,FL10,FL11,FL12,FL13,FL14,FL15,FL16,FL17)
