library(dplyr)
library(tidyr)
library(stringr)
data_fnl <- read_csv("https://raw.githubusercontent.com/jorgehsaavedra/20201124-test-convocatoria/main/01.%20transformacion_datos/data_fnl.csv")
data_org <- read_csv2("https://raw.githubusercontent.com/jorgehsaavedra/20201124-test-convocatoria/main/01.%20transformacion_datos/data_org.csv") %>% 
  select(-"Tipo Vehiculo")

#1-Tranformar los nombres de las columnas.
names(data_org)[2]=iconv(names(data_org)[2],"ISO-8859-1","UTF-8")
data_org$Parada=iconv(data_org$Parada,"ISO-8859-1","UTF-8")

#2-Renombrar los nombres de las columnas.
names(data_org)=c("FechaContable","FechaTransaccion","HoraTransaccion","Empresa","RutaSae","Linea",
                  "StrParadero","Bus","TipoValidacion","StrLinea")

#3-Crear o eliminar columnas de acuerdo con el dataset final.

data_org=data_org %>% mutate(Paradero=NA,StrRutaSae=NA,Cenefa=NA)

#4-Convertir las columnas de fecha en formato "%Y-%m-%d".

data_org$FechaContable=as.Date(data_org$FechaContable,format="%Y-%m-%d")
data_org$FechaTransaccion=as.Date(data_org$FechaTransaccion,format="%Y/%m/%d")

#5-De la columna 'StrParadero' obtener solo los digitos.

data_org$Paradero=gsub(")","",str_extract(data_org$StrParadero,"[^(]+[$)]"))
data_org$StrParadero=gsub("\\).","",str_extract(data_org$StrParadero,"\\).*"))

#6-De la columna 'StrLinea' obtener solo el texto.
data_org$StrLinea=gsub("\\).","",str_extract(data_org$StrLinea,"\\).*"))

names(data_org)
names(data_fnl)

#7-Proceso de guardado de resultado.
data_org=data_org %>% select("FechaContable","FechaTransaccion","HoraTransaccion","Empresa","Paradero",
                             "StrParadero","Linea","StrLinea","Bus","TipoValidacion",
                             "RutaSae","StrRutaSae","Cenefa")

save(data_org,file = "data_fnl_michael.csv")


