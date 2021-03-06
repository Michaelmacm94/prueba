library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(rAmCharts)
library(shinydashboard)
library(stringr)
library(readr)
library(lubridate)
library(DT)
data_viz <- read_csv("https://raw.githubusercontent.com/jorgehsaavedra/20201124-test-convocatoria/main/03.%20visualizacion_datos/data_viz.csv")
infracciones <- read_csv("https://raw.githubusercontent.com/jorgehsaavedra/20201124-test-convocatoria/main/03.%20visualizacion_datos/infracciones.csv")

infracciones$Codigo_M=substr(infracciones$Codigo,1,3)

cons1=infracciones %>% group_by(Responsable,Valor) %>% summarise(Conteo=length(Codigo)) %>% spread(Valor,Conteo)

#Consultas data_viz

limpieza=function(x){
  x<-toupper(x);x<-gsub("Á","A",x)
  x<-gsub("É","E",x);x<-gsub("Í","I",x)
  x<-gsub("Ó","O",x);x<-gsub("Ú","U",x)
  x<-gsub("\\.","",x);x<-gsub("\\*.","\\-.",x)
}

data_viz$FechaDeIdent=as.Date(data_viz$FechaDeIdent)
data_viz$mes=month(data_viz$FechaDeIdent,locale = Sys.getlocale("LC_TIME"),label = T)

data_viz$Ruta=limpieza(data_viz$Ruta)
data_viz$Ruta[data_viz$Ruta=="1-1 ALAMOS"]="1-1";data_viz$Ruta[data_viz$Ruta=="1-9 VILLAS DEL"]="1-9 VILLAS DEL"
data_viz$Ruta[data_viz$Ruta=="16-2 ENGATIVA C"]="16-2";data_viz$Ruta[data_viz$Ruta=="16-3 AV. ELDORA"]="16-3 AV DORADO"
data_viz$Ruta[data_viz$Ruta=="16-3 AV. ELDOR"]="16-3 AV DORADO";data_viz$Ruta[data_viz$Ruta=="16-5 VILLA AMAL"]="16-5"
data_viz$Ruta[data_viz$Ruta=="3-10 USME CENTR"]="3-10";data_viz$Ruta[data_viz$Ruta=="5-4 FLORIDA"]="5-4"
data_viz$Ruta[data_viz$Ruta=="C 19"]="C19";data_viz$Ruta[data_viz$Ruta=="C19 PORTAL SUBA"]="C19"
data_viz$Ruta[data_viz$Ruta=="CF19"]="C19";data_viz$Ruta[data_viz$Ruta=="CF29"]="C29"
data_viz$Ruta[data_viz$Ruta=="CH15"]="C15";data_viz$Ruta[data_viz$Ruta=="FJ23"]="J23"
data_viz$Ruta[data_viz$Ruta=="FISCALA"]="LA FISCALA";data_viz$Ruta[data_viz$Ruta=="LK10"]="L10"
data_viz$Ruta[data_viz$Ruta=="MD81"]="D81";data_viz$Ruta[data_viz$Ruta=="MK86"]="M86"
data_viz$Ruta[data_viz$Ruta=="ML82"]="M82";data_viz$Ruta[data_viz$Ruta=="PATIO EL DORADO"]="PATIO DORADO"
data_viz$Ruta[data_viz$Ruta=="PATIO TINTAL 1-"]="PATIO TINTAL 1";data_viz$Ruta[data_viz$Ruta=="PATIO TINTAL1-"]="PATIO TINTAL 1"
data_viz$Ruta[data_viz$Ruta=="RETORNANDO SERV"]="RETOMANDO SERVI";data_viz$Ruta[data_viz$Ruta=="RUTERO APAGADO"]="RUTEROS APAGADO"

data_viz$Ruta[data_viz$Ruta=="740 TABLA 28"]="740";data_viz$Ruta[data_viz$Ruta=="740GM"]="740"
data_viz$Ruta[data_viz$Ruta=="801"]="801A";data_viz$Ruta[data_viz$Ruta=="135"]="135B"
data_viz$Ruta[data_viz$Ruta=="403"]="403A";data_viz$Ruta[data_viz$Ruta=="135GM"]="135B"
data_viz$Ruta[data_viz$Ruta=="740GM"]="740"

