library(RColorBrewer)
library(plotrix)
library(tidyverse)
# Carga de Datos
BD = read.csv ( 'D:/Documentos/Alejandro Ciencia de Datos/Seminario de Estadística/Scrips Clases/Exporta.csv - Exporta.csv.csv', header =T , sep = "," )
view(BD)

#Filtro por algunas variables
anio=2019
producto="ArÃ¡ndano"
BDx <- BD %>% filter(AÃ.o == anio, Producto == producto)

view(BDx)

#Construcción de tabla
A <- BDx%>%group_by(Estado.domicilio) %>% 
  summarise(N=n(),Volumen=sum(TOTAL.Cantidad.1))%>% 
  arrange (desc(Volumen)) %>% 
  mutate (Porcentaje=round(Volumen/sum(Volumen)*100,2))%>%
  rename(Estado=Estado.domicilio)
write.csv(A,paste(producto,anio,"Estado.csv"))

#Construcción de gráfica
png(file = paste(producto,anio,"Estado.png"))
etiquetas=c(as.character(A$Estado[1]),as.character(A$Estado[2]),as.character(A$Estado[3]),as.character(A$Estado[4]))
pie3D(A$Porcentaje, labels = A$Porcentaje, main = "Total de exportación por estado",col=rainbow(5))
legend("topright", etiquetas, cex = 0.8,fill=rainbow(5))
dev.off()

f1 <- function(anio, producto){
  BDx <- BD %>% filter(AÃ.o == anio, Producto == producto)
  
  #Construcción de tabla
  A <- BDx%>%group_by(Estado.domicilio) %>% 
    summarise(N=n(),Volumen=sum(TOTAL.Cantidad.1))%>% 
    arrange (desc(Volumen)) %>% 
    mutate (Porcentaje=round(Volumen/sum(Volumen)*100,2))%>%
    rename(Estado=Estado.domicilio)
  write.csv(A,paste(producto,anio,"Estado.csv"))
  
  #Construcción de gráfica
  png(file = paste(producto,anio,"Estado.png"))
  etiquetas=c(as.character(A$Estado[1]),as.character(A$Estado[2]),as.character(A$Estado[3]),as.character(A$Estado[4]))
  pie(A$Porcentaje, labels = A$Porcentaje, main = "Total de exportación por estado",col=rainbow(5))
  legend("topright", etiquetas, cex = 0.8,fill=rainbow(5))
  dev.off()
  
}

f1(anio = 2020, producto = 'Aguacate')


