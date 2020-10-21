
## BASES DE DATOS

# asignar un archivo de excel 
base <- read_excel('D:/Documentos/Alejandro Ciencia de Datos/Seminario de Estadística/Tareas/Semana 2/carpetas-de-investigacion-pgj-de-la-ciudad-de-mexico.xlsx')
View(base)

# regresa los nombres de las columnas de un dataframe
colnames(base)

# arrange() ordena las filas de una data frame por los valores de las columnas seleccionadas

# Ordenar la base por una columna mes_hechos
ord <- base %>%
  arrange(mes_hechos)
View(ord)

ord1 <- arrange(.data = base,fecha_hechos)
View(ord1)

# ordenar la base por la culumna 