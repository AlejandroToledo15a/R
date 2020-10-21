
# 3. Si hacemos una lista de los números naturales menores a 10 que son multiplos de 
# 3 o 5 obtendremos 3, 5, 6, 9, la suma de esos multiplos es 23. Encuentre la suma de todos
# los multiplos de 3 o 5 menores a 1000.

f1 <- function(n){
  aux <- 0
  for(i in 1:n-1){
    if(i%%3 == 0 | i%%5 == 0){
      aux = i + aux
    }
  }
  aux
}

f1(10)
f1(1000)

# 4. Usted trabaja en un casino donde existe el siguiente juego: un jugador lanza 
# tres dados y gana 20 pesos por cada 6 que aparece, el costo de jugar es de 10 pesos.
# Escriba una función que simule el juego y regrese la cantidad que se gana en cada 
# juego i.e. ganancia - costo. Calcule la media y varianza al simular 100, 1000 y 10000 
# juegos.

# Función que regresa la ganancia en cada juego.
f1 <- function(){
  ls <- sample(1:6,3,replace = T)
  aux = 0
  for(i in 1:length(ls)){
    if(ls[i] == 6){
      aux = 20 + aux
    }
  }
  ganancia <- aux - 10
  return(ganancia)
}
  
# Función que simula n juegos.
f2 <- function(simulaciones = n){
  replicate(n,f1())
}

# 100, 1000, 10000 simulaciones.
x1 <- f2(100)
x2 <- f2(1000)
x3 <- f2(10000)

#Data Frame con esperanzas y varianzas.
Resultados <- data.frame(
  "Simulaciones" = c("100","1,000","10,000"), 
  "Media" = c(mean(x1),mean(x2),mean(x3)), 
  "Varianza" = c(var(x1),var(x2),var(x3))
)

Resultados

# Explique sus hallazgos al dueño del casino.

# 

# Adapte su función para que tome los argumentos: numero de juegos, dinero inicial, 
# costo del juego, apuesta del juego y que devuelva una lista con:
# Un data frame que tenga como columnas: la ganancia del i-ésimo juego y la ganancia 
# acumulada hasta el juego i

f <- function(n, x, y, z, i){
  set.seed(1643)
  g<-0:n
  names(g) <- 0:n
  
  for(s in 0:n){
    if(s == 0){
      g["0"] = x
    }else{
      ls <- sample(1:6,3,replace = T)
      aux = 0
      for(j in 1:length(ls)){
        if(ls[j] == 6){
          aux = z + aux
        }
      }
      g[as.character(s)] = aux - y
    }
  }
  if(sum(g[1:i+1]) < 0){
    data.frame("La ganancia del i-ésimo juego es: " = g[as.character(i)],
               "Lo siento, para el i-ésimo juego te quedaste sin dinero")
  }else{
    data.frame("La ganancia del i-ésimo juego es: " = g[as.character(i)],
               "La ganancia acumulada al i-ésimo juego es: " = sum(g[0:i+1]))
    
  }
  
}
  
f(n = 10,x = 1000,y = 20,z = 20,i = 4)


g<-0:10
names(g) <- 0:10
t(g)

plot(c(0,t(g)), c(0,cumsum(t(g))), type = "b", col = "blue", pch = 20)


# Un grafico de la ganancia acumulada

# Explore y explique las siguientes combinaciones para dinero inicial, costo del juego, 
# apuesta del juego: (10,10,10), (10,2,5), (100, 10, 20), (1000, 5, 10).


# 

numero = '312296a177'
substring(numero,1,2)
ls <- unlist(strsplit(numero,""))
ls[][
  
]

