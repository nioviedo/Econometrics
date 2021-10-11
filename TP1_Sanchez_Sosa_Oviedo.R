#-------------ECONOMETRÍA UDESA 2020-----------#
#-------------Trabajo práctico 1---------------#
#Por Juan Bautista Sosa, Guido Sánchez y Nicolás Oviedo

#-----Mise en place-----
#Limpiamos el entorno, fijamos el directorio de trabajo e invocamos las librerías que necesitamos
rm(list=ls())
gc()

setwd("C:/Users/juanb/OneDrive/Documentos/Juan/UdeSA/Econometría Avanzada/TPs/TP1")

library("xlsx")
library("readxl")
library("corrplot")
library("ggplot2")

#----Ejercicio 1.1----
#Creamos las primeras 50 bases de datos dfi, siendo i un número entre 1 y 50
#Cada base de datos se compone de vectores x1, x2, x3, u, y generados aleatoriamente

for (i in 1:50){
  set.seed(i)
  x1 <- runif(n=100, min=0,max=100)
  x2 <- runif(n=100, min=0,max=100)
  x3 <- runif(n=100, min=0,max=100)
  u <- rnorm(n=100, mean=0,sd=40)
  y <- 300+x1+x2-x3+u

    assign(paste0("df",i), data.frame(x1,x2,x3,u,y))
}

#----Ejercicio 1.2----

#Generamos la matriz de correlaciones de las x de la primera base de datos

rho <- data.frame(df1$x1, df1$x2, df1$x3)
rho <- round(cor(rho), 3)
corrplot(rho, method="number", bg="purple")

#----Ejercicio 1.3-----
#Creamos una matriz para guardar coeficientes de correlación
#Alojaremos el vector de coeficientes que surge de regresar y en x en cada base de datos
Matrix_betas <- matrix(,0,4)
colnames(Matrix_betas) <- c("b0","b1","b2","b3")

#Regresamos y en x en cada una de las bases de datos
for(j in 1:50){
  df = get(paste('df',j,sep=''))
  Base = coef(lm(y~x1+x2+x3, df))
  Matrix_betas <- rbind(Matrix_betas,Base) #Guardamos los coeficientes de la regresión j en la matriz antes definida
}

rownames(Matrix_betas) <- t(c(1:50)) #Renombramos las filas por prolijidad

#Guardamos nuestra matriz en Excel
write.xlsx(Matrix_betas, file="primera_estimacion.xlsx", sheetName = "1ra Estimación", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#----Ejercicio 1.4----
#Importamos el Excel que acabamos de generar
Matrix_betas_1 <- read_excel("primera_estimacion.xlsx")
Matrix_betas_1[1] <- NULL

#Graficamos beta 1 contra b2 y contra b3
ggplot(Matrix_betas_1, aes(x=b1, y=b2))+geom_point(size=2, shape=16)+ggtitle("Gráfico 1 (Primera estimación)")+scale_x_continuous(limits = c(0,2.5))+scale_y_continuous(limits = c(0,2.5))
ggplot(Matrix_betas_1, aes(x=b1, y=b3))+geom_point(size=2, shape=16)+ggtitle("Gráfico 2. (Primera estimación)")+scale_x_continuous(limits = c(0,2.5))+scale_x_continuous(limits = c(0,2.5))+scale_y_continuous(limits = c(-2.5,0))

#--------Ejercicio 1.5-------------
#Creamos nuevas bases de datos con x1 y x2 altamente correlacionados

for (i in 1:50){
  set.seed(i)
  x1 <- runif(n=100, min=0,max=100)
  
  #Bloque de código heredado
    x2 <- scale(matrix( rnorm(100), ncol=1 ))
    xs <- cbind(scale(x1),x2) #Normalizamos los valores del vector x1 y generamos matriz xs uniéndolos a x2
    c1 <- var(xs) #Matriz de varianzas y covarianzas
    chol1 <- solve(chol(c1)) #Triangularizamos la matriz
    newx <- xs
    newc <- matrix(c(1 , 0.987,0.987, 1 ), ncol=2)
    eigen(newc)
    chol2 <- chol(newc)
    xm2 <- newx%*% chol2 * sd(x1) + mean(x1)
  
  x2 <- xm2[, 2]
  x3 <- runif(n=100, min=0,max=100)
  u <- rnorm(n=100, mean=0,sd=40)
  y <- 300+x1+x2-x3+u
  
  assign(paste0("db",i), data.frame(x1,x2,x3,u,y))
}

#---------Ejercicio 1.6-----------
#Preparamos la matriz para alojar coeficienes de correlación
Matriz_ej1_6 <- matrix(,0,4)
colnames(Matriz_ej1_6) <- c("b0","b1","b2","b3")

#Regresamos y en x en cada una de las bases de datos
for(j in 1:50){
  db = get(paste('db',j,sep=''))
  Base = coef(lm(y~x1+x2+x3, db))
  Matriz_ej1_6 <- rbind(Matriz_ej1_6,Base) #Guardamos los coeficientes de la regresión j en la matriz antes definida
}

rownames(Matriz_ej1_6) <- t(c(1:50)) #Renombramos las filas por prolijidad

#Guardamos nuestra matriz en Excel
write.xlsx(Matriz_ej1_6, file="segunda_estimacion.xlsx", sheetName = "Matriz_ej1_6", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#---------Ejercicio 1.7-----------
#Importamos el Excel que acabamos de generar
Matriz_ej1_6_1 <- read_excel("segunda_estimacion.xlsx")
Matriz_ej1_6_1[1] <- NULL

#Graficamos beta 1 contra b2 para las x altamente correlacionadas
ggplot(Matriz_ej1_6_1, aes(x=b1, y=b2))+geom_point(size=2, shape=16)+ggtitle("Gráfico 3. (Segunda estimación)")+scale_x_continuous(limits = c(-1,3))+scale_y_continuous(limits = c(-1,3))

#---------Ejercicio 2.1-----------
#Preparamos la matriz para alojar coeficienes de correlación
Matrix_betas_ej2 <- matrix(,0,3)
colnames(Matrix_betas_ej2) <- c("b0","b1","b3")

#Regresamos y en x1 y x3 en cada una de las bases de datos
for(j in 1:50){
  df = get(paste('df',j,sep=''))
  Base = coef(lm(y~x1+x3, df))
  Matrix_betas_ej2 <- rbind(Matrix_betas_ej2,Base) #Guardamos los coeficientes de la regresión j en la matriz antes definida
}

rownames(Matrix_betas_ej2) <- t(c(1:50)) #Renombramos las filas por prolijidad

#Guardamos nuestra matriz en Excel
write.xlsx(Matrix_betas_ej2, file="tercera_estimacion.xlsx", sheetName = "Matrix_betas_ej2", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#Importamos el Excel que acabamos de generar
Matriz_ej2 <- read_excel("tercera_estimacion.xlsx")
Matriz_ej2[1] <- NULL

#Graficamos beta 1 contra b3
ggplot(Matriz_ej2, aes(x=b1, y=b3))+geom_point(size=2, shape=16)+ggtitle("Gráfico 4. (Tercera estimación)")+scale_x_continuous(limits = c(0,2.5))+scale_y_continuous(limits = c(-2.5,0))


#---------Ejercicio 2.2-----------
#Replicamos el punto anterior pero con las bases de datos con x1 y x2 altamente correlacionados
Matrix_betas_ej22 <- matrix(,0,3)
colnames(Matrix_betas_ej22) <- c("b0","b1","b3")

#Regresamos y en x1 y x3 en cada una de las bases de datos
for(j in 1:50){
  db = get(paste('db',j,sep=''))
  Base = coef(lm(y~x1+x3, db))
  Matrix_betas_ej22 <- rbind(Matrix_betas_ej22,Base) #Guardamos los coeficientes de la regresión j en la matriz antes definida
}

rownames(Matrix_betas_ej22) <- t(c(1:50)) #Renombramos las filas por prolijidad

#Guardamos nuestra matriz en Excel
write.xlsx(Matrix_betas_ej22, file="cuarta_estimacion.xlsx", sheetName = "Matrix_betas_ej22", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#Importamos el Excel que acabamos de generar
Matriz_ej22 <- read_excel("cuarta_estimacion.xlsx")
Matriz_ej22[1] <- NULL

#Graficamos beta 1 contra b3
ggplot(Matriz_ej22, aes(x=b1, y=b3))+geom_point(size=2, shape=16)+ggtitle("Gráfico 5. (Cuarta estimación)")+scale_x_continuous(limits = c(0,2.5))+scale_y_continuous(limits = c(-2.5,0))


#----Fin----
search() #Chequeamos integridad del searchpath

#Última actualización 05/04/2020