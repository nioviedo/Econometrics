#------------------------------------------------------------------------#

#                 TP4 - Econometría Avanzada 
#Duarte, A., Sosa, J.B., Sánchez, G., Oviedo, N.  
#------------------------------------------------------------------------#

#----Mise en place----
# Borro cosas que pueda tener cargadas en environment
rm(list=ls())

# Cargamos los paquetes necesarios:
library(haven)
library(mfx) 
library(margins) 
library(ggplot2) 
library(dplyr)
library(stargazer)
library(corrplot)

#Seteamos el directorio
dataDir <- "C:/Users/Ovi/Desktop/Econometría Avanzada/TP 4"
setwd(dataDir)

# Cargamos la base de datos
mydata <- read_dta("cuarto_trim_2019.dta")

#Selecciono algunas variables de interÃ©s
data <- mydata %>% select(deserta, educ_jefe, hermanos, jmujer, ch14, estado, mujer, ch11, ingresos_familiares, ingreso_per_capita)

#---- Consigna (5, anticipada) ----
#Creamos nueva variable ln_ing:
data$ingreso_per_capita_mas1 <- (data$ingreso_per_capita + 1)
data$ln_ing <- log(data$ingreso_per_capita_mas1)

#Borramos las obs con missing values para ch14 y las que dan ch14=99, y las que dan educ_jefe=0
data$ch14[data$ch14==""] <- NA
data$ch14[data$ch14==99] <- NA
data$ch11[data$ch11==0] <- NA
data$ch11[data$ch11==9] <- NA
data$educ_jefe[data$educ_jefe==0] <- NA
data=na.omit(data)

# Vamos a convertir en factores a dos variables que son categóricas
data$deserta <- factor(data$deserta)
data$educ_jefe <- factor(data$educ_jefe)
data$jmujer <- factor(data$jmujer)
data$ch14 <- factor(data$ch14)
data$estado <- factor(data$estado)
data$mujer <- factor(data$mujer)
data$ch11 <- factor(data$ch11)

head(data)
summary(data) #Para describir el subconjunto relevante de datos
xtabs(~deserta + mujer, data = data) #Desertoras
xtabs(~deserta + hermanos, data = data) #Cantidad de hermanos de los desertores
xtabs(~deserta + jmujer, data = data) #Desertores con jefa de hogar
xtabs(~deserta + educ_jefe, data = data) #Educación del jefe de hogar de desertores
xtabs(~deserta + estado, data = data) #Ocupación


#---- Consigna (2) ----
probit <- glm(deserta ~ educ_jefe + hermanos + jmujer + mujer + ch11 + ln_ing + estado, family = binomial(link = "probit"), 
                data = data)
summary(probit)
stargazer(probit,  type="text", dep.var.labels=c("deserta"), out="probit.txt")

#---- Consigna (3) ----
margin.fit <- probitmfx(deserta ~ educ_jefe + hermanos + jmujer + mujer + ch11 + ln_ing + estado,
                        data = data , atmean = TRUE, robust = TRUE)
margin.fit

#Evaluamos rápidamente efectos parciales en otros puntos de interés
margin.fit.2<-margins(probit, data=data, variables = "ln_ing", at= list(educ_jefe="3", hermanos=3, jmujer="1", mujer="1", ch11="1", ln_ing=0, estado = "3"))
margin.fit.2

margin.fit.3<-margins(probit, data=data, variables = "hermanos", at= list(educ_jefe="3", hermanos=3, jmujer="1", mujer="1",  ch11="1", ln_ing=0, estado = "3"))
margin.fit.3

#---- Consigna (4) ----
#Convertimos las variables en numéricas para correr el MCO
educ_jefe3 <- as.numeric(data$educ_jefe == 3)
educ_jefe4 <- as.numeric(data$educ_jefe == 4)
educ_jefe5 <- as.numeric(data$educ_jefe == 5)
educ_jefe6 <- as.numeric(data$educ_jefe == 6)
educ_jefe7 <- as.numeric(data$educ_jefe == 7)
educ_jefe8 <- as.numeric(data$educ_jefe == 8)
educ_jefe9 <- as.numeric(data$educ_jefe == 9)
ch112 <- as.numeric(data$ch11 == 2)
estado2 <- as.numeric(data$estado == 2)
estado3 <- as.numeric(data$estado == 3)
mujera <- as.numeric(data$mujer == 1)
jmujera <- as.numeric(data$jmujer == 1)
desertaa <- as.numeric(data$deserta == 1)

#Agregamos las variables numéricas a una nueva base de datos para correr MLP
dataMLP <- cbind(data, desertaa, mujera, jmujera, educ_jefe3, educ_jefe4, educ_jefe5, educ_jefe6, educ_jefe7, educ_jefe8, educ_jefe9, ch112, estado2, estado3)

MLP <- lm(desertaa ~ hermanos + jmujera + mujera + ln_ing + educ_jefe3 + educ_jefe4 + educ_jefe5 + educ_jefe6 + educ_jefe7 + educ_jefe8 + educ_jefe9 + ch112 + estado2 + estado3, data = dataMLP)
stargazer(MLP,  type="text", dep.var.labels=c("desertaa"), out="MLP.txt")

#---- Consigna (6) ----#

data6 <- data
data6[, "prediction"] <- predict(probit, data, type = "response") #prediccion de la prob de entrar para todos los individuos

#Gráfico 1
ggplot(data6, aes(x = ln_ing, y = prediction)) + geom_smooth() + geom_point(alpha = 0.15) + scale_y_continuous(limits = c(0,0.45)) + scale_x_continuous(limits = c(5.12,12)) +ylab("Predicción")+xlab("Ingreso per cápita (Escala logaritmica)")+theme_light()


#Gráfico 2
ggplot(data6, aes(x = ln_ing, y = prediction, color=mujer)) + geom_smooth() + geom_point(alpha = 0.15) + geom_point(alpha = 0.15) + scale_y_continuous(limits = c(0,0.1)) + scale_x_continuous(limits = c(5.12,12))+ scale_color_manual(name = "",labels = c("Hombres", "Mujeres"),values=c("skyblue4", "lightpink1"))+ylab("Predicción")+xlab("Ingreso per cápita (Escala logaritmica)")+theme_light()

#Gráfico 3
ggplot(data6, aes(x = ln_ing, y = prediction, color=jmujer)) + geom_smooth() + geom_point(alpha = 0.15) + geom_point(alpha = 0.15) + scale_y_continuous(limits = c(0,0.1)) + scale_x_continuous(limits = c(5.12,12))+ scale_color_manual(name = "",labels = c("Jefes de hogar", "Jefas de hogar"),values=c("skyblue3", "magenta"))+ylab("Predicción")+xlab("Ingreso per cápita (Escala logaritmica)")+theme_light()

#---- Consigna (7) ----#
#Estimamos el Probit
data$deserta <- factor(data$deserta)
data$educ_jefe <- factor(data$educ_jefe)
data$jmujer <- factor(data$jmujer)
data$mujer <- factor(data$mujer)

probit_7  <- glm(deserta ~ educ_jefe + hermanos + jmujer + mujer + ln_ing, family = binomial(link = "probit"), 
                 data = data)
summary(probit_7)

#Definimos 4 individuos tipo:

#1°: mujer con jefe de hogar masculino (educ_jefe lo definimos en 2 porque al definirlo en 3 la probabilidad de desertar se hace muy chica,
# hermanos están evaluada en su mediana, y el nivel de log del ingreso está elejido "a ojo" mirando el gráfico anterior, 
# considerando que es más probable darle un subsidio a alguien con un ingreso menor al promedio) :
muj_jhom <- with(data, data.frame(educ_jefe="2", hermanos=3, mujer="1", jmujer="0", ln_ing=median(ln_ing)))

#2°: mujer con jefe de hogar femenino
muj_jfem <- with(data, data.frame(educ_jefe="2", hermanos=3, mujer="1", jmujer="1", ln_ing=median(ln_ing)))

#3°: varón con jefe de hogar masculino
var_jhom <- with(data, data.frame(educ_jefe="2", hermanos=3, mujer="0", jmujer="0", ln_ing=median(ln_ing)))

#4°: varón con jefe de hogar femenino
var_jfem <- with(data, data.frame(educ_jefe="2", hermanos=3, mujer="0", jmujer="1", ln_ing=median(ln_ing)))

#Convertimos las variables a factor para poder predecir
muj_jhom$deserta <- factor(muj_jhom$deserta)
muj_jhom$educ_jefe <- factor(muj_jhom$educ_jefe)
muj_jhom$jmujer <- factor(muj_jhom$jmujer)
muj_jhom$mujer <- factor(muj_jhom$mujer)

muj_jfem$deserta <- factor(muj_jfem$deserta)
muj_jfem$educ_jefe <- factor(muj_jfem$educ_jefe)
muj_jfem$jmujer <- factor(muj_jfem$jmujer)
muj_jfem$mujer <- factor(muj_jfem$mujer)

var_jhom$deserta <- factor(var_jhom$deserta)
var_jhom$educ_jefe <- factor(var_jhom$educ_jefe)
var_jhom$jmujer <- factor(var_jhom$jmujer)
var_jhom$mujer <- factor(var_jhom$mujer)

var_jfem$deserta <- factor(var_jfem$deserta)
var_jfem$educ_jefe <- factor(var_jfem$educ_jefe)
var_jfem$jmujer <- factor(var_jfem$jmujer)
var_jfem$mujer <- factor(var_jfem$mujer)

#Vemos la predicción del modelo para estos cuatro individuos tipo
prediccion1 <- predict(probit_7, muj_jhom, type="response")
prediccion1

prediccion2 <- predict(probit_7, muj_jfem, type="response")
prediccion2

prediccion3 <- predict(probit_7, var_jhom, type="response")
prediccion3

prediccion4 <- predict(probit_7, var_jfem, type="response")
prediccion4

#Vemos la diferencia entre la probabilidad de deserción para las mujeres con jefe de hogar masculino y las mujeres con jefe de hogar femenino, y luego lo mismo para los varones:
prediccion2 - prediccion1
prediccion4 - prediccion3

#Obtengamos el efecto parcial promedio del ln del ingreso para 
# los individuos que recibirán el subsidio (mujeres con jefe de hogar femenino y varones con jefe de hogar femenino):
attach(data)
margins <- margins(probit_7, variables = "ln_ing", at= list(educ_jefe="2", hermanos=3, mujer="1", jmujer="1", ln_ing=median(ln_ing)))
summary(margins(probit_7, variables = "ln_ing", at= list(educ_jefe="2", hermanos=3, mujer="1", jmujer="1", ln_ing=median(ln_ing))))
AME <- abs(margins$dydx_ln_ing[1])

exp((prediccion2 - prediccion1)/AME)

margins2 <- margins(probit_7, variables = "ln_ing", at= list(educ_jefe="2", hermanos=3, mujer="0", jmujer="1", ln_ing=median(ln_ing)))
summary(margins(probit_7, variables = "ln_ing", at= list(educ_jefe="2", hermanos=3, mujer="0", jmujer="1", ln_ing=median(ln_ing))))
AME2 <- abs(margins2$dydx_ln_ing[1])

exp((prediccion4 - prediccion3)/AME2)
detach(data)
