#-------------ECONOMETRÍA UDESA 2020-----------#
#-------------Trabajo práctico 3---------------#
#Por Juan Bautista Sosa, Agustín Duarte, Guido Sánchez y Nicolás Oviedo

#-----Mise en place-----
#Limpiamos el entorno, fijamos el directorio de trabajo e invocamos las librerías que necesitamos
rm(list=ls())
gc()
dataDir <- "C:/Users/Ovi/Desktop/Econometría Avanzada/TP 3"
setwd(dataDir)
library(stargazer)
library(plm)
library(haven)
library(dplyr)
library(plyr)
library(AER)

data <- read_dta("QOB.dta")

#¿Hay missing values?
which(is.na.data.frame(data))
#No hay missing values

#------ Ejercicio 3 ----
#------- Consigna (a) ----
#Obtenemos estadísticos descriptivos para las variables de educación y salario, dividiéndolas por década de nacimiento y por trimestre
ddply(subset(data, yob>29 & yob<40),~qob, summarise,mean=mean(educ)) 
ddply(subset(data, yob>39 & yob<50),~qob, summarise,mean=mean(educ))

ddply(subset(data, yob>29 & yob<40),~qob, summarise,mean=mean(lwage))
ddply(subset(data, yob>39 & yob<50),~qob, summarise,mean=mean(lwage))

#---- Consigna (b) ----
#Trabajamos exclusivamente con los individuos nacidos en la década del 30
data3039 <- subset(data, yob>29 & yob<40)

#Corremos la regresión por MCO
MCO <- lm(lwage~ educ + ageq + ageqsq + race + married + smsa, data3039)
stargazer(MCO,  type="text", dep.var.labels=c("lwage"), out="MCO.txt")

#---- Consigna (d) ----
z1 <- as.numeric(data3039$qob == 1)
z2 <- as.numeric(data3039$qob == 2)
z3 <- as.numeric(data3039$qob == 3)
data3039 <- cbind(data3039, z1, z2, z3)

#---- Consigna (e) ----
relevancia <- lm(educ~ z1 + z2 + z3, data3039)
summary(relevancia)

#---- Consigna (f) ----
etapa1 <- lm(educ~ z1 + z2 + z3 + ageq + ageqsq + race + married + smsa, data3039)
stargazer(etapa1,  type="text", dep.var.labels=c("educ"), out="first.stage.tex")

#---- Consigna (g) ----
MC2E <- ivreg(lwage~ educ + ageq + ageqsq + race + married + smsa | z1 + z2 + z3 + ageq + ageqsq + race + married + smsa, data = data3039)
summary(MC2E, df = Inf , diagnostics = TRUE)

#---- Consigna (h) ----
summary(etapa1)

hypothesis.matrix <- c("z1=0", "z2=0", "z3=0")
linearHypothesis(etapa1, hypothesis.matrix)

#---- Consigna (i) ----
stargazer(MCO, MC2E,  type="latex", dep.var.labels=c("lwage"), out="summary.tex")
