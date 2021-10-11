#-------------ECONOMETRÍA UDESA 2020-----------#
#-------------Trabajo práctico 2---------------#
#Por Juan Bautista Sosa, Guido Sánchez y Nicolás Oviedo

#-----Mise en place-----
#Limpiamos el entorno, fijamos el directorio de trabajo e invocamos las librerías que necesitamos
rm(list=ls())
gc()
dataDir <- "C:/Users/juanb/OneDrive/Documentos/Juan/UdeSA/Econometría Avanzada/TPs/TP2"
setwd(dataDir)
library(stargazer)
library(plm)
library(haven)
library(Formula)
#Usamos esta library, que es un update de plm, para hacer el test sobre los efectos fijos:
library(nlme)

# Bajamos la base de datos y la convertimos en un data frame
crimen <- read.csv("datoscrimen.csv")
crimen <- pdata.frame(crimen, index = c("county","year"))
attach(crimen)


#Especificamos el modelo base a estimar
modelo <- Formula(lcrmrte ~ lprbarr + lprbconv +
                   lprbpris + lavgsen + lpolpc + ldensity + lpctymle + lwcon + 
                   lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + west + central + urban + lpctmin + d82 + d83 + d84 + d85 + d86 + d87)

#-----Ejercicio 1: Estimación Between----
estimacion.be <- plm(modelo, data = crimen, model = "between")
stargazer(estimacion.be,  type="text",
          dep.var.labels=c("lcrmrte")
          , out="between.txt")

#-----Ejercicio 2: Estimación por Efectos fijos/within----

estimacion.fe <- plm(modelo, data = crimen, model = "within")
stargazer(estimacion.fe,  type="text",
          dep.var.labels=c("lcrmrte")
          , out="within.txt")
summary(estimacion.fe)

#Vemos lo parámetros de las 197 variables binarias por condado
fixef(estimacion.fe)

#Testee formalmente la hipótesis nula de ausencia de efectos fijos. Corremos un modelo MCO para comparar con el modelo de efectos fijos en un test de significatividad conjunta.
MCO <- lm(lcrmrte ~ lprbarr + lprbconv +
            lprbpris + lavgsen + lpolpc + ldensity + lpctymle + lwcon + 
            wtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + west + central + urban + lpctmin + d82 + d83 + d84 + d85 + d86 + d87, data=crimen)
pFtest(estimacion.fe, MCO)



#-----Ejercicio 3: Estimación por Efectos Aleatorios----

estimacion.re <- plm(modelo, data = crimen, model = "random")
stargazer(estimacion.be, estimacion.fe, estimacion.re,  type="latex",
          dep.var.labels=c("lcrmrte")
          , out="summary.txt")

#Test de Hausman
phtest(estimacion.fe,estimacion.re)


#-----Ejercicio 5: tests para la presencia de posibles efectos aleatorios y correlación serial de primer orden-----

#Vemos que NO hay missing values en la parte de la base de datos que usamos para las estimaciones:
ncol(crimen)
is.na.data.frame(crimen[,1:51])
which(is.na.data.frame(crimen[,1:51]))

#Con este resultado, NO es necesario utilizar los tests de Bera, Sosa Escudero & Yoon, 
# que son una versión modificada de tests tradicionales de corelación serial para paneles no balanceados

#Corremos los siguientes 3 tests:

# Test de efectos aleatorios Bresuch Pagan (1980), asumiendo que NO hay correlacion serial en el error idiosincrático.
# H0: ausencia de efectos individuales
# H1: hay efectos individuales
plmtest(estimacion.re)
# p-valor=2.2e-16. RechH0

# Test de efectos inobservables. H0: sigma_m^2 = 0  Wooldridge (2002)
# Es más general que el de Bresuch Pagan porque NO requiere asumir una distribución particular para el error compuesto. Asume no corr serial del error idiosincrático.
# H0: ausencia de efectos individuales
# H1: hay efectos individuales
pwtest(modelo, data = crimen)
# p-valor=5.054e-06. RechH0

# Rechazar H0 en ambos casos NO implica que la estructura de efectos aleatorios en el error es correcta


# Test condicional LM para errores AR(1) o MA(1) bajo Efectos aleatorios, Baltagi & Li (1995)
# Testea correlación serial del error idiosincrático en un contexto de efectos aleatorios
pbltest(lcrmrte ~ lprbarr + lprbconv +
          lprbpris + lavgsen + lpolpc + ldensity + lpctymle + lwcon + 
          wtuc + lwtrd + lwfir + lwser + lwmfg + lwfed + lwsta + lwloc + west + central + urban + lpctmin + d82 + d83 + d84 + d85 + d86 + d87, data = crimen, alternative = "onesided")

# Test de correlación serial Breusch Godfrey para los errores idiosincráticos, asumiendo que NO hay efectos aleatorios.  Permite elegir el orden de correlación
pbgtest(estimacion.re, order = 1)
pbgtest(estimacion.re, order = 2)
pbgtest(estimacion.re, order = 3)
pbgtest(estimacion.re, order = 4)
#P-valores:
#2.2e-16
#1.081e-15
#1.14e-15
#3.036e-15

#Añadimos un test de correlación serial bajo efectos fijos Bhargava A, Franzini L, Narendranathan W (1982).
pbnftest(estimacion.fe)
#H0: no correlación serial del error
#H1: correlación serial
