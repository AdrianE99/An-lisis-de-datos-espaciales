# ------------------------------- ALTURA FINCA ----------------------------------------------------

# ********** 1 - LECTURA DE ARCHIVO ********************************************
# DEFINIR CARPETA DE TRABAJO
setwd("C:/Users/user/Desktop/2021_CURSO_AADE_Online/07_TRABAJO_INTEGRADOR")
getwd()# cual es directorio de trabajo
dir() # lista los archivos del directorio de trabajo
dir(pattern = ".R") # script de R
dir(pattern = ".csv")# muestra los archivos con extensión .csv

# LECTURA DE ARCHIVO EXCEL
library(readxl)
alturaO <- read_excel("Altura.xlsx") 
head(alturaO)
names(alturaO)
summary(alturaO)
str(alturaO)
colnames(alturaO)

# ANALIZA LA TABLA DE DATOS
print(alturaO)
View(alturaO)
edit(alturaO)
head(alturaO)
tail(alturaO)
dim(alturaO)
ncol(alturaO)
nrow(alturaO)

# ***************** 2 - ESTIDISTICA DESCRIPTIVA  ******************************
# RESUME LAS VARIABLES DE LA BASE DE DATOS CON FILAS NA (SIN DATO)
summary(alturaO)

# ESTADISTICA BÁSICA PARA ALTURA ORTO
library(fBasics)
estadisticas_alturaO <- basicStats(alturaO$Altura_orto)
round(estadisticas_alturaO, 2) # redondea los valores

# MEDIDAS ESTADISTICAS DE RESUMEN Y DISPERSIÓN PARA ALTURA_ORTO
(maximo.AO<-max(alturaO$Altura_orto))
(minimo.AO<-min(alturaO$Altura_orto))
(media.AO<-mean(alturaO$Altura_orto))
(mediana.AO<-median(alturaO$Altura_orto))
library(modeest)
(moda.AO<-mfv(alturaO$Altura_orto))
(var.AO<-var(alturaO$Altura_orto))
(DE.AO<-sd(alturaO$Altura_orto))
CV.AO<-(DE.AO/media.AO)*100 # (DE/MEDIA)*100
CV.AO
EE.AO<-(DE.AO)/sqrt(length(alturaO$Altura_orto)) # (DE/RAÍZ DE n)
EE.AO
library(fmsb) 
(c.tiles<-quantile(alturaO$Altura_orto))
(p.tiles<-percentile(alturaO$Altura_orto)) # no entiendo la salida

# 0: simetrica, -: a la derecha, +: a la izquierda
(simetria<-skewness(alturaO$Altura_orto))

# picuda: K>3, plana: k<3
(kurtosis<-kurtosis(alturaO$Altura_orto))

# INTERVALO DE CONFIANZA PARA LA MEDIA DE ALTURA_ORTO
EE<-(DE.AO)/sqrt(length(alturaO$Altura_orto))
(IC<-c(media.AO+ qt(0.975,df=99)*EE,media.AO- qt(0.975,df=99)*EE))

# ****************3- GRÁFICOS PARA ESTUDIAR LA VARIABLE ************************

# GRÁFICO DE PUNTOS: LINEA DE UNIÓN Y MEDIA, MEDIANA, MIN Y MAX (Y=Altura_orto, X= PUNTO GPS)
plot(alturaO$Altura_orto)
lines(alturaO$Altura_orto)
abline(h = mean(alturaO$Altura_orto), col = "red")
abline(h = median(alturaO$Altura_orto), col = "black")
abline(h = min(alturaO$Altura_orto), col = "blue")
abline(h = max(alturaO$Altura_orto), col = "green")

# GRÁFICO DE LINEA (Y=Altura_orto, X= PUNTO GPS)
plot(alturaO$Altura_orto, type = "l")

# GRÁFICO DE PUNTOS (Y=Altura_orto, X= Altitud)
plot(alturaO$Altura_orto,alturaO$Altitud, xlab = "Altitud (GPS)", ylab = "Altura ortométrica",
     main = "Altura_Orto Vs Altura_GPS")

#  GRÁFICO DE PUNTOS (Y=Altura_orto X= Longtiud, Latitud)
par(mfrow=c(1,2)) # par agrupa gráficos en la misma ventana
plot(alturaO$X_UTM, alturaO$Altura_orto, ylab="Altura ortométrica", xlab="Coordenada X")
plot(alturaO$Y_UTM, alturaO$Altura_orto, ylab="Altura ortométrica", xlab="Coordenada Y")
par(mfrow=c(1,1))

# HISTOGRAMA MAS COMPLETO (X = Altura_orto, Y= frecuencia)
hist(alturaO$Altura_orto, main = "Histograma de Altura Ortométrica",
     xlab = "Altura Ortométrica", ylab = "Frecuencia", col = "steelblue")

# BOXPLOT (DIAGRAMA DE CAJAS) Altura_orto
boxplot(alturaO$Altura_orto, col = "orange",main = "Altura ortométrica")

# *************4- ANALISIS DE TENDENCIA EN FUNCIÓN DE LAS COOORDENADAS X E Y ***
library(geoR)
alturaOT <- as.geodata(alturaO, coords.col = c('X_UTM', 'Y_UTM'), data.col = 'Altura_orto')
duplicated(alturaOT)# VERIFICAR SI HAY COORD DUPLICADAS
plot(alturaOT)
plot(alturaOT, trend = "1st") # ELIMINAMOS LA TENDENCIA LINEAL EN X E Y
plot(nutricion1, trend = "2nd") # ELIMINAMOS LA TENDENCIA CUADRATICA EN X E Y

# ************ 5- PRUEBAS DE SUPUESTOS DE NORMALIDAD ***************************
# TEST DE SHAPIRO - n < 50
# P< ALFA RECHAZAMOS H NULA, NO HAY NORMALIDAD
# P> ALFA ACEPTAMOS H NULA,HAY NORMALIDAD

# TEST DE NORMALIDAD
library(nortest)
# prueba de Anderson-Darling
ad.test(alturaO$Altura_orto)
# Lilliefors (Kolmogorov-Smirnov)
lillie.test(alturaO$Altura_orto)
# Shapiro - prueba sobre los residuos
shapiro.test(alturaO$Altura_orto) 
# Q-Q PLOT
library(car)
qqPlot(alturaO$Altura_orto, col="red", col.lines = "black")

#*************** 6 - EVALUAR LA CONTINUIDAD ESPACIAL DE LOS DATOS **************
library(sp)
library(gstat)
limites <- seq(0,467.0131, by = 25) # uso la mediana
altura_sp <- SpatialPointsDataFrame(coords = alturaO[, c('X_UTM','Y_UTM')], data = alturaO[, 'Altura_orto'])
distancias <- spDists(altura_sp)
summary(as.numeric(distancias))
min(distancias)
max(distancias)
mean(distancias)
median(distancias)
hscat(Altura_orto ~ 1, data = altura_sp, breaks = limites)
#par(mfrow = c(2, 2))
#par(mfrow = c(1, 1))

# ************** 7- VISUALIZAR LOS PUNTOS DE GPS EN OPEN STREETMAP *************
library(leaflet)
leaflet(alturaO) %>%
  addTiles() %>%
  addCircleMarkers(radius =0.05,color = ~c('red'), data = alturaO, lng = ~Long, lat = ~Lat)

#*************** 8 - VISUALIZACIONES DE DATOS ESPACIALES ************************
# INTERPOLACIÓN LINEAL, CURVAS DE NIVEL Y LOS PUNTOS MUESTREO
library(fields)
library(akima)
int.alturaO <- with(alturaO, interp(x = X_UTM, y = Y_UTM, z = Altura_orto))
image.plot(int.alturaO) # 
contour(int.alturaO, add = TRUE)
points(Y_UTM ~ X_UTM, alturaO, col = "black")

# REPRESENTACIÓN 3D DE LA ALTURA
persp(int.aq$x,int.aq$y,int.aq$z,xlab="X_UTM",ylab="Y_UTM",zlab="Altura_orto",phi=30,theta=20
,col="lightblue",expand=.5, ticktype="detailed")

#*#*************** 9 - VARIOGRAMAS***********************************************
#*** Obtenga un geodata 
library(geoR)
AlturaO.geo <- as.geodata(alturaO, coords=11:12, data.col=8)

#*** Variograma Altura_orto
vario.AlturaO <- variog(AlturaO.geo)
vario.AlturaO$v
vario.AlturaO$n
vario.AlturaO$u
vario.AlturaO$max.dist
plot(vario.AlturaO, main = "variograma omnidireccional", col="blue", pch=20)

#*** variograma BIN
bin.AlturaO <- variog(AlturaO.geo,max.dist = 1400.828) # calcula un semivariogrÃ¡mas omnidireccional
names(bin.AlturaO)
plot(bin.AlturaO)

bin.AlturaO$u # vector de distancias
bin.AlturaO$v # vector con los valores estimados para u
bin.AlturaO$n # numero de pares
bin.AlturaO$bins.lim
bin.AlturaO$max.dist
bin.AlturaO$direction # direcciÃ³n de calculo del variograma
bin.AlturaO$sd # DE de los valores de cada bin
plot(bin.AlturaO, main = "variograma bin", col="red", pch=20)

#*** variograma cloud
cloud.AlturaO <- variog(AlturaO.geo, option = "cloud")
plot(cloud.AlturaO, main = "variograma cloud")

#*** variograma smooth 
smooth.AlturaO <- variog(AlturaO.geo, op = "smooth",band=800)
plot(smooth.AlturaO, type = 'l',main = "variograma smooth")

#*** variograma Robusto
vario_R <- variog(AlturaO.geo, estimator.type = "modulus", max.dist = 1400.828)
plot(vario_R,main = " variograma robusto")

#************* VARIOGRAMAS EN DIRECCIONES DE ORIENTACION FIJA 0 °, 45 °, 90 ° y 135 °
#********Variogramas direccionales en conjunto
vario4.AlturaO <- variog4(AlturaO.geo, max.dist = 1400.828,type="1")
plot(vario4.AlturaO)
#********Parto el grafico en 3 filas y 2 columnas
par(mfrow = c(3, 2))
#********COMPARACION VARIO BIN CON VARIOGRAMA EN DIRECCION 0
vario_0 <- variog(AlturaO.geo, max.dist = 1400.828, dir = 0, tol = pi/8)
plot(bin.AlturaO, type = "l", lty=2)
lines(vario_0)
legend("topleft", legend=c("omnidirectional", expression(0 * degree)),
       lty=c(2,1), bty = "n")
#********COMPARACION VARIO BIN CON VARIOGRAMA EN DIRECCION  45
vario_45 <- variog(AlturaO.geo, max.dist = 1400.828, dir = pi/4, tol = pi/8)
plot(bin.AlturaO, type = "l", lty=2)
lines(vario_45)
legend("topleft", legend=c("omnidirectional", expression(45 * degree)),
       lty=c(2,1), bty = "n")
#********COMPARACION VARIO BIN CON VARIOGRAMA EN DIRECCION  90
vario_90 <- variog(AlturaO.geo, max.dist = 1400.828, dir = pi/2, tol = pi/8)
plot(bin.AlturaO, type = "l", lty=2)
lines(vario_90)
legend("topleft", legend=c("omnidirectional", expression(90 * degree)),
       lty=c(2,1), bty = "n")
#********COMPARACION VARIO BIN CON VARIOGRAMA EN DIRECCION  135
vario_135 <- variog(AlturaO.geo, max.dist = 1400.828, dir = (3*pi)/4, tol = pi/8)
plot(bin.AlturaO, type = "l", lty=2)
lines(vario_135)
legend("topleft", legend=c("omnidirectional", expression(135 * degree)),
       lty=c(2,1), bty = "n")
#********COMPARACION VARIO BIN CON VARIOGRAMA EN DIRECCION  180
vario_180 <- variog(AlturaO.geo, max.dist = 1400.828, dir = pi, tol = pi/8)
plot(bin.AlturaO, type = "l", lty=2)
lines(vario_135)
legend("topleft", legend=c("omnidirectional", expression(180 * degree)),
       lty=c(2,1), bty = "n")
#********COMPARACION VARIO BIN CON VARIOGRAMA EN DIRECCION  270 - NO VA
#vario_270 <- variog(AlturaO.geo, max.dist = 1400.828, dir = (3*pi)/2, tol = pi/8)
#plot(bin.AlturaO, type = "l", lty=2)
#lines(vario_270)
#legend("topleft", legend=c("omnidirectional", expression(270 * degree)),
#       lty=c(2,1), bty = "n")
par(mfrow = c(1, 1))

# *************  10 - AJUSTAR PARAMETROS DEL VARIOGRAMA **********************
#funcion eyefit() # ajuste a ojo
#windows()
#eyefit(vario.AlturaO)

# Grafico las lineAs de los modelos para la direccion 135
# vARIOGRAMA GAUSSIANO A 135°, PARAMETROS DEL VARIOGRAMA: sill(80.59);range(592.77);nugget(13.89)
plot(vario_135, pch=20, col="red")
lines.variomodel(cov.model="gaussian", cov.pars=c(89,592.77), nug=13.89,max.dist=1400.828, col="green",lwd=2,lty =1)
legend(300, 30,legend= c( "Gaussiano"), lty = c(1), col = c("green"),cex=0.8,box.lty=0)
#****** Funcion de  maxima verosimilitud o maximum likelihood (ML)
vario.ml <- likfit(AlturaO.geo, ini = c(89,592.77), nugget =13.89, lik.method = "ML")
vario.ml
AIC(vario.ml)
#***** Funcion de verosimilitud restringida o Restricted Maximum Likelihood (REML)
vario.reml <- likfit(AlturaO.geo, ini = c(89,592.77),nugget =13.89, lik.method = "RML")
vario.reml
AIC(vario.reml)
#*******AJUSTE DE VEROSIMILITUD CON RESPECTO AL VARIOGRAMA 135
plot(vario_135, pch=1)
lines.variomodel(cov.model="gaussian", cov.pars=c(67.14,965.91), nug=13.89, max.dist=1400.828, col="red", lwd=2,lty =1)
lines.variomodel(cov.model="gaussian", cov.pars=c(212.7,3122.3), nug=13.89, max.dist=1400.828, col="green", lwd=2,lty =1)
legend(1, 100, c("ML", "REML"), lty = c(1, 1), col = c("red", "green"),cex=0.8,box.lty=0)

# ********* 11 - VERIFICAR  PARAMETROS POR FRONTERA ENVELOPED DEL VARIOGRAMA ****
enenv.indep <- variog.mc.env(AlturaO.geo, obj.var = vario.AlturaO)
env.model.ml <- variog.model.env(AlturaO.geo, obj.var = vario.AlturaO, model = vario.ml)
env.model.reml <- variog.model.env(AlturaO.geo, obj.var = vario.AlturaO, model = vario.reml)

#****** ML - MAXIMA VEROSIMILITUD
#********Parto el grafico en 3 filas y 2 columnas
par(mfrow = c(1, 2))
plot(vario.AlturaO, envelope = env.model.ml, max.dist = 1400.828)
lines(vario.ml, lty = 2, lwd = 2, max.dist = 1400.828,col = "red")
legend(30, 200,legend= c( "ML"), lty = c(1), col = c("red"),cex=0.8,box.lty=0)
# (30,200) indican la posición en x e y de la leyenda

#***** REML - MAXIMA VEROSIMILITUD RESTRINGIDA
plot(vario.AlturaO, envelope = env.model.reml)
lines(vario.reml, lty = 2, lwd = 2, max.dist = 1400.828,col = "green")
legend(30, 300,legend= c( "REML"), lty = c(1), col = c("green"),cex=0.8,box.lty=0)
# volvemos a colocar la pantallA original
par(mfrow = c(1, 1))

# ********************  12 - KRIGING ORDINARIO *********************************
# Defino Grilla utilizanod Xmin, Xmax, Ymin,Ymax
# le restamos -150m a Xmin y sumamos 150m a Xmax
# le restamos -150m a Ymin y sumamos 150m a Ymax
pred.grid <- expand.grid(x = seq(510291, 511401, l = 100), y = seq(6443937, 6445470, l = 100))
plot(AlturaO.geo$coords, pch = 20)
points(pred.grid, pch = 3, cex = 0.2, col="green")
#****** krige- modelo GaussianO a 135
ko.reml <- krige.conv(AlturaO.geo, locations =  pred.grid,
                      krige = krige.control( cov.model="gaussian", cov.pars=c(89.59,592.77), nug=13.89))

# GRAFICO DE PREDICCIONES Y VAR KRIGING
library(sp)
# Convertir el data.frame GeoR en data.frame espacial de sp
alturaO_sp <- alturaO
names(alturaO_sp)
coordinates(alturaO_sp) <- ~ X_UTM + Y_UTM
# Crear SpatialPixelDataFrame a partir del objeto geoR
ko.reml_sp <- SpatialPixelsDataFrame(points = pred.grid,data = data.frame(ko.reml[1:2]))
# Gráfico de las predicciones
#layout(matrix(c(1,2), 1,2)) #PARTIR PANTALLA EN DOS
#layout(matrix(c(1,1), 1,1)) #VOLVER A LA PANTALLA ORIGINAL
spplot(ko.reml_sp, zcol = 'predict', col.regions = heat.colors(100), contour=TRUE)
# Gráfico incluyendo puntos de muestreo
spplot(ko.reml_sp, zcol = 'predict', sp.layout = list('sp.points', alturaO_sp, col = "blue" ),
       col.regions = heat.colors(100), contour=TRUE)
# Gráfico incluyendo puntos de muestreo
spplot(ko.reml_sp, zcol = 'krige.var', sp.layout = list('sp.points', alturaO_sp, col = "blue"),
       col.regions = heat.colors(100), contour=TRUE)

# ESTADISTICAS DESCRIPTICAS DE LAS PREDICCIONES Y VAR KRIGING
summary(ko.reml)
summary(ko.reml$predict)# PARA VER LA DISTRIBUCIÓN DE LAS PREDICCIONES
summary(ko.reml$krige.var) # VARIANZA KRIGING
hist(ko.reml$predict) # HISTOGRAMA PARA LOS SUPUESTO DE NORMALIDAD

# EXPORTAR KRIGING PREDICCIONES GEOTIF
#library(raster)
# ********************* 13 - AUTOCORRELACIÓN ESPACIAL **************************
library(gridExtra)
library(ape)
library(spdep)
#install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
# escalar los datos de Altura_orto
alturaOE <- scale(alturaO$Altura_orto)
head(alturaOE)
View(alturaOE)
# 11 y 12 son las coordenadas UTM y las toma como una matriz
distancia<- as.matrix(dist(alturaO[,11:12])) 
# calcula la distancia entre cada sensor o estación de muestreo
distancia<-round(distancia,2)
# Grafica la tabla de distancias
grid.table(distancia)
ponderacion<- 1/distancia
diag(ponderacion)<- 0
# Ho = no hay autocorrelacion espacial o es = 0
# H1= hay autocorrelacion espacial
# p< alfa rechazo Ho, P> alfa acepto Ho
Moran.I(alturaO$Altura_orto,ponderacion)

#*********************** 14 - CORRELOGRAMA *************************************
# Correlograma espacial basado en el IM, función identifica a los vecinos de los puntos de la 
# región por distancia euclidiana entre inferior (mayor que) y superior (inferior o igual a) 

# abrir librerias
#install.packages() # instalar librerias
#installed.packages()# ver paquetes instalados
library(sf)
library(sp)
library(permute)
library(lattice)
library(vegan)
library(spData)
library(spdep)

setwd("C:/Users/user/Desktop/2021_CURSO_AADE_Online/07_TRABAJO_INTEGRADOR")
getwd()
library(readxl)
finca <-read_excel("Altura.xlsx")
View(finca)
alt_orto <- finca[,8] # altura ortométrica
View(alt_orto)
summary(alt_orto)
utm_xy <- finca[,11:12] # Cordenadas UTM: X_UTM, Y_UTM
View(utm_xy)
summary(utm_xy)
plot(utm_xy$X_UTM, utm_xy$Y_UTM, col="green4", pch=20)

#  estandarizar con decostand método hellinge si la variable no es simetrica, 
#  estandarizar con scale la variable alt_orto porque es normal
#alt_ortoE <- decostand(alt_orto,"hellinge")
alt_ortoE <- scale(alt_orto)
View(alt_ortoE)
boxplot(alt_orto, ylab=" variable original")
boxplot(alt_ortoE, ylab="variable estandarizada")

# escalado y centrado: utilizando scale para las Cord. UTM 
utm_xy.c<-scale(utm_xy, center=T, scale = F)
View(utm_xy.c)

# VECINDAD CON DIFERENTES DISTANCIAS:  113, 123, 133, 143, 153
# 113 es la distancia mínima entre ubicaciones
(nbl1<-dnearneigh(as.matrix(utm_xy),103,113))
plot(nbl1, utm_xy.c)
summary(nbl1)

(nbl2<-dnearneigh(as.matrix(utm_xy),103,123)) 
plot(nbl2,utm_xy.c)
summary(nbl2)
layout(matrix(c(1,2), 1,2)) #PARTIR PANTALLA EN DOS
(nbl3<-dnearneigh(as.matrix(utm_xy),103,133))
plot(nbl3, utm_xy.c, main = "distancia 143 m")
summary(nbl3)

(nbl4<-dnearneigh(as.matrix(utm_xy),103,143))# SELECCIONO ESTA 
plot(nbl4, utm_xy.c, main = "distancia 143 m")
summary(nbl4)
layout(matrix(c(1,1), 1,1)) #VOLVER A LA PANTALLA ORIGINAL

(nbl5<-dnearneigh(as.matrix(utm_xy),103,153))
plot(nbl5, utm_xy.c)
summary(nbl5)

(nbl6<-dnearneigh(as.matrix(utm_xy),103,163))
plot(nbl6, utm_xy.c)
summary(nbl6)

# Determinar  Correlograma altura orto metrica
nbl4<-dnearneigh(as.matrix(utm_xy),103,143) # colocar distancia de vecindad
summary(nbl)
AO.correlog <- sp.correlogram(nbl4, alt_ortoE, order=10, 
                              method ="I", zero.policy = TRUE)
summary(AO.correlog)
summary(AO.correlog,utm_xy)
print(AO.correlog, p.adj.method="holm")
plot(AO.correlog, main="Correlograma de altura ortométrica")
 

# BORRAR TODOS LOS OBJETOS DE R Y SALIR
rm (list = ls (all = TRUE))
objects()
q()
