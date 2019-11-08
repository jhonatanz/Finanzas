library(tidyverse)
library(readxl)
library(lubridate)
rm(list = ls())
paths <- list.files("proteccion", full.names = T)
dfx <- read_excel(paths[1])
names(dfx) <- make.names(names(dfx))
meses <- month.name
#inicializamos mes y año
m <- 11
y <- 2017
i <- 1
dfx <-mutate(dfx, mes = meses[m], year = y)
dfx <- dfx[0,]
#leemos los 6 primeros meses que tienen 29 acciones
while(i<=6){
  a <- read_excel(paths[i])
  if(m > 12){
    m <- 1
    y <- y+1
  }
  a <-mutate(a, mes = meses[m], year = y)
  names(a)<-names(dfx)
  dfx<-rbind(a, dfx)
  i <- i+1
  m <- m+1
}
dfy <- read_excel(paths[i])
names(dfy) <- make.names(names(dfy))
dfy <-mutate(dfy, mes = meses[m], year = y)
dfy <- dfy[0,]
#leemos los demas meses que tienen 30 acciones
while(i<length(paths)){
  a <- read_excel(paths[i])
  if(m > 12){
    m <- 1
    y <- y+1
  }
  a <-mutate(a, mes = meses[m], year = y)
  names(a)<-names(dfy)
  dfy<-rbind(a, dfy)
  m <- m+1
  i <- i+1
}
#definimos la variable fecha
dfx <- dfx %>%
  mutate(fecha = as_date(paste(year, mes, dia, sep = "-")))%>%
  select(-dia, -year, -mes)
dfy <- dfy %>%
  mutate(fecha = as_date(paste(year, mes, dia, sep = "-")))%>%
  select(-dia, -year, -mes)
#creamos el dataframe en forma correcta
dfx1 <- gather(dfx, names(select(dfx, -fecha)), key = "Nemotecnico", value = "valor")
dfy1 <- gather(dfy, names(select(dfy, -fecha)), key = "Nemotecnico", value = "valor")
df <- rbind(dfx1, dfy1)
df$Nemotecnico[grep("ACCIONES.EE.UU", df$Nemotecnico)]<-c("ACCIONES.ESTADOS.UNIDOS")
#Calculo de rentabilidad y riesgo
bynemo <- arrange(df, Nemotecnico, fecha)
bynemo <- group_by(bynemo, Nemotecnico)
bynemo$valor <- gsub("\\$", "", bynemo$valor)
bynemo$valor <- gsub(",", "", bynemo$valor)
bynemo$valor <- as.numeric(bynemo$valor)
bynemo <- mutate(bynemo, VariacionA = valor-lag(valor), VariacionP = 100*(valor-lag(valor))/valor)
bynemo <- bynemo[complete.cases(bynemo), ]
bynemo$Nemotecnico <- gsub("ACCIONES\\.", "", bynemo$Nemotecnico)
#calculo de riesgo y rentabilidad
bynemo$Nemotecnico[grep("PROTECCION.VISTA", bynemo$Nemotecnico)]<-c("VISTA")
bynemo2<-bynemo[grep("^PROTE", bynemo$Nemotecnico, invert = TRUE), ]
bynemo2<-bynemo2[grep("^RENTA", bynemo2$Nemotecnico, invert = TRUE), ]
bynemo2<-bynemo2[grep("^ALTA", bynemo2$Nemotecnico, invert = TRUE), ]
bynemo2<-bynemo2[grep("^SECTOR", bynemo2$Nemotecnico, invert = TRUE), ]
tabla_var_p <- select(bynemo2, fecha, Nemotecnico, VariacionP)
tabla_var_p2 <- spread(tabla_var_p, key = Nemotecnico, value = VariacionP)
tabla_var_p2[is.na(tabla_var_p2)] <- 0
matriz_cov <- cov(select(tabla_var_p2, -fecha))
#tabla de rentabilidades diarias y riesgo diarios
tabla_rent<-summarise(bynemo2, 
                      hist_renta = 100*(valor[which.max(fecha)]-valor[which.min(fecha)])/
                        valor[which.min(fecha)],
                      prom_renta_d = mean(VariacionP),
                      Riesgo = var(VariacionP),
                      fecha_inicial = min(fecha),
                      fecha_final = max(fecha),
                      std_dev = sd(VariacionP))
#portafolio actual
w<-c(17.22, 0, 0, 0, 10.24, 0, 6.5, 0, 0, 9.63, 0, 0, 0, 9.77, 0, 0, 0, 9.49, 9.75, 
     0, 0, 0, 4.98, 22.42)
w<-w/100
#calculo de rentabilidad y riesgo diario del portafolio
rent_w <- t(w)%*%tabla_rent$prom_renta_d
risk_w <- sqrt(t(w) %*% matriz_cov %*% w)
#rentabilidades pero ahora haciendo un calulo semanal
bynemo3 <- bynemo2
s <- seq(from = 1, to = 52*7, by = 7)
s <- bynemo3$fecha[which.max(bynemo3$fecha)]-s
z <- bynemo3$fecha %in% s
test1 <- bynemo3[z, ]
test1 <- select(test1, fecha, Nemotecnico, valor)
test1 <- mutate(test1, VariacionA = valor-lag(valor), VariacionP = 100*(valor-lag(valor))/valor)
test1 <- test1[complete.cases(test1), ]
tabla_rent_s<-summarise(test1, 
                      hist_renta = 100*(valor[which.max(fecha)]-valor[which.min(fecha)])/
                        valor[which.min(fecha)],
                      prom_renta_d = mean(VariacionP),
                      Riesgo = var(VariacionP),
                      fecha_inicial = min(fecha),
                      fecha_final = max(fecha))
rent_s <- t(w)%*%tabla_rent_s$prom_renta_d
risk_s <- sqrt(t(w) %*% matriz_cov %*% w)
prop_d <-rent_w/risk_w
prop_s <- rent_s/risk_s
hist(tabla_rent$prom_renta_d, breaks = 10, probability = TRUE)
hist(tabla_rent_s$prom_renta_d, breaks = 10, probability = TRUE)
sd(tabla_rent$prom_renta_d)
  