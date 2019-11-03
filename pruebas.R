library(DBI)
library(RMySQL)
library(tidyverse)
library(lubridate)
rm(list = ls())
con <- dbConnect(MySQL(), group = "bolsa")
df <- dbGetQuery(con, "SELECT * FROM valores")
df$fecha <- as.Date(df$fecha)
df1 <- df %>%
  arrange(Nemotecnico, fecha)%>%
  filter(fecha > "2018-10-01")%>%
  select(-ind)%>%
  unique()

x = c(1:length(df1$fecha))
plot(x, df1$Precio_Cierre, type = "l")
plot(df1$fecha, df1$Precio_Cierre, type = "l")
plot(df1$fecha, df1$VariacionP, type = "l")
df_ayer<-filter(df, fecha == today()-1)
r1<-c(1.5, 0.1, 0.5)
r2<-c(0.2, 1.3, 1.5)
p1<- seq(0, 1, 0.01)
p2<-1-p1
gan<-p1*r1[1]+p2*r2[1]+p1*r1[2]+p2*r2[2]+p1*r1[3]+p2*r2[3]
plot(c(1:length(gan)), gan, type = "l")
max(gan)
#Pruebas para calcular rentabilidad y riesgo de cada accion
bynemo<-group_by(df1, Nemotecnico)
tabla_rent<-summarise(bynemo, 
                      hist_renta = 100*(Precio_Cierre[which.max(fecha)]-Precio_Cierre[which.min(fecha)])/
                        Precio_Cierre[which.min(fecha)],
                      prom_renta_d = mean(VariacionP),
                      Riesgo = var(VariacionP),
                      fecha_vieja = min(fecha))
for(i in 2:length(df$VariacionP)){
  df$VariacionP[i] = 100*(df$Precio_Cierre[i]-df$Precio_Cierre[i-1])/df$Precio_Cierre[i-1]
}
#calculo de rentabilidad y riesgo de portafolio
tabla_var_p <- select(df1, fecha, Nemotecnico, VariacionP)
tabla_var_p2 <- spread(tabla_var_p, key = Nemotecnico, value = VariacionP)
tabla_var_p2[is.na(tabla_var_p2)] <- 0
matriz_cov <- cov(select(tabla_var_p2, -fecha))
#generacion del vector de pesos del portafolio
#primer vector, todas las acciones por igual
v1 <- 1:length(nombres)
v1[] <- 1/length(nombres)
rent_v1 <- t(v1)%*%tabla_rent$hist_renta
risk_v1 <- t(v1) %*% matriz_cov %*% v1
#segundo vector, generacion aleatoria
v2 <- runif(40)
#normalizacion de v2
v2 <- v2/sum(v2)
rent_v2 <- t(v2)%*%tabla_rent$hist_renta
risk_v2 <- t(v2) %*% matriz_cov %*% v2
#tercer vector, portafolio compuesto solo por la accion mas riesgosa
v3 <- 1:length(nombres)
v3[] <- 0
v3[9] <- 1
rent_v3 <- t(v3)%*%tabla_rent$hist_renta
risk_v3 <- t(v3) %*% matriz_cov %*% v3
dbDisconnect(con)
test1<-filter(df1, Nemotecnico == nombres[1])$fecha
test2<-filter(df1, Nemotecnico == nombres[2])$fecha
