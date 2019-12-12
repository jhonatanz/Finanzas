library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
con <- dbConnect(MySQL(), group = "bolsa")
df <- dbGetQuery(con, "SELECT * FROM valores")
dbDisconnect(con)
df$fecha <- as.Date(df$fecha)
df_A <- filter(df, Nemotecnico == 'BCOLOMBIA')
i<-length(df_A$Precio_Cierre)
tenkan <- vector(mode = "numeric", length = i)
kijun <- vector(mode = "numeric", length = i)
senkou_B <- vector(mode = "numeric", length = i)
while(i>9){
  tenkan[i]<-(max(df_A$Precio_Mayor[(i-9):i])+min(df_A$Precio_Menor[(i-9):i]))/2
  i<-i-1
}
i<-length(df_A$Precio_Cierre)
while(i>26){
  kijun[i]<-(max(df_A$Precio_Mayor[(i-26):i])+min(df_A$Precio_Menor[(i-26):i]))/2
  i<-i-1
}
i<-length(df_A$Precio_Cierre)
while(i>52){
  senkou_B[i]<-(max(df_A$Precio_Mayor[(i-52):i])+min(df_A$Precio_Menor[(i-52):i]))/2
  i<-i-1
}
senkou_A <- (tenkan+kijun)/2
senkou_A[1:52]<-0
chikou<-vector(mode = "numeric", length = length(df_A$Precio_Cierre))
chikou[1:(length(chikou)-26)]<-df_A$Precio_Cierre[26:length(chikou)]
# el chikou se tiene que correr 26 dias hacia atras

# estandarizacion de tamaños de los vectores del ichimoku
tenkan_sen <- c(tenkan, vector(mode = "numeric", length = 26))
kijun_sen <- c(kijun, vector(mode = "numeric", length = 26))
senkou_span_B <- c(vector(mode = "numeric", length = 26), senkou_B)
senkou_span_A <- c(vector(mode = "numeric", length = 26), senkou_A)
chikou_span <- c(chikou, vector(mode = "numeric", length = 26))
# construccion del vector de fechas
dates <- vector(length = 26)
#se asigna la ultima fecha al vector "dates"
dates[1:26] <- df_A$fecha[length(df_A$fecha)]
dates2 <- 1:26
#se generan 26 fechas en el futuro
dates1 <- dates+dates2
dates3 <- c(df_A$fecha, dates1)
# Vector de los precios de cierre
cierre <- df_A$Precio_Cierre
cierre <- c(cierre, vector(length = 26))
# Construccion del data frame, se reemplazan los ceros por NA para que no sean graficados
df_ichi <- data.frame(fechas = dates3, p_cierre = cierre, TS = tenkan_sen, KS = kijun_sen, 
                      SSA = senkou_span_A, SSB = senkou_span_B, CS =chikou_span)
df_ichi[df_ichi==0] <- NA
# Construccion de graficas
grafica <- ggplot(data = df_ichi[52:length(df_ichi$fechas), ])
grafica <- grafica + geom_line(mapping = aes(x = fechas, y = p_cierre), colour = "BLACK")
grafica <- grafica + geom_line(mapping = aes(x = fechas, y = TS), colour = "BLUE")
grafica <- grafica + geom_line(mapping = aes(x = fechas, y = KS), colour = "MAGENTA")
grafica <- grafica + geom_line(mapping = aes(x = fechas, y = SSA), colour = "GREEN")
grafica <- grafica + geom_line(mapping = aes(x = fechas, y = SSB), colour = "RED")
grafica <- grafica + geom_line(mapping = aes(x = fechas, y = CS), colour = "YELLOW")
print(grafica)
  