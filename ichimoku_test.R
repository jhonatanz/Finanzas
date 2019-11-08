library(DBI)
library(RMySQL)
library(dplyr)
con <- dbConnect(MySQL(), group = "bolsa")
df <- dbGetQuery(con, "SELECT * FROM valores")
dbDisconnect(con)
df$fecha <- as.Date(df$fecha)
df_A <- filter(df, Nemotecnico == 'BCOLOMBIA')
tenkan <- vector(mode = "numeric")
kijun <- vector(mode = "numeric")
senkou_B <- vector(mode = "numeric")
i<-length(df_A$Precio_Cierre)
while(i>52){
  x<-(max(df_A$Precio_Mayor[(i-9):i])+min(df_A$Precio_Menor[(i-9):i]))/2
  tenkan<-c(tenkan, x)
  i<-i-1
}
i<-length(df_A$Precio_Cierre)
while(i>52){
  x<-(max(df_A$Precio_Mayor[(i-26):i])+min(df_A$Precio_Menor[(i-26):i]))/2
  kijun<-c(kijun, x)
  i<-i-1
}
i<-length(df_A$Precio_Cierre)
while(i>52){
  x<-(max(df_A$Precio_Mayor[(i-52):i])+min(df_A$Precio_Menor[(i-52):i]))/2
  senkou_B<-c(senkou_B, x)
  i<-i-1
}
senkou_A <- (tenkan+kijun)/2
