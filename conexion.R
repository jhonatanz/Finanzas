library(DBI)
library(RMySQL)
library(xml2)
library(tidyverse)
library(readxl)
library(lubridate)
setwd("~/Documents/R_projects/Finanzas/")
rm(list = ls())
system("wget --no-check-certificate https://www.bvc.com.co/pps/tibco/portalbvc/Home/Mercados/enlinea/acciones?action=dummy -O page.html
")
xml_file <- read_html("page.html")
#crea el node_set de las todas las paginas del documento
a <- xml_find_all(xml_file, "//a")
#construct a character matrix with the elements <a from the html
b <- cbind(xml_text(a), xml_attr(a, "href"), xml_attr(a, "onclick"))
b <- gsub(" ", "", b)
b <- gsub("\n", "", b)
b <- gsub("\t", "", b)
b <- gsub("\r", "", b)
#convert to a dataframe, apply filters to obtain the stocks list
lista_acc <- data.frame(b , stringsAsFactors = F)
names(lista_acc) <- c("Acciones", "href", "onclick")
lista_acc %>% distinct()
lista_acc <- lista_acc %>%
  filter(Acciones !="" & href !="" & onclick!="") %>%
  select(Acciones)
lista_acc <- distinct(lista_acc)
lista_acc <- mutate(lista_acc,
                    part0 ="SELECT Nemotecnico, MAX(fecha) FROM valores WHERE Nemotecnico = \"",
                    part1 = Acciones, 
                    part2 = "\""
                    )
#stock list with a column with the MySQL query required
lista_acc <- unite(lista_acc, compl, part0, part1, part2, sep = "", remove = TRUE)
#connection to the stock values database
con <- dbConnect(MySQL(), group = "bolsa")
#retrieve of latest dates for each stock
i <- 1
#copying the data structure in the df dataframe
df <- dbGetQuery(con, lista_acc$compl[1])
df <- df[0, ]
#loop retrieving the stock's last date to update the database
while(i <= length(lista_acc$Acciones)){
  d <- dbGetQuery(con, lista_acc$compl[i])
  if (is.na(d$Nemotecnico)) {
    d$Nemotecnico <-lista_acc$Acciones[i]
    d$`MAX(fecha)` <-as.character(today()-179)
    df <- rbind(df, d)
  }else{
    df <- rbind(df, d)
  }
  i <- i+1
}
#construct the queries for the bvc page
df1 <- mutate(df,
              part1 = "https://www.bvc.com.co/mercados/DescargaXlsServlet?archivo=acciones_detalle&nemo=", 
              part2 = Nemotecnico,
              part3 = "&tipoMercado=1&fechaIni=",
              part4 = `MAX(fecha)`,
              part5 = "&fechaFin=",
              part6 = as.character(today())
              )
df2 <- unite(df1, compl, part1, part2, part3, part4, part5, part6, sep = "", remove = TRUE)
write.table(df2$compl, file = "listurl",quote = FALSE, row.names = FALSE, col.names = FALSE)

system("wget --no-check-certificate -i listurl")
old_paths <- list.files()
old_paths <- grep("^Descarga", old_paths, value = T)
#old_paths <- sort(old_paths)
dft <- df2 %>%
  mutate(path = Nemotecnico, ext = ".xls")%>%
  unite(new_path, path, ext, sep ="", remove = T)
new_paths <- dft$new_path
file.rename(sort(old_paths), sort(new_paths))
system("rm files/*")
system("mv *.xls files")
#system("rename 's/DescargaXlsServlet\\?archivo\\=acciones_detalle\\&nemo\\=//g' *")
#system("rename 's/\\&tipoMercado\\=1\\&fechaIni\\=2018-09-27\\&fechaFin\\=2019-03-11/.xls/g' *")
#system("find . -name \"*.xls.1\" -type f -delete")
paths <- list.files("files", full.names = T)
df3 <- read_excel(paths[1])
for(j in 2:length(paths)){
  e <- read_excel(paths[j])
  df3 <- rbind(df3, e)
}
df4 <- dbGetQuery(con, "SELECT * FROM valores WHERE Nemotecnico = \"ECOPETROL\"")
names(df3) <- names(df4)[2:length(names(df4))]
dbWriteTable(conn = con, name = "valores", value = df3, append = TRUE, row.names = FALSE)
dbDisconnect(con)
system("rm page.html listurl")
rm(list = ls())