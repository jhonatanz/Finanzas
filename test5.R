library(xml2)
library(tidyverse)
library(readxl)
rm(list = ls())
system("wget --no-check-certificate https://www.bvc.com.co/pps/tibco/portalbvc/Home/Mercados/enlinea/acciones?action=dummy -O page.html
")
xml_file <- read_html("page.html")
#crea el node_set de las todas las paginas del documento
a <- xml_find_all(xml_file, "//a")
#construct a character matrix with the elements <a from the html
c <- cbind(xml_text(a), xml_attr(a, "href"), xml_attr(a, "onclick"))
c <- gsub(" ", "", c)
c <- gsub("\n", "", c)
c <- gsub("\t", "", c)
c <- gsub("\r", "", c)
#convert to a dataframe, apply filters to obtain the stocks list
lista_acc <- data.frame(c, stringsAsFactors = F)
names(lista_acc) <- c("Acciones", "href", "onclick")
lista_acc <- lista_acc %>%
      filter(Acciones!="" & href !="" & onclick!="") %>%
      select(Acciones)
lista_acc <- mutate(lista_acc,
                    part0 ="wget -O ",
                    part1 = 
                      "https://www.bvc.com.co/mercados/DescargaXlsServlet?archivo=acciones_detalle&nemo=", 
                    part2 = "&tipoMercado=1&fechaIni=2018-09-27&fechaFin=2019-03-11", 
                    part3 = Acciones,
                    part4 = ".xls")
#creates a new colum with the url of each stock table of value of unit
lista_acc2 <- unite(lista_acc, compl, part1, part3, part2, sep = "", remove = TRUE)
rm(a, c, xml_file)
#create a table with the url of each stock table
write.table(lista_acc2$compl, file = "test2",quote = FALSE, row.names = FALSE, col.names = FALSE)
#download the information and store this in excel files
system("wget --no-check-certificate -i test2")
system("rename 's/DescargaXlsServlet\\?archivo\\=acciones_detalle\\&nemo\\=//g' *")
system("rename 's/\\&tipoMercado\\=1\\&fechaIni\\=2018-09-27\\&fechaFin\\=2019-03-11/.xls/g' *")
system("find . -name \"*.xls.1\" -type f -delete")
system("mv *.xls files")
paths <- list.files("files", full.names = T)
df3 <- read_excel(paths[1])
for(i in 2:length(paths)){
  a <- read_excel(paths[i])
  df3 <- rbind(df, a)
}
write.csv(df3, file = "valores.csv", row.names = F)
