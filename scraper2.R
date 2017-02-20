library(rvest)
library(stringi)
library(RCurl)
library(dplyr)
library(tidyr)
library(xml2)
library(plyr)


# OBTENIENDO UNA FECHA PARTICULAR (PRUEBA)
senado <- read_html("http://www.senado.gob.mx/index.php?watch=36&sm=3&ano=1&tp=O&np=1&lg=63&gp=TOTAL&id=2353")
senado_summary <- senado %>% 
  html_nodes(".tableA tr td div") %>% 
  html_text()
date_long <- senado_text[1]
matter <- senado_text[2]
senado_tables <- senado %>% 
  html_nodes(".tableA.Pardo") %>% 
  html_table()
key <- c("PRO" = 1, "ABSTENCION" = 0, "EN CONTRA"=-1)
votes <- data.frame(senado_tables[[1]][ ,2:3], row.names = NULL) %>% 
  `names<-`(c("SENADOR", "VOTO")) %>% 
  mutate(SENADOR = stri_trans_general(SENADOR, "Latin-ASCII")) %>% 
  mutate(SENADOR = gsub("Ma.", "Maria", SENADOR)) %>% 
  mutate(VOTO = key[VOTO])


# OBTENER INFORMACION DE TODOS LOS PERIODOS DISPONIBLES
senado_main <- html_session("http://www.senado.gob.mx/index.php?watch=36")
links <- senado_main %>% 
  html_nodes(".tableA ul li a") 
links_text <- links %>% html_text()
links_href <- links %>% html_attr("href")
lg <- gsub("lg=", "", unlist(stringr::str_extract_all(links_href, "lg=([^=&]*)")))
tp <- gsub("tp=", "", unlist(stringr::str_extract_all(links_href, "tp=([^=&]*)")))
np <- gsub("np=", "", unlist(stringr::str_extract_all(links_href, "np=([^=&]*)")))
ano <- gsub("ano=", "", unlist(stringr::str_extract_all(links_href, "ano=([^=&]*)")))
num <- integer(0)
for (l in unique(lg)) for (a in unique(ano[lg == l])) num <- c(num, 1:(sum(lg == l & ano == a)))
codigo <- sprintf("L%s_A%s_N%s_P%s%s", lg, ano, num, tp, np)
links_info <- data.frame(
  legislatura = lg, 
  ano = ano,
  numero = num,
  tipo = tp,
  periodo = np,
  codigo = code,
  texto = links_text,
  href = links_href,
  stringsAsFactors = FALSE
)

# DESCARGAR LA INFORMACION DE TODOS LOS PERIODOS DISPONIBLES (PRIMERO LO DESCARGO PORQUE HAY ERRORES EN LA PAGINA)
session <- html_session("http://www.senado.gob.mx/index.php?watch=36")
for (i in 1:nrow(links_info)) {
  texto <- links_info$texto[i]
  codigo <- links_info$codigo[i]
  session %>% 
    follow_link(texto) %>% 
    read_html() %>% 
    write_xml(paste0("source_html/", codigo, ".html")) 
}

  



