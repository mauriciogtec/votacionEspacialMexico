library(rvest)
library(stringi)
library(RCurl)
library(dplyr)
library(tidyr)

# OBTENIENDO UNA FECHA PARTICULAR
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


# OBTENIENDO TODO EL PRIMER PERIODO DE LA LEGISLATURA 36
url_code <- getURL("http://www.senado.gob.mx/index.php?watch=36&sm=1&ano=1&tp=O&np=1&lg=63")
legis <- read_html("http://www.senado.gob.mx/index.php?watch=36&sm=1&ano=1&tp=O&np=1&lg=63")
