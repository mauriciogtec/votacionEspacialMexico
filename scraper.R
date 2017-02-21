library(rvest)
library(stringi)
library(dplyr)
library(tidyr)
library(xml2)
library(dplyr)
library(xts)

options(stringsAsFactors = FALSE)

# OBTENIENDO UNA FECHA PARTICULAR (PRUEBA)
# senado <- read_html("http://www.senado.gob.mx/index.php?watch=36&sm=3&ano=1&tp=O&np=1&lg=63&gp=TOTAL&id=2353")
# senado_summary <- senado %>% 
#   html_nodes(".tableA tr td div") %>% 
#   html_text()
# date_long <- senado_text[1]
# matter <- senado_text[2]
# senado_tables <- senado %>% 
#   html_nodes(".tableA.Pardo") %>% 
#   html_table()
# key <- c("PRO" = 1, "ABSTENCION" = 0, "EN CONTRA"=-1)
# votes <- data.frame(senado_tables[[1]][ ,2:3], row.names = NULL) %>% 
#   `names<-`(c("SENADOR", "VOTO")) %>% 
#   mutate(SENADOR = stri_trans_general(SENADOR, "Latin-ASCII")) %>% 
#   mutate(SENADOR = gsub("Ma.", "Maria", SENADOR)) %>% 
#   mutate(VOTO = key[VOTO])


# 1. OBTENER INFORMACION DE TODOS LOS PERIODOS DISPONIBLES ========================
links <- read_html("http://www.senado.gob.mx/index.php?watch=36") %>% 
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
  codigo = codigo,
  texto = links_text,
  href = links_href,
  stringsAsFactors = FALSE
)

# 2. DESCARGAR LA INFORMACION DE TODOS LOS PERIODOS DISPONIBLES  ======================
# (PRIMERO DESCARGAR PORQUE HAY ERRORES EN LA PAGINA Y NO CONVIENE SCRAPEAR ONLINE)
session <- html_session("http://www.senado.gob.mx/index.php?watch=36")
for (i in 1:nrow(links_info)) {
  texto <- links_info$texto[i]
  codigo <- links_info$codigo[i]
  session %>% 
    follow_link(texto) %>% 
    read_html() %>% 
    write_xml(paste0("source_html/", codigo, ".html")) 
}

  
# 3. CONSTRUIR LA BASE DE DATOS DE VOTACIONES POR CADA PERIODO LEGISLATIVO =======
for (i in 1:nrow(links_info)) {
  texto <- links_info$texto[i]
  codigo <- links_info$codigo[i]
  html <- read_html(paste0("source_html/", codigo, ".html"))
  a <- html %>% 
    html_nodes(".tableA.Pardo td div a")
  a_href <- a %>% 
    html_attr("href")
  a_text <- a %>% 
    html_text()
  votacion <- a_text %>% 
    stri_trans_general("Latin-ASCII") %>% 
    gsub("[\n\r\t]+", " ", .) %>% 
    gsub('"|[ ]+', ' ', .) %>% 
    gsub("[ ]+\\.", "\\.", .) %>% 
    stringr::str_trim() %>% 
    toupper()
  lista_senadores <- character(0)
  lista_dates <- character(0)
  voting_data_list <- lapply(seq_along(a), function(j) {
    session <- html_session(paste0("http://www.senado.gob.mx/", a_href[j]))
    fecha_text <- session %>% 
      read_html() %>% 
      html_nodes(".tableA.Pardo tr td div strong:first-child") %>% 
      html_text() %>% 
      stri_trans_general("Latin-ASCII") %>% 
      toupper()
    fecha_date <- fecha_text %>% 
      gsub("ENERO", "01", .) %>% 
      gsub("FEBRERO", "02", .) %>% 
      gsub("MARZO", "03", .) %>% 
      gsub("ABRIL", "04", .) %>% 
      gsub("MAYO", "05", .) %>% 
      gsub("JUNIO", "06", .) %>% 
      gsub("JULIO", "07", .) %>% 
      gsub("AGOSTO", "08", .) %>% 
      gsub("SEPTIEMBRE", "09", .) %>% 
      gsub("OCTUBRE", "10", .) %>% 
      gsub("NOVIEMBRE", "11", .) %>% 
      gsub("DICIEMBRE", "12", .) %>% 
      gsub("[A-Z]|[ ]+", "" , .) %>% 
      lubridate::dmy()
    lista_dates <<- c(lista_dates, as.character(fecha_date))
    voting_html <- session %>% 
      follow_link("Ver Detalles") %>% 
      read_html()
    voting_table <- voting_html %>% 
      html_nodes(".tableA.Pardo") %>% 
      html_table()
    temp <- data.frame(voting_table[[1]][ ,2:3], row.names = NULL) %>% 
      `names<-`(c("SENADOR", "VOTO")) %>% 
      mutate(SENADOR = toupper(stri_trans_general(SENADOR, "Latin-ASCII"))) %>% 
      mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>% 
      mutate(VOTO = c("PRO" = 1, "ABSTENCION" = 0, "EN CONTRA"=-1)[VOTO])
    lista_senadores <<- unique(c(lista_senadores, temp$SENADOR))
    temp
  })
  lista_senadores <- sort(lista_senadores)
  voting_results <- do.call("rbind", lapply(voting_data_list, function(temp) {
    t(temp$VOTO[match(lista_senadores, temp$SENADOR)])
  })) %>% 
    data.frame(FECHA = lubridate::ymd(lista_dates), .) %>% 
    `names<-`(c("FECHA", lista_senadores)) 
  voting_info <- data.frame(
    FECHA = lista_dates,
    ASUNTO = votacion,
    HREF = paste0("http://www.senado.gob.mx/", a_href)
  )
  saveRDS(object = voting_results, file = paste0("source_rds/", codigo, "_RESULTS", ".RDS"))
  saveRDS(object = voting_info, file = paste0("source_rds/", codigo, "_INFO", ".RDS"))
}

