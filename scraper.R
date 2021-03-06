library(rvest)
library(stringi)
library(dplyr)
library(tidyr)
library(xml2)
library(dplyr)
library(xts)

options(stringsAsFactors = FALSE)

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
saveRDS(links_info, "info_voting_periods.RDS")

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
    if (j <= 86) return(NULL)
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
      mutate(VOTO = toupper(stri_trans_general(VOTO, "Latin-ASCII"))) %>%
      mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>% 
      mutate(VOTO = c("PRO" = 1, "ABSTENCION" = 0, "CONTRA"=-1)[VOTO])
    lista_senadores <<- unique(c(lista_senadores, temp$SENADOR))
    temp
  })
  lista_senadores <- sort(lista_senadores)
  voting_results <- do.call("rbind", lapply(voting_data_list, function(temp) {
      t(temp$VOTO[match(lista_senadores, temp$SENADOR)])
    })) %>% 
    data.frame(
      FECHA = lubridate::ymd(lista_dates), 
      ASUNTO = votacion, 
      HREF = paste0("http://www.senado.gob.mx/", a_href),
      .
    ) %>% 
    `names<-`(c("FECHA", "ASUNTO", "HREF", lista_senadores)) 
  # voting_info <- data.frame(
  #   FECHA = lista_dates,
  #   ASUNTO = votacion,
  #   HREF = paste0("http://www.senado.gob.mx/", a_href)
  # )
  saveRDS(object = voting_results, file = paste0("source_rds/", codigo, "_RESULTS", ".RDS"))
  # saveRDS(object = voting_info, file = paste0("source_rds/", codigo, "_INFO", ".RDS"))
}


# 4. INFORMACION DEL PARTIDO =====================================================
links <- read_html("http://www.senado.gob.mx/index.php?watch=35") %>% 
  html_nodes(".tableA ul li a") 
links_text <- links %>% html_text()
links_href <- links %>% html_attr("href")
lg <- gsub("lg=", "", unlist(stringr::str_extract_all(links_href, "lg=([^=&]*)")))
tp <- gsub("tp=", "", unlist(stringr::str_extract_all(links_href, "tp=([^=&]*)")))
np <- gsub("np=", "", unlist(stringr::str_extract_all(links_href, "np=([^=&]*)")))
ano <- gsub("ano=", "", unlist(stringr::str_extract_all(links_href, "ano=([^=&]*)")))
num <- integer(0)
for (l in unique(lg)) for (a in unique(ano[lg == l])) num <- c(num, 1:(sum(lg == l & ano == a)))
codigo <- sprintf("L%s_A%s_N%s_P%s%s_ASISTENCIA", lg, ano, num, tp, np)
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
saveRDS(links_info, "info_asistencia.RDS")

# 5. DESCARGAR LA INFORMACION DE TODOS LOS PERIODOS DISPONIBLES  ======================
# (PRIMERO DESCARGAR PORQUE HAY ERRORES EN LA PAGINA Y NO CONVIENE SCRAPEAR ONLINE)
session <- html_session("http://www.senado.gob.mx/index.php?watch=35")
for (i in 1:nrow(links_info)) {
  texto <- links_info$texto[i]
  codigo <- links_info$codigo[i]
  session %>% 
    follow_link(texto) %>% 
    read_html() %>% 
    write_xml(paste0("source_html/", codigo, ".html")) 
}


# 6. CONSTRUIR LA BASE DE DATOS DE VOTACIONES POR CADA PERIODO LEGISLATIVO =======
for (i in 1:nrow(links_info)) {
  # if (i < 5) next # debugging
  texto <- links_info$texto[i]
  codigo <- links_info$codigo[i]
  html <- read_html(paste0("source_html/", codigo, ".html"))
  a <- html %>% 
    html_nodes(".tableA.Pardo td div a")
  a_href <- a %>% 
    html_attr("href")
  a_text <- a %>% 
    html_text()
  fechas <- a_text %>% 
    stri_trans_general("Latin-ASCII") %>% 
    toupper() %>% 
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
    gsub("[A-Z]|[ ]+|\\.", "" , .) %>% 
    lubridate::dmy()
  # cat(sprintf("Codigo %s: %s\n", i, codigo)) # debugging
  asistencia_data_list <- lapply(seq_along(a), function(j) {
    # cat(sprintf("--| link %s: %s\n", j, a_text[j])) # debugging
    html <- read_html(paste0("http://www.senado.gob.mx/", a_href[j]))
    print(j)
    asistencia_data <- html %>% 
      html_nodes(".tableA.Pardo") %>% 
      html_table(fill = TRUE) %>% 
      lapply(function(d) d[complete.cases(d), ]) %>% 
      do.call("rbind", .) %>% 
      `names<-`(c("SENADOR", "PARTIDO", "ASISTENCIA_INFO")) %>% 
      mutate(SENADOR = toupper(stri_trans_general(SENADOR, "Latin-ASCII"))) %>% 
      mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>% 
      mutate(ASISTENCIA_INFO = toupper(stri_trans_general(ASISTENCIA_INFO, "Latin-ASCII"))) %>% 
      mutate(ASISTENCIA = ASISTENCIA_INFO == "ASISTENCIA") %>% 
      data.frame(FECHA = fechas[j], .)
    asistencia_data
  })
  asistencia <- do.call("rbind", asistencia_data_list)
  print(codigo)
  saveRDS(object = asistencia, file = paste0("source_rds/", codigo, "_ASISTENCIA", ".RDS"))
  lapply(function(d) na.omit(d))
  ensamble <- try({
    asistencia_data <- asistencia_data %>% 
      do.call("rbind", .) %>% 
      `names<-`(c("SENADOR", "PARTIDO", "ASISTENCIA_INFO")) %>% 
      mutate(SENADOR = toupper(stri_trans_general(SENADOR, "Latin-ASCII"))) %>% 
      mutate(SENADOR = gsub("MA\\.", "MARIA", SENADOR)) %>% 
      mutate(ASISTENCIA = ASISTENCIA_INFO == "ASISTENCIA") %>% 
      data.frame(FECHA = fechas[j], .)
  }, silent = TRUE)
  if (inherits(ensamble, "try-error")) {
    warning(sprintf("Error at (codigo: %s, link: %s), skipped...", codigo, a_text[j]))
    return(data.frame())
  } else {
    return(asistencia_data)
  }
  asistencia <- do.call("rbind", asistencia_data_list)
  saveRDS(object = asistencia, file = paste0("source_rds/", codigo, ".RDS"))
}


