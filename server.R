
# SERVER

library(shiny)
library(plotly)
library(dplyr)

options(stringsAsFactors = FALSE)

info_asistencia <- readRDS("info_asistencia.RDS")
info_voting <- readRDS("info_voting_periods.RDS")


shinyServer(function(input, output, session) {

  # ==== LEER DATOS Y MENUS DINAMICOS ============================================
  asistencia <- reactive({
    readRDS(paste0("source_rds/", input$period, "_ASISTENCIA.RDS"))
  })
  voting_info <- reactive({
    readRDS(paste0("source_rds/", input$period, "_INFO.RDS"))
  })
  voting_results <- reactive({
    readRDS(paste0("source_rds/", input$period, "_RESULTS.RDS"))
  })

  # ==== ASISTENCIAS Y PARTIDOS ============================================
  
  observe({
    input$period
    updateSelectInput(
      session = session,
      inputId = "asistencia_period_date",
      choices = unique(asistencia()$FECHA)
    )
  })
  
  output$asistencia_data_table_summary <- renderTable({
    if(input$asistencia_period_date == "") return(NULL)
    data <- asistencia() %>% 
      filter(FECHA == input$asistencia_period_date) %>% 
      select(SENADOR, PARTIDO, ASISTENCIA_INFO)
    data_tab <- table(data$PARTIDO, data$ASISTENCIA_INFO) %>%
      as.data.frame.matrix
    data_tab$TOTAL <- apply(data_tab, 1, sum)
    data_tab <- rbind(data_tab, data.frame(t(apply(data_tab, 2, sum)), row.names = "TOTAL", check.names = FALSE))
    data_tab
  })
  output$asistencia_data_table_full <- renderTable({
    if(input$asistencia_period_date == "") return(NULL)
    data <- asistencia() %>% 
      filter(FECHA == input$asistencia_period_date) %>% 
      select(SENADOR, PARTIDO, ASISTENCIA_INFO)
  })
  
  
  # ==== VOTACION POR PARTIDO ==============================================
  
  observe({
    input$period
    updateSelectInput(
      session = session,
      inputId = "voting_date",
      choices = sort(unique(voting_info()$FECHA), decreasing = TRUE)
    )
  })
  
  observe({
    input$voting_date
    updateSelectInput(
      session = session,
      inputId = "voting_subject",
      choices = voting_info()$ASUNTO[voting_info()$FECHA == input$voting_date]
    )
  })
  
  output$subject_full <- renderText({
    input$voting_subject  
  })
  

  info_inday <- reactive({
    if(is.null(input$voting_subject)) return(NULL)
    
    clean_and_order <- function(u) paste(sort(unlist(strsplit(gsub("[ ]+", " ", stringr::str_trim(u)), " "))), collapse = " ")
    
    find_partido <- function(x, today, asistencia) {
      temp <- asistencia %>%
        filter(SENADOR_ORDERED == x) %>%
        select(SENADOR_ORDERED, PARTIDO) %>%
        distinct()
      if (nrow(temp) > 1) warning(sprintf("Cambio de partido: %s", x))
      temp$PARTIDO[1]
    }
    
    VOTACION <- voting_results() %>%
      filter(ASUNTO == input$voting_subject) %>%
      .[ ,-(1:3)]

    VOTACION <- data.frame(
      SENADOR = names(VOTACION)[],
      VOTACION = as.integer(VOTACION[1, ])
    )
    
    asistencia <- asistencia() %>%
      mutate(SENADOR_ORDERED = sapply(SENADOR, clean_and_order))
    VOTACION$PARTIDO <- sapply(VOTACION$SENADOR, function(x) find_partido(clean_and_order(x), input$voting_date, asistencia))

    VOTACION
  })
  
  
  output$voting_table <- renderTable({
    # if (is.null(info_inday())) return(NULL)
    info_inday()
  })

  output$voting_plot <- renderPlotly({
    if (is.null(info_inday())) return(NULL)
    tabl <- info_inday()
    tabl$PARTIDO[is.na(tabl$VOTACION)] <- "(REP. AUSENTES)"
    tabl <- table(tabl$PARTIDO) 
    plot_data <- data.frame(
      PARTIDO = names(tabl),
      VOTO = as.integer(tabl),
      TEXT = c(paste("Ausentismo: ", scales::percent(as.integer(tabl)[1]/sum(as.integer(tabl)))), 
               paste("% del voto:", scales::percent(as.integer(tabl)[-1]/sum(as.integer(tabl)[-1]))))
    )
    # p <- ggplot(data = plot_data, aes(x = PARTIDO, y = VOTO, fill = PARTIDO)) +
    #   geom_bar(stat = "identity")
    #   theme_bw()
    # ggplotly(p, tooltip = plot_data$TEXT)
    plot_ly(
      data = plot_data,
      x = ~ PARTIDO,
      y = ~ VOTO,
      color = ~ PARTIDO,
      type = "bar"
    ) %>% 
      add_annotations(
        text = plot_data$TEXT
      )
  })


  # ==== MDS ===============================================================

})
