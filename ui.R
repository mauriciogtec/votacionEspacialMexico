
# USER INTERFACE 

library(shiny)
library(plotly)

info_asistencia <- readRDS("info_asistencia.RDS")
info_voting <- readRDS("info_voting_periods.RDS")

shinyUI(fluidPage(
  
  titlePanel("Senado Mexico"),
  
  sidebarLayout(
    # SIDEBAR PANEL ================================
    sidebarPanel(
      radioButtons(
        inputId = "func",
        label = "Function",
        choices = c(
          "VOTACION" = "vot",
          "ASISTENCIA" = "asist",
          "MULTIDIMENSIONAL SCALING" = "mds"
        )
      )  
    ),
    
    # PANEL PRINCIPAL ===============================
    mainPanel(
      selectInput(
        inputId = "period",
        label = "Periodo y Legislatura",
        choices = sort(info_voting$codigo, decreasing = TRUE)
      ),
      # ____VOTOS =============================
      conditionalPanel(
        condition = "input.func == 'vot'",
        selectInput(
          inputId = "voting_date",
          label = "Fecha",
          choices = NULL
        ),
        selectInput(
          inputId = "voting_subject",
          label = "Asunto",
          choices = NULL,
          selectize = FALSE
        ),
        textOutput("subject_full"),
        plotlyOutput("voting_plot"),
        br(),
        tableOutput("voting_table")
      ),
      # ____ASISTENCIA =============================
      conditionalPanel(
        condition = "input.func == 'asist'",
        selectInput(
          inputId = "asistencia_period_date",
          label = "fecha",
          choices = NULL # las opciones se cambian reactivamente al escoger el periodo en server
        ),
        tableOutput("asistencia_data_table_summary"),
        tableOutput("asistencia_data_table_full")
      )
      # ____MDS ======================================
    )
  )
))
