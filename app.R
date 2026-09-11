library(shiny)

# ============================================================
# PROBABILIDAD MORTAL - Version Shiny para RStudio
# ============================================================
# Reglas:
#  - Verde   (+3 puntos)
#  - Amarilla(+1 punto)
#  - Roja    -> pierdes TODO lo acumulado en el turno
#  - Puedes "plantarte" para asegurar tus puntos del turno
# ============================================================

ui <- fluidPage(
  titlePanel("💀 Probabilidad Mortal"),

  fluidRow(
    column(
      width = 5,
      wellPanel(
        h4("Reglas"),
        tags$ul(
          tags$li(tags$span(style = "color:#2e7d32; font-weight:bold;", "Verde: +3 puntos")),
          tags$li(tags$span(style = "color:#f9a825; font-weight:bold;", "Amarilla: +1 punto")),
          tags$li(tags$span(style = "color:#c62828; font-weight:bold;", "Roja: pierdes TODO lo acumulado en el turno")),
          tags$li("Puedes 'Plantarte' para asegurar tus puntos")
        )
      ),
      wellPanel(
        h4("Estado de la caja"),
        uiOutput("tabla_probabilidades")
      )
    ),

    column(
      width = 7,
      wellPanel(
        h3(textOutput("puntos_riesgo")),
        br(),
        actionButton("btn_extraer", "Extraer ficha", class = "btn-success"),
        actionButton("btn_plantarse", "Plantarse", class = "btn-warning"),
        actionButton("btn_reiniciar", "Reiniciar juego", class = "btn-danger"),
        br(), br(),
        h4("Mensajes"),
        uiOutput("mensaje")
      )
    )
  )
)

server <- function(input, output, session) {

  # Estado inicial del juego, guardado en reactiveValues
  estado_inicial <- function() {
    list(
      verdes = 5,
      amarillas = 10,
      rojas = 2,
      puntos_turno = 0,
      mensaje = "Presiona 'Extraer ficha' para comenzar.",
      color_mensaje = "#333333",
      juego_activo = TRUE
    )
  }

  rv <- reactiveValues()
  vals <- estado_inicial()
  for (nm in names(vals)) rv[[nm]] <- vals[[nm]]

  # ---- Tabla de probabilidades (con colores) ----
  output$tabla_probabilidades <- renderUI({
    total <- rv$verdes + rv$amarillas + rv$rojas
    if (total == 0) {
      prob_v <- prob_a <- prob_r <- 0
    } else {
      prob_v <- rv$verdes / total
      prob_a <- rv$amarillas / total
      prob_r <- rv$rojas / total
    }

    fila <- function(color, etiqueta, cantidad, prob) {
      tags$tr(
        tags$td(style = paste0("background-color:", color, "; color:white; font-weight:bold; padding:6px;"), etiqueta),
        tags$td(style = "padding:6px; text-align:center;", cantidad),
        tags$td(style = "padding:6px; text-align:center;", sprintf("%.1f%%", 100 * prob))
      )
    }

    tags$table(
      style = "width:100%; border-collapse: collapse;",
      tags$tr(
        tags$th("Color"), tags$th("Cantidad"), tags$th("Probabilidad")
      ),
      fila("#2e7d32", "Verde (+3)", rv$verdes, prob_v),
      fila("#f9a825", "Amarilla (+1)", rv$amarillas, prob_a),
      fila("#c62828", "Roja (pierdes todo)", rv$rojas, prob_r)
    )
  })

  output$puntos_riesgo <- renderText({
    paste("Puntos en riesgo este turno:", rv$puntos_turno)
  })

  output$mensaje <- renderUI({
    tags$p(style = paste0("color:", rv$color_mensaje, "; font-weight:bold; white-space:pre-line;"), rv$mensaje)
  })

  # ---- Botón: Extraer ficha ----
  observeEvent(input$btn_extraer, {
    if (!rv$juego_activo) {
      rv$mensaje <- "El juego terminó. Presiona 'Reiniciar juego' para volver a jugar."
      rv$color_mensaje <- "#333333"
      return()
    }

    total <- rv$verdes + rv$amarillas + rv$rojas
    if (total == 0) {
      rv$mensaje <- "¡Se han agotado todas las fichas de la caja!"
      rv$color_mensaje <- "#333333"
      rv$juego_activo <- FALSE
      return()
    }

    caja <- c(
      rep("verde", rv$verdes),
      rep("amarilla", rv$amarillas),
      rep("roja", rv$rojas)
    )
    ficha <- sample(caja, 1)

    if (ficha == "verde") {
      rv$verdes <- rv$verdes - 1
      rv$puntos_turno <- rv$puntos_turno + 3
      rv$mensaje <- "¡Una ficha VERDE! (+3 puntos)"
      rv$color_mensaje <- "#2e7d32"

    } else if (ficha == "amarilla") {
      rv$amarillas <- rv$amarillas - 1
      rv$puntos_turno <- rv$puntos_turno + 1
      rv$mensaje <- "¡Una ficha AMARILLA! (+1 punto)"
      rv$color_mensaje <- "#f9a825"

    } else if (ficha == "roja") {
      rv$rojas <- rv$rojas - 1
      perdidos <- rv$puntos_turno
      rv$puntos_turno <- 0
      rv$mensaje <- paste0(
        "¡Una ficha ROJA! Has perdido los ", perdidos, " puntos que llevabas.\n",
        "Fin de la partida."
      )
      rv$color_mensaje <- "#c62828"
      rv$juego_activo <- FALSE
    }

    # Si la caja se vacía tras la extracción, termina el juego
    if (rv$verdes + rv$amarillas + rv$rojas == 0 && rv$juego_activo) {
      rv$mensaje <- paste(rv$mensaje, "\n¡Se han agotado todas las fichas de la caja!")
      rv$juego_activo <- FALSE
    }
  })

  # ---- Botón: Plantarse ----
  observeEvent(input$btn_plantarse, {
    if (!rv$juego_activo) {
      rv$mensaje <- "El juego terminó. Presiona 'Reiniciar juego' para volver a jugar."
      rv$color_mensaje <- "#333333"
      return()
    }
    rv$mensaje <- paste0("Te plantaste con ", rv$puntos_turno, " puntos asegurados.")
    rv$color_mensaje <- "#1565c0"
    rv$puntos_turno <- 0
    rv$juego_activo <- FALSE
  })

  # ---- Botón: Reiniciar juego ----
  observeEvent(input$btn_reiniciar, {
    vals <- estado_inicial()
    for (nm in names(vals)) rv[[nm]] <- vals[[nm]]
  })
}

shinyApp(ui = ui, server = server)
