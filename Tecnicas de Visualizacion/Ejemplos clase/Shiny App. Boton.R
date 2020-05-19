library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("boton", "Aburrida?"),
      sliderInput("slider", "Cuanto?", min = 0, max = 10, value = 5)
    ),
    mainPanel(
      verbatimTextOutput("valorBoton")
    )
  )
)

server <- function(input, output){   
  output$valorBoton <- renderText({  
    as.character(input$boton) 
  })
  # creamos un observador. No hay output, no genero nada para guardarlo en algun sitio.- Simplemente digo que se va a mostrar algo cuando pulse el boton
  observeEvent({
    input$boton  # manejador de mas de una linea -- 
    input$slider
  }, {
    showNotification(paste("Sirvete", input$boton, "copas")) # me muestra el mensaje en la pantalla como un pop up
  }, ignoreInit = TRUE) # once = TRUE solo lo ejecuta una vez # para q no te sirvan 0 copas # ignorenull, si hay algun valor nulo, pasa de el
  
  
}  # server solo nos imprime cosas, no hace que ocurra nada
# los eventos no son solo botones, es cualquier input que cambies
shinyApp(ui = ui, server = server) 