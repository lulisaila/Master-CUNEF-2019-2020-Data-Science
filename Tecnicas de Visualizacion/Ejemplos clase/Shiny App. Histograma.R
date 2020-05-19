library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel('Histograma'),
  sidebarLayout(
    sidebarPanel(
      sliderInput('cuantos', label = '¿Cuantas muestras quieres?', value = 5, min = 0, max = 100),
      selectInput('tipo', label = 'Elige un tipo de muestra', choice = c('Gaussiana' = 'gauss', 'Uniforme' = 'unif')),
      actionButton('boton', label = 'Dale click para mostrar el resultado!')
    ),
    mainPanel(
      plotOutput('histograma')
    )
  )
)

server <- function(input, output){
  output$histograma <- renderPlot({
    input$boton
    rdistribucion <- ifelse(isolate(input$tipo) == "gauss", rnorm, runif) # isolate siempre se usa con el
    #input y se utiliza para que no se ejecute lo del renderplot hasta que se le de click al boton, por ejemplo,
    # pero funciona tambien con otros tipos de input
    hist(rdistribucion(isolate(input$cuantos)))
  })
  
}

shinyApp(ui = ui, server = server)