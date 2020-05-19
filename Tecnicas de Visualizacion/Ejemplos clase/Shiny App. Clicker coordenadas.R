library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Ejemplo clicker"),
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      plotOutput("grafica", click = "eventoClick"), 
      verbatimTextOutput("resultadoClick") 
    )
  )
)

server <- function(input, output){
  observeEvent(input$eventoClick, {
    print("Has pulsado la grafica")
  })
  
  output$resultadoClick <- renderText({
    as.character(nearPoints(mtcars, input$eventoClick)) # queremos añadir un punto al dataset
  })
  
  output$grafica <- renderPlot({
    ggplot(mtcars) + geom_point(aes(x = disp, y = hp))
  }) 
  
}

shinyApp(ui, server)  
