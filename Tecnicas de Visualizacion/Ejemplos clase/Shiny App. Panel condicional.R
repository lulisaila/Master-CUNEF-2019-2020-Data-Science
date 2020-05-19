#30/10/2019
# Esto puede ser un ejercicio de un examen
# hay que ser capaz de hacer este ejercicio en 10 minutos 

library(shiny)

ui <- fluidPage(  
    titlePanel("Dashboard"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dist", label = ("Distribucion"), 
                    choices = list("Poisson" = "pois", "Gamma" = "gam")
                    ),
        conditionalPanel(condition = "input.dist == 'pois'",     # sirven para ocultar o mostrar informacion
          numericInput("num1", label = "Primer parametro", value = 1)),
        conditionalPanel(condition = "input.dist =='gam'", 
          numericInput("num2", label = "Segundo parametro", value = 1),  # ponemos dos parametros porq gamma necesita dos parametros
          numericInput("num3", label = "Tercer parametro", value = 1))  # esta parte es dinamica.
      ),  
      mainPanel(
        plotOutput("plotgraf")
      )
    )
  )


server <- function(input, output){
  output$plotgraf <- renderPlot({
    
    grafica <- if (input$dist == "pois"){
      rpois(1000, lambda = input$num1)
    } else if (input$dist == "gam") {
      rgamma(1000, input$num2, input$num3)
    }
    hist(grafica)
  })
}

shinyApp(ui = ui, server = server)
