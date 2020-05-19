# funcion h3 para crear cabeceras en tercer nivel

library(shiny)
library(ggplot2)

ui<-  fluidPage(  
    titlePanel("Mi primer dashboard"),
    sidebarLayout(  # todo lo que se llame layout significa distribucion
      sidebarPanel(
          selectInput("VarX", label = h3("Variable X"), 
                      choices = list("Desplazamiento" = "displ", "Cilindrada" = "cyl",
                                     "Millas ciudad" = "cty", "Millas autopista" = "hwy"),
                      selected = "cyl"),
          selectInput("VarY", label = h3("Variable Y"), 
                      choices = list("Desplazamiento" = "displ", "Cilindrada" = "cyl",
                                     "Millas ciudad" = "cty", "Millas autopista" = "hwy"),
                      selected = "cyl")
          
      ),  
      mainPanel(
        textOutput("Ejemplo"),
        plotOutput("PlotEjemplo")
      )
    )
  )


server <- function(input, output){
  output$Ejemplo <- renderText({
    paste("Este es el grafico con las variables ", input$VarX, " x ", input$VarY)
    
  })
  output$PlotEjemplo <- renderPlot({
    ggplot(mpg, aes_string(x = isolate(input$VarX), y = input$VarY)) + geom_point()  # con get tb va
  })
}

shinyApp(ui = ui, server = server)
