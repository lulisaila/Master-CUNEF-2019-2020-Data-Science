#---------------------------------------#
# PRACTICA 2: #
# LUCIA SAIZ LAPIQUE #
# TECNICAS DE VISUALIZACION #
#---------------------------------------#
  

library(shiny)
library(datasets)
library(ggplot2)
data('mpg')
data('mtcars')
data('cars')
data('anscombe')
data('trees')
data('Titanic')
# llamamos a los datos 

ui <- fluidPage(
  titlePanel("Practica 2"),
  sidebarLayout(
    sidebarPanel( # en primer lugar creamos dos select, uno para cada opcion que el usuario
      # debe elegir, y que apareceran en el panel lateral. Dentro de cada select introducimos 
      # todas las variables de las cuales el usuario podrá elegir.
      selectInput("var1", label = "Elige una variable entre las siguientes:", 
                    choices = list("mpg" = "mpg", "mtcars" = "mtcars", "cars" = "cars",
                                   "anscombe" = "anscombe","trees" = "trees", "Titanic" = "Titanic"),
                  selected = "mpg"),
      selectInput("var2", label = "Elige entre las siguientes opciones:",
                    choices = list("Mostrar summary" = "sum", "Mostrar primeras filas" = "head",
                                   "Mostrar últimas filas" = "tail"),
                  selected = "sum"),
      # con actionButton, creamos un boton que realizara la funcion que le especifiquemos despues
      # en el server
      actionButton("muestra", label = "Mostrar resultado")
      ),
    mainPanel(
      verbatimTextOutput("ResultadoFinal")
    )
  )
)


server <- function(input, output) {
  output$ResultadoFinal <- renderPrint({  # queremos que nos exponga el texto que ha solicitado el usuario
    input$muestra  # pero solo cuando le de al boton de mostrar el resultado

    var1 <- isolate(input$var1)  # tenemos que airlar las variables, ya que, si no, R no
    # es capaz de interpretarlas
    var2 <- isolate(input$var2) 
    
    eleccion <- switch(var1, "mpg" = mpg, "mtcars" = mtcars, "cars" = cars,
                       "anscombe" = anscombe,"trees" = trees, "Titanic" = Titanic)  # la funcion
    # switch identifica el primer valor introducido con el que ha elegido el usuario y 
    # devuelve lo que ha escogido el usuario
    
    resultado <- if(input$var2 == "sum"){
      summary(eleccion)
    } else if(input$var2 == "head"){
      head(eleccion)
    } else if(input$var2 == "tail"){
      tail(eleccion)
    }
    # creamos un if con dos else para que las tres opciones queden correctamente englobadas
    # y que si el usuario elige la primera opcion, la segunda o la tercera, salga el resultado 
    # correspondiente. Con el objeto que obtuvimos en el switch y que guardamos en "eleccion"
    # hacemos las condicionales con eso. 
    print(resultado) 
  }) 
}

shinyApp(ui = ui, server = server)



