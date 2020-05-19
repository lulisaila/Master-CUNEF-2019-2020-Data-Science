# sidebarlayout

# con un boton que pone "Generar Datos"
# y un slider que pone "tamaño"

# crear un observador que atienda al boton e imprima una notificacion:
# creamos un data frame de dos columnas con rnorm
# el nuero de filas sera el numero aportado por el slider

# para ver qué funciona, haz dentro del observer un print(tuDataFrame)

# hacer que se grabe el dataset en un csv

# Hacer otro boton que se llame borrar y que un observador borre el fichero df.csv
# El comando para borrar se llama: file.remve("NOMBREDELFICHERO")

# https://pastebin.com/QGg2AKd6

# Hacer un tercer boton que ponga leer y un observador que lo observe que no haga nada todavia

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      actionButton("boton", "Generar Datos"),
      sliderInput("slider", "Tamaño", min = 0, max = 10, value = 3),
      actionButton("remove", "Borrar datos"),
      actionButton("leer", "Leer datos")
    ),
    mainPanel(
      tableOutput("ilusion")
    )
  )
)

server <- function(input, output){
  readingResult <- reactiveVal(NULL) # al inicio, el usuario aun no ha pulsado leer, asiq eu el valor inicial de esto va a ser un na 
  output$ilusion <- renderTable({
    
    readingResult() ## no olvidar el parentesis, es fundamental. Quiero q se guarde en la
    # cajita magica (valor reactivo)
    
  })
  
  observeEvent(input$boton, {
    tuDataFrame <- data.frame(x = rnorm(input$slider), x2 = rnorm(input$slider))
    print(tuDataFrame)
    write.csv(tuDataFrame, file = "tuDataFrame.csv")
  }, ignoreInit = TRUE) # el ignore hay q ponerlo siempre despues del observe
  
  observeEvent(input$remove,{
    file.remove("tuDataFrame.csv")
  }, ignoreInit = TRUE)
  
  observeEvent(input$leer,{
    datos <- read.csv("tuDataFrame.csv")
    print(datos)
    
    # para guardar en la cajita magica
    readingResult(datos)
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)  


# consejo, mientras menos observadores usemos mejor. Tenemos que ser capaces de hacer todo esto
# sin observadores, a menos que sea totalmente necesario y no haya otra forma 
# maria es tonta