library(shiny)


ui <- fluidPage(
    titlePanel("Lucia Saiz Lapique, Ejercicio 3"),
    sidebarLayout(
        sidebarPanel(
            actionButton('guardar', 'Guardar CSV'),
            actionButton('borrar', 'Borrar CSV'),
            actionButton('leer', 'Leer CSV')
        ),
        mainPanel(
            tableOutput('tabla')
        )
    )
)


server <- function(input, output) {
    
    observeEvent(input$guardar,{
        x = rnorm(20)
        y = rnorm(20)
        datos = as.data.frame(x)
        datos['y'] = y
        print(datos)
        write.csv(datos, file = 'Datos_Aleatorios.csv')
    }, ignoreInit = TRUE)
    
    observeEvent(isolate(input$borrar),{
        file.remove('Datos_Aleatorios.csv')
    }, ignoreInit = TRUE)
    
    observeEvent(isolate(input$leer),{
        datos <- read.csv('Datos_Aleatorios.csv')
        print(datos)
        output$tabla <- renderTable({  # para llamar a la tabla que queremos mostrar
        datos
        })
    }, ignoreInit = TRUE)
}


shinyApp(ui = ui, server = server)
