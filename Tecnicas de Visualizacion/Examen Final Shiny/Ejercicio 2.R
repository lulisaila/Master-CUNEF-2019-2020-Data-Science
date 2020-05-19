library(shiny)
library(ggplot2)


ui <- fluidPage(
    titlePanel('Lucia Saiz Lapique, Ejercicio 2'),
    sidebarLayout(
        sidebarPanel(
            sliderInput('elem', 'Numero de elementos', value = 20, min = 1, max = 50, step = 1),  # el slider
            # nos permite elegir el numero de elementos que queremos. El step sirve para elegir el salto de 
            # un numero al siguiente. podemos omitir la palabra label
            actionButton('boton', 'Nuevo Dataset')  # no necesita mas paramentros. Podemos omitir la palabra label
        ),
        mainPanel(
           plotOutput("pointPlot")  # que lo que salga a la derecha sea un grafico
        )
    )
)


server <- function(input, output) {
    observeEvent(input$boton,{  # para tener en cuenta el boton y que con el se ejecute otra cosa, usamos el 
        # observe event
        output$pointPlot <- renderPlot({  # como output nos interesa el grafico
            x = rnorm(isolate(input$elem))  # isolate necesario para que no se cambie el dataset hasta que
            # el usuario presione el boton
            y = rnorm(isolate(input$elem))
            datos = as.data.frame(x)
            datos['y'] = y
            ggplot(datos) +
                geom_point(aes(x = x, y = y))
        })
    })
    
}


shinyApp(ui = ui, server = server)