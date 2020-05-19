library(shiny)
library(tidyverse)
data(mpg)
force(mpg)


ui <- fluidPage(
    titlePanel("Lucia Saiz Lapique, Ejercicio 1"),  ## header
    sidebarLayout(
        sidebarPanel(
            selectInput(
                'color', label = 'Elige el color que quieras', choices = c('Grey' = 'grey', 'Red' = 'red', 
                                                                    'Blue' = 'blue')
            )
        ),
        mainPanel(
           plotOutput("barPlot")  # introducimos en la parte de la derecha un grafico de barras que viene explicado
           # en el server
        )
    )
)


server <- function(input, output) {

    output$barPlot <- renderPlot({
        ggplot(mpg, aes(x = cyl)) +
            geom_bar(fill = input$color)  
    })
}


shinyApp(ui = ui, server = server)  
