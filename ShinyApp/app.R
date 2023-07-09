
ui <- fluidPage(

    # Application title
    titlePanel("Ranking del IMTM Municipios de la Región del Biobío"),
    fluidRow(
      column(4,
             selectInput("Provincia",
                         "Provincia:",
                         c("Todas",
                           unique(as.character(data$Provincia))))
      ),
      column(4,
             selectInput("FIGEM",
                         "FIGEM:",
                         c("Todas",
                           unique(as.character(data$FIGEM))))
      )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
)
server <- function(input, output) {
      
      # Filter data based on selections
      output$table <- DT::renderDataTable(DT::datatable({
        if (input$Provincia != "Todas") {
          data <- data(data$Provincia == input$Provincia,)
        }
        if (input$FIGEM != "Todas") {
          data <- data(data$FIGEM == input$FIGEM,)
        }
        data
      }))
      
    }

shinyApp(ui, server)     