library(bslib)
library(shiny)
source("helpers.R")

# Cargar datos (archivo RDS con datos del censo por condado)
counties <- readRDS("data/counties.rds")

# Interfaz de usuario
ui <- page_sidebar(
  title = "censusVis",
  sidebar = sidebar(
    helpText("Create demographic maps with information from the 2010 US Census."),
    
    selectInput("var",
                label = "Choose a variable to display",
                choices = c("Percent White",
                            "Percent Black",
                            "Percent Hispanic",
                            "Percent Asian"),
                selected = "Percent White"),
    
    sliderInput("range",
                label = "Range of interest:",
                min = 0, max = 100, value = c(0, 100))
  ),
  
  card(
    card_header("Map Output"),
    card_body(plotOutput("map"))
  )
)

# Lógica del servidor
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var,
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var,
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend, min = input$range[1], max = input$range[2])
  })
}

shinyApp(ui = ui, server = server)
