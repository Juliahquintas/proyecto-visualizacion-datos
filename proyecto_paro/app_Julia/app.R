

library(tidyverse)
library(shiny)
library(plotly)
library(lubridate)

# Leer los CSV
df_contratos <- read_csv2("../data/datos_compactos/contratos_anuales_por_municipio.csv")
df_paro <- read_csv2("../data/datos_compactos/paro_anual_por_municipio.csv")


# Combinar
df <- df_contratos %>%
  full_join(df_paro, by=c("Código de CA","Comunidad Autónoma","Codigo Provincia",
                          "Provincia","Codigo Municipio","Municipio","anio"="anio"))

library(shiny)
library(tidyverse)
library(plotly)
library(readr)

# Leer y combinar CSV
df_contratos <- read_csv2("../data/datos_compactos/contratos_anuales_por_municipio.csv",
                          show_col_types = FALSE)
df_paro <- read_csv2("../data/datos_compactos/paro_anual_por_municipio.csv",
                     show_col_types = FALSE)

df <- df_contratos %>%
  full_join(df_paro, by=c("Código de CA","Comunidad Autónoma","Codigo Provincia",
                          "Provincia","Codigo Municipio","Municipio","anio"="anio"))

# Calcular media de contratos por año
df_summary <- df %>%
  mutate(`Total Contratos` = as.numeric(str_replace_all(`Total Contratos`, ",", ""))) %>%
  group_by(anio) %>%
  summarise(media_contratos = mean(`Total Contratos`, na.rm = TRUE)) %>%
  ungroup()

# App Shiny
ui <- fluidPage(
  titlePanel("Media de contratos por año (todos los municipios)"),
  mainPanel(
    plotlyOutput("bar_plot")
  )
)

server <- function(input, output, session) {
  output$bar_plot <- renderPlotly({
    p <- ggplot(df_summary, aes(x=factor(anio), y=media_contratos)) +
      geom_col(fill="steelblue") +
      labs(x="Año", y="Media de contratos") +
      theme_minimal()
    
    ggplotly(p, tooltip=c("x","y"))
  })
}

shinyApp(ui, server)


