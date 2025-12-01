# Título: Visualizador de Municipios de España con Coordenadas CSV
# Autor: Gemini (Modelo de Lenguaje Grande)
# Descripción: Esta aplicación carga los datos y coordenadas (LONGITUD_ETRS89 y LATITUD_ETRS89)
# de tu archivo CSV 'MUNICIPIOS.csv' y los visualiza en un mapa Leaflet interactivo.

# --- 1. Carga de Librerías Necesarias ---
library(shiny)
library(leaflet) # Para mapas interactivos
library(readr)   # Para leer archivos CSV
library(dplyr)   # Para manipulación de datos
library(sf)      # Para trabajar con datos espaciales

# --- 2. Preparación de Datos ---

# RUTA DEL ARCHIVO CSV DE MUNICIPIOS
ruta_csv_municipios <- "mapData/MUNICIPIOS.csv"
# RUTA DEL ARCHIVO CSV DE PROVINCIAS PARA MAPEO DE CCAA
ruta_csv_provincias <- "mapData/PROVINCIAS.csv"

# Cargar y limpiar el CSV de PROVINCIAS
tryCatch({
    # Cargar la tabla de mapeo Provincia -> CCAA
    mapeo_provincias <- read_delim(
        ruta_csv_provincias,
        delim = ";",
        col_types = cols(.default = "c"),
        locale = locale(encoding = "WINDOWS-1252")
    ) %>%
        # Seleccionar y renombrar las columnas clave para la unión
        select(COD_PROV, PROVINCIA_REF = PROVINCIA, CCAA = COMUNIDAD_AUTONOMA)
        
}, error = function(e) {
    message("Error al cargar o procesar el CSV de provincias: ", e$message)
    stop("Deteniendo la app. Asegúrate de que 'PROVINCIAS.csv' existe.")
})


# Cargar y limpiar el CSV de MUNICIPIOS
tryCatch({
    # Usamos read_delim del paquete readr para ser más robustos con el delimitador ";"
    datos_municipios_crudo <- read_delim(
        ruta_csv_municipios, 
        delim = ";", 
        col_types = cols(.default = "c"), 
        locale = locale(encoding = "WINDOWS-1252")
    )
    
    # Preparación de datos (Renombrado, limpieza y unión)
    datos_mapa <- datos_municipios_crudo %>%
        
        # Renombrar columnas y preparar datos
        rename(
            nombre = NOMBRE_ACTUAL,
            longitud = LONGITUD_ETRS89,
            latitud = LATITUD_ETRS89,
            cod_geo = COD_GEO,
            COD_PROV = COD_PROV # Usamos esta columna como clave de unión
        ) %>%
        
        # Convertir coordenadas y población
        mutate(
            # Reemplazar comas por puntos para la conversión numérica (estándar en R)
            longitud = as.numeric(gsub(",", ".", longitud)),
            latitud = as.numeric(gsub(",", ".", latitud)),
            Poblacion = as.numeric(gsub(",", ".", POBLACION_MUNI))
        ) %>%
        
        # --- ASIGNACIÓN DE CCAA MEDIANTE UNIÓN (JOIN) ---
        # Unir con la tabla de mapeo de provincias usando COD_PROV
        left_join(mapeo_provincias, by = "COD_PROV") %>%
        
        # Crear variable de Categoría para visualización
        mutate(
            Categoria = factor(
                case_when(
                    Poblacion > 50000 ~ "Grande (>50k hab)",
                    Poblacion > 10000 ~ "Mediana (10k-50k hab)",
                    TRUE ~ "Pequeña (<10k hab)"
                )
            )
        ) %>%
        
        # Seleccionar solo las columnas finales que nos interesan
        # Usamos la columna PROVINCIA del CSV de municipios y la CCAA del archivo de mapeo
        select(cod_geo, nombre, longitud, latitud, Poblacion, Categoria, PROVINCIA, CCAA) %>%
        
        # Eliminar filas con coordenadas o nombre faltantes
        filter(!is.na(longitud), !is.na(latitud), !is.na(nombre))
    
    # Convertir el data frame a objeto SF (Spatial Feature)
    datos_mapa_sf <- datos_mapa %>%
        st_as_sf(coords = c("longitud", "latitud"), crs = 4326) 
    
    # Datos globales para el selector
    opciones_ccaa <- unique(datos_mapa$CCAA)
    opciones_provincia <- unique(datos_mapa$PROVINCIA)
    
}, error = function(e) {
    message("Error al cargar o procesar el CSV de municipios: ", e$message)
    stop("Deteniendo la app. Asegúrate de que 'MUNICIPIOS.csv' existe y que el formato de coordenadas es correcto.")
})


# --- 3. Definición de la Interfaz de Usuario (UI) ---
ui <- fluidPage(
    # Estilo básico
    tags$head(
        tags$style(HTML("
            .leaflet-container {
                height: 80vh; /* Ajustado para dejar espacio a los selectores */
                border-radius: 8px;
                box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            }
            .header-title {
                text-align: center;
                color: #004d99;
                padding-bottom: 10px;
            }
        "))
    ),
    
    div(class = "header-title",
        h1("Mapa Interactivo de Municipios de España por Agrupación Administrativa")
    ),
    
    # Controles de agrupación y filtrado
    fluidRow(
        column(4, 
               selectInput(
                   "ccaa_select", "Filtrar por Comunidad Autónoma:", 
                   choices = c("Todas" = "", sort(opciones_ccaa)),
                   selected = ""
               )
        ),
        column(4, 
               # El selector de Provincia ahora es dependiente
               selectInput(
                   "provincia_select", "Filtrar por Provincia:", 
                   choices = c("Todas" = ""), # Inicialmente vacío, se rellena en el servidor
                   selected = ""
               )
        ),
        column(4, 
               actionButton("reset_map", "Mostrar Mapa de España Completo", class = "btn-primary", style="margin-top: 25px;")
        )
    ),
    
    # Salida del mapa Leaflet
    leafletOutput("mapa_municipios")
)


# --- 4. Definición del Servidor (Server Logic) ---
server <- function(input, output, session) {

    # 4.1. Filtro de provincias reactivo (depende de la CCAA seleccionada)
    provincias_filtradas <- reactive({
        if (input$ccaa_select == "") {
            # Si no hay CCAA seleccionada, devuelve todas las provincias
            return(datos_mapa$PROVINCIA)
        } else {
            # Si hay CCAA seleccionada, devuelve solo las provincias de esa CCAA
            datos_mapa %>% 
                filter(CCAA == input$ccaa_select) %>% 
                pull(PROVINCIA) %>% 
                unique()
        }
    })
    
    # 4.2. Observador para actualizar el selector de provincias
    observeEvent(provincias_filtradas(), {
        # Obtiene las opciones de provincia actualizadas
        opciones_prov <- sort(provincias_filtradas())
        
        # Actualiza el selector de provincia
        updateSelectInput(
            session, 
            "provincia_select", 
            choices = c("Todas" = "", opciones_prov),
            # Asegura que la selección actual siga siendo válida (o se resetee)
            selected = if(input$provincia_select %in% opciones_prov) input$provincia_select else ""
        )
    })
    
    # 4.3. Filtrado de datos reactivo para el mapa
    datos_filtrados <- reactive({
        datos <- datos_mapa_sf
        
        # Filtrar por CCAA si está seleccionada
        if (input$ccaa_select != "") {
            datos <- datos %>% filter(CCAA == input$ccaa_select)
        }
        
        # Filtrar por Provincia si está seleccionada
        if (input$provincia_select != "") {
            datos <- datos %>% filter(PROVINCIA == input$provincia_select)
        }
        
        return(datos)
    })
    
    # 4.4. Lógica para resetear el mapa
    observeEvent(input$reset_map, {
        updateSelectInput(session, "ccaa_select", selected = "")
        updateSelectInput(session, "provincia_select", selected = "")
        # Recentra el mapa a la vista general de España al resetear
        leafletProxy("mapa_municipios") %>%
            setView(lng = -3.70, lat = 40.41, zoom = 6)
    })


    # Paleta de colores (calculada fuera del renderLeaflet para ser eficiente)
    pal <- colorFactor(
        palette = c("#cc0000", "#ffcc00", "#00b300"), 
        domain = datos_mapa_sf$Categoria
    )
    
    # 4.5. Renderizado del mapa Leaflet (Mapa base estático)
    output$mapa_municipios <- renderLeaflet({
        leaflet() %>%
            addTiles(
                options = providerTileOptions(minZoom = 5)
            ) %>%
            # Centrar inicialmente en el centro de España
            setView(lng = -3.70, lat = 40.41, zoom = 6) %>%
            # Añadir la leyenda al mapa base estático
            addLegend(pal = pal, values = datos_mapa_sf$Categoria, opacity = 0.9, title = "Población Municipal",
                      position = "bottomright")
    })
    
    # 4.6. Proxy para actualizar los marcadores dinámicamente (AHORA SIEMPRE CON CLUSTERING GEOGRÁFICO)
    observe({
        
        # Ya no se comprueba la vista de "full Spain", siempre se usan los datos filtrados
        datos_actuales <- datos_filtrados()
        
        proxy <- leafletProxy("mapa_municipios") %>%
            # Limpiar todos los marcadores y agrupadores anteriores
            clearMarkers() %>%
            clearMarkerClusters()
            
        # Recalcular las etiquetas de popup para los municipios actuales
        labels_actuales <- paste0(
            "<strong>", datos_actuales$nombre, " (", datos_actuales$PROVINCIA, ")", "</strong><br/>",
            "Cód. GEO: ", datos_actuales$cod_geo, "<br/>",
            "Población: ", format(datos_actuales$Poblacion, big.mark = ".", decimal.mark = ","), "<br/>",
            "Categoría: ", datos_actuales$Categoria
        ) %>% lapply(htmltools::HTML)

        proxy %>%
            # Añadir los marcadores con CLUSTERING GEOGRÁFICO estándar
            addCircleMarkers(
                data = datos_actuales,
                radius = ~log(Poblacion + 1) * 2,
                color = ~pal(Categoria),
                stroke = FALSE, 
                fillOpacity = 0.8,
                popup = labels_actuales,
                clusterOptions = markerClusterOptions(), # Clustering geográfico SIEMPRE activo
                layerId = ~cod_geo 
            )
            
        # Ajustar el zoom del mapa automáticamente al área filtrada si hay datos
        if (nrow(datos_actuales) > 0) {
            # Calcula los límites para el ajuste de la vista
            bounds <- st_bbox(datos_actuales)
            proxy %>%
                fitBounds(
                    lng1 = bounds["xmin"], lat1 = bounds["ymin"],
                    lng2 = bounds["xmax"], lat2 = bounds["ymax"]
                )
        }
    })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)