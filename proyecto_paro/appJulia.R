
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinyjs)
library(scales)
library(leaflet)
library(sf)
library(mapSpain)
library(DT)

# --- Configuración Inicial ---
anios <- 2010:2025
anios_titulos <- paste(min(anios), max(anios), sep = "–")
Pmin = 0.02
Pmax = 0.98


source("preprocessing.R")

# Carga de datos inicial
res <- descargar_datasets_sepe(anios = anios, dir_data = "data")
# Descargamos población (fundamental para el nuevo cálculo del mapa)
res <- descargar_y_procesar_poblacion(codigos_ine = 2854:2908, dir_data = "data", anio_min = 2010, anio_max = 2025)


# Función de lectura ROBUSTA (Latin1)
leer_sepe_csv <- function(ruta) {
  if(!file.exists(ruta)) return(NULL)
  tryCatch({
    df <- read.csv(ruta, sep = ";", stringsAsFactors = FALSE, fileEncoding = "Latin1", check.names = FALSE)
    if(ncol(df) <= 1) {
      df <- read.csv(ruta, sep = ",", stringsAsFactors = FALSE, fileEncoding = "Latin1", check.names = FALSE)
    }
    return(df)
  }, error = function(e) {
    read.csv(ruta, stringsAsFactors = FALSE)
  })
}


# MAPEO DE COMUNIDADES AUTONOMAS

# mapa_ca <- tibble::tibble(
#   cod_ca = c("01","02","03","04","05","06","07","08","09","10",
#              "11","12","13","14","15","16","17","18","19"),
#   ca = c(
#     "Andalucía","Aragón","Asturias","Illes Balears","Canarias",
#     "Cantabria","Castilla y León","Castilla-La Mancha","Cataluña",
#     "Comunitat Valenciana","Extremadura","Galicia","Madrid",
#     "Murcia","Navarra","País Vasco","La Rioja","Ceuta","Melilla"
#   )
# )



###################################################################
### UI (INTERFAZ DE USUARIO) ######
###################################################################

ui <- fluidPage(
  useShinyjs(),
  titlePanel(paste("SEPE — Análisis Provincial y Geográfico (", anios_titulos, ")")),
  
  sidebarLayout( 
    
    # PANEL DONDE ESTAN LOS CONTROLES
    sidebarPanel(
      h4("Configuración Global"),
      condition = "input.tabs_main != 'tab_relacion'",
      selectInput("metrica_sel", "Indicador:",
                  choices = c("Paro Registrado" = "paro",
                              "Contratos Registrados" = "contratos",
                              "Demandantes de Empleo" = "dtes"),
                  selected = "contratos"),
      
      # --- FILTROS PESTAÑA 1 EVOLUCION TEMPORAL ---
      conditionalPanel(
        condition = "input.tabs_main == 'tab_grafico'",
        h4("Filtros Evolución"),
        selectInput("prov1", "Provincia Principal:", choices = NULL), 
        selectInput("prov2", "Comparar con (Opcional):", choices = NULL),
        
        sliderInput("rango_anios", "Rango de Años:",
                    min = min(anios), max = max(anios),
                    value = c(min(anios), max(anios)),
                    step = 1, sep = ""),
        
        radioButtons("granularidad", "Escala Temporal:",
                     choices = c("Anual (Media)" = "anual", 
                                 "Mensual (Detalle)" = "mensual"),
                     selected = "mensual"),
        
        checkboxInput("relativo", "Ver distribución % (Áreas Apiladas)", value = FALSE)
      ),
      
      # --- FILTROS PESTAÑA 2 MAPA ---
      conditionalPanel(
        condition = "input.tabs_main == 'tab_mapa'",
        h4("Filtros Mapa"),
        
        # 1. Granularidad propia del mapa
        radioButtons("granularidad_mapa", "Escala Temporal:",
                     choices = c("Anual" = "anual", 
                                 "Mensual" = "mensual"),
                     selected = "anual"),
        
        # 2. Slider Dinámico (Anual o Mensual con Play)
        uiOutput("ui_slider_mapa_dinamico"),
        
        selectInput("sector_mapa", "Sector a visualizar:", 
                    choices = c("Agricultura", "Industria", "Construcción", "Servicios", "Sin empleo Anterior"),
                    selected = "Servicios"),
        
        helpText("Nota: El valor relativo (%) se calcula sobre la POBLACIÓN TOTAL de la provincia.")
      ),


      # --- FILTROS PESTAÑA 3 COMPARACION CONTRATOS-PARO ---
      conditionalPanel(
        condition = "input.tabs_main == 'tab_relacion'",
        h4("Filtros Relación Paro–Contratos"),
        # En el sidebarPanel, dentro del conditionalPanel de 'tab_relacion'
        selectInput("ccaa_rel", "Comunidad Autónoma:", choices = NULL),
        selectInput("sector_rel", "Sector:", choices = c("Agricultura", "Industria", "Construcción", "Servicios")),
        sliderInput("desfase_anios", "Desfase entre Paro y Contratos (años):", min = 0, max = 5, value = 1, step = 1)
      ),


      conditionalPanel(
        condition = "input.tabs_main == 'tab_desigualdades'",
        h4("Filtros Desigualdades Demográficas"),
        selectInput("ccaa_des", "Comunidad Autónoma:", choices = NULL),
        selectInput("metrica_des", "Métrica:", choices = c("Paro", "Contratos")),
        checkboxGroupInput("grupo_des", "Variables:", choices = c("Edad", "Género", "Tipo de Contrato"), selected = c("Edad", "Género"))
      ),


      width = 3
    ),
    
    # PANEL PRINCIPAL, DONDE ESTAN LAS VISUALIZACIONES
    mainPanel(
      tabsetPanel(id = "tabs_main",
                  
        tabPanel("Evolución Temporal", value = "tab_grafico",
                 br(),
                 h3(textOutput("titulo_grafico")),
                 plotOutput("plot_sectores", height = "600px")
        ),
        
        tabPanel("Mapa Geográfico", value = "tab_mapa",
                 br(),
                 h3(textOutput("titulo_mapa")),
                 leafletOutput("mapa_leaflet", height = "650px")
        ),

        tabPanel(
          "Relación Paro–Contratos",
            value = "tab_relacion",
            
            br(),
            h3("Relación entre desempleo y contratación sectorial"),
            p("Esta sección analiza la relación entre el nivel de desempleo en un sector"),
            p("y la contratación generada en ese mismo sector en años posteriores."),
            p("Se utilizarán gráficos tipo bubble chart con filtros por comunidad autónoma, sector y desfase temporal."),
            plotOutput("plot_relacion", height = "600px")
        ),

        tabPanel(
            "Desigualdades Demográficas",
            value = "tab_desigualdades",
            br(),
            h3("Distribución y desigualdades del empleo"),
            selectInput("prov1", "Provincia:", choices = NULL),
            selectInput("metrica_des", "Métrica:", choices = c("Paro","Contratos")),
            checkboxGroupInput("grupo_des", "Variables:", choices = c("Edad","Género"), selected = c("Edad","Género")),
            plotOutput("plot_des_sexo", height = "300px"),
            plotOutput("plot_des_edad", height = "300px"),
            plotOutput("plot_des_comp", height = "300px")   
        ),

        tabPanel(
            "Explorador de Datos", value = "tab_tabla",
            br(),
            h3("Tabla de Datos Brutos"),
            p("Utiliza los cuadros de búsqueda bajo cada columna para filtrar los datos específicos."),
            downloadButton("descargar_datos", "Descargar CSV"), # Botón opcional para exportar
            br(), br(),
            DTOutput("tabla_maestra")
        )

      ),
      br(),
      verbatimTextOutput("debug_info"),
      width = 9
    )
  )
)



###################################################################
### SERVER (LÓGICA DE LA APP) ######
###################################################################


server <- function(input, output, session) {

  # --- 0. Carga de Población ---
  poblacion_data <- reactive({
    archivos <- list.files("data", pattern = "poblacion|padron", full.names = TRUE, recursive = TRUE)
    if (length(archivos) == 0) return(NULL)
    
    df_pob <- leer_sepe_csv(archivos[1])
    if (is.null(df_pob)) return(NULL)
    if (!all(c("anio", "Cod provincia", "poblacion_total") %in% names(df_pob))) return(NULL)
    
    df_pob %>%
      select(anio, cod_prov = `Cod provincia`, poblacion_total) %>%
      mutate(cod_prov = as.integer(cod_prov), anio = as.integer(anio))
  })

  # --- 1. Carga de Geometrías ---
  mapa_spain_sf <- reactive({
    tryCatch({
      geo <- esp_get_prov(moveCAN = TRUE)
      geo <- st_transform(geo, crs = 4326) 
      geo %>% mutate(cod_prov_int = as.integer(cpro))
    }, error = function(e) return(NULL))
  })
  



  


    agregador_sectores_provincial <- function(df_raw, tipo_metrica) {

        if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)

        # --- Limpieza nombres ---
        names(df_raw) <- gsub("\\s+", " ", names(df_raw))

        # --- Fecha ---
        if (!"Cod mes" %in% names(df_raw)) return(NULL)

        df_raw <- df_raw %>%
            mutate(
                mes_cod = as.character(`Cod mes`),
                anio = as.integer(substr(mes_cod, 1, 4)),
                mes  = as.integer(substr(mes_cod, 5, 6))
            )

        # --- Provincia ---
        if ("Cod provincia" %in% names(df_raw)) {
            df_raw$cod_prov <- as.integer(df_raw$`Cod provincia`)
        } else {
            df_raw$cod_prov <- NA
        }

        # --- Prefijo según métrica ---
        prefijo <- switch(
            tipo_metrica,
            "paro" = "Paro",
            "contratos" = "Contratos",
            "dtes" = "Dtes Empleo"
        )

        # --- Columnas automáticas (empiezan por Paro / Contratos / Dtes) ---
        cols_prefijo <- names(df_raw)[startsWith(names(df_raw), prefijo)]

        # --- Columnas TOTAL que NO siguen patrón ---
        cols_totales <- intersect(
            names(df_raw),
            c("Paro", "Contratos", "Total Paro", "Total Contratos")
        )

        cols_interes <- unique(c(cols_prefijo, cols_totales))
        if (length(cols_interes) == 0) return(NULL)

        # --- PASO CLAVE: PASO A FORMATO LARGO ---
        df_largo <- df_raw %>%
            select(anio, mes, cod_prov, Provincia, `Comunidad Aut`, all_of(cols_interes)) %>%
            pivot_longer(
                cols = all_of(cols_interes),
                names_to = "variable",
                values_to = "valor"
            )

        # --- Limpieza texto ---
        df_largo <- df_largo %>%
            mutate(
                variable = trimws(variable),
                variable = gsub(paste0("^", prefijo), "", variable)
            )

        # --- Etiquetas semánticas (NO destructivas) ---
        df_largo <- df_largo %>%
            mutate(
                tipo = tipo_metrica,

                es_total = variable %in% c("", "Total", "total Paro Registrado", "Total Contratos"),

                sector = case_when(
                    grepl("Agricultura", variable) ~ "Agricultura",
                    grepl("Industria", variable) ~ "Industria",
                    grepl("Construcción", variable) ~ "Construcción",
                    grepl("Servicios", variable) ~ "Servicios",
                    grepl("Sin empleo", variable) ~ "Sin empleo Anterior",
                    TRUE ~ "Otros"
                ),

                genero = case_when(
                    grepl("Mujer", variable, ignore.case = TRUE) ~ "Mujer",
                    grepl("Hombre", variable, ignore.case = TRUE) ~ "Hombre",
                    TRUE ~ "Total"
                ),

                edad = case_when(
                    grepl("Menor", variable) ~ "<25",
                    grepl("25", variable) & grepl("44", variable) ~ "25-44",
                    grepl("45", variable) ~ "45+",
                    TRUE ~ "Total"
                )
            )

        # --- Agregación mínima ---
        df_largo %>%
            group_by(anio, mes, cod_prov, Provincia, `Comunidad Aut`,
                    tipo, sector, genero, edad, es_total) %>%
            summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
    
    }








    # --- Datos Desigualdades ---
  datos_desigualdad <- reactive({
    df <- datos_base()
    req(df)
  
    # Filtrar por métrica seleccionada en la pestaña (Paro o Contratos)
    metrica <- ifelse(input$metrica_des == "Paro", "paro", "contratos")
     df <- df %>% filter(metrica == metrica)
  
     # Filtrar por CCAA si se ha seleccionado
    if (!is.null(input$ccaa_des) && input$ccaa_des != "") {
      # Suponiendo que tienes un mapping cod_ca -> provincias
      cod_prov_ccaa <- mapa_ca %>% filter(cod_ca == input$ccaa_des) %>% pull(cod_ca)
      df <- df %>% filter(`Cod CA` == input$ccaa_des)
    }
  
    # Filtrar por variables seleccionadas (edad/género/tipo de contrato)
    if ("Edad" %in% input$grupo_des) {
      df <- df %>% filter(!edad %in% c("Total"))
    }
  
    if ("Género" %in% input$grupo_des) {
      df <- df %>% filter(!genero %in% c("Total"))
    }
  
    # Para contratos: si no hay desagregación por edad, dejar solo Total
    if (metrica == "contratos" & all(df$edad == "Total")) {
      df$edad <- "Total"
    }
  
    df
  })







  # --- 3. Carga de Datos Base ---
  datos_base <- reactive({
    lista_final <- list()
    withProgress(message = 'Cargando datos...', value = 0, {
      n_anios <- length(anios)
      for (i in seq_along(anios)) {
        ano <- anios[i]
        setProgress(i/n_anios, detail = paste("Año", ano))
        
        r_paro  <- list.files("data", pattern = paste0("Paro.*", ano, ".*csv"), full.names = TRUE, recursive = TRUE)
        r_contr <- list.files("data", pattern = paste0("Contratos.*", ano, ".*csv"), full.names = TRUE, recursive = TRUE)
        r_dtes  <- list.files("data", pattern = paste0("Dtes.*", ano, ".*csv"), full.names = TRUE, recursive = TRUE)
        
        if(length(r_paro) > 0) {
          d_list <- lapply(r_paro, leer_sepe_csv) 
          d <- bind_rows(d_list) 
          r <- agregador_sectores_provincial(d, "paro")
          if(!is.null(r)) { r$metrica <- "paro"; lista_final[[paste("p", ano)]] <- r }
        }
        if(length(r_contr) > 0) {
          d_list <- lapply(r_contr, leer_sepe_csv)
          d <- bind_rows(d_list)
          r <- agregador_sectores_provincial(d, "contratos")
          if(!is.null(r)) { r$metrica <- "contratos"; lista_final[[paste("c", ano)]] <- r }
        }
        if(length(r_dtes) > 0) {
          d_list <- lapply(r_dtes, leer_sepe_csv)
          d <- bind_rows(d_list)
          r <- agregador_sectores_provincial(d, "dtes")
          if(!is.null(r)) { r$metrica <- "dtes"; lista_final[[paste("d", ano)]] <- r }
        }
      }
    })
    res <- bind_rows(lista_final)
    if(!is.null(res)) {
      res$anio <- as.integer(res$anio)
      res$mes <- as.integer(res$mes)
      res$fecha <- as.Date(paste(res$anio, res$mes, "01", sep="-"))
    }
    res %>% arrange(fecha)
  })



  # --- Updates de UI ---
  observe({
    df <- datos_base()
    req(df)
    provs <- sort(unique(df$Provincia))
    sel_def <- if("Madrid" %in% provs) "Madrid" else provs[1]
    updateSelectInput(session, "prov1", choices = provs, selected = sel_def)
    updateSelectInput(session, "prov2", choices = c("Ninguna" = "", provs), selected = "")
  })

  observe({
      df <- datos_base()
      req(df)

      comunidades <- sort(unique(df$`Comunidad Aut`))

      updateSelectInput(
          session, "ccaa_rel",
          choices = c("Todas" = "ALL", comunidades),
          selected = "ALL"
      )
  })








  
  # --- UI DINÁMICA: SLIDER MAPA ---
  output$ui_slider_mapa_dinamico <- renderUI({
    req(input$granularidad_mapa)
    df <- datos_base()
    req(df) 
    if (nrow(df) == 0) return(NULL)
    
    if (input$granularidad_mapa == "anual") {
      anios_disp <- sort(unique(df$anio))
      if(length(anios_disp) == 0) return(NULL)
      sliderInput("tiempo_mapa_sel", "Año:",
                  min = min(anios_disp), max = max(anios_disp),
                  value = max(anios_disp), step = 1, sep = "",
                  animate = animationOptions(interval = 2000, loop = FALSE))
    } else {
      fechas_disp <- sort(unique(df$fecha))
      if(length(fechas_disp) == 0) return(NULL)
      sliderInput("tiempo_mapa_sel", "Mes y Año:",
                  min = min(fechas_disp), max = max(fechas_disp),
                  value = max(fechas_disp), timeFormat = "%m/%Y", step = 30, 
                  animate = animationOptions(interval = 1000, loop = FALSE))
    }
  })

  # --- 4. Transformación Gráfico Lineal ---
  datos_grafico <- reactive({
    req(input$prov1, input$metrica_sel)
    df <- datos_base()
    req(df)

    provs_sel <- c(input$prov1)
    if (input$prov2 != "") provs_sel <- c(provs_sel, input$prov2)
    
    df_filt <- df %>% 
      filter(metrica == input$metrica_sel, Provincia %in% provs_sel,
             anio >= input$rango_anios[1], anio <= input$rango_anios[2], sector != "Total", sector != "Otros")
    
    if (input$granularidad == "anual") {
      df_agrupado <- df_filt %>%
        group_by(anio, Provincia, sector, metrica) %>%
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), .groups = "drop")
    } else {
      df_agrupado <- df_filt 
    }
    
    # if (input$relativo) {
    #   df_agrupado <- df_agrupado %>%
    #     group_by(fecha, Provincia) %>%
    #     mutate(total_momento = sum(valor, na.rm = TRUE), valor_final = (valor / total_momento) * 100) %>%
    #     ungroup()

    if (input$relativo) {
        df_agrupado <- df_agrupado %>%
            group_by(fecha, Provincia) %>%
            mutate(
            total_momento = sum(valor[sector != "Total"], na.rm = TRUE),
            valor_final = ifelse(
                sector == "Total",
                100,
                (valor / total_momento) * 100
            )
        ) %>%
        ungroup()
    } else {
      df_agrupado$valor_final <- df_agrupado$valor
    }
    df_agrupado
  })
  




  # --- 5. Transformación para MAPA (Con limpieza de NAs) ---
  datos_para_mapa <- reactive({
    req(input$tiempo_mapa_sel, input$sector_mapa, input$metrica_sel, input$granularidad_mapa)
    df <- datos_base()
    df_pob <- poblacion_data() 
    req(df, df_pob)
    
    # 1. Filtro Temporal y Agregación
    if (input$granularidad_mapa == "anual") {
      anio_target <- as.integer(input$tiempo_mapa_sel)
      
      df_filt <- df %>% 
        filter(metrica == input$metrica_sel, anio == anio_target) %>%
        group_by(cod_prov, Provincia, sector) %>%
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), .groups = "drop")
      
      anio_join <- anio_target 
      
    } else {
      fecha_target <- as.Date(input$tiempo_mapa_sel)
      anio_join <- as.integer(format(fecha_target, "%Y")) 
      
      fechas_disponibles <- unique(df$fecha)
      fecha_cercana <- fechas_disponibles[which.min(abs(fechas_disponibles - fecha_target))]
      
      df_filt <- df %>% filter(metrica == input$metrica_sel, fecha == fecha_cercana)
    }

    # 2. Filtro Sector
    df_sector <- df_filt %>%
      filter(sector == input$sector_mapa) %>%
      group_by(cod_prov, Provincia) %>%
      summarise(valor_abs = sum(valor, na.rm=TRUE), .groups="drop")
    
    # 3. Cruce Población y Limpieza NA
    df_pob_anio <- df_pob %>% filter(anio == anio_join)
    
    df_final <- left_join(df_sector, df_pob_anio, by = "cod_prov") %>%
      mutate(
        # SI VALOR_ABS es NA (no hay contratos), lo ponemos a 0
        valor_abs = tidyr::replace_na(valor_abs, 0),
        # Calculamos relativo. Si pob es NA o 0, el resultado será NA o Inf, lo trataremos después
        valor_rel = (valor_abs / poblacion_total) * 100
      )
    
    return(df_final)
  })

  # --- 6. Render Plots ---
  output$titulo_grafico <- renderText({
    txt_metrica <- switch(input$metrica_sel, "paro" = "Paro", "contratos" = "Contratos", "dtes" = "Demandantes")
    txt_tipo <- if (input$relativo) "(Distribución %)" else "(Valores Absolutos)"
    paste0("Evolución ", txt_metrica, " por Sector ", txt_tipo)
  })
  
  output$titulo_mapa <- renderText({
    req(input$tiempo_mapa_sel)
    txt_fecha <- if(input$granularidad_mapa == "mensual") format(as.Date(input$tiempo_mapa_sel), "%B %Y") else paste("Año", input$tiempo_mapa_sel)
    paste0("Mapa: ", input$sector_mapa, " (% sobre Población) - ", txt_fecha)
  })

  output$plot_sectores <- renderPlot({
    df_plot <- datos_grafico()
    req(df_plot)
    validate(need(nrow(df_plot) > 0, "Sin datos."))
    
    df_plot$sector <- tools::toTitleCase(df_plot$sector)
    p <- ggplot(df_plot, aes(x = fecha, y = valor_final)) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom", strip.text = element_text(face="bold", size=20),
            axis.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
            legend.title=element_text(size=18, face="bold"), legend.text=element_text(size=16))

    if (input$relativo) {
      p <- p + geom_area(aes(fill = sector), alpha=0.85, color="white", size=0.2) +
        coord_cartesian(expand=FALSE) + scale_fill_brewer(palette="Set1") + 
        labs(x=NULL, y="% del Total", fill="Sector")
    } else {
      p <- p + geom_line(aes(color=sector, group=sector), size=1.2) +
        scale_color_brewer(palette="Set1") + 
        labs(x=NULL, y="Total Registros", color="Sector")
    }
    
    num_anios <- input$rango_anios[2] - input$rango_anios[1]
    if (input$granularidad == "mensual") {
      if (num_anios <= 3) { b<-"1 month"; l<-"%b %Y" } else if (num_anios <= 5) { b<-"3 months"; l<-"%b %Y" } else { b<-"6 months"; l<-"%m/%Y" }
      p <- p + scale_x_date(date_labels=l, date_breaks=b, expand=c(0.01,0)) + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
    } else {
      p <- p + scale_x_date(date_labels="%Y", date_breaks="1 year") + theme(axis.text.x=element_text(angle=0, hjust=0.5))
    }
    if (length(unique(df_plot$Provincia)) > 1) p <- p + facet_wrap(~Provincia, scales="free_y", ncol=1)
    else p <- p + ggtitle(unique(df_plot$Provincia)) + theme(plot.title=element_text(size=24, face="bold", hjust=0.5))
    p
  })
  
  # --- 7. Render Leaflet (CORREGIDO: ESCALAS DISTINTAS Y NaNs) ---
  output$mapa_leaflet <- renderLeaflet({
    # 1. Datos del momento actual
    datos <- datos_para_mapa()
    mapa_sf <- mapa_spain_sf()
    
    # 2. Datos globales para la escala
    df_global <- datos_base()
    df_pob <- poblacion_data()
    
    validate(need(!is.null(mapa_sf), "Cargando geometrías..."),
             need(nrow(datos) > 0, "No hay datos para esta fecha/año."))
    
    # --- CÁLCULO DE LA ESCALA GLOBAL FIJA ---
    
    # Filtramos la historia completa de este Indicador y Sector
    df_base_hist <- df_global %>%
      filter(metrica == input$metrica_sel, sector == input$sector_mapa)
    
    # Agrupamos según la granularidad seleccionada para que la escala tenga sentido
    if (input$granularidad_mapa == "anual") {
      # MODO ANUAL: Sumas/Medias anuales
      df_rango_global <- df_base_hist %>%
        group_by(anio, cod_prov) %>%
        summarise(
          valor_abs = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE),
          .groups = "drop"
        )
    } else {
      # MODO MENSUAL: Sumas mensuales
      df_rango_global <- df_base_hist %>%
        group_by(anio, mes, cod_prov) %>%
        summarise(
          valor_abs = sum(valor, na.rm=TRUE),
          .groups = "drop"
        )
    }
    
    # Calculamos % histórico sobre población
    df_rango_global <- df_rango_global %>%
      left_join(df_pob, by = c("cod_prov", "anio")) %>%
      mutate(valor_rel = (valor_abs / poblacion_total) * 100)
    
    # === CAMBIO: CALCULO DE PERCENTILES Pmin% (Min) y Pmax% (Max) ===
    quantiles <- quantile(df_rango_global$valor_rel, probs = c(Pmin, Pmax), na.rm = TRUE)
    min_global <- quantiles[1]
    max_global <- quantiles[2]
    
    # Seguridad anti-errores (por si todo es 0 o hay NAs)
    if(is.na(min_global)) min_global <- 0
    if(is.na(max_global) || max_global <= min_global) max_global <- min_global + 0.001
    
    # -------------------------------------------------------------------------
    
    mapa_completo <- left_join(mapa_sf, datos, by = c("cod_prov_int" = "cod_prov")) %>%
      mutate(
        # 1. Valores Reales para mostrar en texto (limpiando NAs)
        valor_abs_show = ifelse(is.na(valor_abs), 0, valor_abs),
        valor_rel_real = ifelse(is.na(valor_rel) | is.infinite(valor_rel), 0, valor_rel),
        
        # 2. CLAMPING / TOPADO (Para el color):
        # Todo lo que esté por debajo del Pmin se iguala al Pmin.
        # Todo lo que esté por encima del Pmax se iguala al Pmax.
        # Esto evita el warning de "values outside color scale".
        valor_para_color = pmin(pmax(valor_rel_real, min_global), max_global)
      )
    
    # La paleta va del Pmin al Pmax
    pal <- colorNumeric(palette = "YlOrRd", domain = c(min_global, max_global), na.color = "#e0e0e0")
    
    popup_txt <- paste0(
      "<strong>", mapa_completo$Provincia.x, "</strong><br>",
      "Sector: ", input$sector_mapa, "<br>",
      "-------------------------<br>",
      "Total Absoluto: ", format(round(mapa_completo$valor_abs_show, 0), big.mark = ".", decimal.mark = ","), "<br>",
      "Población Prov: ", format(round(mapa_completo$poblacion_total, 0), big.mark = ".", decimal.mark = ","), "<br>",
      # Mostramos el valor REAL, no el topado
      "<strong>% sobre Población: ", format(round(mapa_completo$valor_rel_real, 3), decimal.mark = ","), "%</strong>"
    )
    
    leaflet(mapa_completo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -3.7, lat = 40.4, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(valor_para_color), # Usamos el valor topado para pintar
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        popup = popup_txt
      ) %>%
      addLegend(pal = pal, values = ~valor_para_color, opacity = 0.7, 
                title = paste0("% Pob. (Sat. 5-95%)"),
                labFormat = labelFormat(suffix = "%"),
                position = "bottomright")
  })
  
  output$debug_info <- renderText({
    if (input$tabs_main == "tab_grafico") {
       df <- datos_grafico()
       return(paste("Registros Gráfico:", nrow(df)))
    } else {
       df <- datos_para_mapa()
       return(paste("Registros Mapa:", nrow(df), "| Temporal:", input$tiempo_mapa_sel))
    }
  })





##########################################################
## ===IDIOM TABLA 
##########################################################

# --- Renderizado de la Tabla Interactiva ---
  output$tabla_maestra <- renderDT({
    df <- datos_base()
    req(df)
  
    datatable(
      df,
      filter = 'top', # Esto añade los cuadros de búsqueda bajo los títulos
      rownames = FALSE,
      options = list(
        pageLength = 15,       # Cuántas filas mostrar por página
        autoWidth = TRUE,
        scrollX = TRUE,        # Permite scroll horizontal si hay muchas columnas
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json') # En español
      )
    )
  })



##########################################################
## ===IDIOM 3 
##########################################################

# =====================================================
# RELACIÓN PARO (t) vs CONTRATOS (t + k) 
# =====================================================

  # 1. Paro en el año t
  datos_paro_t <- reactive({   # reactive es para que la función trabaje siempre sobre los datos actualizados
    df <- datos_base()
    req(df)  # cargamos el df con todos los datos y la col metrica que especifica si es contrato/paro o que es

    df %>%
        filter(metrica == "paro") %>%
        group_by(anio, cod_prov, Provincia,`Comunidad Aut`, sector) %>% # un unico valor por año, provincia y sector
        summarise(
            paro_t = mean(valor, na.rm = TRUE), # media de paro en una provincia, en un año, en un sector
            .groups = "drop" # devuelve el dataframe normal
        )

  })

  # 2. Contratos en el año t + k
  datos_contratos_tk <- reactive({
    df <- datos_base()
    req(df)
  
  # ⚠️ k fijo de momento (luego será input)
    # k <- 1
    k <- as.numeric(input$desfase_anios)
  
    df %>%
      filter(metrica == "contratos") %>% #
      group_by(anio, cod_prov, Provincia, `Comunidad Aut`, sector) %>%
      summarise(
        contratos = sum(valor, na.rm = TRUE), # suma del nº contratos en una provincia, en un año, en un sector.
        .groups = "drop"
      ) %>%
      mutate(anio = anio - k)  # desplazamiento temporal
      # osea los contratos de 2013 se asignan a 2011
  })

# 3. Dataset final Paro vs Contratos

# juntamos y si no exiten para el mismo año, sector y provincia valores de ambos, se descarta
  datos_relacion_paro_contratos <- reactive({
    df_paro <- datos_paro_t()
    df_contr <- datos_contratos_tk()
    req(df_paro, df_contr)
  
    inner_join(
      df_paro,
      df_contr,
      by = c("anio", "cod_prov", "Provincia", "sector")
    ) %>%
      filter(!is.na(paro_t), !is.na(contratos))
  })


# =====================================================
# === VISUALIZACION RELACION PARO vs CONTRATOS ==========
# =====================================================

# Reactive filtrado según CCAA, sector y desfase

  datos_relacion_filtrados <- reactive({

    df <- datos_relacion_paro_contratos()
    req(df)

    # ---- 1) Filtro por CCAA ----
    if (input$ccaa_rel != "ALL") {
      df <- df %>% filter(`Comunidad Aut` == input$ccaa_rel)
    }

    # ---- 2) Filtro por sector ----
    if (!is.null(input$sector_rel) && input$sector_rel != "") {
      df <- df %>% filter(sector == input$sector_rel)
    }

    # ---- 3) Normalización con población ----
    df_pob <- poblacion_data()
    req(df_pob)

    df <- df %>%
      left_join(df_pob, by = c("cod_prov", "anio")) %>%
      mutate(
        paro_rate       = paro_t   / poblacion_total * 1000,
        contratos_rate  = contratos / poblacion_total * 1000
      ) %>%
      filter(
        !is.na(paro_rate),
        !is.na(contratos_rate),
        is.finite(paro_rate),
        is.finite(contratos_rate)
      )

    df   # <-- ESTE SÍ ES EL RETURN CORRECTO
  })


  output$plot_relacion <- renderPlot({
  df <- datos_relacion_filtrados()
  req(nrow(df) > 0)

  ggplot(df, aes(x = paro_rate, y = contratos_rate)) +
    geom_point(aes(color = sector), alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
    # ZOOM AUTOMÁTICO: Ignora el 1% de datos extremos para que no se vea "aplastado"
    coord_cartesian(
      xlim = c(0, quantile(df$paro_rate, 0.99, na.rm = TRUE)),
      ylim = c(0, quantile(df$contratos_rate, 0.99, na.rm = TRUE))
    ) +
    theme_minimal(base_size = 16) +
    labs(
      x = "Paro por 1.000 habitantes (Año t)",
      y = paste0("Contratos por 1.000 hab. (Año t + ", input$desfase_anios, ")"),
      title = paste("Relación Normalizada —", input$ccaa_rel)
    )
  })

    


##########################################################
## ===IDIOM COMPARATIVAS POR EDAD/GENERO 
##########################################################


  output$plot_des_sexo <- renderPlot({
    req("Género" %in% input$grupo_des)
  
    df <- datos_desigualdad() %>%
      group_by(fecha, genero, sector) %>%
      summarise(valor = sum(valor, na.rm=TRUE), .groups="drop") %>%
      filter(genero %in% c("Hombre","Mujer"))  # Eliminamos totales
  
    ggplot(df, aes(x=fecha, y=valor, color=genero)) +
      geom_line(size=1.2) +
      facet_wrap(~sector, scales="free_y") +  # Una gráfica por sector
      labs(
        title=paste("Evolución por sexo —", input$prov1),
        x=NULL, y="Total"
      ) +
      theme_minimal(base_size = 14)
  })


  output$plot_des_edad <- renderPlot({
    req("Edad" %in% input$grupo_des)
  
    df <- datos_desigualdad() %>%
      group_by(fecha, edad, sector) %>%
      summarise(valor = sum(valor, na.rm=TRUE), .groups="drop")
  
    ggplot(df, aes(x=fecha, y=valor, color=edad)) +
      geom_line(size=1.2) +
      facet_wrap(~sector, scales="free_y") +
      labs(
        title=paste("Evolución por edad —", input$prov1),
        x=NULL, y="Total"
      ) +
      theme_minimal(base_size = 14)
  })


  # output$plot_des_comp <- renderPlot({
  #   df <- datos_desigualdad() %>%
  #     group_by(fecha, edad) %>%
  #     summarise(valor=sum(valor, na.rm=TRUE), .groups="drop") %>%
  #     group_by(fecha) %>%
  #     mutate(porcentaje = valor / sum(valor) * 100)
  
  #   ggplot(df, aes(x=fecha, y=porcentaje, fill=edad)) +
  #     geom_area() +
  #     labs(
  #       title=paste("Composición porcentual por edad —", input$prov1),
  #       x=NULL, y="%"
  #     ) +
  #     theme_minimal(base_size = 14)
  # })

 

  output$debug_info <- renderPrint({
    df <- datos_relacion_paro_contratos()
    req(df)

    cat("CCAA disponibles en la relación paro-contratos:\n")
    print(unique(df$`Comunidad Aut`))
  })












}











###################################################################
### LANZAMIENTO DE LA APP ######
###################################################################

shinyApp(ui = ui, server = server)