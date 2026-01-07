
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


### --------lEER LOS CSV Y CONVERTIRLOS EN UN DF-----------
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




# ###################################################################
# ### UI (INTERFAZ DE USUARIO) ######
# ###################################################################

ui <- fluidPage(

  useShinyjs(),

  titlePanel(paste("SEPE Analytics — Empleo y Paro (", anios_titulos, ")")),

  sidebarLayout(

    # ----------- SIDEBAR (FILTROS) -----------
    sidebarPanel(
      width = 3,

      h4("Configuración Global"),

      selectInput(
        "metrica_sel", "Indicador:",
        choices = c(
          "Paro Registrado" = "paro",
          "Contratos Registrados" = "contratos",
          "Demandantes de Empleo" = "dtes"
        ),
        selected = "contratos"
      ),

      # ---- PESTAÑA 1 ----
      conditionalPanel(
        condition = "input.tabs_main == 'tab_grafico'",
        h4("Filtros Evolución"),
        selectInput("prov1", "Provincia Principal:", choices = NULL),
        selectInput("prov2", "Comparar con (Opcional):", choices = NULL),

        sliderInput(
          "rango_anios", "Rango de Años:",
          min = min(anios), max = max(anios),
          value = c(min(anios), max(anios)),
          step = 1, sep = ""
        ),

        radioButtons(
          "granularidad", "Escala Temporal:",
          choices = c(
            "Anual (Media)" = "anual",
            "Mensual (Detalle)" = "mensual"
          ),
          selected = "mensual"
        ),

        checkboxInput("relativo", "Ver distribución % (Áreas Apiladas)", FALSE)
      ),

      # ---- PESTAÑA 2 ----
      conditionalPanel(
        condition = "input.tabs_main == 'tab_mapa'",
        h4("Filtros Mapa"),

        radioButtons(
          "granularidad_mapa", "Escala Temporal:",
          choices = c("Anual" = "anual", "Mensual" = "mensual"),
          selected = "anual"
        ),

        uiOutput("ui_slider_mapa_dinamico"),

        selectInput(
          "sector_mapa", "Sector a visualizar:",
          choices = c(
            "Agricultura", "Industria", "Construcción",
            "Servicios", "Sin empleo Anterior"
          ),
          selected = "Servicios"
        ),

        helpText(
          "Nota: El valor relativo (%) se calcula sobre la POBLACIÓN TOTAL de la provincia."
        )
      ),

      # ---- PESTAÑA 3 ----
      conditionalPanel(
        condition = "input.tabs_main == 'tab_relacion'",
        h4("Filtros Relación Paro–Contratos"),

        selectInput("ccaa_rel", "Comunidad Autónoma:", choices = NULL),

        selectInput(
          "sector_rel", "Sector:",
          choices = c("Todos", "Agricultura", "Industria", "Construcción", "Servicios")
        ),

        selectInput(
          "dimension_rel", "Dimensión adicional:",
          choices = c(
            "Ninguna" = "ninguna",
            "Género" = "genero",
            "Tipo de Contrato" = "tipo_contrato"
          )
        ),

        sliderInput(
          "desfase_anios", "Desfase (años):",
          min = 0, max = 5, value = 1, step = 1
        ),

        checkboxInput("normalizar_poblacion", "Normalizar por población", TRUE)
      ),

      # ---- PESTAÑA 4 ----
      conditionalPanel(
        condition = "input.tabs_main == 'tab_desigualdades'",
        h4("Configuración Desigualdades"),

        radioButtons(
          "tipo_analisis_des", "Tipo de Análisis:",
          choices = c(
            "Brecha de Género" = "genero",
            "Tipo de Contrato" = "contrato",
            "Distribución por Edad" = "edad",
            "Comparación Provincial" = "provincial",
            "Evolución Temporal" = "temporal"
          ),
          selected = "genero"
        ),

        selectInput("ccaa_des", "Comunidad Autónoma:", choices = NULL),

        conditionalPanel(
          condition = "input.tipo_analisis_des == 'genero' ||
                       input.tipo_analisis_des == 'contrato' ||
                       input.tipo_analisis_des == 'edad'",
          selectInput(
            "sector_des", "Sector:",
            choices = c(
              "Todos", "Agricultura", "Industria",
              "Construcción", "Servicios", "Sin empleo Anterior"
            ),
            selected = "Todos"
          )
        ),

        conditionalPanel(
          condition = "input.tipo_analisis_des == 'provincial'",
          radioButtons(
            "metrica_prov", "Métrica a Comparar:",
            choices = c(
              "% Mujeres" = "pct_mujeres",
              "Brecha de Género" = "brecha_genero",
              "% Contratos Temporales" = "pct_temporales",
              "% Jóvenes (<25)" = "pct_jovenes"
            ),
            selected = "brecha_genero"
          )
        ),

        conditionalPanel(
          condition = "input.tipo_analisis_des == 'temporal'",
          selectInput("prov_temporal", "Provincia:", choices = NULL),
          radioButtons(
            "metrica_temporal", "Métrica:",
            choices = c(
              "Brecha de Género" = "brecha_genero",
              "% Mujeres" = "pct_mujeres",
              "% Contratos Temporales" = "pct_temporales",
              "% Contratos Indefinidos" = "pct_indefinidos"
            ),
            selected = "brecha_genero"
          )
        ),

        conditionalPanel(
          condition = "input.tipo_analisis_des != 'temporal'",
          sliderInput(
            "anio_des", "Año:",
            min = min(anios), max = max(anios),
            value = max(anios), step = 1, sep = ""
          )
        ),

        conditionalPanel(
          condition = "input.tipo_analisis_des == 'temporal'",
          sliderInput(
            "rango_anios_des", "Rango de Años:",
            min = min(anios), max = max(anios),
            value = c(max(anios) - 5, max(anios)),
            step = 1, sep = ""
          )
        )
      )
    ),

    # ----------- MAIN PANEL (GRÁFICOS) -----------
    mainPanel(
      width = 9,

      tabsetPanel(id = "tabs_main",

        tabPanel(
          "Evolución Temporal", value = "tab_grafico",
          br(),
          h3(textOutput("titulo_grafico")),
          plotOutput("plot_sectores", height = "600px")
        ),

        tabPanel(
          "Mapa Geográfico", value = "tab_mapa",
          br(),
          h3(textOutput("titulo_mapa")),
          leafletOutput("mapa_leaflet", height = "650px")
        ),

        tabPanel(
          "Relación Paro y Contratos los años posteriores",
          value = "tab_relacion",
          br(),
          plotOutput("plot_relacion", height = "600px"),
          uiOutput("estadisticas_relacion")
        ),

        tabPanel(
          "Desigualdades Demográficas",
          value = "tab_desigualdades",
          br(),
          h3(textOutput("titulo_desigualdades")),

          conditionalPanel(
            condition = "input.tipo_analisis_des == 'genero'",
            plotOutput("plot_brecha_barras", height = "600px"),
            plotOutput("plot_brecha_sector", height = "400px")
          ),

          conditionalPanel(
            condition = "input.tipo_analisis_des == 'contrato'",
            plotOutput("plot_contrato_stacked", height = "500px"),
            plotOutput("plot_contrato_temporal", height = "300px")
          ),

          conditionalPanel(
            condition = "input.tipo_analisis_des == 'edad'",
            plotOutput("plot_piramide_edad", height = "500px"),
            plotOutput("plot_edad_provincia", height = "400px")
          ),

          conditionalPanel(
            condition = "input.tipo_analisis_des == 'provincial'",
            plotOutput("plot_heatmap_provincial", height = "600px"),
            plotOutput("plot_top10_provincias", height = "400px")
          ),

          conditionalPanel(
            condition = "input.tipo_analisis_des == 'temporal'",
            plotOutput("plot_evolucion_temporal", height = "500px"),
            plotOutput("plot_tendencia", height = "300px")
          ),

          wellPanel(
            h4("Métricas Clave"),
            uiOutput("metricas_resumen")
          )
        ),

        tabPanel(
          "Explorador de Datos", value = "tab_tabla",
          br(),
          h3("Tabla de Datos Brutos"),
          downloadButton("descargar_datos", "Descargar CSV"),
          br(), br(),
          DTOutput("tabla_maestra")
        )
      ),

      br(),
      verbatimTextOutput("debug_info")
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

    # --- Etiquetas semánticas ---
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
          grepl("hombre", variable, ignore.case = TRUE) ~ "Hombre",
          grepl("mujer", variable, ignore.case = TRUE) ~ "Mujer",
          TRUE ~ "Total"
        ),
        
        edad = case_when(
          grepl("Menor", variable) | grepl("< 25", variable) ~ "<25",
          grepl("25", variable) & grepl("45", variable) ~ "25-44",
          grepl(">=45", variable) | grepl(">= 45", variable) ~ "45+",
          TRUE ~ "Total"
        ),
        
        # NUEVO: Tipo de contrato para datos de contratos
        tipo_contrato = case_when(
          tipo_metrica == "contratos" & grepl("indefinido", variable, ignore.case = TRUE) ~ "Indefinido",
          tipo_metrica == "contratos" & grepl("temporal", variable, ignore.case = TRUE) ~ "Temporal",
          tipo_metrica == "contratos" & grepl("convertido", variable, ignore.case = TRUE) ~ "Convertido",
          TRUE ~ "Total"
        )
      )

    # --- Agregación mínima ---
    df_largo %>%
      group_by(anio, mes, cod_prov, Provincia, `Comunidad Aut`,
              tipo, sector, genero, edad, tipo_contrato, es_total) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  }


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
    updateSelectInput(session, "prov1_des", choices = provs, selected = sel_def)
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
    
    updateSelectInput(
      session, "ccaa_des",
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


  #########################################################
  # Update para provincia temporal
  observe({
    df <- datos_base()
    req(df)
    provs <- sort(unique(df$Provincia))
    sel_def <- if("Madrid" %in% provs) "Madrid" else provs[1]
    updateSelectInput(session, "prov_temporal", choices = provs, selected = sel_def)
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
             anio >= input$rango_anios[1], anio <= input$rango_anios[2], 
             sector != "Total", sector != "Otros")
    
    if (input$granularidad == "anual") {
      df_agrupado <- df_filt %>%
        group_by(anio, Provincia, sector, metrica) %>%
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), .groups = "drop")
      df_agrupado$fecha <- as.Date(paste0(df_agrupado$anio, "-01-01"))
    } else {
      df_agrupado <- df_filt 
    }
    
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

  # --- 5. Transformación para MAPA ---
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
        summarise(valor = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE), 
                 .groups = "drop")
      
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
        valor_abs = tidyr::replace_na(valor_abs, 0),
        valor_rel = (valor_abs / poblacion_total) * 100
      )
    
    return(df_final)
  })

  # --- 6. Render Plots ---
  output$titulo_grafico <- renderText({
    txt_metrica <- switch(input$metrica_sel, 
                         "paro" = "Paro", 
                         "contratos" = "Contratos", 
                         "dtes" = "Demandantes")
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
  
  # --- 7. Render Leaflet ---
  output$mapa_leaflet <- renderLeaflet({
    datos <- datos_para_mapa()
    mapa_sf <- mapa_spain_sf()
    
    df_global <- datos_base()
    df_pob <- poblacion_data()
    
    validate(need(!is.null(mapa_sf), "Cargando geometrías..."),
             need(nrow(datos) > 0, "No hay datos para esta fecha/año."))
    
    # CÁLCULO DE LA ESCALA GLOBAL FIJA
    df_base_hist <- df_global %>%
      filter(metrica == input$metrica_sel, sector == input$sector_mapa)
    
    if (input$granularidad_mapa == "anual") {
      df_rango_global <- df_base_hist %>%
        group_by(anio, cod_prov) %>%
        summarise(
          valor_abs = if(input$metrica_sel == "contratos") sum(valor, na.rm=TRUE) else mean(valor, na.rm=TRUE),
          .groups = "drop"
        )
    } else {
      df_rango_global <- df_base_hist %>%
        group_by(anio, mes, cod_prov) %>%
        summarise(
          valor_abs = sum(valor, na.rm=TRUE),
          .groups = "drop"
        )
    }
    
    df_rango_global <- df_rango_global %>%
      left_join(df_pob, by = c("cod_prov", "anio")) %>%
      mutate(valor_rel = (valor_abs / poblacion_total) * 100)
    
    quantiles <- quantile(df_rango_global$valor_rel, probs = c(Pmin, Pmax), na.rm = TRUE)
    min_global <- quantiles[1]
    max_global <- quantiles[2]
    
    if(is.na(min_global)) min_global <- 0
    if(is.na(max_global) || max_global <= min_global) max_global <- min_global + 0.001
    
    mapa_completo <- left_join(mapa_sf, datos, by = c("cod_prov_int" = "cod_prov")) %>%
      mutate(
        valor_abs_show = ifelse(is.na(valor_abs), 0, valor_abs),
        valor_rel_real = ifelse(is.na(valor_rel) | is.infinite(valor_rel), 0, valor_rel),
        valor_para_color = pmin(pmax(valor_rel_real, min_global), max_global)
      )
    
    pal <- colorNumeric(palette = "YlOrRd", domain = c(min_global, max_global), na.color = "#e0e0e0")
    
    popup_txt <- paste0(
      "<strong>", mapa_completo$Provincia.x, "</strong><br>",
      "Sector: ", input$sector_mapa, "<br>",
      "-------------------------<br>",
      "Total Absoluto: ", format(round(mapa_completo$valor_abs_show, 0), big.mark = ".", decimal.mark = ","), "<br>",
      "Población Prov: ", format(round(mapa_completo$poblacion_total, 0), big.mark = ".", decimal.mark = ","), "<br>",
      "<strong>% sobre Población: ", format(round(mapa_completo$valor_rel_real, 3), decimal.mark = ","), "%</strong>"
    )
    
    leaflet(mapa_completo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -3.7, lat = 40.4, zoom = 6) %>%
      addPolygons(
        fillColor = ~pal(valor_para_color),
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
  ## === TABLA INTERACTIVA 
  ##########################################################
  
  output$tabla_maestra <- renderDT({
    df <- datos_base()
    req(df)
  
    datatable(
      df,
      filter = 'top',
      rownames = FALSE,
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      )
    )
  })



  ##########################################################
  ## === RELACIÓN PARO (t) vs CONTRATOS (t + k) 
  ##########################################################

  # 1. Paro en el año t
  datos_paro_t <- reactive({
    df <- datos_base()
    req(df)
    
    df_paro <- df %>%
      filter(metrica == "paro") %>%
      group_by(anio, cod_prov, Provincia, `Comunidad Aut`, sector, genero) %>%
      summarise(
        paro_t = mean(valor, na.rm = TRUE),
        .groups = "drop"
      )
    
    return(df_paro)
  })

  # 2. Contratos en el año t + k
  datos_contratos_tk <- reactive({
    df <- datos_base()
    req(df)
    
    k <- as.numeric(input$desfase_anios)
    
    df_contratos <- df %>%
      filter(metrica == "contratos") %>%
      group_by(anio, cod_prov, Provincia, `Comunidad Aut`, sector, genero, tipo_contrato) %>%
      summarise(
        contratos = sum(valor, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(anio = anio - k)
    
    return(df_contratos)
  })

  # 3. Dataset final Paro vs Contratos
  datos_relacion_filtrados <- reactive({
    df_paro <- datos_paro_t()
    df_contr <- datos_contratos_tk()
    req(df_paro, df_contr)
    
    # Unir datos
    df_relacion <- inner_join(
      df_paro,
      df_contr,
      by = c("anio", "cod_prov", "Provincia", "sector")
    ) %>%
      filter(!is.na(paro_t), !is.na(contratos))
    
    # Aplicar filtros
    if (input$ccaa_rel != "ALL") {
      df_relacion <- df_relacion %>% filter(`Comunidad Aut.x` == input$ccaa_rel)
    }
    
    if (input$sector_rel != "Todos") {
      df_relacion <- df_relacion %>% filter(sector == input$sector_rel)
    }
    
    # Filtrar por dimensión adicional
    if (input$dimension_rel == "genero") {
      df_relacion <- df_relacion %>%
        filter(genero.x == genero.y, genero.x != "Total") %>%
        rename(genero = genero.x)
    } else if (input$dimension_rel == "tipo_contrato") {
      df_relacion <- df_relacion %>%
        filter(tipo_contrato != "Total")
    }
    
    # Normalizar por población si está seleccionado
    if (input$normalizar_poblacion) {
      df_pob <- poblacion_data()
      req(df_pob)
      
      df_relacion <- df_relacion %>%
        left_join(df_pob, by = c("cod_prov", "anio")) %>%
        mutate(
          paro_rate = paro_t / poblacion_total * 1000,
          contratos_rate = contratos / poblacion_total * 1000
        ) %>%
        filter(!is.na(paro_rate), !is.na(contratos_rate),
               is.finite(paro_rate), is.finite(contratos_rate))
    } else {
      df_relacion <- df_relacion %>%
        mutate(
          paro_rate = paro_t,
          contratos_rate = contratos
        )
    }
    
    return(df_relacion)
  })




  # 4. Gráfico de relación 
  output$plot_relacion <- renderPlot({
    df <- datos_relacion_filtrados()
    req(nrow(df) > 0)
    
    # Crear gráfico base
    p <- ggplot(df, aes(x = paro_rate, y = contratos_rate))
    
    # Añadir dimensión visual según selección
    if (input$dimension_rel == "ninguna") {
      p <- p + geom_point(alpha = 0.6, size = 3, color = "steelblue")
    } else if (input$dimension_rel == "genero") {
      p <- p + geom_point(aes(color = genero), alpha = 0.6, size = 3) +
        scale_color_brewer(palette = "Set1", name = "Género")
    } else if (input$dimension_rel == "tipo_contrato") {
      p <- p + geom_point(aes(color = tipo_contrato, size = contratos_rate), alpha = 0.6) +
        scale_color_brewer(palette = "Set2", name = "Tipo Contrato") +
        scale_size_continuous(name = "Tasa Contratos", range = c(2, 8))
    }
    
    # Añadir línea de tendencia y personalizar
    p <- p +
      geom_smooth(method = "lm", color = "darkred", linetype = "dashed", 
                  se = TRUE, alpha = 0.2) +
      theme_minimal(base_size = 14) +
      labs(
        x = ifelse(input$normalizar_poblacion,
                   "Paro por 1.000 habitantes (Año t)",
                   "Paro Total (Año t)"),
        y = ifelse(input$normalizar_poblacion,
                   paste0("Contratos por 1.000 hab. (Año t + ", input$desfase_anios, ")"),
                   paste0("Contratos Totales (Año t + ", input$desfase_anios, ")")),
        title = paste("Relación Paro-Contratos:", 
                     ifelse(input$ccaa_rel == "ALL", "Todas las CCAA", input$ccaa_rel)),
        subtitle = ifelse(input$dimension_rel != "ninguna",
                         paste("Dimensión:", input$dimension_rel),
                         "")
      ) +
      theme(legend.position = "bottom")
    
    # Ajustar ejes para mejor visualización
    if (input$normalizar_poblacion) {
      p <- p + coord_cartesian(
        xlim = c(0, quantile(df$paro_rate, 0.95, na.rm = TRUE)),
        ylim = c(0, quantile(df$contratos_rate, 0.95, na.rm = TRUE))
      )
    }
    
    return(p)
  })

  # 5. Estadísticas de relación
  output$estadisticas_relacion <- renderUI({
    df <- datos_relacion_filtrados()
    req(nrow(df) > 0)
    
    # Calcular correlación
    correlacion <- cor(df$paro_rate, df$contratos_rate, use = "complete.obs")
    
    # Calcular estadísticas básicas
    stats <- df %>%
      summarise(
        n_observaciones = n(),
        media_paro = mean(paro_rate, na.rm = TRUE),
        media_contratos = mean(contratos_rate, na.rm = TRUE),
        correlacion = correlacion
      )
    
    # Crear texto informativo
    tags$div(
      class = "well",
      h4("Estadísticas de la relación:"),
      tags$ul(
        tags$li(paste("Número de observaciones:", stats$n_observaciones)),
        tags$li(paste("Correlación:", round(stats$correlacion, 3))),
        tags$li(paste("Paro medio:", round(stats$media_paro, 2), 
                      ifelse(input$normalizar_poblacion, "por 1.000 hab.", ""))),
        tags$li(paste("Contratos medios:", round(stats$media_contratos, 2),
                      ifelse(input$normalizar_poblacion, "por 1.000 hab.", "")))
      )
    )
  })

  ##########################################################
  ## === DESIGUALDADES DEMOGRÁFICAS 1 
  ##########################################################

  # --- Funciones Helper para Desigualdades ---
  
  # Función para calcular métricas de desigualdad
  calcular_metricas_desigualdad <- function(df) {
    req(df)
    
    # Calcular métricas básicas
    df_metrics <- df %>%
      group_by(Provincia, anio, sector, genero, edad, tipo_contrato) %>%
      summarise(
        valor_total = sum(valor, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Métricas por género
    genero_metrics <- df_metrics %>%
      filter(genero %in% c("Hombre", "Mujer")) %>%
      group_by(Provincia, anio, sector, genero) %>%
      summarise(
        valor_genero = sum(valor_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(Provincia, anio, sector) %>%
      summarise(
        total = sum(valor_genero, na.rm = TRUE),
        pct_hombres = ifelse(total > 0, 
                           sum(valor_genero[genero == "Hombre"], na.rm = TRUE) / total * 100,
                           0),
        pct_mujeres = ifelse(total > 0,
                           sum(valor_genero[genero == "Mujer"], na.rm = TRUE) / total * 100,
                           0),
        brecha_genero = pct_mujeres - pct_hombres,
        .groups = "drop"
      )
    
    # Métricas por tipo de contrato (solo para contratos)
    contrato_metrics <- df_metrics %>%
      filter(tipo_contrato %in% c("Temporal", "Indefinido", "Convertido")) %>%
      group_by(Provincia, anio, sector, tipo_contrato) %>%
      summarise(
        valor_contrato = sum(valor_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(Provincia, anio, sector) %>%
      summarise(
        total_contratos = sum(valor_contrato, na.rm = TRUE),
        pct_temporales = ifelse(total_contratos > 0,
                              sum(valor_contrato[tipo_contrato == "Temporal"], na.rm = TRUE) / total_contratos * 100,
                              0),
        pct_indefinidos = ifelse(total_contratos > 0,
                               sum(valor_contrato[tipo_contrato == "Indefinido"], na.rm = TRUE) / total_contratos * 100,
                               0),
        pct_convertidos = ifelse(total_contratos > 0,
                               sum(valor_contrato[tipo_contrato == "Convertido"], na.rm = TRUE) / total_contratos * 100,
                               0),
        .groups = "drop"
      )
    
    # Métricas por edad
    edad_metrics <- df_metrics %>%
      filter(edad %in% c("<25", "25-44", "45+")) %>%
      group_by(Provincia, anio, sector, edad) %>%
      summarise(
        valor_edad = sum(valor_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(Provincia, anio, sector) %>%
      summarise(
        total_edad = sum(valor_edad, na.rm = TRUE),
        pct_jovenes = ifelse(total_edad > 0,
                           sum(valor_edad[edad == "<25"], na.rm = TRUE) / total_edad * 100,
                           0),
        pct_adultos = ifelse(total_edad > 0,
                           sum(valor_edad[edad == "25-44"], na.rm = TRUE) / total_edad * 100,
                           0),
        pct_mayores = ifelse(total_edad > 0,
                           sum(valor_edad[edad == "45+"], na.rm = TRUE) / total_edad * 100,
                           0),
        .groups = "drop"
      )
    
    # Combinar todas las métricas
    full_metrics <- genero_metrics %>%
      left_join(contrato_metrics, by = c("Provincia", "anio", "sector")) %>%
      left_join(edad_metrics, by = c("Provincia", "anio", "sector"))
    
    return(full_metrics)
  }
  
  # Función para filtrar datos según selección
  datos_desigualdades_filtrados <- reactive({
    df <- datos_base()
    req(df, input$tipo_analisis_des)
    
    # Filtrar por métrica (siempre usamos contratos para desigualdades)
    df <- df %>% filter(metrica == "contratos")
    
    # Filtrar por CCAA
    if (!is.null(input$ccaa_des) && input$ccaa_des != "ALL") {
      df <- df %>% filter(`Comunidad Aut` == input$ccaa_des)
    }
    
    # Filtrar por sector (si aplica)
    if (!is.null(input$sector_des) && input$sector_des != "Todos" &&
        input$tipo_analisis_des %in% c("genero", "contrato", "edad")) {
      df <- df %>% filter(sector == input$sector_des)
    }
    
    # Filtrar por año (para análisis no temporales)
    if (input$tipo_analisis_des != "temporal" && !is.null(input$anio_des)) {
      df <- df %>% filter(anio == input$anio_des)
    }
    
    # Filtrar por rango de años (para análisis temporal)
    if (input$tipo_analisis_des == "temporal" && !is.null(input$rango_anios_des)) {
      df <- df %>% filter(anio >= input$rango_anios_des[1],
                         anio <= input$rango_anios_des[2])
    }
    
    # Filtrar por provincia (para análisis temporal específico)
    if (input$tipo_analisis_des == "temporal" && !is.null(input$prov_temporal)) {
      df <- df %>% filter(Provincia == input$prov_temporal)
    }
    
    return(df)
  })



  output$titulo_desigualdades <- renderText({
    tipo <- switch(input$tipo_analisis_des,
                  "genero" = "Brecha de Género",
                  "contrato" = "Tipo de Contrato",
                  "edad" = "Distribución por Edad",
                  "provincial" = "Comparación Provincial",
                  "temporal" = "Evolución Temporal")
    
    if (input$tipo_analisis_des == "temporal") {
      paste0("Análisis de Desigualdades: ", tipo, " (", 
             input$rango_anios_des[1], "-", input$rango_anios_des[2], ")")
    } else {
      paste0("Análisis de Desigualdades: ", tipo, " (Año ", input$anio_des, ")")
    }
  })


    # 1. Barras divergentes para brecha de género
  output$plot_brecha_barras <- renderPlot({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    # Preparar datos para gráfico de barras divergentes
    plot_data <- metrics %>%
      arrange(brecha_genero) %>%
      mutate(Provincia = factor(Provincia, levels = Provincia))
    
    ggplot(plot_data, aes(x = Provincia)) +
      geom_segment(aes(xend = Provincia, y = 0, yend = brecha_genero,
                      color = ifelse(brecha_genero > 0, "Mujeres", "Hombres")),
                  size = 2) +
      geom_point(aes(y = brecha_genero, 
                    color = ifelse(brecha_genero > 0, "Mujeres", "Hombres")),
                size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = c("Hombres" = "#FF6B6B", "Mujeres" = "#4ECDC4")) +
      coord_flip() +
      labs(x = NULL, y = "Brecha de Género (% Mujeres - % Hombres)",
          title = "Brecha de Género por Provincia",
          subtitle = "Valores positivos = Mayor % de mujeres") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            plot.title = element_text(face = "bold", size = 16))
  })
  
  # 2. Gráfico de barras apiladas 100% para tipo de contrato
  output$plot_contrato_stacked <- renderPlot({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    # Preparar datos para stacked bars
    plot_data <- metrics %>%
      select(Provincia, pct_temporales, pct_indefinidos, pct_convertidos) %>%
      pivot_longer(cols = starts_with("pct_"),
                  names_to = "tipo",
                  values_to = "porcentaje") %>%
      mutate(tipo = gsub("pct_", "", tipo),
            tipo = factor(tipo, levels = c("temporales", "indefinidos", "convertidos")))
    
    ggplot(plot_data, aes(x = Provincia, y = porcentaje, fill = tipo)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_brewer(palette = "Set2", 
                       labels = c("Temporales", "Indefinidos", "Convertidos")) +
      scale_y_continuous(labels = scales::percent_format()) +
      coord_flip() +
      labs(x = NULL, y = "Proporción",
          title = "Composición por Tipo de Contrato",
          fill = "Tipo de Contrato") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # 3. Pirámide poblacional por edad
  output$plot_piramide_edad <- renderPlot({
    df <- datos_desigualdades_filtrados()
    
    # Preparar datos para pirámide
    piramide_data <- df %>%
      filter(genero %in% c("Hombre", "Mujer"),
            edad %in% c("<25", "25-44", "45+")) %>%
      group_by(genero, edad) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      group_by(genero) %>%
      mutate(pct = valor / sum(valor) * 100) %>%
      mutate(valor_pyramid = ifelse(genero == "Hombre", -pct, pct))
    
    ggplot(piramide_data, aes(x = edad, y = valor_pyramid, fill = genero)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_text(aes(label = paste0(round(abs(valor_pyramid), 1), "%")),
                position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold") +
      coord_flip() +
      scale_fill_manual(values = c("Hombre" = "#3498db", "Mujer" = "#e74c3c")) +
      scale_y_continuous(labels = function(x) paste0(abs(x), "%"),
                        limits = max(abs(piramide_data$valor_pyramid)) * c(-1.1, 1.1)) +
      labs(x = "Grupo de Edad", y = "Porcentaje",
          title = "Pirámide Poblacional por Edad y Género",
          fill = "Género") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # 4. Heatmap provincial
  output$plot_heatmap_provincial <- renderPlot({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    # Seleccionar métrica según input
    metrica_seleccionada <- switch(input$metrica_prov,
                                  "pct_mujeres" = metrics$pct_mujeres,
                                  "brecha_genero" = metrics$brecha_genero,
                                  "pct_temporales" = metrics$pct_temporales,
                                  "pct_jovenes" = metrics$pct_jovenes)
    
    plot_data <- metrics %>%
      mutate(valor_heatmap = metrica_seleccionada) %>%
      arrange(desc(valor_heatmap))
    
    ggplot(plot_data, aes(x = Provincia, y = sector, fill = valor_heatmap)) +
      geom_tile(color = "white", size = 0.5) +
      scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                          midpoint = median(plot_data$valor_heatmap, na.rm = TRUE),
                          name = "Valor") +
      geom_text(aes(label = round(valor_heatmap, 1)), 
                color = "black", size = 3) +
      coord_flip() +
      labs(x = NULL, y = NULL,
          title = paste("Mapa de Calor:", 
                       switch(input$metrica_prov,
                              "pct_mujeres" = "% Mujeres",
                              "brecha_genero" = "Brecha de Género",
                              "pct_temporales" = "% Contratos Temporales",
                              "pct_jovenes" = "% Jóvenes (<25)"))) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 5. Evolución temporal
  output$plot_evolucion_temporal <- renderPlot({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    # Seleccionar métrica según input
    metrica_col <- switch(input$metrica_temporal,
                         "brecha_genero" = "brecha_genero",
                         "pct_mujeres" = "pct_mujeres",
                         "pct_temporales" = "pct_temporales",
                         "pct_indefinidos" = "pct_indefinidos")
    
    plot_data <- metrics %>%
      group_by(anio) %>%
      summarise(valor = mean(.data[[metrica_col]], na.rm = TRUE),
               .groups = "drop")
    
    ggplot(plot_data, aes(x = anio, y = valor)) +
      geom_line(size = 1.5, color = "#2c3e50") +
      geom_point(size = 3, color = "#e74c3c") +
      geom_smooth(method = "loess", se = FALSE, color = "#3498db", linetype = "dashed") +
      labs(x = "Año", y = switch(input$metrica_temporal,
                                 "brecha_genero" = "Brecha de Género",
                                 "pct_mujeres" = "% Mujeres",
                                 "pct_temporales" = "% Contratos Temporales",
                                 "pct_indefinidos" = "% Contratos Indefinidos"),
          title = paste("Evolución Temporal:", 
                       switch(input$metrica_temporal,
                              "brecha_genero" = "Brecha de Género",
                              "pct_mujeres" = "Porcentaje de Mujeres",
                              "pct_temporales" = "Contratos Temporales",
                              "pct_indefinidos" = "Contratos Indefinidos"))) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 16))
  })
  
  # 6. Gráficos adicionales (ejemplos)
  output$plot_brecha_sector <- renderPlot({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    ggplot(metrics, aes(x = sector, y = brecha_genero, fill = sector)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      scale_fill_brewer(palette = "Set3") +
      labs(x = NULL, y = "Brecha de Género",
          title = "Distribución de la Brecha por Sector") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot_top10_provincias <- renderPlot({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    # Seleccionar métrica
    metrica_top <- switch(input$metrica_prov,
                         "pct_mujeres" = "pct_mujeres",
                         "brecha_genero" = "brecha_genero",
                         "pct_temporales" = "pct_temporales",
                         "pct_jovenes" = "pct_jovenes")
    
    top_data <- metrics %>%
      group_by(Provincia) %>%
      summarise(valor = mean(.data[[metrica_top]], na.rm = TRUE)) %>%
      arrange(desc(valor)) %>%
      head(10)
    
    ggplot(top_data, aes(x = reorder(Provincia, valor), y = valor, fill = Provincia)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(valor, 1)), hjust = -0.2) +
      scale_fill_viridis_d() +
      coord_flip() +
      labs(x = NULL, y = "Valor",
          title = paste("Top 10 Provincias -",
                       switch(input$metrica_prov,
                              "pct_mujeres" = "% Mujeres",
                              "brecha_genero" = "Brecha de Género",
                              "pct_temporales" = "% Contratos Temporales",
                              "pct_jovenes" = "% Jóvenes"))) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  # 7. Panel de métricas resumen
  output$metricas_resumen <- renderUI({
    df <- datos_desigualdades_filtrados()
    metrics <- calcular_metricas_desigualdad(df)
    
    # Calcular estadísticas resumen
    if (nrow(metrics) > 0) {
      stats <- list(
        "Número de observaciones" = nrow(metrics),
        "Brecha de género media" = paste0(round(mean(metrics$brecha_genero, na.rm = TRUE), 2), "%"),
        "% Mujeres medio" = paste0(round(mean(metrics$pct_mujeres, na.rm = TRUE), 2), "%"),
        "% Contratos temporales" = paste0(round(mean(metrics$pct_temporales, na.rm = TRUE), 2), "%"),
        "% Jóvenes (<25)" = paste0(round(mean(metrics$pct_jovenes, na.rm = TRUE), 2), "%")
      )
      
      # Crear lista de métricas
      tagList(
        lapply(names(stats), function(nom) {
          tags$div(
            style = "margin-bottom: 8px;",
            tags$strong(paste0(nom, ": ")),
            tags$span(stats[[nom]])
          )
        })
      )
    } else {
      tags$p("No hay datos disponibles para las selecciones actuales.")
    }
  })
  














  ##########################################################
  ## === DESIGUALDADES DEMOGRÁFICAS 
  ##########################################################
  
  # Datos para desigualdades
  # datos_desigualdad <- reactive({
  #   df <- datos_base()
  #   req(df)
    
  #   # Filtrar por métrica seleccionada
  #   metrica <- ifelse(input$metrica_des == "Paro", "paro", "contratos")
  #   df <- df %>% filter(metrica == metrica)
    
  #   # Filtrar por CCAA si se ha seleccionado
  #   if (!is.null(input$ccaa_des) && input$ccaa_des != "ALL") {
  #     df <- df %>% filter(`Comunidad Aut` == input$ccaa_des)
  #   }
    
  #   # Filtrar por provincia
  #   if (!is.null(input$prov1_des) && input$prov1_des != "") {
  #     df <- df %>% filter(Provincia == input$prov1_des)
  #   }
    
  #   return(df)
  # })

  # output$plot_des_sexo <- renderPlot({
  #   req("Género" %in% input$grupo_des)
    
  #   df <- datos_desigualdad() %>%
  #     filter(genero %in% c("Hombre", "Mujer")) %>%
  #     group_by(fecha, genero, sector) %>%
  #     summarise(valor = sum(valor, na.rm=TRUE), .groups="drop")
    
  #   ggplot(df, aes(x=fecha, y=valor, color=genero)) +
  #     geom_line(size=1.2) +
  #     facet_wrap(~sector, scales="free_y") +
  #     labs(
  #       title=paste("Evolución por sexo —", 
  #                  ifelse(is.null(input$prov1_des) || input$prov1_des == "", 
  #                         input$ccaa_des, input$prov1_des)),
  #       x=NULL, y="Total"
  #     ) +
  #     theme_minimal(base_size = 14)
  # })

  # output$plot_des_edad <- renderPlot({
  #   req("Edad" %in% input$grupo_des)
    
  #   df <- datos_desigualdad() %>%
  #     filter(!edad %in% c("Total")) %>%
  #     group_by(fecha, edad, sector) %>%
  #     summarise(valor = sum(valor, na.rm=TRUE), .groups="drop")
    
  #   ggplot(df, aes(x=fecha, y=valor, color=edad)) +
  #     geom_line(size=1.2) +
  #     facet_wrap(~sector, scales="free_y") +
  #     labs(
  #       title=paste("Evolución por edad —", 
  #                  ifelse(is.null(input$prov1_des) || input$prov1_des == "", 
  #                         input$ccaa_des, input$prov1_des)),
  #       x=NULL, y="Total"
  #     ) +
  #     theme_minimal(base_size = 14)
  # })

  # output$plot_des_comp <- renderPlot({
  #   df <- datos_desigualdad() %>%
  #     group_by(fecha, edad) %>%
  #     summarise(valor=sum(valor, na.rm=TRUE), .groups="drop") %>%
  #     group_by(fecha) %>%
  #     mutate(porcentaje = valor / sum(valor) * 100)
    
  #   ggplot(df, aes(x=fecha, y=porcentaje, fill=edad)) +
  #     geom_area() +
  #     labs(
  #       title=paste("Composición porcentual por edad —", 
  #                  ifelse(is.null(input$prov1_des) || input$prov1_des == "", 
  #                         input$ccaa_des, input$prov1_des)),
  #       x=NULL, y="%"
  #     ) +
  #     theme_minimal(base_size = 14)
  # })

}



###################################################################
### LANZAMIENTO DE LA APP ######
###################################################################

shinyApp(ui = ui, server = server)







