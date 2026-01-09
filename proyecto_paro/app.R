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
library(plotly)


# Configuración Inicial
anios <- 2010:2025
anios_titulos <- paste(min(anios), max(anios), sep = "-")
Pmin = 0.02
Pmax = 0.98

source("preprocessing.R")

# Carga de datos inicial
res <- descargar_datasets_sepe(anios = anios, dir_data = "data")
# Descargamos población (fundamental para el nuevo cálculo del mapa)
res <- descargar_y_procesar_poblacion(codigos_ine = 2854:2908, dir_data = "data", anio_min = 2010, anio_max = 2025)
# Mapeo Provincia -> CCAA (para agregación)
ruta_csv_provincias <- "mapData/PROVINCIAS.csv"


### --------lEER LOS CSV Y CONVERTIRLOS EN UN DF-----------
# Función de lectura (Latin1)
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

mapeo_provincias <- readr::read_delim(
  ruta_csv_provincias,
  delim = ";",
  col_types = readr::cols(.default = "c"),
  locale = readr::locale(encoding = "WINDOWS-1252")
) %>%
  dplyr::transmute(
    cod_prov = as.integer(COD_PROV),
    CCAA = COMUNIDAD_AUTONOMA
  )



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

      conditionalPanel(
        condition = "input.tabs_main != 'tab_desigualdades'",

        h4("Configuración Global"),

        selectInput(
          "metrica_sel", "Indicador:",
          choices = c(
            "Paro Registrado" = "paro",
            "Contratos Registrados" = "contratos",
            "Demandantes de Empleo" = "dtes"
          ),
          selected = "contratos"
        )
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
        condition = "input.tabs_main == 'tab_desigualdades'",
        
        h4("Configuración de brechas"),
        
        radioButtons(
          "metrica_des",
          "Indicador",
          choices = c(
            "Contratos" = "contratos",
            "Paro registrado" = "paro"
          ),
          selected = "contratos",
          inline = TRUE
        ),


        sliderInput(
          "anio_sel_des",         
          "Año",                  
          min = min(anios),       
          max = max(anios),       
          value = max(anios),     
          step = 1,
          sep = "",               
          animate = TRUE           
        ),


        radioButtons(
          "nivel_geo_des",
          "Nivel territorial",
          choices = c(
            "España" = "espana",
            "Comunidad Autónoma" = "ccaa"
          ),
          selected = "espana"
        ),

        conditionalPanel(
          condition = "input.nivel_geo_des == 'ccaa'",
          selectInput(
            "ccaa_sel_des",
            "Comunidad Autónoma",
            choices = c("Todas las CCAA" = "todas", "Seleccionar una CCAA" = "una"),
            selected = "todas"
          ),
          
          conditionalPanel(
            condition = "input.ccaa_sel_des == 'una'",
            selectInput(
              "ccaa_especifica_des",
              "Selecciona una CCAA:",
              choices = NULL
            )
          )
        ),

        conditionalPanel(
          condition = "input.nivel_geo_des == 'provincia'",
          selectInput(
            "prov_sel_des",
            "Provincia",
            choices = NULL,
            selected = "28"
          )
        ),

        uiOutput("ui_tipo_brecha_des")

      ),

      conditionalPanel(
        condition = "input.tabs_main == 'tab_ccaa'",
        h4("Filtros CCAA vs España"),

        radioButtons("modo_p1", "Mostrar:",
          choices = c("Valores agregados" = "abs",
                      "Variación interanual (%)" = "yoy",
                      "Divergencia vs España" = "div"),
          selected = "yoy"
        ),

        sliderInput("rango_anios_p1", "Rango de años:",
          min = min(anios), max = max(anios),
          value = c(min(anios), max(anios)),
          step = 1, sep = ""
        )
      )
    ),

    # ----------- MAIN PANEL (GRÁFICOS) -----------
    mainPanel(
      width = 9,

      tabsetPanel(id = "tabs_main",

        tabPanel(
          "Evolución Temporal Sectores", value = "tab_grafico",
          br(),
          h3(textOutput("titulo_grafico")),
          plotOutput("plot_sectores", height = "600px")
        ),

        tabPanel(
          "Mapa Geográfico Sectores", value = "tab_mapa",
          br(),
          h3(textOutput("titulo_mapa")),
          leafletOutput("mapa_leaflet", height = "650px")
        ),

        tabPanel(
          "Desigualdades Demográficas",
          value = "tab_desigualdades",
          br(),
          h3(textOutput("titulo_desigualdades")),
          plotOutput("plot_brecha", height = "500px")
        ),

        tabPanel(
          "Variaciones Regionales", 
          value = "tab_ccaa",
          br(),
          plotlyOutput("plot_ccaa_p1", height = "520px"),
          br(),
          h4("CCAA que más divergen de España (por año)"),
          DTOutput("tabla_top_div_p1")
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

  # Agregación CCAA
  datos_ccaa <- reactive({
    df <- datos_base()
    req(df)

    df %>%
      left_join(mapeo_provincias, by = "cod_prov") %>%
      filter(!is.na(CCAA)) %>%
      group_by(fecha, anio, mes, metrica, CCAA) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")
  })

  # España (agregado nacional)
  datos_espana <- reactive({
    df <- datos_ccaa()
    req(df)

    df %>%
      group_by(fecha, anio, mes, metrica) %>%
      summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(CCAA = "España")
  })

  # Cálculo de variación (YoY) y divergencia vs España
  divergencia_ccaa <- reactive({
    df_ccaa <- datos_ccaa()
    df_es <- datos_espana()
    req(df_ccaa, df_es)

    dfA <- df_ccaa %>%
      group_by(anio, metrica, CCAA) %>%
      summarise(valor = sum(valor, na.rm=TRUE), .groups="drop") %>%
      arrange(CCAA, metrica, anio) %>%
      group_by(CCAA, metrica) %>%
      mutate(yoy = (valor / dplyr::lag(valor) - 1) * 100) %>%
      ungroup()

    esA <- df_es %>%
      group_by(anio, metrica) %>%
      summarise(valor_es = sum(valor, na.rm=TRUE), .groups="drop") %>%
      arrange(metrica, anio) %>%
      group_by(metrica) %>%
      mutate(yoy_es = (valor_es / dplyr::lag(valor_es) - 1) * 100) %>%
      ungroup()

    dfA %>%
      left_join(esA, by = c("anio","metrica")) %>%
      mutate(divergencia = yoy - yoy_es)  # + => crece más rápido / cae menos que España
  })

  ccaa_seleccionadas_tabla <- reactive({
  df <- tabla_div_p1_data()
  sel <- input$tabla_top_div_p1_rows_selected

  if (is.null(sel) || length(sel) == 0) return(character(0))

  unique(df$CCAA[sel])
})


tabla_div_p1_data <- reactive({
  req(input$metrica_sel, input$rango_anios_p1)

  divergencia_ccaa() %>%
    filter(metrica == input$metrica_sel) %>%
    filter(anio >= input$rango_anios_p1[1], anio <= input$rango_anios_p1[2]) %>%
    filter(!is.na(divergencia)) %>%
    mutate(abs_div = abs(divergencia)) %>%
    group_by(anio) %>%
    slice_max(order_by = abs_div, n = 5, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      Año = anio,
      CCAA = CCAA,
      `Divergencia` = round(divergencia, 2)
    ) %>%
    arrange(Año, desc(abs(`Divergencia`)))
})
  # --- Updates de UI ---
  observe({
    df <- datos_base()
    req(df)
    provs <- sort(unique(df$Provincia))
    sel_def <- if("Madrid" %in% provs) "Madrid" else provs[1]
    updateSelectInput(session, "prov1", choices = provs, selected = sel_def)
    updateSelectInput(session, "prov2", choices = c("Ninguna" = "", provs), selected = "")
    
    # Actualizar selectores de desigualdades
    updateSelectInput(session, "prov_sel_des", choices = provs, selected = sel_def)
    
    # Actualizar comunidades autónomas
    comunidades <- sort(unique(df$`Comunidad Aut`))
    updateSelectInput(session, "ccaa_especifica_des", choices = comunidades)
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
  ## === TABLA INTERACTIVA CON SELECTORES PARA CATEGORÍAS
  ##########################################################
  
  output$tabla_maestra <- renderDT({
    df <- datos_base()
    req(df)
    
    # Crear selectores para columnas categóricas
    columnas_categoricas <- c(
      "Provincia", "Comunidad Aut", "metrica", "sector", 
      "genero", "edad", "tipo_contrato"
    )
    
    # Filtrar solo columnas que existen en los datos
    columnas_existentes <- columnas_categoricas[columnas_categoricas %in% names(df)]
    
    # Crear opciones para DataTable
    options_list <- list(
      pageLength = 15,
      autoWidth = TRUE,
      scrollX = TRUE,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    )
    
    # Agregar selectores para cada columna categórica
    for(col in columnas_existentes) {
      unique_vals <- unique(df[[col]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      
      options_list[[paste0("columnDefs")]] <- if(is.null(options_list[["columnDefs"]])) {
        list(list(
          targets = which(names(df) == col) - 1,
          searchBuilderType = "select",
          searchBuilderOptions = list(
            values = unique_vals,
            condition = '=',
            conditionName = 'equals'
          )
        ))
      } else {
        c(options_list[["columnDefs"]], list(list(
          targets = which(names(df) == col) - 1,
          searchBuilderType = "select",
          searchBuilderOptions = list(
            values = unique_vals,
            condition = '=',
            conditionName = 'equals'
          )
        )))
      }
    }
    
    datatable(
      df,
      filter = 'top',
      rownames = FALSE,
      extensions = c('Buttons', 'SearchBuilder'),
      options = options_list
    )
  })
  
  # Función para descargar datos filtrados
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("datos_sepe_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- datos_base()
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  ##########################################################
  ## === DESIGUALDADES DEMOGRÁFICAS - VERSIÓN MEJORADA
  ##########################################################



  output$ui_tipo_brecha_des <- renderUI({
    
    if (input$metrica_des == "contratos") {
      selectInput(
        "tipo_brecha_des",
        "Tipo de brecha",
        choices = c(
          "Género" = "genero",
          "Tipo de contrato" = "tipo_contrato",
          "Sector" = "sector"
        ),
        selected = "genero"
      )
    } else {
      selectInput(
        "tipo_brecha_des",
        "Tipo de brecha",
        choices = c(
          "Género" = "genero",
          "Edad" = "edad",
          "Sector" = "sector"
        ),
        selected = "genero"
      )
    }
  })




  # ===============================
  # DATOS PARA BRECHAS
  # ===============================

  datos_brechas <- reactive({
    req(input$tipo_brecha_des, input$anio_sel_des, input$nivel_geo_des)
    df <- datos_base()
    req(df)
    
    # Filtrar solo contratos para análisis de desigualdades
    df_filt <- df %>% filter(metrica == input$metrica_des, anio == as.numeric(input$anio_sel_des))
    
    # Determinar si estamos en modo "todas las CCAA" o "una CCAA específica"
    if (input$nivel_geo_des == "ccaa") {
      if (input$ccaa_sel_des == "una" && !is.null(input$ccaa_especifica_des)) {
        # Modo una CCAA específica
        df_filt <- df_filt %>% filter(`Comunidad Aut` == input$ccaa_especifica_des)
        df_filt <- df_filt %>% mutate(territorio = `Comunidad Aut`)
      } else {
        # Modo todas las CCAA
        df_filt <- df_filt %>% mutate(territorio = `Comunidad Aut`)
      }
    } else if (input$nivel_geo_des == "provincia" && !is.null(input$prov_sel_des)) {
      df_filt <- df_filt %>% filter(cod_prov == as.numeric(input$prov_sel_des))
      df_filt <- df_filt %>% mutate(territorio = Provincia)
    } else if (input$nivel_geo_des == "espana") {
      df_filt <- df_filt %>% mutate(territorio = "España")
    }
    
    # Filtrar según el tipo de brecha
    if (input$tipo_brecha_des == "genero") {
      df_filt <- df_filt %>% filter(genero != "Total")
      if (input$nivel_geo_des == "ccaa" && input$ccaa_sel_des == "todas") {
        # Para comparación de todas las CCAA: calcular porcentaje de mujeres en cada CCAA
        df_filt <- df_filt %>%
          group_by(territorio, genero) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(
            total_territorio = sum(valor),
            porcentaje = valor / total_territorio
          )
      } else {
        # Para un solo territorio: cálculo normal
        df_filt <- df_filt %>%
          group_by(territorio, genero) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(porcentaje = valor / sum(valor))
      }
    } 
    else if (input$tipo_brecha_des == "edad") {
      req(input$metrica_des == "paro")
      df_filt <- df_filt %>% filter(edad != "Total")
      if (input$nivel_geo_des == "ccaa" && input$ccaa_sel_des == "todas") {
        # Para comparación de todas las CCAA: calcular porcentaje de cada grupo de edad
        df_filt <- df_filt %>%
          group_by(territorio, edad) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(
            total_territorio = sum(valor),
            porcentaje = valor / total_territorio
          )
      } else {
        df_filt <- df_filt %>%
          group_by(territorio, edad) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(porcentaje = valor / sum(valor))
      }
    }
    else if (input$tipo_brecha_des == "tipo_contrato") {
      req(input$metrica_des == "contratos")
      df_filt <- df_filt %>% filter(tipo_contrato != "Total")
      if (input$nivel_geo_des == "ccaa" && input$ccaa_sel_des == "todas") {
        df_filt <- df_filt %>%
          group_by(territorio, tipo_contrato) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(
            total_territorio = sum(valor),
            porcentaje = valor / total_territorio
          )
      } else {
        df_filt <- df_filt %>%
          group_by(territorio, tipo_contrato) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(porcentaje = valor / sum(valor))
      }
    }
    else if (input$tipo_brecha_des == "sector") {
      df_filt <- df_filt %>% filter(sector != "Otros", sector != "Total")
      if (input$nivel_geo_des == "ccaa" && input$ccaa_sel_des == "todas") {
        df_filt <- df_filt %>%
          group_by(territorio, sector) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(
            total_territorio = sum(valor),
            porcentaje = valor / total_territorio
          )
      } else {
        df_filt <- df_filt %>%
          group_by(territorio, sector) %>%
          summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
          group_by(territorio) %>%
          mutate(porcentaje = valor / sum(valor))
      }
    }
    
    return(df_filt)
  })



  output$titulo_desigualdades <- renderText({
    nivel_texto <- switch(input$nivel_geo_des,
                         "espana" = "España",
                         "ccaa" = if(input$ccaa_sel_des == "todas") "Todas las CCAA" else paste("CCAA:", input$ccaa_especifica_des),
                         "provincia" = paste("Provincia:", input$prov_sel_des))
    
    tipo_texto <- switch(input$tipo_brecha_des,
                        "genero" = "Brecha de Género",
                        "edad" = "Distribución por Edad",
                        "tipo_contrato" = "Tipos de Contrato",
                        "sector" = "Estructura Sectorial")
    
    paste(tipo_texto, "-", nivel_texto, "- Año", input$anio_sel_des)
  })




  output$plot_brecha <- renderPlot({
    df_plot <- datos_brechas()
    req(df_plot)
    validate(need(nrow(df_plot) > 0, "No hay datos para esta selección"))
    
    # Determinar si estamos comparando todas las CCAA
    es_comparacion_ccaa <- (input$nivel_geo_des == "ccaa" && input$ccaa_sel_des == "todas")
    
    if (input$tipo_brecha_des == "genero") {
      # Ordenar si es comparación de todas las CCAA
      if (es_comparacion_ccaa) {
        # Ordenar territorios por porcentaje de mujeres
        porcentaje_mujeres <- df_plot %>%
          filter(genero == "Mujer") %>%
          arrange(porcentaje) %>%
          pull(territorio)
        
        df_plot$territorio <- factor(df_plot$territorio, levels = porcentaje_mujeres)
        
        p <- ggplot(df_plot, aes(x = territorio, y = porcentaje, fill = genero)) +
          geom_col(position = "fill", width = 0.7) +  # <-- aquí está el cambio
          coord_flip() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(values = c("Hombre" = "#91bfdb", "Mujer" = "#fc8d59")) +
          labs(title = "Comparación de Brecha de Género entre CCAA", 
              subtitle = paste("Porcentaje de mujeres contratadas - Año", input$anio_sel_des),
              x = "Comunidad Autónoma", 
              y = "Proporción",
              fill = "Género") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "top",
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(size = 12, color = "gray50"),
            axis.text.y = element_text(size = 11)
          )
      } else{
        # Gráfico normal para un solo territorio
        p <- ggplot(df_plot, aes(x = genero, y = porcentaje, fill = genero)) +
          geom_col(width = 0.6) +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(values = c("Hombre" = "#91bfdb", "Mujer" = "#fc8d59")) +
          labs(title = "Distribución por Género", 
               x = "Género", 
               y = "% sobre el total") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }






    else if (input$tipo_brecha_des == "edad") {
      if (es_comparacion_ccaa) {
        # Gráfico de comparación entre CCAA para grupos de edad
        # Calcular porcentaje de jóvenes (<25) para ordenar
        porcentaje_jovenes <- df_plot %>%
          filter(edad == "<25") %>%
          arrange(porcentaje) %>%
          pull(territorio)
        
        df_plot$territorio <- factor(df_plot$territorio, levels = porcentaje_jovenes)
        
        p <- ggplot(df_plot, aes(x = territorio, y = porcentaje, fill = edad)) +
          geom_col(position = "fill", width = 0.7) +
          coord_flip() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_brewer(palette = "Set2", name = "Grupo de Edad") +
          labs(title = "Distribución por Edad entre CCAA", 
               subtitle = paste("Composición por grupos de edad - Año", input$anio_sel_des),
               x = "Comunidad Autónoma", 
               y = "Proporción") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "top",
            plot.title = element_text(face = "bold", size = 16)
          )
      } else {
        p <- ggplot(df_plot, aes(x = edad, y = porcentaje, fill = edad)) +
          geom_col(width = 0.6) +
          scale_y_continuous(labels = scales::percent) +
          labs(title = "Distribución por Edad", 
               x = "Grupo de Edad", 
               y = "% sobre el total") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
    else if (input$tipo_brecha_des == "tipo_contrato") {
      if (es_comparacion_ccaa) {
        # Gráfico de comparación entre CCAA para tipos de contrato
        # Calcular porcentaje de contratos temporales para ordenar
        porcentaje_temporales <- df_plot %>%
          filter(tipo_contrato == "Temporal") %>%
          arrange(porcentaje) %>%
          pull(territorio)
        
        df_plot$territorio <- factor(df_plot$territorio, levels = porcentaje_temporales)
        
        p <- ggplot(df_plot, aes(x = territorio, y = porcentaje, fill = tipo_contrato)) +
          geom_col(position = "fill", width = 0.7) +
          coord_flip() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c("Temporal" = "#bdfc59", "Indefinido" = "#9eb7c7", "Convertido" = "#ffffbf"),
            name = "Tipo de Contrato"
          ) +
          labs(title = "Distribución por Tipo de Contrato entre CCAA", 
               subtitle = paste("Tasa de temporalidad - Año", input$anio_sel_des),
               x = "Comunidad Autónoma", 
               y = "Proporción") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "top",
            plot.title = element_text(face = "bold", size = 16)
          )
      } else {
        p <- ggplot(df_plot, aes(x = tipo_contrato, y = porcentaje, fill = tipo_contrato)) +
          geom_col(width = 0.6) +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c(
              "Temporal" = "#bdfc59",    # rojo anaranjado
              "Indefinido" = "#9eb7c7",  # azul celeste
              "Convertido" = "#ffffbf"   # amarillo
            ),
            name = "Tipo de Contrato"
          ) +
          labs(title = "Distribución por Tipo de Contrato", 
               x = "Tipo de Contrato", 
               y = "% sobre el total") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
    else if (input$tipo_brecha_des == "sector") {
      if (es_comparacion_ccaa) {
        # Gráfico de comparación entre CCAA para sectores
        p <- ggplot(df_plot, aes(x = territorio, y = porcentaje, fill = sector)) +
          geom_col(position = "fill", width = 0.7) +
          coord_flip() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_brewer(palette = "Set3", name = "Sector") +
          labs(title = "Estructura Sectorial entre CCAA", 
               subtitle = paste("Distribución por sectores - Año", input$anio_sel_des),
               x = "Comunidad Autónoma", 
               y = "Proporción") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "top",
            plot.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 10)
          )
      } else {
        p <- ggplot(df_plot, aes(x = reorder(sector, porcentaje), y = porcentaje, fill = sector)) +
          geom_col(width = 0.7) +
          coord_flip() +
          scale_y_continuous(labels = scales::percent) +
          labs(title = "Estructura Sectorial", 
               x = NULL, 
               y = "% sobre el total") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
    
    return(p)
  })

output$plot_ccaa_p1 <- plotly::renderPlotly({
  req(input$metrica_sel, input$rango_anios_p1)
  
  ccaa_sel <- ccaa_seleccionadas_tabla()
  if (length(ccaa_sel) == 0) {
    req(input$ccaa_p1)
    ccaa_sel <- input$ccaa_p1
  }
  
  # Datos anuales CCAA
  df_ccaa <- datos_ccaa() %>%
    filter(metrica == input$metrica_sel) %>%
    group_by(anio, CCAA) %>%
    summarise(valor = sum(valor, na.rm=TRUE), .groups="drop") %>%
    arrange(CCAA, anio) %>%
    group_by(CCAA) %>%
    mutate(yoy = (valor / lag(valor) - 1) * 100) %>%
    ungroup()
  
  # España anual
  df_es <- datos_espana() %>%
    filter(metrica == input$metrica_sel) %>%
    group_by(anio) %>%
    summarise(valor_es = sum(valor, na.rm=TRUE), .groups="drop") %>%
    arrange(anio) %>%
    mutate(yoy_es = (valor_es / lag(valor_es) - 1) * 100)
  
  df <- df_ccaa %>%
    left_join(df_es, by="anio") %>%
    mutate(div = yoy - yoy_es) %>%
    filter(anio >= input$rango_anios_p1[1], anio <= input$rango_anios_p1[2]) %>%
    filter(CCAA %in% ccaa_sel)
  
  df_spain <- df_es %>%
    transmute(anio = anio, valor = valor_es, yoy = yoy_es, div = 0, CCAA = "España")
  
  y <- switch(input$modo_p1, abs = "valor", yoy = "yoy", div = "div")
  
  # Etiqueta/tooltip personalizada
  df <- df %>%
    mutate(texto = paste0(
      "<b>", CCAA, "</b><br>",
      "Año: ", anio, "<br>",
      "Valor: ", round(.data[[y]], 2)
    ))
  
  df_spain <- df_spain %>%
    mutate(texto = paste0(
      "<b>España</b><br>",
      "Año: ", anio, "<br>",
      "Valor: ", round(.data[[y]], 2)
    ))
  
  df <- df %>% filter(!is.na(.data[[y]]))
  df_spain <- df_spain %>% filter(!is.na(.data[[y]]))
  
  p <- ggplot() +
    # España
    geom_line(
      data = df_spain,
      aes(x = anio, y = .data[[y]], group = CCAA, color = CCAA),
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    geom_point(
      data = df_spain,
      aes(x = anio, y = .data[[y]], color = CCAA),
      size = 1.3
    ) +
    # CCAA seleccionadas
    geom_line(
      data = df,
      aes(x = anio, y = .data[[y]], group = CCAA, color = CCAA),
      linewidth = 0.4
    ) +
    geom_point(
      data = df,
      aes(x = anio, y = .data[[y]], color = CCAA),
      size = 1
    ) +
    theme_minimal(base_size = 14) +
    labs(
      x = NULL,
      y = if (input$modo_p1=="abs") "Total anual"
      else if (input$modo_p1=="yoy") "Variación (%)"
      else "Divergencia vs España",
      color = NULL
    )
  
  plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(hovermode = "closest")

})

output$tabla_top_div_p1 <- DT::renderDT({
  df <- tabla_div_p1_data()
  req(df)

  DT::datatable(
    df,
    rownames = FALSE,
    selection = list(mode = "multiple", selected = c(1)),
    options = list(
      pageLength = 8,
      scrollX = TRUE
    )
  )
})

}

###################################################################
### LANZAMIENTO DE LA APP ######
###################################################################

shinyApp(ui = ui, server = server)



