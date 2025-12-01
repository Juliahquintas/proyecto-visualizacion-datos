# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(httr)
library(scales) 
library(shinyjs) 

# Definir la lista de años al inicio
anios <- 2010:2024
anios_titulos <- paste(min(anios), max(anios), sep = "–")

# --------------------------------------------------------------------------------------
# ---------- 1. FUNCIÓN PARA LEER UN FICHERO DEL SEPE ----------
# --------------------------------------------------------------------------------------
leer_sepe_csv <- function(ruta) {

  if (!file.exists(ruta)) {
    warning(sprintf("Archivo no encontrado: %s", ruta))
    return(NULL)
  }

  # 1. Leer el CSV sin convertir a factors
  df <- tryCatch(
    read.csv(
      ruta,
      sep = ";",
      fileEncoding = "latin1",
      header = TRUE,
      skip = 1,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      message(sprintf("Error leyendo %s : %s", basename(ruta), e$message))
      return(NULL)
    }
  )

  if (is.null(df)) return(NULL)

  # 2. Detectar columnas numéricas según el CSV original (las que contienen números)
  es_col_numerica <- sapply(df, function(x) {
    any(grepl("[0-9]", x, perl = TRUE), na.rm = TRUE)
  })

  cols_num <- names(df)[es_col_numerica]

  # 3. Reemplazar "<5" por "0" SOLO en las columnas numéricas
  df[cols_num] <- lapply(df[cols_num], function(col) {
    col_chr <- as.character(col)
    col_chr[col_chr == "<5"] <- "0"
    col_chr
  })

  # 4. Convertir esas columnas a numeric (CORRECCIÓN: manejo de coma decimal)
  df[cols_num] <- lapply(df[cols_num], function(col) {
    # El punto decimal es una coma en estos CSVs del SEPE, se añade gsub para corregir
    col_corrected <- gsub(",", ".", as.character(col), fixed = TRUE)
    suppressWarnings(as.numeric(col_corrected))
  })

  df
}

# --------------------------------------------------------------------------------------
# ---------- 1b. CORREGIR LOS NOMBRES DE LAS CCAA ----------
# --------------------------------------------------------------------------------------
normalizar_ccaa <- function(x) {
  x <- trimws(x)    # quita espacios delante/detrás

  x <- gsub("–", "-", x)      # guiones raros -> guion normal
  x <- gsub("—", "-", x)
  x <- gsub("−", "-", x)
  x <- gsub("  +", " ", x)    # varios espacios -> uno

  # casos específicos
  x <- gsub("Castilla *- *La Mancha", "Castilla-La Mancha", x)
  x <- gsub("Castilla *La Mancha", "Castilla-La Mancha", x)

  x
}

# --------------------------------------------------------------------------------------
# ---------- PRE 2. DESCARGA DEL DATASET (Se ejecuta al cargar el script) ----------
# --------------------------------------------------------------------------------------

# Carpetas destino (Definiciones globales usadas por el bucle de descarga y el server)
dir_data <- file.path("data")
dir_contratos <- file.path(dir_data, "contratos")
dir_paro      <- file.path(dir_data, "paro")
dir_dtes_empleo <- file.path(dir_data, "dtes_empleo")

dir.create(dir_contratos, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_paro, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_dtes_empleo, recursive = TRUE, showWarnings = FALSE)

# Función auxiliar para descargar si existe
descargar_si_existe <- function(url, destfile) {
  # (Lógica de descarga idéntica a la anterior para mantener funcionalidad)
  res_head <- tryCatch(httr::HEAD(url, timeout(10)), error = function(e) NULL)
  status <- NULL
  if (!is.null(res_head)) status <- httr::status_code(res_head)

  if (is.null(status) || status >= 400) {
    res_get <- tryCatch(httr::GET(url, httr::progress(), httr::write_disk(destfile, overwrite = TRUE), timeout(60)),
                        error = function(e) e)
    if (inherits(res_get, "error")) {
      message(sprintf("No se pudo descargar %s : %s", basename(destfile), res_get$message))
      if (file.exists(destfile)) file.remove(destfile)
      return(FALSE)
    } else {
      status_get <- httr::status_code(res_get)
      if (status_get >= 400) {
        message(sprintf("No disponible (HTTP %s): %s", status_get, basename(destfile)))
        if (file.exists(destfile)) file.remove(destfile)
        return(FALSE)
      } else {
        message(sprintf("Descargado: %s", basename(destfile)))
        return(TRUE)
      }
    }
  } else {
    res <- tryCatch(httr::GET(url, httr::progress(), httr::write_disk(destfile, overwrite = TRUE), timeout(60)),
                    error = function(e) e)
    if (inherits(res, "error")) {
      message(sprintf("Error descargando %s : %s", basename(destfile), res$message))
      if (file.exists(destfile)) file.remove(destfile)
      return(FALSE)
    } else {
      st <- httr::status_code(res)
      if (st >= 400) {
        message(sprintf("No disponible (HTTP %s): %s", st, basename(destfile)))
        if (file.exists(destfile)) file.remove(destfile)
        return(FALSE)
      } else {
        message(sprintf("Descargado: %s", basename(destfile)))
        return(TRUE)
      }
    }
  }
}

# Base URL
base_contratos <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Contratos_por_municipios_%s_csv.csv"
base_paro      <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_%s_csv.csv"
base_dtes_empleo <- "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Dtes_empleo_por_municipios_%s_csv.csv"

# Bucle de descarga
for (ano in anios) {
  # contratos
  url_c <- sprintf(base_contratos, ano)
  dest_c <- file.path(dir_contratos, sprintf("Contratos_por_municipios_%s_csv.csv", ano))
  if (!file.exists(dest_c)) {
    message(sprintf("Comprobando contratos %s ...", ano))
    descargar_si_existe(url_c, dest_c)
  } else {
    message(sprintf("Ya existe: %s", dest_c))
  }

  # paro
  url_p <- sprintf(base_paro, ano)
  dest_p <- file.path(dir_paro, sprintf("Paro_por_municipios_%s_csv.csv", ano))
  if (!file.exists(dest_p)) {
    message(sprintf("Comprobando paro %s ...", ano))
    descargar_si_existe(url_p, dest_p)
  } else {
    message(sprintf("Ya existe: %s", dest_p))
  }

  # demandantes de empleo
  url_d <- sprintf(base_dtes_empleo, ano)
  dest_d <- file.path(dir_dtes_empleo, sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano))
  if (!file.exists(dest_d)) {
    message(sprintf("Comprobando demandantes de empleo %s ...", ano))
    descargar_si_existe(url_d, dest_d)
  } else {
    message(sprintf("Ya existe: %s", dest_d))
  }
}


# --------------------------------------------------------------------------------------
# ---------- 3. UI (AJUSTADA) ----------
# --------------------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(), # Inicializar shinyjs para las funciones de JS
  titlePanel(paste("Datos de Empleo SEPE por Comunidad Autónoma (", anios_titulos, ")")),

  sidebarLayout(
    sidebarPanel(
      h4("Filtros"),
      # El selectInput se inicializará con NULL y se actualizará en el server
      selectInput(
        "ccaa_sel",
        "Comunidad Autónoma:",
        choices  = NULL, # Se inicializa en NULL y se actualiza reactivamente
        selected = NULL,
        multiple = FALSE
      ),
      # Nuevo selector de métrica
      selectInput(
        "metrica_sel",
        "Métrica a visualizar:",
        choices = c("Paro Registrado" = "paro",
                    "Contratos Registrados" = "contratos",
                    "Demandantes de Empleo" = "dtes"),
        selected = "paro",
        multiple = FALSE
      )
    ),

    mainPanel(
      # Se actualiza el título y la descripción para ser dinámicos
      h3(textOutput("titulo_grafico1")),
      p("Evolución de la métrica seleccionada para la Comunidad Autónoma elegida."),
      plotOutput("plot_bar_ccaa", height = "350px"),
      tags$hr(),
      h3(textOutput("titulo_grafico2")),
      p("Vista conjunta (Heatmap) de la métrica seleccionada por Comunidad y año."),
      plotOutput("plot_heatmap", height = "500px"),
      tags$hr(),
      h4("Info de Carga de Datos (Debug)"),
      pre(textOutput("availability_info")) # Info para el usuario sobre la carga de datos
    )
  )
)


# --------------------------------------------------------------------------------------
# ---------- 4. SERVER (REFACTORIZADO Y CORREGIDO) ----------
# --------------------------------------------------------------------------------------
server <- function(input, output, session) {

  # --- función auxiliar: agregar por CCAA para un CSV ya leído
  agregador_ccaa <- function(df_raw, patrones_busqueda = c("paro", "contrat", "demand", "dtes", "total")) {
    # Depende de la función global: normalizar_ccaa
    if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) return(NULL)

    # normalizar columna comunidad si existe
    if ("Comunidad Autónoma" %in% names(df_raw)) {
      df_raw$`Comunidad Autónoma` <- normalizar_ccaa(df_raw$`Comunidad Autónoma`)
    } else {
      warning("No encontrada la columna 'Comunidad Autónoma' en un fichero.")
      return(NULL)
    }

    # detectar columna numérica relevante por nombre
    numeric_cols <- names(df_raw)[sapply(df_raw, is.numeric)]
    candidate_cols <- names(df_raw)[sapply(names(df_raw), function(n) {
      any(vapply(patrones_busqueda, function(p) grepl(p, n, ignore.case = TRUE), logical(1)))
    }) & names(df_raw) %in% numeric_cols]

    # PRIORIDAD: Usar "total Paro Registrado" o "Total" si existe, sino el primer candidato
    chosen <- NULL
    if ("total Paro Registrado" %in% candidate_cols) {
      chosen <- "total Paro Registrado"
    } else if ("Total" %in% candidate_cols) {
      chosen <- "Total"
    } else if (length(candidate_cols) > 0) {
      chosen <- candidate_cols[1]
    } else if (length(numeric_cols) > 0) {
      chosen <- numeric_cols[1]
    }

    if (is.null(chosen)) {
       warning("No se ha podido identificar la columna de valor para agregación.")
       return(NULL)
    }

    # comprobar que existe Código mes para extraer año
    if (!("Código mes" %in% names(df_raw))) {
      warning("No hay columna 'Código mes' para extraer el año; se omite este fichero.")
      return(NULL)
    }

    df_raw %>%
      mutate(anio = suppressWarnings(as.integer(substr(`Código mes`, 1, 4)))) %>%
      filter(!is.na(anio)) %>%
      group_by(anio, comunidad = `Comunidad Autónoma`) %>%
      summarise(valor = mean(.data[[chosen]], na.rm = TRUE), .groups = "drop")
  }

  # --- función para leer un CSV con manejo de errores (safe_read) ---
  safe_read <- function(ruta) {
    # Depende de la función global: leer_sepe_csv
    if (!file.exists(ruta)) {
      message(sprintf("Archivo no encontrado: %s", basename(ruta)))
      return(NULL)
    }
    leer_sepe_csv(ruta)
  }

  # --- 1. CARGA DE DATOS (Módulo reactivo para cargar una sola vez) ---
  datos_agregados <- reactive({
    # Depende de la variable global: anios
    # Depende de las variables globales: dir_contratos, dir_paro, dir_dtes_empleo

    contratos_list <- list()
    paro_list <- list()
    dtes_list <- list()

    withProgress(message = 'Cargando y agregando datos...', value = 0, {
      n_anios <- length(anios)
      for (i in seq_along(anios)) {
        ano <- anios[i]
        setProgress(i/n_anios, detail = paste("Procesando año", ano))

        # Rutas (usan las variables globales de carpeta)
        ruta_contratos <- file.path("data", "contratos", sprintf("Contratos_por_municipios_%s_csv.csv", ano))
        ruta_paro      <- file.path("data", "paro", sprintf("Paro_por_municipios_%s_csv.csv", ano))
        ruta_dtes      <- file.path("data", "dtes_empleo", sprintf("Dtes_empleo_por_municipios_%s_csv.csv", ano))

        # Contratos
        df_c <- safe_read(ruta_contratos)
        if (!is.null(df_c)) {
          agg_c <- agregador_ccaa(df_c, patrones_busqueda = c("contrat", "contrato", "total"))
          if (!is.null(agg_c)) contratos_list[[as.character(ano)]] <- agg_c %>% rename(contratos_total = valor)
        }

        # Paro
        df_p <- safe_read(ruta_paro)
        if (!is.null(df_p)) {
          agg_p <- agregador_ccaa(df_p, patrones_busqueda = c("paro", "total"))
          if (!is.null(agg_p)) paro_list[[as.character(ano)]] <- agg_p %>% rename(paro_total = valor)
        }

        # Demandantes de empleo (Dtes)
        df_d <- safe_read(ruta_dtes)
        if (!is.null(df_d)) {
          agg_d <- agregador_ccaa(df_d, patrones_busqueda = c("demand", "dtes", "demandant", "total"))
          if (!is.null(agg_d)) dtes_list[[as.character(ano)]] <- agg_d %>% rename(dtes_total = valor)
        }
      }
    })

    # Combinar listas y unir en un solo data frame largo
    contratos_ccaa <- if (length(contratos_list) > 0) bind_rows(contratos_list) else tibble::tibble(anio = integer(), comunidad = factor(), contratos_total = numeric())
    paro_ccaa_local <- if (length(paro_list) > 0) bind_rows(paro_list) else tibble::tibble(anio = integer(), comunidad = factor(), paro_total = numeric())
    dtes_ccaa <- if (length(dtes_list) > 0) bind_rows(dtes_list) else tibble::tibble(anio = integer(), comunidad = factor(), dtes_total = numeric())

    # Unión completa de las 3 métricas
    df_merged <- paro_ccaa_local %>%
      full_join(contratos_ccaa, by = c("anio", "comunidad")) %>%
      full_join(dtes_ccaa, by = c("anio", "comunidad"))

    # Convertir comunidad a factor y anio a integer una vez
    if (nrow(df_merged) > 0) {
        df_merged$comunidad <- as.factor(df_merged$comunidad)
        df_merged$anio <- as.integer(df_merged$anio)
    }

    return(df_merged)
  })


  # --- 2. ACTUALIZACIÓN DE LA UI CON DATOS CARGADOS ---
  observeEvent(datos_agregados(), {
      df <- datos_agregados()
      ccaa_choices <- sort(unique(df$comunidad))

      # Actualizar selectInput para CCAA
      updateSelectInput(
          session,
          "ccaa_sel",
          choices = ccaa_choices,
          selected = if (length(ccaa_choices) > 0) ccaa_choices[1] else NULL
      )

      # Actualizar el título de la UI (solo si hay datos)
      if (nrow(df) > 0) {
          anios_presentes <- unique(df$anio)
          if (length(anios_presentes) > 0) {
              # Se usa shinyjs::runjs para actualizar el título del navegador
              new_title <- paste("Datos de Empleo SEPE por Comunidad Autónoma (", min(anios_presentes), "–", max(anios_presentes), ")")
              shinyjs::runjs(paste0("document.title = '", new_title, "';"))
          }
      }
  })

  # --- 3. DATOS REACTIVOS FILTRADOS ---
  datos_plot <- reactive({
      df_merged <- datos_agregados()
      req(nrow(df_merged) > 0)

      metrica_col <- paste0(input$metrica_sel, "_total")

      if (!(metrica_col %in% names(df_merged))) {
          return(NULL)
      }

      df_long <- df_merged %>%
          select(anio, comunidad, valor = !!metrica_col) %>%
          drop_na(valor)

      return(df_long)
  })

  # Título de la métrica seleccionada (para gráficos)
  titulo_metrica <- reactive({
      switch(input$metrica_sel,
             "paro" = "Paro Registrado",
             "contratos" = "Contratos Registrados",
             "dtes" = "Demandantes de Empleo",
             "Métrica Desconocida")
  })


  # ---- Títulos Dinámicos ----
  output$titulo_grafico1 <- renderText({
      paste("Gráfico 1: Evolución de", titulo_metrica(), "en una CCAA")
  })
  output$titulo_grafico2 <- renderText({
      paste("Gráfico 2:", titulo_metrica(), "por año y CCAA (vista conjunta)")
  })


  # ---- Gráfico 1: barras por año para una CCAA ----
  output$plot_bar_ccaa <- renderPlot({
    # Depende del paquete 'scales'
    df_long <- datos_plot()
    req(df_long)
    req(input$ccaa_sel)

    df_ccaa <- df_long %>%
      filter(comunidad == input$ccaa_sel) %>%
      arrange(anio)

    validate(
      need(nrow(df_ccaa) > 0, paste("No hay datos de", titulo_metrica(), "para la comunidad seleccionada."))
    )

    df_ccaa$anio_f <- factor(df_ccaa$anio, levels = sort(unique(df_ccaa$anio)))

    ggplot(df_ccaa, aes(x = anio_f, y = valor)) +
      geom_col(width = 0.7, fill = "steelblue") +
      geom_text(aes(label = format(round(valor, 0), big.mark = ".", decimal.mark = ",")),
                position = position_dodge(width = 0.7),
                vjust = -0.5, size = 3) +
      labs(
        x = "Año",
        y = paste(titulo_metrica(), "(media anual)"),
        title = paste("Evolución de", titulo_metrica(), "en", input$ccaa_sel)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(face = "bold")
      )
  })

  # ---- Gráfico 2: heatmap año × CCAA ----
  output$plot_heatmap <- renderPlot({
    df_long <- datos_plot()
    req(df_long)

    years_levels <- sort(unique(df_long$anio), decreasing = TRUE)
    df_long$anio_f <- factor(df_long$anio, levels = years_levels)
    df_long$comunidad_f <- factor(df_long$comunidad, levels = sort(unique(df_long$comunidad)))


    ggplot(df_long,
           aes(x = comunidad_f,
               y = anio_f,
               fill = valor)) +
      geom_tile(color = "white") +
      # Ajuste del color: rojo para paro (más alto = peor), azul para contratos (más alto = mejor)
      scale_fill_gradient(
          low = switch(input$metrica_sel, "paro" = "white", "steelblue1"),
          high = switch(input$metrica_sel, "paro" = "firebrick", "darkblue"),
          name = paste(titulo_metrica(), "(media anual)")
      ) +
      labs(
        x = "Comunidad Autónoma",
        y = "Año",
        title = paste("Distribución de", titulo_metrica(), "por Comunidad Autónoma y año")
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank()
      )
  })

  # --- Info de Carga de Datos (Debug) ---
  output$availability_info <- renderText({
    df <- datos_agregados()
    if (nrow(df) == 0) {
      return("Error: No se han cargado datos de SEPE. Revisa las rutas de los archivos.")
    }

    n_ccaa <- length(unique(df$comunidad))
    
    # Se añade la comprobación de columna antes de llamar a sum/unique
    
    sum_paro <- if ("paro_total" %in% names(df)) sum(!is.na(df$paro_total)) else 0
    sum_contratos <- if ("contratos_total" %in% names(df)) sum(!is.na(df$contratos_total)) else 0
    sum_dtes <- if ("dtes_total" %in% names(df)) sum(!is.na(df$dtes_total)) else 0
    
    avg_paro <- if (n_ccaa > 0) sum_paro / n_ccaa else 0
    avg_contratos <- if (n_ccaa > 0) sum_contratos / n_ccaa else 0
    avg_dtes <- if (n_ccaa > 0) sum_dtes / n_ccaa else 0
    
    anios_presentes <- if (nrow(df) > 0) paste(sort(unique(df$anio)), collapse = ", ") else "Ninguno"
    
    lines <- c()
    lines <- c(lines, sprintf("Años disponibles: %s", anios_presentes))
    lines <- c(lines, sprintf("Comunidades autónomas únicas: %d", n_ccaa))
    lines <- c(lines, sprintf("Paro (registros): %d (≈ %.0f años por CCAA)", sum_paro, avg_paro))
    lines <- c(lines, sprintf("Contratos (registros): %d (≈ %.0f años por CCAA)", sum_contratos, avg_contratos))
    lines <- c(lines, sprintf("Demandantes (registros): %d (≈ %.0f años por CCAA)", sum_dtes, avg_dtes))
    paste(lines, collapse = "\n")
  })
}

# Línea final para ejecutar la aplicación
shinyApp(ui = ui, server = server)