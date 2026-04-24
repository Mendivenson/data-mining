library(rvest)
library(httr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)

# Parámetro principal del scraping
year_objetivo <- 2025

# Vector con los nombres de los meses tal como aparecen en la URL
meses <- c(
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december"
)

# Número de días por mes para el año objetivo
dias_mes <- c(
  january = 31,
  february = 28,
  march = 31,
  april = 30,
  may = 31,
  june = 30,
  july = 31,
  august = 31,
  september = 30,
  october = 31,
  november = 30,
  december = 31
)

# Encabezados para simular una navegación desde un navegador real
headers_scraping <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
  `Referer` = "https://www.accuweather.com/"
)

# Construimos la URL mensual para Bogotá y el año deseado
construir_url_mes <- function(mes, year_objetivo) {
  paste0(
    "https://www.accuweather.com/en/co/bogota/107487/",
    mes, "-weather/107487?year=", year_objetivo
  )
}

# Extraemos y limpiamos la información diaria de un mes
extraer_datos_mes <- function(mes, year_objetivo, dias_mes, meses, handle_web, headers_scraping) {
  url <- construir_url_mes(mes, year_objetivo)

  # Informamos qué URL se está consultando
  cat("\nConsultando:", url, "\n")

  # Realizamos la petición HTTP reutilizando la misma sesión
  res <- GET(url, handle = handle_web, headers_scraping)

  # Si la respuesta no es exitosa, devolvemos NULL para saltar el mes
  if (status_code(res) != 200) {
    cat("Saltando", mes, "- HTTP", status_code(res), "\n")
    return(NULL)
  }

  # Leemos el HTML de la página del mes
  pagina <- read_html(content(res, "text", encoding = "UTF-8"))

  # Seleccionamos los paneles diarios del calendario mensual
  dias <- pagina %>% 
          html_elements("a.monthly-daypanel")

  # Extraemos día, temperatura alta y temperatura baja de cada panel
  datos_mes <- map_dfr(dias, function(x) {
    tibble(
      dia = x %>% 
        html_element(".date") %>% 
        html_text2(),
      
      high = x %>% 
        html_element(".high") %>% 
        html_text2(),
      
      low = x %>% 
        html_element(".low") %>% 
        html_text2()
    )
  })

  # Convertimos las variables de texto a valores numéricos
  datos_mes <- datos_mes %>%
    mutate(
      dia = as.integer(dia),
      high = as.integer(str_extract(high, "\\d+")),
      low = as.integer(str_extract(low, "\\d+"))
    )

  # Ubicamos el inicio del mes real en el calendario extraído
  inicio <- which(datos_mes$dia == 1)[1]

  # Si no aparece el día 1, se omite el mes
  if (is.na(inicio)) {
    cat("No se encontró el primer día 1 en", mes, "\n")
    return(NULL)
  }

  # Recuperamos cuántos días debe tener el mes
  n_dias <- dias_mes[[mes]]

  # Conservamos solo los días del mes objetivo y construimos la fecha completa
  datos_mes %>%
    slice(inicio:(inicio + n_dias - 1)) %>%
    mutate(
      mes = mes,
      year = year_objetivo,
      fecha = seq.Date(
        from = as.Date(sprintf("%d-%02d-01", year_objetivo, match(mes, meses))),
        by = "day",
        length.out = n_dias
      )
    ) %>%
    select(fecha, dia, high, low, mes, year)
}

# Creamos una sesión reutilizable y una lista para almacenar resultados
h <- handle("https://www.accuweather.com")
resultados <- list()

# Recorremos todos los meses del año objetivo
for (mes in meses) {
  resultados[[mes]] <- extraer_datos_mes(
    mes = mes,
    year_objetivo = year_objetivo,
    dias_mes = dias_mes,
    meses = meses,
    handle_web = h,
    headers_scraping = headers_scraping
  )

  # Hacemos una pausa aleatoria para no saturar el sitio
  Sys.sleep(sample(15:30, 1))
}

# Unimos la información de todos los meses en una sola tabla
datos_final <- bind_rows(resultados)

# Mostramos únicamente fecha, temperatura máxima y mínima
datos_final %>%
  dplyr::select(fecha, high, low) %>%
  knitr::kable()
