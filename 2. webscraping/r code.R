knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 5
)
options(stringsAsFactors = FALSE)

mostrar_tabla <- function(x, n = 10, caption = NULL) {
  x_mostrar <- head(x, n)

  if (knitr::is_html_output()) {
    tabla_html <- knitr::kable(
      x_mostrar,
      format = "html",
      caption = caption,
      table.attr = 'class="table table-striped table-hover table-condensed"'
    )

    knitr::asis_output(
      paste0('<div class="table-scroll">', as.character(tabla_html), '</div>')
    )
  } else {
    knitr::kable(
      x_mostrar,
      format = "latex",
      caption = caption,
      booktabs = TRUE
    )
  }
}

paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr"
)

instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

invisible(lapply(paquetes, library, character.only = TRUE))

termino_busqueda <- "laptop"
url_amazon <- paste0("https://www.amazon.com/s?k=", URLencode(termino_busqueda))
url_amazon

pagina_simple <- read_html(url_amazon)
pagina_simple

pagina_simple %>%
  html_elements(".a-row .a-price-whole") %>%
  html_text2()

# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal
respuesta_amazon <- request(url_amazon) %>%
                    req_user_agent(user_agent_navegador) %>%
                    req_headers(`accept-language` = "en-US,en;q=0.9") %>%
                    req_perform()

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
pagina_amazon <- respuesta_amazon %>%
                 resp_body_html()

# Mostramos el objeto HTML obtenido
pagina_amazon

productos <- pagina_amazon %>%
  html_elements(".s-result-item[data-component-type='s-search-result']")

length(productos)

# Definimos una función que recibe el nodo HTML de un producto
extraer_producto_amazon <- function(nodo) {
  # Extraemos el título visible del producto
  titulo <- nodo %>%
    html_element("h2 span") %>%
    html_text2()

  # Extraemos el enlace relativo hacia la página del producto
  enlace_relativo <- nodo %>%
    html_element("h2 a") %>%
    html_attr("href")

  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".a-price .a-offscreen") %>%
    html_text2()

  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  rating <- nodo %>%
    html_element(".a-icon-alt") %>%
    html_text2()

  # Extraemos el número de reseñas o valoraciones asociadas al producto
  n_resenas <- nodo %>%
    html_element("[aria-label$='ratings'], .s-link-style .s-underline-text") %>%
    html_text2()

  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    # Si el enlace no existe, dejamos un valor faltante; si existe, construimos la URL completa
    enlace = ifelse(
      is.na(enlace_relativo),
      NA_character_,
      paste0("https://www.amazon.com", enlace_relativo)
    ),
    precio = precio,
    rating = rating,
    n_resenas = n_resenas
  )
}

tabla_productos <- map_dfr(productos, extraer_producto_amazon) %>%
                   clean_names()

mostrar_tabla(tabla_productos, n = 10, caption = "Primeros resultados extraídos desde Amazon")

tabla_productos_limpia <- tabla_productos %>%
  mutate(
    titulo = str_squish(titulo),
    precio = str_remove_all(precio, "[$,]"),
    precio = suppressWarnings(as.numeric(precio)),
    rating = str_extract(rating, "[0-9]+\\.?[0-9]*"),
    rating = suppressWarnings(as.numeric(rating)),
    n_resenas = str_remove_all(n_resenas, "[,]"),
    n_resenas = str_extract(n_resenas, "[0-9]+"),
    n_resenas = suppressWarnings(as.integer(n_resenas))
  ) %>%
  filter(!is.na(titulo), titulo != "")

mostrar_tabla(tabla_productos_limpia, n = 10, caption = "Resultados con limpieza básica")

resumen_na <- tibble(
  variable = names(tabla_productos_limpia),
  n_na = sapply(tabla_productos_limpia, function(x) sum(is.na(x)))
)

mostrar_tabla(resumen_na, n = nrow(resumen_na), caption = "Valores faltantes por variable")

construir_url_amazon <- function(termino, pagina = 1) {
  paste0(
    "https://www.amazon.com/s?k=", URLencode(termino),
    "&page=", pagina
  )
}

construir_url_amazon("laptop", 3)

# Definimos una función para scrapear una página específica de resultados en Amazon
scrapear_pagina_amazon <- function(termino, pagina = 1, pausa = 2) {
  # Construimos la URL de búsqueda usando el término y el número de página
  url <- construir_url_amazon(termino, pagina)

  # Introducimos una pausa para no hacer solicitudes demasiado seguidas
  Sys.sleep(pausa)

  # Enviamos la solicitud HTTP usando un user-agent y
  # encabezados similares a los del navegador
  respuesta <- request(url) %>%
    req_user_agent(user_agent_navegador) %>%
    req_headers(`accept-language` = "en-US,en;q=0.9") %>%
    req_perform()

  # Convertimos la respuesta recibida en un documento HTML analizable
  pagina_html <- respuesta %>%
    resp_body_html()

  # Seleccionamos los nodos que corresponden a productos 
  #reales en la página de resultados
  nodos <- pagina_html %>%
    html_elements(".s-result-item[data-component-type='s-search-result']")

  # Extraemos la información de cada producto, limpiamos nombres 
  #de variables y añadimos la página de origen
  map_dfr(nodos, extraer_producto_amazon) %>%
    clean_names() %>%
    mutate(pagina = pagina)
}

paginas_a_consultar <- 1:3

productos_varias_paginas <- map_dfr(
  paginas_a_consultar,
  ~ scrapear_pagina_amazon(termino = "laptop", pagina = .x, pausa = 2)
)

productos_varias_paginas <- productos_varias_paginas %>%
  mutate(
    titulo = str_squish(titulo),
    precio = str_remove_all(precio, "[$,]"),
    precio = suppressWarnings(as.numeric(precio)),
    rating = str_extract(rating, "[0-9]+\\.?[0-9]*"),
    rating = suppressWarnings(as.numeric(rating)),
    n_resenas = str_remove_all(n_resenas, "[,]"),
    n_resenas = str_extract(n_resenas, "[0-9]+"),
    n_resenas = suppressWarnings(as.integer(n_resenas))
  )

mostrar_tabla(productos_varias_paginas, n = 15, caption = "Productos obtenidos desde varias páginas")

resumen_productos <- productos_varias_paginas %>%
  summarise(
    n_registros = n(),
    n_titulos_unicos = n_distinct(titulo, na.rm = TRUE),
    precio_promedio = mean(precio, na.rm = TRUE),
    rating_promedio = mean(rating, na.rm = TRUE)
  )

mostrar_tabla(resumen_productos, n = 1, caption = "Resumen exploratorio")

if (!requireNamespace("RSelenium", quietly = TRUE)) {
  install.packages("RSelenium")
}

library(RSelenium)

chrome_bin <- Sys.which("google-chrome")
chromedriver_bin <- Sys.which("chromedriver")
chrome_version <- tryCatch(
  system2(chrome_bin, "--version", stdout = TRUE),
  error = function(e) "versión de Chrome no disponible"
)

if (!nzchar(chrome_bin)) {
  stop("No se encontró Google Chrome en el PATH del sistema.")
}

if (!nzchar(chromedriver_bin)) {
  stop(
    paste0(
      "No se encontró `chromedriver` en el PATH del sistema. ",
      "Chrome detectado: ", chrome_version, ". ",
      "Instale un ChromeDriver compatible y vuelva a ejecutar la sección de Selenium."
    )
  )
}

rD <- rsDriver(
  browser = "chrome",
  chromever = NULL,
  phantomver = NULL,
  verbose = FALSE,
  port = 4567L
)

remDr <- rD$client
remDr$open()

url_busqueda <- construir_url_amazon("laptop", 1)
remDr$navigate(url_busqueda)
Sys.sleep(5)

html_renderizado <- remDr$getPageSource()[[1]]
pagina_renderizada <- read_html(html_renderizado)

nodos_renderizados <- pagina_renderizada %>%
  html_elements(".s-result-item[data-component-type='s-search-result']")

length(nodos_renderizados)

# Repetimos varias veces la acción de bajar hasta el final de la página
for (i in 1:5) {
  # Ejecutamos JavaScript en el navegador para hacer scroll hacia abajo
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  # Esperamos un momento para permitir que cargue nuevo contenido dinámico
  Sys.sleep(5)
}

# Obtenemos el HTML actualizado después del scroll
html_scroll <- remDr$getPageSource()[[1]]
# Convertimos ese HTML renderizado en un documento analizable con R
pagina_scroll <- read_html(html_scroll)

# Extraemos los productos visibles después del scroll y los 
#unimos en una tabla
productos_scroll <- pagina_scroll %>%
  html_elements(".s-result-item[data-component-type='s-search-result']") %>%
  map_dfr(extraer_producto_amazon)

# Mostramos una parte de los resultados obtenidos después de 
#la interacción dinámica
mostrar_tabla(productos_scroll, n = 10, caption = "Resultados después del scroll")

# Creamos una lista vacía para guardar los resultados de cada página
resultados_selenium <- list()

# Recorremos varias páginas de resultados una por una
for (p in 1:3) {
  # Abrimos en el navegador automatizado la URL correspondiente a 
  #la página p
  remDr$navigate(construir_url_amazon("laptop", p))
  # Esperamos unos segundos para que la página termine de cargar
  Sys.sleep(4)

  # Recuperamos el HTML renderizado de la página actual
  html_actual <- remDr$getPageSource()[[1]]
  # Convertimos el HTML obtenido en un documento que podamos analizar 
  #con R
  pagina_actual <- read_html(html_actual)

  # Extraemos los productos visibles de esa página y añadimos el 
  #número de página
  tabla_pagina <- pagina_actual %>%
    html_elements(".s-result-item[data-component-type='s-search-result']") %>%
    map_dfr(extraer_producto_amazon) %>%
    mutate(pagina = p)

  # Guardamos la tabla de la página actual dentro de la lista de resultados
  resultados_selenium[[p]] <- tabla_pagina
}

amazon_selenium_paginas <- bind_rows(resultados_selenium)
mostrar_tabla(amazon_selenium_paginas, n = 15, caption = "Resultados extraídos con Selenium")

remDr$close()
rD$server$stop()

scrapear_pagina_segura <- purrr::possibly(
  scrapear_pagina_amazon,
  otherwise = tibble(
    titulo = NA_character_,
    enlace = NA_character_,
    precio = NA_character_,
    rating = NA_character_,
    n_resenas = NA_character_,
    pagina = NA_integer_
  )
)

resultado_seguro <- map_dfr(1:2, 
                            ~scrapear_pagina_segura("laptop", .x, pausa = 2))
mostrar_tabla(resultado_seguro, 
              n = 10, 
              caption = "Extracción segura con manejo de errores")

write_csv(productos_varias_paginas, "amazon_laptop_resultados.csv")
