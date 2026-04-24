# =============================================================================
# Taller práctico de Web Scraping con R — Parte 1
# Curso de Minería de Datos
# Universidad Nacional de Colombia — Departamento de Estadística, Sede Bogotá
# Autor: Andrés Felipe Flórez Rivera
# =============================================================================


# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------
# 4. LEER UNA PÁGINA WEB
# -----------------------------------------------------------------------------

# Cargamos los paquetes necesarios
paquetes <- c(
               "rvest", "xml2", "dplyr", "stringr",
               "purrr", "janitor", "readr", "knitr"
             )
# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
invisible(lapply(paquetes, library, character.only = TRUE))

# Usaremos un sitio hecho para practicar scraping: Quotes to Scrape
url_quotes <- "https://quotes.toscrape.com/"

# Leemos el HTML de la página
pagina_quotes <- read_html(url_quotes)
pagina_quotes

# Extraer el título de la página
pagina_quotes %>%
  html_element("title") %>%
  html_text2()

# Extraer todos los textos de las citas
citas <- pagina_quotes %>%
  html_elements(".quote .text") %>%
  html_text2()

citas

# Extraer los autores
autores <- pagina_quotes %>%
  html_elements(".quote .author") %>%
  html_text2()

autores

# Construir una tabla para almacenar las citas
tabla_citas <- tibble(
  cita = citas,
  autor = autores
)

mostrar_tabla(tabla_citas, n = 10, caption = "Primeras citas extraídas")


# -----------------------------------------------------------------------------
# 5. CITAS, AUTORES Y ETIQUETAS
# -----------------------------------------------------------------------------

nodos_citas <- pagina_quotes %>%
               html_elements(".quote")
length(nodos_citas)

# Extraer información por bloque
tabla_citas_completa <- map_dfr(nodos_citas, function(nodo) {
  cita <- nodo %>% html_element(".text") %>% html_text2()
  autor <- nodo %>% html_element(".author") %>% html_text2()
  tags <- nodo %>% html_elements(".tags .tag") %>% html_text2()

  tibble(
    cita = cita,
    autor = autor,
    tags = paste(tags, collapse = ", ")
  )
})

mostrar_tabla(tabla_citas_completa, n = 10, caption = "Citas, autores y etiquetas")


# -----------------------------------------------------------------------------
# 6. LIMPIEZA BÁSICA DEL TEXTO EXTRAÍDO
# -----------------------------------------------------------------------------

tabla_citas_limpia <- tabla_citas_completa %>%
  mutate(
    # Quitamos las comillas tipográficas de la cita
    cita = str_replace_all(cita, "[\u201c\u201d]", ""),
    # Eliminamos espacios sobrantes en el texto de la cita
    cita = str_squish(cita),
    # Limpiamos espacios innecesarios en el nombre del autor
    autor = str_squish(autor),
    # Calculamos cuántas etiquetas tiene cada cita
    n_tags = if_else(tags == "", 0L, str_count(tags, ",") + 1L)
  )

mostrar_tabla(tabla_citas_limpia, n = 10, caption = "Citas limpias con número de etiquetas")

# Resumen rápido
tabla_citas_limpia %>%
  count(autor, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Autores con más citas")


# -----------------------------------------------------------------------------
# 7. NAVEGAR VARIAS PÁGINAS
# -----------------------------------------------------------------------------

# Crear vector de URLs
urls_paginas_quotes <- c(
  "https://quotes.toscrape.com/page/1/",
  "https://quotes.toscrape.com/page/2/",
  "https://quotes.toscrape.com/page/3/"
)

urls_paginas_quotes

# Función para scrapear una página de quotes
scrapear_quotes <- function(url) {
  # Leemos el HTML de la página
  pagina <- read_html(url)
  # Seleccionamos todos los bloques que contienen citas
  nodos <- pagina %>% html_elements(".quote")

  # Recorremos cada bloque de cita y unimos los resultados en una tabla
  map_dfr(nodos, function(nodo) {
    # Extraemos el texto de la cita
    cita <- nodo %>% html_element(".text") %>% html_text2()
    # Extraemos el nombre del autor
    autor <- nodo %>% html_element(".author") %>% html_text2()
    # Extraemos todas las etiquetas asociadas a la cita
    tags <- nodo %>% html_elements(".tag") %>% html_text2()

    # Construimos una fila con la información extraída
    tibble(
      # Guardamos la URL de origen
      url_origen = url,
      # Limpiamos comillas y espacios del texto de la cita
      cita = str_squish(str_replace_all(cita, "[\u201c\u201d]", "")),
      # Limpiamos espacios extra en el nombre del autor
      autor = str_squish(autor),
      # Unimos las etiquetas en un solo texto separado por comas
      tags = paste(tags, collapse = ", ")
    )
  })
}

# Aplicar la función a varias páginas
quotes_tres_paginas <- map_dfr(urls_paginas_quotes, scrapear_quotes)
mostrar_tabla(quotes_tres_paginas, n = 10, caption = "Resultados combinados de varias páginas")

# Validación básica
quotes_tres_paginas %>%
  summarise(
    filas = n(),
    autores_unicos = n_distinct(autor),
    citas_duplicadas = sum(duplicated(cita))
  ) %>%
  mostrar_tabla(n = 10, caption = "Validación básica del scraping de citas")


# -----------------------------------------------------------------------------
# 8. SCRAPING DE UN CATÁLOGO DE LIBROS
# -----------------------------------------------------------------------------

# Leer la página principal de Books to Scrape
url_books <- "https://books.toscrape.com/catalogue/page-1.html"
pagina_books <- read_html(url_books)
pagina_books

# Identificar los bloques de producto
nodos_libros <- pagina_books %>%
                html_elements("article.product_pod")
length(nodos_libros)

# Extraer datos de cada libro
libros_pagina_1 <- map_dfr(nodos_libros, function(nodo) {
  # Extraemos el título del libro desde el atributo title del enlace
  titulo <- nodo %>%
            html_element("h3 a") %>%
            html_attr("title")

  # Extraemos el precio como texto
  precio <- nodo %>%
            html_element(".price_color") %>%
            html_text2()

  # Extraemos el texto de disponibilidad del libro
  disponibilidad <- nodo %>%
                    html_element(".availability") %>%
                    html_text2()

  # Extraemos la clase CSS donde está codificado el rating
  rating_clase <- nodo %>%
                  html_element("p.star-rating") %>%
                  html_attr("class")

  # Extraemos el enlace relativo hacia el detalle del libro
  enlace_relativo <- nodo %>%
                    html_element("h3 a") %>%
                    html_attr("href")

  # Construimos una fila con las variables extraídas
  tibble(
    titulo = titulo,
    precio_texto = precio,
    disponibilidad = disponibilidad,
    rating_raw = rating_clase,
    enlace_relativo = enlace_relativo
  )
})

mostrar_tabla(libros_pagina_1, n = 10, caption = "Libros extraídos de una página")


# -----------------------------------------------------------------------------
# 9. LIMPIEZA DEL CATÁLOGO
# -----------------------------------------------------------------------------

# Función para limpiar la clase CSS del rating
limpiar_rating <- function(x) {
  # Quitamos el prefijo "star-rating" y dejamos solo la categoría
  x %>%
  str_remove("star-rating\\s+")
}

# Transformamos las variables extraídas a un formato más útil para análisis
libros_pagina_1_limpia <- libros_pagina_1 %>%
                          mutate(
                            # Convertimos el precio de texto a número
                            precio = precio_texto %>%
                              str_replace_all("[^0-9\\.]", "") %>%
                              as.numeric(),
                            # Limpiamos la clase del rating para dejar solo su valor
                            rating = limpiar_rating(rating_raw),
                            # Creamos una variable lógica que indica si el libro está disponible
                            disponible = str_detect(str_to_lower(disponibilidad), "in stock"),
                            # Construimos el enlace completo hacia el detalle del libro
                            enlace = paste0("https://books.toscrape.com/catalogue/", enlace_relativo)
                          ) %>%
                          select(titulo, precio, disponibilidad, disponible, rating, enlace)

mostrar_tabla(libros_pagina_1_limpia, n = 10, caption = "Libros con variables transformadas")


# -----------------------------------------------------------------------------
# 10. SCRAPEAR VARIAS PÁGINAS DEL CATÁLOGO
# -----------------------------------------------------------------------------

urls_books <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:3,
  ".html"
)

urls_books

# Función general para libros
scrapear_libros <- function(url) {

  # Leemos el HTML de la página indicada
  pagina <- read_html(url)

  # Seleccionamos todos los bloques que representan libros
  nodos <- pagina %>%
           html_elements("article.product_pod")

  # Recorremos cada bloque y unimos los resultados en una sola tabla
  map_dfr(nodos, function(nodo) {
    # Extraemos el título del libro desde el atributo title
    titulo <- nodo %>%
              html_element("h3 a") %>%
              html_attr("title")

    # Extraemos el precio tal como aparece en la página
    precio <- nodo %>%
              html_element(".price_color") %>%
              html_text2()

    # Extraemos el texto de disponibilidad
    disponibilidad <- nodo %>%
                      html_element(".availability") %>%
                      html_text2()

    # Extraemos la clase CSS que contiene la información del rating
    rating_raw <- nodo %>%
                  html_element("p.star-rating") %>%
                  html_attr("class")

    # Extraemos el enlace relativo hacia la página de detalle
    enlace_relativo <- nodo %>%
                       html_element("h3 a") %>%
                       html_attr("href")

    # Construimos una fila con los datos limpios y listos para analizar
    tibble(
      url_origen = url,
      titulo = titulo,
      precio = precio %>% str_replace_all("[^0-9\\.]", "") %>% as.numeric(),
      disponibilidad = str_squish(disponibilidad),
      rating = rating_raw %>% str_remove("star-rating\\s+"),
      enlace = paste0("https://books.toscrape.com/catalogue/", enlace_relativo)
    )
  })
}

# Ejecutamos scraping multi-página
libros_varias_paginas <- map_dfr(urls_books, scrapear_libros)
mostrar_tabla(libros_varias_paginas, n = 10, caption = "Libros combinados de varias páginas")

# Exploración rápida: conteo por rating
libros_varias_paginas %>%
  count(rating, sort = TRUE) %>%
  mostrar_tabla(n = 10, caption = "Conteo de libros por rating")

# Resumen de precios
libros_varias_paginas %>%
  summarise(
    n_libros = n(),
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_min = min(precio, na.rm = TRUE),
    precio_max = max(precio, na.rm = TRUE)
  ) %>%
  mostrar_tabla(n = 10, caption = "Resumen de precios del catálogo")


# -----------------------------------------------------------------------------
# 11. EXTRAER TABLAS HTML DIRECTAMENTE
# -----------------------------------------------------------------------------

# Definimos la URL de una página que contiene tablas HTML
url_tabla <- "https://www.w3schools.com/html/html_tables.asp"
# Leemos el HTML de la página
pagina_tabla <- read_html(url_tabla)

# Extraemos todas las tablas HTML y las convertimos en data frames
tablas <- pagina_tabla %>%
          html_table(fill = TRUE)
# Contamos cuántas tablas fueron encontradas en la página
length(tablas)

# Tomamos la primera tabla extraída y limpiamos sus nombres de columnas
tabla_ejemplo <- tablas[[1]] %>%
                 janitor::clean_names()

mostrar_tabla(tabla_ejemplo, n = 10, caption = "Ejemplo de tabla HTML extraída directamente")


# -----------------------------------------------------------------------------
# 12. BUENAS PRÁCTICAS EN SCRAPING CON R
# -----------------------------------------------------------------------------

# 12.2 Validar resultados
libros_varias_paginas %>%
  summarise(
    filas = n(),
    titulos_unicos = n_distinct(titulo),
    faltantes_precio = sum(is.na(precio)),
    duplicados_titulo = sum(duplicated(titulo))
  )

# 12.3 Hacer scraping pausado (eval = FALSE en el taller)
for (u in urls_books) {
  # Hacemos una pausa de 1 segundo entre peticiones
  Sys.sleep(1)
  # Mostramos en consola la URL que se está procesando
  print(u)
  # Aquí iría la lectura de la página
  # read_html(u)
}

# 12.4 Guardar resultados intermedios
write_csv(quotes_tres_paginas, "quotes_tres_paginas.csv")
write_csv(libros_varias_paginas, "libros_varias_paginas.csv")

# 12.5 Manejo básico de errores
scrapear_libros_seguro <- purrr::possibly(scrapear_libros, otherwise = tibble())

resultado_seguro <- map_dfr(urls_books, scrapear_libros_seguro)
resultado_seguro
