# =============================================================================
# Taller práctico de Web Scraping con Python — Parte 1
# Curso de Minería de Datos
# Universidad Nacional de Colombia — Departamento de Estadística, Sede Bogotá
# Autor: Andrés Felipe Flórez Rivera
# =============================================================================


# -----------------------------------------------------------------------------
# 4. LEER UNA PÁGINA WEB
# Paquetes necesarios
# -----------------------------------------------------------------------------

# pip install requests beautifulsoup4 pandas lxml

import re
import time
import requests
import pandas as pd
from bs4 import BeautifulSoup


# -----------------------------------------------------------------------------
# Usaremos un sitio hecho para practicar scraping: Quotes to Scrape
# -----------------------------------------------------------------------------

url_quotes = "https://quotes.toscrape.com/"

# Hacemos la petición y parseamos el HTML
respuesta    = requests.get(url_quotes)
pagina_quotes = BeautifulSoup(respuesta.text, "lxml")
print(pagina_quotes.prettify()[:500])  # Preview del HTML


# -----------------------------------------------------------------------------
# Extraer el título de la página
# -----------------------------------------------------------------------------

pagina_quotes.find("title").get_text()


# -----------------------------------------------------------------------------
# Extraer todos los textos de las citas
# -----------------------------------------------------------------------------

citas = [el.get_text(strip=True) for el in pagina_quotes.select(".quote .text")]
citas


# -----------------------------------------------------------------------------
# Extraer los autores
# -----------------------------------------------------------------------------

autores = [el.get_text(strip=True) for el in pagina_quotes.select(".quote .author")]
autores


# -----------------------------------------------------------------------------
# Construir una tabla para almacenar las citas
# -----------------------------------------------------------------------------

# Creamos el DataFrame
tabla_citas = pd.DataFrame({"cita": citas, "autor": autores})

# Mostramos las primeras filas
tabla_citas.head(10)


# -----------------------------------------------------------------------------
# 5. CITAS, AUTORES Y ETIQUETAS
# -----------------------------------------------------------------------------

nodos_citas = pagina_quotes.select(".quote")
len(nodos_citas)


# -----------------------------------------------------------------------------
# Extraer información por bloque
# -----------------------------------------------------------------------------

registros = []
for nodo in nodos_citas:
    cita  = nodo.select_one(".text").get_text(strip=True)
    autor = nodo.select_one(".author").get_text(strip=True)
    tags  = [t.get_text(strip=True) for t in nodo.select(".tags .tag")]
    registros.append({"cita": cita, "autor": autor, "tags": ", ".join(tags)})

tabla_citas_completa = pd.DataFrame(registros)
tabla_citas_completa.head(10)


# -----------------------------------------------------------------------------
# 6. LIMPIEZA BÁSICA DEL TEXTO EXTRAÍDO
# -----------------------------------------------------------------------------

tabla_citas_limpia = tabla_citas_completa.copy()

# Quitamos las comillas tipográficas y compactamos espacios
tabla_citas_limpia["cita"] = (
    tabla_citas_limpia["cita"]
    .str.replace(r'[\u201c\u201d]', "", regex=True)
    .str.split().str.join(" ")       # equivalente a str_squish
)
tabla_citas_limpia["autor"] = tabla_citas_limpia["autor"].str.split().str.join(" ")

# Calculamos cuántas etiquetas tiene cada cita
tabla_citas_limpia["n_tags"] = tabla_citas_limpia["tags"].apply(
    lambda x: 0 if x == "" else len(x.split(", "))
)

tabla_citas_limpia.head(10)


# -----------------------------------------------------------------------------
# Resumen rápido
# -----------------------------------------------------------------------------

(tabla_citas_limpia
    .groupby("autor")
    .size()
    .reset_index(name="n")
    .sort_values("n", ascending=False)
    .head(10))


# -----------------------------------------------------------------------------
# 7. NAVEGAR VARIAS PÁGINAS
# Crear vector de URLs
# -----------------------------------------------------------------------------

urls_paginas_quotes = [
    "https://quotes.toscrape.com/page/1/",
    "https://quotes.toscrape.com/page/2/",
    "https://quotes.toscrape.com/page/3/"
]
urls_paginas_quotes


# -----------------------------------------------------------------------------
# Función para scrapear una página de quotes
# -----------------------------------------------------------------------------

def scrapear_quotes(url):
    pagina = BeautifulSoup(requests.get(url).text, "lxml")
    nodos  = pagina.select(".quote")
    registros = []
    for nodo in nodos:
        cita  = nodo.select_one(".text").get_text(strip=True)
        autor = nodo.select_one(".author").get_text(strip=True)
        tags  = [t.get_text(strip=True) for t in nodo.select(".tag")]
        registros.append({
            "url_origen": url,
            # Limpiamos comillas y espacios
            "cita": " ".join(re.sub(r'[\u201c\u201d]', "", cita).split()),
            "autor": " ".join(autor.split()),
            "tags": ", ".join(tags)
        })
    return pd.DataFrame(registros)


# -----------------------------------------------------------------------------
# Aplicar la función a varias páginas
# -----------------------------------------------------------------------------

quotes_tres_paginas = pd.concat(
    [scrapear_quotes(u) for u in urls_paginas_quotes],
    ignore_index=True
)
quotes_tres_paginas.head(10)


# -----------------------------------------------------------------------------
# Validación básica
# -----------------------------------------------------------------------------

pd.DataFrame([{
    "filas":            len(quotes_tres_paginas),
    "autores_unicos":   quotes_tres_paginas["autor"].nunique(),
    "citas_duplicadas": int(quotes_tres_paginas["cita"].duplicated().sum())
}])


# -----------------------------------------------------------------------------
# 8. SCRAPING DE UN CATÁLOGO DE LIBROS
# Leer la página principal
# -----------------------------------------------------------------------------

url_books_p1 = "https://books.toscrape.com/catalogue/page-1.html"
pagina_books  = BeautifulSoup(requests.get(url_books_p1).text, "lxml")
pagina_books.title.get_text()


# -----------------------------------------------------------------------------
# Identificar los bloques de producto
# -----------------------------------------------------------------------------

nodos_libros = pagina_books.select("article.product_pod")
len(nodos_libros)


# -----------------------------------------------------------------------------
# Extraer datos de cada libro
# -----------------------------------------------------------------------------

registros = []
for nodo in nodos_libros:
    titulo          = nodo.select_one("h3 a")["title"]
    precio_texto    = nodo.select_one(".price_color").get_text(strip=True)
    disponibilidad  = nodo.select_one(".availability").get_text(strip=True)
    rating_raw      = " ".join(nodo.select_one("p.star-rating")["class"])
    enlace_relativo = nodo.select_one("h3 a")["href"]
    registros.append({
        "titulo":          titulo,
        "precio_texto":    precio_texto,
        "disponibilidad":  disponibilidad,
        "rating_raw":      rating_raw,
        "enlace_relativo": enlace_relativo
    })

libros_pagina_1 = pd.DataFrame(registros)
libros_pagina_1.head(10)


# -----------------------------------------------------------------------------
# 9. LIMPIEZA DEL CATÁLOGO
# -----------------------------------------------------------------------------

def limpiar_rating(x):
    # Quitamos el prefijo "star-rating" y dejamos solo la categoría
    return re.sub(r"star-rating\s*", "", x).strip()

libros_pagina_1_limpia = libros_pagina_1.copy()

# Convertimos el precio de texto a número
libros_pagina_1_limpia["precio"] = (
    libros_pagina_1_limpia["precio_texto"]
    .str.replace(r"[^\d.]", "", regex=True)
    .astype(float)
)
# Limpiamos el rating
libros_pagina_1_limpia["rating"]     = libros_pagina_1_limpia["rating_raw"].apply(limpiar_rating)
# Variable lógica de disponibilidad
libros_pagina_1_limpia["disponible"] = (
    libros_pagina_1_limpia["disponibilidad"].str.lower().str.contains("in stock")
)
# Enlace completo
libros_pagina_1_limpia["enlace"] = (
    "https://books.toscrape.com/catalogue/" + libros_pagina_1_limpia["enlace_relativo"]
)
libros_pagina_1_limpia = libros_pagina_1_limpia[
    ["titulo", "precio", "disponibilidad", "disponible", "rating", "enlace"]
]
libros_pagina_1_limpia.head(10)


# -----------------------------------------------------------------------------
# 10. SCRAPEAR VARIAS PÁGINAS DEL CATÁLOGO
# URLs de las páginas
# -----------------------------------------------------------------------------

urls_books = [
    f"https://books.toscrape.com/catalogue/page-{i}.html"
    for i in range(1, 4)
]
urls_books


# -----------------------------------------------------------------------------
# Función general para libros
# -----------------------------------------------------------------------------

def scrapear_libros(url):
    pagina = BeautifulSoup(requests.get(url).text, "lxml")
    nodos  = pagina.select("article.product_pod")
    registros = []
    for nodo in nodos:
        titulo          = nodo.select_one("h3 a")["title"]
        precio_texto    = nodo.select_one(".price_color").get_text(strip=True)
        disponibilidad  = nodo.select_one(".availability").get_text(strip=True)
        rating_raw      = " ".join(nodo.select_one("p.star-rating")["class"])
        enlace_relativo = nodo.select_one("h3 a")["href"]
        registros.append({
            "url_origen":    url,
            "titulo":        titulo,
            "precio":        float(re.sub(r"[^\d.]", "", precio_texto)),
            "disponibilidad": " ".join(disponibilidad.split()),
            "rating":        limpiar_rating(rating_raw),
            "enlace":        "https://books.toscrape.com/catalogue/" + enlace_relativo
        })
    return pd.DataFrame(registros)


# -----------------------------------------------------------------------------
# Ejecutamos scraping multi-página
# -----------------------------------------------------------------------------

libros_varias_paginas = pd.concat(
    [scrapear_libros(u) for u in urls_books],
    ignore_index=True
)
libros_varias_paginas.head(10)


# -----------------------------------------------------------------------------
# Conteo por rating
# -----------------------------------------------------------------------------

(libros_varias_paginas
    .groupby("rating")
    .size()
    .reset_index(name="n")
    .sort_values("n", ascending=False)
    .head(10))


# -----------------------------------------------------------------------------
# Resumen de precios
# -----------------------------------------------------------------------------

pd.DataFrame([{
    "n_libros":        len(libros_varias_paginas),
    "precio_promedio": round(libros_varias_paginas["precio"].mean(), 2),
    "precio_min":      libros_varias_paginas["precio"].min(),
    "precio_max":      libros_varias_paginas["precio"].max()
}])


# -----------------------------------------------------------------------------
# 11. EXTRAER TABLAS HTML DIRECTAMENTE
# -----------------------------------------------------------------------------

url_tabla = "https://www.w3schools.com/html/html_tables.asp"

# pd.read_html() extrae directamente todas las tablas de la página
tablas = pd.read_html(url_tabla)
len(tablas)

# Ver primera tabla
tabla_ejemplo = tablas[0].copy()
# Equivalente a janitor::clean_names()
tabla_ejemplo.columns = [
    c.lower().replace(" ", "_") for c in tabla_ejemplo.columns
]
tabla_ejemplo.head(10)


# -----------------------------------------------------------------------------
# 12. BUENAS PRÁCTICAS EN SCRAPING CON PYTHON
# -----------------------------------------------------------------------------

# 12.2 Validar resultados
pd.DataFrame([{
    "filas":             len(libros_varias_paginas),
    "titulos_unicos":    libros_varias_paginas["titulo"].nunique(),
    "faltantes_precio":  int(libros_varias_paginas["precio"].isna().sum()),
    "duplicados_titulo": int(libros_varias_paginas["titulo"].duplicated().sum())
}])

# 12.3 Hacer scraping pausado
for u in urls_books:
    time.sleep(1)   # Pausa de 1 segundo entre peticiones
    print(u)
    # Aquí iría la petición real
    # requests.get(u)

# 12.4 Guardar resultados intermedios
quotes_tres_paginas.to_csv("quotes_tres_paginas.csv", index=False)
libros_varias_paginas.to_csv("libros_varias_paginas.csv", index=False)

# 12.5 Manejo básico de errores
def scrapear_libros_seguro(url):
    try:
        return scrapear_libros(url)
    except Exception as e:
        print(f"Error en {url}: {e}")
        return pd.DataFrame()   # equivalente a otherwise = tibble()

resultado_seguro = pd.concat(
    [scrapear_libros_seguro(u) for u in urls_books],
    ignore_index=True
)
resultado_seguro
