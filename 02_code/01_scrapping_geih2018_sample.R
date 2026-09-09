# ==============================================================================
# 01_scrapinggeih2018_sample.R
# Taller 1 - Big Data y Machine Learning
# 2026-09-01
#
# Objetivos:
#   Extraer, consolidar y guardar los 10 fragmentos de datos publicados en:
#   https://ignaciomsarmiento.github.io/GEIH2018_sample/
#
# Inputs:
#   1. Pagina principal:
#      https://ignaciomsarmiento.github.io/GEIH2018_sample/
#
#   2. Archivos HTML con las tablas de datos:
#      https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html
#      ...
#      https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html
#
# Outputs:
#   1. Base consolidada en memoria:
#      base_final.xlsx
#
#   2. Archivo Excel exportado:
#      GEIH2018.xlsx
#
# Nota metodológica:
#   - La pagina principal enlaza paginas HTM dentro de geih_page_1.html, ..., geih_page_10.html.
# ==============================================================================

# ------------------------------------------------------------------------------
# (0) Declarar rutas del proyecto desde la ubicacion del script
# ------------------------------------------------------------------------------

# (0.0) paquete rstudioapi
if (!require("pacman")) { # si no se puede cargar, require==FALSE
  install.packages("pacman")
  library(pacman)
}

p_load(rstudioapi)
library(rstudioapi)


# (0.1) Obtener la ruta completa del script actual
ruta_script <- rstudioapi::getActiveDocumentContext()$path

# (0.2) Obtener la carpeta donde esta este script
dir_codigo <- dirname(ruta_script)

# (0.3) Definir el root del proyecto como la carpeta anterior a 02_code
root <- normalizePath(file.path(dir_codigo, ".."), winslash = "/", mustWork = TRUE)

# (0.4) Definir ruta del proyecto
setwd(root)

# (0.5) Carpetas principales del proyecto
# "01_data"
# "02_code"
# "03_outputs"


# ------------------------------------------------------------------------------
# (1) Preparar el ambiente de trabajo
# ------------------------------------------------------------------------------

# (1.1) Definir los paquetes requeridos
paquetes <- c("httr", "rvest", "dplyr", "writexl","stringr","purrr")

p_load(paquetes)

for (p in paquetes) {
  library(p, character.only=TRUE)
} 

# ------------------------------------------------------------------------------
# (2) Leer la pagina principal del sitio GEIH 2018
# ------------------------------------------------------------------------------

# (2.1) Definir la URL principal de la pagina
GEIH2018_URL <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

# (2.2) Leer el codigo HTML de la pagina principal
GEIH2018_HTML <- read_html(GEIH2018_URL)


# ------------------------------------------------------------------------------
# (3) Identificar los links disponibles en la pagina principal
# ------------------------------------------------------------------------------

# (3.1) Extraer todos los enlaces contenidos en etiquetas <a>
links <- GEIH2018_HTML %>%
  html_elements("a") %>%
  html_attr("href") %>%
  url_absolute(GEIH2018_URL)

# (3.2) Revisar los primeros links encontrados
head(links)

# (3.3) Crear una tabla con el texto visible y el href de cada link
links_tabla <- tibble(
  texto = GEIH2018_HTML %>% html_elements("a") %>% html_text2(),
  href  = GEIH2018_HTML %>% html_elements("a") %>% html_attr("href")
)

# (3.4) Imprimir los links encontrados
print(links_tabla, n = 16)


# ------------------------------------------------------------------------------
# (4) Construir los links de las tablas de datos
# ------------------------------------------------------------------------------

# (4.1) Filtrar links tipo page1.html, page2.html, etc.
links_datos <- links_tabla %>%
  filter(str_detect(href, "^page\\d+\\.html$"))

# (4.2) Convertir esos links relativos en links completos
links_completos <- links_datos$href %>%
  url_absolute(GEIH2018_URL)

# (4.3) Construir directamente los links de las 10 tablas HTML
Url_tablas <- str_c(
  "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
  1:10,
  ".html"
)

# (4.4) Revisar las URLs finales que se van a leer
Url_tablas


# ------------------------------------------------------------------------------
# (5) Probar la extraccion con la primera tabla
# ------------------------------------------------------------------------------

# (5.1) Leer el primer archivo HTML de datos
tabla_directa <- read_html(
  "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
) %>%
  html_table()

# (5.2) Revisar la primera tabla extraida
tabla_directa[[1]]


# ------------------------------------------------------------------------------
# (6) Crear una funcion auxiliar para extraer cada tabla
# ------------------------------------------------------------------------------

extraer_tabla <- function(link, chunk_id) {
  
  # (6.1) Hacer una pausa responsable antes de cada solicitud
  Sys.sleep(runif(1, 2, 4))
  
  # (6.2) Leer el HTML del link recibido
  tabla <- read_html(link) %>%
    html_table()
  
  # (6.3) Extraer la primera tabla del HTML
  tabla[[1]] %>%
    rename(id_fila = 1) %>%
    mutate(chunk=chunk_id,
           url_origen = link) # para tener un id del chunk
}

# (6.4) Crear una version segura de la funcion
extraer_tabla_segura <- possibly(extraer_tabla, otherwise = NULL)


# ------------------------------------------------------------------------------
# (7) Extraer y unir los 10 fragmentos
# ------------------------------------------------------------------------------

# (7.1) Aplicar la funcion a las 10 URLs
base_final <- map2(Url_tablas, 1:10, extraer_tabla_segura) %>%
  compact() %>%
  bind_rows()

# (7.2) Revisar el numero total de filas
nrow(base_final)

# (7.3) Validar que la base tenga las 32.177 observaciones esperadas
stopifnot(nrow(base_final) == 32177)

# (7.4) Abrir la base en el visor de RStudio
View(base_final)


# ------------------------------------------------------------------------------
# (8) Exportar la base consolidada
# ------------------------------------------------------------------------------

# (8.1) Guardar la base final en formato Excel
write_xlsx(base_final, "01_data/01_raw/geih2018_sample.xlsx")