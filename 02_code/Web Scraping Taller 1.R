#-----------------------------  Datos Taller 1 ---------------------------------

#Librerias
library(pacman)
library(dplyr)
library(writexl)
p_load(
  tidyverse, # Manipulación de datos.
  rvest,     # Web scraping.
  httr,      # Solicitud de datos para páginas dinámicas.
  jsonlite   # Lectura de datos en formato .json. 
)

#Datos-URL
GEIH2018_URL <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
GEIH2018_HTML <- read_html(GEIH2018_URL)

# Extraer todos los links dentro de un contenedor específico
links <- GEIH2018_HTML %>%
  html_elements("a") %>%
  html_attr("href") %>%
  url_absolute(GEIH2018_URL)
head(links)

# Cuales son los links:

links_tabla <- tibble(
  texto = GEIH2018_HTML %>% html_elements("a") %>% html_text2(),
  href  = GEIH2018_HTML %>% html_elements("a") %>% html_attr("href")
)

print(links_tabla, n = 16)

#Escoger los links de interes
links_datos <- links_tabla %>%
  filter(str_detect(href, "^page\\d+\\.html$"))
#Links Completos:
links_completos <- links_datos$href %>%
  url_absolute(GEIH2018_URL)

links_completos

#Links Tablas directas 

Url_tablas <- str_c("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",1:10,".html")
Url_tablas

#Tabla directa - Intento 1
tabla_directa <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>%
  html_table()

#Extraccion -  Creaccion de Funcion auxiliar
extraer_tabla <- function(link) {
  Sys.sleep(0.3)
  
  tabla <- read_html(link) %>% html_table()
  
  tabla[[1]] %>%
    rename(id_fila = 1) %>%    # renombra la columna en posición 1, para evitar errores
    mutate(url_origen = link)
}
extraer_tabla_segura <- possibly(extraer_tabla, otherwise = NULL)

base_final <- map(Url_tablas, extraer_tabla_segura) %>%
  compact() %>%
  bind_rows()

nrow(base_final)  
View(base_final)

#Exportar base de datos a excel
write_xlsx(base_final, "GEIH2018.xlsx")



