#------------------------------------------------------------------------------#
# 02_cleaning
# Taller 1 - Big Data y Machine Learning
# 2026-09-05
#
# Objetivos:
#   Filtrar las personas con ingresos totales positivos, mayores de 18 años y que
#   pertenecen a la categoría de ocupados
# Inputs:
#   `base_final`, creada en `0.R`.
# Outputs:
#   `base_analisis.rds` y .csv, con la muestra filtrada y las variables renombradas,
#   lista para las estadísticas descriptivas y el modelo de predicción.
#------------------------------------------------------------------------------#


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
# (1) importar paquetes necesarios
# ------------------------------------------------------------------------------

if (!require("tidyverse")) { # si no se puede cargar, require==FALSE
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("readxl")) { # si no se puede cargar, require==FALSE
  install.packages("readxl")
  library(readxl)
}

# ------------------------------------------------------------------------------
# (2) Restriccion de la muestra a mayores de edad, ocupados con ingresos positivos
# ------------------------------------------------------------------------------

base_final <- read_excel("01_data/01_raw/geih2018_sample.xlsx")

muestra <- base_final %>%
  filter(
    age >= 18,
    ocu == 1,
    y_total_m > 0)  

print(muestra)

# ------------------------------------------------------------------------------
# (3) Limpieza de variables
# ------------------------------------------------------------------------------

# (3.1) vector de vars relevantes
text_vars <- c('edad' = 'age', 'sexo' = 'sex', 'estrato_energia' = 'estrato1', 
               'tipo_ocupacion' = 'relab',
               'maximo_nivel_educativo' = 'maxEducLevel', 
               'ocupado' = 'ocu',
               'nivel_educativo_alto' = 'p6210', 'grado_escolar_aprobado' = 'p6210s1',
               'actividad_ultima_semana' = 'p6240', 
               'cotizante' = 'p6920', 'pet' = 'pet',
               'llave_hogar' = 'secuencia_p', 'llave_persona'='orden', 'llave_vivienda'='directorio', 
               'horas_trabajadas' = 'totalHoursWorked',
               'ingreso_total' = 'y_total_m', 
               'tamanio_firma'='sizeFirm', 'informal'='informal',
               'chunk'='chunk', 'chunk_origen' = 'url_origen')

base_analisis <- muestra %>%
  select(all_of(text_vars))

# (3.2) construcción de variables de interes

base_analisis <- base_analisis %>%
  mutate(
    # (3.2.1) años de educacion
    # Años continuos -, para el término cuantitativo de Mincer
    ans_educ = case_when(
      nivel_educativo_alto %in% c(1, 2) ~ 0,
      nivel_educativo_alto ==3 ~ grado_escolar_aprobado,
      nivel_educativo_alto == 4 & grado_escolar_aprobado == 0 ~ 5,
      nivel_educativo_alto == 4 & grado_escolar_aprobado > 0 ~ grado_escolar_aprobado,
      nivel_educativo_alto == 5 ~ grado_escolar_aprobado,
      nivel_educativo_alto == 6 ~ 11 + (grado_escolar_aprobado / 2),
      nivel_educativo_alto == 9 & grado_escolar_aprobado == 99 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # (3.2.2) educacion como factor
    nivel_educ = case_when(
      nivel_educativo_alto == 1 ~ "ninguno",
      nivel_educativo_alto == 2 ~ "preescolar",
      nivel_educativo_alto == 3 ~ "primaria",
      nivel_educativo_alto == 4 ~ "secundaria",
      nivel_educativo_alto == 5 ~ "media",
      nivel_educativo_alto == 6 ~ "terciaria",
      TRUE ~ NA_character_          # código 9 = N/A
    ),
    nivel_educ = relevel(factor(nivel_educ), ref = "ninguno"),
    
    # (3.2.3) experiencia potencial
    exp_potencial = pmax(edad - ans_educ - 6, 0),
    
    # (3.2.4) estrato como factor
    estrato_factor = factor(estrato_energia),
    
    # (3.2.5) log del ingreso
    ln_ingreso_total = log(ingreso_total),
    
    # (3.2.6) educacion con particion
    edu_may_10 = ifelse(ans_educ>10,1,0),
    
    # (3.2.7) ocupacion como factor
    tipo_ocupacion = factor(tipo_ocupacion)
  )

View(base_analisis)

# ------------------------------------------------------------------------------
# (4) guardamos la base
# ------------------------------------------------------------------------------

# archivo en formato R
saveRDS(base_analisis, "01_data/02_clean/base_analisis.rds")
write_csv(base_analisis, "01_data/02_clean/base_analisis.csv")
