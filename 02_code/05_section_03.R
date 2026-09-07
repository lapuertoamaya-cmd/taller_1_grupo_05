#------------------------------------------------------------------------------#
# 05_section_05.R
# Taller 1 - Big Data y Machine Learning
# 2026-09-06
#
# Objetivos:
#   ...
#
# Inputs:
#   01_data/02_clean/base_analisis.rds
# Outputs:
#   `...
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

if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load(
  tidyverse,  # Manipulación, descriptivas y gráficos
  caret,      # RMSE y validación
  gt          # Formato y exportación de tablas
)

# ------------------------------------------------------------------------------
# (2) definicion de muestras de entrenamiento y validacion
# ------------------------------------------------------------------------------

training <- base_analisis %>%
  filter(chunk %in% c(1:7))

test <- base_analisis %>%
  filter(chunk %in% c(8:10))

# ------------------------------------------------------------------------------
# (3) descriptivas
# ------------------------------------------------------------------------------

# (3.1) descriptivas generales



# (3.2) descriptivas train


# (99) que captura más varianza, estrato normal o como factor
modelo_estrato_num <- lm(ln_ingreso_total ~ estrato_energia)



# (3.3) descriptivas test







