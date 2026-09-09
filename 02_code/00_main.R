  #------------------------------------------------------------------------------#
  # 00_main.R
  # Taller 1 - Big Data y Machine Learning
  # 2026-09-06
  #
  # Objetivo:
  #    Definir el directorio de trabajo y el orden de ejecución principal
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
  
  # (0.4) Carpetas principales del proyecto
  # "01_data"
  # "02_code"
  # "03_outputs"
  
  # (0.5) Definir ruta del proyecto
  setwd(root)
  
  
  # ------------------------------------------------------------------------------
  # (1) Ejecucion principal
  # ------------------------------------------------------------------------------
  
  scripts <- c(
    "02_code/01_scrapping_geih2018_sample.R",
    "02_code/02_cleaning.R",
    "02_code/05_seccion_1.R",
    "02_code/06_seccion_2.R",
    "02_code/07_seccion_3.R"
  )
  
  for (script in scripts) {
    message("Ejecutando: ", script)
    source(script, encoding = "UTF-8")
}