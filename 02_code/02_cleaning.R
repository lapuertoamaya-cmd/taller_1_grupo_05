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
#   `base_analisis.rds`, con la muestra filtrada y las variables renombradas,
#   lista para las estadísticas descriptivas y el modelo de predicción.
#------------------------------------------------------------------------------#

install.packages("dplyr")
library(dplyr)
library(tidyverse)

# 1. Restringir la muestra
muestra <- base_final %>%
  filter(
    age >= 18,
    ocu == 1,
    y_total_m > 0)  

print(muestra)

text_vars <- c('edad' = 'age', 'sexo' = 'sex', 'estrato_energia' = 'estrato1', 'tipo_ocupacion' = 'relab',
               'maximo_nivel_educativo' = 'maxEducLevel', 'ocupado' = 'ocu',
               'nivel_educativo_alto' = 'p6210', 'Grado_escolar_aprobado' = 'p6210s1',
               'actividad_ultima_semana' = 'p6240', 'cotizante' = 'p6920', 'pet' = 'pet',
               'llave_hogar' = 'secuencia_p', 'horas_trabajadas' = 'totalHoursWorked',
               'ingreso_total' = 'y_total_m',  'chunk_origen' = 'url_origen')

base_analisis <- muestra %>%
  select(all_of(text_vars))

# Construcción de variables de interes
base_analisis <- base_analisis %>%
  mutate(
    # Años continuos -, para el término cuantitativo de Mincer
    ans_educ = case_when(
      nivel_educativo_alto %in% c(1, 2) ~ 0,
      nivel_educativo_alto %in% c(3, 4, 5) ~ Grado_escolar_aprobado,          # grado absoluto
      nivel_educativo_alto == 6 ~ 11 + (Grado_escolar_aprobado / 2),           # en semestres
      TRUE ~ NA_real_
    )
  )

# Construcción de Factor Educación
base_analisis <- base_analisis %>%
  mutate(
    nivel_educ = case_when(
      maximo_nivel_educativo == 1 ~ "ninguno",
      maximo_nivel_educativo == 2 ~ "preescolar",
      maximo_nivel_educativo == 3 ~ "primaria_incompleta",
      maximo_nivel_educativo == 4 ~ "primaria_completa",
      maximo_nivel_educativo == 5 ~ "secundaria_incompleta",
      maximo_nivel_educativo == 6 ~ "secundaria_completa",
      maximo_nivel_educativo == 7 ~ "terciaria",
      TRUE ~ NA_character_          # código 9 = N/A
    ),
    nivel_educ = relevel(factor(nivel_educ), ref = "ninguno")
  )

# Experiencia potencial:
base_analisis <- base_analisis %>%
  mutate(
    experiencia_pot = pmax(edad - ans_educ - 6, 0)
  )
View(base_analisis)

# Sacamos el número de página del link, para poder separar después los chunks
# 1-7 (entrenamiento) de los chunks 8-10 (validación) que pide la Sección 3.
base_analisis <- base_analisis %>%
  mutate(chunk_num = as.integer(str_extract(chunk_origen, "(?<=page_)\\d+")))

table(base_analisis$chunk_num)

# Guardar en disco, para que 03_limpieza.R (o el siguiente script) cargue esto
# directamente sin tener que repetir el scraping cada vez que se corra el pipeline.
saveRDS(base_analisis, "base_analisis.rds")


