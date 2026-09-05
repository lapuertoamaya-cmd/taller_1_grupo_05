
# Objetivos:
#   Filtrar las personas con ingresos totales positivos, mayores de 18 años y que
#   pertenecen a la categoría de ocupados
# Inputs:
#   `base_final`, creada en `0.R`.
# Outputs:
#   `base_analisis.rds`, con la muestra filtrada y las variables renombradas,
#   lista para las estadísticas descriptivas y el modelo de predicción.

install.packages("dplyr")
library(dplyr)

# 1. Restringir la muestra
muestra <- base_final %>%
  filter(
    age >= 18,
    ocu == 1,
    y_total_m > 0)  

print(muestra)

text_vars <- c('edad' = 'age', 'estrato_energia' = 'estrato1', 'tipo_ocupacion' = 'relab',
               'maximo_nivel_educativo' = 'maxEducLevel', 'ocupado' = 'ocu',
               'nivel_educativo_alto' = 'p6210', 'Grado_escolar_aprobado' = 'p6210s1',
               'actividad_ultima_semana' = 'p6240', 'cotizante' = 'p6920', 'pet' = 'pet',
               'llave_hogar' = 'secuencia_p', 'horas_trabajadas' = 'totalHoursWorked')

base_analisis <- muestra %>%
  select(all_of(text_vars))

# Guardar en disco, para que 03_limpieza.R (o el siguiente script) cargue esto
# directamente sin tener que repetir el scraping cada vez que se corra el pipeline.
saveRDS(base_analisis, "base_analisis.rds")

