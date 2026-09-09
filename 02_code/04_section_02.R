#===============================================================================
#-------------------------------- Sección 2 ------------------------------------
# Paquetes
install.packages("dplyr")
install.packages("tidyverse")
install.packages("boot")
install.packages("stargazer")


#Libreriras
library(dplyr)
library(tidyverse)
library(boot)
library(stargazer)
library(dplyr)
library(tidyr)
library(purrr)

#Estadisticas Descriptivas------------------------------------------------------
base_analisis <- base_analisis %>% mutate(log_ingreso = log(ingreso_total))
continuous_vars <- c("log_ingreso", "ans_educ", "edad", "experiencia_pot")
discrete_vars   <- c("sexo", "nivel_educ", "estrato_energia")

# --- Función para estadísticas continuas ---
stats_continuas <- function(df, vars) {
  map_dfr(vars, function(v) {
    x <- df[[v]]
    tibble(
      variable = v,
      media    = mean(x, na.rm = TRUE),
      de       = sd(x, na.rm = TRUE),
      min      = min(x, na.rm = TRUE),
      p10      = quantile(x, 0.10, na.rm = TRUE),
      mediana  = median(x, na.rm = TRUE),
      p90      = quantile(x, 0.90, na.rm = TRUE),
      max      = max(x, na.rm = TRUE)
    )
  })
}

# --- Función para estadísticas discretas/dummies ---
stats_discretas <- function(df, vars, var_salario = "log_ingreso") {
  n_total <- nrow(df)
  map_dfr(vars, function(v) {
    df %>%
      group_by(categoria = as.character(.data[[v]])) %>%
      summarise(
        N                     = n(),
        promedio_log_salario  = mean(.data[[var_salario]], na.rm = TRUE),
        de_log_salario        = sd(.data[[var_salario]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        variable   = v,
        porcentaje = 100 * N / n_total
      ) %>%
      select(variable, categoria, N, porcentaje, promedio_log_salario, de_log_salario)
  })
}

# --- Subconjuntos (ajusta los valores según cómo esté codificado 'sexo') ---
base_hombres <- base_analisis %>% filter(sexo == 0)
base_mujeres <- base_analisis %>% filter(sexo == 1)

# --- Tabla de continuas ---
tabla_continuas <- bind_rows(
  stats_continuas(base_analisis, continuous_vars) %>% mutate(grupo = "Total"),
  stats_continuas(base_hombres,  continuous_vars) %>% mutate(grupo = "Hombres"),
  stats_continuas(base_mujeres,  continuous_vars) %>% mutate(grupo = "Mujeres")
) %>%
  pivot_longer(cols = media:max, names_to = "estadistico", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  arrange(variable, factor(estadistico, levels = c("media","de","min","p10","mediana","p90","max")))

# --- Tabla de discretas ---
tabla_discretas <- bind_rows(
  stats_discretas(base_analisis, discrete_vars) %>% mutate(grupo = "Total"),
  stats_discretas(base_hombres,  discrete_vars) %>% mutate(grupo = "Hombres"),
  stats_discretas(base_mujeres,  discrete_vars) %>% mutate(grupo = "Mujeres")
) %>%
  pivot_longer(cols = c(N, porcentaje, promedio_log_salario, de_log_salario),
               names_to = "estadistico", values_to = "valor") %>%
  pivot_wider(names_from = grupo, values_from = valor) %>%
  arrange(variable, categoria,
          factor(estadistico, levels = c("N","porcentaje","promedio_log_salario","de_log_salario")))

#Tabla variables continuas
stargazer(as.data.frame(tabla_continuas), type = "text", summary = FALSE, rownames = FALSE)

#Tabla variables discretas
stargazer(as.data.frame(tabla_discretas), type = "text", summary = FALSE, rownames = FALSE)

#Modelo uncicamente con brecha de genero
#Modelo brecha de genero--------------------------------------------------------
Modelo_brecha <- lm(
  log(ingreso_total) ~ sexo,
  data = base_analisis
)

#Resultados tabulados-----------------------------------------------------------
stargazer(
  Modelo_brecha,
  type = "text",
  
  title = "Resultados de la regresión unicamente con Sexo",
  
  dep.var.labels = "Logaritmo del ingreso mensual",
  
  covariate.labels = c(
    "Sexo"
  ),
  
  ci = TRUE,
  ci.level = 0.95,
  
  keep.stat = c(
    "n",
    "rsq",
    "adj.rsq"
  ),
  
  digits = 3,
  
  star.cutoffs = c(0.1, 0.05, 0.01),
  
  notes = "Intervalos de confianza al 95%. * p<0.10; ** p<0.05; *** p<0.01."
)

#Boostrap-----------------------------------------------------------------------
#Funciónn de calculo del modelo 
calcular_model_incond_boot <- function(data, indices) {
  
  # 1. Remuestreo con reemplazo
  muestra_boot <- data[indices, ]
  
  # 2. Estimar el mismo modelo en la muestra bootstrap
  modelo_boot <- lm(
    log(ingreso_total) ~ sexo,
    data = muestra_boot
  )
  
  # 3. Extraer el coeficiente de sexo
  coef_sexo <- coef(modelo_boot)["sexo"]
  
  # 4. Devolverlo
  return(coef_sexo)
}

set.seed(123) #semilla

boot_sexo <- boot(
  data = base_analisis,
  statistic = calcular_model_incond_boot,
  R = 1000
)

# Coeficiente original
boot_sexo$t0

# Distribución de los 1000 coeficientes bootstrap
boot_sexo$t

# Sesgo bootstrap
bias_boot_sexo <- mean(boot_gap$t) - boot_gap$t0

# Error estándar bootstrap
se_boot_sexo <- sd(boot_gap$t)

#Intervalos de confianza bootstrap
IC_boot_sexo <- boot.ci(
  boot_sexo,
  type = "perc"
)
IC_boot_sexo

#Transformacion de los intervalos boot
limite_inferior <- IC_boot_sexo$percent[4]
limite_superior <- IC_boot_sexo$percent[5]
 
Transformación_IC_inferior <- ((exp(limite_inferior) - 1) * 100)
Transformación_IC_superior <- ((exp(limite_superior) - 1) * 100)

Transformación_IC_inferior
Transformación_IC_superior

#Interpretación ----------------------------------------------------------------
# Resultado porcentual 
Transformación_Beta <- ((exp(coef(Modelo_brecha)["sexo"]) - 1) * 100)
Transformación_Beta

#Interpretación del Coeficiente
#En promedio, el salario de las mujeres es 21,71% mas bajo en comparación a los 
#hombres. Tenemos una confianza del 95% de que el efecto poblacional de la 
#discriminación salarial estara entre (-0,267, -0,209).

#Interpretación IC bootstrap
#El intervalo de confianza bootstrap ajustado al 95% indica que el efecto 
#poblacional de la variable sexo sobre el ingreso mensual se encuentra entre
#−23,34% y −18,93%.

#En otras palabras, con un nivel de confianza del 95%, se estima que la brecha 
#salarial asociada al sexo se encuentra entre 18,93% y 23,34% en menor ingreso
#para las mujeres respecto de los hombres.

#Modelo de genero con controles-------------------------------------------------
modelo_gap_controles <- lm(
  log(ingreso_total) ~ sexo + nivel_educ + nivel_educ:ans_educ + 
    experiencia_pot + I(experiencia_pot^2) + factor(estrato_energia),
  data = base_analisis
)

#Resultados modelo controles----------------------------------------------------
stargazer(
  modelo_gap_controles,
  type = "text",
  
  title = "Resultados de la regresión sexo y controles",
  
  dep.var.labels = "Logaritmo del ingreso mensual",
  
  covariate.labels = c(
    "Sexo"
  ),
  
  ci = TRUE,
  ci.level = 0.95,
  
  keep.stat = c(
    "n",
    "rsq",
    "adj.rsq"
  ),
  
  digits = 3,
  
  star.cutoffs = c(0.1, 0.05, 0.01),
  
  notes = "Intervalos de confianza al 95%. * p<0.10; ** p<0.05; *** p<0.01."
)


#Boostrap-----------------------------------------------------------------------
#Funciónn de calculo del modelo 
calcular_gap_boot <- function(data, indices) {
  
  # 1. Remuestreo con reemplazo
  muestra_boot <- data[indices, ]
  
  # 2. Estimar el mismo modelo en la muestra bootstrap
  modelo_boot <- lm(
    log(ingreso_total) ~ sexo + nivel_educ + nivel_educ:ans_educ + 
      experiencia_pot + I(experiencia_pot^2) + 
      factor(estrato_energia),
    data = muestra_boot
  )
  
  # 3. Extraer el coeficiente de sexo
  coef_sexo <- coef(modelo_boot)["sexo"]
  
  # 4. Devolverlo
  return(coef_sexo)
}

set.seed(123) #semilla

boot_gap <- boot(
  data = base_analisis,
  statistic = calcular_gap_boot,
  R = 1000
)

# Coeficiente original
boot_gap$t0

# Distribución de los 1000 coeficientes bootstrap
boot_gap$t

# Sesgo bootstrap
bias_boot_gap <- mean(boot_gap$t) - boot_gap$t0

# Error estándar bootstrap
se_boot_gap <- sd(boot_gap$t)

#Intervalos de confianza bootstrap
IC_boot_gap <- boot.ci(
  boot_gap,
  type = "perc"
)
IC_boot_gap

#Interpretación ----------------------------------------------------------------
# Resultado porcentual 
Transformación_Beta_gap <- ((exp(coef(modelo_gap_controles)["sexo"]) - 1) * 100)
Transformación_Beta_gap

#Transformacion de los intervalos boot
limite_inferior <- IC_boot_gap$percent[4]
limite_superior <- IC_boot_gap$percent[5]

Transformación_IC_inferior_gap <- ((exp(limite_inferior) - 1) * 100)
Transformación_IC_superior_gap <- ((exp(limite_superior) - 1) * 100)

Transformación_IC_inferior_gap
Transformación_IC_superior_gap

#Interpretación del coeficiente 
#En promedio, el salario de las mujeres, es 26,48% mas bajo en comparación a los 
#hombres cuando se controla por nivel educativo, estrato de energia y experiencia
#potencial. Tenemos una confianza del 95% de que el efecto poblacional de la 
#discriminación salarial estara entre (-0,330, -0,285).

#Interpretación IC bootstrap
#El intervalo de confianza bootstrap ajustado al 95% indica que el efecto 
#poblacional de la variable sexo sobre el ingreso mensual se encuentra entre
#−28,22% y −24,76%.

#En otras palabras, con un nivel de confianza del 95%, se estima que la brecha 
#salarial asociada al sexo se encuentra entre 24,76% y 28,22% en menor ingreso
#para las mujeres respecto de los hombres.


# 2. Frisch-Waugh-Lovell -------------------------------------------------------

# 1) El teorema de Frisch-Waugh-Lovell (FWL) establece que, en una regresión
# lineal múltiple, el coeficiente asociado a la variable de interés X1 puede
# obtenerse mediante una regresión simple entre los residuos de Y y los
# residuos de X1, una vez eliminada de ambas variables la parte explicada
# linealmente por las variables de control X2. Esto es posible gracias a las
# propiedades de las matrices de proyección y aniquiladora.

# De este teorema se obtiene que el coeficiente de interés será igual a:
# B1^{hat} = (X1'M2X1)^(-1)X1'M2Y, donde M2 = I - P2 es la matriz aniquiladora
# asociada a X2. Esta expresión puede reescribirse como:
# B1^{hat} = (residuo_X1'residuo_X1)^(-1)(residuo_X1'residuo_Y).
# Por tanto, el coeficiente de FWL puede obtenerse al realizar una regresión
# de los residuos de Y sobre los residuos de X1, donde ambos residuos se
# obtienen previamente al eliminar de Y y X1, respectivamente, la parte
# explicada linealmente por X2.

#Paso 1) Obterner los residuos de Y
modelo_y <- lm(
  log(ingreso_total) ~ nivel_educ + nivel_educ:ans_educ  + 
    experiencia_pot + I(experiencia_pot^2) + factor(estrato_energia), 
  data = base_analisis
) 

res_y <- residuals(modelo_y) #Esto es igual a M2*Y


#Paso 2) Obtener los residuos de X1
modelo_sex <- lm(
  sexo ~ nivel_educ + nivel_educ:ans_educ  + 
    experiencia_pot + I(experiencia_pot^2) + factor(estrato_energia),
  data = base_analisis
)

res_sex <- residuals(modelo_sex) #Esto es igual a M2*X1

#Paso 3) Regresión entre los residuos de Y y residuos de X1
fwl_sex <- lm(res_y ~ res_sex)

#Resultados tabulados-----------------------------------------------------------
stargazer(
  fwl_sex,
  type = "text",
  
  title = "Resultados de la regresión FWL",
  
  dep.var.labels = "Residuos  Y",
  
  covariate.labels = c(
    "Residuos X1"
  ),
  
  ci = TRUE,
  ci.level = 0.95,
  
  keep.stat = c(
    "n",
    "rsq",
    "adj.rsq"
  ),
  
  digits = 3,
  
  star.cutoffs = c(0.1, 0.05, 0.01),
  
  notes = "Intervalos de confianza al 95%. * p<0.10; ** p<0.05; *** p<0.01."
)

#Confirmación de resultados
coef(modelo_gap_controles)["sexo"]
coef(fwl_sex)["res_sex"]
beta_fwl <- coef(fwl_sex)["res_sex"]

#Error estandar del modelo
summary(fwl_sex)$coefficients["res_sex", "Std. Error"]

#Error estandar analitico FWL --------------------------------------------------
#El SE que da R por defecto NO es correcto: usa grados de libertad de una regresión
#de 2 parámetros (n-2), en vez de los grados de libertad reales del modelo completo 
#(n-k, con todos los controles).Hay que corregirlo manualmente.

df <- n_model - k #Grados de libertad

sigma2 <- sum(residuals(modelo_gap_controles)^2) / df 
se_analitico <- sqrt(sigma2 / sum(res_sex^2))
se_analitico


#Bootstrap----------------------------------------------------------------------
FWL_boot <- function(data, indices) {
  

  # 1. Muestra bootstrap
  muestra_boot <- data[indices, ]
  
  
  # 2. Crear variables necesarias
  muestra_boot$log_ingreso <- log(muestra_boot$ingreso_total)
  

  # 3. Regresión de Y sobre todos los controles
  modelo_y <- lm(
    log_ingreso ~ nivel_educ + nivel_educ:ans_educ +
      experiencia_pot + I(experiencia_pot^2) +
      factor(estrato_energia),
    data = muestra_boot
  )
  
  # Residuos de Y después de eliminar los controles
  y_res <- residuals(modelo_y)
  
  
  # 4. Regresión de sexo sobre todos los controles
  modelo_sexo <- lm(
    sexo ~ nivel_educ + nivel_educ:ans_educ +
      experiencia_pot + I(experiencia_pot^2) +
      factor(estrato_energia),
    data = muestra_boot
  )
  
  # Residuos de sexo después de eliminar los controles
  sexo_res <- residuals(modelo_sexo)
  
  
  # 5. Estimador FWL
  beta_FWL <- sum(sexo_res * y_res) /
    sum(sexo_res^2)
  
 
  
  # 6. Devolver coeficiente FWL

  return(beta_FWL)
}


set.seed(123) #Semilla

boot_FWL <- boot(
  data = base_analisis,
  statistic = FWL_boot,
  R = 1000
)

#Coeficiente del modelo original
boot_FWL$t0

# Distribución de los 1000 coeficientes bootstrap
boot_FWL$t

# Sesgo bootstrap
bias_boot_FWL <- mean(boot_FWL$t) - boot_FWL$t0
bias_boot_FWL

# Error estándar bootstrap
se_boot_FWL <- sd(boot_FWL$t)
se_boot_FWL

#Intervalos de confianza bootstrap
IC_boot_gap_FWL <- boot.ci(
  boot_FWL,
  type = "perc"
)
IC_boot_gap_FWL

