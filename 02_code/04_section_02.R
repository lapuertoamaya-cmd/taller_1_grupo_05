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

#Recodificando la variable sexo donde 1: mujer y 0: Hombre
base_analisis <- base_analisis %>%
  mutate(sexo = 1 - sexo)

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
    data = base_analisis
  )
  
  # 3. Extraer el coeficiente de sexo
  coef_sexo <- coef(modelo_boot)["sexo"]
  
  # 4. Devolverlo
  return(coef_sexo)
}

set.seed(123) #semilla

boot_Sexo <- boot(
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


#Interpretación ----------------------------------------------------------------
# Resultado porcentual 
Transformación_Beta <- ((exp(coef(Modelo_brecha)["sexo"]) - 1) * 100)
Transformación_Beta

#Interpretación
#En promedio, el salario de las mujeres es 21,71% mas bajo en comparación a los 
#hombres. Tenemos una confianza del 95% de que el efecto poblacional de la 
#discriminación salarial estara entre (-0,267, -0,209).

#Modelo de genero con controles-------------------------------------------------
modelo_gap_controles <- lm(
  log(ingreso_total) ~ sexo + nivel_educ + nivel_educ:ans_educ + 
    experiencia_pot + I(experiencia_pot^2) + factor(estrato_energia),
  data = base_analisis
)

#Resultados---------------------------------------------------------------------
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

