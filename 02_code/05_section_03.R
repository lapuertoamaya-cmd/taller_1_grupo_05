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
  gt,          # Formato y exportación de tablas
  writexl
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

vars_desc <- c(
  "edad",
  "ans_educ",
  "edu_may_10",
  "informal",
  "estrato_energia",
  "estrato_factor",
  "horas_trabajadas" ,
  "tamanio_firma",
  "sexo",
  "exp_potencial",
  "ln_ingreso_total"
)

# (3.1) descriptivas generales

descr_grales <- base_analisis |>
  select(all_of(vars_desc)) |>
  summarise(
    across(
      everything(),
      list(
        N = ~ sum(!is.na(.x)),
        media = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        mediana = ~ median(.x, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(N|media|sd|mediana|p25|p75|min|max)$"
  ) |>
  mutate(
    variable = factor(variable, levels = vars_desc)
  ) |>
  arrange(variable) |>
  mutate(
    variable = as.character(variable)
  )

write_xlsx(
  descr_grales,
  path = "03.1_tabla_descriptivas_grales.xlsx"
)

# (3.2) descriptivas training
descr_train <- training |>
  select(all_of(vars_desc)) |>
  summarise(
    across(
      everything(),
      list(
        N = ~ sum(!is.na(.x)),
        media = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        mediana = ~ median(.x, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(N|media|sd|mediana|p25|p75|min|max)$"
  ) |>
  mutate(
    variable = factor(variable, levels = vars_desc)
  ) |>
  arrange(variable) |>
  mutate(
    variable = as.character(variable)
  )

write_xlsx(
  descr_train,
  path = "03.1_tabla_descriptivas_train.xlsx"
)





# (3.3) descriptivas test



# ------------------------------------------------------------------------------
# (4) estructura de correlaciones entre las variables seleccionadas
# ------------------------------------------------------------------------------

# (4.1) mapa de correlaciones

labels_corr <- c(
  "edad"              = "Edad",
  "ans_educ"          = "Educación",
  "informal"          = "Informal",
  "estrato_energia"   = "Estrato",
  "horas_trabajadas"  = "Horas trabajadas",
  "tamanio_firma"     = "Tamaño firma",
  "sexo"              = "Sexo",
  "exp_potencial"     = "Experiencia",
  "ln_ingreso_total"  = "Ingreso"
)

mat_corr


corr_plot <- mat_corr |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(
    -var1,
    names_to = "var2",
    values_to = "correlacion"
  ) |>
  ggplot(aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile() +
  geom_text(aes(label = round(correlacion, 2))) +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  scale_x_discrete(labels = labels_corr) +
  scale_y_discrete(labels = labels_corr) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Correlación"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("03_outputs/03.1_corr_variables.png", plot=corr_plot)

# (4.2) visualizacion de correlaciones concretas con el ingreso

visualizador_train_corr <- function(var,label,color) {
  ggplot(training, aes(x=.data[[var]], y=ln_ingreso_total)) +
    geom_point(color=color) +
    labs(x=label, y="Log Ingreso") +
    theme_classic(base_family="serif")
}

visualizador_train_factor <- function(var,label) {
  
  ggplot(training, aes(x=factor(.data[[var]]), y=ln_ingreso_total)) +
    geom_boxplot() +
    labs(x=label, y="Log Ingreso") +
    theme_classic(base_family="serif")
}

# (4.2.1) edad
visualizador_train_corr("edad", "Edad", "darkblue")

# (4.2.3) edu
edu <- visualizador_train_corr("ans_educ", "Años de Educación", "darkgreen")
ggsave("03_outputs/03.2_corr_edu.png", plot=edu)

# (4.2.4) tamanio firma
tf <- visualizador_train_corr("tamanio_firma", "Tamaño de la Firma", "purple")
tf2 <- visualizador_train_factor("tamanio_firma", "Tamaño de la Firma")

ggsave("03_outputs/03.3_corr_tam_firma.png", plot=tf)
ggsave("03_outputs/03.4_box_tam_firma.png", plot=tf2)

# (4.2.5) estrato
estrato <- visualizador_train_corr("estrato_energia", "Estrato", "darkorange")
estrato_f <- visualizador_train_factor("estrato_factor", "Estrato")

ggsave("03_outputs/03.5_corr_estrato.png", plot=estrato)
ggsave("03_outputs/03.6_box_estrato.png", plot=estrato_f)

# (4.2.6) horas trabajadas
horas <- visualizador_train_corr("horas_trabajadas", "Horas Trabajadas", "darkred")
ggsave("03_outputs/03.7_corr_horas_trabajadas.png", plot=horas)

# ------------------------------------------------------------------------------
# (5) estimacion principal de modelos
# ------------------------------------------------------------------------------

# (5.1) modelo de edad incondicional

modelo_incondicional <- lm(log(ingreso_total) ~ edad + I(edad^2), data = base_analisis)

# (5.4) modelo de genero condicional


# (5.5) modelo adicional 1


# (5.6) modelo adicional 2

# (5.7) modelo adicional 3

# (5.8) modelo adicional 4

# (5.9) modelo adicional 5


# ------------------------------------------------------------------------------
# (6) xxx
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# (7) 
# ------------------------------------------------------------------------------



# (9.1)

loocv_ols <- function(modelo) {
  
  e <- resid(modelo)
  h <- hatvalues(modelo)
  
  sqrt(mean((e / (1 - h))^2))
}

modelo_final <- lm(
  ln_ingreso_total ~ edad + ans_educ + sexo + informal, # ejemplo, lo cambio cuando esté el final
  data = training
)

loocv_ols(modelo_final)
















# (99) revision general de modelos

# (99.1) que captura más varianza, estrato normal o como factor? FACTOR
modelo_estrato_num <- lm(ln_ingreso_total ~ estrato_energia, data=training)
modelo_estrato_fac <- lm(ln_ingreso_total ~ estrato_factor, data=training)
summary(modelo_estrato_fac)
summary(modelo_estrato_num)

# (99.2) educacion y educacion al cuadrado aporta?
modelo_edu <- lm(ln_ingreso_total ~ ans_educ, data=training)
modelo_edu2 <- lm(ln_ingreso_total ~ ans_educ + I(ans_educ^2), data=training)
modelo_edu_dummy10 <- lm(ln_ingreso_total ~ ans_educ + ans_educ:edu_may_10 , data=training)
summary(modelo_edu)
summary(modelo_edu2)
summary(modelo_edu_dummy10)


# (99.3) edad y edad2 aporta? Sí, poquito, por si solo
modelo_edad <- lm(ln_ingreso_total ~ edad, data=training)
modelo_edad2 <- lm(ln_ingreso_total ~ edad + I(edad^2), data=training)
summary(modelo_edad)
summary(modelo_edad2)

# (99.3) edad y edad2 aporta? Sí, poquito, por si solo
modelo_educ_edad <- lm(ln_ingreso_total ~ ans_educ + edad, data=training)
modelo_educ2_edad <- lm(ln_ingreso_total ~ ans_educ + I(ans_educ^2) + edad, data=training)
modelo_educ_edad2 <- lm(ln_ingreso_total ~ ans_educ + edad + I(edad^2), data=training)
modelo_educ2_edad2 <- lm(ln_ingreso_total ~ ans_educ + I(ans_educ^2) + edad + I(edad^2), data=training)

summary(modelo_educ_edad)
summary(modelo_educ2_edad)
summary(modelo_educ_edad2)
summary(modelo_educ2_edad2)

#(99.4) pero los cuadrádos no parecen tener buena intuición económica, intentemos con dummy los dos ultimos modelos
modelo_educ_dummy_edad <- lm(ln_ingreso_total ~ ans_educ*edu_may_10 + edad, data=training)
modelo_educ_dummy_edad2 <- lm(ln_ingreso_total ~ ans_educ*edu_may_10 + edad + I(edad^2), data=training)


summary(modelo_educ_dummy_edad)
summary(modelo_educ_dummy_edad2)




