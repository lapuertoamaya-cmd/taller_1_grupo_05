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

p_load(rstudioapi, stargazer, readxl,
       tidyverse,  # Manipulación, descriptivas y gráficos
       caret,      # RMSE y validación
       gt,          # Formato y exportación de tablas
       writexl,
       purr)  # para imap_dfr

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
# (1) importar base
# ------------------------------------------------------------------------------

base_analisis <- read.csv("01_data/02_clean/base_analisis.csv")

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

# (3.1) variables
labels_num <- c(
  edad              = "Edad",
  ans_educ          = "Años de educación",
  horas_trabajadas  = "Horas trabajadas",
  mujer             = "Mujer",
  informal          = "Informal",
  cuenta_propia     = "Cuenta propia",
  ingreso_total  = "Ingreso"
)

labels_cat <- c(
  nivel_educ      = "Nivel educativo",
  estrato_factor  = "Estrato",
  tamanio_firma   = "Tamaño de firma"
)

# (3.2) funcion para vars continuas y categoricas

descriptivas_num <- function(data) {
  
  map_dfr(names(labels_num), function(v) {
    
    x <- data[[v]]
    
    tibble(
      variable = labels_num[[v]],
      N = sum(!is.na(x)),
      media = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE),
      p25 = quantile(x, 0.25, na.rm = TRUE),
      p75 = quantile(x, 0.75, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })
}

descriptivas_cat <- function(data) {
  
  map_dfr(names(labels_cat), function(v) {
    
    data |>
      filter(!is.na(.data[[v]])) |>
      group_by(categoria = as.character(.data[[v]])) |>
      summarise(
        N = n(),
        ingreso_medio = mean(ingreso_total, na.rm = TRUE),
        ingreso_mediano = median(ingreso_total, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        porcentaje = N / sum(N),
        variable = labels_cat[[v]],
        .before = 1
      )
  })
}

# (3.3) ejecucion para training, test y general

descr_train_num <- descriptivas_num(training)
descr_train_cat <- descriptivas_cat(training)

descr_test_num <- descriptivas_num(test)
descr_test_cat <- descriptivas_cat(test)

descr_general_num <- descriptivas_num(base_analisis)
descr_general_cat <- descriptivas_cat(base_analisis)


write_xlsx(
  list(
    "Train - Numéricas" = descr_train_num,
    "Train - Categóricas" = descr_train_cat,
    "Test - Numéricas" = descr_test_num,
    "Test - Categóricas" = descr_test_cat,
    "Total - Numéricas" = descr_general_num,
    "Total - Categóricas" = descr_general_cat
  ),
  path = "03_outputs/03.1_descriptivas.xlsx"
)

# ------------------------------------------------------------------------------
# (4) estructura de correlaciones entre las variables seleccionadas en TRAINING
# ------------------------------------------------------------------------------

# (4.1) mapa de correlaciones

labels_corr <- c(
  "edad"              = "Edad",
  "ans_educ"          = "Años de Educación",
  "edu_sup"           = "Educacion Superior",
  "informal"          = "Informal",
  "estrato_energia"   = "Estrato",
  "estrato_factor",
  "horas_trabajadas"  = "Horas trabajadas",
  "tamanio_firma"     = "Tamaño Firma",
  "mujer"             = "Mujer",
  "exp_potencial"     = "Experiencia",
  "ln_ingreso_total"  = "Ingreso"
)

vars_corr <- names(labels_corr) # saca los nombres de las vars

X_corr <- model.matrix( # lo convierte en una matriz que convierte factores a dummies
  reformulate(vars_corr),
  data=training,
  na.action = na.pass
)[, -1, drop = FALSE]

mat_corr <- cor(X_corr, use = "pairwise.complete.obs")

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

ggsave("03_outputs/03.0_corr_variables.png", plot=corr_plot)

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
edad1 <- visualizador_train_corr("edad", "Edad", "darkblue")
ggsave("03_outputs/03.1_corr_edad.png", plot=edad1)

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

# (5.0) funcion de estimacion
estimacion_modelos <- function(X, train=TRUE) {
  formula <- reformulate(termlabels=X, response="ln_ingreso_total")
  if (train) {
    modelo <- lm(formula, data=training)
  }
  else {
    modelo <- lm(formula, data=test)
  }
  print(summary(modelo))
  return(modelo)
}

# (5.1) estimacion modelo TRAIN 

# (5.1.1) modelo de edad incondicional
m1.1 <- c("edad","I(edad^2)")
rm1.1 <- estimacion_modelos(m1.1)

# (5.1.2) modelo de edad condicional
m1.2 <-c("edad","I(edad^2)", "horas_trabajadas", "tipo_ocupacion")
rm1.2 <- estimacion_modelos(m1.2)

# (5.1.3) modelo genero incondicional
m2.1 <- c("mujer")
rm2.1 <- estimacion_modelos(m2.1)
  
# (5.1.4) modelo de genero condicional
m2.2 <- c("mujer", "nivel_educ", "nivel_educ:ans_educ", "exp_potencial", "I(exp_potencial^2)", "estrato_factor")
rm2.2 <- estimacion_modelos(m2.2)

# (5.1.5) modelo adicional 1
ma1 <- c("edad", "I(edad^2)", "ans_educ", "nivel_educ:ans_educ")
rma1 <- estimacion_modelos(ma1)

# (5.1.6) modelo adicional 2
ma2 <- c("edad", "I(edad^2)", "tamanio_firma", "horas_trabajadas", "estrato_energia", "informal")
rma2 <- estimacion_modelos(ma2)
  
# (5.1.7) modelo adicional 3
ma3 <- c("edad", "I(edad^2)", "nivel_educ*ans_educ", "tamanio_firma", "horas_trabajadas", "estrato_energia")
rma3 <- estimacion_modelos(ma3)
  
# (5.1.8) modelo adicional 4
ma4 <- c("edad", "I(edad^2)", "ans_educ", "tamanio_firma", "estrato_factor", "informal:cuenta_propia")
rma4 <- estimacion_modelos(ma4)

# (5.1.9) modelo adicional 5
ma5 <- c("edad", "I(edad^2)", "nivel_educ*ans_educ", "factor(tamanio_firma)", "estrato_factor")
rma5 <- estimacion_modelos(ma5)

# (5.1.10) modelo adicional 6
ma6 <- c("edad", "I(edad^2)", "ans_educ", "nivel_educ:ans_educ", "estrato_factor", "informal")
rma6 <- estimacion_modelos(ma6)

# (5.1.11) modelo adicional 7
ma7 <- c("edad", "I(edad^2)", "tamanio_firma", "estrato_energia", "cuenta_propia")
rma7 <- estimacion_modelos(ma7)

# (5.1.12) modelo adicional 8
ma8 <- c("edad", "I(edad^2)", "ans_educ*estrato_factor", "nivel_educ:ans_educ", "factor(tamanio_firma)")
rma8 <- estimacion_modelos(ma8)

# (5.1.13) modelo adicional 9
ma9 <- c("edad", "I(edad^2)", "horas_trabajadas","ans_educ","nivel_educ:ans_educ", "factor(tamanio_firma)", "estrato_factor")
rma9 <- estimacion_modelos(ma9)

# (5.1.14) modelo adicional 10
ma10 <- c("edad", "I(edad^2)", "horas_trabajadas", "ans_educ", "nivel_educ:ans_educ", "tamanio_firma", "informal*cuenta_propia", "estrato_energia")
rma10 <- estimacion_modelos(ma10)

# (5.1.15) modelo adicional 11
ma11 <- c("edad", "I(edad^2)", "horas_trabajadas","nivel_educ:ans_educ", "tamanio_firma", "informal:cuenta_propia")
rma11 <- estimacion_modelos(ma11)

# (5.1.16) modelo adicional 12
ma12 <- c("edad", "I(edad^2)", "nivel_educ:horas_trabajadas", "nivel_educ:ans_educ", "tamanio_firma", "informal:cuenta_propia")
rma12 <- estimacion_modelos(ma12)

# (5.1.16) modelo adicional 13
ma13 <- c("edad", "I(edad^2)", "horas_trabajadas",
          "edu_sup*ans_educ", "tamanio_firma", "informal*cuenta_propia", "estrato_energia")
rma13 <- estimacion_modelos(ma13)

# (5.1.17) modelo adicional 14
ma14 <- c("edad", "I(edad^2)", "edu_sup*horas_trabajadas",
          "edu_sup*ans_educ", "tamanio_firma", "informal:cuenta_propia", "estrato_energia")
rma14 <- estimacion_modelos(ma14)

# (5.1.18) modelo adicional 15
ma15 <- c("edad", "I(edad^2)", "informal:cuenta_propia", "informal",
          "edu_sup*ans_educ","tamanio_firma*mujer", "edu_sup*horas_trabajadas",
          "estrato_energia")
rma15 <- estimacion_modelos(ma15)

# (5.1.19) modelo adicional 16
ma16 <- c("edad", "I(edad^2)", "informal:cuenta_propia", 
          "edu_sup*ans_educ", "edu_sup:horas_trabajadas", "estrato_energia*mujer*factor(tamanio_firma)")
rma16 <- estimacion_modelos(ma16)

# (5.1.19) modelo adicional 17
ma17 <- c("edad:horas_trabajadas:cuenta_propia*informal", "I(edad^2)",
          "nivel_educ:ans_educ:horas_trabajadas", "estrato_energia*mujer*tamanio_firma")
rma17 <- estimacion_modelos(ma17)

# (5.1.20) modelo adicional 18
ma18 <- c("edad", "cuenta_propia*informal", "I(edad^2)", "ans_educ*edu_sup", "horas_trabajadas*estrato_energia","mujer*tamanio_firma")
rma18 <- estimacion_modelos(ma18)

# (5.1.21) modelo adicional 19
ma19 <- c("edad", "cuenta_propia:informal", "informal",
          "ans_educ*edu_sup", "edu_sup*horas_trabajadas", "estrato_energia", "mujer*tamanio_firma")
rma19 <- estimacion_modelos(ma19)

# (5.1.22) modelo adicional 20
ma20 <- c("edad:horas_trabajadas", "cuenta_propia*informal", "I(edad^2)",
          "mujer:ans_educ*nivel_educ", "horas_trabajadas*estrato_factor", "factor(tamanio_firma)")
rma20 <- estimacion_modelos(ma20)

# (5.1.19) exportacion
stargazer(rm1.1, rm1.2, rm2.1, rm2.2, rma1, rma2, rma3, rma4, rma5,
          type="text", 
          out="03_outputs/03.4a_modelos_train.txt")

stargazer(rma6, rma7, rma8, rma9, rma10, rma11, rma12, rma13, rma14,
          type="text", 
          out="03_outputs/03.4b_modelos_train.txt")

stargazer(rma15, rma16, rma17, rma18, rma19, rma20,
          type="text", 
          out="03_outputs/03.4c_modelos_train.txt")

# (5.2) desempeño y validacion

# (5.2.1) lista de modelos

modelos <- list(
  "M1.1" = rm1.1,
  "M1.2" = rm1.2,
  "M2.1" = rm2.1,
  "M2.2" = rm2.2,
  "MA1" = rma1,
  "MA2" = rma2,
  "MA3" = rma3,
  "MA4" = rma4,
  "MA5" = rma5,
  "MA6" = rma6,
  "MA7" = rma7,
  "MA8" = rma8,
  "MA9" = rma9,
  "MA10" = rma10,
  "MA11" = rma11,
  "MA12" = rma12,
  "MA13" = rma13,
  "MA14" = rma14,
  "MA15" = rma15,
  "MA16" = rma16,
  "MA17" = rma17,
  "MA18" = rma18,
  "MA19" = rma19,
  "MA20" = rma20
)

# (5.2.2) funcion generadora de un tible de desempeño conjunto
desempeno_modelos <- function(modelos) {
  
  imap_dfr(modelos, function(m, nombre) { #imap divide la lista en elementos y le entrega a la funcion los elementos
    
    pred_train <- predict(m, newdata = training)
    pred_test  <- predict(m, newdata = test)
    
    tibble(
      modelo = nombre,
      complejidad = sum(!is.na(coef(m))),
      RMSE_train = sqrt(mean((training$ln_ingreso_total - pred_train)^2, na.rm = TRUE)),
      RMSE_test  = sqrt(mean((test$ln_ingreso_total - pred_test)^2, na.rm = TRUE))
    )
  })
}

# (5.2.3) ejecucion
resultados_modelos <- desempeno_modelos(modelos)
print(resultados_modelos, n=100)

# (5.2.4) grafica
ggplot(resultados_modelos) +
  geom_line(aes(x = complejidad, y = RMSE_train, color = "Training")) +
  geom_line(aes(x = complejidad, y = RMSE_test, color = "Testing")) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Training" = "darkorange",
      "Testing" = "darkgreen"
    )
  ) +
  labs(
    x = "Número de Parámetros",
    y = "RMSE"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(ncol = 2)
  )

ggsave("03_outputs/03.8_complejidad_rmse.png")

# ------------------------------------------------------------------------------
# (6) estimamos modelos por loocv
# ------------------------------------------------------------------------------

loocv_ols <- function(modelo) {
  
  e <- resid(modelo)
  h <- hatvalues(modelo)
  
  sqrt(mean((e / (1 - h))^2))
}

# (6.1) estimacion
resultados_modelos$loocv <- map_dbl(modelos, loocv_ols)

# (6.2) exportar todos los resultados de los modelos
write_xlsx(resultados_modelos, path="03_outputs/03.5_rmse_test_loocv.xlsx")

# ------------------------------------------------------------------------------
# (7) estimacion de importancia de vars en mejores modelos (rma15, rma18, rma19)
# ------------------------------------------------------------------------------

vars_importancia <- c(
  "edad",
  "ans_educ",
  "edu_sup",
  "estrato_energia",
  "horas_trabajadas",
  "tamanio_firma",
  "factor(tamanio_firma)",
  "estrato_factor",
  "mujer",
  "informal",
  "cuenta_propia",
  "nivel_educ"
)

importancia_var <- function(modelo, variable) {
  
  terminos <- attr(terms(modelo), "term.labels")
  terminos_sin <- terminos[!grepl(variable, terminos, fixed = TRUE)]
  
  modelo_sin <- lm(
    reformulate(terminos_sin, response = "ln_ingreso_total"),
    data = training
  )
  
  rmse_full <- sqrt(mean(
    (test$ln_ingreso_total - predict(modelo, test))^2,
    na.rm = TRUE
  ))
  
  rmse_sin <- sqrt(mean(
    (test$ln_ingreso_total - predict(modelo_sin, test))^2,
    na.rm = TRUE
  ))
  
  tibble(
    variable = variable,
    RMSE_full = rmse_full,
    RMSE_sin = rmse_sin,
    importancia = rmse_sin - rmse_full
  )
}

# mejor modelo
tabla_importancia_rma18 <- map_dfr(
  vars_importancia,
  ~ importancia_var(rma18, .x)
)

# segundo mejor
tabla_importancia_rma15 <- map_dfr(
  vars_importancia,
  ~ importancia_var(rma15, .x)
)

# tercer mejor
tabla_importancia_rma19 <- map_dfr(
  vars_importancia,
  ~ importancia_var(rma19, .x)
)

write_xlsx(
  list(
    "1 MA18" = tabla_importancia_rma18,
    "2 MA15" = tabla_importancia_rma15,
    "3 MA 19" = tabla_importancia_rma19
  ),
  path = "03_outputs/03.6_tablas_importancia_vars.xlsx"
)

# (7.2) grafico
tabla_importancia <- bind_rows(
  tabla_importancia_rma18 |> mutate(modelo = "1°: MA18 (p=15, RMSE=0.607)"),
  tabla_importancia_rma15 |> mutate(modelo = "2°: MA15 (p=14, RMSE=0.608)"),
  tabla_importancia_rma19 |> mutate(modelo = "3°: MA19 (p=13, RMSE=0.615)")
)

ggplot(
  tabla_importancia,
  aes(x = variable, y = importancia, fill = modelo)
) +
  geom_col(position = "dodge") +
  labs(
    x = NULL,
    y = "Aumento del RMSE al retirar la variable",
    fill = "Modelo"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  )
ggsave("03_outputs/03.9_vars_importance.png")

#-------------------------------------------------------------------------------
# (8) dependencia de las vars mas improtantes
#-------------------------------------------------------------------------------

var_imp <- "horas_trabajadas"

grid <- seq(
  min(training[[var_imp]], na.rm = TRUE),
  max(training[[var_imp]], na.rm = TRUE),
  length.out = 100
)

dependencia <- map_dfr(grid, function(x) {
  
  d <- training
  d[[var_imp]] <- x
  
  tibble(
    valor = x,
    pred = mean(predict(rma18, newdata = d), na.rm = TRUE)
  )
})

dep_h_tr <-ggplot(dependencia, aes(x = valor, y = pred)) +
  geom_line() +
  labs(
    x = "Horas trabajadas",
    y = "Log ingreso predicho"
  ) +
  theme_classic(base_family = "serif")

var_imp <- "ans_educ"

grid <- seq(
  min(training[[var_imp]], na.rm = TRUE),
  max(training[[var_imp]], na.rm = TRUE),
  length.out = 100
)

dependencia <- map_dfr(grid, function(x) {
  
  d <- training
  d[[var_imp]] <- x
  
  tibble(
    valor = x,
    pred = mean(predict(rma18, newdata = d), na.rm = TRUE)
  )
})

dep_edu <- ggplot(dependencia, aes(x = valor, y = pred)) +
  geom_line() +
  labs(
    x = "Años de Educación",
    y = "Log ingreso predicho"
  ) +
  theme_classic(base_family = "serif")


var_imp <- "horas_trabajadas"

grid <- expand_grid(
  horas_trabajadas = seq(
    min(training$horas_trabajadas),
    max(training$horas_trabajadas),
    length.out = 100
  ),
  estrato_energia = sort(unique(training$estrato_energia))
)

dependencia <- pmap_dfr(grid, function(horas_trabajadas, estrato_energia) {
  
  d <- training
  d$horas_trabajadas <- horas_trabajadas
  d$estrato_energia <- estrato_energia
  
  tibble(
    horas_trabajadas = horas_trabajadas,
    estrato_energia = estrato_energia,
    pred = mean(predict(rma18, newdata = d), na.rm = TRUE)
  )
})

dep_h_tr_estrato <- ggplot(
  dependencia,
  aes(
    x = horas_trabajadas,
    y = pred,
    color = factor(estrato_energia)
  )
) +
  geom_line() +
  labs(
    x = "Horas trabajadas",
    y = "Log ingreso predicho",
    color = "Estrato"
  ) +
  theme_classic(base_family = "serif")

var_imp <- "estrato_energia"

grid <- seq(
  min(training[[var_imp]], na.rm = TRUE),
  max(training[[var_imp]], na.rm = TRUE),
  length.out = 100
)

dependencia <- map_dfr(grid, function(x) {
  
  d <- training
  d[[var_imp]] <- x
  
  tibble(
    valor = x,
    pred = mean(predict(rma18, newdata = d), na.rm = TRUE)
  )
})

dep_estrato <- ggplot(dependencia, aes(x = valor, y = pred)) +
  geom_line() +
  labs(
    x = "Estrato",
    y = "Log ingreso predicho"
  ) +
  theme_classic(base_family = "serif")


ggsave("03_outputs/03.10_dependencia_y_h_tr.png", plot=dep_h_tr_estrato)
ggsave("03_outputs/03.11_dependencia_ans_educ.png", plot=dep_edu)
ggsave("03_outputs/03.12_dependencia_estrat.png", plot=dep_estrato)

# FIN