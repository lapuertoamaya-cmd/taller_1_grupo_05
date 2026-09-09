# Taller 1 - Big Data y Machine Learning

Este repositorio contiene el flujo de trabajo del Taller 1. El proyecto descarga una muestra de la GEIH 2018, construye una base de analisis para poblacion ocupada con ingresos positivos y desarrolla ejercicios descriptivos, econometricos y predictivos sobre ingresos laborales.

El objetivo de esta organizacion es facilitar la replicabilidad del analisis: separar datos, codigo y resultados; documentar el orden de ejecucion; y permitir que otra persona entienda que insumo produce cada etapa.

## Estructura del proyecto

```text
taller_1_grupo_05/
├── 01_data/
│   ├── 01_raw/
│   └── 02_clean/
├── 02_code/
├── 03_outputs/
├── 00_main.R
└── README.md
```

## Carpetas

### `01_data/`

Contiene los datos usados y producidos por el taller.

- `01_data/01_raw/`: datos originales descargados desde la fuente web. Actualmente contiene `geih2018_sample.xlsx`, que consolida los fragmentos publicados en la pagina de la muestra GEIH 2018.
- `01_data/02_clean/`: datos limpios y listos para analisis. Actualmente contiene `base_analisis.rds` y `base_analisis.csv`.

### `02_code/`

Contiene los scripts de R del proyecto.

- `00_main.R`: script maestro previsto para declarar rutas y ejecutar el flujo completo.
- `01_scrapping_geih2018_sample.R`: descarga y consolida los 10 fragmentos HTML de la muestra GEIH 2018.
- `02_cleaning.R`: filtra la muestra de analisis y construye variables relevantes para los ejercicios posteriores.
- `04_section_02.R`: contiene ejercicios de estadisticas descriptivas, brecha salarial, bootstrap y Frisch-Waugh-Lovell.
- `05_section_03.R`: construye muestras de entrenamiento y prueba, estima modelos predictivos, evalua RMSE, LOOCV e importancia de variables.
- `Web Scraping Taller 1.R`: version alternativa o preliminar del scraping web.

### `03_outputs/`

Contiene los resultados exportados por los scripts.

- Tablas descriptivas y de desempeno en formato `.xlsx`.
- Resultados de modelos en archivos `.txt`.
- Graficas de correlaciones, ajuste predictivo, importancia de variables y dependencia parcial en formato `.png`.

## Flujo de trabajo

El pipeline general del taller es:

1. Descargar y consolidar datos crudos desde la pagina web de la muestra GEIH 2018.
2. Filtrar la muestra a personas mayores de edad, ocupadas y con ingreso laboral positivo.
3. Renombrar variables y construir variables derivadas, como anos de educacion, experiencia potencial, logaritmo del ingreso y variables categoricas.
4. Separar la muestra en entrenamiento y prueba usando el identificador de chunk.
5. Estimar modelos descriptivos, econometricos y predictivos.
6. Exportar tablas, graficas y resultados a `03_outputs/`.

## Datos

La fuente principal es:

```text
https://ignaciomsarmiento.github.io/GEIH2018_sample/
```

Los datos se encuentran publicados como tablas HTML en 10 fragmentos:

```text
https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html
...
https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html
```

La base cruda consolidada se guarda en:

```text
01_data/01_raw/geih2018_sample.xlsx
```

La base limpia se guarda en:

```text
01_data/02_clean/base_analisis.rds
01_data/02_clean/base_analisis.csv
```

## Replicabilidad

Para replicar el taller desde cero, se recomienda ejecutar los scripts desde la raiz del repositorio o mediante el archivo maestro `00_main.R`, verificando antes que los nombres de los scripts llamados alli coincidan con los nombres existentes en `02_code/`.

Orden sugerido de ejecucion:

```r
source("02_code/01_scrapping_geih2018_sample.R", encoding = "UTF-8")
source("02_code/02_cleaning.R", encoding = "UTF-8")
source("02_code/04_section_02.R", encoding = "UTF-8")
source("02_code/05_section_03.R", encoding = "UTF-8")
```

El scraping incluye pausas entre solicitudes para reducir la carga sobre el servidor. La base cruda debe conservar el identificador de `chunk`, porque este permite separar los fragmentos 1 a 7 como entrenamiento y los fragmentos 8 a 10 como prueba.

## Dependencias

Los scripts usan principalmente los siguientes paquetes de R:

- `tidyverse`
- `dplyr`
- `rvest`
- `httr`
- `readxl`
- `writexl`
- `caret`
- `boot`
- `stargazer`
- `gt`
- `rstudioapi`
- `pacman`

Algunos scripts instalan o cargan dependencias automaticamente con `pacman::p_load()`. Para una ejecucion mas controlada, conviene tener los paquetes instalados antes de correr el pipeline completo.

## Productos principales

Entre los productos generados se encuentran:

- `03_outputs/03.1_descriptivas.xlsx`: estadisticas descriptivas para train, test y muestra total.
- `03_outputs/03.4a_modelos_train.txt`, `03_outputs/03.4b_modelos_train.txt`, `03_outputs/03.4c_modelos_train.txt`: resultados de modelos estimados en entrenamiento.
- `03_outputs/03.5_rmse_test_loocv.xlsx`: comparacion de desempeno predictivo por RMSE y LOOCV.
- `03_outputs/03.6_tablas_importancia_vars.xlsx`: importancia de variables en modelos seleccionados.
- `03_outputs/*.png`: graficas de correlacion, relaciones bivariadas, complejidad contra RMSE, importancia de variables y dependencia parcial.

## Reglas del repositorio

- Mantener datos crudos en `01_data/01_raw/` y no modificarlos manualmente.
- Guardar bases procesadas en `01_data/02_clean/`.
- Mantener todo el codigo reproducible en `02_code/`.
- Exportar tablas y figuras solamente a `03_outputs/`.
