# =============================================================================
# T3 — Regresión económica: CASEN y ENE
#
# Autor: Camila Bravo Figueroa
# Fecha: 13-10-2026
# Descripción: 

# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)

# =============================================================================
# Parte A: CASEN 
# =============================================================================
# -----------------------------------------------------------------------------
# A1. Dos modelos
# -----------------------------------------------------------------------------

# Para cargar los datos y agregar la columna de experiencia potencial de Mincer.
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

# Para crear el modelo de regresión simple sin variables de control ->  ¿cuánto 
# más ingreso se asocia con cada año adicional de educación al comparar cualquier 
# par de personas?
m1 <- lm(ingreso ~ educ, data = casen)

# Para crear el modelo de regresión con controles -> ¿cuánto más ingreso se asocia
# con cada año adicional de educación al comparar personas con la misma experiencia 
# y género?
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)
        
# -----------------------------------------------------------------------------
# A2. La tabla de coeficientes, con `broom` + `dplyr`
# -----------------------------------------------------------------------------

# Para crear un objeto al que llamar cuando se quiera observar la tabla de 
# coeficientes obtenida con el código.
tabla_coeficientes <- tidy(m2, conf.int = TRUE) |>
  # Para quitar la fila del intercepto.
  filter(term != "(Intercept)") |>   
  # Para redondear la estimación y el IC, y agregar una columna lógica que indica 
  # si el p-value es menor al 5 %.
  mutate(across(c(estimate, conf.low, conf.high), round),
         significativo = p.value < 0.05) |>     
  # Para seleccionar las columnas de interés.
  select(term, estimate, conf.low, conf.high, p.value, significativo) |>
  # Para ordenar de menor a mayor p-value, con lo más significativo arriba.
  arrange(p.value)

tabla_coeficientes

# -----------------------------------------------------------------------------
# A3. Comparar modelos
# -----------------------------------------------------------------------------

# Crea un modelo que considere la interacción permitiendo que la pendiente de 
# educación  difiera entre hombres y mujeres con la misma experiencia.
m3 <- lm(ingreso ~ educ * genero + experiencia, data = casen)

# Junta los 3 modelos con nombres para identificar cada uno.
modelos <- list(simple = m1, controles = m2, mio = m3)

# Para comparar los tres modelos, resumiendo cada uno en una fila. Usando ´lapply()´
# para aplicarlo en cada elemento de la lista.
lapply(modelos, glance) |>
  # Apila las tres tablas en una sola usando .id para crear la columna ´modelo´ 
  # con los nombres de cada uno.
  bind_rows(.id = "modelo") |>
  # Deja solo las columnas de interés para comparar.
  select(modelo, r.squared, adj.r.squared, AIC, nobs)

# ¿Qué pasa con el R² y con el R² ajustado? ¿El modelo mejora algo?

# El R² sube y el R² ajustado disminuye. Sin embargo, esto no es señal de mejora, 
# sino que el aumento en R² se debe a que cada modelo contiene al anterior, y la 
# disminución de R² ajustado se debe a que castiga la inclusión de variables 
# que no aportan capacidad explicativa.

# -----------------------------------------------------------------------------
# A4. ¿Cambió el coeficiente de educación?
# -----------------------------------------------------------------------------

# Para apilar los coeficientes de los tres modelos y comparar solo el de educación.
bind_rows(simple    = tidy(modelos$simple),
          controles = tidy(modelos$controles),
          mio       = tidy(modelos$mio), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

# Para verificar la correlación entre educación y experiencia en la muestra.
cor(casen$educ, casen$experiencia)

# ¿Por qué cambia o no cambia el coeficiente de educación?
#
# Para que exista un cambio en un coeficiente, el control debe estar relacionado 
# con educ e ingreso a la vez, pero en este caso, la experiencia casi no se relaciona 
# con educ (-0.156), por lo que del modelo "simple" al de "controles", el coeficiente 
# casi no cambia. Por otro lado, en el modelo "mio" existe un cambio porque con la 
# interacción entre educación y género el término educ pasa a medir la pendiente 
# solo para la categoría base F.

# -----------------------------------------------------------------------------
# A5. Diagnóstico
# -----------------------------------------------------------------------------

# Para identificar las tres observaciones peor predichas (mayor residuo en
# valor absoluto) usando augment() y arrange().
augment(m2) |>
  select(ingreso, educ, genero, .fitted, .resid) |>
  mutate(across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  head(3)

# ¿Qué tienen en común?

# Los tres son hombres con residuos negativos grandes (entre -$249.293 y -$298.210),  
# lo cual indica que ganan entre ~$250.000 y ~$300.000 menos de lo que el modelo 
# predice para sus años de educación y experiencia.

# Para obtener la máxima distancia de Cook (.cooksd) y el máximo apalancamiento 
# (.hat) del modelo.
augment(m2) |>
  summarise(max_cooksd = max(.cooksd),
            max_hat    = max(.hat))

# Para identificar a qué fila original de casen (.rownames) corresponde la
# mayor distancia de Cook .
augment(m2) |>
  slice_max(.cooksd, n = 1) |>
  select(.rownames, ingreso, educ, genero, .cooksd)

# Para re-estimar m2 sin la observación más influyente (fila 43) en un solo
# pipe y comparar el coeficiente de educación.
m2_sin <- casen |>
  filter(row_number() != 43) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

bind_rows(todos = tidy(m2), sin_43 = tidy(m2_sin), .id = "muestra") |>
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)

# Al quitar la observación 43, el coeficiente de educ sube de $42.246 a $45.218, 
# manteniéndose estadísticamente significativo.

# -----------------------------------------------------------------------------
# A6. Interpretación de la Parte A
# -----------------------------------------------------------------------------

# En el modelo con controles, cada año adicional de educación se asocia con
# $42.246 más de ingreso mensual, en promedio, manteniendo experiencia y
# género constantes (IC 95 %: $29.487 a $55.004).

# El coeficiente de `generoM` es $55.686. Esto significa que ser hombre se asocia, 
# en promedio,con $55.686 más de ingreso mensual respecto a una mujer (la categoría 
# base, F) con la misma educación y experiencia.

# El coeficiente de generoM es grande ($55.686) pero no significativo (p-value = 0.204).
# Además, con un n = 55 el error estándar es muy alto, por lo que no se puede distinguir 
# del ruido muestral.

