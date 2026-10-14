# =============================================================================
# T3 — Regresión económica: CASEN y ENE
#
# Autor: Camila Bravo Figueroa
# Fecha: 13-10-2026
# Descripción: Estimación, diagnóstico e interpretación de modelos de regresión
# lineal sobre ingresos en CASEN (Parte A) y análisis de regresión múltiple e
# interacción sobre la brecha de género en horas efectivas de trabajo en la ENE
# del trimestre mayo-julio 2026 (Partes B y C).

# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)

# =============================================================================
# Parte A - CASEN 
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

# =============================================================================
# Parte B - ENE: tu pregunta
# =============================================================================
# -----------------------------------------------------------------------------
# B1. Declara tu pregunta
# -----------------------------------------------------------------------------

# En la A3 se encontró una brecha promedio de 5 horas efectivas semanales menos  
# en mujeres ocupadas respecto a los hombres. ¿Cuánto de esa brecha de género se 
# mantiene al comparar personas de igual edad, nivel educacional, categoría 
# ocupacional y condición de formalidad, y difiere esa brecha entre el empleo 
# formal e informal?


# FICHA DE LA FUENTE:
# - Institución: Instituto Nacional de Estadísticas (INE)
# - Encuesta: Encuesta Nacional de Empleo (ENE)
# - Trimestre: Mayo - Julio 2026
# - Archivo base: data/raw/ene-2026-06-mjj.csv (y manual data/raw/codigos-ene-2020.pdf)
# - Enlace de descarga: https://www.ine.gob.cl/estadisticas-por-tema/mercado-laboral/ocupacion-y-desocupacion
# - Fecha de descarga: 29-09-2026
# - Unidad de observación: Persona (submuestra de análisis: personas ocupadas, activ == 1)
# -----------------------------------------------------------------------------
# B2. Prepara la base
# -----------------------------------------------------------------------------

# En la A3 se redujo la base a 6 columnas (sexo, activ, fact_cal, efectivas, 
# sexo_txt, situacion), faltando controles para agregar. Por lo cual, se retoma 
# el archivo original para agregar los controles necesarios y auditar sus faltantes 
# y centinelas.

ene_raw <- read.csv2("data/raw/ene-2026-06-mjj.csv")

# Se seleccionan las columnas necesarias originales, más las de control.
ene_sub <- ene_raw |>
  select(
    sexo,
    activ,
    fact_cal,
    efectivas,
    edad,
    ocup_form,
    categoria_ocupacion,
    cine11_1d
  )

# Auditoría de faltantes y códigos
colSums(is.na(ene_sub))
table(ene_sub$activ, useNA = "ifany")
table(ene_sub$ocup_form, useNA = "ifany")
table(ene_sub$categoria_ocupacion, useNA = "ifany")
table(ene_sub$cine11_1d, useNA = "ifany")

# Caza de códigos centinela en las horas efectivas de trabajo.
sort(table(ene_sub$efectivas[ene_sub$efectivas > 80]), decreasing = TRUE)

  
# Al revisar los datos, activ tiene 15.305 NAs estructurales (menores de 15 años) y 
# tanto efectivas como ocup_form tienen 56.096 NAs estructurales, que corresponden 
# a quienes no trabajan (desocupados, inactivos y menores de edad);esos mismos 
# 56.096 casos aparecen con código 0 en categoria_ocupacion, por lo que al filtrar 
# solo a los ocupados (41.850) se limpian automáticamente. Además, se pasan a NA 
# los centinelas 888 (103 casos) y 999 (8 casos) en efectivas, y el código 9 
# ("Sin clasificación", 503 casos) en cine11_1d para que no distorsionen el modelo.

ocupados <- ene_sub |>
  filter(activ == 1) |>
  mutate(
    mujer     = sexo == 2,
    informal  = ocup_form == 2,
    efectivas = na_if(efectivas, 999),
    efectivas = na_if(efectivas, 888),
    categoria = case_when(
      categoria_ocupacion == 1     ~ "Empleador",
      categoria_ocupacion == 2     ~ "Cuenta propia",
      categoria_ocupacion == 3     ~ "Asalariado privado",
      categoria_ocupacion == 4     ~ "Asalariado público",
      categoria_ocupacion %in% 5:6 ~ "Servicio doméstico",
      categoria_ocupacion == 7     ~ "Familiar no remunerado"
    ),
    educ_nivel = case_when(
      cine11_1d == 0     ~ "Sin instrucción",
      cine11_1d == 1     ~ "Primaria",
      cine11_1d == 2     ~ "Secundaria",
      cine11_1d == 3     ~ "Técnica superior",
      cine11_1d == 4     ~ "Universitaria",
      cine11_1d %in% 5:6 ~ "Postgrado"
    )
  )

# Se verifican los NA después de la limpieza en los 41.850 ocupados
colSums(is.na(ocupados))


# Tras filtrar a los 41.850 ocupados y limpiar, quedan exactamente 111 NA en
# efectivas (los 103 casos con 888 y 8 con 999) y 58 NA en educ_nivel (ocupados
# que tenían el código 9 = "Sin clasificación" en cine11_1d).

# -----------------------------------------------------------------------------
# B3. Al menos dos modelos
# -----------------------------------------------------------------------------

# Modelo base, con las horas efectivas según sexo 
b1 <- lm(efectivas ~ mujer, data = ocupados, weights = fact_cal)

# Modelo con controles, agregando edad, formalidad, categoría ocupacional y nivel
# educacional.
b2 <- lm(efectivas ~ mujer + edad + I(edad^2) + informal + categoria + educ_nivel,
         data = ocupados, weights = fact_cal)

# -----------------------------------------------------------------------------
# B4. Extrae y compara con `broom` + `dplyr`
# -----------------------------------------------------------------------------

# Crea la tabla con el coeficiente de la variable de interés en ambos modelos
bind_rows(base      = tidy(b1, conf.int = TRUE),
          controles = tidy(b2, conf.int = TRUE), .id = "modelo") |>
  filter(term == "mujerTRUE") |>
  mutate(across(c(estimate, std.error, conf.low, conf.high), round, 2)) |>
  select(modelo, term, estimate, std.error, conf.low, conf.high, p.value)

# En el modelo base la brecha es de -5,03 horas semanales (igual que en la A3), 
# y al controlar por las demás variables en b2 apenas cambia a -4,81 horas.

# Aplica glance() de ambos modelos apilados
modelos_ene <- list(base = b1, controles = b2)

lapply(modelos_ene, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, AIC, nobs)

# El R² y el R² ajustado suben de 0,0256 a 0,118 al agregar los controles, lo que 
# muestra que estas variables sí aportan a explicar las horas trabajadas.

# Un tercer modelo con una interacción para evaluar si la brecha de horas por género 
# difiere entre el empleo formal e informal.
b3_int <- lm(efectivas ~ mujer * informal + edad + I(edad^2) + categoria + educ_nivel,
             data = ocupados, weights = fact_cal)

tidy(b3_int, conf.int = TRUE) |>
  filter(term %in% c("mujerTRUE", "informalTRUE", "mujerTRUE:informalTRUE")) |>
  mutate(across(c(estimate, std.error, conf.low, conf.high), round, 2)) |>
  select(term, estimate, std.error, conf.low, conf.high, p.value)

# En los empleos formales la brecha es de -3,83 horas, pero con la interacción 
# (-3,75) en los empleos informales la diferencia aumenta a -7,58 horas semanales.

# -----------------------------------------------------------------------------
# B5. Sensibilidad
# -----------------------------------------------------------------------------

# Para evaluar cuánto cambia el coeficiente de mujerTRUE si estimamos el modelo 
# con controles sin ponderar.
b2_sin_ponderar <- lm(efectivas ~ mujer + edad + I(edad^2) + informal + categoria + educ_nivel,
                      data = ocupados)

bind_rows(ponderado    = tidy(b2),
          sin_ponderar = tidy(b2_sin_ponderar), .id = "estimacion") |>
  filter(term == "mujerTRUE") |>
  mutate(across(c(estimate, std.error), round, 2)) |>
  select(estimacion, term, estimate, std.error, p.value)

# Sin ponderar, el coeficiente pasa de -4,81 a -4,90 horas semanales, por lo que 
# el resultado casi no varía con o sin el factor de expansión.

# -----------------------------------------------------------------------------
# B6. Interpretación de la Parte B
# -----------------------------------------------------------------------------

# En el modelo base, que compara directamente el promedio general entre hombres y 
# mujeres ocupados, ser mujer se asocia con 5,03 horas efectivas menos de trabajo 
# a la semana, tal como se había visto en la A3. Al pasar al modelo con controles, 
# que ahora responde cuánto difiere la jornada entre hombres y mujeres que tienen 
# la misma edad, nivel educacional, categoría ocupacional y formalidad, la brecha 
# casi no se mueve y se asocia con 4,81 horas semanales menos. Sin embargo, al 
# agregar la interacción se ve que en un empleo formal esta diferencia se asocia 
# con 3,83 horas menos, mientras que en la informalidad se suman otras 3,75 horas 
# menos, llegando a una brecha de 7,58 horas semanales.
#
# Si bien con más de 41.600 observaciones casi cualquier coeficiente resulta 
# estadísticamente significativo, una diferencia de cerca de 5 horas semanales 
# es un resultado grande e importante en la práctica. 

# Ahora bien, al leer estos resultados hay que tener en cuenta que 
# el modelo no incluye variables como el cuidado de los hijos o las tareas del 
# hogar, que suelen recaer más en las mujeres y reducen el tiempo disponible para 
# el trabajo remunerado, y que además los errores estándar que calcula lm() con 
# weights no consideran el diseño muestral de la ENE; por lo que el análisis 
# realizado presenta limitaciones claras.

# =============================================================================
# Parte C: Comunicar
# =============================================================================

# Tabla comparativa con modelsummary() para los modelos de la Parte B
modelsummary(list("Base" = b1, "Con controles" = b2, "Interacción" = b3_int),
             stars   = TRUE,
             gof_map = c("nobs", "r.squared"))

# Exportación de los dos archivos CSV 
modelsummary(modelos_ene, output = "data/processed/t3_tabla_ene.csv")
tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)



