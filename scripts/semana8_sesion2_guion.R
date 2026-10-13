# =============================================================================
# GUION DE CLASE — Semana 8 · Sesión 2: Múltiples Variables y Diagnósticos
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Nombre: Camila Bravo Figueroa      Fecha: 13-10-2026
#
# CÓMO USAR: corre cada línea con Cmd/Ctrl+Enter.
#   🔵 CORRE Y OBSERVA · ✏️ COMPLETA (____) · 🔮 PREDICE · 🟢 TU TURNO · ✅ Deberías ver
# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)

casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))
m1 <- lm(ingreso ~ educ, data = casen)   # el modelo simple de la Sesión 1


# -----------------------------------------------------------------------------
# BLOQUE A — Regresión múltiple
# -----------------------------------------------------------------------------
# La fórmula crece con  + . Cada coeficiente se interpreta "ceteris paribus":
# el cambio asociado a esa variable, MANTENIENDO CONSTANTES las demás.

# ✏️ COMPLETA: agrega `genero` al modelo (educ + experiencia + genero).
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)
round(coef(m2))

# ✅ Deberías ver: educ 42246 · experiencia -517 · generoM 55686

# 💡 `genero` es texto -> R crea sola la DUMMY. `generoM` = diferencia de los
#    hombres respecto a la categoría base (F), a igual educación y experiencia.

# 🔮 PREDICE: al agregar controles, ¿el coeficiente de educ SUBE, BAJA o se
#    queda casi igual respecto al modelo simple? Compáralos:
coef(m1)["educ"]    # modelo simple
coef(m2)["educ"]    # con controles

# ⚠️ Controlar intenta AISLAR una relación, pero NO garantiza causalidad.


# -----------------------------------------------------------------------------
# BLOQUE B — Extraer resultados con broom + dplyr
# -----------------------------------------------------------------------------
# summary() sirve para LEER, no para USAR. broom lo convierte en TABLA
# (un data frame), y a una tabla se le aplica todo lo que sabes de dplyr.

# ✏️ COMPLETA: coeficientes como tabla, PIDIENDO el intervalo de confianza al 95 %
tidy(m2, conf.int = TRUE)

# 💡 Si el intervalo [conf.low, conf.high] NO incluye el 0, el coeficiente es
#    significativo al 5 %. Mira educ y generoM: ¿cuál incluye el 0?

# ✏️ COMPLETA: arma la "tabla que importa":
#    sin intercepto, redondeada, con una columna lógica de significancia,
#    ordenada de lo más claro (p-value más chico) a lo menos claro.
tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>                       # quitar filas
  mutate(across(c(estimate, conf.low, conf.high), round),
         significativo = p.value < 0.05) |>            # umbral del 5 %
  select(term, estimate, conf.low, conf.high, p.value, significativo) |>
  arrange(p.value)                                        # ordenar por p-value

# ✅ Deberías ver: 3 filas; educ arriba con significativo = TRUE,
#    generoM y experiencia con FALSE.

# ✏️ COMPLETA: del glance() saca r.squared, adj.r.squared, el error típico
#    de la predicción (en pesos), AIC y nobs.
glance(m2) |> select(r.squared, adj.r.squared, sigma, AIC, nobs)

# ✅ Deberías ver: 0.475 · 0.444 · 156502 · 1478 · 55
#    (55 y no 60: hay 5 NA en ingreso que lm() descarta).

# 💡 tidy() = los COEFICIENTES (varias filas). glance() = el MODELO (una fila).


# -----------------------------------------------------------------------------
# BLOQUE C — Muchos modelos con dplyr
# -----------------------------------------------------------------------------
# lm() no recibe los datos como PRIMER argumento, así que el pipe no los pasa
# solo. El placeholder  _  indica dónde van:  data = _

# ✏️ COMPLETA: crea la variable y estima en un solo pipe.
m3 <- casen |>
  mutate(superior = educ >= 13) |>
  lm(ingreso ~ superior + experiencia + genero, data = _)
tidy(m3) |> select(term, estimate, p.value) |> mutate(estimate = round(estimate))

# ✅ Deberías ver: superiorTRUE 239879. Una variable lógica entra como dummy.

# 🔵 CORRE Y OBSERVA: tres modelos guardados en una LISTA con nombre
#    (antes, completa el hueco del tercer modelo)
modelos <- list(
  simple    = lm(ingreso ~ educ, data = casen),
  controles = lm(ingreso ~ educ + experiencia + genero, data = casen),
  mincer    = lm(ingreso ~ educ + experiencia + I(experiencia^2) + genero, data = casen)
)
# ✏️ COMPLETA (arriba): experiencia al cuadrado (ecuación de Mincer). Dentro de
#    una fórmula, ^ tiene otro significado: hay que "protegerlo" con una función
#    de una sola letra.

# ✏️ COMPLETA: aplica glance() a cada modelo y apílalos con una columna
#    que diga de qué modelo viene cada fila.
lapply(modelos, glance) |>
  bind_rows(.id= "modelo") |>
  select(modelo, r.squared, adj.r.squared, AIC, nobs)

# 🔮 PREDICE: el R² sube de un modelo al siguiente. ¿Y el R² AJUSTADO?
#    ¿Qué te dice eso sobre los controles que agregamos?

# ✏️ COMPLETA: ¿cambió el coeficiente de educ entre los tres modelos?
bind_rows(simple    = tidy(modelos$simple),
          controles = tidy(modelos$controles),
          mincer    = tidy(modelos$mincer), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

# Un control mueve un coeficiente solo si se relaciona A LA VEZ con la
# educación y con el ingreso. ¿Se relaciona la experiencia con la educación?
cor(casen$educ, casen$experiencia)

# ✅ Deberías ver: 42080 · 42246 · 42690, y una correlación de -0.156.

# ✏️ COMPLETA: una regresión POR GRUPO (misma idea que correlacion_por_grupo()
#    de la Semana 7). Dentro de group_modify(), los datos de cada grupo se
#    llaman .x
casen |>
  group_by(genero) |>
  group_modify(~ tidy(lm(ingreso ~ educ, data = .x))) |>
  filter(term == "educ") |>
  select(genero, estimate, std.error, p.value)

# ✅ Deberías ver: F 46862 y M 38295, con errores estándar de ~9.000:
#    la diferencia está dentro del ruido.

# 🟢 TU TURNO: repite la regresión por grupo, pero por SECTOR.
#    ¿En qué sector la educación NO es significativa? Mira table(casen$sector):
#    ¿cuántas personas tiene ese sector? ¿Qué explica el resultado?

casen |>
  group_by(sector) |>
  group_modify(~ tidy(lm(ingreso ~ educ, data = .x))) |>
  filter(term == "educ") |>
  select(sector, estimate, std.error, p.value)


# -----------------------------------------------------------------------------
# BLOQUE D — Diagnósticos
# -----------------------------------------------------------------------------
# ✏️ COMPLETA: augment() = una fila por persona, con lo predicho y el residuo.
#    Ordena por el TAMAÑO del residuo (sin importar el signo) para encontrar a
#    los peor predichos.
augment(m2) |>
  select(ingreso, educ, genero, .fitted, .resid) |>
  mutate(across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  head(3)

# ✅ Deberías ver: tres hombres con residuos de -298210, -296393 y -249293.

# 💡 augment() trae además .hat (apalancamiento) y .cooksd (distancia de
#    Cook): cuánto pesa cada persona en el ajuste. En el laboratorio las usas
#    para ver si el resultado depende de una sola persona.

# Predecir PERFILES concretos (deben tener las mismas columnas que el modelo):
perfiles <- tibble(educ        = c(12, 16, 16),
                   experiencia = 10,
                   genero      = c("F", "F", "M"))
# ✏️ COMPLETA: ¿qué datos nuevos le pasas a predict()?
perfiles |> mutate(ingreso_predicho = round(predict(m2, newdata = perfiles)))

# ✅ Deberías ver: 604810 · 773793 · 829479
#    Entre la fila 1 y la 2 solo cambia educ (4 × 42.246); entre la 2 y la 3,
#    solo el género (55.686).

# 🟢 TU TURNO: predice el ingreso de un HOMBRE con 12 años de educación y 20 de
#    experiencia. ¿Cuánto más/menos que la mujer con 16 años de educación y
#    10 de experiencia? ¿Qué coeficiente explica la mayor parte de la diferencia?

comparar <- tibble(educ        = c(12, 16),
                   experiencia = c(20, 10),
                   genero      = c("M", "F"))

comparar <- comparar |>
  mutate(ingreso_predicho = round(predict(m2, newdata = comparar)))
comparar

# Diferencia total: hombre - mujer
comparar$ingreso_predicho[1] - comparar$ingreso_predicho[2]

# Descomposición: cuánto cambió cada variable x su coeficiente
efecto_educ <- coef(m2)["educ"]        * (12 - 16)   # hombre tiene 4 años menos de educación
efecto_exp  <- coef(m2)["experiencia"] * (20 - 10)   # hombre tiene 10 años más de experiencia
efecto_gen  <- coef(m2)["generoM"]     * (1 - 0)     # hombre = 1, categoría base (F) = 0

round(c(educ = efecto_educ, experiencia = efecto_exp, genero = efecto_gen))

# Comprobación: las tres piezas deben sumar la diferencia total
round(efecto_educ + efecto_exp + efecto_gen)

# Un hombre con 12 años de educación y 20 de experiencia gana, en promedio, 
# $118.463 menos que una mujer con 16 años de educación y 10 años de experiencia.
# La mayor parte de esa diferencia se asocia con el coeficiente de educación, el 
# cual indica que cada año adicional de educación se asocia con $42.246 más de 
# ingreso mensual promedio.

# -----------------------------------------------------------------------------
# BLOQUE E — Comunicar: tablas profesionales
# -----------------------------------------------------------------------------
# ✏️ COMPLETA: los dos modelos lado a lado, con estrellas de significancia
#    (esto pide la T3)
modelsummary(list("Simple" = m1, "Múltiple" = m2),
             stars = TRUE,
             gof_map = c("nobs", "r.squared"))

# Exportar (para la entrega). No lo corras si no quieres generar archivos:
modelsummary(modelos, output = "output/tabla_regresion.csv")
tidy(m2, conf.int = TRUE) |> write.csv("output/coeficientes.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# CIERRE — Estimar lm(Y ~ X1 + X2) · Interpretar (ceteris paribus) · Extraer
#          (tidy/glance/augment + dplyr) · Comparar (lapply + bind_rows) ·
#          Por grupo (group_modify) · Diagnosticar augment() · Comunicar modelsummary().
# Al laboratorio: labs/semana8_lab_esqueleto.R · T3 (14 %): tareas/tarea_semana8.md
# =============================================================================
