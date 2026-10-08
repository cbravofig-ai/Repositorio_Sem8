# =============================================================================
# Laboratorio Semana 8 — Regresión: lo que la clase dejó abierto
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Autor: Camila Bravo Figueroa
# Fecha: 08-10-2026
#
# Objetivo: llevar la regresión más allá de lo visto en clases.
#   PARTE 1 (CASEN): cuatro preguntas que la clase dejó abiertas.
#     ¿Qué pasa si pongo dos variables que dicen lo mismo? ¿La diferencia de
#     retornos entre hombres y mujeres es real? ¿El resultado depende de una
#     sola persona? ¿Cuánto es la brecha en PORCENTAJE?
#   PARTE 2 (ENE del INE, 41.850 ocupados): regresión con datos reales.
#     La U de informalidad por edad de la Semana 7, ahora con controles,
#     ponderada, por región, y el efecto de un centinela sobre un coeficiente.
#
# Prepara la T3 (calificada, 14 %): ver tareas/tarea_semana8.md.
#
# CÓMO USARLO: donde veas ____ tienes que escribir tú. Corre cada bloque con
#              Cmd/Ctrl + Enter y compara con el "✅ Deberías ver".
#
# ⚠️ Ábrelo desde el .Rproj, no con RStudio a secas, o la ruta no funcionará.
#
# Regla IA: ChatGPT es CONSULTOR, no escritor. Debes poder explicar cada línea.
#           La INTERPRETACIÓN de los coeficientes es tuya.
# =============================================================================

library(dplyr)
library(broom)
library(modelsummary)

casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)   # el de la Sesión 2


# =============================================================================
# PARTE 1 — CASEN: cuatro preguntas abiertas
# =============================================================================

# -----------------------------------------------------------------------------
# PASO 1 — Dos variables que dicen lo mismo
# -----------------------------------------------------------------------------
# En la Semana 7 vimos que edad y experiencia correlacionan 0,98, y se advirtió:
# "en la Semana 8 esto importa: ponerlas juntas en una regresión trae
# problemas". Veamos cuál.

# TODO: agrega experiencia a un modelo que ya tiene educ y edad.
m_col <- lm(ingreso ~ educ + edad + experiencia, data = casen)
tidy(m_col)

# ✅ Deberías ver: la fila de experiencia con estimate NA (y todo NA).
#
# TODO: comprueba por qué. Si experiencia = edad - educ - 6 en TODAS las filas,
#       esta diferencia debería ser 0 siempre.
casen |>
  mutate(chequeo = edad - educ - 6 - experiencia) |>
  summarise(maxima_diferencia = max(abs(chequeo)))

# ✅ Deberías ver: 0
#
# 💡 Colinealidad perfecta: experiencia es una combinación EXACTA de edad y
#    educ (ninguna persona tiene edad - educ - 6 negativa, así que pmax() nunca
#    actuó). R no puede separar el efecto de algo que no aporta información
#    nueva, y no da error: pone NA y sigue. Stata hace lo mismo y avisa
#    "omitted because of collinearity" (Módulo IV).
#
# ¿Por qué el modelo de Mincer usa experiencia y NO edad junto a educ? por esa colinealidad.
# ____________________________________________________________________________


# -----------------------------------------------------------------------------
# PASO 2 — ¿El retorno a la educación es distinto para hombres y mujeres?
# -----------------------------------------------------------------------------
# En la Sesión 2, una regresión por grupo dio F 46.862 y M 38.295, y se dijo
# que la diferencia "está dentro del ruido". Aquí la ponemos a prueba.

# TODO: primero, la diferencia que vimos en clase, calculada con dplyr.
casen |>
  group_by(genero) |>
  group_modify(~ tidy(lm(ingreso ~ educ, data = .x))) |>
  filter(term == "educ") |>
  ungroup() |>
  summarise(diferencia_M_menos_F =estimate[genero == "M"] - estimate[genero == "F"])

# ✅ Deberías ver: -8567 (los hombres, 8.567 pesos menos por año de educación)

# TODO: ahora en UN solo modelo. El operador * crea una INTERACCIÓN: deja que
#       la pendiente de educ sea distinta según el género.
m_int <- lm(ingreso ~ educ * genero, data = casen)
tidy(m_int) |>
  filter(term == "educ:generoM") |>          # el término de la interacción
  select(term, estimate, std.error, p.value)

# ✅ Deberías ver: educ:generoM -8567, std.error 12597, p.value 0.500
#
# 💡 Es EXACTAMENTE la diferencia anterior: el modelo con interacción es las
#    dos regresiones por grupo escritas como una sola. La ventaja es que ahora
#    la diferencia tiene su propio error estándar y su propio p-value.
#
# ¿Se puede afirmar que el retorno a la educación difiere por género? ¿Por qué?
# ____________________________________________________________________________


# -----------------------------------------------------------------------------
# PASO 3 — ¿El resultado depende de una sola persona?
# -----------------------------------------------------------------------------
# En clase, augment() trajo la columna .cooksd (distancia de Cook): cuánto
# pesa cada persona en el ajuste. Ninguna pesa demasiado (máximo 0,087). Pero
# "no domina" no es lo mismo que "no influye". Lo comprobamos re-estimando.

# TODO: las tres observaciones más influyentes (mayor distancia de Cook).
augment(m2) |>
  slice_max(.cooksd, n = 3) |>
  select(.rownames, ingreso, educ, genero, .cooksd)

# ✅ Deberías ver: filas 43, 27 y 8; la 43 con .cooksd 0.0869
#    (.rownames es el número de fila ORIGINAL en casen; augment() lo agrega
#     porque lm() descartó las filas con NA y la numeración ya no es 1, 2, 3...)

# TODO: re-estima m2 SIN la fila más influyente, en un solo pipe (Sesión 2).
m2_sin <- casen |>
#  filter(!(row_number() %in% c(43,27,8))) |>
  filter(row_number() != 43) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

# TODO: compara el coeficiente de educ con y sin esa persona.
bind_rows(todos = tidy(m2), sin_43 = tidy(m2_sin), .id = "muestra") |>
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)

# ✅ Deberías ver: todos 42246 · sin_43 45218 (sube 7 %, sigue significativo)
#
# Una sola persona de 55 mueve el coeficiente en 3.000 pesos. ¿Cambia la
# CONCLUSIÓN económica? ¿Qué te dice esto sobre trabajar con n = 55? ________
# ____________________________________________________________________________


# -----------------------------------------------------------------------------
# PASO 4 — De pesos a porcentajes
# -----------------------------------------------------------------------------
# En la Sesión 1 vimos log(ingreso) ~ educ (≈ 7,3 % por año). Ahora el modelo
# múltiple en log: cada coeficiente se lee como cambio PORCENTUAL.
# Lectura exacta: 100 × (exp(b) − 1).

ml <- lm(log(ingreso) ~ educ + experiencia + genero, data = casen)

# TODO: tabla con el efecto en % de cada variable.
tidy(ml) |>
  filter(term != "(Intercept)") |>
  mutate(efecto_pct = round(100 * (exp(estimate) - 1), 1),
         p.value    = round(p.value, 3)) |>
  select(term, efecto_pct, p.value)

# ✅ Deberías ver: educ 7.6 · experiencia -0.2 · generoM 12.5 (p.value 0.144)
#
# ¿Cómo se lee generoM = 12,5 %? ¿Cambia la conclusión respecto al modelo en
# pesos (55.686, p = 0.204)? ____________________________________________________


# =============================================================================
# PARTE 2 — ENE: regresión con datos reales
# =============================================================================
# ⏱ La lectura tarda 2-3 segundos.

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

# -----------------------------------------------------------------------------
# PASO 5 — Preparar la base de ocupados
# -----------------------------------------------------------------------------
# Códigos (manual codigos-ene-2020.pdf):
#   ocup_form            1 = formal, 2 = informal (solo ocupados)
#   sexo                 1 = hombre, 2 = mujer
#   categoria_ocupacion  1 empleador · 2 cuenta propia · 3 asalariado privado ·
#                        4 asalariado público · 5 y 6 servicio doméstico
#                        (puertas afuera / adentro) · 7 familiar no remunerado

# TODO: completa la preparación.
ocupados <- ene |>
  filter(activ == 1) |>                        # solo ocupados
  mutate(informal  = ocup_form == 2,              # LÓGICA: TRUE / FALSE
         mujer     = sexo == 2,
         categoria = case_when(
           categoria_ocupacion == 1      ~ "Empleador",
           categoria_ocupacion == 2      ~ "Cuenta propia",
           categoria_ocupacion == 3      ~ "Asalariado privado",
           categoria_ocupacion == 4      ~ "Asalariado público",
           categoria_ocupacion %in% 5:6  ~ "Servicio doméstico",
           categoria_ocupacion == 7      ~ "Familiar no remunerado"),
         region_txt = regiones[region]) |>
  select(informal, mujer, edad, categoria, region_txt, habituales, fact_cal)

nrow(ocupados)
ocupados |> summarise(sin_ponderar = mean(informal),
                      ponderada    = weighted.mean(informal, fact_cal))

# ✅ Deberías ver: 41850 filas · informalidad 0.299 sin ponderar, 0.265 ponderada
#
# 💡 MODELO DE PROBABILIDAD LINEAL. Si la variable dependiente es TRUE/FALSE,
#    lm() la trata como 1/0, y su promedio es una PROPORCIÓN. Cada coeficiente
#    se lee como el cambio en la PROBABILIDAD de ser informal, en puntos
#    porcentuales (pp): un coeficiente 0,05 = 5 pp más.


# -----------------------------------------------------------------------------
# PASO 6 — La U de la Semana 7, ahora con regresión
# -----------------------------------------------------------------------------
# En la Semana 7, una tabla mostró que la informalidad forma una U con la edad.
# Una U se modela con edad y edad al cuadrado, como la experiencia en Mincer.

# TODO: completa el modelo.
e1 <- lm(informal ~ edad + I(edad^2) + mujer, data = ocupados)
tidy(e1)

# ✅ Deberías ver: edad -0.0282 · I(edad^2) 0.000350 · mujerTRUE 0.0436
#
# mujerTRUE = 0,0436: a igual edad, ser mujer se asocia con una probabilidad
# de ser informal 4,4 pp mayor.

# TODO: ¿a qué edad la informalidad es MÍNIMA? En una parábola b1·x + b2·x²,
#       el mínimo está en x = -b1 / (2·b2). Saca los coeficientes con pull().

b1 <- tidy(e1) |> filter(term == "edad")      |> pull(estimate)
b2 <- tidy(e1) |> filter(term == "I(edad^2)")      |> pull(estimate)

-b1 / (2 * b2)

# ✅ Deberías ver: 40.2 -> la informalidad baja hasta los 40 años y luego sube.

glance(e1) |> select(r.squared, nobs)

# ✅ Deberías ver: r.squared 0.0455 · nobs 41850
#
# ⚠️ Todos los p-values son minúsculos y el R² es 0,05. No se contradicen: con
#    41.850 personas, cualquier diferencia real es significativa; el R² dice
#    que la edad y el sexo explican una parte chica de quién es informal.
#    Significativo ≠ importante ≠ suficiente.


# -----------------------------------------------------------------------------
# PASO 7 — Ponderar
# -----------------------------------------------------------------------------
# En la Semana 6 la tasa de desocupación cambió al ponderar con fact_cal.
# lm() acepta los pesos con el argumento weights.

# TODO: el mismo modelo, ponderado.
e1w <- lm(informal ~ edad + I(edad^2) + mujer, data = ocupados,
          weights = fact_cal)
tidy(e1w)

# TODO: compara los coeficientes con y sin ponderar, lado a lado.
bind_rows(sin_ponderar = tidy(e1), ponderado = tidy(e1w), .id = "modelo") |>
  filter(term != "(Intercept)") |>
  select(modelo, term, estimate) |>
  arrange(term, modelo)                  # que queden juntas las filas de cada término

# ✅ Deberías ver: mujerTRUE 0.0436 sin ponderar -> 0.0410 ponderado.
#    Los coeficientes se mueven poco: el patrón no es un artefacto de la muestra.
#
# ⚠️ Con weights, los COEFICIENTES son los poblacionales, pero los errores
#    estándar de lm() no consideran el diseño muestral de la ENE (estratos,
#    conglomerados). Para errores estándar correctos se usa el paquete survey,
#    fuera del alcance del curso. Aquí, mira los coeficientes.


# -----------------------------------------------------------------------------
# PASO 8 — ¿Qué hay detrás de la brecha de género?
# -----------------------------------------------------------------------------
# La Semana 7 cerró con una advertencia: la informalidad "puede deberse a ...
# el sector. La tabla describe; no explica." Agreguemos el TIPO de empleo.

# TODO: agrega categoria al modelo ponderado.
e2 <- lm(informal ~ edad + I(edad^2) + mujer + categoria, data = ocupados,
         weights = fact_cal)

tidy(e2) |>
  filter(term != "(Intercept)") |>
  mutate(pp = round(100 * estimate, 1)) |>          # a puntos porcentuales
  select(term, pp)

# ✅ Deberías ver (categoría base: Asalariado privado, la primera en orden
#    alfabético): Cuenta propia +45.8 pp · Familiar no remunerado +82.2 ·
#    Servicio doméstico +38.5 · mujerTRUE +1.8
#    (I(edad^2) sale 0 por el redondeo: es un número muy chico, no cero.)

# TODO: compara los dos modelos con lapply() + glance() (Sesión 2).
modelos_ene <- list(sin_categoria = e1w, con_categoria = e2)
lapply(modelos_ene, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, nobs)

# TODO: y el coeficiente de mujer en ambos.
bind_rows(sin_categoria = tidy(e1w), con_categoria = tidy(e2), .id = "modelo") |>
  filter(term == "mujerTRUE") |>
  mutate(pp = round(100 * estimate, 1)) |>
  select(modelo, pp)

# ✅ Deberías ver: R² 0.039 -> 0.242 · mujer 4.1 pp -> 1.8 pp
#
# 💡 La brecha de género cae a menos de la mitad. Entre un hombre y una mujer
#    con el MISMO tipo de empleo, la diferencia en informalidad es chica: gran
#    parte de la brecha se asocia a que las mujeres están más en cuenta propia
#    y servicio doméstico.
#
# ⚠️ Esto NO dice que la brecha "no existe". Controlar por categoría cambia la
#    PREGUNTA: de "¿hombres vs mujeres?" a "¿hombres vs mujeres en el mismo
#    tipo de empleo?". Por qué las mujeres terminan en esos empleos es otra
#    pregunta, que esta regresión no responde.
#
# ¿Cuál de las dos cifras (4,1 o 1,8 pp) reportarías en un informe sobre la
# brecha de género en informalidad? ¿Por qué? ____________________________________


# -----------------------------------------------------------------------------
# PASO 9 — Una regresión por región
# -----------------------------------------------------------------------------
# ¿La brecha de género (sin controlar por categoría) es igual en todo Chile?

# TODO: estima e1w en cada región y quédate con el coeficiente de mujer,
#       con su intervalo de confianza.
brecha_region <- ocupados |>
  group_by(region_txt) |>
  group_modify(~ tidy(lm(informal ~ mujer + edad + I(edad^2),
                         data = .x, weights = fact_cal),
                      conf.int = TRUE)) |>
  filter(term == "mujerTRUE") |>
  ungroup() |>
  mutate(across(c(estimate, conf.low, conf.high), ~ round(100 * .x, 1)),
         distinta_de_cero = conf.low > 0) |>
  select(region_txt, estimate, conf.low, conf.high, distinta_de_cero) |>
  arrange(desc(estimate))

brecha_region

# ✅ Deberías ver: Atacama arriba (12.4 pp) y La Araucanía abajo (-0.7 pp).
#    Ñuble: 1.0 pp, con un intervalo que incluye el cero.

# TODO: ¿en cuántas regiones la brecha es distinguible de cero?
brecha_region |> count(___)

# ✅ Deberías ver: FALSE 6 · TRUE 10
#
# ¿Qué tienen en común las regiones con brecha mayor? ¿Y por qué Aysén
# (n chico) tiene un intervalo tan ancho? ______________________________________


# -----------------------------------------------------------------------------
# PASO 10 — Un 888 dentro de una regresión
# -----------------------------------------------------------------------------
# En la Semana 6, los centinelas 888 y 999 de habituales movían el PROMEDIO de
# horas. ¿Qué le hacen a un COEFICIENTE?

h_sucia  <- lm(habituales ~ mujer + edad, data = ocupados)

# TODO: el mismo modelo con las horas limpias (los dos centinelas -> NA).
h_limpia <- ocupados |>
  mutate(habituales = na_if(na_if(habituales, ____), ____)) |>
  lm(habituales ~ mujer + edad, data = _)

bind_rows(sucia = tidy(h_sucia), limpia = tidy(h_limpia), .id = "base") |>
  filter(term != "(Intercept)") |>
  select(base, term, estimate, std.error) |>
  arrange(term, base)

# ✅ Deberías ver:
#    edad       sucia -0.0540 (se 0.0159)  ->  limpia -0.0343 (se 0.00468)
#    mujerTRUE  sucia -4.84   (se 0.454)   ->  limpia -4.29   (se 0.133)
#
# 💡 Son 111 personas de 41.850 (0,27 %). Ningún error, ningún aviso: el
#    coeficiente de edad cambia en un 57 % y los errores estándar se triplican.
#    Un 888 es, para lm(), una persona que trabaja 888 horas a la semana.
#
# ¿Qué habrías reportado sobre la edad y las horas si no limpiabas? ____________


# -----------------------------------------------------------------------------
# PASO 11 — La tabla del informe
# -----------------------------------------------------------------------------
# TODO: los tres modelos de informalidad lado a lado.
modelsummary(list("Sin ponderar" = e1, "Ponderado" = e1w, "Con categoría" = e2),
             stars   = TRUE,
             gof_map = c("nobs", "r.squared"))

# TODO: exporta los coeficientes del modelo con categoría.
dir.create("data/processed", showWarnings = FALSE)
tidy(e2, conf.int = TRUE) |>
  write.csv("data/processed/lab8_informalidad.csv", row.names = ____)


# -----------------------------------------------------------------------------
# INTERPRETACIÓN (5 a 8 líneas)
# -----------------------------------------------------------------------------
# Responde, con las cifras de la PARTE 2: ¿qué se asocia con la probabilidad
# de ser informal en Chile (mayo-julio 2026)?
#   1. Di "se asocia", nunca "causa".
#   2. Reporta magnitudes en puntos porcentuales, no solo signos o p-values.
#   3. Nombra UNA limitación (variables omitidas, errores estándar sin diseño
#      muestral, modelo de probabilidad lineal, etc.).
#
# INTERPRETACIÓN:
# ____________________________________________________________________________
# ____________________________________________________________________________
# ____________________________________________________________________________


# -----------------------------------------------------------------------------
# AUTOEVALUACIÓN antes de entregar
# -----------------------------------------------------------------------------
# [ ] El script corre completo de una vez (reinicia R y córrelo entero).
# [ ] Sé por qué un coeficiente sale NA cuando hay colinealidad perfecta.
# [ ] Sé leer un término de interacción (educ:generoM) y de dónde sale.
# [ ] Re-estimé un modelo sin la observación más influyente y comparé.
# [ ] Sé leer un coeficiente en un modelo con log(Y) como porcentaje.
# [ ] Sé leer un coeficiente de un modelo de probabilidad lineal en pp.
# [ ] Calculé la edad mínima de la U a partir de los coeficientes.
# [ ] Usé weights = fact_cal y sé qué NO corrige.
# [ ] Sé explicar por qué cambió la brecha de género al controlar por categoría.
# [ ] Estimé una regresión por región con group_modify().
# [ ] Vi qué le hace un centinela a un coeficiente.


# -----------------------------------------------------------------------------
# ENTREGA: guarda este script y súbelo a tu repositorio de GitHub.
# Commit sugerido: "Lab S8: regresión con CASEN y ENE"
#
# Siguiente paso: la T3 (tareas/tarea_semana8.md) te pide hacer esto mismo con
# TU pregunta y TU base de la ENE limpia de la A3.
# -----------------------------------------------------------------------------
