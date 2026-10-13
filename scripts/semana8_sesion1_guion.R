# =============================================================================
# GUION DE CLASE — Semana 8 · Sesión 1: Regresión Lineal Simple
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
#
# Nombre: Camila Bravo Figueroa      Fecha: 13-10-2026
#
# ⚠️ Esto NO es una clase de econometría: es una clase de R. Si aún no cursas
#    Econometría, basta con la idea de las primeras slides:
#      ingreso = b0 + b1 x educ + error
#    b1 (la pendiente) dice cuántos pesos más se asocian a cada año de educación.
#    La regresión es la recta que mejor resume la nube de puntos.
#
# CÓMO USAR: corre cada línea con Cmd/Ctrl+Enter.
#   🔵 CORRE Y OBSERVA · ✏️ COMPLETA (____) · 🔮 m ml,. lm PREDICE · 🟢 TU TURNO · ✅ Deberías ver
# =============================================================================

library(dplyr)

# Cargamos CASEN y creamos experiencia potencial (Mincer): edad - educ - 6
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

# -----------------------------------------------------------------------------
# BLOQUE A — La fórmula lm(Y ~ X)
# -----------------------------------------------------------------------------
# En R los modelos se escriben con una FÓRMULA:  Y ~ X  ("Y explicada por X").

# 🔮 PREDICE: el coeficiente de educación sobre ingreso, ¿será positivo o negativo?

# ✏️ COMPLETA: el ingreso explicado por la educación. Lo que se quiere explicar
#    (Y) va a la IZQUIERDA del ~; lo que explica (X), a la derecha.
modelo <- lm(ingreso ~ educ, data = casen)
modelo                 # imprime solo los coeficientes

# ✅ Deberías ver: (Intercept) 120494  educ 42080  -> b0 y b1

# 💡 El modelo es un OBJETO (una lista, Semana 4): se interroga con funciones.
# ✏️ COMPLETA con la función que corresponde: fitted() o residuals().
coef(modelo)               # los coeficientes estimados: b0 y b1
head(fitted(modelo), 3)      # el ingreso que PREDICE la recta para cada persona
head(residuals(modelo), 3)      # residuos = observado − predicho

# ✅ Deberías ver: fitted 625452.5 709612.2 415053.1 | residuos -105452 -242612 -52053
#
# 🔮 PREDICE: la persona 1 tiene 12 años de educación y gana $520.000.
#    Calcula A MANO el ingreso que le predice la recta. ¿Coincide con fitted()?
coef(modelo)[1] + coef(modelo)[2] * 12

# ✅ Deberías ver: 625452.5. Y su residuo: 520000 - 625452.5 = -105452.5
#    (gana $105.452 MENOS de lo que predice la recta para su educación).
#
# ⚠️ Fíjate en los nombres de fitted(): 1, 2, 4. Falta la 3 porque su ingreso es
#    NA y lm() la descartó, como use = "complete.obs" en la Semana 7. El modelo
#    usa 55 personas, no 60.


# -----------------------------------------------------------------------------
# BLOQUE B — El output de summary()
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA: el reporte completo
summary(modelo)

# 💡 CÓMO LEERLO, pieza por pieza:
#    Estimate        el coeficiente (b0 o b1)                     educ: 42080
#    Std. Error      cuánto podría variar con otra muestra        6289
#    t value         Estimate / Std. Error                         6.69
#    Pr(>|t|)        p-value: prob. de ver esto si b1 fuera 0      1.42e-08
#    ***             atajo visual del p-value (*** < 0,001)
#    Residual standard error   error típico de la predicción     156000 pesos
#    (5 observations deleted)  filas descartadas por NA
#    Multiple R-squared        fracción de la variación explicada  0.458
#    Adjusted R-squared        R² corregido por n.º de variables   0.448 (Sesión 2)

# ✏️ COMPLETA: extrae solo el coeficiente de educación, por su NOMBRE.
coef(modelo)["educ"]

# ✅ Deberías ver: 42079.87

# ✏️ COMPLETA: extrae el R² del modelo (pista: summary() es una LISTA, Semana 4).
summary(modelo)$r.squared

# ✅ Deberías ver: 0.4579 -> la educación sola explica el 46 % de por qué los
#    ingresos difieren entre personas.

# 🔮 PREDICE: en la Semana 7 la correlación educ-ingreso fue 0,677. Elévala al
#    cuadrado. ¿Qué número obtienes? 0,458329
#    Pista: ingreso tiene NA; el argumento de cor() que los descarta es de la
#    Semana 7.
cor(casen$educ, casen$ingreso, use = "complete.obs")^2

# ✅ Deberías ver: 0.4579. Con UNA variable explicativa, R² = correlación².
#
# ⚠️ 46 % es alto porque esta base es didáctica. En la CASEN real una ecuación
#    de Mincer suele explicar entre 20 y 35 %. Un R² bajo es normal en datos
#    sociales y no invalida el coeficiente.

# 💡 En la Sesión 2, broom::tidy() convierte todo esto en una tabla ordenada.


# -----------------------------------------------------------------------------
# BLOQUE C — Interpretación
# -----------------------------------------------------------------------------
# 🔵 CORRE Y OBSERVA
round(coef(modelo))     # intercepto y pendiente, redondeados

# ✅ Deberías ver: (Intercept) ~120494   educ ~42080
#  - Pendiente (educ): cada año más de educación SE ASOCIA con ~$42.080 más de
#    ingreso, en promedio.  ← el número de interés.
#  - Intercepto: ingreso predicho con educ = 0 (extrapolación: en la muestra
#    nadie tiene menos de 6 años de educación).

# 🟢 TU TURNO: ¿cuánto más predice la recta para alguien con 16 años de
#    educación que para alguien con 12? Usa solo la pendiente.
4 * coef(modelo)["educ"]

# ✅ Deberías ver: 168319.5 -> cuatro años más, cuatro pendientes más
#    (la recta predice 625.453 con 12 años y 793.772 con 16).

# ✏️ COMPLETA: la tabla de coeficientes con sus p-values (¿el efecto es
#    distinto de cero?). Es otro elemento de la lista summary(): mira names().
names(summary(modelo))
summary(modelo)$coefficients             # una MATRIZ: filas = coeficientes; columnas =
                                         # Estimate, Std. Error, t value, Pr(>|t|)

# ✅ Deberías ver: p-value de educ 1.42e-08 (muy lejos de 0,05) y del
#    intercepto 0.151 (no significativo, y no importa: no es el número de interés).

# ⚠️ Significancia estadística ≠ importancia económica. Mira SIEMPRE ambas.

# 🟢 TU TURNO: completa la frase de interpretación (plantilla T3):
#   "El coeficiente de educ es 42079.87; cada año adicional de educación se asocia
#    con $42.080 más de ingreso mensual, en promedio."


# -----------------------------------------------------------------------------
# BLOQUE D — Ejemplo completo + forma log (Mincer)
# -----------------------------------------------------------------------------
# ✏️ COMPLETA: el ejemplo completo, de la estimación a los dos números clave.
m1 <- lm(ingreso ~ educ, data = casen)
round(coef(m1)["educ"])             # pesos por año de educación
round(summary(m1)$r.squared, 3)          # cuánto explica el modelo

# ✅ Deberías ver: 42080 y 0.458

# ✏️ COMPLETA: forma log. El ingreso entra en LOGARITMO -> el coeficiente se
#    lee como % (Mincer).
m_log <- lm(log(ingreso) ~ educ, data = casen)
round(coef(m_log)["educ"], 3)       # ≈ retorno porcentual por año

# ✅ Deberías ver: 0.073  ->  cada año adicional se asocia con un ingreso ≈ 7,3 %
#    mayor. (Exacto: 100 x (exp(0.073) - 1) = 7,6 %; con coeficientes chicos
#    ambas lecturas casi coinciden.)

# 🟢 TU TURNO: estima `ingreso ~ experiencia`. ¿El R² es mayor o menor que el de
#    educación? ¿Qué te dice eso sobre cuál variable explica más el ingreso?
m_exp <- lm(ingreso ~ experiencia, data = casen)
round(summary(m_exp)$r.squared, 3)

# ✅ Deberías ver: 0.008



# -----------------------------------------------------------------------------
# CIERRE — lm(Y~X, data=) estima · summary() reporta · coef()/$r.squared extraen
#          pendiente = "se asocia con $X más, en promedio" · R² = % de variación.
# Puente Sesión 2: el mundo real tiene MUCHAS variables -> regresión múltiple.
# =============================================================================
