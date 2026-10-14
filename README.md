# T3 - Regresión económica: CASEN y ENE

Tarea calificada n°3 de la Semana 8, enfocada en la estimación, extracción, 
comparación, diagnóstico e interpretación de modelos de regresión lineal en R. 
En la Parte A se estima y diagnostica la ecuación de ingresos con datos de la 
CASEN utilizando el flujo de `broom` y `dplyr`. En la Parte B se retoma el 
análisis de la brecha de género en las horas efectivas de trabajo en la Encuesta 
Nacional de Empleo (ENE), incorporando controles sociodemográficos y laborales, 
un término de interacción y un análisis de sensibilidad.

**Pregunta:** En la A3 se encontró una brecha promedio de 5 horas efectivas 
semanales menos en mujeres ocupadas respecto a los hombres. ¿Cuánto de esa brecha 
de género se mantiene al comparar personas de igual edad, nivel educacional, 
categoría ocupacional y condición de formalidad, y difiere esa brecha entre el 
empleo formal e informal?

**Respuesta:** En el modelo base ponderado, ser mujer se asocia con 5,03 horas 
efectivas menos de trabajo a la semana, y al controlar por edad, nivel educacional, 
categoría ocupacional y formalidad la brecha casi no se mueve y se asocia con 4,81 
horas semanales menos. Sin embargo, al agregar la interacción entre género e 
informalidad se ve que en un empleo formal esta diferencia se asocia con 3,83 horas 
menos, mientras que en la informalidad se suman otras 3,75 horas menos, llegando a 
una brecha de 7,58 horas semanales. Estos resultados muestran que la diferencia de 
jornada se mantiene a igualdad de características observables y se acentúa en la 
informalidad.

## Datos

Los datos utilizados en esta tarea provienen de dos fuentes principales almacenadas 
en la carpeta `data/raw/`:

- `data/raw/casen_reducido.csv`: submuestra didáctica de la encuesta CASEN con 60 
  observaciones (55 válidas en `ingreso` y 5 `NA`), utilizada en la Parte A para 
  modelar los retornos a la educación y la experiencia potencial de Mincer.
- `data/raw/ene-2026-06-mjj.csv` (junto con su manual `data/raw/codigos-ene-2020.pdf`): 
  Encuesta Nacional de Empleo del INE (trimestre mayo-julio 2026) con 97.946 
  observaciones iniciales y 222 variables. En la Parte B se retoma el archivo 
  original para incorporar los controles necesarios a los 41.850 ocupados (`activ == 1`), 
  auditando los faltantes estructurales y limpiando los códigos centinela (`888` y 
  `999` en `efectivas`, y el código `9` de «Sin clasificación» en `cine11_1d`).

## Cómo correrlo

1. Abrir el archivo `Repositorio_Sem8.Rproj`.
2. Abrir el script `scripts/tarea_t3.R`.
3. Ejecutar el script completo desde el inicio (requiere los paquetes `dplyr`, `broom` y `modelsummary`).

## Estructura

```text
Repositorio_Sem8/
├── data/
│   ├── processed/
│   │   ├── t3_coeficientes_casen.csv
│   │   └── t3_tabla_ene.csv
│   └── raw/
│       ├── casen_reducido.csv
│       ├── codigos-ene-2020.pdf
│       └── ene-2026-06-mjj.csv
├── outputs/
├── scripts/
│   ├── semana8_lab_esqueleto.R
│   ├── semana8_sesion1_guion.R
│   ├── semana8_sesion2_guion.R
│   └── tarea_t3.R
├── .gitattributes
├── .gitignore
├── README.md
└── Repositorio_Sem8.Rproj
```

**Nota**: El desarrollo de la T3 se encuentra en el script `scripts/tarea_t3.R`, 
el cual genera automáticamente los archivos `t3_coeficientes_casen.csv` y 
`t3_tabla_ene.csv` en `data/processed/`. Cualquier otro archivo dentro de `scripts/` 
corresponde a material de apoyo utilizado durante las sesiones de clase y laboratorio.

## Autor

Camila Bravo Figueroa — octubre 2026

## Declaración de autoría y uso de IA
- Herramienta utilizada: Gemini Pro
- Para qué la usé: Para la revisión de las líneas de código y para que me ayudara 
                   a estructurar los modelos de regresión e interacción de la Semana 8 
                   dándole continuidad a mi pregunta de análisis de la tarea A3.
- Qué hice yo: Definí la pregunta y los controles a utilizar, ejecuté la auditoría de 
               variables en R y escribí los códigos junto con sus respectivos comentarios 
               para entender, durante el proceso, a qué parte del análisis tributaba cada 
               línea. Además, los comentarios, interpretaciones y respuestas fueron 
               redactados con mi propio criterio y son de mi autoría.
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.

