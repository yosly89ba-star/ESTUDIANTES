
# ============================================================
# EGRESADOS UNE 2020-2025
# SCRIPT 01: DIAGNOSTICO, DEPURACION Y LIMPIEZA
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(stringi)

# Crear carpeta para guardar resultados
dir.create("salidas", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. LECTURA DE LA BASE ORIGINAL
# ------------------------------------------------------------

ruta <- "EGRESADOS-UNE-2020-2025 DIRECCIÓN DE REGISTRO.csv"

raw <- read_delim(
  ruta,
  delim = ";",
  locale = locale(encoding = "latin1"),
  col_types = cols(.default = col_character()),
  trim_ws = TRUE,
  show_col_types = FALSE
)

df <- raw |>
  clean_names() |>
  mutate(
    across(everything(), ~ na_if(str_squish(.x), "")),
    promedio_final = as.numeric(promedio_final),
    creditos_acumulados = as.integer(creditos_acumulados),
    annio_egreso = as.integer(annio_egreso),
    annio_matricula1 = as.integer(annio_matricula1),
    annio_solicitud_tramite =
      as.integer(annio_solicitud_tramite),
    fecha_egreso = mdy(
      str_extract(
        fecha_egreso,
        "^\\d{1,2}/\\d{1,2}/\\d{4}"
      )
    )
  )

# Copia original: no modificar
df_original <- df

cat("Filas originales:", nrow(df_original), "\n")
cat("Columnas originales:", ncol(df_original), "\n")

glimpse(df_original)


# ------------------------------------------------------------
# 2. DIAGNOSTICO DE VALORES FALTANTES
# ------------------------------------------------------------

faltantes <- df_original |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "faltantes"
  ) |>
  mutate(
    porcentaje = round(
      100 * faltantes / nrow(df_original), 3
    )
  ) |>
  arrange(desc(faltantes))

print(faltantes, n = Inf)

write_csv(faltantes, "salidas/01_faltantes_iniciales.csv")


# ------------------------------------------------------------
# 3. DIAGNOSTICO DE DUPLICADOS
# ------------------------------------------------------------

cat(
  "Filas completamente duplicadas:",
  sum(duplicated(df_original)), "\n"
)

duplicados_uuid <- df_original |>
  count(uuid, name = "cantidad_registros") |>
  filter(cantidad_registros > 1) |>
  arrange(desc(cantidad_registros))

cat("UUID repetidos:", nrow(duplicados_uuid), "\n")
cat(
  "Filas asociadas a UUID repetidos:",
  sum(duplicados_uuid$cantidad_registros), "\n"
)

write_csv(
  duplicados_uuid,
  "salidas/02_uuid_repetidos.csv"
)

# Normalizar texto para comparar nombres de programas
norm_txt <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

# Identificar grupos repetidos por UUID, nivel y programa
duplicados_logicos <- df_original |>
  mutate(programa_norm = norm_txt(programa_estudios)) |>
  group_by(uuid, nivel_academico, programa_norm) |>
  filter(n() > 1) |>
  ungroup() |>
  arrange(uuid, nivel_academico, programa_norm)

cat(
  "Filas en grupos lógicos repetidos:",
  nrow(duplicados_logicos), "\n"
)

write_csv(
  duplicados_logicos,
  "salidas/03_duplicados_logicos.csv"
)

# Comprobar UUID con diferentes sexos
inconsistencias_sexo <- df_original |>
  group_by(uuid) |>
  summarise(
    cantidad_sexos = n_distinct(sexo, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(cantidad_sexos > 1)

cat(
  "UUID con más de un sexo:",
  nrow(inconsistencias_sexo), "\n"
)

write_csv(
  inconsistencias_sexo,
  "salidas/04_uuid_sexo_inconsistente.csv"
)


# ------------------------------------------------------------
# 4. DIAGNOSTICO DE PROMEDIOS Y CREDITOS
# ------------------------------------------------------------

cat("\nResumen de promedios:\n")
print(summary(df_original$promedio_final))

cat(
  "Promedios iguales a cero:",
  sum(df_original$promedio_final == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Promedios faltantes:",
  sum(is.na(df_original$promedio_final)), "\n"
)

cat("\nResumen de créditos:\n")
print(summary(df_original$creditos_acumulados))

cat(
  "Créditos iguales a cero:",
  sum(df_original$creditos_acumulados == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Créditos negativos:",
  sum(df_original$creditos_acumulados < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Créditos faltantes:",
  sum(is.na(df_original$creditos_acumulados)), "\n"
)


# ------------------------------------------------------------
# 5. DIAGNOSTICO TEMPORAL
# ------------------------------------------------------------

revision_temporal <- df_original |>
  mutate(
    anio_fecha_egreso = year(fecha_egreso),
    discrepancia_egreso =
      !is.na(anio_fecha_egreso) &
      !is.na(annio_egreso) &
      anio_fecha_egreso != annio_egreso
  )

cat(
  "Año de fecha distinto del año declarado:",
  sum(revision_temporal$discrepancia_egreso, na.rm = TRUE),
  "\n"
)

cat("\nDistribución por año de egreso:\n")
print(table(df_original$annio_egreso, useNA = "ifany"))

# Fechas anteriores a 2020 para verificar la exclusión acordada
casos_fecha_anterior <- df_original |>
  filter(
    !is.na(fecha_egreso),
    year(fecha_egreso) < 2020
  )

print(casos_fecha_anterior, n = Inf)

write_csv(
  casos_fecha_anterior,
  "salidas/05_fechas_anteriores_2020.csv"
)

# Revisar años de primera matrícula anteriores a 2000
casos_matricula_antigua <- df_original |>
  filter(
    !is.na(annio_matricula1),
    annio_matricula1 < 2000
  ) |>
  arrange(annio_matricula1)

cat(
  "Matrículas anteriores a 2000:",
  nrow(casos_matricula_antigua), "\n"
)

write_csv(
  casos_matricula_antigua,
  "salidas/06_matriculas_antiguas.csv"
)


# ------------------------------------------------------------
# 6. INICIO DE LA DEPURACION
# ------------------------------------------------------------

limpio <- df_original |>
  mutate(
    programa_norm = norm_txt(programa_estudios)
  )

# 6.1. Guardar el registro con fecha anterior a 2020
# Solo se aplica la exclusión si hay exactamente un caso,
# tal como se identificó en la revisión previa.

casos_excluir <- limpio |>
  filter(
    !is.na(fecha_egreso),
    year(fecha_egreso) < 2020
  )

if (nrow(casos_excluir) != 1) {
  stop(
    "Se esperaba un único registro con fecha anterior a 2020. ",
    "Revise salidas/05_fechas_anteriores_2020.csv."
  )
}

registro_excluido <- casos_excluir

# Excluir el registro revisado
limpio <- limpio |>
  filter(
    is.na(fecha_egreso) |
      year(fecha_egreso) >= 2020
  )

write_csv(
  registro_excluido,
  "salidas/07_registro_excluido_fecha.csv"
)


# ------------------------------------------------------------
# 7. EXCLUSION DE LOS DOS REGISTROS ESPECIFICOS REVISADOS
# ------------------------------------------------------------

# Primer par: conservar el registro de 210 créditos.
# Excluir el registro de 202 créditos.
# Segundo par: conservar el registro de 213 créditos.
# Excluir el registro de 210 créditos.

limpio <- limpio |>
  filter(
    !(
      uuid == "0962a176eace2f5d27340c6f60970a8a" &
        creditos_acumulados == 202
    ),
    !(
      uuid == "2fe4de3201045a37db17e88c64af3132" &
        creditos_acumulados == 210
    )
  )

cat("Filas después de las exclusiones:", nrow(limpio), "\n")


# ------------------------------------------------------------
# 8. CORRECCION DEL CODIGO DE SEMESTRE
# ------------------------------------------------------------

limpio <- limpio |>
  mutate(
    semestre_corregido = semestre_egreso == "2024N1",
    semestre_egreso = if_else(
      semestre_egreso == "2024N1",
      "20241",
      semestre_egreso
    )
  )

cat(
  "Registros con semestre corregido:",
  sum(limpio$semestre_corregido, na.rm = TRUE), "\n"
)

cat(
  "Códigos 2024N1 restantes:",
  sum(limpio$semestre_egreso == "2024N1", na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------
# 9. TRATAMIENTO DE FALTANTES EN TIPO DE ESTUDIO
# ------------------------------------------------------------

# Se conservan las filas y se identifica la falta de información
limpio <- limpio |>
  mutate(
    detalle_tipo_estudio = replace_na(
      detalle_tipo_estudio,
      "SIN DATO"
    )
  )

cat(
  "Faltantes en detalle_tipo_estudio:",
  sum(is.na(limpio$detalle_tipo_estudio)), "\n"
)

cat(
  "Registros clasificados como SIN DATO:",
  sum(limpio$detalle_tipo_estudio == "SIN DATO", na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------
# 10. IMPUTACION DE LOS DOS PROMEDIOS CERO CON LA MEDIANA
# ------------------------------------------------------------

# Identificar los ceros antes de reemplazarlos
limpio <- limpio |>
  mutate(
    promedio_no_registrado =
      !is.na(promedio_final) & promedio_final == 0
  )

# Calcular la mediana de los promedios válidos y positivos
mediana_promedio <- median(
  limpio$promedio_final[
    !is.na(limpio$promedio_final) &
      limpio$promedio_final > 0
  ],
  na.rm = TRUE
)

# Imputar los ceros con la mediana
limpio <- limpio |>
  mutate(
    promedio_final = if_else(
      promedio_no_registrado,
      mediana_promedio,
      promedio_final
    )
  )

cat("Mediana usada para imputación:", mediana_promedio, "\n")

cat(
  "Promedios imputados:",
  sum(limpio$promedio_no_registrado, na.rm = TRUE), "\n"
)

cat(
  "Promedios cero restantes:",
  sum(limpio$promedio_final == 0, na.rm = TRUE), "\n"
)

cat(
  "Promedios faltantes restantes:",
  sum(is.na(limpio$promedio_final)), "\n"
)

print(summary(limpio$promedio_final))


# ------------------------------------------------------------
# 11. CREACION DE VARIABLES DERIVADAS
# ------------------------------------------------------------

limpio <- limpio |>
  mutate(
    # Diferencia aproximada entre años calendario
    duracion_anios = if_else(
      !is.na(annio_egreso) &
        !is.na(annio_matricula1),
      annio_egreso - annio_matricula1,
      NA_integer_
    ),
    
    # Año extraído de la fecha de egreso
    anio_fecha_egreso = year(fecha_egreso),
    
    # Identificar diferencias entre los dos campos de año
    discrepancia_egreso =
      !is.na(annio_egreso) &
      !is.na(anio_fecha_egreso) &
      annio_egreso != anio_fecha_egreso,
    
    # Marcar duraciones que requieren revisión
    duracion_extrema =
      !is.na(duracion_anios) &
      (duracion_anios > 10 | duracion_anios < 0)
  )


# ------------------------------------------------------------
# 12. IDENTIFICACION DE VALORES ATIPICOS POR NIVEL
# ------------------------------------------------------------

marca_iqr <- function(x) {
  if (sum(!is.na(x)) < 2) {
    return(rep(NA, length(x)))
  }
  
  cuartiles <- quantile(
    x,
    probs = c(0.25, 0.75),
    na.rm = TRUE,
    names = FALSE
  )
  
  rango_iqr <- cuartiles[2] - cuartiles[1]
  
  x < cuartiles[1] - 1.5 * rango_iqr |
    x > cuartiles[2] + 1.5 * rango_iqr
}

limpio <- limpio |>
  group_by(nivel_academico) |>
  mutate(
    atip_promedio = marca_iqr(promedio_final),
    atip_duracion = marca_iqr(duracion_anios)
  ) |>
  ungroup()

# Resumen de atípicos
resumen_atipicos <- limpio |>
  group_by(nivel_academico) |>
  summarise(
    registros = n(),
    atipicos_promedio = sum(atip_promedio, na.rm = TRUE),
    atipicos_duracion = sum(atip_duracion, na.rm = TRUE),
    duracion_minima = min(duracion_anios, na.rm = TRUE),
    duracion_maxima = max(duracion_anios, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_atipicos)

write_csv(
  resumen_atipicos,
  "salidas/08_resumen_atipicos.csv"
)


# ------------------------------------------------------------
# 13. REVISAR UUID REPETIDOS SIN ELIMINARLOS AUTOMATICAMENTE
# ------------------------------------------------------------

repetidos_pendientes <- limpio |>
  count(
    uuid, nivel_academico, programa_norm,
    name = "cantidad"
  ) |>
  filter(cantidad > 1) |>
  arrange(desc(cantidad))

cat(
  "Grupos repetidos pendientes de revisión:",
  nrow(repetidos_pendientes), "\n"
)

write_csv(
  repetidos_pendientes,
  "salidas/09_repetidos_pendientes.csv"
)


# ------------------------------------------------------------
# 14. VALIDACION FINAL
# ------------------------------------------------------------

cat("\n========== VALIDACION FINAL ==========\n")

cat("Filas originales:", nrow(df_original), "\n")
cat("Filas limpias:", nrow(limpio), "\n")
cat("Columnas finales:", ncol(limpio), "\n")

cat(
  "Promedios imputados:",
  sum(limpio$promedio_no_registrado, na.rm = TRUE), "\n"
)

cat(
  "Promedios faltantes:",
  sum(is.na(limpio$promedio_final)), "\n"
)

cat(
  "Faltantes en tipo de estudio:",
  sum(is.na(limpio$detalle_tipo_estudio)), "\n"
)

cat(
  "Discrepancias de año de egreso:",
  sum(limpio$discrepancia_egreso, na.rm = TRUE), "\n"
)

cat(
  "Semestres corregidos:",
  sum(limpio$semestre_corregido, na.rm = TRUE), "\n"
)

# Verificar la distribución de variables principales
print(limpio |> count(nivel_academico, sort = TRUE))
print(limpio |> count(situacion_alumno, sort = TRUE))
print(limpio |> count(sexo, sort = TRUE))
print(limpio |> count(sede, sort = TRUE))

# Resumen de faltantes finales
faltantes_finales <- limpio |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "faltantes"
  ) |>
  filter(faltantes > 0) |>
  arrange(desc(faltantes))

print(faltantes_finales, n = Inf)

write_csv(
  faltantes_finales,
  "salidas/10_faltantes_finales.csv"
)


# ------------------------------------------------------------
# 15. GUARDAR LA BASE LIMPIA
# ------------------------------------------------------------

# Mantener df_revision para continuar con tus siguientes análisis
df_revision <- limpio

write_csv(
  df_revision,
  "salidas/egresados_une_limpio.csv",
  na = ""
)

saveRDS(
  df_revision,
  "salidas/egresados_une_limpio.rds"
)

cat("\nProceso terminado. Archivos guardados en salidas.\n")

dim(df_revision)

table(df_revision$promedio_no_registrado, useNA = "ifany")

colSums(is.na(df_revision))

