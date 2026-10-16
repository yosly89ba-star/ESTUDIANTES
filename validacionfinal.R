
# ============================================================
# VALIDACION FINAL DE LA BASE LIMPIA
# EGRESADOS UNE 2020-2025
# ============================================================

# 1. Dimensiones
cat("\n--- 1. DIMENSIONES ---\n")
print(dim(df_revision))

# 2. Valores faltantes
cat("\n--- 2. VALORES FALTANTES ---\n")
print(colSums(is.na(df_revision)))

# 3. Promedios imputados
cat("\n--- 3. PROMEDIOS IMPUTADOS ---\n")
print(table(
  df_revision$promedio_no_registrado,
  useNA = "ifany"
))

cat(
  "Promedios cero restantes:",
  sum(df_revision$promedio_final == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Promedios negativos:",
  sum(df_revision$promedio_final < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Promedios superiores a 20:",
  sum(df_revision$promedio_final > 20, na.rm = TRUE),
  "\n"
)

# 4. Rangos de variables principales
cat("\n--- 4. RANGOS ---\n")

print(summary(df_revision$promedio_final))
print(summary(df_revision$creditos_acumulados))
print(summary(df_revision$duracion_anios))

# 5. Años de egreso
cat("\n--- 5. AÑOS DE EGRESO ---\n")
print(table(df_revision$annio_egreso, useNA = "ifany"))

cat(
  "Años fuera de 2020-2025:",
  sum(
    !is.na(df_revision$annio_egreso) &
      !(df_revision$annio_egreso %in% 2020:2025)
  ),
  "\n"
)

# 6. Códigos de semestre
cat("\n--- 6. SEMESTRES ---\n")
print(sort(unique(df_revision$semestre_egreso)))

cat(
  "Códigos 2024N1 restantes:",
  sum(df_revision$semestre_egreso == "2024N1", na.rm = TRUE),
  "\n"
)

# 7. Duplicados exactos
cat("\n--- 7. DUPLICADOS EXACTOS ---\n")
cat(
  "Filas duplicadas en todas las columnas:",
  sum(duplicated(df_revision)),
  "\n"
)

# 8. UUID repetidos
cat("\n--- 8. UUID REPETIDOS ---\n")

uuid_repetidos <- df_revision |>
  count(uuid, name = "n") |>
  filter(n > 1)

cat("UUID repetidos:", nrow(uuid_repetidos), "\n")

# 9. Situación del alumno por nivel
cat("\n--- 9. NIVEL Y SITUACION ---\n")

print(
  df_revision |>
    count(nivel_academico, situacion_alumno) |>
    arrange(nivel_academico, situacion_alumno)
)

# 10. Variables derivadas
cat("\n--- 10. VARIABLES DERIVADAS ---\n")

cat(
  "Discrepancias de año:",
  sum(df_revision$discrepancia_egreso, na.rm = TRUE),
  "\n"
)

cat(
  "Duraciones negativas:",
  sum(df_revision$duracion_anios < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Duraciones extremas marcadas:",
  sum(df_revision$duracion_extrema, na.rm = TRUE),
  "\n"
)

# 11. Guardar reporte de validación
reporte_validacion <- tibble(
  indicador = c(
    "Filas finales",
    "Columnas finales",
    "Valores faltantes totales",
    "Promedios imputados",
    "Promedios iguales a cero",
    "Filas duplicadas exactas",
    "UUID repetidos",
    "Discrepancias de año",
    "Duraciones negativas"
  ),
  resultado = c(
    nrow(df_revision),
    ncol(df_revision),
    sum(is.na(df_revision)),
    sum(df_revision$promedio_no_registrado, na.rm = TRUE),
    sum(df_revision$promedio_final == 0, na.rm = TRUE),
    sum(duplicated(df_revision)),
    nrow(uuid_repetidos),
    sum(df_revision$discrepancia_egreso, na.rm = TRUE),
    sum(df_revision$duracion_anios < 0, na.rm = TRUE)
  )
)

write_csv(
  reporte_validacion,
  "salidas/11_reporte_validacion_final.csv"
)

# Guardar versión validada
write_csv(
  df_revision,
  "salidas/egresados_une_validado.csv",
  na = ""
)

saveRDS(
  df_revision,
  "salidas/egresados_une_validado.rds"
)

cat("\nValidación ejecutada. Reporte guardado en salidas.\n")

