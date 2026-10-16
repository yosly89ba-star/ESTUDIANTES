
# ============================================================
# PROYECTO: EGRESADOS UNE 2020-2025
# SCRIPT 02: ANALISIS EXPLORATORIO PARA ARTICULO CIENTIFICO
# ============================================================

library(tidyverse)
library(ggplot2)
library(gt)

# ------------------------------------------------------------
# 1. CARGAR DATOS Y CONFIGURAR SALIDAS
# ------------------------------------------------------------

df_revision <- readRDS("salidas/egresados_une_validado.rds")

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/graficos", recursive = TRUE, showWarnings = FALSE)

stopifnot(nrow(df_revision) == 11675)

# ------------------------------------------------------------
# 2. PALETA DE COLORES Y TEMA DE PUBLICACION
# ------------------------------------------------------------

azul <- "#174A6E"
turquesa <- "#279D9B"
dorado <- "#D9A441"
coral <- "#D96C5F"
gris <- "#718096"
claro <- "#EAF1F5"

paleta <- c(
  "PREGRADO" = azul,
  "POSGRADO" = turquesa,
  "SEGUNDA ESPECIALIDAD" = dorado
)

tema_articulo <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 14, face = "bold", color = azul,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 10, color = gris,
      margin = margin(b = 12)
    ),
    axis.title = element_text(
      size = 11, face = "bold", color = azul
    ),
    axis.text = element_text(size = 10, color = "#344054"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E5EAF0", linewidth = 0.4
    ),
    legend.title = element_text(face = "bold", color = azul),
    legend.position = "bottom",
    plot.background = element_rect(
      fill = "white", color = NA
    ),
    panel.background = element_rect(
      fill = "white", color = NA
    ),
    plot.margin = margin(12, 15, 12, 12)
  )

# Función para guardar gráficos en alta resolución
guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 5) {
  ggsave(
    filename = file.path("salidas/graficos", nombre),
    plot = grafico,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

# Función para guardar tablas en CSV y HTML
guardar_tabla <- function(datos, nombre, titulo) {
  write_csv(
    datos,
    file.path("salidas/tablas", paste0(nombre, ".csv"))
  )
  
  tabla_html <- datos |>
    gt() |>
    tab_header(title = md(paste0("**", titulo, "**"))) |>
    opt_row_striping() |>
    tab_options(
      table.font.names = c("Arial", "sans-serif"),
      table.font.size = 12,
      heading.title.font.size = 15,
      heading.title.font.weight = "bold",
      column_labels.background.color = azul,
      column_labels.font.weight = "bold",
      column_labels.font.color = "white",
      table.border.top.color = azul,
      table.border.top.width = px(3),
      table.border.bottom.color = azul,
      table.border.bottom.width = px(2),
      row.striping.background_color = "#F0F5F8"
    )
  
  gtsave(
    tabla_html,
    file.path("salidas/tablas", paste0(nombre, ".html"))
  )
}

# ------------------------------------------------------------
# 3. TABLA Y GRAFICO: NIVEL ACADEMICO
# ------------------------------------------------------------

tabla_nivel <- df_revision |>
  count(nivel_academico, name = "Frecuencia") |>
  mutate(
    Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)
  ) |>
  arrange(desc(Frecuencia))

print(tabla_nivel)

grafico_nivel <- ggplot(
  tabla_nivel,
  aes(
    x = reorder(nivel_academico, Frecuencia),
    y = Frecuencia,
    fill = nivel_academico
  )
) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      format(Frecuencia, big.mark = ","),
      " (", format(Porcentaje, nsmall = 2), "%)"
    )),
    hjust = -0.08, size = 3.7, color = "#344054"
  ) +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0, 0.25))
  ) +
  coord_flip() +
  labs(
    title = "Distribución de egresados por nivel académico",
    subtitle = "UNE, periodo 2020–2025",
    x = NULL,
    y = "Número de registros"
  ) +
  tema_articulo

print(grafico_nivel)
guardar_grafico(grafico_nivel, "01_nivel_academico.png")
guardar_tabla(tabla_nivel, "01_nivel_academico", "Distribución por nivel académico")

# ------------------------------------------------------------
# 4. TABLA Y GRAFICO: SITUACION SEGUN NIVEL
# ------------------------------------------------------------

tabla_situacion <- df_revision |>
  count(nivel_academico, situacion_alumno, name = "Frecuencia") |>
  group_by(nivel_academico) |>
  mutate(
    Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)
  ) |>
  ungroup()

print(tabla_situacion)

grafico_situacion <- ggplot(
  tabla_situacion,
  aes(
    x = nivel_academico,
    y = Frecuencia,
    fill = situacion_alumno
  )
) +
  geom_col(width = 0.68, color = "white", linewidth = 0.25) +
  scale_fill_manual(values = c(
    "EGRESADO" = azul,
    "BACHILLER" = turquesa,
    "TITULO" = dorado
  )) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Situación del alumno según nivel académico",
    subtitle = "Frecuencia de registros por categoría",
    x = "Nivel académico",
    y = "Número de registros",
    fill = "Situación del alumno"
  ) +
  tema_articulo

print(grafico_situacion)
guardar_grafico(grafico_situacion, "02_situacion_por_nivel.png", 9, 5)
guardar_tabla(tabla_situacion, "02_situacion_por_nivel", "Situación del alumno por nivel")

# ------------------------------------------------------------
# 5. TABLA Y GRAFICO: SEXO
# ------------------------------------------------------------

tabla_sexo <- df_revision |>
  count(sexo, name = "Frecuencia") |>
  mutate(
    Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)
  )

print(tabla_sexo)

grafico_sexo <- ggplot(
  tabla_sexo,
  aes(x = sexo, y = Frecuencia, fill = sexo)
) +
  geom_col(width = 0.58, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(
      format(Frecuencia, big.mark = ","),
      "\n", Porcentaje, "%"
    )),
    vjust = -0.25, size = 3.8, color = "#344054"
  ) +
  scale_fill_manual(values = c(
    "F" = turquesa,
    "M" = azul
  )) +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Distribución de egresados por sexo",
    subtitle = "UNE, periodo 2020–2025",
    x = "Sexo",
    y = "Número de registros"
  ) +
  tema_articulo

print(grafico_sexo)
guardar_grafico(grafico_sexo, "03_distribucion_sexo.png", 7, 5)
guardar_tabla(tabla_sexo, "03_distribucion_sexo", "Distribución por sexo")

# ------------------------------------------------------------
# 6. TABLA Y GRAFICO: AÑO DE EGRESO
# ------------------------------------------------------------

tabla_anio <- df_revision |>
  count(annio_egreso, name = "Frecuencia") |>
  mutate(
    Porcentaje = round(100 * Frecuencia / sum(Frecuencia), 2)
  ) |>
  arrange(annio_egreso)

print(tabla_anio)

grafico_anio <- ggplot(
  tabla_anio,
  aes(x = factor(annio_egreso), y = Frecuencia)
) +
  geom_col(fill = azul, width = 0.65) +
  geom_line(
    aes(group = 1, y = Frecuencia),
    color = turquesa, linewidth = 0.9
  ) +
  geom_point(color = dorado, size = 3) +
  geom_text(
    aes(label = format(Frecuencia, big.mark = ",")),
    vjust = -0.7, size = 3.5, color = "#344054"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    title = "Registros por año de egreso",
    subtitle = "Distribución anual, 2020–2025",
    x = "Año de egreso",
    y = "Número de registros"
  ) +
  tema_articulo

print(grafico_anio)
guardar_grafico(grafico_anio, "04_anio_egreso.png")
guardar_tabla(tabla_anio, "04_anio_egreso", "Distribución por año de egreso")

# ------------------------------------------------------------
# 7. TABLA Y GRAFICO: PROMEDIO FINAL
# ------------------------------------------------------------

resumen_promedio <- df_revision |>
  summarise(
    N = n(),
    Media = mean(promedio_final),
    Mediana = median(promedio_final),
    Desviacion_estandar = sd(promedio_final),
    Minimo = min(promedio_final),
    Q1 = quantile(promedio_final, 0.25),
    Q3 = quantile(promedio_final, 0.75),
    Maximo = max(promedio_final)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(resumen_promedio)

grafico_promedio <- ggplot(
  df_revision,
  aes(x = promedio_final)
) +
  geom_histogram(
    bins = 30,
    fill = turquesa,
    color = "white",
    linewidth = 0.35
  ) +
  geom_vline(
    aes(xintercept = mean(promedio_final)),
    color = coral, linewidth = 1, linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = median(promedio_final)),
    color = azul, linewidth = 1, linetype = "dotted"
  ) +
  labs(
    title = "Distribución del promedio final",
    subtitle = "Línea discontinua: media; línea punteada: mediana",
    x = "Promedio final",
    y = "Frecuencia"
  ) +
  tema_articulo

print(grafico_promedio)
guardar_grafico(grafico_promedio, "05_distribucion_promedio.png")
guardar_tabla(
  resumen_promedio, "05_resumen_promedio",
  "Estadísticos descriptivos del promedio final"
)

# ------------------------------------------------------------
# 8. TABLA Y GRAFICO: CREDITOS ACUMULADOS
# ------------------------------------------------------------

resumen_creditos <- df_revision |>
  summarise(
    N = n(),
    Media = mean(creditos_acumulados),
    Mediana = median(creditos_acumulados),
    Desviacion_estandar = sd(creditos_acumulados),
    Minimo = min(creditos_acumulados),
    Q1 = quantile(creditos_acumulados, 0.25),
    Q3 = quantile(creditos_acumulados, 0.75),
    Maximo = max(creditos_acumulados)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(resumen_creditos)

grafico_creditos <- ggplot(
  df_revision,
  aes(x = creditos_acumulados)
) +
  geom_histogram(
    bins = 30,
    fill = azul,
    color = "white",
    linewidth = 0.35
  ) +
  labs(
    title = "Distribución de créditos acumulados",
    subtitle = "UNE, periodo 2020–2025",
    x = "Créditos acumulados",
    y = "Frecuencia"
  ) +
  tema_articulo

print(grafico_creditos)
guardar_grafico(grafico_creditos, "06_distribucion_creditos.png")
guardar_tabla(
  resumen_creditos, "06_resumen_creditos",
  "Estadísticos descriptivos de créditos acumulados"
)

# ------------------------------------------------------------
# 9. TABLA Y GRAFICO: PROMEDIO SEGUN NIVEL ACADEMICO
# ------------------------------------------------------------

resumen_por_nivel <- df_revision |>
  group_by(nivel_academico) |>
  summarise(
    N = n(),
    Media = mean(promedio_final),
    Mediana = median(promedio_final),
    Desviacion_estandar = sd(promedio_final),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(resumen_por_nivel)

grafico_promedio_nivel <- ggplot(
  df_revision,
  aes(
    x = nivel_academico,
    y = promedio_final,
    fill = nivel_academico
  )
) +
  geom_boxplot(
    width = 0.62,
    alpha = 0.88,
    outlier.color = coral,
    outlier.alpha = 0.5,
    outlier.size = 1.2
  ) +
  scale_fill_manual(values = paleta) +
  labs(
    title = "Promedio final según nivel académico",
    subtitle = "La caja representa el rango intercuartílico",
    x = "Nivel académico",
    y = "Promedio final"
  ) +
  tema_articulo +
  theme(legend.position = "none")

print(grafico_promedio_nivel)
guardar_grafico(
  grafico_promedio_nivel,
  "07_promedio_por_nivel.png",
  9, 5
)
guardar_tabla(
  resumen_por_nivel, "07_promedio_por_nivel",
  "Promedio final según nivel académico"
)

# ------------------------------------------------------------
# 10. EXPORTAR REPORTE DE FRECUENCIAS Y RESUMENES
# ------------------------------------------------------------

cat("\n========================================\n")
cat("ANALISIS EXPLORATORIO COMPLETADO\n")
cat("========================================\n")
cat("Registros analizados:", nrow(df_revision), "\n")
cat("Variables disponibles:", ncol(df_revision), "\n")
cat("Tablas CSV y HTML: salidas/tablas/\n")
cat("Graficos PNG a 300 dpi: salidas/graficos/\n")
