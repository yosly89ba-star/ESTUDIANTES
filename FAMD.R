
# ============================================================
# SCRIPT 03: ANALISIS FACTORIAL DE DATOS MIXTOS (FAMD)
# Egresados UNE, periodo 2020-2025
# ============================================================

# 1. Instalar paquetes si todavía no están instalados
paquetes <- c("tidyverse", "FactoMineR", "factoextra")

faltantes <- paquetes[
  !paquetes %in% installed.packages()[, "Package"]
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

# 2. Cargar paquetes
library(tidyverse)
library(FactoMineR)
library(factoextra)

# 3. Crear carpeta de resultados
if (!dir.exists("resultados_FAMD")) {
  dir.create("resultados_FAMD")
}

# 4. Cargar la base validada
# Si el archivo está en otra carpeta, ajusta la ruta.
df_revision <- readRDS("salidas/egresados_une_validado.rds")

# 5. Verificar las variables necesarias
variables_necesarias <- c(
  "promedio_final",
  "creditos_acumulados",
  "duracion_anios",
  "nivel_academico"
)

faltan <- setdiff(variables_necesarias, names(df_revision))

if (length(faltan) > 0) {
  stop(
    paste(
      "Faltan estas variables en la base:",
      paste(faltan, collapse = ", ")
    )
  )
}

# 6. Seleccionar variables para el análisis factorial
datos_famd <- df_revision %>%
  select(all_of(variables_necesarias)) %>%
  mutate(
    nivel_academico = factor(nivel_academico)
  )

# 7. Comprobar estructura y valores ausentes
str(datos_famd)

cat("Número de registros:", nrow(datos_famd), "\n")
cat("Número de variables:", ncol(datos_famd), "\n")
cat("Valores ausentes:", sum(is.na(datos_famd)), "\n")

# 8. Comprobar valores extremos de duración
summary(datos_famd$duracion_anios)

# 9. Revisar frecuencias del nivel académico
table(datos_famd$nivel_academico, useNA = "ifany")

##

# ============================================================
# REVISION DE DURACIONES EXTREMAS
# ============================================================

# Frecuencias de duración
datos_famd %>%
  count(duracion_anios, sort = TRUE) %>%
  print(n = 30)

# Registros con duración igual o superior a 10 años
casos_duracion_10 <- df_revision %>%
  filter(duracion_anios >= 10) %>%
  select(
    nivel_academico,
    facultad,
    programa_estudios,
    situacion_alumno,
    annio_matricula1,
    annio_egreso,
    duracion_anios
  ) %>%
  arrange(desc(duracion_anios))

cat(
  "Registros con duración >= 10 años:",
  nrow(casos_duracion_10), "\n"
)

# Mostrar los 20 casos de mayor duración
print(head(casos_duracion_10, 20))

#comparar la duracion con por nivel academico

# ============================================================
# COMPARACION DE DURACION POR NIVEL ACADEMICO
# ============================================================

# Resumen estadístico por nivel
resumen_duracion <- df_revision %>%
  group_by(nivel_academico) %>%
  summarise(
    total = n(),
    mediana = median(duracion_anios, na.rm = TRUE),
    media = mean(duracion_anios, na.rm = TRUE),
    minimo = min(duracion_anios, na.rm = TRUE),
    maximo = max(duracion_anios, na.rm = TRUE),
    casos_10_o_mas = sum(duracion_anios >= 10, na.rm = TRUE),
    porcentaje_10_o_mas = round(
      mean(duracion_anios >= 10, na.rm = TRUE) * 100, 2
    ),
    .groups = "drop"
  )

print(resumen_duracion)

# Revisar las duraciones más frecuentes por nivel
df_revision %>%
  count(nivel_academico, duracion_anios) %>%
  group_by(nivel_academico) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  arrange(nivel_academico, desc(n)) %>%
  print(n = 30)

# Guardar los resultados
write.csv(
  resumen_duracion,
  "resultados_FAMD/resumen_duracion_por_nivel.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 
# ============================================================
# 10. EJECUTAR EL ANALISIS FACTORIAL DE DATOS MIXTOS (FAMD)
# ============================================================

# Verificar que no existan valores ausentes
stopifnot(sum(is.na(datos_famd)) == 0)

# Ejecutar FAMD
set.seed(123)

modelo_famd <- FactoMineR::FAMD(
  datos_famd,
  ncp = 5,
  graph = FALSE
)

# ============================================================
# 11. VARIANZA EXPLICADA POR DIMENSION
# ============================================================

tabla_varianza <- as.data.frame(modelo_famd$eig)

tabla_varianza <- data.frame(
  Dimension = paste0("Dim", seq_len(nrow(tabla_varianza))),
  Varianza_porcentual = tabla_varianza[, 2],
  Varianza_acumulada = tabla_varianza[, 3]
)

print(round(tabla_varianza, 2))

write.csv(
  tabla_varianza,
  "resultados_FAMD/varianza_explicada.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 12. GRAFICO DE VARIANZA EXPLICADA
# ============================================================

grafico_varianza <- factoextra::fviz_eig(
  modelo_famd,
  addlabels = TRUE,
  barfill = "#279D9B",
  barcolor = "#174A6E",
  linecolor = "#D9A441"
) +
  ggplot2::labs(
    title = "Varianza explicada por las dimensiones factoriales",
    x = "Dimensiones factoriales",
    y = "Varianza explicada (%)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(grafico_varianza)

ggplot2::ggsave(
  "resultados_FAMD/01_varianza_explicada.png",
  plot = grafico_varianza,
  width = 9,
  height = 6,
  dpi = 300
)

cat("\nFAMD ejecutado correctamente.\n")
cat("Resultados guardados en resultados_FAMD.\n")
#######

# ============================================================
# 13. CONSULTAR RESULTADOS DEL FAMD
# ============================================================

# Mostrar la varianza explicada
print(round(tabla_varianza, 2))

# Comprobar que el modelo contiene las coordenadas individuales
dim(modelo_famd$ind$coord)

# Mostrar las primeras filas de las coordenadas factoriales
head(round(modelo_famd$ind$coord, 3))


# Mostrar la tabla de varianza sin redondear la columna de texto
tabla_varianza %>%
  mutate(
    Varianza_porcentual = round(Varianza_porcentual, 2),
    Varianza_acumulada = round(Varianza_acumulada, 2)
  ) %>%
  print()
##

# ============================================================
# 14. CONTRIBUCIONES DE LAS VARIABLES
# ============================================================

# Contribucion de las variables cuantitativas
contrib_cuant <- modelo_famd$quanti$contrib

print(round(contrib_cuant, 2))

write.csv(
  contrib_cuant,
  "resultados_FAMD/contribuciones_cuantitativas.csv",
  fileEncoding = "UTF-8"
)

# ============================================================
# 15. COS2 DE LAS VARIABLES CUANTITATIVAS
# ============================================================

cos2_cuant <- modelo_famd$quanti$cos2

print(round(cos2_cuant, 3))

write.csv(
  cos2_cuant,
  "resultados_FAMD/cos2_cuantitativas.csv",
  fileEncoding = "UTF-8"
)

# ============================================================
# 16. CONTRIBUCIONES DE LAS CATEGORIAS CUALITATIVAS
# ============================================================

contrib_cualit <- modelo_famd$quali.var$contrib

print(round(contrib_cualit, 2))

write.csv(
  contrib_cualit,
  "resultados_FAMD/contribuciones_categorias.csv",
  fileEncoding = "UTF-8"
)

# ============================================================
# 17. COS2 DE LAS CATEGORIAS CUALITATIVAS
# ============================================================

cos2_cualit <- modelo_famd$quali.var$cos2

print(round(cos2_cualit, 3))

write.csv(
  cos2_cualit,
  "resultados_FAMD/cos2_categorias.csv",
  fileEncoding = "UTF-8"
)
##3
# Contribuciones de las variables cuantitativas
print(round(modelo_famd$quanti$contrib, 2))

# Contribuciones de las categorías de nivel académico
print(round(modelo_famd$quali.var$contrib, 2))
############

# ============================================================
# GRAFICOS DE CONTRIBUCIONES DEL FAMD
# ============================================================

library(tidyverse)

# Crear carpeta si todavía no existe
if (!dir.exists("resultados_FAMD")) {
  dir.create("resultados_FAMD")
}

# Colores
azul <- "#174A6E"
turquesa <- "#279D9B"
dorado <- "#D9A441"

# ------------------------------------------------------------
# GRAFICO 1. CONTRIBUCIONES DE VARIABLES CUANTITATIVAS
# ------------------------------------------------------------

contrib_cuant_graf <- as.data.frame(
  modelo_famd$quanti$contrib
) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(
    cols = starts_with("Dim."),
    names_to = "Dimension",
    values_to = "Contribucion"
  )

p_contrib_cuant <- ggplot(
  contrib_cuant_graf,
  aes(x = reorder(Variable, Contribucion),
      y = Contribucion)
) +
  geom_col(fill = azul, width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", Contribucion)),
    hjust = -0.15, size = 3.5
  ) +
  facet_wrap(~ Dimension, ncol = 2) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Contribución de las variables cuantitativas",
    x = NULL,
    y = "Contribución (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(p_contrib_cuant)

ggsave(
  "resultados_FAMD/06_contribuciones_cuantitativas.png",
  p_contrib_cuant, width = 10, height = 7, dpi = 300
)

# ------------------------------------------------------------
# GRAFICO 2. CONTRIBUCIONES DE CATEGORIAS DE NIVEL ACADEMICO
# ------------------------------------------------------------

contrib_cualit_graf <- as.data.frame(
  modelo_famd$quali.var$contrib
) %>%
  rownames_to_column("Categoria") %>%
  pivot_longer(
    cols = starts_with("Dim."),
    names_to = "Dimension",
    values_to = "Contribucion"
  )

p_contrib_cualit <- ggplot(
  contrib_cualit_graf,
  aes(x = reorder(Categoria, Contribucion),
      y = Contribucion)
) +
  geom_col(fill = turquesa, width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", Contribucion)),
    hjust = -0.15, size = 3.5
  ) +
  facet_wrap(~ Dimension, ncol = 2) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Contribución de las categorías de nivel académico",
    x = NULL,
    y = "Contribución (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

print(p_contrib_cualit)

ggsave(
  "resultados_FAMD/07_contribuciones_nivel_academico.png",
  p_contrib_cualit, width = 10, height = 7, dpi = 300
)

cat("Gráficos de contribuciones guardados correctamente.\n")
##########

# ============================================================
# MAPA FACTORIAL DE LAS CATEGORIAS DE NIVEL ACADEMICO
# ============================================================

library(factoextra)
library(ggplot2)

p_mapa_famd <- fviz_famd_var(
  modelo_famd,
  axes = c(1, 2),
  repel = TRUE,
  col.var = "#174A6E",
  col.quali.var = "#D96C5F"
) +
  labs(
    title = "Mapa factorial del nivel académico de los egresados",
    subtitle = "Dimensiones 1 y 2 del análisis FAMD",
    x = "Dimensión 1",
    y = "Dimensión 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

print(p_mapa_famd)

ggsave(
  "resultados_FAMD/08_mapa_factorial_nivel_academico.png",
  plot = p_mapa_famd,
  width = 9,
  height = 7,
  dpi = 300
)
#33

library(ggplot2)
library(dplyr)

# Coordenadas de las categorías cualitativas
coord_nivel <- as.data.frame(modelo_famd$quali.var$coord) %>%
  tibble::rownames_to_column("Categoria")

# Contribuciones de las categorías en Dim 1 y Dim 2
contrib_nivel <- as.data.frame(modelo_famd$quali.var$contrib) %>%
  tibble::rownames_to_column("Categoria") %>%
  mutate(
    Contribucion_plano = `Dim.1` + `Dim.2`
  ) %>%
  select(Categoria, Contribucion_plano)

# Unir coordenadas y contribuciones
mapa_nivel <- left_join(
  coord_nivel,
  contrib_nivel,
  by = "Categoria"
)

# Mapa factorial
p_figura7 <- ggplot(
  mapa_nivel,
  aes(
    x = `Dim.1`,
    y = `Dim.2`,
    label = Categoria,
    color = Contribucion_plano
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray75",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray75",
    linewidth = 0.4
  ) +
  geom_point(size = 4) +
  geom_text(
    nudge_y = 0.08,
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_gradient(
    low = "#9ECAE1",
    high = "#B2182B",
    name = "Contribución\nal plano (%)"
  ) +
  labs(
    title = "Figura 7. Plano Dim 1 – Dim 2",
    subtitle = "Categorías coloreadas por contribución",
    x = "Dimensión 1 (52,46 %)",
    y = "Dimensión 2 (21,16 %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35"),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p_figura7)

ggsave(
  "resultados_FAMD/Figura_7_plano_Dim1_Dim2.png",
  plot = p_figura7,
  width = 10,
  height = 7,
  dpi = 300
)

###

# Contribución de las variables cuantitativas a la Dimensión 1
fviz_contrib(
  modelo_famd,
  choice = "quanti.var",
  axes = 1,
  top = 10,
  fill = "#174A6E",
  color = "#174A6E"
)
################

# Contribución de las variables cuantitativas a la Dimensión 2
p_contrib_dim2 <- factoextra::fviz_contrib(
  modelo_famd,
  choice = "quanti.var",
  axes = 2,
  top = 10,
  fill = "#279D9B",
  color = "#279D9B"
) +
  labs(
    title = "Contribución de las variables cuantitativas",
    subtitle = "Dimensión 2 del análisis FAMD",
    x = "Variables",
    y = "Contribución (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_contrib_dim2)

ggsave(
  "resultados_FAMD/contribucion_variables_Dim2.png",
  plot = p_contrib_dim2,
  width = 8,
  height = 5,
  dpi = 300
)
####

library(FactoMineR)
library(factoextra)
library(ggplot2)

# Obtener resultados de las variables cuantitativas
quanti.var <- get_famd_var(modelo_famd, "quanti.var")

# Consultar coordenadas, contribuciones y cos2
print(quanti.var$coord)
print(round(quanti.var$contrib, 2))
print(round(quanti.var$cos2, 3))

# Diagrama factorial de variables cuantitativas
p_var_cuant <- fviz_famd_var(
  modelo_famd,
  choice = "quanti.var",
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  axes = c(1, 2)
) +
  labs(
    title = "Variables cuantitativas en el plano factorial",
    subtitle = "FAMD: dimensiones 1 y 2",
    x = "Dimensión 1 (52,46 %)",
    y = "Dimensión 2 (21,16 %)"
  ) +
  theme_minimal(base_size = 12)

print(p_var_cuant)

ggsave(
  "resultados_FAMD/10_mapa_variables_cuantitativas.png",
  plot = p_var_cuant,
  width = 9,
  height = 7,
  dpi = 300
)

####

# ============================================================
# MAPA FACTORIAL DE VARIABLES CUANTITATIVAS COLOREADAS POR COS2
# ============================================================

p_cos2_cuant <- factoextra::fviz_famd_var(
  modelo_famd,
  choice = "quanti.var",
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  axes = c(1, 2)
) +
  labs(
    title = "Calidad de representación de las variables cuantitativas",
    subtitle = "Plano factorial de las dimensiones 1 y 2",
    x = "Dimensión 1 (52,46 %)",
    y = "Dimensión 2 (21,16 %)"
  ) +
  theme_minimal(base_size = 12)

print(p_cos2_cuant)

ggsave(
  "resultados_FAMD/11_mapa_variables_cos2.png",
  plot = p_cos2_cuant,
  width = 9,
  height = 7,
  dpi = 300
)

# Cos2 acumulado de las dimensiones 1 y 2
cos2_plano <- rowSums(
  modelo_famd$quanti$cos2[, 1:2, drop = FALSE]
)

round(cos2_plano, 3)
###

# ============================================================
# CONTRIBUCIONES DE VARIABLES CUANTITATIVAS: DIM 3 Y DIM 4
# ============================================================

contrib_cuant_34 <- modelo_famd$quanti$contrib[, 3:4]

print(round(contrib_cuant_34, 2))

# Gráfico de contribución: Dimensión 3
p_contrib_dim3 <- factoextra::fviz_contrib(
  modelo_famd,
  choice = "quanti.var",
  axes = 3,
  fill = "#D9A441",
  color = "#D9A441"
) +
  labs(
    title = "Contribución de las variables cuantitativas",
    subtitle = "Dimensión 3",
    x = "Variables",
    y = "Contribución (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_contrib_dim3)

ggsave(
  "resultados_FAMD/contribucion_variables_Dim3.png",
  p_contrib_dim3,
  width = 8, height = 5, dpi = 300
)

# Gráfico de contribución: Dimensión 4
p_contrib_dim4 <- factoextra::fviz_contrib(
  modelo_famd,
  choice = "quanti.var",
  axes = 4,
  fill = "#D96C5F",
  color = "#D96C5F"
) +
  labs(
    title = "Contribución de las variables cuantitativas",
    subtitle = "Dimensión 4",
    x = "Variables",
    y = "Contribución (%)"
  ) +
  theme_minimal(base_size = 12)

print(p_contrib_dim4)

ggsave(
  "resultados_FAMD/contribucion_variables_Dim4.png",
  p_contrib_dim4,
  width = 8, height = 5, dpi = 300
)
###calcular la dimension

# ============================================================
# CALIDAD DE REPRESENTACIÓN: DIM 1-2 FRENTE A DIM 1-4
# ============================================================

cos2_cuant <- modelo_famd$quanti$cos2

tabla_cos2_dimensiones <- data.frame(
  Variable = rownames(cos2_cuant),
  Cos2_Dim1_Dim2 = rowSums(cos2_cuant[, 1:2, drop = FALSE]),
  Cos2_Dim1_Dim4 = rowSums(cos2_cuant[, 1:4, drop = FALSE])
) %>%
  mutate(
    Cos2_Dim1_Dim2 = round(Cos2_Dim1_Dim2, 3),
    Cos2_Dim1_Dim4 = round(Cos2_Dim1_Dim4, 3)
  )

print(tabla_cos2_dimensiones)

write.csv(
  tabla_cos2_dimensiones,
  "resultados_FAMD/tabla_cos2_dimensiones.csv",
  row.names = FALSE
)
#

# ============================================================
# FAMD: CATEGORÍAS CUALITATIVAS COLOREADAS POR CONTRIBUCIÓN
# ============================================================

library(factoextra)
library(ggplot2)

# Extraer resultados de las categorías cualitativas
quali.var <- factoextra::get_famd_var(
  modelo_famd,
  "quali.var"
)

# Consultar coordenadas, cos2 y contribuciones
print(quali.var$coord)
print(round(quali.var$cos2, 3))
print(round(quali.var$contrib, 2))

# Mapa factorial coloreado por contribución
p_mapa_cualitativas <- factoextra::fviz_famd_var(
  modelo_famd,
  choice = "quali.var",
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  axes = c(1, 2),
  repel = TRUE
) +
  labs(
    title = "Figura 7. Plano Dim 1 – Dim 2",
    subtitle = "Categorías coloreadas por contribución",
    x = "Dimensión 1 (52,46 %)",
    y = "Dimensión 2 (21,16 %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35"),
    axis.title = element_text(face = "bold")
  )

print(p_mapa_cualitativas)

# Guardar en alta resolución
ggsave(
  "resultados_FAMD/Figura_7_categorias_contribucion.png",
  plot = p_mapa_cualitativas,
  width = 10,
  height = 7,
  dpi = 300
)
####

# ============================================================
# MAPA FACTORIAL DE INDIVIDUOS COLOREADOS POR COS2
# ============================================================

library(factoextra)
library(ggplot2)

# Extraer resultados de los individuos
ind_famd <- factoextra::get_famd_ind(modelo_famd)

# Revisar coordenadas, cos2 y contribuciones
dim(ind_famd$coord)
head(ind_famd$coord)
head(ind_famd$cos2)
head(ind_famd$contrib)

# Gráfico de individuos coloreados por calidad de representación
p_ind_cos2 <- factoextra::fviz_famd_ind(
  modelo_famd,
  col.ind = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  axes = c(1, 2),
  repel = FALSE,
  alpha.ind = 0.45,
  pointsize = 1.2
) +
  labs(
    title = "Distribución factorial de los egresados",
    subtitle = "Individuos coloreados por cos²",
    x = "Dimensión 1 (52,46 %)",
    y = "Dimensión 2 (21,16 %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35")
  )

print(p_ind_cos2)

ggsave(
  "resultados_FAMD/12_individuos_cos2.png",
  p_ind_cos2,
  width = 10,
  height = 7,
  dpi = 300
)
