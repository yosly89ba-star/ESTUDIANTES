# ============================================================
# PASO 1. PREPARAR COORDENADAS PARA K-MEANS
# ============================================================

# Extraer coordenadas de los egresados
coord_famd <- as.data.frame(modelo_famd$ind$coord)

# Verificar dimensiones y nombres
dim(coord_famd)
names(coord_famd)

# Crear conjuntos de coordenadas para comparar
coord_12 <- coord_famd[, 1:2, drop = FALSE]
coord_13 <- coord_famd[, 1:3, drop = FALSE]
coord_14 <- coord_famd[, 1:4, drop = FALSE]

# Comprobar valores faltantes
colSums(is.na(coord_famd))

# Guardar las coordenadas
saveRDS(
  list(
    coord_12 = coord_12,
    coord_13 = coord_13,
    coord_14 = coord_14
  ),
  "resultados_FAMD/coordenadas_para_kmeans.rds"
)
##

# ============================================================
# PASO 2. SELECCIONAR K-MEANS
# Comparar 2, 3 y 4 dimensiones
# ============================================================
install.packages(c("FactoMineR", "factoextra", "cluster", "dplyr", "ggplot2", "readr"))

library(cluster)
library(ggplot2)
library(dplyr)

set.seed(123)

# Conjuntos de coordenadas factoriales
conjuntos <- list(
  "2 dimensiones" = coord_12,
  "3 dimensiones" = coord_13,
  "4 dimensiones" = coord_14
)

# Muestra para calcular silhouette
n_muestra <- min(2000, nrow(coord_famd))
id_muestra <- sample(seq_len(nrow(coord_famd)), n_muestra)

resultados <- list()
modelos <- list()

# Evaluar k entre 2 y 8
for (nombre in names(conjuntos)) {
  
  datos <- conjuntos[[nombre]]
  resultados_k <- data.frame()
  
  for (k in 2:8) {
    
    set.seed(123)
    
    modelo <- kmeans(
      datos,
      centers = k,
      nstart = 25,
      iter.max = 100
    )
    
    # Asignaciones de la muestra
    grupos_muestra <- modelo$cluster[id_muestra]
    datos_muestra <- datos[id_muestra, , drop = FALSE]
    
    # Silhouette promedio
    sil <- silhouette(
      grupos_muestra,
      dist(datos_muestra)
    )
    
    sil_promedio <- mean(sil[, "sil_width"])
    
    # Índice Davies-Bouldin
    # Calculado a partir de las distancias a centroides
    centroides <- modelo$centers
    dispersion <- numeric(k)
    
    for (j in seq_len(k)) {
      integrantes <- datos[modelo$cluster == j, , drop = FALSE]
      
      distancias <- sqrt(
        rowSums(
          sweep(integrantes, 2, centroides[j, ], "-")^2
        )
      )
      
      dispersion[j] <- mean(distancias)
    }
    
    dist_centros <- as.matrix(dist(centroides))
    razon <- matrix(0, nrow = k, ncol = k)
    
    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        if (i != j) {
          razon[i, j] <-
            (dispersion[i] + dispersion[j]) /
            dist_centros[i, j]
        }
      }
    }
    
    db <- mean(apply(razon, 1, max))
    
    resultados_k <- rbind(
      resultados_k,
      data.frame(
        Dimensiones = nombre,
        k = k,
        WSS = modelo$tot.withinss,
        Silhouette = sil_promedio,
        Davies_Bouldin = db
      )
    )
    
    modelos[[paste(nombre, k, sep = "_")]] <- modelo
  }
  
  resultados[[nombre]] <- resultados_k
}

tabla_resultados <- bind_rows(resultados)

# Mostrar resultados ordenados
print(
  tabla_resultados %>%
    arrange(desc(Silhouette)),
  row.names = FALSE
)

# Guardar tabla
write.csv(
  tabla_resultados,
  "resultados_FAMD/comparacion_kmeans.csv",
  row.names = FALSE
)

# Gráfico del codo
ggplot(
  tabla_resultados,
  aes(x = k, y = WSS, color = Dimensiones)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2:8) +
  labs(
    title = "Método del codo para seleccionar k",
    x = "Número de clústeres (k)",
    y = "Suma de cuadrados intraclúster (WSS)",
    color = "Coordenadas FAMD"
  ) +
  theme_minimal(base_size = 12)
##ver q esta separando los clusteres

# ============================================================
# PASO 3. VALIDAR LA SOLUCION DE 3 CLUSTERES
# ============================================================

library(dplyr)
library(ggplot2)

# Recuperar el modelo seleccionado:
# 2 dimensiones y 3 clústeres
modelo_k3 <- modelos[["2 dimensiones_3"]]

# Incorporar el clúster a los datos originales
# IMPORTANTE: se conserva el orden de los registros
df_cluster <- df_revision %>%
  mutate(
    Cluster = factor(modelo_k3$cluster)
  )

# 1. Número y porcentaje de egresados por clúster
tabla_tamanos <- df_cluster %>%
  count(Cluster, name = "N") %>%
  mutate(Porcentaje = round(100 * N / sum(N), 2))

print(tabla_tamanos)

# 2. Relación entre clúster y nivel académico
tabla_nivel <- df_cluster %>%
  count(Cluster, nivel_academico) %>%
  group_by(Cluster) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 2)) %>%
  ungroup()

print(tabla_nivel)

# 3. Perfil numérico de cada clúster
perfil_numerico <- df_cluster %>%
  group_by(Cluster) %>%
  summarise(
    N = n(),
    Promedio_media = mean(promedio_final, na.rm = TRUE),
    Promedio_mediana = median(promedio_final, na.rm = TRUE),
    Creditos_media = mean(creditos_acumulados, na.rm = TRUE),
    Creditos_mediana = median(creditos_acumulados, na.rm = TRUE),
    Duracion_media = mean(duracion_anios, na.rm = TRUE),
    Duracion_mediana = median(duracion_anios, na.rm = TRUE),
    .groups = "drop"
  )

print(perfil_numerico)

# 4. Guardar tablas
write.csv(
  tabla_tamanos,
  "resultados_FAMD/tamanos_clusters.csv",
  row.names = FALSE
)

write.csv(
  tabla_nivel,
  "resultados_FAMD/clusters_por_nivel_academico.csv",
  row.names = FALSE
)

write.csv(
  perfil_numerico,
  "resultados_FAMD/perfil_numerico_clusters.csv",
  row.names = FALSE
)

# 5. Gráfico de composición por nivel académico
ggplot(
  tabla_nivel,
  aes(x = Cluster, y = Porcentaje, fill = nivel_academico)
) +
  geom_col(position = "stack") +
  labs(
    title = "Composición académica de los clústeres",
    x = "Clúster",
    y = "Porcentaje de egresados",
    fill = "Nivel académico"
  ) +
  theme_minimal(base_size = 12)

### 

# ============================================================
# ANALISIS COMPARATIVO: PCA + K-MEANS
# ============================================================

library(FactoMineR)
library(dplyr)

# 1. Seleccionar las variables cuantitativas
datos_pca <- df_revision %>%
  select(
    promedio_final,
    creditos_acumulados,
    duracion_anios
  )

# 2. Verificar datos faltantes
print(colSums(is.na(datos_pca)))

# 3. Ejecutar PCA con variables estandarizadas
modelo_pca <- PCA(
  datos_pca,
  scale.unit = TRUE,
  ncp = 3,
  graph = FALSE
)

# 4. Revisar la varianza explicada
print(modelo_pca$eig)

# 5. Extraer coordenadas factoriales
coord_pca <- as.data.frame(modelo_pca$ind$coord)

# 6. Aplicar K-means con tres clústeres
set.seed(123)

modelo_pca_k3 <- kmeans(
  coord_pca[, 1:3, drop = FALSE],
  centers = 3,
  nstart = 25,
  iter.max = 100
)

# 7. Añadir los clústeres a los registros originales
df_cluster_pca <- df_revision %>%
  mutate(Cluster_PCA = factor(modelo_pca_k3$cluster))

# 8. Comparar clústeres con nivel académico
tabla_pca_nivel <- df_cluster_pca %>%
  count(Cluster_PCA, nivel_academico) %>%
  group_by(Cluster_PCA) %>%
  mutate(
    Porcentaje = round(100 * n / sum(n), 2)
  ) %>%
  ungroup()

print(tabla_pca_nivel)

# 9. Caracterizar los clústeres
perfil_pca <- df_cluster_pca %>%
  group_by(Cluster_PCA) %>%
  summarise(
    N = n(),
    Promedio = round(mean(promedio_final, na.rm = TRUE), 2),
    Creditos = round(mean(creditos_acumulados, na.rm = TRUE), 2),
    Duracion = round(mean(duracion_anios, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(perfil_pca)
###

# Revisar la duración del clúster 2 del PCA

df_cluster_pca %>%
  filter(Cluster_PCA == "2") %>%
  summarise(
    N = n(),
    Minimo = min(duracion_anios, na.rm = TRUE),
    Q1 = quantile(duracion_anios, 0.25, na.rm = TRUE),
    Mediana = median(duracion_anios, na.rm = TRUE),
    Media = mean(duracion_anios, na.rm = TRUE),
    Q3 = quantile(duracion_anios, 0.75, na.rm = TRUE),
    Maximo = max(duracion_anios, na.rm = TRUE),
    Mayores_10_anios = sum(duracion_anios >= 10, na.rm = TRUE)
  )
### 

# ============================================================
# REVISAR LA COHERENCIA TEMPORAL DEL CLUSTER 2
# ============================================================

library(dplyr)

# Resumen por año de primera matrícula y año de egreso
revision_temporal <- df_cluster_pca %>%
  filter(Cluster_PCA == "2") %>%
  group_by(annio_matricula1, annio_egreso) %>%
  summarise(
    N = n(),
    Duracion_media = round(mean(duracion_anios, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(N))

print(revision_temporal, n = 30)

# Distribución por nivel académico
revision_nivel <- df_cluster_pca %>%
  filter(Cluster_PCA == "2") %>%
  count(nivel_academico, sort = TRUE) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 2))

print(revision_nivel)

# Distribución por situación del alumno
revision_situacion <- df_cluster_pca %>%
  filter(Cluster_PCA == "2") %>%
  count(situacion_alumno, sort = TRUE) %>%
  mutate(Porcentaje = round(100 * n / sum(n), 2))

print(revision_situacion)
#revisar posibles inconsistencias

# Revisar coherencia del año de egreso en el clúster 2

df_cluster_pca %>%
  filter(Cluster_PCA == "2") %>%
  summarise(
    Total = n(),
    Con_discrepancia = sum(discrepancia_egreso, na.rm = TRUE),
    Porcentaje_discrepancia = round(
      100 * mean(discrepancia_egreso, na.rm = TRUE), 2
    ),
    Duracion_extrema = sum(duracion_extrema, na.rm = TRUE),
    Porcentaje_duracion_extrema = round(
      100 * mean(duracion_extrema, na.rm = TRUE), 2
    )
  )
####
library(dplyr)

perfil_final_pca <- df_cluster_pca %>%
  group_by(Cluster_PCA) %>%
  summarise(
    Total = n(),
    Porcentaje = round(n() / nrow(df_cluster_pca) * 100, 2),
    
    Promedio_media = round(mean(promedio_final, na.rm = TRUE), 2),
    Promedio_mediana = round(median(promedio_final, na.rm = TRUE), 2),
    
    Creditos_media = round(mean(creditos_acumulados, na.rm = TRUE), 2),
    Creditos_mediana = round(median(creditos_acumulados, na.rm = TRUE), 2),
    
    Duracion_media = round(mean(duracion_anios, na.rm = TRUE), 2),
    Duracion_mediana = round(median(duracion_anios, na.rm = TRUE), 2),
    
    .groups = "drop"
  )

perfil_final_pca
##comprobar los indices se siluesta y deivis
perfil_final_pca %>%
  select(
    Cluster_PCA,
    Creditos_mediana,
    Duracion_media,
    Duracion_mediana
  )


##########graficas


library(cluster)
library(ggplot2)

# Coordenadas de los tres componentes principales
coord <- as.matrix(modelo_pca$ind$coord[, 1:3])

# Muestra fija para calcular la silueta
set.seed(123)
id_muestra <- sample(seq_len(nrow(coord)),
                     min(2000, nrow(coord)))

coord_muestra <- coord[id_muestra, , drop = FALSE]
dist_muestra <- dist(coord_muestra)

# Evaluar K-means para k = 2, 3, 4 y 5
resultados <- list()

for (k in 2:5) {
  
  set.seed(123)
  modelo <- kmeans(
    coord,
    centers = k,
    nstart = 25,
    iter.max = 100
  )
  
  sil <- silhouette(
    modelo$cluster[id_muestra],
    dist_muestra
  )
  
  # Índice Davies-Bouldin
  centros <- modelo$centers
  dist_centros <- as.matrix(dist(centros))
  
  dispersion <- sapply(1:k, function(j) {
    miembros <- coord[modelo$cluster == j, , drop = FALSE]
    centro <- centros[j, ]
    mean(sqrt(rowSums(
      sweep(miembros, 2, centro, "-")^2
    )))
  })
  
  razon <- matrix(0, k, k)
  
  for (i in 1:k) {
    for (j in 1:k) {
      if (i != j) {
        razon[i, j] <-
          (dispersion[i] + dispersion[j]) /
          dist_centros[i, j]
      }
    }
  }
  
  resultados[[as.character(k)]] <- data.frame(
    k = k,
    Silueta_media = mean(sil[, "sil_width"]),
    Davies_Bouldin = mean(apply(razon, 1, max)),
    WSS = modelo$tot.withinss
  )
}

tabla_validacion <- do.call(rbind, resultados)
print(tabla_validacion)

# Gráfica 1: silueta
g_silueta <- ggplot(
  tabla_validacion,
  aes(x = k, y = Silueta_media)
) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "Validación del número de clústeres",
    x = "Número de clústeres (k)",
    y = "Silueta media"
  ) +
  theme_minimal(base_size = 12)

print(g_silueta)

# Gráfica 2: Davies-Bouldin
g_db <- ggplot(
  tabla_validacion,
  aes(x = k, y = Davies_Bouldin)
) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "Índice Davies-Bouldin por número de clústeres",
    x = "Número de clústeres (k)",
    y = "Davies-Bouldin"
  ) +
  theme_minimal(base_size = 12)

print(g_db)

###

library(ggplot2)
library(tidyr)
library(dplyr)

# Gráfica 1. Índice de silueta
g_silueta <- ggplot(
  tabla_validacion,
  aes(x = k, y = Silueta_media)
) +
  geom_line(linewidth = 0.8, color = "#2878B5") +
  geom_point(size = 3, color = "#2878B5") +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "Validación del número de clústeres",
    subtitle = "Índice de silueta: valores mayores son preferibles",
    x = "Número de clústeres (k)",
    y = "Silueta media"
  ) +
  theme_minimal(base_size = 12)

print(g_silueta)


# Gráfica 2. Índice Davies-Bouldin
g_db <- ggplot(
  tabla_validacion,
  aes(x = k, y = Davies_Bouldin)
) +
  geom_line(linewidth = 0.8, color = "#D97732") +
  geom_point(size = 3, color = "#D97732") +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "Índice Davies-Bouldin por número de clústeres",
    x = "Número de clústeres (k)",
    y = "Davies-Bouldin"
  ) +
  theme_minimal(base_size = 12)

print(g_db)

#
print(tabla_validacion)
###

library(cluster)

set.seed(123)

# Coordenadas de los tres componentes principales
coord <- as.matrix(modelo_pca$ind$coord[, 1:3])

# Ajustar K-means con tres clústeres
set.seed(123)
modelo_k3_validado <- kmeans(
  coord,
  centers = 3,
  nstart = 25,
  iter.max = 100
)

# Calcular silueta en una muestra de 2000 registros
set.seed(123)
id_muestra <- sample(
  seq_len(nrow(coord)),
  min(2000, nrow(coord))
)

sil_k3 <- silhouette(
  modelo_k3_validado$cluster[id_muestra],
  dist(coord[id_muestra, , drop = FALSE])
)

# Gráfica de silueta
plot(
  sil_k3,
  main = "Análisis de silueta: K-means con 3 clústeres",
  col = c("#4E79A7", "#F28E2B", "#59A14F"),
  border = NA
)
##comprobar la estabilidad 

# Coordenadas de los tres componentes principales
coord <- as.matrix(modelo_pca$ind$coord[, 1:3])

# Primera ejecución
set.seed(123)
km_a <- kmeans(coord, centers = 3, nstart = 25, iter.max = 100)

# Segunda ejecución con otra semilla
set.seed(456)
km_b <- kmeans(coord, centers = 3, nstart = 25, iter.max = 100)

# Comparar las asignaciones, aunque cambie el número
# con el que se identifica cada clúster
install.packages("mclust")
library(mclust)

ARI <- adjustedRandIndex(km_a$cluster, km_b$cluster)
cat("Índice de estabilidad ARI:", round(ARI, 4), "\n")
###

library(dplyr)

df_cluster_pca %>%
  group_by(Cluster_PCA, nivel_academico) %>%
  summarise(
    Registros = n(),
    .groups = "drop"
  ) %>%
  group_by(Cluster_PCA) %>%
  mutate(
    Porcentaje_en_cluster = round(
      100 * Registros / sum(Registros), 2
    )
  ) %>%
  ungroup() %>%
  arrange(Cluster_PCA, desc(Registros))

### guaradar las gtaficas

library(ggplot2)
library(dplyr)

# Crear carpeta para las figuras
dir.create("graficas_articulo", showWarnings = FALSE)

# 1. Gráfica del índice de silueta
g_silueta <- ggplot(
  tabla_validacion,
  aes(x = k, y = Silueta_media)
) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "Comparación del índice de silueta",
    x = "Número de clústeres (k)",
    y = "Silueta media"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "graficas_articulo/01_silueta.png",
  g_silueta, width = 8, height = 5, dpi = 300
)

# 2. Gráfica Davies-Bouldin
g_db <- ggplot(
  tabla_validacion,
  aes(x = k, y = Davies_Bouldin)
) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "Comparación del índice Davies-Bouldin",
    x = "Número de clústeres (k)",
    y = "Índice Davies-Bouldin"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "graficas_articulo/02_davies_bouldin.png",
  g_db, width = 8, height = 5, dpi = 300
)

# 3. Distribución de los clústeres en los dos primeros componentes
coord <- as.data.frame(modelo_pca$ind$coord[, 1:2])
coord$Cluster_PCA <- df_cluster_pca$Cluster_PCA

set.seed(123)
ids <- sample(seq_len(nrow(coord)), min(3000, nrow(coord)))
datos_grafico <- coord[ids, ]

g_pca <- ggplot(
  datos_grafico,
  aes(x = Dim.1, y = Dim.2, color = Cluster_PCA)
) +
  geom_point(alpha = 0.6, size = 1.3) +
  labs(
    title = "Perfiles de egresados identificados mediante K-means",
    x = "Componente principal 1 (60,80 %)",
    y = "Componente principal 2 (23,99 %)",
    color = "Clúster"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "graficas_articulo/03_distribucion_pca.png",
  g_pca, width = 8, height = 6, dpi = 300
)

# Mostrar las gráficas
print(g_silueta)
print(g_db)
print(g_pca)

# Confirmar archivos generados
list.files("graficas_articulo")


install.packages("isotree")
### entrenar modelo 

library(isotree)
library(dplyr)

# Variables para Isolation Forest
datos_if <- df_revision %>%
  select(
    promedio_final,
    creditos_acumulados,
    duracion_anios
  )

# Revisar dimensiones y valores faltantes
cat("Número de registros:", nrow(datos_if), "\n")
cat("Número de variables:", ncol(datos_if), "\n")
cat("Valores faltantes:", sum(is.na(datos_if)), "\n")

summary(datos_if)
### 
# Fijar semilla para reproducibilidad
set.seed(123)

# Entrenar Isolation Forest
modelo_if <- isolation.forest(
  datos_if,
  ntrees = 300,
  sample_size = min(256, nrow(datos_if)),
  ndim = 1
)

# Calcular puntuaciones de anomalía
puntuacion_if <- predict(
  modelo_if,
  datos_if,
  type = "score"
)

# Revisar las puntuaciones
summary(puntuacion_if)

# Examinar las primeras 10 puntuaciones
head(puntuacion_if, 10)
## identificr registros inisuales

# Umbral exploratorio: percentil 95
umbral_if <- as.numeric(
  quantile(puntuacion_if, probs = 0.95, na.rm = TRUE)
)

cat("Umbral de anomalía (percentil 95):",
    round(umbral_if, 4), "\n")

# Incorporar puntuaciones y clasificación al conjunto original
df_revision$Puntuacion_IF <- as.numeric(puntuacion_if)

df_revision$Anomalia_IF <- ifelse(
  df_revision$Puntuacion_IF >= umbral_if,
  "Anómalo",
  "No anómalo"
)

# Resumen de resultados
table(df_revision$Anomalia_IF)

# Porcentajes
prop.table(table(df_revision$Anomalia_IF)) * 100

##caracterizar registros 

library(dplyr)

# Resumen de anomalías por nivel académico
resumen_if_nivel <- df_revision %>%
  group_by(nivel_academico, Anomalia_IF) %>%
  summarise(
    cantidad = n(),
    promedio = round(mean(promedio_final, na.rm = TRUE), 2),
    creditos_mediana = median(creditos_acumulados, na.rm = TRUE),
    duracion_mediana = median(duracion_anios, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_if_nivel)

# Distribución de los registros señalados por nivel
df_revision %>%
  filter(Anomalia_IF == "Anómalo") %>%
  count(nivel_academico, sort = TRUE) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
  print()

# Revisar los 15 registros con mayor puntuación
df_revision %>%
  arrange(desc(Puntuacion_IF)) %>%
  select(
    nivel_academico,
    facultad,
    programa_estudios,
    situacion_alumno,
    promedio_final,
    creditos_acumulados,
    duracion_anios,
    Puntuacion_IF,
    Anomalia_IF
  ) %>%
  head(15) %>%
  print()
#

df_revision %>%
  filter(Anomalia_IF == "Anómalo") %>%
  dplyr::count(nivel_academico) %>%
  arrange(desc(n)) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )
##15 coasos con mayor puntuacion

casos_if <- df_revision %>%
  arrange(desc(Puntuacion_IF)) %>%
  select(
    nivel_academico,
    facultad,
    programa_estudios,
    situacion_alumno,
    promedio_final,
    creditos_acumulados,
    duracion_anios,
    Puntuacion_IF
  ) %>%
  head(15)

print(as.data.frame(casos_if), row.names = FALSE)

# 

# 1. Verificar que ambos conjuntos tengan el mismo número de filas
stopifnot(nrow(df_revision) == nrow(df_cluster_pca))

# 2. Incorporar el clúster al conjunto de datos
df_revision$Cluster_PCA <- df_cluster_pca$Cluster_PCA

# 3. Cruzar clústeres y anomalías
table(
  df_revision$Cluster_PCA,
  df_revision$Anomalia_IF
)

# 4. Calcular el porcentaje de anomalías dentro de cada clúster
df_revision %>%
  group_by(Cluster_PCA) %>%
  summarise(
    total = n(),
    anomalos = sum(Anomalia_IF == "Anómalo"),
    porcentaje_anomalo = round(
      mean(Anomalia_IF == "Anómalo") * 100, 2
    ),
    .groups = "drop"
  )
###
list.files(
  path = "C:/Users/Lenovo/Documents",
  pattern = "EGRESADOS.*\\.csv$",
  recursive = TRUE,
  ignore.case = TRUE,
  full.names = TRUE
)

list.files(
  "C:/Users/Lenovo/Documents/ESTUDIANTES/salidas",
  recursive = TRUE
)
