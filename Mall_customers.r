# Instalamos y cargamos los paquetes necesarios
install.packages(c("tidyverse", "cluster", "factoextra"))
library(tidyverse)
library(cluster)
library(factoextra)

# 1. Cargar el dataset
df <- read.csv("Mall_Customers.csv")

# 2. Exploración y limpieza de datos
glimpse(df)  # Ver estructura
summary(df)  # Resumen estadístico
df <- df %>%
  rename(Gender = Genre, Income = `Annual Income (k$)`, Score = `Spending Score (1-100)`) %>%
  mutate(Gender = ifelse(Gender == "Male", 1, 0))  # Codificar género como 1 (Hombre) y 0 (Mujer)

# 3. Normalización de variables numéricas
df_scaled <- df %>%
  select(Age, Income, Score) %>%
  scale()

# 4. Aplicar K-Means
set.seed(123)  # Asegurar reproducibilidad
wss <- map_dbl(2:6, function(k) kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss)

# Gráfico del método del codo
elbow_plot <- data.frame(k = 2:6, wss = wss) %>%
  ggplot(aes(k, wss)) +
  geom_line() + geom_point() +
  ggtitle("Método del Codo") +
  theme_minimal()
print(elbow_plot)

# Seleccionamos el número óptimo de clusters (suponiendo k=5)
kmeans_result <- kmeans(df_scaled, centers = 5, nstart = 25)
df$Cluster_KMeans <- as.factor(kmeans_result$cluster)

# 5. Clustering jerárquico
d <- dist(df_scaled, method = "euclidean")
hc <- hclust(d, method = "ward.D")
plot(hc, main = "Dendrograma de Clustering Jerárquico")

# Cortamos en 5 clusters
df$Cluster_HC <- as.factor(cutree(hc, k = 5))

# 6. Evaluación con coeficiente de silueta
sil_kmeans <- silhouette(kmeans_result$cluster, d)
sil_hc <- silhouette(cutree(hc, k = 5), d)

# Promedio de silueta
cat("Silueta K-Means:", mean(sil_kmeans[, 3]), "\n")
cat("Silueta Jerárquico:", mean(sil_hc[, 3]), "\n")

# 7. Visualización de clusters
ggplot(df, aes(Income, Score, color = Cluster_KMeans)) +
  geom_point() +
  ggtitle("Clusters según K-Means") +
  theme_minimal()

ggplot(df, aes(Income, Score, color = Cluster_HC)) +
  geom_point() +
  ggtitle("Clusters según Clustering Jerárquico") +
  theme_minimal()

# 8. Análisis descriptivo de segmentos
df %>%
  group_by(Cluster_KMeans) %>%
  summarise(Age = mean(Age), Income = mean(Income), Score = mean(Score))

df %>%
  group_by(Cluster_HC) %>%
  summarise(Age = mean(Age), Income = mean(Income), Score = mean(Score))