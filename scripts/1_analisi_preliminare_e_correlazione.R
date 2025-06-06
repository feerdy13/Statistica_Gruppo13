# Caricamento pacchetti necessari
library(ggplot2)
library(GGally)     # per ggpairs (FACOLTATIVO)
library(corrplot)   # per visualizzare la matrice di correlazione
library(dplyr)      # per manipolazione dati
library(readr)      # per leggere CSV

# 1. Caricamento del dataset
data <- read_csv("data/DataSet_gruppo13.csv")

# 2. Statistiche descrittive personalizzate
statistiche <- data %>%
  summarise(across(everything(), list(
    media = mean,
    mediana = median,
    dev_std = sd,
    minimo = min,
    massimo = max
  )))

# Trasposizione per migliorarne la leggibilità
print(t(statistiche))

# 3. Istogrammi per ogni variabile
for (var in names(data)) {
  print(
    ggplot(data, aes_string(var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(title = paste("Istogramma di", var))
  )
}

# 4. Boxplot per ogni variabile
for (var in names(data)) {
  print(
    ggplot(data, aes_string(y = var)) +
      geom_boxplot(fill = "tomato", color = "black") +
      theme_minimal() +
      labs(title = paste("Boxplot di", var), y = var)
  )
}

# 5. Scatter plot: ogni variabile indipendente vs y_VideoQuality
indipendenti <- names(data)[names(data) != "y_VideoQuality"]
for (x in indipendenti) {
  print(
    ggplot(data, aes_string(x = x, y = "y_VideoQuality")) +
      geom_point(color = "darkgreen") +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = paste("Scatter plot:", x, "vs y_VideoQuality"))
  )
}

# 6. Matrice di correlazione
cor_matrix <- cor(data)
print(cor_matrix)

# 7. Visualizzazione della matrice di correlazione (heatmap)
corrplot(cor_matrix, method = "color", type = "upper",
         col = colorRampPalette(c("white", "blue"))(100),
         addCoef.col = "black", number.cex = 0.7, tl.col = "black")
