# Caricamento dei pacchetti necessari
install.packages("ggplot2")
install.packages("GGally")
install.packages("corrplot")
install.packages("dplyr")
install.packages("readr")

library(ggplot2)
library(GGally)     # per ggpairs
library(corrplot)   # per visualizzare la matrice di correlazione
library(dplyr)      # per manipolazione dati
library(readr)      # per leggere file CSV


# Caricamento del dataset
dati <- read_csv("data/DataSet_gruppo13.csv")


# Statistiche descrittive (media, mediana, dev. standard, min, max)
statistiche_descrittive <- dati %>%
  summarise(across(everything(), list(
    media = mean,
    mediana = median,
    dev_standard = sd,
    minimo = min,
    massimo = max
  )))

# Trasposizione per migliorare la leggibilità a video
print(t(statistiche_descrittive))


# Istogrammi per ogni variabile
for (variabile in names(dati)) {
  print(
    ggplot(dati, aes_string(variabile)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(title = paste("Istogramma di", variabile),
           x = variabile,
           y = "Frequenza")
  )
}


# Boxplot per ogni variabile
for (variabile in names(dati)) {
  print(
    ggplot(dati, aes_string(y = variabile)) +
      geom_boxplot(fill = "tomato", color = "black") +
      theme_minimal() +
      labs(title = paste("Boxplot di", variabile),
           y = variabile)
  )
}


# Calcolo della matrice di correlazione
matrice_correlazioni <- cor(dati)
print(matrice_correlazioni)

# Visualizzazione grafica della matrice di correlazione (heatmap)
corrplot(matrice_correlazioni, method = "color", type = "upper",
         col = colorRampPalette(c("white", "blue"))(100),
         addCoef.col = "black", number.cex = 0.7, tl.col = "black", 
         mar = c(0, 1, 0, 0))

# Aggiunta del titolo in alto a sinistra (lato 3 = in alto)
mtext("Matrice di correlazione", side = 3, adj = 0, line = 1.5, cex = 1.2, font = 2)


# Scatter plot: ogni variabile indipendente vs y_VideoQuality
variabili_indipendenti <- names(dati)[names(dati) != "y_VideoQuality"]
for (x in variabili_indipendenti) {
  print(
    ggplot(dati, aes_string(x = x, y = "y_VideoQuality")) +
      geom_point(color = "darkgreen") +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_minimal() +
      labs(title = paste("Scatter plot:", x, "vs y_VideoQuality"),
           x = x, y = "y_VideoQuality")
  )
}
