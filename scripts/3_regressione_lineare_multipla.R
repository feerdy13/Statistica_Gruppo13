# Caricamento del dataset
dati <- read.csv("data/DataSet_gruppo13.csv")  # Modifica percorso se necessario

# Costruzione del modello di regressione multipla
modello_multiplo <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x3_TIME + 
                         x4_MP + x5_CROP + x6_FOCAL + x7_PixDensity, 
                       data = dati)

# Sommario del modello (visualizzazione completa in console)
summary(modello_multiplo)

# Estrazione dei coefficienti e degli intervalli di confidenza
coefficenti <- summary(modello_multiplo)$coefficients
intervalli_confidenza <- confint(modello_multiplo)

# Creazione della tabella riepilogativa dei risultati
tabella_risultati <- data.frame(
  Variabile = rownames(coefficenti),
  Coefficiente = round(coefficenti[, "Estimate"], 3),
  Valore_p = round(coefficenti[, "Pr(>|t|)"], 4),
  Confidenza_95_basso = round(intervalli_confidenza[, 1], 3),
  Confidenza_95_alto = round(intervalli_confidenza[, 2], 3),
  stringsAsFactors = FALSE
)

# Rimuove l'intercetta dalla tabella
tabella_risultati <- tabella_risultati[tabella_risultati$Variabile != "(Intercept)", ]

# Imposta indici numerici sulle righe (1, 2, 3, ...) al posto dei nomi
rownames(tabella_risultati) <- seq_len(nrow(tabella_risultati))

# Calcolo di R² e R² aggiustato
r_quadro <- summary(modello_multiplo)$r.squared
r_quadro_aggiustato <- summary(modello_multiplo)$adj.r.squared

#  Stampa dei risultati
cat("R² =", round(r_quadro, 3), "\n")
cat("R² aggiustato =", round(r_quadro_aggiustato, 3), "\n\n")
print(tabella_risultati)
