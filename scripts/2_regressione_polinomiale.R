# Caricamento del dataset
dati <- read.csv("data/DataSet_gruppo13.csv")

# Elenco delle variabili indipendenti
variabili_indipendenti <- c("x1_ISO", "x2_FRatio", "x3_TIME", "x4_MP", 
                            "x5_CROP", "x6_FOCAL", "x7_PixDensity")

# Inizializzazione della tabella dei risultati
tabella_risultati <- data.frame(
  Variabile = character(),
  R_quadro = numeric(),
  Coefficiente_B1 = numeric(),
  Valore_p = numeric(),
  Confidenza_95_basso = numeric(),
  Confidenza_95_alto = numeric(),
  stringsAsFactors = FALSE
)

# Ciclo su ogni variabile per calcolare regressione e disegnare grafico
for (variabile in variabili_indipendenti) {
  
  # Modello di regressione lineare semplice
  formula <- as.formula(paste("y_VideoQuality ~", variabile))
  modello <- lm(formula, data = dati)
  
  # Estrazione statistiche
  r_quadro <- summary(modello)$r.squared
  coefficiente <- summary(modello)$coefficients[2, 1]
  p_value <- summary(modello)$coefficients[2, 4]
  conf_basso <- confint(modello)[2, 1]
  conf_alto <- confint(modello)[2, 2]
  
  # Aggiunta dei risultati alla tabella
  tabella_risultati <- rbind(tabella_risultati, data.frame(
    Variabile = variabile,
    R_quadro = round(r_quadro, 3),
    Coefficiente_B1 = round(coefficiente, 3),
    Valore_p = round(p_value, 4),
    Confidenza_95_basso = round(conf_basso, 3),
    Confidenza_95_alto = round(conf_alto, 3)
  ))
  
  # Parte grafica
  x_valori <- dati[[variabile]]
  x_seq <- seq(min(x_valori), max(x_valori), length.out = 100)
  dati_nuovi <- data.frame(x = x_seq)
  colnames(dati_nuovi) <- variabile
  
  previsioni <- predict(modello, newdata = dati_nuovi, interval = "confidence")
  
  plot(x_valori, dati$y_VideoQuality,
       main = paste("y_VideoQuality ~", variabile),
       xlab = variabile, ylab = "Qualità video (y_VideoQuality)",
       pch = 19, col = "steelblue")
  
  lines(x_seq, previsioni[, "fit"], col = "red", lwd = 2)
  lines(x_seq, previsioni[, "lwr"], col = "darkorange", lty = 2)
  lines(x_seq, previsioni[, "upr"], col = "darkorange", lty = 2)
  
  legend("bottomright", legend = c("Retta di regressione", "Intervallo di confidenza al 95%"),
         col = c("red", "darkorange"), lty = c(1, 2), bty = "n", cex = 0.7)
}
