# Caricamento del dataset
dati <- read.csv("data/DataSet_gruppo13.csv")  # Modifica il percorso se necessario


# Modello di regressione multipla
modello_multiplo <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x3_TIME +
                         x4_MP + x5_CROP + x6_FOCAL + x7_PixDensity,
                       data = dati)


# Calcolo R² e R² aggiustato
r2 <- summary(modello_multiplo)$r.squared
r2_aggiustato <- summary(modello_multiplo)$adj.r.squared


cat("R² =", round(r2, 3), "\n")
cat("R² aggiustato =", round(r2_aggiustato, 3), "\n")


# Residui
residui <- resid(modello_multiplo)
valori_predetti <- fitted(modello_multiplo)


# Q-Q Plot dei residui
qqnorm(residui, main = "Q-Q Plot dei residui")
qqline(residui, col = "red", lwd = 2)


# Residui vs Valori Predetti
plot(valori_predetti, residui,
     main = "Residui vs Valori Predetti",
     xlab = "Valori Predetti",
     ylab = "Residui",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lwd = 2)


# Istogramma dei residui
hist(residui,
     breaks = 20,
     main = "Istogramma dei residui",
     xlab = "Residui",
     col = "skyblue", border = "white")

