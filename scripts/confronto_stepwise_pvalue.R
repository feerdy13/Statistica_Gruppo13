# Caricamento pacchetti
install.packages("MASS")

library(MASS)


# Caricamento del dataset
dati <- read.csv("data/DataSet_gruppo13.csv")


# Modello completo con tutte le variabili
modello_completo <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x3_TIME +
                         x4_MP + x5_CROP + x6_FOCAL + x7_PixDensity,
                       data = dati)


# Modello stepwise (AIC)
modello_stepwise <- step(modello_completo, direction = "backward", trace = FALSE)

# Modello basato sui p-value (solo variabili significative)
modello_pvalue <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x5_CROP, data = dati)


# Sommario del modello stepwise
summary(modello_stepwise)

# Sommario del modello pvalue
summary(modello_pvalue)


# Metriche Stepwise
r2_step <- summary(modello_stepwise)$r.squared
r2aggiustato_step <- summary(modello_stepwise)$adj.r.squared
aic_step <- AIC(modello_stepwise)
bic_step <- BIC(modello_stepwise)

# Metriche P-value
r2_pval <- summary(modello_pvalue)$r.squared
r2ggiustato_pval <- summary(modello_pvalue)$adj.r.squared
aic_pval <- AIC(modello_pvalue)
bic_pval <- BIC(modello_pvalue)


# 🖨️ Confronto dei modelli
cat("Modello Stepwise:\n")
cat("R² =", round(r2_step, 3), "\n")
cat("R² aggiustato =", round(r2aggiustato_step, 3), "\n")
cat("AIC =", round(aic_step, 2), "\n")
cat("BIC =", round(bic_step, 2), "\n\n")

cat("Modello P-value (ridotto):\n")
cat("R² =", round(r2_pval, 3), "\n")
cat("R² aggiustato =", round(r2ggiustato_pval, 3), "\n")
cat("AIC =", round(aic_pval, 2), "\n")
cat("BIC =", round(bic_pval, 2), "\n")
