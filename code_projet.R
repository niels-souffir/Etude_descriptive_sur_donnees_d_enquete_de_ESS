# --- CHARGEMENT DES PACKAGES ET DONNEES ---
library(data.table)

data_path <- "ESS9e03_2.csv"  # Adapter le chemin

# Importation avec fread
df <- fread(file = data_path,
            sep = ",",
            dec = ".",
            header = TRUE,
            encoding = "UTF-8")

# --- NETTOYAGE ET SELECTION ---
cols_to_keep <- c("idno", "pspwght", "trstplt", "trstplc",
                  "gndr", "agea", "edulvlb", "hinctnta",
                  "polintr", "cntry")

df_sub <- df[, ..cols_to_keep]

df_fr <- df_sub[cntry == "FR"]

# Gestion des valeurs manquantes et recodage
df_fr[trstplt > 10, trstplt := NA]
df_fr[agea > 100, agea := NA]
df_fr[hinctnta > 10, hinctnta := NA]
df_fr[polintr > 4, polintr := NA]

# Recodage du Genre et Education
df_fr$gndr <- factor(df_fr$gndr,
                     levels = c(1, 2),
                     labels = c("Homme", "Femme"))

df_fr$educ_cat <- "Moyen"
df_fr$educ_cat[df_fr$edulvlb < 300] <- "Faible"
df_fr$educ_cat[df_fr$edulvlb >= 600] <- "Eleve"

df_fr$educ_cat <- factor(df_fr$educ_cat,
                         levels = c("Faible", "Moyen", "Eleve"))

# Categories d’age
df_fr$age_cat <- cut(df_fr$agea,
                     breaks = c(15, 25, 45, 65, 100),
                     labels = c("15-24", "25-44", "45-64", "65+"),
                     right = FALSE)

df_fr <- df_fr[!is.na(trstplt)]

# --- ANALYSE UNIVARIEE ---
mu <- mean(df_fr$trstplt, na.rm = TRUE)

hist(df_fr$trstplt,
     breaks = 10,
     col = "#3498db",
     main = "Distribution de la confiance politique en France",
     xlab = "Note de confiance (0-10)",
     ylab = "Frequence")

abline(v = mu, col = "red", lwd = 3)

# --- ANALYSE BIVARIEE ---
# Education
boxplot(trstplt ~ educ_cat,
        data = df_fr,
        main = "Confiance selon le niveau d'education",
        col = c("#e74c3c", "#f1c40f", "#2ecc71"))

# Revenu (Line plot + Boxplot)
res_revenu <- df_fr[, .(Moyenne = mean(trstplt, na.rm = TRUE)), by = hinctnta]
res_revenu <- res_revenu[order(hinctnta)]

plot(res_revenu$hinctnta,
     res_revenu$Moyenne,
     type = "b",
     pch = 16,
     col = "#2980b9",
     lwd = 2,
     main = "Confiance moyenne selon le revenu")

grid()

boxplot(trstplt ~ hinctnta,
        data = df_fr,
        main = "Confiance politique selon le decile de revenu",
        col = "#3498db")

# Age (Courbe en U)
res_age <- df_fr[, .(Moyenne = mean(trstplt, na.rm = TRUE)), by = age_cat]
res_age <- res_age[order(age_cat)]

plot(as.numeric(res_age$age_cat),
     res_age$Moyenne,
     type = "b",
     pch = 19,
     col = "#8e44ad",
     lwd = 2,
     xaxt = "n",
     main = "Confiance politique selon l'age")

axis(1, at = 1:4, labels = res_age$age_cat)
grid()

# Interet politique
res_pol <- tapply(df_fr$trstplt,
                  df_fr$polintr,
                  mean,
                  na.rm = TRUE)

barplot(res_pol,
        main = "Confiance selon l'interet politique",
        col = "#9b59b6")

# --- Variable Confiance sociale elevee a partir de trstplc (confiance dans la police) ---
df_fr$trust_social <- ifelse(df_fr$trstplc >= 7, "Eleve", "Faible")

# Boxplot : confiance politique selon confiance sociale
boxplot(trstplt ~ trust_social,
        data = df_fr,
        main = "Confiance politique selon la confiance en la police",
        xlab = "Confiance sociale",
        ylab = "Note de confiance politique (0-10)",
        col = c("#e67e22", "#3498db"))

# --- SYNTHESE STATISTIQUE ---
# Correlations
vars_num <- df_fr[, .(trstplt, agea, hinctnta, polintr)]
print(cor(vars_num, use = "complete.obs"))

# Regression
modele <- lm(trstplt ~ agea + gndr + hinctnta + educ_cat, data = df_fr)
summary(modele)

# --- COMPARAISON EUROPEENNE ---
target_countries <- c("FR", "DE", "SE")

df_comp <- df[cntry %in% target_countries]
df_comp[trstplt > 10, trstplt := NA]

tab_comp <- df_comp[, .(Moyenne = mean(trstplt, na.rm = TRUE)), by = cntry]

couleurs <- rep("#95a5a6", nrow(tab_comp))
couleurs[tab_comp$cntry == "FR"] <- "#c0392b"

barplot(tab_comp$Moyenne,
        names.arg = tab_comp$cntry,
        col = couleurs,
        main = "Comparaison Europeenne")