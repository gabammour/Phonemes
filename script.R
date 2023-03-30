{
  library(caret)
  library(klaR)
  library(ggplot2)
  library(FactoMineR)
  library(factoextra)
  library(NbClust)
  library(JLutils)
  library(DiscriMiner)
  library(dplyr)
}

# Importation des données

base <- read.csv2("phoneme.csv", sep = ",")
summary(base)
base <- base %>%
  select(ncol(base), everything()) %>%  # sélectionner la dernière colonne et toutes les autres colonnes
  relocate(c(names(base)[ncol(base)], names(base)[-ncol(base)]))
base <- base[,-c(3)]
write.csv2(base, "/Users/gabammour/Desktop/my_desk /S2/Latentes/Projet-20230216/base.csv", append = FALSE)
df <- read.csv2("base.csv")
str(base)

# 	Découpage du jeu de données en un échantillon d’apprentissage et test suivant la variable locuteur. 

df <- df %>% select(g, everything())
df$g = as.factor(df$g)
df <- df %>% mutate(across(starts_with("x."), as.numeric))

# Création de train et test
df_train <- df[grepl("train", df$speaker), ]
df_train  = df_train[,-c(2)]
df_test <- df[grepl("test", df$speaker), ]
df_test  = df_test[,-c(2)]
df  = df[,-c(2)]

# ACP

# Classification par hclust






