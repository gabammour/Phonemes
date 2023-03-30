#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Chargement des packages----
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
library(dplyr)
library(tidyverse)
library(ggplot2)
library(parallel)
library(FactoMineR)
library(factoextra)


#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Chargement BDD----
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
df <- read.csv2("phoneme.csv", sep= ",")

df <- df %>%
  select(ncol(df), everything()) %>%  # sélectionner la dernière colonne et toutes les autres colonnes
  relocate(c(names(df)[ncol(df)], names(df)[-ncol(df)]))

df <- df %>% select(g, everything())
df$g = as.factor(df$g)
df <- df %>% mutate(across(starts_with("x."), as.numeric))
df <- df[, -which(names(df) == "row.names")]

# Création de train et test
df_train <- df[grepl("train", df$speaker), ]
df_train  = df_train[,-c(2)]
df_test <- df[grepl("test", df$speaker), ]
df_test  = df_test[,-c(2)]
df  = df[,-c(2)]

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Statistiques descrpitves
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

phoneme_groups <- split(df, df$g)


# Fonction pour calculer les statistiques descriptives
descriptive_stats <- function(data) {
  # Sélectionner uniquement les colonnes numériques (supposons que les colonnes "x.1" à "x.256" sont numériques)
  numeric_data <- data[, grepl("^x\\.", colnames(data))]
  
  summary_stats <- data.frame(
    mean = colMeans(numeric_data),
    median = apply(numeric_data, 2, median),
    sd = apply(numeric_data, 2, sd),
    min = apply(numeric_data, 2, min),
    max = apply(numeric_data, 2, max)
  )
  return(summary_stats)
}

# Appliquer la fonction aux groupes de phonèmes
phoneme_stats <- lapply(phoneme_groups, descriptive_stats)


# Sélectionnez uniquement les colonnes numériques
#numeric_data <- df[, grepl("^x\\.", colnames(df))] 
# Ajoutez une colonne pour le phonème
#numeric_data$g <- df$g

# Convertissez les données en format long
#long_data <- reshape2::melt(numeric_data, id.vars = "g", variable.name = "variable", value.name = "value")
# Convertir la liste en data.frame
phoneme_stats_df <- do.call(rbind, phoneme_stats)

# Ajouter une colonne avec les noms des phonèmes
phoneme_stats_df$phoneme <- rep(names(phoneme_groups), each = nrow(phoneme_stats[[1]]))

# Ajouter une colonne avec les numéros de variable (périodogramme)
phoneme_stats_df$variable <- factor(rep(1:nrow(phoneme_stats[[1]]), length(unique(phoneme_stats_df$phoneme))), levels = 1:nrow(phoneme_stats[[1]]))

# Convertir les données en format long
long_phoneme_stats_df <- reshape2::melt(phoneme_stats_df, id.vars = c("phoneme", "variable"), variable.name = "statistic", value.name = "value")

# Ajouter une colonne avec les numéros de variable (périodogramme)
mean_data <- long_phoneme_stats_df[long_phoneme_stats_df$statistic == "mean", ]


# Extraire les moyennes de long_phoneme_stats_df
mean_data <- long_phoneme_stats_df[long_phoneme_stats_df$statistic == "mean",]

# Créer une colonne "variable" avec les numéros de variable (périodogramme) pour chaque phonème
mean_data$variable <- factor(rep(1:nrow(phoneme_stats[[1]]), length(unique(mean_data$phoneme))), levels = 1:nrow(phoneme_stats[[1]]))


# Ajouter une colonne avec les numéros de variable (périodogramme)
ggplot(mean_data, aes(x = variable, y = value, color = phoneme, group = phoneme)) +
  geom_line() +
  scale_x_discrete(breaks = seq(1, nrow(phoneme_stats[[1]]), by = 50), labels = seq(1, nrow(phoneme_stats[[1]]), by = 50)) +
  theme_bw() +
  labs(title = "Mean Log-Periodogram Values by Phoneme and Variable",
       x = "Variable (Periodogram)",
       y = "Mean Value") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# ACP----
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
res.pca <- PCA (df,scale.unit=TRUE,quali.sup=1,graph=FALSE)
res<-barplot(res.pca$eig[1:10],xlab="Dim.",ylab="Percentage of variance")
plot(res.pca,choix="var")

my_colors <- colorRampPalette(c("black", "black", "black", "black", "black"))(length(unique(df$g)))
FactoMineR::plot.PCA(res.pca,choix="ind",habillage=1, label="quali", col.quali = my_colors)

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Dendrogramme----
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
data.cr<-scale(df_train[,-1],center=TRUE,scale=TRUE)

data.dist<-dist(data.cr)
data.hca<-hclust(data.dist,method="ward.D2")
par(mfrow=c(1,2))
barplot(rev(data.hca$height)[1:30],main="Evolution du critére d agrégation - VISA")
plot(data.hca,hang=-1,cex=0.8)

# Nous selectionnons 4 classes.

K=4
A2Rplot(data.hca, k=K, show.labels=TRUE)


res.cutree <- cutree(data.hca,4)

centers <- aggregate(data.cr, by=list(res.cutree), FUN=mean) #gravity centers
centers <- centers[,-1]

data.kmeans <- kmeans(data.cr, centers=centers, algorithm="MacQueen")
data.kmeans.alea <- kmeans(data.cr, centers=centers, nstart = 50, iter.max = 50, algorithm="MacQueen")

data.tot <- cbind(as.data.frame(df_train), Clas = as.factor(data.kmeans$cluster)) 

par(mfrow = c(1,4))

table(data.tot[,c(1,258)])

chisq.test(table(data.tot[,c(1,258)]))

# Discrimination 

intrain<-createDataPartition(df_train$g,p=0.8,list=FALSE)
#intrain
save(intrain, file="intrain.Rdata")
table(df_train$g)/nrow(df_train)


#AFD
X <- df_train[intrain, 2:257]
y <- df_train[intrain,1]
res.desDA <- desDA(X,y, covar = "within")
round(res.desDA$power,3)
# Toutes les fréquences sont liés aux phonèmes

rap.cor <- res.desDA$values[1]/(1+res.desDA$values[1]) ##rapport de corrélation de la var discriminante
round(res.desDA$discor, 4) #corrélation var discriminante et var X


# calcul de la pertinence des X à entrer
res.desDA.forward <- greedy.wilks(X,y,niveau=0.05)
res.desDA.forward
#Difference de vecteur de moyenne, le centres de gravités sont ils les mêmes ? Quand on rajoute une variables est ce que cela change les résultats.
#Il commennce par la var qui discirmine le plus, ensuite, il rajoute conditionnellement à la premiere. Il rajoute celle qui conditionnnelemtn à la premiere, permet de mieux discriminer. 
