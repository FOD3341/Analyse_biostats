# load data frame

data <- read.table("raw_data/bdd_meneherbe.txt",header = T, sep = "")

# load some packages
library(GGally)
library(ggeffects)

# visualize data
ggpairs(data)

# a) effet total augmentation d’un (1) µg/L de la concentration totale en herbicidessur la biomasse en plantes aquatiques

# linear model

moda <- lm(Plantes.aquat.kgm2 ~ Herbicides.ugL,data = data)

summary(moda)

# residual
plot(moda)

effecta <- ggeffect(moda,"Herbicides.ugL")


# Visualisation avec données brutes
ggplot() +
  # Ajouter les points de données bruts
  geom_point(data = data, aes(x = Herbicides.ugL, y = Plantes.aquat.kgm2), 
             color = "blue", alpha = 0.6) +
  # Ajouter la ligne de l'effet marginal estimé
  geom_line(data = effecta, aes(x = x, y = predicted), color = "red", size = 1) +
  # Ajouter l'intervalle de confiance
  geom_ribbon(data = effecta, aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "red", alpha = 0.2) +
  labs(x = "Herbicides (µg/L)", y = "Plantes aquatiques (kg/m²)",
       title = "Effet des herbicides sur la biomasse des plantes aquatiques") +
  theme_minimal()

# reponse a 

# lorsque la concentration en herbicide augment de 1 ug/L, il y une diminution de -0.018853 kg/m2