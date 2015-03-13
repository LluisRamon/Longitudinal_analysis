# Import dataset ----------------------------------------------------------

cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")
cows$dose <- factor(cows$dose, levels = c("L", "M", "H"))

str(cows)
head(cows)
summary(cows)

table(cows$time)
table(cows$nbirth)
table(cows$nbirth, cows$id)

table(cows$id)

# There are 9 mesures for each id
# TODO: In de .doc says there are 5 cows but there are 10 id and with different
# doses. I don't understand data.

boxplot(pcv ~ time, data = cows)
boxplot(pcv ~ dose, data = cows)
boxplot(pcv ~ nbirth, data = cows)

# Some graphics -----------------------------------------------------------

library(ggplot2) # Package for graphics

qplot(nbirth, pcv, data = cows, facets = .~ dose, colour = factor(time))


# Posible graphs, but I am not sure about data structure
qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose) 
qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose, 
      colour = nbirth) 


# Add a linear model ------------------------------------------------------

cows$id <- as.factor(cows$id)
hist(cows$pcv)
hist(log(cows$pcv))

lcows <- lm(log(pcv) ~ . -id, data = cows)
summary(lcows)
vcov(summary(lcows))
influence(lcows)

plot(lcows)

hist(residuals(lcows))

cows$residual[!is.na(cows$pcv)] <- residuals(lcows)
              
plot(cows$id, cows$residual)
plot(cows$time, cows$residual)
plot(cows$nbirth, cows$residual)
plot(cows$dose, cows$residual)

cows$residual <- NULL

# Covariance and Correlation Structure
library(tidyr)
cows.w <- spread(cows, time, pcv)
cor(cows.w[, c("1", "2", "3")], use = "complete.obs")

# Two step modeling -------------------------------------------------------

library(nlme)  # mainly for graphs
library(dplyr)
library(tidyr)

cows.com <- cows[complete.cases(cows), ]
cows.com$dose.n <- as.numeric(cows.com$dose)
cows.com$idDose <- paste(cows.com$id, cows.com$dose, sep = "_")

cows.gd <- groupedData(pcv ~ time|idDose, data = cows.com, outer = ~dose) 
# cows.gd <- groupedData(pcv ~ time|idDose, data = cows.com, inner = ~nbirth)

plot(cows.gd)
plot(cows.gd, outer = TRUE)

cows.lmList <- lmList(pcv ~ time, cows.gd)

betas <- as.data.frame(coef(cows.lmList))

names(betas) <- c("Intercept", "slope")

betas <- add_rownames(betas, var = "idDose")
betas.info <- cows.com %>% group_by(idDose, id, dose) %>% summarize(nbirth = mean(nbirth))

bdd <- left_join(betas, betas.info)

# Two stage analysisi with dose
modbeta0 <- lm(Intercept ~ dose, bdd)
modbeta0 <- lm(Intercept ~ 0 + dose, bdd) # Intercept 0
summary(modbeta0)

# Plot the residuals
plot(modbeta0)

modbeta1 <- lm(slope ~ dose, bdd)
summary(modbeta1)

# Plot the residuals
plot(modbeta1)

# Two stage analysisi with id
modbeta0 <- lm(Intercept ~ factor(id), bdd)
summary(modbeta0)

modbeta1 <- lm(slope ~ factor(id), bdd)
summary(modbeta1)

# Linear mixed model ------------------------------------------------------

library(lme4)

cows.gd.lme <- lmer(pcv ~ time + (time | idDose), data = cows.com)
cows.gd.lme <- lmer(pcv ~ time + dose + (time | idDose), data = cows.com)

cows.gd.lme

summary(cows.gd.lme)
