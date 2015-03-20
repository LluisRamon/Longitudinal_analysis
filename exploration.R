# Import dataset ----------------------------------------------------------

cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")
cows$dose <- factor(cows$dose, levels = c("L", "M", "H"))
str(cows)
head(cows)

# Summary statistics ------------------------------------------------------

summary(cows)

boxplot(pcv ~ time, data = cows)
boxplot(pcv ~ dose, data = cows)
boxplot(pcv ~ nbirth, data = cows)

# Some graphics -----------------------------------------------------------

library(ggplot2) # Package for graphics

qplot(nbirth, pcv, data = cows, facets = .~ dose, colour = factor(time))

qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose) 
qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose, 
      colour = nbirth)
qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose, 
      colour = factor(id))

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
cor(cows[, 3:5], use = "complete.obs")

library(tidyr)
cows.w <- spread(cows, time, pcv)
cor(cows.w[, c("1", "2", "3")], use = "complete.obs")
# Correlation for each dose

cond <- cows.w$dose == "L"
cor(cows.w[cond, c("1", "2", "3")], use = "complete.obs")
# A lot of missing values

cond <- cows.w$dose == "M"
cor(cows.w[cond, c("1", "2", "3")], use = "complete.obs")

cond <- cows.w$dose == "H"
cor(cows.w[cond, c("1", "2", "3")], use = "complete.obs")
# No missing values

# Two step modeling -------------------------------------------------------

library(nlme)  # mainly for graphs
# version 3.1-120 is required
library(dplyr)

cows.com <- cows[complete.cases(cows), ]
cows.com$idDose <- paste(cows.com$id, cows.com$dose, sep = "_")

cows.gd <- groupedData(pcv ~ time|idDose, data = cows.com, 
                       outer = ~dose, inner = ~nbirth) 

plot(cows.gd)
plot(cows.gd, outer = TRUE)
plot(cows.gd, inner = TRUE)

cows.lmList <- nlme:::lmList(pcv ~ time, cows.gd)

betas <- as.data.frame(coef(cows.lmList))

names(betas) <- c("Intercept", "slope")

betas <- add_rownames(betas, var = "idDose")
betas.info <- cows.com %>% group_by(idDose, id, dose) %>% summarize(nbirth = mean(nbirth))

bdd <- left_join(betas, betas.info)

# Two stage analysisi with dose
modbeta0 <- lm(Intercept ~ dose, bdd)
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

lm(slope ~ Intercept, bdd)

# Linear mixed model ------------------------------------------------------

library(lattice)
library(lme4)

cows.gd.lme <- lmer(pcv ~ time + dose + (time | idDose), data = cows.com)
cows.gd.lme0 <- lmer(pcv ~ time + dose + (0 + time | idDose), data = cows.com)
cows.gd.lme1 <- lmer(pcv ~ time + dose + nbirth + (time | idDose), data = cows.com)
cows.gd.lme10 <- lmer(pcv ~ time + dose + nbirth + (0 + time | idDose), data = cows.com)


xyplot(pcv ~ time | idDose, data = cows.com, type = "l")

summary(cows.gd.lme)
summary(cows.gd.lme0)
summary(cows.gd.lme1)
summary(cows.gd.lme10)
# correlation -1 issues
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2010q1/003519.html
# lRT ~ couleurs + (1 + couleurs | nom)
# IRT ~ couleurs + (1|nom:couleurs) + (1|nom)

anova(cows.gd.lme, cows.gd.lme0)
anova(cows.gd.lme0, cows.gd.lme1)
anova(cows.gd.lme1, cows.gd.lme10)

# This model looks to be better
cows.gd.lme10

residus <- residuals(cows.gd.lme10)

plot(residus)
hist(residus)

plot(cows.gd.lme10)
plot(cows.gd.lme10, dose ~ resid(., scaled=TRUE))
VarCorr(cows.gd.lme10)

intervals(cows.gd.lme10)
qqnorm(cows.gd.lme10)


ranef(cows.gd.lme10)
pr1 <- profile(cows.gd.lme10)
confint(pr1)
splom(pr1)
dotplot(ranef(cows.gd.lme, condVar = TRUE))
dotplot(ranef(cows.gd.lme))
qqmath(ranef(cows.gd.lme, condVar = TRUE))

# Models with lme for including correlation structure

cows.gd <- groupedData(pcv ~ time|idDose, data = cows.com, 
                       outer = ~dose, inner = ~nbirth)

cows.gd <- groupedData(pcv ~ time|idDose, data = cows.com, 
                       inner = ~nbirth) 

mod1 <- lme(pcv ~ time + dose, data = cows.gd)
mod1 <- lme(pcv ~ time + dose, random = ~1|idDose, data = cows.gd)
# mod1 <- lme(pcv ~ time + dose, random = ~time|idDose, data = cows.gd) 
# Convergence problems. Seen in previous models correlation -1 between random effects
mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, data = cows.gd)
mod1 <- lme(pcv ~ time + dose, random = list(~1|idDose, ~time-1|idDose), data = cows.gd)

summary(mod1)


mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, 
            correlation = corAR1(0.5), data = cows.gd)
mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, 
            correlation = corAR1(form = ~1|idDose), data = cows.gd)
summary(mod1)
