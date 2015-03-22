# Import dataset ----------------------------------------------------------

cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")
cows$dose <- factor(cows$dose, levels = c("L", "M", "H"))
str(cows)
head(cows)

library("nlme") 
# version 3.1-120 is required
library("dplyr")

cows.com <- cows[complete.cases(cows), ]
cows.com$idDose <- paste(cows.com$id, cows.com$dose, sep = "_")

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


# Covariance and Correlation Structure ------------------------------------

cor(cows[, 3:5], use = "complete.obs")

library("tidyr")
cows.w <- spread(cows, time, pcv)
cor(cows.w[, c("1", "2", "3")], use = "pairwise.complete.obs")
# Correlation for each dose

cond <- cows.w$dose == "L"
cor(cows.w[cond, c("1", "2", "3")], use = "pairwise.complete.obs")
# A lot of missing values in the data

cond <- cows.w$dose == "M"
cor(cows.w[cond, c("1", "2", "3")], use = "pairwise.complete.obs")

cond <- cows.w$dose == "H"
cor(cows.w[cond, c("1", "2", "3")], use = "pairwise.complete.obs")
# No missing values in the data

# Two step modeling -------------------------------------------------------

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

# Graph option 1
q <- qplot(factor(time), pcv, group = id, data = cows.com, geom = c("point", "line")) + facet_grid(dose ~ id)
q + geom_abline(aes(intercept = Intercept, slope = slope), colour = "red", data = bdd, alpha = 0.8)

# Graph option 2
q <- qplot(factor(time), pcv, group = id, data = cows.com, geom = "point") + facet_grid(.~ dose)
q + geom_abline(aes(intercept = Intercept, slope = slope, colour = factor(dose)), data = bdd)

# Linear mixed model ------------------------------------------------------

library("lattice")
library("lme4")

cows.gls <- lm(pcv ~ time + dose, data = cows.com, method="REML")
cows.lme <- lmer(pcv ~ time + dose + (1 | idDose), data = cows.com)

anova(cows.gls, cows.lme)

cows.lme <- lmer(pcv ~ time + dose + (time | idDose), data = cows.com)
cows.lme0 <- lmer(pcv ~ time + dose + (0 + time | idDose), data = cows.com)
cows.lme1 <- lmer(pcv ~ time + dose + nbirth + (time | idDose), data = cows.com)
cows.lme10 <- lmer(pcv ~ time + dose + nbirth + (0 + time | idDose), data = cows.com)


xyplot(pcv ~ time | idDose, data = cows.com, type = "l")

summary(cows.lme)
summary(cows.lme0)
summary(cows.lme1)
summary(cows.lme10)


anova(cows.lme, cows.lme0)
anova(cows.lme0, cows.lme1)
anova(cows.lme1, cows.lme10)

# This model looks to be better
cows.lme10

residus <- residuals(cows.lme10)

plot(residus)
hist(residus)

plot(cows.lme10)
plot(cows.lme10, dose ~ resid(., scaled=TRUE))
VarCorr(cows.lme10)

# Interesting functions
ranef(cows.lme10)
pr1 <- profile(cows.lme10)
confint(pr1)
dotplot(ranef(cows.lme, condVar = TRUE))
dotplot(ranef(cows.lme))
qqmath(ranef(cows.lme, condVar = TRUE))

# Models with lme for including correlation structure ---------------------

cows.gd <- groupedData(pcv ~ time|idDose, data = cows.com, 
                       outer = ~dose, inner = ~nbirth)

# Fixed effect
cows.gls <- gls(pcv ~ time + dose, data = cows.com, method="REML")

# Random effect
cows.lme.Inte <- lme(pcv ~ time + dose, random = ~1|idDose, data = cows.gd)
cows.lme.Slop <- lme(pcv ~ time + dose, random = ~0 + time|idDose, data = cows.gd)

anova(cows.gls, cows.lme.Inte)
anova(cows.gls, cows.lme.Slop)

AIC(cows.lme.Inte)
AIC(cows.lme.Slop)

cows.lme <- lme(pcv ~ time + dose, random = ~time|idDose, data = cows.gd) 
# Crashes. With lmer we see a correlation of -1, so with just one random effect is enogh

# Add nbirth fixed effect 

cows.lme.Slop <- lme(pcv ~ time + dose, random = ~0 + time|idDose, data = cows.gd, method = "ML")
cows.lme.Slop.birth <- lme(pcv ~ time + dose + nbirth, random = ~0 + time|idDose, data = cows.gd, method = "ML")

anova(cows.lme.Slop, cows.lme.Slop.birth)

# Random effect with nbirth

cows.lme.Slop.birth <- lme(pcv ~ time + dose + nbirth, random = ~0 + time|idDose, data = cows.gd)
cows.lme.Slop.birth <- lme(pcv ~ time + dose + nbirth, random = ~0 + time|id/dose, data = cows.gd)
cows.lme.Slop.birthR <- lme(pcv ~ time + dose + nbirth, random = ~0 + time + nbirth|idDose, 
                           data = cows.gd)

anova(cows.lme.Slop.birth, cows.lme.Slop.birthR)

summary(cows.lme.Slop.birth)

# Correlation structure
ACF(cows.lme.Slop.birth)

cows.lme.Slop.birthInt <- lme(pcv ~ time*dose - dose + nbirth, random = ~0 + time|idDose, data = cows.gd)

summary(cows.lme.Slop.birthInt)

mod1 <- lme(pcv ~ time + dose, data = cows.gd)

# mod1 <- lme(pcv ~ time + dose, random = ~time|idDose, data = cows.gd) 
# Convergence problems. Seen in previous models correlation -1 between random effects
mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, data = cows.gd)
mod1 <- lme(pcv ~ time + dose, random = list(~1|idDose, ~time-1|idDose), data = cows.gd)

# Alternative model specification
mod1 <- lme(pcv ~ time + dose, random = ~1|id/dose, data = cows.com)

summary(mod1)

mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, 
            correlation = corAR1(0.5), data = cows.gd)
mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, 
            correlation = corAR1(form = ~1|idDose), data = cows.gd)
mod1 <- lme(pcv ~ time + dose, random = ~time -1|idDose, 
            correlation = corARMA(p = 2), data = cows.gd)
summary(mod1)
