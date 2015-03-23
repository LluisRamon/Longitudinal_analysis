# Load packages -----------------------------------------------------------
library("nlme") 
# version 3.1-120 is required
library("dplyr")
library("ggplot2")
library("tidyr")
library("corrplot")

# Import dataset ----------------------------------------------------------
cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")
cows$dose <- factor(cows$dose, levels = c("L", "M", "H"))
str(cows)
head(cows)

cows.com <- na.omit(cows)
cows.com$idDose <- paste(cows.com$id, cows.com$dose, sep = "_")

# Summary statistics ------------------------------------------------------
str(cows)
head(cows)
summary(cows)

boxplot(pcv ~ time, data = cows)
boxplot(pcv ~ dose, data = cows)
boxplot(pcv ~ nbirth, data = cows)

cows$timeDose <- paste(cows$time, cows$dose, sep = ".")
cows$timeDose <- factor(cows$timeDose, levels = c("1.L", "2.L", "3.L", "1.M", "2.M", "3.M", "1.H", "2.H", "3.H"))
ggplot(aes(timeDose, pcv, fill = dose), data = cows) + geom_boxplot()
cows$timeDose <- NULL

# Some graphs -----------------------------------------------------------

qplot(nbirth, pcv, data = cows, facets = .~ dose, colour = factor(time))
qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose) 
qplot(time, pcv, data = cows, group = id, geom = "line", facets = .~ dose, 
      colour = nbirth)
qplot(factor(time), pcv, data = cows, group = id, geom = "line", facets = .~ dose, 
      colour = factor(id)) + scale_color_discrete(guide = 'none')


# Linear model on nbirths ------------------------------------------------------

lcowsnbirth <- lm(pcv ~ nbirth, data = cows[cows$time == 1,])
summary(lcowsnbirth)

# Missing values ----------------------------------------------------------

missingsTable <- 10 - table(cows.com$dose, cows.com$time)
missingsTable

# Covariance and Correlation Structure ------------------------------------

cows.w <- spread(cows, time, pcv)
corrplot(cor(cows.w[, c("1", "2", "3")], use = "pairwise.complete.obs"), "ellipse")


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


# Multivariate model ------------------------------------------------------

TODO(Gerard): Add step forward multivariate models

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


# Models with lme for including correlation structure ---------------------

# Fixed effect
cows.gls <- gls(pcv ~ time + dose, data = cows.com, method = "REML")

# Random effect
cows.lme.Inte <- lme(pcv ~ time + dose, random = ~1|idDose, data = cows.com)
cows.lme.Slop <- lme(pcv ~ time + dose, random = ~0 + time|idDose, data = cows.com)

anova(cows.gls, cows.lme.Inte)
anova(cows.gls, cows.lme.Slop)

# We see in anova that cows.lme.Slop has better AIC so we continue with this model.

# cows.lme <- lme(pcv ~ time + dose, random = ~time|idDose, data = cows.gd) 
# Crashes. With lmer we see a correlation of -1, so with just one random effect is enogh
# library("lme4")
# lmer(pcv ~ time + dose + (time | idDose), data = cows.com)

# Add nbirth fixed effect

cows.lme.Slop <- update(cows.lme.Slop, method = "ML")
cows.lme.Slop.birth <- update(cows.lme.Slop, fixed  = ~ . + nbirth, method = "ML")

anova(cows.lme.Slop, cows.lme.Slop.birth)

# Random effect with nbirth

# We recalculate lme with method REML
cows.lme.Slop.birth <- update(cows.lme.Slop.birth, method = "REML")
cows.lme.Slop.birthR <-  update(cows.lme.Slop.birth, random = ~0 + time + nbirth|idDose, method = "REML")

anova(cows.lme.Slop.birth, cows.lme.Slop.birthR)

# We keep this model
summary(cows.lme.Slop.birth)

# Correlation structure
cows.lme.Slop.birth <- lme(pcv ~ time + dose + nbirth, random = ~0 + time|idDose, data = cows.com)
cows.lme.corAR1 <- update(cows.lme.Slop.birth, correlation = corAR1())
cows.lme.CompSymm <- update(cows.lme.Slop.birth, correlation = corCompSymm())
cows.lme.corARMA <- update(cows.lme.Slop.birth, correlation = corARMA(p = 2))

anova(cows.lme.Slop.birth, cows.lme.corAR1)
anova(cows.lme.Slop.birth, cows.lme.CompSymm)
anova(cows.lme.Slop.birth, cows.lme.corARMA)

# We keep this model
cows.fixef <- fixed.effects(cows.lme.Slop.birth)
summary(cows.lme.Slop.birth)
intervals(cows.lme.Slop.birth)

cows.ranef <- ranef(cows.lme.Slop.birth, augFrame = TRUE)[c(13, 12, 24),]
cows.ranef$intercept <- cows.fixef[1]
cows.ranef <- add_rownames(cows.ranef, "idDose")

bdd <- data.frame(x = 1:3, ymin = 20, 
                  ymax = 25, pcv = 25)

q <- ggplot(aes(time, pcv), data = cows.com) + geom_blank() + 
  ylim(c(0, 30)) + xlim(c(0, 4))
q + geom_abline(intercept = cows.fixef[1], slope = cows.fixef[2]) + 
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax), data = bdd) + 
  geom_abline(aes(intercept = intercept, slope = time, colour = idDose), data = cows.ranef)


# With block and longitudinal model ---------------------------------------

cows.lme.nested <- lme(pcv ~ time + dose + nbirth, random = ~0 + time|id/dose, data = cows.com)

summary(cows.lme.nested)

coefficients(cows.lme.nested)
coef(cows.lme.nested)
intervals(cows.lme.nested)

fixed.effects(cows.lme.nested )
randomEffects <- ranef(cows.lme.nested)

ranEfDf <- randomEffects$dose
ranEfDf <- add_rownames(ranEfDf, "idDose")
ranEfDf <- separate(ranEfDf, idDose, c("id", "dose"), remove = FALSE)

qplot(time, idDose, data = ranEfDf, color = dose)
qplot(time, id, data = ranEfDf, color = dose, size = I(4))
