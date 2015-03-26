# Load packages -----------------------------------------------------------
library("nlme") 
# version 3.1-120 is required
library("dplyr")
library("ggplot2")
library("tidyr")
library("corrplot")
library("grid")
library("gridExtra")

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

# dep variable:
# pcv: the higher the better.

# indep variables:
# dose: we want to know if the dose is associated with lower PCV
# nbirth: we want to know if this covariate influences the effect of the dose.
# time: we dont particularly want to see differences in time, but maybe 
# 3rd observations of the doses are higher, ...

# we do not want:
# id: we dont want the effect of the dose to be explained by the cow.

# Each observation of each cow will be considered independent.
model <- lm(pcv ~ time + dose, data=cows)
summary(model)
model0 <- model
add1(model0, scope=~.^2+nbirth, test="F")

model <- lm(pcv ~ time * dose, data=cows)
summary(model)
model1 <- model
add1(model1, scope=~.^2+nbirth, test="F")

model <- lm(pcv ~ time*dose + nbirth, data=cows)
summary(model)
model2 <- model
add1(model2, scope=~.^2, test="F") #nothing else to add
drop1(model2, test="F") #nothing to remove

anova(model2)

# dose H has a huge effect, in a possitive way. Also, in general higher
# times give better PCV values (which is logical). nbirth has a significative
# negative effect on the PCV.
# time and dose almost interact for high doses. This interaction is synergistic.
# higher times and dose H give even higher pcv, but this effect is higher when 
# combined together.

## diagnostic:
# high pcv has high positive errors. Doesnt seem to be random.
plot(model2$residuals, model2$y)
# problems with doseH
plot(model2$model$dose, model2$residuals)
# NO homoscedasticity
plot(model2$model$time, model2$residuals)
# not centered
plot(model2$model$nbirth, model2$residuals)
## BAD MODEL: we already know that, since obervations are not independent.

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


# Validation 

model2 <- cows.lme.nested
model2$residuals <- predict(model2)-cows.com$pcv
v1 <- qplot(cows.com$pcv, model2$residuals, xlab = "PCV value", ylab = "Residuals")
v1 <- v1 + geom_hline(yintercept=0) + geom_smooth(span = 1.2)
v2 <- qplot(model2$data$dose, model2$residuals, ylab="Residuals", xlab = "Dose", geom = "boxplot", fill = model2$data$dose) + guides(fill = FALSE) + scale_x_discrete(labels = c("Low", "Medium", "High"))
v3 <- qplot(model2$data$time, model2$residuals, ylab="Residuals", xlab="Time") + 
  scale_x_discrete(labels = 1:3) + geom_hline(yintercept=0)
v4 <- qplot(model2$data$nbirth, model2$residuals, ylab = "Residuals", xlab = "Number of births (nbirth)") + geom_hline(yintercept=0)
grid.arrange(v1, v2, v3, v4, ncol = 2)