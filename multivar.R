#' ---
#' title: "Study of the efficacy of Berenil applied to trypanosomosis's infected cattle"
#' author: Gerard Castellà, Mathieu Marauri and Lluis Ramon
#' date: \today
#' ---
  
# Import dataset ----------------------------------------------------------

cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")

cows$id <- as.factor(cows$id)
cows$dose <- factor(cows$dose, levels=c("L", "M", "H"))

########
## LM ##
########

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

table(cows$time, is.na(cows$pcv))

model <- lm(pcv ~ time + dose, data=cows)
summary(model)
model0 <- model

model <- lm(pcv ~ time + dose + nbirth, data=cows)
summary(model)
model1 <- model
# dose H has a huge effect, in a possitive way. Also, in general higher
# times give better PCV values (which is logical). nbirth has a significative
# negative effect on the PCV.

model <- lm(pcv ~ time*dose + nbirth, data=cows)
summary(model)
model2 <- model
# time and dose almost interact for high doses. This interaction is synergistic.
# higher times and dose H give even higher pcv, but this effect is higher when 
# combined together.

model <- lm(pcv ~ dose + time*nbirth, data=cows)
summary(model)
model <- lm(pcv ~ time + dose*nbirth, data=cows)
summary(model)
# no other interactions are significative.

# so... model selection, for the more pasimonious model says...
add1(model0, scope=~.^2+nbirth, test="F")
add1(model1, scope=~.^2, test="F")
add1(model2, scope=~.^2, test="F")

## final model:
model2 <- lm(pcv ~ time*dose + nbirth, data=cows, y=T, model=T)
summary(model2)

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