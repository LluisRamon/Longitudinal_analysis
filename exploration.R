# Import dataset ----------------------------------------------------------

cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")

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

library(nlme)

cows.gd <- groupedData(pcv ~ time| id, data = cows, outer = ~ dose)
plot(cows.gd)
plot(cows.gd, outer = TRUE)

# Covariance and Correlation Structure
library(tidyr)
cows.w <- spread(cows, time, pcv)
cor(cows.w[, c("1", "2", "3")], use = "complete.obs")


cows.lmList <- lmList(pcv ~ time | id, cows, na.action = )

