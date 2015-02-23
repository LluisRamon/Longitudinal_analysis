# Import dataset ----------------------------------------------------------

cows <- read.table("data/cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

# TODO: Can we change variable names? easier ones? no capital letters, etc?

str(cows)
head(cows)
summary(cows)

table(cows$Time)
table(cows$numbirths)
table(cows$numbirths, cows$ID)

table(cows$ID)

# There are 9 mesures for each ID
# TODO: In de .doc says there are 5 cows but there are 10 ID and with different
# doses. I don't understand data.

boxplot(PCV ~ Time, data = cows)
boxplot(PCV ~ Dose, data = cows)
boxplot(PCV ~ numbirths, data = cows)

# Some graphics -----------------------------------------------------------

library(ggplot2) # Package for graphics

qplot(numbirths, PCV, data = cows, facets = .~ Dose, colour = factor(Time))


# Posible graphs, but I am not sure about data structure
qplot(Time, PCV, data = cows, group = ID, geom = "line", facets = .~ Dose) 
qplot(Time, PCV, data = cows, group = ID, geom = "line", facets = .~ Dose, 
      colour = numbirths) 
