        # ----- Summary statistics ----- #

  # Dataset #
cows <- read.table("cattle_mes dades.txt", header = TRUE, 
                   sep = "\t", dec = ",", na.strings = "")

names(cows) <- c("id", "dose", "pcv", "time", "nbirth")

# -------------------------------------------------------------------------------- #


  # Packages #

# Graphics
install.packages("ggplot2")
library(ggplot2)
library(nlme)

# -------------------------------------------------------------------------------- #


  # Basic statistical summary #
summary(cows)

boxplot(cows$pcv, data=cows,main="Boxplot of the pcv")

cow1 <- subset(cows, id==1)
cow1
summary(cow1)
qplot(time, pcv, data = cow1, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 1")

cow2 <- subset(cows, id==2)
cow2
summary(cow2)
qplot(time, pcv, data = cow2, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 2")

cow3 <- subset(cows, id==3)
cow3
summary(cow3)
qplot(time, pcv, data = cow3, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 3")

cow4 <- subset(cows, id==4)
cow4
summary(cow4)
qplot(time, pcv, data = cow4, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 4")

cow5 <- subset(cows, id==5)
cow5
summary(cow5)
qplot(time, pcv, data = cow5, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 5")

cow6 <- subset(cows, id==6)
cow6
summary(cow6)
qplot(time, pcv, data = cow6, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 6")

cow7 <- subset(cows, id==7)
cow7
summary(cow7)
qplot(time, pcv, data = cow7, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 7")

cow8 <- subset(cows, id==8)
cow8
summary(cow8)
qplot(time, pcv, data = cow8, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 8")

cow9 <- subset(cows, id==9)
cow9
summary(cow9)
qplot(time, pcv, data = cow9, geom="line", facets=.~ dose, 
                   main="Pcv evolution of cow 9")

cow10 <- subset(cows, id==10)
cow10
summary(cow10)
qplot(time, pcv, data = cow10, geom="line", facets=.~ dose, 
      main="Pcv evolution of cow 10")

qplot(time, pcv, data = cows, main="Global view",group = id, geom = "line", facets = .~ dose, 
      colour = nbirth) 

# -------------------------------------------------------------------------------- #


  # Influence of dose #

cowstreated <- subset(cows, time!=1 )

cowshigh <- subset(cowstreated, dose=="H")
summary(cowshigh)
cowsmedium <- subset(cowstreated, dose=="M")
summary(cowsmedium)
cowslow <- subset(cowstreated, dose=="L")
summary(cowslow)

boxplot(pcv ~ dose, data = cowstreated,main="Pcv after treatment")


qplot(time, pcv, data = cows, group=id, geom="line", facets=.~ dose, 
      main="PCV evolution")

lcowsdose <- lm(pcv~dose, data = cows)
summary(lcowsdose)

# -------------------------------------------------------------------------------- #


  # Influence of time #

cowstime1 <- subset(cows, time==1)
cowstime2 <- subset(cows, time==2)
cowstime3 <- subset(cows, time==3)

boxplot(cowstime1$pcv,data=cowstime1,xlab="time",ylab="pcv",main="Pcv at time 1")

summary(cowstime1)
summary(cowstime2)
summary(cowstime3)

boxplot(pcv ~ time,data = cows,xlab="time",ylab="pcv",main="Boxplot pcv bytime")

lcowstime <- lm(pcv~time, data = cows)
summary(lcowstime)

# Effect of the first dose
#cow1
cow1high <- subset(cow1, dose=="H")
cow1medium <- subset(cow1, dose=="M")
cow1low <- subset(cow1, dose=="L")
effect1stdosecow1 <- c((cow1high[2,3]-cow1high[1,3])/cow1high[1,3]*100,
                       (cow1medium[2,3]-cow1medium[1,3])/cow1medium[1,3]*100,
                       (cow1low[2,3]-cow1low[1,3])/cow1low[1,3]*100)
#cow2
cow2high <- subset(cow2, dose=="H")
cow2medium <- subset(cow2, dose=="M")
cow2low <- subset(cow2, dose=="L")
effect1stdosecow2 <- c((cow2high[2,3]-cow2high[1,3])/cow2high[1,3]*100,
                       (cow2medium[2,3]-cow2medium[1,3])/cow2medium[1,3]*100,
                       (cow2low[2,3]-cow2low[1,3])/cow2low[1,3]*100)
#cow3
cow3high <- subset(cow3, dose=="H")
cow3medium <- subset(cow3, dose=="M")
cow3low <- subset(cow3, dose=="L")
effect1stdosecow3 <- c((cow3high[2,3]-cow3high[1,3])/cow3high[1,3]*100,
                       (cow3medium[2,3]-cow3medium[1,3])/cow3medium[1,3]*100,
                       (cow3low[2,3]-cow3low[1,3])/cow3low[1,3]*100)
#cow4
cow4high <- subset(cow4, dose=="H")
cow4medium <- subset(cow4, dose=="M")
cow4low <- subset(cow2, dose=="L")
effect1stdosecow4 <- c((cow4high[2,3]-cow4high[1,3])/cow4high[1,3]*100,
                       (cow4medium[2,3]-cow4medium[1,3])/cow4medium[1,3]*100,
                       (cow4low[2,3]-cow4low[1,3])/cow4low[1,3]*100)
#cow5
cow5high <- subset(cow5, dose=="H")
cow5medium <- subset(cow5, dose=="M")
cow5low <- subset(cow5, dose=="L")
effect1stdosecow5 <- c((cow5high[2,3]-cow5high[1,3])/cow5high[1,3]*100,
                       (cow5medium[2,3]-cow5medium[1,3])/cow5medium[1,3]*100,
                       (cow5low[2,3]-cow5low[1,3])/cow5low[1,3]*100)
#cow6
cow6high <- subset(cow6, dose=="H")
cow6medium <- subset(cow6, dose=="M")
cow6low <- subset(cow6, dose=="L")
effect1stdosecow6 <- c((cow6high[2,3]-cow6high[1,3])/cow6high[1,3]*100,
                       (cow6medium[2,3]-cow6medium[1,3])/cow6medium[1,3]*100,
                       (cow6low[2,3]-cow6low[1,3])/cow6low[1,3]*100)
#cow7
cow7high <- subset(cow7, dose=="H")
cow7medium <- subset(cow7, dose=="M")
cow7low <- subset(cow7, dose=="L")
effect1stdosecow7 <- c((cow7high[2,3]-cow7high[1,3])/cow7high[1,3]*100,
                       (cow7medium[2,3]-cow7medium[1,3])/cow7medium[1,3]*100,
                       (cow7low[2,3]-cow7low[1,3])/cow7low[1,3]*100)
#cow8
cow8high <- subset(cow8, dose=="H")
cow8medium <- subset(cow8, dose=="M")
cow8low <- subset(cow2, dose=="L")
effect1stdosecow8 <- c((cow8high[2,3]-cow8high[1,3])/cow8high[1,3]*100,
                       (cow8medium[2,3]-cow8medium[1,3])/cow8medium[1,3]*100,
                       (cow8low[2,3]-cow8low[1,3])/cow8low[1,3]*100)
#cow9
cow9high <- subset(cow9, dose=="H")
cow9medium <- subset(cow9, dose=="M")
cow9low <- subset(cow9, dose=="L")
effect1stdosecow9 <- c((cow9high[2,3]-cow9high[1,3])/cow9high[1,3]*100,
                       (cow9medium[2,3]-cow9medium[1,3])/cow9medium[1,3]*100,
                       (cow9low[2,3]-cow9low[1,3])/cow9low[1,3]*100)
#cow10
cow10high <- subset(cow10, dose=="H")
cow10medium <- subset(cow10, dose=="M")
cow10low <- subset(cow10, dose=="L")
effect1stdosecow10 <- c((cow10high[2,3]-cow10high[1,3])/cow10high[1,3]*100,
                        (cow10medium[2,3]-cow10medium[1,3])/cow10medium[1,3]*100,
                        (cow10low[2,3]-cow10low[1,3])/cow10low[1,3]*100)

listeffect1stdose <- list(effect1stdosecow1,effect1stdosecow2,effect1stdosecow3,effect1stdosecow4,
                      effect1stdosecow5,effect1stdosecow6,effect1stdosecow7,effect1stdosecow8,
                      effect1stdosecow9,effect1stdosecow10)
effect1stdose <- data.frame(listeffect1stdose)
colnames(effect1stdose) <- c("cow1","cow2","cow3","cow4","cow5","cow6","cow7","cow8","cow9","cow10")
rownames(effect1stdose) <- c("high","medium","low")
effect1stdose

rowMeans(effect1stdose,na.rm=TRUE)

# Effect of the second dose
#cow1
effect2nddosecow1 <- c((cow1high[3,3]-cow1high[2,3])/cow1high[2,3]*100,
                       (cow1medium[3,3]-cow1medium[2,3])/cow1medium[2,3]*100,
                       (cow1low[3,3]-cow1low[2,3])/cow1low[2,3]*100)
#cow2
effect2nddosecow2 <- c((cow2high[3,3]-cow2high[2,3])/cow2high[2,3]*100,
                       (cow2medium[3,3]-cow2medium[2,3])/cow2medium[2,3]*100,
                       (cow2low[3,3]-cow2low[2,3])/cow2low[2,3]*100)
#cow3
effect2nddosecow3 <- c((cow3high[3,3]-cow3high[2,3])/cow3high[2,3]*100,
                       (cow3medium[3,3]-cow3medium[2,3])/cow3medium[2,3]*100,
                       (cow3low[3,3]-cow3low[2,3])/cow3low[2,3]*100)
#cow4
effect2nddosecow4 <- c((cow4high[3,3]-cow4high[2,3])/cow4high[2,3]*100,
                       (cow4medium[3,3]-cow4medium[2,3])/cow4medium[2,3]*100,
                       (cow4low[3,3]-cow4low[2,3])/cow4low[2,3]*100)
#cow5
effect2nddosecow5 <- c((cow5high[3,3]-cow5high[2,3])/cow5high[2,3]*100,
                       (cow5medium[3,3]-cow5medium[2,3])/cow5medium[2,3]*100,
                       (cow5low[3,3]-cow5low[2,3])/cow5low[2,3]*100)
#cow6
effect2nddosecow6 <- c((cow6high[3,3]-cow6high[2,3])/cow6high[2,3]*100,
                       (cow6medium[3,3]-cow6medium[2,3])/cow6medium[2,3]*100,
                       (cow6low[3,3]-cow6low[2,3])/cow6low[2,3]*100)
#cow7
effect2nddosecow7 <- c((cow7high[3,3]-cow7high[2,3])/cow7high[2,3]*100,
                       (cow7medium[3,3]-cow7medium[2,3])/cow7medium[2,3]*100,
                       (cow7low[3,3]-cow7low[2,3])/cow7low[2,3]*100)
#cow8
effect2nddosecow8 <- c((cow8high[3,3]-cow8high[2,3])/cow8high[2,3]*100,
                       (cow8medium[3,3]-cow8medium[2,3])/cow8medium[2,3]*100,
                       (cow8low[3,3]-cow8low[2,3])/cow8low[2,3]*100)
#cow9
effect2nddosecow9 <- c((cow9high[3,3]-cow9high[2,3])/cow9high[2,3]*100,
                       (cow9medium[3,3]-cow9medium[2,3])/cow9medium[2,3]*100,
                       (cow9low[3,3]-cow9low[2,3])/cow9low[2,3]*100)
#cow10
effect2nddosecow10 <- c((cow10high[3,3]-cow10high[2,3])/cow10high[2,3]*100,
                        (cow10medium[3,3]-cow10medium[2,3])/cow10medium[2,3]*100,
                        (cow10low[3,3]-cow10low[2,3])/cow10low[2,3]*100)

listeffect2nddose <- list(effect2nddosecow1,effect2nddosecow2,effect2nddosecow3,effect2nddosecow4,
                          effect2nddosecow5,effect2nddosecow6,effect2nddosecow7,effect2nddosecow8,
                          effect2nddosecow9,effect2nddosecow10)
effect2nddose <- data.frame(listeffect2nddose)
colnames(effect2nddose) <- c("cow1","cow2","cow3","cow4","cow5","cow6","cow7","cow8","cow9","cow10")
rownames(effect2nddose) <- c("high","medium","low")
effect2nddose

rowMeans(effect2nddose,na.rm=TRUE)

# The first dose has an effect more important than the second one. It is especially true for 
# the high dose and the medium one. Since there are a lot of missing values for the low dose
# we cannot make any conclusion about the effect for the low dose.
# The effect of the first dose also gives us insight about the difference in efficiency between
# the doses. The high dose is reponsible for an increase in pcv much more important thant the two
# other doses, medium and low.

# -------------------------------------------------------------------------------- #


  # Influence of nbirth #

plot(cows$nbirth,cows$pcv)
boxplot(pcv ~ nbirth, xlab="time",ylab="pcv",data = cows,main="Boxplot pcv by nbirth")

table(cows$nbirth)

boxplot(pcv ~ nbirth, data = cowstime1,main="Boxplot pcv~nbirth, time 1")
boxplot(pcv ~ nbirth, data = cowstime2,main="Boxplot pcv~nbirth, time 2")
boxplot(pcv ~ nbirth, data = cowstime3,main="Boxplot pcv~nbirth, time 3")

lcowsnbirth <- lm(pcv~nbirth, data = cows)
summary(lcowsnbirth)

# Effect of nbirth on the effect of dose.

t(effect1stdose[1,])
cows.nbirth <- as.factor(cows[1+9*(0:9),"nbirth"])

# Effect of the first high dose.
cows.effect1stdosehigh.nbirth <- data.frame(t(effect1stdose[1,]),cows.nbirth)
lcows.effect1stdosehigh.nbirth <- lm(high~cows.nbirth,data=cows.effect1stdosehigh.nbirth)
summary(lcows.effect1stdosehigh.nbirth)

# Effect of the second high dose.
cows.effect2nddosehigh.nbirth <- data.frame(t(effect2nddose[1,]),cows.nbirth)
lcows.effect2nddosehigh.nbirth <- lm(high~cows.nbirth,data=cows.effect2nddosehigh.nbirth)
summary(lcows.effect2nddosehigh.nbirth)

# Effect of the first medium dose.
cows.effect1stdosemedium.nbirth <- data.frame(t(effect1stdose[2,]),cows.nbirth)
lcows.effect1stdosemedium.nbirth <- lm(medium~cows.nbirth,data=cows.effect1stdosemedium.nbirth)
summary(lcows.effect1stdosemedium.nbirth)

# Effect of the second medium dose.
cows.effect2nddosemedium.nbirth <- data.frame(t(effect2nddose[2,]),cows.nbirth)
lcows.effect2nddosemedium.nbirth <- lm(medium~cows.nbirth,data=cows.effect2nddosemedium.nbirth)
summary(lcows.effect2nddosemedium.nbirth)

# Effect of the first low dose.
cows.effect1stdoselow.nbirth <- data.frame(t(effect1stdose[3,]),cows.nbirth)
lcows.effect1stdoselow.nbirth <- lm(low~cows.nbirth,data=cows.effect1stdoselow.nbirth)
summary(lcows.effect1stdoselow.nbirth)

# Effect of the second low dose.
cows.effect2nddoselow.nbirth <- data.frame(t(effect2nddose[3,]),cows.nbirth)
lcows.effect2nddoselow.nbirth <- lm(low~cows.nbirth,data=cows.effect2nddoselow.nbirth)
summary(lcows.effect2nddoselow.nbirth)

# None of the linear models has significant parameters.
# It is difficult to say something about the influence of nbirth.

# Influence of nbirth and pcv at time 1

cows.high.time1 <- subset(cowstime1,dose=="H")
cows.medium.time1 <- subset(cowstime1,dose=="M")
cows.low.time1 <- subset(cowstime1,dose=="L")
cows.time1nbirth.effect1stdosehigh <- data.frame(t(effect1stdose[1,]),cows.high.time1)
cows.time1nbirth.effect1stdosemedium <- data.frame(t(effect1stdose[2,]),cows.medium.time1)
cows.time1nbirth.effect1stdoselow <- data.frame(t(effect1stdose[3,]),cows.low.time1)

lcows.time1nbirth.effect1stdosehigh <- lm(high~pcv*nbirth,data=cows.time1nbirth.effect1stdosehigh)
summary(lcows.time1nbirth.effect1stdose)

lcows.time1nbirth.effect1stdosemedium <- lm(medium~pcv*nbirth,
                                            data=cows.time1nbirth.effect1stdosemedium)
summary(lcows.time1nbirth.effect1stdosemedium)

lcows.time1nbirth.effect1stdoselow <- lm(low~pcv*nbirth,data=cows.time1nbirth.effect1stdoselow)
summary(lcows.time1nbirth.effect1stdoselow)

# -------------------------------------------------------------------------------- #


  # Missing values #

cowssinna <- cows[complete.cases(cows),]
length(cowssinna[,1])
summary(cowssinna)
cowsna <- cows[!complete.cases(cows),]
length(cowsna[,1])
summary(cowsna)

# It is clear that no cows died when treated with a high dose. If a cow dies while being treated
# with the medium dose it is always at time 3 whereas it can die at time 2 when treated with 
# the low dose. 

# Mean of the pcv values before the missing value.
(19.3+18.9+20.2+19.9+17.8+17.5+19.8+16.7+17.1)/9

# -------------------------------------------------------------------------------- #

  # Linear model #

lcows <- lm(pcv~., data = cows)
summary(lcows)
par(mfrow=c(2,2))
plot(lcows)
confint(lcows, level=0.95)
lcows.dose.time.id <- lm(pcv ~dose+time+id, data=cows)
summary(lcows.dose.time.id)
confint(lcows.dose.time.id, level=0.95)
par(mfrow=c(1,1))
