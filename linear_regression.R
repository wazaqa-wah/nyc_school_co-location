install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("segmented")



eMdata <- read.csv('/Users/ziqiwang/Desktop/co-location/Data and script/elementary_and_middle_school_data.csv')


names(eMdata) <- c('index','dbn','ela','math','coLocated','borough','econNeed','enrollment','disability','ell','poverty')
summary(eMdata)
sapply(eMdata, class) 

eMdata[is.na(eMdata) | eMdata == "Inf"] <- NA
#1st time: just to see 2 scores and if the school is co-located
mlm1 <- lm(cbind(ela, math) ~ coLocated, data = eMdata)
summary(mlm1)

vcov(mlm1)
cov2cor(vcov(mlm1))


#2nd time: 2 score, co-located, and economic need index
mlm2 <- lm(cbind(ela, math) ~ coLocated + econNeed, data = eMdata)
summary(mlm2)

cov2cor(vcov(mlm2))


#3rd time: 2 score, co-located, disability, ell,  economic need index, poverty
mlm3 <- lm(cbind(ela, math) ~ coLocated + econNeed + disability + ell + poverty, data = eMdata)
summary(mlm3)

#ela: r: 0.1543, better
#math: R-squared:0.06245, eh; p-value small

#4th time: 2 score, co-located, borough, disability, ell,  economic need index, poverty
mlm4 <- lm(cbind(ela, math) ~ coLocated*borough + econNeed + disability + ell + poverty, data = eMdata)
summary(mlm4)

mlm9 <- lm(cbind(ela, math) ~ coLocated + borough + econNeed + disability + ell + poverty + enrollment, data = eMdata)
summary(mlm9)

# did a little test and found out that the data is not linear
res <- resid(mlm4)
plot(fitted(mlm4), res)
qqnorm(res)
qqline(res) 

plot(eMdata$coLocated, eMdata$math, pch = 19, col = "black")

#Jiacheng recommended me to have R pick a model for me, so here's a little experiement
eMdata1 <- eMdata %>% select(-c('index','dbn','ela','openYear','management','enrollment')) %>% na.omit()
#have R pick model
intercept_only <- lm(math ~ 1, data=eMdata1, na.action = na.omit)
all <- lm(math ~ ., data=eMdata1, na.action = na.omit)

forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward$anova
forward$coefficients
summary(forward)

forward1 <- step(intercept_only, direction='both', scope=formula(all))

#back to high school linear regression
hData <- read.csv('/Users/ziqiwang/Desktop/co-location/Data and script/high_school_data.csv')


names(hData) <- c('index','dbn','fouryear','sixyear','coLocated','borough','econNeed','enrollment','disability','ell','poverty')
summary(hData)
sapply(hData, class) 

hData[is.na(hData) | hData == "Inf"] <- NA

#1st time: just to see 2 scores and if the school is co-located
mlm5 <- lm(cbind(fouryear, sixyear) ~ coLocated, data = hData)
summary(mlm5)

#3rd time: 2 score, co-located, disability, ell,  economic need index, poverty
mlm6 <- lm(cbind(fouryear, sixyear) ~ coLocated + econNeed + disability + ell + poverty, data = hData)
summary(mlm6)

#4th time: 2 score, co-located, borough, disability, ell,  economic need index, poverty
mlm7 <- lm(cbind(fouryear, sixyear) ~ coLocated*borough + econNeed + disability + ell + poverty, data = hData)
summary(mlm7)

mlm8 <- lm(cbind(fouryear, sixyear) ~ coLocated+borough + econNeed + disability + ell + poverty + enrollment, data = hData)
summary(mlm8)

#just curious about the residuel
res1 <- resid(mlm7)
plot(fitted(mlm7), res)
qqnorm(res1)
qqline(res1)
