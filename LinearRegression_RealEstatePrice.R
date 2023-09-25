real_estate<- read.csv("Real estate.csv", header = TRUE, sep=",")

head(real_estate)

boxplot(real_estate$X3.distance.to.the.nearest.MRT.station)
boxplot(real_estate$Y.house.price.of.unit.area)

model = lm(Y.house.price.of.unit.area~X1.transaction.date+X2.house.age+
             X3.distance.to.the.nearest.MRT.station+X4.number.of.convenience.stores
           , data = real_estate)
summary(model)

fit = fitted(model)
fit

error =resid(model)
error

library(car)
vif(model)

#H0: Errors are normally distributed
#H1: Errors are not normally distributed
library(nortest)
lillie.test(error)
shapiro.test(error)
ad.test(error)
#Because p-value < 0.05, we reject H0. Therefore, the errors aren't normally distributed

#H0: Errors are homoscedastic
#H1: Errors are heteroscedastic
library(lmtest)
bptest(model)
#Because p-value > 0.05, we fail to reject H0. Therefore, the errors are homoscedastic

#H0: Errors are nonautocorrelated 
#H1: Errors are autocorrelated
dwtest(model)
#Because p-value > 0.05, we fail to reject H0. Therefore, there's no autocorrelation

library(MASS)
#robust regression
library(robustbase)
mrobust <- lmrob(Y.house.price.of.unit.area~X2.house.age
                 +X3.distance.to.the.nearest.MRT.station
                 +X4.number.of.convenience.stores, data=real_estate)
summary(mrobust)
robust<-rlm(Y.house.price.of.unit.area~X2.house.age
            +X3.distance.to.the.nearest.MRT.station
            +X4.number.of.convenience.stores, data=real_estate)

summary(robust)

# Normality test for robust regression
error3 = resid(mrobust)
lillie.test(error3)

error2 = resid(robust)
error2
lillie.test(error2)