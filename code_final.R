library(dplyr)
library(MASS)
library(faraway)
require(leaps)
library(Rcmdr)
library(Metrics)

#load the cleaned data
setwd('')

data = read.csv('lyft_clean.csv')

#sampling the data
sample_sub = sample(nrow(data), 4000, replace = F)
sample_test = sample(nrow(data), 4000, replace = F)
data_test = data[sample_test,]
data_sample = data[sample_sub, ]

#categorize some variables and delete some variables 
data = data_sample %>%
  mutate(
    source = factor(source),
    destination = factor(destination),
    order_type = factor(order_type),
    is_rain = factor(rain),
    time_range = factor(time_range)) %>%
  dplyr::select(-X,-id,-rain,-time,-hour) 
summary(data)

#full model
lmod_full = lm(price~., data)
summary(lmod_full)

par(mfrow=c(2,2))
plot(lmod_full) #Residual plot seems not to be random, QQ plots are skewed
shapiro.test(residuals(lmod_full)) #There might be some influential/outliers


#predictors pairwise relationship
par(mfrow=c(2,4))
for(i in c(3:10)){
  x <- names(data)[i]
  qqnorm(data[,i],xlab=x)
  qqline(data[,i])
} #some variables are skewed, need some transformation

#boxcox
par(mfrow=c(2,4))
for(i in c(3:10)){
  x <- names(data)[i]
  boxcox(I(data[,i]+0.001)~1,xlab=x, seq(-2, 3, 0.1))
}#do some transformation

par(mfrow=c(2,3))
x <- names(data)[3]
boxcox(I(data[,3]+0.001)~1,xlab=x, seq(0.2, 0.6, 0.05)) #distance boxcox

x <- names(data)[6]
boxcox(I(data[,6]+0.001)~1,xlab=x, seq(1.5, 2.5, 0.05)) #temp boxcox

x <- names(data)[10]
boxcox(I(data[,10]+0.001)~1,xlab=x, seq(0.4, 0.8, 0.05)) #temp boxcox



#transform predictors
lmod_transP = lm(price~source + destination + I(distance^(0.5)) + surge_multiplier + 
                   I(temp^2) + clouds + pressure + humidity + I(wind^(0.5)) + order_type +
                   is_rain + time_range, data)
summary(lmod_transP)

par(mfrow=c(2,2))
plot(lmod_transP) #QQ plot is a little better now but still skew, need response transformation
plot(lmod_full)

par(mfrow=c(3,3))
termplot(lmod_transP, terms = 3:9, partial.resid=TRUE)#Partial Residual plots seem to be ok

#response transformation
boxcox(lmod_transP, seq(0.3,0.5,.05)) #lamda = 0.4

lmod_trans = lm(price^(0.4)~source + destination + I(distance^(0.5)) + surge_multiplier + 
                  I(temp^2) + clouds + pressure + humidity + I(wind^(0.5)) + order_type +
                  is_rain + time_range, data)
summary(lmod_trans)

par(mfrow = c(2,1))
residTrans<-residuals(lmod_trans)
fitTrans<-fitted(lmod_trans)
plot(fitTrans,residTrans,main="Post Transform Residual Plot",pch=16)
abline(h=0)
qqnorm(residTrans,main="Post Transform QQPlot")
qqline(residTrans) 

#outliers
studres<-rstudent(lmod_trans)
sort(studres)
out.ind = which(abs(studres) > 3)
data = data[-out.ind, ]

par(mfrow = c(1,1))
lev = hatvalues(lmod_trans) #extract the leverage values
labels = row.names(data)
halfnorm(lev, labs=labels,ylab='Leverages'
         , xlim=c(0,4))
cook = cooks.distance(lmod_trans) #find Cook's distance
halfnorm(cook,labs=labels,ylab="Cook's distance",xlim=c(0,4))
data = data[-c(79131, 230695, 81586), ]

lmod_out = lm(price^(0.4)~source + destination + I(distance^(0.5)) + surge_multiplier + 
                I(temp^2) + clouds + pressure + humidity + I(wind^(0.5)) + order_type +
                is_rain + time_range, data)
summary(lmod_out)

par(mfrow = c(2,2))
residTrans<-residuals(lmod_out)
fitTrans<-fitted(lmod_out)
plot(fitTrans,residTrans,main="Post Transform Residual Plot",pch=16)
abline(h=0)
qqnorm(residTrans,main="Post Transform QQPlot")
qqline(residTrans) #Much higher R, and Residual and QQ plot are much better


#Stepwise forward selection based on BIC
obj_BIC <- stepwise(lmod_out,direction="forward/backward",criterion="BIC",trace=F)
summary(obj_BIC) 
#formula = price^(0.4) ~ order_type + I(distance^(0.5)) + surge_multiplier + source + destination
#Stepwise forward selection based on AIC
obj_AIC <- stepwise(lmod_out,direction="forward/backward",criterion="AIC",trace=F)
summary(obj_AIC) 
#formula = price^(0.4) ~ order_type + I(distance^(0.5)) + surge_multiplier + source + destination
AIC(lmod_out)
AIC(obj_AIC) 
AIC(obj_BIC)

BIC(lmod_out)
BIC(obj_AIC) 
BIC(obj_BIC)

lmod_stepwise = obj_AIC
#same models with the same BIC score and AIC score
#model = price^(0.4) ~ order_type + I(distance^(0.5)) + surge_multiplier + source + destination

residTrans<-residuals(lmod_stepwise)
fitTrans<-fitted(lmod_stepwise)
plot(fitTrans,residTrans,main="Post Transform Residual Plot",pch=16)
abline(h=0)
qqnorm(residTrans,main="Post Transform QQPlot")
qqline(residTrans) #model = model_stepwise

#Interaction
lmod_int = lm(price^(0.4) ~ distance*surge_multiplier + order_type + I(distance^(0.5)) + surge_multiplier + source + destination, data)

AIC(lmod_stepwise)
AIC(lmod_int)#better
summary(lmod_int)
anova(lmod_stepwise, lmod_int) #p < 0.05, so we choose the lmod_int as final model

AIC(lmod_full)
AIC(lmod_trans)
AIC(lmod_stepwise)
AIC(lmod_int)#best score

par(mfrow = c(1,2))
residTrans<-residuals(lmod_int)
fitTrans<-fitted(lmod_int)
plot(fitTrans,residTrans,main="Post Transform Residual Plot",pch=16)
abline(h=0)
qqnorm(residTrans,main="Post Transform QQPlot")
qqline(residTrans)


rmse(fitted(lmod_int),data$price)
rmse(predict(lmod_int,data_test),data_test$price)
summary(lmod_int)
#model2
lmod1 <- lm(price~.,data=lyft_clean)
boxcox(lmod1)

plot(lmod1)
summary(lmod1)
lmod2 <- lm(log(price)~.,data=lyft_clean)
summary(lmod2)
plot(lmod2)
lmod3 <- lm(price^(0.4)~.,data=lyft_clean)
summary(lmod3)
plot(lmod3)#choose lmod3
#outlier
par(mfrow=c(1,2))
lev <- hatvalues(lmod3) #extract the leverage values
labels <- row.names(lyft_clean)
halfnorm(lev,labs=labels,ylab="Leverages",xlim=c(0,6))
cook <- cooks.distance(lmod3)#find Cook's distance
halfnorm(cook,labs=labels,ylab="Cook's distance",xlim=c(0,6))
studres <- rstudent(lmod3)
range(studres)
out.ind <- which(abs(studres)>3)
lyft_clean[out.ind,]
lyft_clean1new = lyft_clean[-out.ind,]
lmod3new <- lm(price^(0.4)~.,data=lyft_clean1new)
summary(lmod3new)
#stepwise
obj1 <- stepwise(lmod3new,direction="forward/backward",criterion="AIC",trace=F)
summary(obj1)
#interation term
new1_int = lm(formula = price^(0.4) ~ order_type + distance+ distance * surge_multiplier 
              + surge_multiplier + source + destination + is_rain +clouds, data = lyft_clean1new)
summary(new1_int)
plot(new1_int)
AIC(new1_int)
new2_int = lm(formula = price^(0.4) ~ order_type + distance+ distance * surge_multiplier 
              + surge_multiplier + source + destination + clouds+is_rain +temp + pressure, data = lyft_clean1new)
anova(new1_int,new2_int)
summary(new2_int)
AIC(new1_int)
AIC(new2_int)
obj2 <- stepwise(new2_int,direction="forward/backward",criterion="AIC",trace=F)
obj2
summary(obj2)
obj3 <- stepwise(new2_int,direction="forward/backward",criterion="BIC",trace=F)
summary(obj2)

#model 3

data_xr = data_sample %>%
  mutate(
    price_mult = price / surge_multiplier
  ) %>%
  dplyr::select(-price, -surge_multiplier)


lmod_xr =  lm(I(price_mult^(0.3)) ~ order_type + I(distance^(0.5)) + humidity + wind +
                source + pressure + clouds + is_rain + temp + destination, data=data_xr)

par(mfrow=c(1,1))
lev <- hatvalues(lmod_xr) #extract the leverage values
labels <- row.names(lmod_xr)
halfnorm(lev,labs=labels,ylab="Leverages",xlim=c(0,4))
cook <- cooks.distance(lmod_xr)#find Cook's distance
halfnorm(cook,labs=labels,ylab="Cook's distance",xlim=c(0,4))
studres <- rstudent(lmod_xr)
range(studres)
out.ind <- which(abs(studres)>3)
data_xr[out.ind,]
data_xr = data_xr[-out.ind,]
lmod_xr =  lm(I(price_mult^(0.3)) ~ order_type + I(distance^(0.5)) + humidity + wind +
                source + pressure + clouds + is_rain + temp + destination, data=data_xr)
summary(lmod_xr)

obj1 <- stepwise(lmod_xr,direction="forward/backward",criterion="AIC",trace=F)
obj2 <- stepwise(lmod_xr,direction="forward/backward",criterion="BIC",trace=F)
obj1
obj2

lmod_xr_AIC = lm(price_mult^(0.3)~ order_type + I(distance^(0.5)) + 
                   source + destination, data = data_xr)

summary(lmod_xr_AIC)
plot(lmod_xr_AIC)

#Compare
AIC(lmod_xr_AIC)
AIC(lmod_int)

