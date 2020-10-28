kc=read.csv(choose.files())
data=kc# creating backup
str(kc)
View(kc)
kc$date= substr(kc$date,1,8)
library(lubridate)
#converting to date format
kc$date=ymd(kc$date)
str(kc)
#dropping unnecessary columns
kc1=kc[,c(-1,-2,-18,-19,-20,-21)] #or
#kc1=kc[, c(-which(colnames(kc)=="date"),-which(colnames(kc)=="id"))]
kc1[]
View(kc1)
str(kc1)
par(mfrow=c(1,1))
summary(kc1)
# conversion to appropiate data type
attach(kc1)
kc1 = within(kc1,{
  bedrooms= as.numeric(bedrooms)
  waterfront= factor(waterfront)
  condition = factor(condition,ordered = TRUE)
  view=factor(view)
  grade=as.numeric(grade)
  floors=factor(floors,ordered = TRUE)
  zipcode= factor(zipcode)
})
#data transformation 
table(kc1$bathrooms)
kc1$bathrooms[kc1$bathrooms>0 & kc1$bathrooms<1]=ceiling(kc1$bathrooms[kc1$bathrooms>0 & kc1$bathrooms<1])
kc1$bathrooms=round(kc1$bathrooms)
table(kc1$bathrooms)
kc1$bathrooms=as.numeric(kc1$bathrooms)
str(kc1)
View(kc1)
table(zipcode)
#detecting outlier
options(scipen = 111)
hist(kc1$price)
boxplot(kc1$price)
quantile(kc1$price,seq( 0,1,0.05))
quantile(kc1$price,seq( 0.9,1,0.01))
quantile(kc1$price,seq( 0.99,1,0.001))
#removing outliers
kc1[kc1$price>3500000,]
table(kc1$view)
table(kc1$grade)
plot(density(kc$price))
kc2=kc1[kc1$price<=3500000,]
nrow(kc1)-nrow(kc2)
boxplot(kc2$price)
quantile(kc2$price,seq( 0,1,0.05))
quantile(kc2$price,seq( 0.9,1,0.01))
kc2=kc1[kc1$price<=2000000,]
apply(kc2,2,function(x) sum(is.na(x)))
boxplot(kc2$price)
quantile(kc2$price,seq( 0.9,1,0.01))
nrow(kc1)-nrow(kc2)
#outliers removed
# data visualization charts
detach(kc1)
View(kc2)
scatter.smooth(kc2$price~kc2$bedrooms)
plot(kc2$price~kc2$bedrooms)
cor(kc2$price,kc2$bedrooms)
scatter.smooth(kc2$price~kc2$bathrooms)
cor(kc2$price,kc2$bathrooms)
plot(kc2$price~kc2$sqft_living)
plot(kc2$price~kc2$sqft_lot)
cor(kc2$sqft_living,kc2$sqft_lot)#checking multicollinearity
cor(kc2$sqft_living,kc2$price)
cor(kc2$price,kc2$sqft_lot)
scatter.smooth(kc2$price~kc2$floors)
tapply(kc2$price,kc2$waterfront==0,FUN = mean)
mean(kc2$price)
scatter.smooth(kc2$price~kc2$waterfront)
boxplot(kc2$price~kc2$waterfront,outline=FALSE)
boxplot(kc2$price~kc2$view)
boxplot(kc2$price~kc2$condition)
boxplot(kc2$price~kc2$grade)
plot(kc2$price~kc2$grade)
cor(kc2$price,kc2$grade)
cor(kc2$sqft_living,kc2$sqft_above)
plot(kc2$price~kc2$sqft_basement)
cor(kc2$price,kc2$sqft_basement)
plot(kc2$price~kc2$yr_built)
tapply(kc2$price,kc2$zipcode,FUN = mean)
tapply(kc2$price,kc2$yr_renovated==0,FUN = mean)
kc2$renovate = ifelse(kc2$yr_renovated==0,"N","Y")
kc2$renovate= as.factor(kc2$renovate)
boxplot(kc2$price~kc2$renovate)
plot(kc2$price[kc2$sqft_basement!=0]~kc2$sqft_basement[kc2$sqft_basement!=0])
cor(kc2$price[kc2$sqft_basement!=0],kc2$sqft_basement[kc2$sqft_basement!=0])
colnames(kc2)
#data split
set.seed(11)
library(caTools)
smpl= sample.split(kc2,0.8)
train.kc2= subset(kc2, smpl==T)
test.kc2 = subset(kc2,smpl==F)

#models

m1=lm(train.kc2$price~.,data = train.kc2)
summary(m1)


m2= lm(train.kc2$price~ bedrooms + bathrooms + sqft_living + I(waterfront==1) + view +
         I(condition==1)+ grade + sqft_basement + zipcode + yr_built
       + I(renovate=="Y") + I(floors==1) + sqft_lot ,data = train.kc2)
summary(m2)

# AIC Usage
library(MASS)
sa= stepAIC(m1)
summary(sa)

?anova(m1,m2)

#tests multi collinearity
library(car)
vif(m2)

#fitting prediction $ MAPE
fitted(m2)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))
train.kc2$predi = fitted(m2)

library(Metrics)
mape(train.kc2$price,train.kc2$predi)


#errors analysis, null hypo = errors are normally distributed
plot(density(train.kc2$predi))
library(nortest)
ad.test(m2$residuals) #not normallly distributed
pearson.test(m2$residuals)
qqnorm(m2$residuals)

#auto correlation, null hypo= no auto correlation..value betw 1-3
durbinWatsonTest(m2)

#checking homoscedasticity, null hypo = error is homogeneous
plot(train.kc2$price- train.kc2$predi)
library(lmtest)
bptest(m2) # errors not homogeneous

ncvTest(m2)

# applying on test data

test.kc2$pred = predict(m2,test.kc2)
test.kc2$price-test.kc2$pred

mape(test.kc2$price,test.kc2$pred)
plot(test.kc2$price-test.kc2$pred)


