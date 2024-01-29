# We have incomplete information on the electricity consumption for two years (2017 and 2018) of an industry
# The objective is to try to make a graphical visualization on those two years (Only for the first 07 months)
#by assigning values to the missing data (some months don't have data)

## Value for the first year (Kw)

puissance2017= c(355,350,355,340,335,305)
mean(puissance2017)

## Value for the second year (Kw)

puissance2018= c(420,385,345,340,305)
mean(puissance2018)

## For the month that we have information of the both years, we will try to see
##the correlation between those data

x=c(340,335,305)
y=c(420,385,345)

cov=mean(x*y)-mean(x)*mean(y) #covariance 
mean(x)
mean(y)
varx=mean(x*x)-mean(x)*mean(x) #variance for x
vary=mean(y*y)-mean(y)*mean(y) #variance for y
cor=cov/sqrt(varx*vary) #Correlation

cor # The correlation is high, so we will predict the missing data by using a linear regression 

predictor= lm(y1~x1)
summary(resultat)
abline(resultat) ## To add the line
coef(resultat) ##to see the parameters of the regression

## We will make predictions on the missing month by using the information of months that we have
##on the same month for the other year.
## For example, for fev 2017, we have the information, we will predict fev 2018 by doing

1.860465*(350)-224.418605

##Plot 


puissance2017 <- c(355,350,355,340,335,305,303.38) ## the new vector with the predicted value
puissance2018 <-c(436.05,426.74,436.05,420,385,345,340) ##the new vector with the predicted value

facture=data.frame(puissance2017,puissance2018) ## A data frame with the information of the both dataframe

seriefacture=ts(c(puissance2017,puissance2018,frequency=1)) ##create the time series

plot.ts(c(puissance2017,puissance2018),ylab="puissance en kw",xlab="periode",xaxt="n")
axis(1, at=c(2,4,6,8,10,12,14),labels=c("fev2017","avr2017","juin2017","jan2018","mars2018","mai2018","juillet2018"))
par(new=T)
curve(371+0*x,add=T,col="red") ##the line for the median
help(legend)
legend("topleft","médiane",lty=1,col="red")
a=mean(puissance2017)
b=mean(puissance2018)
(a+b)/2
