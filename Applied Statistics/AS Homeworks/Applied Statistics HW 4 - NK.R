#Nate Kaduk
#Applied Statistics HW IV
#Professor Li
#9/29/2022-10/6/2022

library(alr4)
ordLSE<-function(data){
  #Create vector of the numeric values of data columns
  x=data[,1]
  y=data[,2]
  
  #Initialize the means
  xBar <-mean(x)
  yBar <-mean(y)
  
  
  #Initialize sum of squares for each part
  SXX <- sum((x-xBar)^2)
  SYY <- sum((y-yBar)^2)
  SXY<- sum((x-xBar)*(y-yBar))
  
  #Create estimators
  hatBeta1 <- SXY/SXX
  hatBeta0 <- yBar-(hatBeta1 * xBar)
  
  #Add line with those estimators
  abline(a= hatBeta0, b=hatBeta1)
}

#2.13.3
#Create vectors for response and predictor
mHeight<- Heights[,"mheight"]
dHeight<- Heights[,"dheight"]

#Standard x, y for mHeight and dHeight
y<- dHeight
x<-mHeight

yBar <- mean(y)
xBar <- mean(x)

#Find sum of differences for use in beta estimates
SXX <- sum((x-xBar)^2)
SYY <- sum((y-yBar)^2)
SXY<- sum((x-xBar)*(y-yBar))

#Find Beta1Hat
beta1Hat <- SXY/SXX

#Find beta0Hat:
beta0Hat <- yBar - beta1Hat*xBar

#Find xStar for the predicted value
xStar <- 64
#Find yTilStar
yTilStar <- beta0Hat + beta1Hat*xStar

#Create a size variable for n in the formulas and for the degrees of freedom
size <- length(x)

#Find deltaHat
dhNum <- sum((y-(beta0Hat+beta1Hat*x))^2)
deltaSQHat <- dhNum/(size-2)

#Standard Error for yTilStar
seYTS <- sqrt(deltaSQHat*(1+(1/size)+(((xStar-xBar)^2)/SXX)))




#Alpha for (1-alpha)*100% predictive interval
alpha <- .01

yTilStar - qt(1-alpha/2, length(x)-2)*seYTS
yTilStar + qt(1-alpha/2, length(x)-2)*seYTS


#The 99% predictive interval reveals the daughter's height to be between 
#58.74045 and 70.43805 for a mother with a height of 64 inches


#######################################################################################################
#2.16.1
#See first few values of UN11 to know what I am doing
head(UN11)

#Make a vectors based on ppgdp and fertility
ppgdpCol <- UN11[, 4]
fertilityCol <- UN11[, 3]

#Take the log of those vectors
ppgdpColLog <- log(ppgdpCol)
fertilityColLog <- log(fertilityCol)

#Find mean and sum
xBar <- mean(ppgdpColLog)
yBar <- mean(fertilityColLog)
SXX <- sum((ppgdpColLog-xBar)^2)
SYY <- sum((fertilityColLog-yBar)^2)
SXY <- sum((ppgdpColLog-xBar)*(fertilityColLog-yBar))

#Estimate betahats
beta1Hat <- SXY/SXX
beta0Hat <- yBar - beta1Hat*xBar

#Get the rediual sum of squares and degrees of freedom
RSS<- sum((fertilityColLog-(beta0Hat+beta1Hat*ppgdpColLog))^2)
df <- length(ppgdpColLog)-2

#Find deltaHat
deltaHat <- RSS/df



#Print out Estimates (Not worried about plotting, as next question does the plot)
print("Beta1Hat: ")
beta1Hat
print("beta0Hat: ")
beta0Hat
print("deltaHat: ")
deltaHat

#Use summary of the linear regression model to check work
UNModel <- lm(fertilityColLog ~ ppgdpColLog)
summary(UNModel)

#beta1Hat and beta0Hat estimates are the same, being around -0.207 and 2.67, respectively.  
#The residual standard error is the same as the sqrt root of the deltaHat estimate.

resid(UNModel)

plot(fitted(UNModel), resid(UNModel))
abline(a=0, b=0)

###############################################################################
#2.16.2
#Plot log(fertility) vs log(ppgdp)
plot(ppgdpColLog, fertilityColLog, xlab = "ppgdp", ylab = "Fertility")
title("Fertility Log Scatterplot")

regLineData <- cbind(ppgdpColLog, fertilityColLog)
ordLSE(regLineData)

#########################################################################
#2.16.3
#H0: Beta1 = 0
#Ha: Beta1 < 0

#Make point of reference
beta1Star <- 0

#Find the numerator and denominator for beta1Hat
hatBeta1Num <- sum((ppgdpColLog-mean(ppgdpColLog))*(fertilityColLog-mean(fertilityColLog))) 
hatBeta1Den<-sum((ppgdpColLog-mean(ppgdpColLog))^2)
#Initialize hatbeta1
beta1Hat <- hatBeta1Num/hatBeta1Den

#Create beta0hat for deltaSQHat
beta0Hat <- mean(fertilityColLog)-beta1Hat*mean(ppgdpColLog)

#Create deltaSQHat for the standard error of beta1Hat
dhNumSum <- sum((fertilityColLog-(beta0Hat+beta1Hat*ppgdpColLog))^2)
deltaSQHat <- dhNumSum/(length(ppgdpColLog)-2)

#Find standard error for beta1Hat
seBeta1Hat <- sqrt(deltaSQHat/hatBeta1Den)

#Find t-statistic
tStat <- (beta1Hat-beta1Star)/seBeta1Hat

#Find p-value
1-pt(abs(tStat), length(ppgdpColLog)-2)

#The p-value obtained (0) is less than the level of significance, 0.05.  Therefore, there exists sufficient 
#evidence to reject the null hypothesis in favor of Ha.

###################################################
#2.16.5
#Create x,y based on the log values
x<- ppgdpColLog
y<-fertilityColLog

#Find means
yBar <- mean(y)
xBar <- mean(x)

#Find sum of differences for use in beta estimates
SXX <- sum((x-xBar)^2)
SYY <- sum((y-yBar)^2)
SXY<- sum((x-xBar)*(y-yBar))

#Find Beta1Hat
beta1Hat <- SXY/SXX

#Find beta0Hat:
beta0Hat <- yBar - beta1Hat*xBar

#Find xStar for the predicted value
xStar <- log(1000)
#Find yTilStar
yTilStar <- beta0Hat + beta1Hat*xStar

#Create a size variable for n in the formulas and for the degrees of freedom
size <- length(x)

#Find deltaHat
dhNum <- sum((y-(beta0Hat+beta1Hat*x))^2)
deltaSQHat <- dhNum/(size-2)

#Standard Error for yTilStar
seYTS <- sqrt(deltaSQHat*(1+(1/size)+(((xStar-xBar)^2)/SXX)))


#Alpha for (1-alpha)*100% predictive interval
alpha <- .05

print("Predictive interval for logFertility: ")

aValue<- yTilStar - qt(1-alpha/2, length(x)-2)*seYTS
aValue
bValue <-yTilStar + qt(1-alpha/2, length(x)-2)*seYTS
bValue

#Predictive interval for regular fertility
print("The value for regular fertility is between ")
exp(aValue)
print(" and ")
exp(bValue)

