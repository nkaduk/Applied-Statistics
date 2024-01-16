#Applied Statistics HW III
#Nate Kaduk
#Professor Li
#9/22/2022 - 9/29/2022

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
  
  #Output the hat betas for problem 2.13.1
  print("This is hatBeta1: ")
  print(hatBeta1)
  print("This is hatBeta0: ")
  print(hatBeta0)
  
  #Add line with those estimators
  abline(a= hatBeta0, b=hatBeta1)
}

#Load Library
library(alr4)

###############################################################################
#2.6.1
#See the beginning values
head(ftcollinstemp)

#Create vectors from the dataset so I can plot values
fallVector <- ftcollinstemp[, 'fall']
winterVector <- ftcollinstemp[,'winter']

#Create plot
#Fall predicts in the study, so it is the predictor here
plot(fallVector, winterVector, xlab="Fall", ylab="Winter",main="ftCollins Fall V Winter")

#As expected, winter values are lower than their fall counterparts.
#There are a few points scattered up and down the ride side of the graph
#but the vast majority of points seem to travel in a cluster with an upward slope.
#As a result, I believe these values have a positive correlation.


#######################################################################
#2.6.2

#Add a regression line
ordLSE(cbind(fallVector, winterVector))

#H0: beta1 = 0, or the slope for the regression line is zero
#Ha: beta1 does not = 0, or the slope for the regression line is not zero

#Make point of reference (to check whether or not beta1=beta1Star)
beta1Star <- 0

#Make numerator and denominator for beta1hat
hatBeta1Num <- sum((fallVector-mean(fallVector))*(winterVector-mean(winterVector))) 
hatBeta1Den<-sum((fallVector-mean(fallVector))^2)
#Initialize hatbeta1
hatBeta1 <- hatBeta1Num/hatBeta1Den

#Find beta0hat (standard error of hatBeta1 requires deltaHat which needs beta0Hat)
beta0Hat <- mean(winterVector)-hatBeta1*mean(fallVector)

#Create numerator
dhNumSum <- sum((winterVector - (beta0Hat+hatBeta1*fallVector))^2)
#Create deltaHat
deltaHat <- dhNumSum/(length(winterVector)-2)

#Get standard error for beta1Hat
seBeta1Hat<-sqrt(deltaHat/hatBeta1Den)

#Compute t-statistic
tStat <- (hatBeta1-beta1Star)/seBeta1Hat

#Output the p-value
print("P-value for beta1 estimate:")
2*(1-pt(tStat, length(winterVector)-2))

#Since the p-value is less than 0.05, there exists enough evidence to reject the null hypothesis.
#In other words, there exists enough evidence to reject the claim that the slope is zero. 

#########################################################################################
#2.13.1

#Create vectors for response and predictor
mHeight<- Heights[,"mheight"]
dHeight<- Heights[,"dheight"]
#Plot values
plot(mHeight,dHeight, xlab = "mheight", ylab = "dheight", main = "Heights")


#This part was moved out of the ORDLSE function so I can use the values later for the estimates.
#Create vector of the numeric values of data columns
x=mHeight
y=dHeight

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

#Output the hat betas for problem 2.13.1
print("This is hatBeta1: ")
print(hatBeta1)
print("This is hatBeta0: ")
print(hatBeta0)

#Note: hatBeta0 is the intercept when dheight=0 (an impossibility considering heights can't be zero). 
#The intercept here is 29.9, but this is just so that the line can fit the values given.
#If heights normally started near (0,0), I would expect the intercept to be less and have an increase in slope.
#The slope estimator is in-line with what the y-values are supposed to be.   
#By plugging in x-values to y=0.542x + 29.9, you can get about where the daughter's height is.

#Add line with those estimators
abline(a= hatBeta0, b=hatBeta1)



#Create deltaHat and output
#dhNumSum <- sum((mHeight - (hatBeta0+hatBeta1*dHeight))^2)
#CHECK WHICH IS Y AND WHICH ONE IS X!
dhNumSum <- sum((dHeight - (hatBeta0+hatBeta1*mHeight))^2)
deltaHat <- dhNumSum/(length(mHeight)-2)
print("Estimate for delta:")
deltaHat

#The standard deviation is not perfect, but considering the values we are dealing with, it is a solid estimate (8.3).

#Create variance estimates and output
varHatBeta0 <-deltaHat*(1/length(mHeight)+xBar^2/SXX)
varHatBeta1 <-deltaHat/SXX

print("Variance estimate for beta0hat:")
varHatBeta0
print("Variance estimate for beta1hat")
varHatBeta1

#VarHatBeta0 has a small bit of variance (4.25) but not too much.
#As for varHatBeta1, I know the slope estimate was small in general, but
#a variance that steady (0.001), even if it is just an estimate almost seems too good to be true.

#Turns out the part in the square root is the same as the variance estimate
#Find se hats and output them
seBeta0Hat <- sqrt(varHatBeta0)
seBeta1Hat<-sqrt(varHatBeta1)
print("Standard error for beta0Hat:")
seBeta0Hat
print("Standard error for beta1Hat:")
seBeta1Hat

#Considering the values here are double-digit, the standard error for beta0Hat is a solid 2.06.
#The standard error for beta1Hat is even better, at 0.03298


#Create sum of squares due to regression.
#SSReg <- (SYY - ((SXY)^2/SXX))

SSReg <- ((SXY)^2)/SXX
#Compute Coefficient of Determination

coDet <- SSReg/SYY
print("This is the coefficent of determination: ")
coDet

#The coefficient of determination stands at 0.76, meaning there is a decent relationship
#between daughter height and mother height.


#Overall Summary:
#The estimate for the variance/standard error is small, which coincides with the graph having
#a large cluster of points in the middle.  The estimates for beta1 and beta0 are used to produce
#the regression line shown in the "Heights" graph, which is a fair fit.


###########################################################################
#2.13.2
alpha <- .01

#Print out confidence intervals
print("A 99% confindence interval reveals hatBeta1 to be between")
hatBeta1 - qt(1-alpha/2, length(dHeight)-2)*seBeta1Hat
print(" and ")
hatBeta1 + qt(1-alpha/2, length(dHeight)-2)*seBeta1Hat

#Note: The estimate for beta1Hat is 0.5417, which is in the interval.

#We are 99% confident that the value for the slope is in the interval 0.475 to .609
