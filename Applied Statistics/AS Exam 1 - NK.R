#Exam 1
#Nate Kaduk
#Professor Li
#10/11/2022

#Load library
library(alr4)

#Output head to see what I am dealing with
head(water)

#Make vectors
BSVec <- water[, "BSAAM"]
OPVec <- water[,"OPSLAKE"]

#1
#Wording says predict BSAAM runoof volume using OPSLAKE
#Predictor is OPSLAKE, response is BSAAM

#Predictor: OPSLAKE = x
#Response: BSAAM = y


#2
#Make Scatterplot
plot(OPVec, BSVec, main = "Water Scatterplot, OPSLAKE V BSAAM")

#The lines travel in an upward, linear direction.  
#From this, I take that as OPVec increases, so does BSVec

#3

#Find means of x and y
xBar <- mean(OPVec)
yBar <- mean(BSVec)

#Find the sum values
SXX<- sum((OPVec-xBar)^2)
SXY<- sum((OPVec-xBar)*(BSVec-yBar))
SYY<-sum((BSVec-yBar)^2)

#Create estimators
beta1Hat<- SXY/SXX
beta0Hat<- yBar - xBar*beta1Hat

#Find residual sum of Squares
RSS <- sum((BSVec-(beta0Hat+beta1Hat*OPVec))^2)
#Degrees of freedom is the length of the x or y vector minus two
#Doesn't matter whether you choose x or y, as both have the same length
df <- length(OPVec)-2
#Get sigma squared estimate
sigma2Hat <- RSS/df

#Output results
print("Estimates")
beta1Hat
beta0Hat
print("Variance estimate: ")
sigma2Hat



#beta1Hat = 3752.486
#beta0Hat = 27014.59
#sigma2Hat = 79610011

#######################################
#Just wanted to check sigma2Hat again, as its value is massive
a<- beta0Hat+beta1Hat*OPVec
b<- BSVec
(b-a)^2
sum((b-a)^2)/41
yHat = beta0Hat + beta1Hat*BSVec
res = BSVec - yHat
plot(BSVec, res)
plot(OPVec, res)
#Seeing as the residuals are massive, I think the variance should be correct.

######################################



#4
#Find variance
#B0H means beta0Hat
#B1H means beta1Hat
varB0H <- sigma2Hat*(1/length(OPVec)+(xBar^2)/SXX)
varB1H <- sigma2Hat/SXX

#Find Standard deviation
seB0H <- sqrt(varB0H)
seB1H <- sqrt(varB1H)

print("Standard errors: ")
seB0H
seB1H


#5
#Add regression line to plot
plot(OPVec, BSVec, main = "Water Scatterplot, OPSLAKE V BSAAM")
#Take results from 3 to make line
abline(a=beta0Hat, b=beta1Hat)

#6
#H0: B1=0
#Ha: B1 > 0

B1Star = 0

tStat = (beta1Hat-B1Star)/seB1H



#Output
print("T-Statistic and p-value: ")
tStat
1-pt(abs(tStat), df)


#The p-value of 0 is less than the level of significance for 0.01.  As a result, there exists
#sufficient enough evidence to reject the null hypothesis.  In other words, there exists enough
#evidence to suggest the slope is not zero, or there is a relation between OPVec and BSVec.



#7
#The confidence interval is for slope, so the value before the +/- should be beta1Hat

#Confidence level alpha
alphaCon <- .01
lowerBoundCI <-beta1Hat - qt(1-alphaCon/2, df)*seB1H
upperBoundCI <- beta1Hat + qt(1-alphaCon/2, df)*seB1H

#Output
print("Bounds for 99% confidence interval: ")
lowerBoundCI
upperBoundCI

#The lower bound is 3169.759
#The Upper bound is 4335.213
#I am 99% confident that beta1 is between 3169.759 and 4335.213


#8
#Get predictive value
xStar <- 22

#Initialize y tilde star
yTilStar <- beta0Hat+beta1Hat*xStar

#Big portion in standard error formula
bigPart<- ((xStar-xBar)^2)/SXX
#Standard error for yTilStar
seYTS <- sqrt(sigma2Hat*(1+1/length(OPVec)+bigPart))

#Alpha for predictive interval
alphaPI<- 0.01

lowerBoundPI<- yTilStar-qt(1-alpha/2, df)*seYTS
upperBoundPI <- yTilStar+qt(1-alpha/2, df)*seYTS

#Output
print("Lower and upper bounds for 99% predictive interval: ")
lowerBoundPI
upperBoundPI

#Lower Bound: 84693.98
#Upper Bound: 134444.6

#There is a 99% chance that an OPVec value of 22 would lie between 84693.98 and 134444.6 for
#its BSVec y-values
