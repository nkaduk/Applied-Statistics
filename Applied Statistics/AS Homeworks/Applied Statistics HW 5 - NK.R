##Nate Kaduk
#Professor Li
#Homework 5


library(alr4)

#Since each slope with have its own estimate, I want to make a function to get an estimate
betaHat <- function(x,y){
  xBar <- mean(x)
  yBar <- mean(y)
  
  SXX <- sum((x-xBar)^2)
  SXY<- sum((x-xBar)*(y-yBar))
  
  betaHat <- SXY/SXX
} 


############################################################################
#3.3.3
#Head to see what I am dealing with
head(BGSgirls)

#Response Vector
bResp <- BGSgirls[, "BMI18"]

#Predictor vectors
HT2Vec <- BGSgirls[, "HT2"]
WT2Vec <- BGSgirls[, "WT2"]
HT9Vec <- BGSgirls[, "HT9"]
WT9Vec <- BGSgirls[, "WT9"]
ST9Vec <- BGSgirls[, "ST9"]

#Compute individual slope estimates
betaHT2 <- betaHat(HT2Vec, bResp)
betaWT2 <- betaHat(WT2Vec, bResp)
betaHT9 <- betaHat(HT9Vec, bResp)
betaWT9 <- betaHat(WT9Vec, bResp)
betaST9 <- betaHat(ST9Vec, bResp)

#Beta hat vector
betaHatVec <- c(betaHT2, betaWT2, betaHT9, betaWT9, betaST9)
#Find the number of entries
sizeN <- length(bResp)


#For the intercept of the beta matrix, repeat 1 a number of times equal to the column length
firstCol = rep(1, sizeN)
#Create betaMatrix
betaMatrix <- matrix(c(firstCol, HT2Vec, WT2Vec, HT9Vec, WT9Vec, ST9Vec), nrow = length(HT2Vec), ncol = 6)
#Make the response vector a matrix to compare with betaMatrix
yMatrix <- matrix(bResp)

#Transpose
betaMatrixTP <- t(betaMatrix)
#Outside the parenthesis in the betahat vector formula
outsidePar <- betaMatrixTP %*% yMatrix

#Inside the parenthesis and inverse it
insidePar <- betaMatrixTP %*% betaMatrix
inversePar <- solve(insidePar)

#Final Bhat vector is the product
finalBVec <- inversePar %*% outsidePar
finalBVec

summary(lm(bResp~HT2Vec+WT2Vec+HT9Vec+WT9Vec+ST9Vec, data=BGSgirls))
#Find values needed for sigma2 and R2
SSReg <- t(finalBVec)%*% insidePar %*% finalBVec
SYY <- t(yMatrix)%*%yMatrix
RSS <- SYY - SSReg
RSS

RSSElem <- finalBVec[1]+finalBVec[2]*HT2Vec+finalBVec[3]*WT2Vec+finalBVec[4]*HT9Vec+finalBVec[5]*WT9Vec+finalBVec[6]*ST9Vec
RSSNext<- sum((bResp-RSSElem)^2)
RSSNext

#Find sigma2 and R2
sigma2 <- RSS/(sizeN-6)
sigma2
#Sigma2 estimate is 4.5787
R2 <- SSReg/SYY
R2
#Coefficient of determination is 0.99

#Compute t-values with H0 and Ha
#beta1
#H0: B1=0
#Ha: B1 !=0


beta1Star<-0
#Estimated variance for multiple linear regression is found by multiplying sigma2 by the elements
#of (X'X)^-1 -> according to book.  The standard error is just the square root of that.

#Make Sum corresponding to only beta1
otherVar <-inversePar[2,2]
#Standard Error for beta1Hat
seBH1<- sqrt(sigma2*otherVar)
tStat1 <- (finalBVec[2]-beta1Star)/seBH1
tStat1
#tStat1 = -1.483


#Two-sided test p-values:

result <- 2*(1-pt(abs(tStat1), sizeN-6))
result

#The p-value is roughly .143, which is greater than .05.  As a result, there exists enough evidence
#to fail to reject the null hypothesis.


#beta2
#H0: B2=0
#Ha: B2 !=0

beta2Star<-0
#Estimated variance for multiple linear regression is found by multiplying sigma2 by the elements
#of (X'X)^-1 -> according to book.  The standard error is just the square root of that.

#Make Sum corresponding to only beta2
otherVar <-inversePar[3,3]
#Standard Error for beta2Hat
seBH2<- sqrt(sigma2*otherVar)
tStat2 <- (finalBVec[3]-beta2Star)/seBH2
tStat2
#tStat: -1.14


#Two-sided test p-values:

result <- 2*(1-pt(abs(tStat2), sizeN-6))
result

#The p-value of .259 is  greater than the level of significance.  Therefore, we can fail to reject
#the null hypothesis.


#beta3
#H0: B3=0
#Ha: B3 !=0

beta3Star<-0
#Estimated variance for multiple linear regression is found by multiplying sigma2 by the elements
#of (X'X)^-1 -> according to book.  The standard error is just the square root of that.

#Make Sum corresponding to only beta3
otherVar <-inversePar[4,4]
#Standard Error for beta3Hat
seBH3<- sqrt(sigma2*otherVar)
tStat3 <- (finalBVec[4]-beta3Star)/seBH3
tStat3
#tStat: 0.084


#Two-sided test p-values:

result <- 2*(1-pt(abs(tStat3), sizeN-6))
result

#Since the p-value (.934) is less than the level of significance, evidence suggests to fail to reject
#the null hypothesis


#beta4
#H0: B4=0
#Ha: B4 !=0

beta4Star<-0
#Estimated variance for multiple linear regression is found by multiplying sigma2 by the elements
#of (X'X)^-1 -> according to book.  The standard error is just the square root of that.

#Make Sum corresponding to only beta4
otherVar <-inversePar[5, 5]
#Standard Error for beta4Hat
seBH4<- sqrt(sigma2*otherVar)
tStat4 <- (finalBVec[5]-beta4Star)/seBH4
tStat4
#tStat4: 5.58

#Two-sided test p-values:

result <- 2*(1-pt(abs(tStat4), sizeN-6))
result

#P-value: 5.20 e-07
#There exists enough evidence to reject the null hypothesis in favor of the alternative.


#beta5
#H0: B5=0
#Ha: B5 !=0

beta5Star<-0
#Estimated variance for multiple linear regression is found by multiplying sigma2 by the elements
#of (X'X)^-1 -> according to book.  The standard error is just the square root of that.

#Make Sum corresponding to only beta5
otherVar <-inversePar[6,6]
#Standard Error for beta5Hat
seBH5<- sqrt(sigma2*otherVar)
tStat5 <- (finalBVec[6]-beta5Star)/seBH5
tStat5
#tStat5: -1.999

#Two-sided test p-values:

result <- 2*(1-pt(abs(tStat5), sizeN-6))
result

#The p-value stands at around 0.04985.  As a result, there exists enough evidence to reject the
#null hypothesis.


#The p-values for WT9Vec and ST9Vec were less than 0.05 level of significance (meaning evidence to reject
#zero slope).  As a result, these predictors are better at predicting BMI than their counterparts.

#############################################################################
#3.6.2

head(water)

bResp = water[, "BSAAM"]

BPCVec <- water[,"OPBPC"]
RCVec <- water[,"OPRC"]
LAKEVec <- water[,"OPSLAKE"]

#Size of vectors
sizeW <- length(bResp)

#Repeat 1 sizeW times for intercept in design matrix
waterCol <- rep(1, sizeW)
#Water design matrix
waterDM<- matrix(c(waterCol, BPCVec, RCVec, LAKEVec), ncol=4)

xTranspose <- t(waterDM)

insideInv <- xTranspose %*% waterDM
inversePar <- solve(insideInv)
finalWaterBV <- inversePar %*% xTranspose %*% bResp
finalWaterBV

SSReg <- t(finalWaterBV)%*% insideInv %*% finalWaterBV
SYY <- t(bResp)%*%bResp

RSS <- SYY - SSReg
RSS

R2<- SSReg/SYY
R2

RSSElem <- finalWaterBV[1]+finalWaterBV[2]*BPCVec+finalWaterBV[3]*RCVec+finalWaterBV[4]*LAKEVec
RSSNext <- sum((bResp-RSSElem)^2)
RSSNext

sigma2 <- RSS/(sizeW-4)
sigma2

betaStar <- 0

#Find t-statistic for OPBPC

otherVar <-inversePar[2,2]
#Standard Error for beta5Hat
seBHBPC<- sqrt(sigma2*otherVar)
tStatBPC <- (finalWaterBV[2]-betaStar)/seBHBPC
tStatBPC
#tStatBPC: 0.08

#Extra Stuff
2*(1-pt(tStatBPC, sizeW-4))

bmWater<- matrix(c(waterCol, RCVec, LAKEVec), nrow = sizeW)
waterT <- t(bmWater)
inversePar <- solve(waterT%*%bmWater)
outsidePar<- waterT%*%bResp
finalBVec<- inversePar%*%outsidePar

H0Elem <- finalBVec[1] +finalBVec[2]*RCVec+finalBVec[3]*LAKEVec
RSSH0<- sum((bResp-H0Elem)^2)
RSSH0
fStat <- (RSSH0-RSSNext)/(RSSNext/(sizeW-4))
1-pf(fStat, 1, sizeW-4)

#End of extra stuff

#t-Statistic for OPRC
otherVar <-inversePar[3,3]
#Standard Error for beta5Hat
seBHRC<- sqrt(sigma2*otherVar)
tStatRC <- (finalWaterBV[3]-betaStar)/seBHRC
tStatRC
#tStatRC: 2.89


#t-statistic for OPSLAKE
otherVar <-inversePar[4,4]
#Standard Error for beta5Hat
seBHLake<- sqrt(sigma2*otherVar)
tStatLake <- (finalWaterBV[4]-betaStar)/seBHLake
tStatLake
#tStatLake: 3.05

#For lm, the larger a t-value, the more evidence there is against a "zero slope" null hypothesis.
#In this case, the t-statistic of a predictor corresponds to how influential 
#they are to a response variable.  The tests done were to tell if the slope of a predictor was zero,
#and if it was, the predictor tells you nothing about the response.
#In this case, BPCVec has the lowest t-statistic.  Therefore, it is the least likely to accurately
#tell you about the BSAAM values.

