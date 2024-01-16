#Exam 2
#Applied Statistics
#Nate Kaduk
#Professor Li
#11/17/2022

library(alr4)
head(fuel2001)

bResp <- fuel2001[, "FuelC"]
bRespM <- bResp-mean(bResp)

tax<- fuel2001[,"Tax"]
Dlic<- fuel2001[,"Drivers"] #Needs /population
logM<- log(fuel2001[,"Miles"])
interTM<- Dlic*logM
sizeN<- length(bResp)

checkModel<- lm(bResp~tax+Dlic+logM+interTM, data= fuel2001)
summary(checkModel)
#Note: since the above value produced a computationally singular matrix for me, I went ahead with the centralized version of the beta matrix.

taxCent <- tax - mean(tax)
DCent<- Dlic - mean(Dlic)
logMCent <- logM - mean(logM)
interTMCent<- interTM - mean(interTM)

firstCol <- rep(1, sizeN)
betaMatrix1<- matrix(c(taxCent, DCent, logMCent, interTMCent), nrow=sizeN)
tBM<- t(betaMatrix1)

checkModel<- lm(bResp~taxCent+DCent+logMCent+interTMCent, data= fuel2001)
summary(checkModel)

insidePar <- tBM%*%betaMatrix1
inversePar <- solve(insidePar)
outsidePar<- tBM%*%bRespM
finalBVec1<- inversePar%*%outsidePar
finalBVec1
#Slopes:
#-2.8862 e+04
#-7.5654 e-01
#4.7132 e+04
#1.16946 e-01

#Find intercept
meanVec <- c(mean(tax), mean(Dlic), mean(logM), mean(interTM))
interC <- mean(bResp) - t(finalBVec1) %*% meanVec
interC #417921.8

SSReg<- t(finalBVec1)%*% insidePar %*% finalBVec1
SYY <- t(bRespM)%*%bRespM
RSS<- SYY-SSReg
RSS

#RSSElem<- as.vector(interC) +finalBVec1[1]*taxCent+finalBVec1[2]*DCent+finalBVec1[3]*logMCent+finalBVec1[4]*interTMCent
#RSS<- sum((bRespM-RSSElem)^2)
#RSS

df <- sizeN - 5
sigma2<- RSS/df
sigma2 #143097231254
sqrt(sigma2) # 378281.9


#Test slopes
#NH: beta1=0
#AH: beta1 does not = 0

beta1Star<-0

currentVar <- inversePar[1,1]
currentSE<-sqrt(sigma2*currentVar)
currentSE #12000.78

tStat<- (finalBVec1[1]-beta1Star)/currentSE
tStat #-2.4050

pValB1 <- 2*(1-pt(abs(tStat), df))
pValB1 #0.02025

#Since the p-value is < .05, there exists enough evidence to warrant rejection of the null hypothesis in favor of the alternative.  

#NH: beta2=0
#AH:beta2 does not = 0

beta2Star<-0

currentVar <- inversePar[2,2]
currentSE<-sqrt(sigma2*currentVar)
currentSE #0.30135

tStat<- (finalBVec1[2]-beta2Star)/currentSE
tStat #-2.5105

pValB2 <- 2*(1-pt(abs(tStat), df))
pValB2 #0.01563

#There is sufficent evidence to reject the null hypothesis

#NH: beta3=0
#AH:beta3 does not = 0
beta3Star<-0

currentVar <- inversePar[3,3]
currentSE<-sqrt(sigma2*currentVar)
currentSE #62713.76

tStat<- (finalBVec1[3]-beta3Star)/currentSE
tStat #0.7515

pValB3 <- 2*(1-pt(abs(tStat), df))
pValB3 #0.45615

#There is enough evidence to suggest to fail to reject the null hypothesis

#NH: beta4=0
#AH:beta4 does not = 0
beta4Star<-0

currentVar <- inversePar[4,4]
currentSE<-sqrt(sigma2*currentVar)
currentSE #0.0251

tStat<- (finalBVec1[4]-beta4Star)/currentSE
tStat #4.6576

pValB4 <- 2*(1-pt(abs(tStat), df))
pValB4 #2.74586 e-05

#There exists enough evidence to support rejection of the null hypothesis.

summary(lm(bResp~tax+Dlic+logM+interTM, data = fuel2001))

#The estimates for both slope and intercept match those given by the lm summary.  The t-statistics the values given above as well.
#The p-values match with the lm, and doing each test, the test with the biggest p-value was with logM(0.456).  All others produced
#a p-value less than the level of significance.  The only estimate that didn't match the lm was sigma2 (with a sqrt root of 378281.9).  
#However, this is fairly within round-off range of lm's sigma of 378281.9, and was still able to be used to obtain proper t-statistics and p-values.





#2

#NH: bResp~Tax + log(miles)
#AH: bResp~Tax+Dlic+log(Miles)+Tax*log(Miles)

betaMatrixNH<- matrix(c(rep(1, sizeN),tax, logM), nrow = sizeN )
tBMNH <- t(betaMatrixNH)

insidePar2<-tBMNH%*% betaMatrixNH
inversePar2 <- solve(insidePar2)
outsidePar2 <- tBMNH%*%bResp
finalBVec2<- inversePar2%*%outsidePar2
finalBVec2

SSReg<- t(finalBVec2)%*% insidePar2%*%finalBVec2
SYY<- t(bResp)%*%bResp
RSSNH<- SYY-SSReg
RSSNH
dfNH <- sizeN - 3

#Note: alternative RSS and df were defined in 1, as they are the original model
fStatNum <- (RSSNH - RSS)/(dfNH-df)
fStatDen<- (RSS/df)
fStat<- fStatNum/fStatDen
fStat #838.2383

pValF<- 1-pf(fStat, dfNH-df, df)
pValF #0

#Since the p-value is less than a 0.05 level of significance, there exists enough evidence to warrant rejection of the null hypothesis.


summary(lm(bResp~tax+logM, data=fuel2001))
#Degrees of freedom for the NH model is correct.  The f-statistic p-value is also really low here as well.

#Part 2: MinnLand
#1
head(MinnLand)
region<- MinnLand[,"region"]
year<- MinnLand[,"year"]
AP<- log(MinnLand[,"acrePrice"])

#Print region to see levels
region

#Since there are 6 regions, 5 dummy variables are needed (not including the additional variable of year).  This is because we consider the first level to be when all 
#other dummy variables are 0.

#2
#log(AcrePrice)~year+region
#log(AcrePrice)~year+region+year:region

summary(lm(AP~year+region, data=MinnLand))

#Estimates:
#2.011 e+02 - Intercept
#1.036 e-01 - Year
#7.931 e -01 - West Central
#1.131 e+00 - Central
#1.082 e+00 - South west
#1.294 e+00 - South central
#1.326 e+00 South East

#Standard error for all of them are decently low, with the order of e-02.  The only se higher than that is the intercept.  The only one lower
#is year.  The t-value for all the predictors are quite big with the lowest being West Central at 68.29 and the highest being both Central
#and South central being above 100.  The p-values are all really small. The Residual Standard error is 0.4879.

#3
#NH:log(AcrePrice)~year+region
#AH: log(AcrePrice)~year+region+year:region


sizeM = length(region)
u2=rep(0, sizeM)
u3 = rep(0, sizeM)
u4 = rep(0, sizeM)
u5 = rep(0, sizeM)
u6 = rep(0, sizeM)
u2[which(region=="West Central")]=1
u3[which(region=="Central")]=1
u4[which(region=="South West")]=1
u5[which(region=="South Central")]=1
u6[which(region=="South East")]=1

BMNH<- matrix(c(rep(1,sizeM),year, u2,u3,u4,u5,u6),nrow=sizeM)
tBMNH <- t(BMNH)

insidePar<- tBMNH%*% BMNH
inversePar <- solve(insidePar)
outsidePar<- tBMNH%*%AP
bVecNH<- inversePar%*%outsidePar
bVecNH

RSSElem<- bVecNH[1]+bVecNH[2]*year+bVecNH[3]*u2+bVecNH[4]*u3+bVecNH[5]*u4+bVecNH[6]*u5+bVecNH[7]*u6
RSS<- sum((AP-RSSElem)^2)
RSS #Same as SYY -SSReg with 4449.3

#Added
onePar<- AP - BMNH%*%bVecNH
RSSCheck<- t(onePar)%*%onePar
RSSCheck

SSReg<- t(bVecNH)%*%insidePar%*%bVecNH
SYY<- t(AP)%*%AP
RSSNH<- SYY-SSReg
RSSNH #4449.3
dfNH <- sizeM - 7

BMAH<- matrix(c(rep(1,sizeM),year, u2,u3,u4,u5,u6, u2*year, u3*year, u4*year, u5*year, u6*year),nrow=sizeM)
tBMAH <- t(BMAH)

insidePar<- tBMAH%*% BMAH
inversePar <- solve(insidePar)
outsidePar<- tBMAH%*%AP
bVecAH<- inversePar%*%outsidePar
bVecAH

SSReg<- t(bVecAH)%*%insidePar%*%bVecAH
SYY<- t(AP)%*%AP
RSSAH<- SYY-SSReg
RSSAH #4417.956
dfAH<-sizeM-12

fStatNum <- (RSSNH-RSSAH)/(dfNH-dfAH)
fStatDen <- (RSSAH/dfAH)
fStat<- fStatNum/fStatDen
fStat #26.51681

pVal3<- 1-pf(fStat, dfNH-dfAH, dfAH)
pVal3 #0

#Since the p-value is less the level of significant, there exists enough evidence to warrant rejection of the null hypothesis.  In other words,
#The alternative hypothesis provides a better fit for the data