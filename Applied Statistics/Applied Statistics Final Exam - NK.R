#Final Exam
#Applied Statistics
#Nate Kaduk
#Professor Li

library(alr4)

#1
#NH: reduced model is a better fit
#AH: Full model is better fit
head(landrent)
#Create vectors from data columns
X1<- landrent[,"X1"]
X2<- landrent[,"X2"]
X3<- landrent[,"X3"]

#Get response and size of data
Y1<- landrent[,"Y"]
size1<- length(Y1)

#Null Hypothesis design matrix and beta-vector
NHDM1<- matrix(c(rep(1,size1),X1,X2,X3),nrow=size1)
tNHDM1<- t(NHDM1)

insidePar<- tNHDM1%*%NHDM1
inversePar<- solve(insidePar)
outsidePar<- tNHDM1%*%Y1
bVec1<- inversePar%*%outsidePar
bVec1

SSReg<- t(bVec1)%*%insidePar%*%bVec1
SYY<- t(Y1)%*%Y1
RSSNH1<- SYY-SSReg
RSSNH1 #5385.739

xVec<- cbind(1, X1, X2, X3)
t(Y1-xVec%*%bVec1)%*%(Y1-xVec%*%bVec1)

#Compute df
dfNH1<- size1-4



#Alternative hypothesis
AHDM1<- matrix(c(rep(1,size1), X1, X2, X3, X2*X3), nrow=size1)
tAHDM1<- t(AHDM1)

inversePar<- solve(tAHDM1%*%AHDM1)
outsidePar<- tAHDM1%*%Y1
bVecAH<- inversePar%*%outsidePar
bVecAH

SSReg<- t(bVecAH)%*%tAHDM1%*%AHDM1%*%bVecAH
SYY<- t(Y1)%*%Y1
RSSAH1<- SYY- SSReg
RSSAH1 #5007.338
xVec<- cbind(1, X1, X2, X3, X2*X3)
t(Y1-xVec%*%bVecAH)%*%(Y1-xVec%*%bVecAH)


dfAH1<- size1-5

#Compute f-statistic
fNum1<- (RSSNH1-RSSAH1)/(dfNH1-dfAH1)
fStat<- fNum1/(RSSAH1/dfAH1)
fStat #4.685303

#p-value
1-pf(fStat, dfNH1-dfAH1, dfAH1) #0.03428
#Since the p-value is less than .05, there exists enough evidence to reject the null hypothesis.  In other words,
#the full model is a better fit for the data.  The interaction between cow density and proportion of farmland
#used does impact the average rent per acre planted.



#2
#Full model parameters (Found in part 1):
bVecAH
#Intercept: -9.335005
#X1: 0.895704
#X2: 0.682737
#X3: 30.3982890
#X2*X3: -1.46536

#Sigma2
sigma2<- RSSAH1/dfAH1
sigma2 #80.76351


#Get regular sigma for part 6
sigma<- sqrt(sigma2)
sigma #8.986852



#3
#Alpha for 99% confidence interval
alpha3<- .01

#B1
varB1<-inversePar[2,2]
seB1<- sqrt(sigma2*varB1)
seB1 #0.06682377

lowerBound3B1<- bVecAH[2] - qt(1-alpha3/2, dfAH)
upperBound3B1<- bVecAH[2] + qt(1-alpha3/2, dfAH)
lowerBound3B1 #-1.680389
upperBound3B1#3.471796


#B2
varB2<-inversePar[3,3]
seB2<- sqrt(sigma2*varB2)
seB2 #0.1426819

lowerBound3B2<- bVecAH[3] - qt(1-alpha3/2, dfAH)
upperBound3B2<- bVecAH[3] + qt(1-alpha3/2, dfAH)
lowerBound3B2 #-1.893356
upperBound3B2#3.258829

#B3
varB3<-inversePar[4,4]
seB3<- sqrt(sigma2*varB3)
seB3 #22.23398

lowerBound3B3<- bVecAH[4] - qt(1-alpha3/2, dfAH)
upperBound3B3<- bVecAH[4] + qt(1-alpha3/2, dfAH)
lowerBound3B3 #27.8222
upperBound3B3#32.97438

#B4
varB4<-inversePar[5,5]
seB4<- sqrt(sigma2*varB4)
seB4 #0.6769776

lowerBound3B4<- bVecAH[5] - qt(1-alpha3/2, dfAH)
upperBound3B4<- bVecAH[5] + qt(1-alpha3/2, dfAH)
lowerBound3B4 #-4.041448
upperBound3B4#1.110736

#A value of 0 is included in all confidence intervals except for parameter of X3 (B3 does not have 0 in its interval).


#4
#It is possible to figure out which parameters are zero using confidence intervals alone (with a margin of error).  
#For number 3, the X3 parameter was the only confidence interval to not have a 0.  This should imply that the p-value for beta3
#is different than the others.

#For the level of significance, alpha=.01 was used for the interval above.  As a result, I would attest that a 99%
#confidence level would work nicely for the hypothesis test here as well.





#5
#Get residuals
fitModel<- bVecAH[1]+bVecAH[2]*X1+bVecAH[3]*X2+bVecAH[4]*X3+bVecAH[5]*X2*X3
res<- Y1-fitModel


plot(fitModel, res, main = "Residuals vs Fitted Response")
abline(a=0,b=0)

#The res have a relatively even split below and above the main line.  However, as fitModel increases, so does the distance 
#between points and the main line.  As a result, the variance appears to be nonconstant.
#With outliers, linearity holds, but constant variance is questionable.

#6
#Function to compute Cook's Distance
DiDist<- function(error, currentH, pPlus,sigma){
  rDen <- sigma*sqrt(1-currentH)
  r<- error/rDen
  
  lastTerm<- currentH/(1-currentH)
  DiDist<- (1/pPlus)*(r^2)*lastTerm
}


#Find Hat Matrix
HMatrix<- AHDM1%*%inversePar%*%tAHDM1
HMatrix

#Check the above residuals are correct using another method
yHat<- HMatrix%*%Y1
eHat<- Y1-yHat
res-eHat #0-vector = the same

#Obtain a vector for the cooks distance
cooksDistVec<- DiDist(res, diag(HMatrix), 5, sigma)

#Define the limit that determines outliers
outLimit <- 4/size1
outLimit #0.05970149

cooksDistVec[which(cooksDistVec>outLimit)]
#There are five influential outliers

#Find all the rows of landrent corresponding to the elements with a Cook's Distance larger than the threshold.
landrent[which(cooksDistVec>outLimit),]
#These are the 5th, 33rd, 36th, 57th, and 64th items in landrent.



#################Part B##########################


#1
head(Challeng)

#Call the glm function to fit the data.
#The response is success divided by the number of trials.
bModel<- glm(fail/n~temp+pres+temp:pres, family=binomial, data=Challeng, weights=n)
summary(bModel)
#Residual deviance is 16.529 on 19 degrees of freedom.  Largest slope is temp.  Largest p-value is pres.

#Create variables for the slope estimates given by the glm model
intercept<- 6.1178675
tempPara<- -0.1485947
presPara<- -.0072200
tempPPara<- 0.0002202
#Create vectors for response variables
temp<- Challeng[,"temp"]
pres<- Challeng[,"pres"]
tempIPres<- temp*pres

#Create the equation with the fitted values.
insideE<- intercept+tempPara*temp+presPara*pres+tempPPara*tempIPres
#Plug in the equation to find the probability values for the logistic function.
probVec<- exp(insideE)/(1+exp(insideE))
probVec

#2
#Plug in given temp/pres values into the model obtained from 1.  
#Then plug in the result of that model into the probability function.

negEXP<- exp(intercept*(-1)-tempPara*31-presPara*100-tempPPara*31*100)

probORFail<- 1/(1+negEXP)
probORFail #0.813369

#Probability of failure for an O-ring: 81.34%


#3
#NH: y=beta0+beta1*temp+beta2*pres
#AH: y=beta0+beta1*temp+beta2*pres+beta3*temp*pres

#Do a glm for the reduced model
redModel<- glm(fail/n~temp+pres, family=binomial, data=Challeng, weights=n)
summary(redModel)
#Residual deviance of the above model is 16.565 on 20 degrees of freedom



dfG2AH<- 19
dfG2NH<- 20

G2AH<- 16.529
G2NH<- 16.565

deltaG2<- G2NH-G2AH
deltaG2 #0.036, both models have very similar residual deviances.

#Compute p-value
1-pchisq(deltaG2, dfG2NH-dfG2AH) #0.8495155

#Since the p-value is greater than a .05 level of significance, there exists enough evidence to warrant acceptance of the null
#hypothesis.  In other words, the reduced model is a better fit to the data than the full model.
