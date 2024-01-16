#Nate Kaduk
#Professor Li
#HW 8

library(alr4)

#9.3.1
head(pipeline)
fVec <- pipeline[, "Field"]
labVec<-pipeline[,"Lab"]
plot(FVec, LabVec)

#The graph looks fairly linear, and a simple linear regression model should give a good fit.
#The only downside is the points with LabVec > 70 seem to be spread out left and right.  But
#other than this, a simple regression model would be perfect.

#9.3.2

#Lab measure is response and field measure is predictor

xBar<- mean(fVec)
yBar<- mean(labVec)
SXX <- sum((fVec-xBar)^2)
SXY <- sum((fVec-xBar)*(labVec-yBar))

beta1Hat <- SXY/SXX
beta0Hat<- yBar-beta1Hat*xBar

abline(a=beta0Hat, b = beta1Hat)

yHat<- beta0Hat+beta1Hat*fVec
RES<- labVec-yHat
plot(fVec, RES)
abline(a=0, b = 0)

#From 0-30, this plot looks like it has constant variance.  Most points are centered around the middle line at RES=0.
#From 30-60 a cluster emerges just below the main line as points above the main line reach upward, far from the line.
#For this reason, the variance is not constant.
#To further support nonconstant variance, from 60 and onward, points lie only below the middle line.  
plot(labVec, RES)
abline(a=0, b = 0)
#This graph also supports nonconstant variance, as when labVec increases, points spread out farther from the main line.

#9.11.
#Create vectors from table values
yVec <- c(514.279, 374.164, 426.349, 842.792,317.492)
errorVec11<- c(-163.145, -137.599, -102.409, 183.499, -49.452)
hII<- c(0.256, 0.162, 0.206, 0.084, 0.415)
sigma11<- 64.891
#Also define sigma squared
sigmaSQ11 <- (sigma11)^2
sigmaSQ11 #4745.97

#Function to compute Cook Distance
diDist<- function(error, currentH, pPlus1, sigmaReg){
  rDen<- sigmaReg*sqrt((1-currentH))
  print(sqrt(1-currentH))
  print(rDen)
  r<- error/rDen
  
  lastTerm<- currentH/(1-currentH)
  diDist<- (1/(pPlus1))*(r^2)*lastTerm
  
}
#Compute Cook distances
dist1 <- diDist(errorVec11[1], hII[1], 5, sigma11)
dist2<- diDist(errorVec11[2], hII[2], 5, sigma11)
dist3<- diDist(errorVec11[3], hII[3], 5, sigma11)
dist4<- diDist(errorVec11[4], hII[4], 5, sigma11)
dist5<- diDist(errorVec11[5], hII[5], 5, sigma11)

dist1 # Alaska: 0.5187363
dist2 # New York: 0.18406
dist3 # Hawaii:  0.1444134
dist4 # Wyoming: 0.1420564
dist5 # DC: 0.124971

#A higher di value corresponds to a more influential outlier.
#For this reason, dist1/Alaska is the most influential here.

h=c(0.256,0.162,0.206,0.084,0.415) 
residual=c(-163.145,-137.599,-102.409,183.499,-49.452) 
sigma.hat=64.891 
p=4 
n=46+(1+p) 
sigma.hat*sqrt(1-h)
sqrt(1-h)
residual.std=residual/(sigma.hat*sqrt(1-h)) 
D=(1+p)^(-1)*residual.std^2*h/(1-h) 
D 



#Also checking using a vector-wise method
sigRep <- rep(sigma11, 5)
rVec<- errorVec11/(sigRep*sqrt(1-hII))
sqResVec<- rVec^2
mean(rVec) 

hFrac<- hII/(1-hII)
cook11<- (1/5)*sqResVec*hFrac
cook11 #Same results


####################################################################################
#9.19.
head(drugcost)

#Create vectors for response and predictors
COST <- drugcost[,"COST"]
RXPM<- drugcost[,"RXPM"]
GS<- drugcost[,"GS"]
RI<- drugcost[,"RI"]
COPAY<- drugcost[,"COPAY"]
AGE<- drugcost[,"AGE"]
F19<-drugcost[,"F"]
MM<- drugcost[,"MM"]

#Establish sizes for this model
size19<- length(COST)
predNum <- 7 #Number of predictors
pPlus1 <- predNum + 1 #Including slope

#Fit model
firstCol <- rep(1, size19)
exBM<- c(firstCol, RXPM, GS, RI, COPAY, AGE, F19, MM)
BM19<- matrix(exBM, nrow = size19)
tBM19<- t(BM19)

ins19<- tBM19%*%BM19
inv19<-solve(ins19)
out19<- tBM19%*%COST
bVec19 <-inv19%*%out19
bVec19 #Pictures have result

#Check Model 19
CM19<- lm(COST~RXPM + GS + RI + COPAY + AGE + F19 + MM, data = drugcost)
summary(CM19)
cooks.distance(CM19)
plot(cooks.distance(CM19))

#Calculate degrees of freedom and sigma squared
df19 <- size19-8
df19 #21
sigma2Num <- t(errorVec)%*%errorVec 
sigma2Num
sigma2<- sigma2Num/df19
sigma2 #0.006849
sigma<- sqrt(sigma2)
sigma#0.0827596

#Create Hat Matrix
hatMatrix19<- BM19%*%inv19%*%tBM19
hatMatrix19
#Get diagonal elements
DHM<- diag(hatMatrix19)

yHat19<- BM19%*%bVec19
errorT<- COST-yHat19
errorT
#Larger Di = more influential


#Repeat Sigma 29 times so the vectors line up
sigRep <- rep(sigma, 29)
rVec<- errorT/(sigRep*sqrt(1-DHM))
sqResVec<- rVec^2

tFrac<- (size19-pPlus1-1)/(size19-pPlus1-sqResVec)
tVec<- rVec*sqrt(tFrac)
tVec

#Find cook's distance for each point
hFrac<- DHM/(1-DHM)
cook19<- (1/pPlus1)*sqResVec*hFrac
cook19

myIndex<- 1:size19
plot(myIndex, cook19)
#####################################################################
#Analysis

pharNames<- rownames(drugcost)
mainVec<- cbind(errorT, tVec, rVec, DHM, cook19, COST)
aMatrix<- matrix(mainVec, nrow=size19)
colnames(aMatrix)<- c("Errors", "Student Res", "Standard Res", "Hii", "Cook Distance", "COST")
row.names(aMatrix)<-pharNames
aMatrix

cookThreshold<- 4/size19
outIndex19 <-which(cook19> cookThreshold)
outIndex19
#Print out the names and rows of the pharmacies with a larger Cook's distance from the threshold
pharNames[outIndex19]
drugcost[outIndex19,]

#The errors, residuals, Hii, COST and Cook's distance for each pharmacy is given in aMatrix.  
#This diagnostic was done with 29 - 7 (22) degrees of freedom.
#The variance (sigma2) is 0.006849 with standard deviation (sigma) 0.0827596.  
#The pharmacies with a Cook's distance larger than the threshold are
aMatrix[outIndex19,]
# NCA : 0.2174
#NC/SC : 0.164465
#Chicago : 1.142297
# DE : 1.2426869 

#DE has the biggest Cook's distance.  Therefore, it is the most influential outlier on COST.
plot(GS, COST)

#Even with the massive outlier, the rest of the graph travels with a negative linear relationship.  As a result, more use
#of GS should reduce COST in general.  

plot(RI, COST)

#This has two big outliers, but for the most part the graph travels with a negative linear relationship.  Therefore,
#more RI would should also reduce drug cost.  