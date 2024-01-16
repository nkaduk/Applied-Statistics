#Nate Kaduk
#AS HW 7
#Professor Li

library(alr4)

head(UN11)
#6.1
#H0: lifeExpF~ 1
#HA: lifeExpF ~ group
#Response vector
leVec <- UN11[, "lifeExpF"]
#Number of values
sizeN <- length(leVec)

#Create u vectors for group factor model
u2 <- rep(0, sizeN)
u3 <- rep(0, sizeN)
u2[which(groupUN=="other")]=1
u3[which(groupUN=="africa")]=1

#Repeat 1 sizeN times for slope for each beta matrix
firstCol <- rep(1, sizeN)
#Null hypothesis beta matrix
nullHBM = matrix(firstCol, ncol=1, nrow=sizeN)
traBM <- t(nullHBM)

#Take the inverse of the matricies in the paranthesis of (x'x)^tx'Y
insidePar <- traBM %*% nullHBM
inversePar <- solve(insidePar)

yMatrix <- matrix(leVec)
outsidePar <- traBM%*% yMatrix

finalBVec <- inversePar %*% outsidePar
finalBVec #72.29319

#Elements for H0 RSS
H0Elem <- finalBVec[1]
RSSH0 <- sum((leVec-H0Elem)^2)
RSSH0 #20293.22
dfH0 <- sizeN - 1

#Alternative hypothesis beta matrix
altHBM<-matrix(c(firstCol, u2, u3), ncol=3, nrow = sizeN)
traABM <- t(altHBM)

insidePar <- traABM %*% altHBM
inversePar <- solve(insidePar)

outsidePar <- traABM %*% yMatrix
finalBVec <- inversePar%*% outsidePar
finalBVec #82.4465, -7.1197, -22.6742

#Elements for HAElem expression
HAElem <- finalBVec[1]+finalBVec[2]*u2+finalBVec[3]*u3
RSSHA <- sum((leVec - HAElem)^2)
RSSHA #7750.188
dfHA <- sizeN - 3

fStat <- ((RSSH0 - RSSHA)/(dfH0-dfHA))/(RSSHA/dfHA)
fStat #159.2687

#Compute p-value
2*(1-pt(fStat, dfH0)) #0

#Since the p-value is less than 0.05, there exists sufficient evidence to warrant rejection of
#null hypothesis.  Just slope is not a good predictor for female life expectancy.  




###################################
#6.4.1
#H0: LifeExpF ~ log(ppgdp)+group:log(ppgdp)
#Ha: LifeExpF ~ group+log(ppgdp)+group:log(ppgdp)

#H0 implies that group alone has no impact for the life expectancy of females.
#Ha states that group does impact female life expectancy.  

#6.4.2
#Converting H0, HA into an expression
#BetaI stands for interaction 
#H0: beta0 + beta1log + betaI2Group2log + betaI3Group3log
#Ha:beta0 + betaI1log + betaI2Log + betaI2Group2log + betaI3Group3log

#Create log vector as predictor.
logPP <- log(UN11[,"ppgdp"])

#Interaction parameters.  The u's have already been defined.
u2Inter <- logPP * u2
u3Inter <- logPP * u3

betaMatrixH0<- matrix(c(firstCol, logPP, u2Inter, u3Inter), nrow = sizeN, ncol = 4)
traBM <- t(betaMatrixH0)

insidePar <- traBM %*% betaMatrixH0
inversePar <- solve(insidePar)

yMatrix <- matrix(leVec)
outsidePar <- traBM%*% yMatrix

finalBVec <- inversePar %*% outsidePar
finalBVec

#Parameter Estimates:
#logPP = 3.72450
#logPP:other = -0.06980
#logPP:africa = -1.43032

summary(lm(leVec~log(ppgdp)+group:log(ppgdp), data=UN11))


h0BElem <- finalBVec[1]+finalBVec[2]*logPP+finalBVec[3]*u2Inter+finalBVec[4]*u3Inter
RSSH0 <- sum((leVec-h0BElem)^2)
RSSH0 #5232.008
#Get degrees of freedom for H0 (total size - (3 parameters + slope))
dfH0 <- sizeN - 4

#Ha
betaMatrixHa<- matrix(c(firstCol, u2, u3, logPP, u2Inter, u3Inter), nrow = sizeN, ncol = 6)
traBM <- t(betaMatrixHa)

insidePar <- traBM %*% betaMatrixHa
inversePar <- solve(insidePar)

yMatrix <- matrix(leVec)
outsidePar <- traBM%*% yMatrix

finalBVec <- inversePar %*% outsidePar
finalBVec

#Slope estimates:
#u2 = -11.17310
#u3 = -22.98484
#logPP = 2.24254
#u2Inter = 0.92944
#u3Inter = 1.09498
summary(lm(leVec~log(ppgdp)+group + group:log(ppgdp), data=UN11))

hABElem <- finalBVec[1]+finalBVec[2]*u2+finalBVec[3]*u3+finalBVec[4]*logPP+finalBVec[5]*u2Inter+finalBVec[6]*u3Inter
RSSHA <- sum((leVec-hABElem)^2)
RSSHA #5077.698
dfHA <- sizeN - 6 #total size - (5 parameters + slope)

#Compute F-statistic
fStat <- ((RSSH0-RSSHA)/(dfH0-dfHA))/(RSSHA/dfHA)
fStat #2.932609

#Compute p-value
2*(1-pt(fStat, dfH0)) #0.00376

#Since the p-value is less than a 0.05 level of significance, there exists sufficient evidence
#to reject the null hypothesis.  In other words, group alone, without any interaction on ppgdp,
#does play a role in female life expectancy.