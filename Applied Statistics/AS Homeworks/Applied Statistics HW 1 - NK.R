#Nate Kaduk
#Applied Statistics
#Professor Li
#8/30/2022-9/6/2022

#1
#install.packages("alr4")
library(alr4)

#1.1.1
#The term "dependence of fertility on ppgdp" implies that fertility is dependent on ppgdp.  
#As a result, ppgdp is the predictor and fertility is the response.

#See the first few terms of the table to see what I am dealing with
head(UN11)


#1.1.2
#Make vectors from the numeric data of UN11
ppgdpCol <- UN11[, 4]
fertilityCol <- UN11[, 3]

#Create scatterplot
plot(ppgdpCol, fertilityCol, xlab = "ppgdp", ylab = "Fertility")
#Add title to plot
title("Fertility Scatterplot")


#The dots form a negative correlation, but the correlation is not linear.
#The dots form a more exponentially-curved graph rather than a linear one.
#As a result, a straight-mean line function is not a good summary of this graph.

#1.1.3
plot(log(ppgdpCol), log(fertilityCol), xlab = "ppgdp", ylab = "Fertility")
title("Fertility Log Scatterplot")
#With the log counteracting the exponential quality of the data, the points now form a linear line.
#For this reason, a straight-line mean function would be a good summary for this graph.

#################################################################################################

#2.1.1

#Make matrix before initialization
colNames <- c("ht", "wt")
rowNames <- c(1:10)
beforeMatrix <- rep(0, 20)
A <- matrix(beforeMatrix, nrow = 10, ncol = 2, dimnames = list(rowNames,colNames))

#Add height and weight values to A
height <- c(169.6, 166.8, 157.1, 181.1, 158.4, 165.6, 166.7, 156.5, 168.1, 165.3)
weight <- c(71.2, 58.2, 56.0, 64.5, 53.0, 52.4, 56.8, 49.2, 55.6, 77.8)

A[,1]<- height
A[,2]<- weight

#Plot A
plot(A[,"ht"], A[,"wt"])
title("Height vs weight")

#The dots in the scatterplot move upwards.  In other words, when height increases, weight increases.
#As with most things, there are exceptions.  Except for the two weights above 70, the points have a strong positive relation.
#Therefore, this plot can be used for a linear regression model.


#2.1.2
#Create a function that takes in data and creates the line of best fit based on the least squares
#estimation for that data.
ordLSE<-function(data){
  #Create vector of the numeric values of data columns
  x=data[,1]
  y=data[,2]
  
  #Initialize the means
  xBar <-mean(x)
  yBar <-mean(y)
  
  #Output values to verify
  print("This is x-bar:" )
  print(xBar)
  print("This is y-bar:" )
  print(yBar)
  
  #Initialize sum of squares for each part
  SXX <- sum((x-xBar)^2)
  SYY <- sum((y-yBar)^2)
  SXY<- sum((x-xBar)*(y-yBar))
  
  #Output values to verify
  print("Square values: ")
  print(SXX)
  print(SYY)
  print(SXY)
  
  #Create estimators
  hatBeta1 <- SXY/SXX
  hatBeta0 <- yBar-(hatBeta1 * xBar)
  
  #Add line with those estimators
  abline(a= hatBeta0, b=hatBeta1)
}

ordLSE(A)
