#Applied Statistics HW II
#Nate Kaduk
#Professor Li
#9/8/2022 - 9/14/2022

#Load Library
library(alr4)

#Plot and output UBSprices to see what I am dealing with
plot(UBSprices)
head(UBSprices)

#Move this function over from part 1 for 2.3
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

#Create Scatterplot
plot(UBSprices$rice2003, UBSprices$rice2009)

#Create a vector of the difference between 2009 and 2003 rice.
totalDiff <- UBSprices$rice2009-UBSprices$rice2003


#Search through totalDiff to find the index of the smallest or largest difference, depending on function call
findEx <- function(searchValue){
  for(i in 1:length(totalDiff)){
    if(totalDiff[i]==searchValue){
      return(i)
    }
  }
  return(0)
}
#Initialize largest and smallest search values
largestInc <- max(totalDiff)
smallestInc <- min(totalDiff)

#Display the cities with the largest and smallest increase of rice
#FindEx returns the index that is either the smallest or the largest (governed by function call)
#Rownames(UBSprices)[i] returns the row name (city) where the index is.
print("The city with the largest increase in rice is: ")
rownames(UBSprices)[findEx(largestInc)]
print("The city with the smallest increase in rice is: ")
rownames(UBSprices)[findEx(smallestInc)]

###################################################################################
#2.3
#Combine x and y so I can create a matrix on them
UBSRiceMatrix <- cbind(UBSprices$rice2003, UBSprices$rice2009)
#Add the line to the function
ordLSE(UBSRiceMatrix)

#Main Question:
#The slope of the OLS line is less than 1, does this mean that prices are lower in 2009 than 2003?

#On this, I have a few reasons why that would be false.
#First, the two points passed the x-value of 60 could be considered as outliers.
#If they were removed, the slope of the line would increase.
#Second, and most important.  If the prices in 2009 were lower than in 2003, then the result should
#be a negative if all the values in 2009 would be subtracted by all the values in 2003.
#The variable totalDiff was defined earlier as the difference of each value (Including outliers).
#Checking:
sum(totalDiff)
#Since the sum of all the differences in 2009-2003 is greater than 0 (155.5), the rice prices in 2009
#are greater than the rice prices in 2003.

###############################################################################
#2.4
#Question: Provide two reasons why a simple regression line would not be appropriate here.

#First, there are two outliers.  These outliers extend the graph 60 units beyond all the other dots.
#As a result, the regression line is slanted outward not directly in the middle of the points.
#Second, most points form a cluster below the line, making it extremely difficult to figure out 
#a chosen point from the line.


#Don't mind this code.
#This is just checking to see if the line fairs any better without the outliers.
#I know data-removal is probably not allowed in typical circumstances, but I was curious.  
findEx(-59)
findEx(-21)

newUP2009 <- c(UBSprices$rice2009[1:35], UBSprices$rice2009[38:54])
newUP2003<- c(UBSprices$rice2003[1:35], UBSprices$rice2003[38:54])
plot(newUP2003,newUP2009)
newRM <- cbind(newUP2003,newUP2009)
ordLSE(newRM)

#Although, it certainly does look much better.

