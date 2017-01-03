                            ##################### 
                            ##### Project.1 ##### 
                            #####################
 
--------------------------------------------------------------------------------------------                           
 
loan<-read.csv("C:/Documents and Settings/shree/Desktop/R_Edv/loandata.csv")

dim(loan)

str(loan)

names(loan)

=====================================================================================
#First we try to clean the data and find  missing value if any 
  
summary(loan)

#There are 2 missing values in data
====================================================================================
# Cleaning Missing Values
  
loan<-na.omit(loan)

apply(loan,2,function(x) sum(is.na(x))) 

#Shows no missing values
======================================================================================
## Cleaning the dataset loan.##
  
# Removing month from loan length
  
loan$Loan.Length <- as.integer(gsub(" months$", "", loan$Loan.Length))

# Converting FICO to Numeric

FICO.1 <- as.numeric(substr(loan$FICO.Range,1,3))

FICO.2 <- as.numeric(substr(loan$FICO.Range,5,7))

loan$FICO.Median <- (FICO.1 + FICO.2) / 2

# Cleaning Employment.Length variable

loan$Employment.Length <- gsub("<.*", "0.5", loan$Employment.Length)

loan$Employment.Length[loan$Employment.Length=="10+ years"]<-"10.5"

loan$Employment.Length <-gsub(" years$| year$|\\+","",loan$Employment.Length)

loan$Employment.Length <- as.numeric(loan$Employment.Length)
=========================================================================================
  
sum(is.na(loan$Employment.Length))

# No missing values in Employment.Length
 ======================================================================================== 
  

# Few amount funded valuesare irrelevant, eliminating 0 and -0.10 
  
loan[loan$Amount.Funded.By.Investors<=0,2] <- NA
========================================================================================
# checking NA 
  
sum(is.na(loan))
=========================================================================================
# Ommiting NA and checking dimesion of data
  
loan<-na.omit(loan)
                            
sum(is.na(loan))                          

dim(loan)
==========================================================================================                            
# checking class of each variable
                            
sapply(loan[1,],class)
========================================================================================                           

## We look at some plots of factor variable in relation with Interest.Rate ##
  
# Set plot display to 1 row and 3 graphs
  
par(mfrow = c(1, 3))

# Plot Interest Rate and check for normal distribution

boxplot(loan$Interest.Rate, col = "orange", xlab = "mdata$Interest.Raste",main = "Boxplot")

hist(loan$Interest.Rate, col = "blue", main = "Histogram")

qqnorm(loan$Interest.Rate)

qqline(loan$Interest.Rate, col = "red", lwd = 2)

# Shows that Interest.rate is normally distributed
=======================================================================================
# Set plot display to 1 row and 3 graphs
  
par(mfrow = c(1, 3))

boxplot(loan$Interest.Rate ~ loan$Loan.Length, col = "blue", varwidth = TRUE)

# Shows higher loan length has higher interest rate

boxplot(loan$Interest.Rate ~ loan$Loan.Purpose, col = "orange", varwidth = TRUE)

boxplot(loan$Interest.Rate ~ loan$Home.Ownership, col = "purple", varwidth = TRUE)

# we see that Interest.Rate varies for different factors.

# of Loan.Purpose as well as Home.Ownership
========================================================================================

# Set plot display to 1 row and 3 graphs
  
par(mfrow = c(1, 3))

boxplot(loan$Interest.Rate ~ loan$Employment.Length, col = "blue", varwidth = TRUE)

# we see in above graph median interest rate is almost equal for each 

# factor with little difference.

boxplot(loan$Interest.Rate ~loan$Inquiries.in.the.Last.6.Months, col = "orange",varwidth = TRUE)

boxplot(loan$Interest.Rate ~ loan$Open.CREDIT.Lines, col = "purple", varwidth = TRUE)

# Interest.Rate varies as per the factors.
==================================================================================================
par(mfrow = c(1, 2))

plot(loan$Monthly.Income, loan$Interest.Rate, pch = 19, col = "blue")

plot(loan$Revolving.CREDIT.Balance, loan$Interest.Rate, pch = 19, col = "Orange")

# We have the skewed graph not explaining much 
===================================================================================================
# we try to transform these variable in log variable to 
  
# get the idea of relationship between them an interest.rate

plot(log(loan$Monthly.Income), loan$Interest.Rate, pch = 19, col = "blue")
                        
                          
# Shows increasing Trend.

plot(loan$Revolving.CREDIT.Balance, loan$Interest.Rate, pch = 19, col = "Orange")

# Shows Decreasing Trend.
====================================================================================================
par(mfrow = c(1, 1))

boxplot(loan$Interest.Rate ~ loan$FICO.Range, col = "blue", varwidth = TRUE)

# It is clear from the boxplot that higher the fico range 

# lower the Interest.Rate

boxplot(loan$Interest.Rate ~ loan$State, col = "orange")
======================================================================================================
  
# Lets prepare the data for the regression
  
loan<-loan[,-c(1,11)]

names(loan)
======================================================================================================
# Lets run the correlation matrix for numeric variable
  
# to check which variables are highly correlated 
                            
# Scatterplot Matrices from the glus Package 
                            
library(gclus)
                            
# get data 
                            
dta <- loan[c(1,2,3,6,9,10,11,12,13,14)] 
                            
# get correlations  
                            
dta.r <- abs(cor(dta)) 
                            
# get colors  
                            
dta.col <- dmat.color(dta.r) 
                            
# reorder variables so those with highest correlation
                            
# are closest to the diagonal
                            
dta.o <- order.single(dta.r) 
                            
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       
       main="Variables Ordered and Colored by Correlation" )
                            
# we see that amount requested and amount funded are highly corrleated can 

# cause a problem for multicollinearity in regression model.

# Interest.rate has good corrlation with Amount funded and Amount invested

# Interest.Rate has a high negative correlation with Fico-Median.

=========================================================================================================

# Training And Testing sets For Regression
  
# For Reproducibility
  
set.seed(1234) 

indices<-rbinom(2385,1,prob=0.5) 

# Training Set

trainset<-loan[indices==1,]

# Test Set

testset<-loan[indices==0,] 
======================================================================================================
## Running regression Model on trainig set

# Loading Libraries required fir Regression analysis
  
library(car)

library(MASS)
                            
# We run a regression on trainset
                            
model.1<-lm(Interest.Rate~.,data=trainset)

summary(model.1)
                            
# We run a regression on testset  
                            
model.2<-lm(Interest.Rate~.,data=testset) 
                            
summary(model.2)
                            
# we drop some variables from the model.1 which are not significant

# Adjusted R-squared:  0.7577

# After dropping variable which are not significant from the model 
                            
# and also there is a big difference in the coefficients of models on 
                            
# train set as well as test set.                            
========================================================================================================
# Another model as below.
  
vif(model.1)
======================================================================================

# Also there are Multicollinearity problems inwhich 

# we will try to remove those problem 
========================================================================================================
                              
# Lets check the confidence interval for model.1 coefficients with alpha=0.01  
                              
confint(model.1, level=0.99)               
=========================================================================================================                              
# Lets set another model
  
model.3<-lm(Interest.Rate~Amount.Funded.By.Investors+Loan.Length
            
            + Inquiries.in.the.Last.6.Months+FICO.Median,data=trainset)

summary(model.3)

# All variables are significant with adjusted R^2 =0.7476

# The model is also significant with p value< 0.05

# Display 99% (alpha=0.01) confidence interval bounds

confint(model.3, level=0.99)                 
                     
                        
===================================================================================================                            
vif(model.3)

# There are no multicollinearity problems
===================================================================================================
# Now we draw diagnostics plot
  
par(mfrow = c(1, 2))

hist(model.3$residuals, col = "blue")

qqnorm(model.3$residuals)

qqline(model.3$residuals, col = "red", lwd = 2)

par(mfrow = c(1, 1))

plot(trainset$Interest.Rate, model.3$residuals, pch = 19)

abline(c(0, 0), col = "red", lwd = 3)

plot(model.2$fitted, model.3$residuals, col = "blue", pch = 19)
                            
 # Shows random pattern in errors which seems to be correct.                           
===========================================================================================
par(mfrow=c(2,2))

plot(model.2)

dev.off()

# Graph 1 : Shows that errors are randomly spread little
# bit of nonlinearity in pattern but that is not the problem
# Graph 2:- Shows that errors are fairly normally distributed 
# Graph 3:- Shows ramdom pattern of error which is fine
=================================================================================================

# Lets compare the Model.1 and Model.2

anova(model.1,model.3)

# Shows model.2 is more relevant than model 1 as p value is < 0.05
==============================================================================================
# Predicting target for Train and Test set
  
# precited value of interest.rate on train set
  
pred.1<-predict(model.3,trainset) 

# precited value of interest.rate on set set

pred.2<-predict(model.3,testset)  

obs.1<-trainset$Interest.Rate

obs.2<-testset$Interest.Rate

# Computing RMSE for Trainset

rMSE.1<- sqrt(mean((model.3$residuals)^2))

rMSE.1

#Computing RMSE for test set

error.test<- obs.2-pred.2

rMSE.2<- sqrt(mean((error.test)^2))

rMSE.2

# Lower values of RMSE indicate better fit. 
# RMSE is a good measure of how accurately the model predicts the response, and is the 
# most important criterion for fit if the main purpose of the model is prediction. 
=======================================================================================================
# Lets Run the model.2 on testset
                              
model.4<-lm(Interest.Rate~ Amount.Funded.By.Investors+Loan.Length
              
            +Inquiries.in.the.Last.6.Months+FICO.Median, data=testset)
                            
 summary(model.4)
                            
                            
 vif(model.4)    
                            
# looking at the regression output we have  Adjusted R-squared:  0.7673 
                            
# p-value: < 2.2e-16 and from vif it seems thatr there is no problem 
                            
# multicollinearity . All variables are significant . Model is also 
                            
# significant                           
=========================================================================================================            
            
 # Lets compare the coefficients of regressio model 
                              
 # for test and train set 
                              
  coefficients(model.3)
                            
  coefficients(model.4)  
                            
 # Coeffcient are almost equal on both the set                           
 =========================================================================================================                           
 =========================================================================================================          