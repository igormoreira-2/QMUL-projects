#R Code for Assessed Coursework 3
#Initialization
library(corrplot)
library(ggplot2)
library(dplyr)
setwd('/Users/igorm/OneDrive/Queen Mary University of London/MTH781 - Data Analytics/Assessed Coursework')
dataset.null.val=read.csv('property-sales.csv', na.strings="?") #loads .csv file into R and specify identifier for missing values
property.sales.data=na.omit(dataset.null.val) #omit missing values
attach(property.sales.data)

#***************Question 1***************
str(property.sales.data) #provides structure of the dataset
summary(property.sales.data) #provides summary of the dataset

#Interesting Trends
hist(property.sales.data$SalePrice, breaks=50, xlab='Sale Price', main='Property Sale Price Distribution')
hist(log(property.sales.data$SalePrice), breaks=40, xlab='Sale Price (log scale)', main='Property Sale Price Distribution in Log Scale')
property.sales.data_num = property.sales.data[,-c(1,3,4,8,14,15,17)] #remove columns with categorical data
plot(property.sales.data_num)
cor(property.sales.data_num) #calculates pearson correlation coefficient
corrplot(cor(property.sales.data_num), type="upper", method="number", tl.col = "black", col = heat.colors(100),bg = "black") #plots correlation coefficients
boxplot(SalePrice ~ OverallQual, col='lightblue', xlab="Overall Quality", ylab="Sale Price (US$)", main="Box Plots for Sale Price x Overall Quality")
ggplot(data=property.sales.data) + geom_point(mapping = aes(x=GrLivArea, y=SalePrice, color=OverallQual)) +
  ggtitle('House Sale Price (US$) x Living Area (ft^2) x House Overall Quality') + 
  ylab('House Sale Price (US$)') + xlab('Living Area (ft^2)')
ggplot(data=property.sales.data) + geom_point(mapping = aes(x=GarageArea, y=SalePrice, color=OverallQual)) +
  ggtitle('House Sale Price (US$) x Garage Area (ft^2) x House Overall Quality') + 
  ylab('House Sale Price (US$)') + xlab('Garage Area (ft^2)')
ggplot(data=property.sales.data) + geom_point(mapping = aes(x=YearBuilt, y=SalePrice, color=OverallQual)) +
  ggtitle('House Sale Price (US$) x Year Built x House Overall Quality') + 
  ylab('House Sale Price (US$)') + xlab('Year Built')
saleprice_overallqual_df=data.frame(SalePrice, OverallQual)
mean_saleprice_qual_df = saleprice_overallqual_df %>% 
  group_by(OverallQual) %>% 
  summarise(SalePrice = mean(SalePrice))
ggplot(mean_saleprice_qual_df, aes(x=OverallQual, y=SalePrice)) + geom_bar(stat="identity", fill='lightblue') +
  ggtitle('House Sale Price Average per Overall Quality') + ylab("Average Sale Price (US$)") + 
  xlab('Overall Quality') + geom_text(aes(label=format(SalePrice, digits = 5)))

#***************Question 2***************
#Linear models with multiple features
#Initially fitting a regression with all parameters
ml1 = lm(SalePrice ~ MSZoning + LotArea + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + CentralAir + GrLivArea + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplace + GarageArea + SaleCondition)
summary(ml1)

#Dropping all features with p-values higher than 0.05 (coefficients indicating low statistical significance of the linear fit)
ml2 = lm(SalePrice ~ LotArea + OverallQual + OverallCond + YearBuilt + GrLivArea + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplace + GarageArea)
summary(ml2)
plot(ml2)

#It seems that homoscedasticity condition is violated, testing model log transform
ml3 = lm(log(SalePrice) ~ LotArea + OverallQual + OverallCond + YearBuilt + GrLivArea + BedroomAbvGr + KitchenAbvGr + KitchenQual + Fireplace + GarageArea)
summary(ml3)

#Variable BedroomAbvGr is not longer statiscally siginificant
ml4 = lm(log(SalePrice) ~ LotArea + OverallQual + OverallCond + YearBuilt + GrLivArea + KitchenAbvGr + KitchenQual + Fireplace + GarageArea)
summary(ml4)#The selected features indicate strong statistical significance of the linear fit
plot(ml4)
predictors.data.frame = property.sales.data[, c("LotArea","OverallQual","OverallCond","YearBuilt","GrLivArea","KitchenAbvGr","KitchenQual","Fireplace","GarageArea")]
predictions.data.frame_log = predict(ml4, predictors.data.frame, interval = 'prediction')
predictions.data.frame=exp(predictions.data.frame_log[,1])#converting back true values for predicted sale prices
residual_error=property.sales.data$SalePrice-predictions.data.frame
mse=sum(residual_error^2)/length(property.sales.data$SalePrice)
rmse=sqrt(mse)

#Polynomial model testing
#Test1 - GrLivArea
set.seed(94)
testindex=sample(1:1460,438) #assuming 30% test and 70% train
poly_train=property.sales.data[-testindex,] #creating training set
poly_test=property.sales.data[testindex,] #creating test set
#run loop to determine test MSE & R^2 for different degrees of the polynomial (up to order 10)
n=10
msevals=seq(1:n)
adj.r.squared.vals=seq(1:n)
for(i in 1:n){
  nlin=lm(SalePrice ~ poly(GrLivArea,i),data=poly_train)
  msevals[i]=mean((predict(nlin,poly_test)-poly_test$SalePrice)^2)
  adj.r.squared.vals[i]=summary(nlin)$adj.r.squared}
plot(seq(1:n),msevals, xlab='degrees of freedom', ylab='mean squared error', main='PolyReg for SalePrice and GrLivArea (MSE x Degrees of Freedom)')
plot(seq(1:n),adj.r.squared.vals, xlab='degrees of freedom', ylab='R^2', main='PolyReg for SalePrice and GrLivArea (R^2 x Degrees of Freedom)')
best_poly_r2 = adj.r.squared.vals[which.max(adj.r.squared.vals)]
best_poly_mse = msevals[which.min(msevals)]

#Test2 - YearBuilt
set.seed(94)
testindex=sample(1:1460,438) #assuming 30% test and 70% train
poly_train=property.sales.data[-testindex,] #creating train set
poly_test=property.sales.data[testindex,] #creating test set
n=10
msevals=seq(1:n)
adj.r.squared.vals=seq(1:n)
for(i in 1:n){
  nlin=lm(SalePrice ~ poly(YearBuilt,i),data=poly_train)
  msevals[i]=mean((predict(nlin,poly_test)-poly_test$SalePrice)^2)
  adj.r.squared.vals[i]=summary(nlin)$adj.r.squared}
plot(seq(1:n),msevals, xlab='degrees of freedom', ylab='mean squared error', main='PolyReg for SalePrice and YearBuilt (MSE x Degrees of Freedom)')
plot(seq(1:n),adj.r.squared.vals, xlab='degrees of freedom', ylab='R^2', main='PolyReg for SalePrice and YearBuilt (R^2 x Degrees of Freedom)')
best_poly_r2 = adj.r.squared.vals[which.max(adj.r.squared.vals)]
best_poly_mse = msevals[which.min(msevals)]

#***************Question 3***************
#Dividing data into two subsets (Fireplace YES and NO)
fireplace.no=subset(property.sales.data,Fireplace=="N") #define a subset of the data with the rows where fireplace=="No"
fireplace.yes=subset(property.sales.data,Fireplace=="Y") #define a subset of the data with the rows where fireplace=="Yes"

#plotting a few interesting correlations
plot(fireplace.no$SalePrice,fireplace.no$OverallQual,col="blue") 
points(fireplace.yes$SalePrice,fireplace.yes$OverallQual,col="red")
plot(fireplace.no$SalePrice,fireplace.no$GrLivArea,col="blue") 
points(fireplace.yes$SalePrice,fireplace.yes$GrLivArea,col="red")
plot(fireplace.no$SalePrice,fireplace.no$YearBuilt,col="blue") 
points(fireplace.yes$SalePrice,fireplace.yes$YearBuilt,col="red")

#Logistic models with multiple features
#Initially fitting a regression with all parameters, checking summary output to assess statistical significance
mlgr_fireplace = glm(Fireplace ~ MSZoning + LotArea + BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + CentralAir + GrLivArea + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + GarageArea + SaleCondition + SalePrice, family=binomial)
summary(mlgr_fireplace)
#Dropping all features with p-values higher than 0.05 (coefficients indicating low statistical significance of the linear fit)
mlgr_fireplace_1= glm(Fireplace ~ OverallQual + OverallCond + YearBuilt + CentralAir + GrLivArea + HalfBath + BedroomAbvGr + SalePrice, family=binomial)
summary(mlgr_fireplace_1)
mlgr_fireplace_2= glm(Fireplace ~ OverallCond + YearBuilt + CentralAir + GrLivArea + BedroomAbvGr + SalePrice, family=binomial)
summary(mlgr_fireplace_2)
mlgr_fireplace_3= glm(Fireplace ~ YearBuilt + CentralAir + GrLivArea + BedroomAbvGr + SalePrice, family=binomial)
summary(mlgr_fireplace_3) #best model based on its statistical significance

#Separating test and train data
set.seed(40) #setting seed for reproducibility
testindex=sample(1:1460,438) #defining a vector of random row numbers to sample testset from original dataset (30/70 split)
test=property.sales.data[testindex,] #selecting random rows as test set
train=property.sales.data[-testindex,] #removing the rows for training set
mlgr_3=glm(Fireplace ~ YearBuilt + CentralAir + GrLivArea + BedroomAbvGr + SalePrice, family=binomial, data=train) #run logistic regression on the training set
testprob=predict(mlgr_3,test,type="response") #calculating predicted values for the testset

#Comparing the prediction from our classifier on the test set
testpred=rep("N",438) #creating a vector of the same length as the testset containing only "No"
testpred[testprob>0.5]="Y" #setting entries in the vector to "Yes" when the probability is greater than the threshold of 0.5
confmatrix=table(testpred,test$Fireplace) #calculate the confusion matrix by comparing the vector testpred with the default column in the testset
precision = confmatrix[2,2]/(confmatrix[1,2]+confmatrix[2,2]) #precision
recall = confmatrix[2,2]/(confmatrix[2,1]+confmatrix[2,2]) #recall
misclassification_rate=(confmatrix[1,2]+confmatrix[2,1])/438 #overall misclassification error
accuracy = (confmatrix[1,1]+confmatrix[2,2])/438 #accuracy

#Exploring accuracy for different thresholds using the prediction above (testprob)
t_test=20 #number of threshold tests
accuracy_test=rep(1,t_test) #initializes values for accuracy
t_list=rep(0,t_test) #initializes values for threshold list
for(i in 1:t_test)
{
  threshold=i/(t_test+1) #defines the threshold values, varying from 0 to 1, t_test times
  t_list[i]=threshold
  testpred1=rep("N",438)
  testpred1[testprob>threshold]="Y"
  confmatrix1=table(testpred1, test$Fireplace)
  accuracy_test[i]=(confmatrix1[1,1]+confmatrix1[2,2])/438
}
plot(t_list,accuracy_test, main = 'Accuracy x Threshold Graph', ylab = 'accuracy', xlab = 'threshold')
best_threshold=t_list[which.max(accuracy_test)] #finds best threshold value based on highest accuracy

#Fine-tunned model
testpred=rep("N",438) # 1. create a vector of the same length as the testset containing only "No"
testpred[testprob>best_threshold]="Y" # 2. set those entries in the vector to "Yes" when the probability is greater than the threshold
confmatrix=table(testpred,test$Fireplace) #calculate the confusion matrix by comparing the vector testpred with the default column in the testset
precision_opt = confmatrix[2,2]/(confmatrix[1,2]+confmatrix[2,2]) #precision
recall_opt = confmatrix[2,2]/(confmatrix[2,1]+confmatrix[2,2]) #recall
misclassification_rate_opt=(confmatrix[1,2]+confmatrix[2,1])/438 #overall misclassification error
accuracy_opt = (confmatrix[1,1]+confmatrix[2,2])/438 #accuracy