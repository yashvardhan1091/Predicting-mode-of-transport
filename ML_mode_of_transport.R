setwd("D:/OneDrive/Documents/Great Learnings/Machine Learning/ML_GroupAssignment")
getwd()
Cars<-read.csv("Cars.csv")
str(Cars)
#First of all we will split the data as between train & test. will treat test as unseen data
# Will split between 80:20 ratio
set.seed(101)
library(caTools)
split<-sample.split(Cars,SplitRatio = 0.7)

newCarsData<-subset(Cars,split==TRUE)
TestData <-subset(Cars,split==FALSE) # will keep this data aside for testing purpose only

# Saving TestData on Drive
write.csv(TestData, "TestData.csv")
write.csv(newCarsData,"newCarsData.csv")

str(newCarsData)
str(TestData)
# will split 'newCarsData' further betweeen Train & Validate
# before we do so, will perform some EDA on newCarsData

################################################################
#########################EDA - newCarsData######################
################################################################
class(newCarsData)
dim(newCarsData)
str(newCarsData)
# Engineer, MBA, WorkExp and License showing as integer. Should be converted as factor
newCarsData$Engineer <- as.factor(newCarsData$Engineer)
newCarsData$MBA <- as.factor(newCarsData$MBA)
newCarsData$license <- as.factor(newCarsData$license)

summary(newCarsData)
#Check for the missing value
# Load VIM Library
library(VIM)
aggr(newCarsData,col=c('navy blue','red'), numbers=TRUE, sortVars=TRUE, 
     labels=names(newCarsData),cex.axis=0.7, gap=3)$missing

sapply(newCarsData, function(x) sum(is.na(x))) #to check NA's in the dataset
# Only MBA has one missing value

# Imputing missing value
which (is.na(newCarsData[,"MBA"])) # To check which row has missing values
# Row 104 has missin value
newCarsData[104,]
# Since engineer is 0 , will mark MBA as 0 for this particular row as this employee also has work ex of 2 years and doesn't have a graduation to apply for MBA.
newCarsData[is.na(newCarsData)] <- 0

# Data summary
summary(newCarsData)
# Observations - 
# Age - Mean & Median is almost same or minute variation. It is likely normally distributed with a min of 18 & Max 43
# Salary - There might be some potential outliers in Salary. distribution appears to be right skewed as mean is greater than the median. While min & max salary ranges from 6.50 LKH P.A to 57Lkh P.A.
# Distance - Mean & Median is almost same or minute variation. It is likely normally distributed with a min of 3.20 KM & Max 21.50 KM
# Other variable are categorical, will perform detailed EDA

# As per the problem statement our primary object is to identify if employee will use CAR as mode of transportation
# Based on this requirement a dummy variable to be created 
newCarsData$CarUsage<-ifelse(newCarsData$Transport =='Car',1,0)
table(newCarsData$CarUsage)
# Variable CarUsage is our dependent variable now

# to chek what percentage of employee are commuting using car
sum(newCarsData$CarUsage == 1)/nrow(newCarsData)
# 13% of employee are using Car as mode of transportation

# Lets check the string of new variable 'CarUsage'
str(newCarsData$CarUsage)
# String is showing as number. Lets convert it to the factor
newCarsData$CarUsage <- as.factor(newCarsData$CarUsage)


#	Data distribution / Visualization
# Library - DataExplorer
library(DataExplorer)
plot_histogram(newCarsData)
# As explained above Age & Distance is normally distributed, however Salary & work Exp is right skewed

# Box Plots to visualize outliers
par(mfrow=c(2,2))
bp1=boxplot(newCarsData$Age,horizontal = FALSE,main="Age", col="LightBlue")
bp1$out
length(bp1$out)

bp2=boxplot(newCarsData$Salary,horizontal = FALSE,main="Salary", col="LightBlue")
bp2$out
length(bp2$out)

bp3=boxplot(newCarsData$Distance,horizontal = FALSE,main="Distance", col="LightBlue")
bp3$out
length(bp3$out)

bp3=boxplot(newCarsData$Work.Exp,horizontal = FALSE,main="Work.Exp", col="LightBlue")
bp3$out
length(bp3$out)

# There are 18 outliers for Age, 44 for Salary,7 for Distance & 27 for Work ex

names(newCarsData)

# Pair-wise scatter plots to visualize relationship. Categorical variables excluded
pairs(newCarsData[,c(1,5,6,7)], col="darkcyan", cex.labels = 1) 
# Seems some relationship between Age & Salary & Work exp. 

# to understand the correlation better, lets draw Correlation Plot
library(corrplot)
corrplot(cor(newCarsData[,c(1,5,6,7)]), method = "number")
# as indicated above, there is high correlation of age with workexperiece & salary

# Car Usage by Age, Distance & Salary
library(DataExplorer)
library(ggthemes)
library(ggplot2)
plot_boxplot(newCarsData,by="CarUsage",ggtheme = theme_excel())
qplot(newCarsData$Age,newCarsData$Salary, colour = newCarsData$CarUsage, geom="auto",xlab="Age", ylab="Salary")
# Employee with more age, distance, salary & work exp, are likely to use Car
# Employee who are more than 30 Year old with salary pkg more than 30 LKH P.A., are most likely use Car

# Check license impact on CarUsage
variable.names(newCarsData)
variable<-list('license')
plotG<-list()
for (i in variable) {plotG<-ggplot(newCarsData,aes_string(x=i,fill=newCarsData$CarUsage))+geom_bar(position = "stack")+scale_fill_discrete(name="CarUsage")}
print(plotG) 
# Employee not having license, likely to other modes of transportation. 
# Another interestig fact is, there are few employees using Car but not having license

# Identify who most likely to have license
plot_boxplot(newCarsData,by="license",ggtheme = theme_excel())
# Population with higher age group, salary & work exp most likely to have license


# Car usage comparision between Engineer & MBA
variable.names(newCarsData)
variable<-list('MBA')
plotMBA<-list('MBA')
for (i in variable) {plotMBA<-ggplot(newCarsData,aes_string(x=i,fill=newCarsData$CarUsage))+geom_bar(position = "stack")+scale_fill_discrete(name="CarUsage")}
print(plotMBA) 

variable<-list('Engineer')
plotEng<-list('Engineer')
for (i in variable) {plotEng<-ggplot(newCarsData,aes_string(x=i,fill=newCarsData$CarUsage))+geom_bar(position = "stack")+scale_fill_discrete(name="CarUsage")}
print(plotEng)
# Engineers are most likely to use Cars

#Followings are the observations form EDA
# 1) 13% of employee are using Car as mode of transportation
# 2) There is a correlation of Age with Exp & Salary
# 3) there is a correlation of Exp with Salary as well
# 4) Employee with more age, distance, salary & work exp, are likely to use Car
# 5) Employee who are more than 30 Year old with salary pkg more than 30 LKH P.A., are most likely use Car
# 6) Employee not having license, likely to other modes of transportation
# 7) Population with higher age group, salary & work exp most likely to have license
# 8) Engineers are most likely to use Cars


##########################################################################################
##########################Predctive MODEL#################################################
##########################################################################################

# Will create a new data file for Predictive Modeling Model
names(newCarsData)
ModelData <- newCarsData[,-c(9)] # Removed variable 'Transport' as prediction to be done on car usage only
str(ModelData)
# Split data in Train & Validation data
set.seed(124)
split<-sample(2,nrow(ModelData),replace=TRUE, prob=c(0.7,0.3))
Train<-ModelData[split==1,]
Validation<-ModelData[split==2,]

# Check Train & Test data string
str(Train)
str(Validation)

# check if all mode of transportation available in Train & Test
summary(Train$CarUsage)
summary(Validation$CarUsage)

# will create model on Train dataset with all variable
library(caret)
library(car)
Model1 <- glm(Train$CarUsage~., data=Train, family = binomial)
vif(Model1)
# VIF for Age & Work Ex is high. lets drop Age and re-run the model
Model1 <- glm(Train$CarUsage~., data =Train[,-c(1)], family = binomial)
vif(Model1) # VIF looks okay for all variables

summary((Model1))
# As per the model only Work Ex, distance & licence are the imporant variable

# Apply final model on test data and predicting implied probability
Pred.Logit <- predict.glm(Model1, newdata=Validation, type="response")
Pred.Logit
# Type is selected as 'Response' as it is going to match up with 1 or 0

# Create logit table with cutoff .50
Logit.Table<-table(Validation$CarUsage,Pred.Logit>.50)
Logit.Table

# Add precdicated value in the test data
Validation$pred<-predict.glm(Model1,newdata = Validation,type = "response")

# Overall Accuracy
sum(diag(Logit.Table))/sum(Logit.Table)
# Overall accuracy is 93%, however our objective is to predict how many employees will use a car
7/8
# Our model will be able to prdict 87% employees who is going to use Car. 


#AUC
pROC::auc(Validation$CarUsage, Validation$pred )

#Testing the model on Test Data which is unseen.
TestData<-read.csv("TestData.csv")
TestData<-TestData %>% select(-X)
TestData$CarUsage<-ifelse(TestData$Transport =='Car',1,0)
TestData<-TestData %>% select(-Transport)
TestData$Engineer <- as.factor(TestData$Engineer)
TestData$MBA <- as.factor(TestData$MBA)
TestData$license <- as.factor(TestData$license)
str(TestData)

TestData$pred <- predict.glm(Model1,newdata = TestData,type = "response")

Logit.Table<-table(TestData$CarUsage,TestData$pred>.50)
Logit.Table
140/148
18/20

# Takeaway from Logistic Model1 
#1) Overall Acc is 97.59%
#2) Model able to predict 87% employees who is willing ot use car
#3) AUC of the model is 98% which is good.

#################################################################
#####################Random Forrest##############################
#################################################################

# 1st Model with all variables
model_rf1 <- randomForest(CarUsage ~ ., data = Train, importance = TRUE)
model_rf1
206/217


#OOB estimate of  error rate: 5.16% 

model_rf2 <- randomForest(CarUsage ~ ., data = Train, ntree = 500, importance = TRUE)
model_rf2

model_rf3 <- model2 <- randomForest(CarUsage ~ ., data = Train, ntree = 500, mtry = 3, importance = TRUE)
model_rf3

model_rf4 <- randomForest(CarUsage ~ ., data = Train, ntree = 2000, keep.forest=FALSE, importance = TRUE)
model_rf4
importance(model_rf4)
varImpPlot(model_rf4)


model_rf5 <- randomForest(CarUsage ~ Age + Salary, data = Train, ntree = 500, mtry = 1, importance = TRUE)
model_rf5

model_rf6 <- randomForest(CarUsage ~ Age + Work.Exp + Salary + Distance + license, data = Train, ntree = 2000, mtry=5,importance = TRUE)
model_rf6
importance(model_rf6)
varImpPlot(model_rf6)
206/213



model_rf7 <- randomForest(CarUsage ~ Age + Work.Exp + Salary + Distance + license, data = Train, importance = TRUE)
model_rf7

model_rf8 <- randomForest(CarUsage ~ Age + Work.Exp + Salary + Distance + license + Gender + Engineer, data = Train, ntree = 2500, keep.forest=FALSE, importance = TRUE)
model_rf8

model_rf9 <- randomForest(CarUsage ~ Age + Work.Exp + Salary + Distance + license + Gender + Engineer, data = Train, ntree = 2500, keep.forest=FALSE, importance = TRUE)
model_rf9
# Checking all the combinations - 14.6% error rate is the least I am getting -- So I am going to predict based on model8 because the error rate is reducing when I added all the variables, but I added only few based on the importance and ntree, the error rate was going up

# Predicting on train set
predTrain <- predict(model_rf6, Train, na.action = na.pass, type = "class")
# Checking classification accuracy
table(predTrain, Train$CarUsage) 

#confusionMatrix(table(predTrain, Train$CarUsage)) 
length(predTrain)
length(Train$CarUsage)


# Predicting on Validation set
predval <- predict(model_rf6, Validation, type = "class")
# Checking classification accuracy

mean(predval == Validation$CarUsage)  
#Output - 0.8202247 ---> This is your accuracy
# Output with 30% validation dataset - 0.84

table(predval,Validation$CarUsage)

#Testing the model on TestData
TestData<-read.csv("TestData.csv")
TestData<-TestData %>% select(-X)
TestData$CarUsage<-ifelse(TestData$Transport =='Car',1,0)
TestData<-TestData %>% select(-Transport)
TestData$Engineer <- as.factor(TestData$Engineer)
TestData$MBA <- as.factor(TestData$MBA)
TestData$license <- as.factor(TestData$license)
TestData$CarUsage <- as.factor(TestData$CarUsage)
str(TestData)

predtest <- predict(model_rf6, TestData, type = "class")
mean(predtest == TestData$CarUsage)
table(predtest,TestData$CarUsage)



######## Naive Bayes ###########
Validation<-Validation %>% select(-pred)
Naive_Bayes_Model<-naive_bayes(Train$CarUsage ~., data=Train)
Naive_Bayes_Model

NB_Predictions<-predict(Naive_Bayes_Model,Validation)
table(Validation$CarUsage,NB_Predictions)
confusionMatrix(NB_Predictions,Validation$CarUsage)

NB_Predictions<-predict(Naive_Bayes_Model,TestData)
table(TestData$CarUsage,NB_Predictions)
confusionMatrix(NB_Predictions,TestData$CarUsage)
str(TestData

#######################################################
#   Now we will make KNN Model on Train dataset       #
#######################################################

names(Train)
colnames(Validation)
str(Train)
Train$Gender<-as.numeric(Train$Gender)
Train$Engineer<-as.numeric(Train$Engineer)
Train$MBA <- as.numeric(Train$MBA)
Train$license<-as.numeric(Train$license)
Train$CarUsage<-as.numeric(Train$CarUsage)
Validation$Gender<-as.numeric(Validation$Gender)
Validation$Engineer<-as.numeric(Validation$Engineer)
Validation$MBA <- as.numeric(Validation$MBA)
Validation$license<-as.numeric(Validation$license)
Validation$CarUsage<-as.numeric(Validation$CarUsage)
Validation<-Validation %>% select(-pred)

ks<-c(5,7,9,11,13,15)
accuracy.cars<-vector()
for (k in ks) {
  cars_model<-knn(Train[,-9],Validation[,-9],cl=Train[,9],k=k,prob = TRUE)
  cars.pred<-table(Validation[,9],cars_model)
  accuracy.model.cars<-sum(diag(cars.pred))/sum(cars.pred)
  accuracy.cars<-cbind(accuracy.cars,accuracy.model.cars)
}
accuracy.cars

#######################################################
#    From the above we find that the model has the    #
#    best accuracy when k=7                           #
#######################################################

#######################################################
#         Now check the accuracy on Test dataset      #
#######################################################
TestData<-read.csv("TestData.csv")
str(TestData)

TestData$CarUsage<-ifelse(TestData$Transport =='Car',1,0)
TestData$Gender<-as.numeric(TestData$Gender)
TestData$Engineer<-as.numeric(TestData$Engineer)
TestData$MBA <- as.numeric(TestData$MBA)
TestData$license<-as.numeric(TestData$license)
TestData$CarUsage<-as.numeric(TestData$CarUsage)
TestData[is.na(TestData)] <- 0

table(TestData$CarUsage)

colnames(TestData)
TestData<-TestData %>% select(-Transport)
TestData<-TestData %>% select(-X)
str(TestData)

#######################################################
#       Now let us predict using the value of k=7     #
#######################################################

cars_test<-knn(Train[,-9],TestData[,-9],cl=Train[,9],k=7,prob=TRUE)
cars.pred<-table(TestData[,9],cars_test)
cars.pred

146/148
23/24
# With k=7 we are getting 98.6% as the overall accuracy of the model

##########################################################
######################XGB-MOdel###########################
##########################################################

# Creating Test & Validation data for XGB Model
XGBData<-newCarsData[,-c(9)] # Removed the newly added variable CarUsage
str(XGBData)
summary(XGBData)
# Changing integer & Factor IVs in number
XGBData$Age<-as.numeric(XGBData$Age)
XGBData$Gender<-as.numeric(XGBData$Gender)
XGBData$Engineer<-as.numeric(XGBData$Engineer)
XGBData$MBA<-as.numeric(XGBData$MBA)
XGBData$Work.Exp<-as.numeric(XGBData$Work.Exp)
XGBData$license<-as.numeric(XGBData$license)
XGBData$CarUsage<-as.numeric(XGBData$CarUsage)
summary(XGBData$CarUsage)
XGBData$CarUsage<-ifelse(XGBData$CarUsage ==1,'No','Yes')
summary(XGBData$CarUsage)
XGBData$CarUsage<-as.factor(XGBData$CarUsage)
summary(XGBData)

set.seed(123)
split<-sample(2,nrow(XGBData),replace=TRUE, prob=c(0.7,0.3))
XGBTrain<-XGBData[split==1,]
XGBValidation<-XGBData[split==2,]


# Creating XGB Grid
xgb_grid <- expand.grid(
  nrounds = c(10,25,50,100,150),
  max_depth = c(1,2,3),  # defts can be between 1-5
  eta = 0.3,
  colsample_bytree = 1,
  subsample = 1,
  min_child_weight = 1,
  gamma=0)

# nround is number of iterations
library(caret)
View(xgb_grid)

train_control <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  verboseIter = FALSE, 
  allowParallel = TRUE,
  summaryFunction = twoClassSummary)

Model2<- train(CarUsage~., XGBTrain, trControl = train_control,
               tuneGrid = xgb_grid, method = "xgbTree",verbose = TRUE)


tune_results = Model2$results
Model2$bestTune
#Model with nround 100 is giving best result


# Evaluating XGB Model using validation data
val_xgb<-predict(Model2,XGBValidation)
confusionMatrix(val_xgb,XGBValidation$CarUsage)

# GBB Performance Evaluation
test_pred = predict(Model2, XGBValidation[,-c(9)], type="prob")
pred_df = data.frame(obs = XGBValidation$CarUsage,test_pred)

pred_df$obs

pred_df$pred <- ifelse(pred_df$Yes > 0.50, "Yes", "No")
pred_df$pred <- factor(pred_df$pred, levels = c("Yes","No"))
pred_df$obs <- factor(pred_df$obs, levels = c("Yes","No"))

# add predicted probability to the validation data
XGBvalidatin_with_pred <- XGBValidation[,-c(9)]
XGBvalidatin_with_pred$pred_prob <- test_pred$Yes
XGBvalidatin_with_pred$target <- XGBValidation[,c(9)]

# AUC
twoClassSummary(pred_df, lev = levels(pred_df$obs))
# AUC is approx 98.60 

# lift/gain chart
library(gains)
actual <- ifelse(pred_df$obs == "Yes",1,0)
gains(actual, pred_df$Yes)

confusionMatrix(data = pred_df$pred, reference = pred_df$obs)
# overalla accuracy is 97.56%
# Model is predicting 88% of employee who is going to use car
# As per the lift gain Chart, our model is able to predict 90% of the employee who will be 
# using car in the first 20% of observations

#####Conclusion#####
# Logistic & XGB model is providing good accuracy, however XGB is providing much better accuracy
# Now we will use the data which we kept aside before traning various models
# Read the test data from Drive

TestData<-read.csv("TestData.csv")
summary(TestData) # 24 Car Users
sum(is.na(TestData)) # there is no missing value
str(TestData)
#Changing int & factors to number except target variable
TestData$Age<-as.numeric(TestData$Age)
TestData$Gender<-as.numeric(TestData$Gender)
TestData$Engineer<-as.numeric(TestData$Engineer)
TestData$MBA<-as.numeric(TestData$MBA)
TestData$Work.Exp<-as.numeric(TestData$Work.Exp)
TestData$license<-as.numeric(TestData$license)
str(TestData)

# Converting target variable in a new variable & save it in a new dataset
TestData$CarUsage<-ifelse(TestData$Transport =='Car',"Yes","No")
TestData$CarUsage<-as.factor(TestData$CarUsage)
summary(TestData)

# Saving actual target variable separately
ActualCarUsers<-TestData$CarUsage
summary(ActualCarUsers)

# Removing Target Variable to run the model
TestData<-TestData[,-c(1,10,11)]
str(TestData)

# Try XGB Model on Test Data
test.predict<-predict(Model2,newdata = TestData)
confusionMatrix(test.predict,ActualCarUsers)
# Overall accuracy has been reduced to approx 94%
# Model is able to predict only 85% of employees who will be using car. Whereas prediction was 88% in Train & Valiation dataset

