#path
path<-"/Users/anindaroy/Documents/Thesis/Dataset for thesis"
setwd(path)
rm(summary)

#Library
library(readxl)
library(MASS)
library(dplyr)
library(tidyverse)
library(caret)
library(lattice)
library(VIM)
library(foreign)
library(kernlab)
library(e1071)
library(stats)
library(base)
library(ggplot2)
library(mice)
library(ResourceSelection)
library(survey)
library("ROCR")
library("pROC")
library(randomForest)
library(DescTools)
library(caTools)
library(nnet)
library(Metrics)

#Dataset.....................................................................
my_data<-read.spss("/Users/anindaroy/Documents/Thesis/Dataset for thesis/Obesity_dataset.sav",header=TRUE)
my_data<-as.data.frame(my_data)
names(my_data)
View(my_data)
attach(my_data)
summary(my_data)
str(my_data)


# Chisquare association..............................
chisq.test(table(my_data$NObeyesdad,my_data$family_history_with_overweight),correc=FALSE)
chisq.test(table(my_data$NObeyesdad,my_data$FAVC),correc=FALSE)
chisq.test(table(my_data$NObeyesdad,my_data$CAEC),correc=FALSE)
chisq.test(table(my_data$NObeyesdad,my_data$CALC),correc=FALSE)
chisq.test(table(my_data$NObeyesdad,my_data$SMOKE),correc=FALSE)
chisq.test(table(my_data$NObeyesdad,my_data$Age_group),correc=FALSE)


# EDA ---------------------------------------------------------------------

# correlation for all variables
library(corrplot)
corrplot(cor(data.frame(my_data$Gender,my_data$family_history_with_overweight),method="spearman"),
         method = "number",
         type = "upper" # show only upper side
)

# Gender specific eating habits, physical condition, Obesity level

my_data %>% filter(NObeyesdad=="Insufficient_Weight"| NObeyesdad=="Normal_Weight"| NObeyesdad=="Obesity_Type_I"| NObeyesdad=="Obesity_Type_II"| NObeyesdad=="Obesity_Type_III"| NObeyesdad=="Overweight_Level_I"| NObeyesdad=="Overweight_Level_II") %>% 
  ggplot(aes(NObeyesdad,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of obesity level among Male and Female",x="Obesity Level",y="Percentage")

my_data %>% filter(Gender=="Female"|Gender=="Male") %>% 
  ggplot(aes(Gender,fill=FAVC))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Frequent consumption of high caloric food among Men and Women",x="Gender",y="Percentage")

my_data %>% filter(FCVC=="Never"|FCVC=="Sometimes"|FCVC=="Always") %>% 
  ggplot(aes(FCVC,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Frequent consumption of vegetables among Men and Women",x="Vegetable Consumption",y="Percentage")

my_data %>% filter(NCP=="Between1-2"|NCP=="Three"|NCP=="More than three"|NCP=="More than four") %>% 
  ggplot(aes(NCP,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Men and Women with number of main meals category",x="# Of Main Meals",y="Percentage")

my_data %>% filter(CAEC=="No"|CAEC=="Sometimes"|CAEC=="Frequently"|CAEC=="Always") %>% 
  ggplot(aes(CAEC,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Men and Women with consumption of food between meals",x="Food consumption between meals",y="Percentage")

my_data %>% filter(CH2O=="Less than one littre"|CH2O=="Between 1-2 littres"|CH2O=="More than 2 littres") %>% 
  ggplot(aes(CH2O,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Men and Women with consumption of water daily",x="Daile water consumption",y="Percentage")

my_data %>% filter(CALC=="I don't drink"|CALC=="Sometimes"|CALC=="Frequently"|CALC=="Always") %>% 
  ggplot(aes(CALC,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Men and Women with consumption of alcohol",x="Alcohol consumption",y="Percentage")

my_data %>% filter(SMOKE=="No"|SMOKE=="Yes") %>% 
  ggplot(aes(SMOKE,fill=Gender))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Percentage of Men and Women with smoking status",x="Smoking Status",y="Percentage")

my_data<-my_data %>% mutate(Age_group=cut(my_data$Age,breaks=c(0,20,25,30,35,40,45,50,55,60,65)))
barchart(my_data$Age_group)
table(my_data$Age_group)
my_data %>% filter(Age_group=="(0,20]"|Age_group=="(20,25]"|Age_group=="(25,30]"|Age_group=="(30,35]"|Age_group=="(35,40]"|Age_group=="(40,45]"|Age_group=="(45,50]"|Age_group=="(50,55]"|Age_group=="(55,60]"|Age_group=="(60,65]") %>% 
  ggplot(aes(Age_group,fill=NObeyesdad))+geom_bar(position="fill",alpha=1)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  labs(title="Obesity level in Different Age Groups",x="Age Groups",y="Percentage")

# Train and Test dataset---------------------------------------------------------
set.seed(300)
indxTrain <- createDataPartition(y = my_data$NObeyesdad,p = 0.75,list = FALSE)
dataTrain <- my_data[indxTrain,]
dataTest <- my_data[-indxTrain,]
nrow(dataTrain)
nrow(dataTest)
attach(dataTrain)
attach(dataTest)

# Model1: Multinomial logistic regression
dataTrain$NObeyesdad<-relevel(dataTrain$NObeyesdad,ref="Insufficient_Weight")

multinom_model<-multinom(NObeyesdad~., data=dataTrain)
summary(multinom_model)

# 2-tailed Z test
z<-summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
p<-(1-pnorm(abs(z),0,1))*2
p

# Confusion Matrix & Misclassification Error - Training Data
p<-predict(multinom_model,dataTrain)
tab<-table(p,dataTrain$NObeyesdad)

accuracy<-sum(diag(tab))/sum(tab) #Accuracy of the model

1-accuracy #Misclassification error

# Confusion Matrix & Misclassification Error - Testing Data
p1<-predict(multinom_model,dataTest)
tab<-table(p1,dataTest$NObeyesdad)

confusionMatrix(p1,dataTest$NObeyesdad)

mae(dataTrain$NObeyesdad, predict(multinom_model))


# ROC curve for logistic model1-------------------------
pred = predict(multinom_model, newdata=dataTest,type="prob")
nbROC <- multiclass.roc(dataTest$NObeyesdad,pred,percent=TRUE,levels=c("Insufficient_Weight","Normal_Weight","Obesity_Type_I",
                                                          "Obesity_Type_II","Obesity_Type_III","Overweight_Level_I",
                                                          "Overweight_Level_II"))
nbROC

roc<-nbROC[['rocs']]
r1<-roc[[1]]
plot.roc(r1,pred,type="S", print.thres= 0.5)

# Goodness of fit------------------------------------------
# Perform likelihood ratio test for differences in models
anova(glm.fits1_train,glm.fits2_train,test="LRT")
anova(glm.fits3_train,glm.fits1_train,test="Chisq")
anova(glm.fits4_train,glm.fits1_train,test="LRT")

# Perform Pseudo R^2
PseudoR2(glm.fits1_train, which = NULL)

#Hosmer-Lemeshow test
h1<-hoslem.test(dataTrain$At.study.time, fitted(glm.fits1_train), g=10)
cbind(h1$observed,h1$expected)
h2<-hoslem.test(dataTrain$At.study.time, fitted(glm.fits2_train), g=10)
cbind(h2$observed,h2$expected)
h3<-hoslem.test(dataTrain$At.study.time, fitted(glm.fits4_train), g=10)
cbind(h3$observed,h3$expected)

# Wald test for univariable......................................
varImp(multinom_model)
library(survey)
regTermTest(multinom_model,"Gender")
regTermTest(multinom_model,"Age")
regTermTest(multinom_model,"Height")
regTermTest(multinom_model,"Weight")
regTermTest(multinom_model,"family_history_with_overweight")
regTermTest(multinom_model,"FAVC")
regTermTest(multinom_model,"FCVC")
regTermTest(multinom_model,"NCP")#
regTermTest(multinom_model,"CAEC")#
regTermTest(multinom_model,"SMOKE")#
regTermTest(multinom_model,"CH2O")#
regTermTest(multinom_model,"SCC")
regTermTest(multinom_model,"FAF")
regTermTest(multinom_model,"TUE")
regTermTest(multinom_model,"CALC")#
regTermTest(multinom_model,"MTRANS")


# Model3: Linear SVM model -------------------------------------------------------
trctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3,classProbs=TRUE)
svm_Linear <- train(NObeyesdad~., data = dataTrain, method = "svmLinear",
                    trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10, metric="ROC")

svm_Linear

#Classification Matrix
test_pred <- predict(svm_Linear, newdata = dataTest)
test_pred
confusionMatrix(test_pred, dataTest$NObeyesdad) #Accuracy:74.3%%


# ROC curve..........................................
library(pROC)
svmPredict <- predict(svm_Linear,newdata = dataTest , type="prob")
svmROC <- roc(dataTest$At.study.time,svmPredict[,"Yes"])
svmROC
plot(svmROC, type="S", print.thres= 0.5) # AUC: 86.4%

# Model4: Radial Kernel SVM -----------------------------------------------
# For train data
Radialsvm=svm(NObeyesdad~.,data= dataTrain,kernel="radial",gamma=3,cost=10,scale=F)
Radialsvm
#ConfusionMatrix
table(predicted=Radialsvm$fitted,actual=dataTrain$NObeyesdad)
#misclassification Rate
mean(Radialsvm$fitted!=dataTrain$NObeyesdad)*100 

#Classification Matrix
test_pred <- predict(Radialsvm, newdata = dataTest)
test_pred
confusionMatrix(table(test_pred, dataTest$NObeyesdad)) #Accuracy: 69%

# ROC curve..........................................
library(pROC)
svmPredict <- predict(Radialsvm,newdata = dataTest , type="Prob")
svmROC <- roc(dataTest$At.study.time,svmPredict[])
svmROC
plot(svmROC, type="S", print.thres= 0.5) #AUC:86.04%

# Model5: KNN model -------------------------------------------------------
#Checking distibution in origanl data and partitioned data
prop.table(table(dataTrain$NObeyesdad)) * 100
prop.table(table(dataTest$NObeyesdad)) * 100
prop.table(table(my_data$NObeyesdad)) * 100


# kNN requires variables to be normalized or scaled. caret provide --------
trainX <- dataTrain[,names(dataTrain) != "NObeyesdad"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

#Now verifying 2 class summary function
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE)
knnFit <- train(NObeyesdad~., data = dataTrain, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit, print.thres = 0.5, type="S")

#Get the confusion matrix to see accuracy value and other parameter values
knnPredict <- predict(knnFit,newdata = dataTest )
confusionMatrix(knnPredict, dataTest$NObeyesdad )

# ROC curve..........................................
library(pROC)
knnPredict <- predict(knnFit,newdata = dataTest , type="prob")
knnROC <- roc(dataTest$At.study.time,knnPredict[,"Yes"])
knnROC
plot(knnROC, type="S", print.thres= 0.5)


# Model6: Random Forest ---------------------------------------------------

#Now verifying 2 class summary function
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE)

rfFit <- train(NObeyesdad~., data = dataTrain, method = "rf",prox=TRUE, trControl = ctrl)
rfFit

#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(rfFit, print.thres = 0.5, type="S")

#Get the confusion matrix to see accuracy value and other parameter values
rfPredict <- predict(rfFit,newdata = dataTest )
confusionMatrix(rfPredict, as.factor(dataTest$NObeyesdad))

mae(df$y, predict(model))


# ROC curve..........................................
library(pROC)
rfPredict <- predict(rfFit,newdata = dataTest , type="prob")
rfROC <- roc(dataTest$At.study.time,rfPredict[,"Yes"])
rfROC
plot(rfROC, type="S", print.thres= 0.5) # Accuracy:75%


# Model7: Naive Bayes -----------------------------------------------------

xTrain = dataTrain[,-17] # removing y-outcome variable.
yTrain = dataTrain$NObeyesdad # only y.

xTest = dataTest[,-17]
yTest = dataTest$NObeyesdad

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

prop.table(table(predict(model$finalModel,xTest)$class,yTest)) # table() gives frequency table, prop.table() gives freq% table.#Accuracy 74.2%


# Model8: Decision Tree ---------------------------------------------------
library(rpart)
library(rpart.plot)  # for better formatted plots than the ones in rpart
library(skimr)  # neat alternative to glance + summary

set.seed(300)
oj.full_class <- rpart(formula = NObeyesdad~.,
                       data = dataTrain,
                       method = "class",  # classification (not regression)
                       xval = 10  # 10-fold cross-validation 
)

rpart.plot(oj.full_class, yesno = TRUE)

plotcp(oj.full_class)


oj.class.pred <- predict(oj.full_class, dataTest, type = "class")

plot(dataTest$NObeyesdad, oj.class.pred, 
     main = "Simple Classification: Predicted vs. Actual",
     xlab = "Actual",
     ylab = "Predicted")

(oj.class.conf <- confusionMatrix(data =oj.class.pred, 
                                  reference = as.factor(dataTest$NObeyesdad))) # Accuarcy 64.3%

# ROC curve..........................................
library(pROC)
oj.class.pred <- predict(oj.full_class, dataTest, type = "prob")
nbROC <- roc(dataTest$At.study.time,oj.class.pred[,"Yes"])
nbROC
plot(nbROC, type="S", print.thres= 0.5) # AUC 68.67%

# ROC curves for different classifiers
library("ROCR")
library("gplots")

#logistic model
pred = predict(glm_fit, newdata=dataTest,type="prob")
nbROC <- roc(dataTest$At.study.time,pred[,"Yes"])
nbROC
plot(nbROC, type="S", print.auc=TRUE,print.thres= 0.5,col="blue") 

# SVM

svmPredict <- predict(svm_Linear,newdata = dataTest , type="prob")
svmROC <- roc(dataTest$At.study.time,svmPredict[,"Yes"])
svmROC
plot(svmROC, type="S", print.auc=TRUE,print.thres= 0.5,col="green") 

#KNN
knnPredict <- predict(knnFit,newdata = dataTest , type="prob")
knnROC <- roc(dataTest$At.study.time,knnPredict[,"Yes"])
knnROC
plot(knnROC, type="S", print.auc=TRUE,print.thres= 0.5,col="red") 

# Random forest
rfPredict <- predict(rfFit,newdata = dataTest , type="prob")
rfROC <- roc(dataTest$At.study.time,rfPredict[,"Yes"])
rfROC
plot(rfROC, type="S",print.auc=TRUE, print.thres= 0.5,col="violet")

# Combine
plot(nbROC, type="S",col="blue",xlab="1-Specificity") 
plot(svmROC, type="S",col="green",print.auc.y = .4, add = TRUE) 
plot(knnROC, type="S",col="red",print.auc.y = .3, add = TRUE) 
plot(rfROC, type="S",col="violet",print.auc.y = .2, add = TRUE)
legend("bottomright", c("ROC of SVM(AUC=0.889)", "ROC of LR(AUC=0.866)","ROC of KNN(AUC=0.810)","ROC of RF(AUC=0.774)"), lty=1, 
       col = c("green","blue","red", "violet"), bty="n", inset=c(0,0.10,0.15,0.20))

# PCA

pca<-prcomp(t(my_data), scale=TRUE) 
