
check.and.install.Package<-function(package_name){
  if(!package_name%in%installed.packages()){
    install.packages(package_name)
  }
}

# Required Packages
check.and.install.Package("ggplot2") # plotting
check.and.install.Package("knitr") # report formatting
check.and.install.Package("cluster") # kmeans clustering
check.and.install.Package("HSAUR") # silhouette plotting
check.and.install.Package("fpc") # numbers cluster plot
check.and.install.Package("lattice") # cluster plotting
check.and.install.Package("rpart") # Decision Tress data classification
check.and.install.Package("kernlab") # Support Vector Machines machine learning
check.and.install.Package("randomForest") # Random Forest machine learning
check.and.install.Package("rio") # Random Forest machine learning
check.and.install.Package("laeken")
check.and.install.Package("colorspace")
check.and.install.Package("VIM")
check.and.install.Package("corrplot")
check.and.install.Package("caret")
check.and.install.Package("psych")
check.and.install.Package("pander")
check.and.install.Package("magrittr")
install.packages("glmnet", repos = "http://cran.us.r-project.org")


## Default repo

library(ggplot2) #ggplot2 is a data visualization package for the statistical programming language R.
library(knitr) #KnitR is a really important tool for reproducible research. You create documents that are a mixture of text and code; when processed through KnitR, the code is replaced by the results and/or figures produced.
library(randomForest) #e R package "randomForest" is used to create random forests
library(corrplot) #The corrplot package is a graphical display of a correlation matrix, confidence interval. It also contains some algorithms to do matrix reordering. 
library(lattice)
library(caret) #caret has several functions that attempt to streamline the model building and evaluation process, as well as feature selection and other techniques. One of the primary tools in the package is the train function which can be used to. evaluate, using resampling, the effect of model tuning parameters on performance.
library(colorspace)
library(grid)
library(data.table)
library(VIM) #VIM: Visualization and Imputation of Missing Values. New tools for the visualization of missing and/or imputed values are introduced
library(MASS) #Support Functions and Datasets for Venables and Ripley's MASS
# lda: Linear Discriminant Analysis
# qda: Quadratic Discriminant Analysis
library(psych)
library(pander)
library(magrittr)
library(dplyr)
library(caTools)
library(glmnet)  #This is where ridge and LASSO reside


###################### Leitura do dataset 
# Set Working Directory to folder containing the folder with bank additional data,
# and the Bank_Marketing_Classification.Rmd file

#bank-additional-full.csv with all examples (41188) and 20 inputs, ordered by date (from May 2008 to November 2010), very close to the data analyzed in [Moro et al., 2014]
setwd("C:/Users/Administrator/Google Drive/Universidade-Minho/Interviews-preparation/portuguese-bank-marketing-dataset/bank-additional")
bank_marketing_data_full <- read.csv("bank-additional-full.csv", sep=";",header = TRUE)

#Let's have a look at the structure of our data. We have 20 predictors and 1 output variable and altogether 41188 observations.

dim(bank_marketing_data_full)
#[1] 41188    21

#####################

#Fazer uma explica��o do dataset.
#Missing data 
#  N�o existe missing data contudo temos 'unknowns' que ser�o tratados como missing values
#  There are we have 45211 observations of 21 variables in the dataset (10-Numerical Variables and
#                                                                       11-Categorical Variables).
#  From the distribution of Target variable: "is_success" it is found that data is imbalanced because there is approx. 88% is 'no' and 12% is 'yes'.
  
names(bank_marketing_data_full)

summary (bank_marketing_data_full)

head (bank_marketing_data_full)
#[1] "age"            "job"            "marital"        "education"      "default"        "housing"        "loan"          
#[8] "contact"        "month"          "day_of_week"    "duration"       "campaign"       "pdays"          "previous"      
#[15] "poutcome"       "emp.var.rate"   "cons.price.idx" "cons.conf.idx"  "euribor3m"      "nr.employed"    "y


# First of all, let's know our variables by Category 
split(names(bank_marketing_data_full),sapply(bank_marketing_data_full, function(x) paste(class(x), collapse=" ")))

#str - Compactly Display the Structure of an Arbitrary R Object
str(bank_marketing_data_full)

#It may be useful to prepare some very basic descriptive statistics at this early stage of the data analysis project on raw data set. As one can see below there are categories like 'unknownn' and 'other' at many variables. We keep it in mind for later steps.
pander(summary(bank_marketing_data_full))

#Check NAs and less than 0 values

  #No missing data 
  sum(is.na(bank_marketing_data_full))
  
  # Check missing values for all columns
  sapply(bank_marketing_data_full, function(x) sum(is.na(x)))
  
  # Check less than zero values for all columns
  sapply(bank_marketing_data_full, function(x) sum(x<0, na.rm=TRUE))

#As we will see later there are some variables with 'unknown' values (not equal to NAs). 
#There are two variables with negative values by default (emp.var.rate and cons.conf_idx).

#Target variable: Y (binary: 'Yes' / 'No')
#This variables shows whether the customer subscribed for a term deposit.
#More than 11% of customers subscribed for term deposit that altogether means 4640 observations.
pander(bank_marketing_data_full %>% group_by(y) %>% summarize(n = n()) %>% mutate(percentage = n/sum(n)*100))
  

#- EDA (EXPLORATORY DATA ANALYSIS)
#- Teremos de apresentar alguns gr�ficos e explicar os mais relevantes

#---------------------------------------------------------------------------------------------------------------
#--------------------------------------- plot of numerical features 
#---------------------------------------------------------------------------------------------------------------

#Data Visualization
hist(bank_marketing_data_full$age,breaks = 20,col="green",main = "distribution of age") 

bank_marketing_data_full$lnage <- log(bank_marketing_data_full$age)
pander(summary(bank_marketing_data_full$lnage))

ggplot(bank_marketing_data_full, aes(x = lnage)) + geom_histogram(binwidth = 0.1, col = "white") + theme_bw()

## Age looks like it is right skewed, might as well try transformation
hist(bank_marketing_data_full$duration,breaks = 20,col="green",main = "distribution of call-duration")


nrow(bank_marketing_data_full[bank_marketing_data_full$duration > 3600, ])
bank_marketing_data_full <- bank_marketing_data_full %>% filter(duration < 3600) 

ln_duration <- log(bank_marketing_data_full$duration)
ggplot(bank_marketing_data_full, aes(x = ln_duration)) + geom_histogram(binwidth = 0.1) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#transforming it to cuberoot makes the data into normal distribution
hist(bank_marketing_data_full$campaign,breaks = 20,col="green",main = "distribution of campaign")
hist(bank_marketing_data_full$pdays,breaks = 20,col="green",main = "distribution of pdays")
hist(bank_marketing_data_full$previous,breaks = 20,col="green",main = "distribution of previous")
hist(bank_marketing_data_full$emp.var.rate,breaks = 20,col="green",main = "distribution of emp.var.rate")
hist(bank_marketing_data_full$cons.price.idx,breaks = 20,col="green",main = "distribution of cons.price.idx")
hist(bank_marketing_data_full$cons.conf.idx,breaks = 20,col="green",main = "distribution of cons.conf.idx")
hist(bank_marketing_data_full$euribor3m,breaks = 20,col="green",main = "distribution of euribor3m")
hist(bank_marketing_data_full$nr.employed,breaks = 20,col="green",main = "distribution of nr.employed")




#---------------------------------------------------------------------------------------------------------------
#--------------------------------------- plot of categorial features 
#---------------------------------------------------------------------------------------------------------------

ggplot(bank_marketing_data_full,aes(bank_marketing_data_full$marital))+geom_bar(stat = "count",fill="Green")+labs(x="Reponse",title="Martial status")
ggplot(bank_marketing_data_full,aes(bank_marketing_data_full$education))+geom_bar(stat = "count",fill="Green")+labs(x="Reponse",title="Education")
ggplot(bank_marketing_data_full,aes(bank_marketing_data_full$housing))+geom_bar(stat = "count",fill="Green")+labs(x="Reponse",title="housing loan")
ggplot(bank_marketing_data_full,aes(bank_marketing_data_full$y))+geom_bar(stat = "count",fill="Green")+labs(x="Reponse",title="subscribe (yes/no) a term deposit (variable y)")



#- NORMALIZA��O
#Porqu�?
#- modelo simplificado... distribuiçao gaussiana (ou aprox. gaussiana)

#- MULTICOLINEARIDADE
#- ANOVA
#- Ver mais que fazer neste ponto
#- cor[c(1,11,15,...)] mostra as correla��es, ou ent�o com os plots / boxplot(y~varquantitativa), uma vai ser var quantitativa idade e vamos ver as idades para as pessoas que fizeram ou nao emprestimo, e se ambas estiverem muito parecidas quer dizer que a idade nao influencia muito o resultado. Usar o boxplot e o p-value para verificar se bate certo.


#################Correlation Analysis#################
#It emphsize on what we say using box plot, It can tell if predictor is a good predictor or not a good predictor
#This analysis can help us decide if we can drop some columns/predictors depending upon its correlation with the outcome variable
pairs.panels(bank_marketing_data_full[, c(1:10,21)])
pairs.panels(bank_marketing_data_full[, c(11:17)])

####################################################################
#TRAINING DATA / TEST DATA (splitting data)
#creating train and test datasets
#analisar a proporçao de acertos. remover o y no test data.

require(caTools)  # loading caTools library
## Loading required package: caTools
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(bank_marketing_data_full,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train_data =subset(bank_marketing_data_full,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data=subset(bank_marketing_data_full, sample==FALSE)
dim(train_data)
dim(test_data)

###############################################################
#Logistic Regression with cross validation

model<-glm(y~.,data = train_data,family = binomial)
summary(model)
AIC(model)

#Checking variable importance for GLM
varImp(object=model)

## Prediction for glm model
prediction1<-predict(model,test_data,type = "response")
prediction1<-round(prediction1,0)
prediction1<-as.factor(prediction1)
levels(prediction1)<-c("No to despoist","yes to deposit")
actual1<-test_data[,21]
levels(actual1)<-c("No to despoist","yes to deposit")

## confusion matrix for glm model
conf1<-confusionMatrix(actual1,prediction1,positive = "yes to deposit")
conf1 # from the glm model we can see that accuracy is 90.6%


###############################################################

###################################################
#Logistic Regression with k-fold cross-validation

Control1<-trainControl(method = "cv",number = 10 , savePredictions = TRUE)
mod_fit<-train(y ~ ., data = bank_marketing_data_full, method = "glm", family="binomial",trControl = Control1,tuneLength = 5)
summary(mod_fit)


pred = predict(mod_fit, newdata=test_data)
AIC(pred)

confusionMatrix(pred, test_data$y)

#------------- duration+pdays+month+age+contact+job
Control1<-trainControl(method = "cv",number = 10 , savePredictions = TRUE)
mod_fit<-train(y ~ duration+pdays+month+age+contact+job, data = train_data, method = "glm", family="binomial",trControl = Control1,tuneLength = 5)
summary(mod_fit)

pred = predict(mod_fit, newdata=test_data)
#table(pred,test_data$y)
confusionMatrix(data=pred, test_data$y)


# The following variables seem to be the most relevant inputs in 
# predicting the Success rate of bank direct marketing campaign
# . Duration - call duration
# . Pdays - Number of days since last contact
# . Month - month of contact
# . Age - customer age
# . Contact - cellular/ Telephone
# . Job


###############################################################
#####################Ridge Regression - Variable selection
#important concept is related to variable/predictor selection so as to find out the 
#important variables which are used by the classifier in making the prediction.

require(glmnet)

x = model.matrix(y~., train_data)[,-1]
head(x)
y = train_data$y;

x = model.matrix(y~., bank)[,-1];
y = bank$y;
resp = rep(0,nrow(train_data));
resp[y == 'yes'] = 1;
grid = 10^seq(10,-2,length=100);
dim(train_data)
train = sample(nrow(train_data), 29000);
test = (-train);
test.X = train_data[test,];
test.Y = resp[test];
lasso.mod = glmnet(x[train,], resp[train], alpha = 1, lambda = grid);
plot(lasso.mod);

###############################################################
#####################LASSO
#O LASSO (Least Absolute Shrinkage and Selection Operator ) é um método de selecção e 
#shrinkage proposto para os modelos de regressão linear, introduzido por Robert
#Tibshirani em 1995.

# Considerando que tenho um data frame de nome dados, sendo a primeira coluna a classe

y <- as.double(as.matrix(train_data[, train_data$y])) # Somente classe
x <- as.matrix(train_data$y <- NULL) # Remove classe

cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef(cv.lasso, s=cv.lasso$lambda.min)


#######################################################################################################################




#################################### A PARTIR DAQUI É LIXO ####################################


#Data Correlation Analysis
#In this section, the Correlation of “Age”, “Job”, “Marital”, “Education”, “Housing”, and “Loan is examined to determine Predictive effectiveness, accuracy, and precision. A Correlation Test is performed, the results displayed, and then a Level Plot is created that displays the corresponding values. With a p-value near 0.5 correlation of the Independent Variables, (”Housing" and “Loan”), to the Dependent Variable, (“y”), is proven. The 95% Confidence Interval centers around 0. Therefore, 95% confidence in the correlation is proven.
## Correlation of Age to Term Deposit
## Correlation of Job to Term Deposit
## Correlation of Marital to Term Deposit
## Correlation of Education to Term Deposit
## Correlation of Housing to Term Deposit
## Correlation of Loan to Term Deposit
## Correlation of Housing and Loan to Term Deposit


#- FEATURES SIGNIFICATIVAS P-VALUE NA REGRESSÃO LOGISTICA
#- variáveis nao significativas podemos remover -> simplificação do problema sem grande efeito no resultado final.
#- podemos também usar LASSO para descobrir as vars preditivas.


##################################################################
######################### END OF PROJECT #########################
##################################################################


Extra algorithms


##############################
# Linear Discriminant Analysis
lda.fit=lda(y ~ .,data=train_data)
lda.fit
plot(lda.fit)
lda.pred<-predict(lda.fit, train_data)
names(lda.pred)
summary(lda.pred)

#################################
# Quadratic Discriminant Analysis
str(train_data)
qda.fit=qda(y~.,data=train_data)
qda.fit
qda.class=predict(qda.fit,train_data)$class
table(qda.class,train_data)
mean(qda.class==train_data)




- MODELOS A UTILIZAR
- Logistic Regression - focar aqui! já agora... fazer os seguintes:
  - LDA
- QDA
- KNN

- CROSS VALIDATION

- MODEL ACCURACY

- BIAS/VARIANCE TRADEOFF
- usar na comparaçao entre modelos (LR, LDA, QDA, etc...)
- nao se justifica fazer isto...


Some interesting sites:
  
https://github.com/vinay2k2/Bank-Marketing-Data-Analysis/blob/master/Bank_Marketing_Campaign_Analysis.pdf
https://rpubs.com/ID_Tech/S1
http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
http://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/#logistic-function
http://ricardoscr.github.io/como-usar-ridge-e-lasso-no-r.html
https://www.kaggle.com/rogrezende/cross-validation-glm

The best
http://rstudio-pubs-static.s3.amazonaws.com/157695_7fe1c26be1b54b01a520a9b73ae99c85.html

https://www.r-bloggers.com/evaluating-logistic-regression-models/