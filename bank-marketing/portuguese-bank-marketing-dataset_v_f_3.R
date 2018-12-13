#tem muito material
#https://medium.com/@jameschen_78678/which-customers-are-more-likely-to-respond-to-banks-marketing-campaigns-3f00c512268d


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
install.packages("pROC")
install.packages("PerformanceAnalytics")


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

setwd("/Users/tiagoalves/Desktop/Mestrado/Data Science/Aprendizagem Automatica 1/Projeto/bank-marketing")
bank_marketing_data_full <- read.csv("bank-additional-full.csv", sep=";",header = TRUE)


#Let's have a look at the structure of our data. We have 20 predictors and 1 output variable and altogether 41188 observations.

dim(bank_marketing_data_full)
#[1] 41188    21

#####################

#Fazer uma explica??o do dataset.
#Missing data 
#  N?o existe missing data contudo temos 'unknowns' que ser?o tratados como missing values
#  There are we have 45211 observations of 21 variables in the dataset (10-Numerical Variables and
#                                                                       11-Categorical Variables).
#  From the distribution of Target variable: "is_success" it is found that data is imbalanced because there is approx. 88% is 'no' and 12% is 'yes'.
  
names(bank_marketing_data_full)
#[1] "age"            "job"            "marital"        "education"      "default"        "housing"        "loan"          
#[8] "contact"        "month"          "day_of_week"    "duration"       "campaign"       "pdays"          "previous"      
#[15] "poutcome"       "emp.var.rate"   "cons.price.idx" "cons.conf.idx"  "euribor3m"      "nr.employed"    "y

summary (bank_marketing_data_full)

head (bank_marketing_data_full)


# First of all, let's know our variables by Category 
split(names(bank_marketing_data_full),sapply(bank_marketing_data_full, function(x) paste(class(x), collapse=" ")))

#str - Compactly Display the Structure of an Arbitrary R Object
str(bank_marketing_data_full)

#It may be useful to prepare some very basic descriptive statistics at this early stage of the data analysis project on raw data set. As one can see below there are categories like 'unknownn' and 'other' at many variables. We keep it in mind for later steps.
summary(bank_marketing_data_full)

#Check NAs and less than 0 values

  #No missing data 
  sum(is.na(bank_marketing_data_full))
  
  # Check missing values for all columns
  sapply(bank_marketing_data_full, function(x) sum(is.na(x)))
  
  # Check less than zero values for all columns
  sapply(bank_marketing_data_full, function(x) sum(x<0, na.rm=TRUE))

#As we will see later there are some variables with 'unknown' values (not equal to NAs). 
#There are two variables with negative values by default (emp.var.rate and cons.conf_idx).

  bank_marketing_data_full[bank_marketing_data_full=="unknown"] <- NA
  sum(is.na(bank_marketing_data_full))
  # Check missing values for all columns
  sapply(bank_marketing_data_full, function(x) sum(is.na(x)))

  aggr_plot <- aggr(bank_marketing_data_full, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bank_marketing_data_full), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
  
  counts <- table(bank_marketing_data_full$y)
  barplot(counts,col=c("blue","red"),legend = rownames(counts), main = "Term Deposit")
  
  
#Target variable: Y (binary: 'Yes' / 'No')
#This variables shows whether the customer subscribed for a term deposit.
#More than 11% of customers subscribed for term deposit that altogether means 4640 observations.
pander(bank_marketing_data_full %>% group_by(y) %>% summarize(n = n()) %>% mutate(percentage = n/sum(n)*100))
  

#- EDA (EXPLORATORY DATA ANALYSIS)
#- Teremos de apresentar alguns gr?ficos e explicar os mais relevantes

#---------------------------------------------------------------------------------------------------------------
#--------------------------------------- plot of numerical features 
#---------------------------------------------------------------------------------------------------------------

#Data Visualization
attach(bank_marketing_data_full)


#Box chart of Duration versus y

p_age <- ggplot(bank_marketing_data_full, aes(factor(y), age)) + geom_boxplot(aes(fill = factor(y)))
p_age

p_duration <- ggplot(bank_marketing_data_full, aes(factor(y), duration)) + geom_boxplot(aes(fill = factor(y)))
p_duration

p_campaign <- ggplot(bank_marketing_data_full, aes(factor(y), campaign)) + geom_boxplot(aes(fill = factor(y)))
p_campaign

p_pdays <- ggplot(bank_marketing_data_full, aes(factor(y), pdays)) + geom_boxplot(aes(fill = factor(y)))
p_pdays

#number of contacts performed before this campaign and for this client (numeric)
p_previous <- ggplot(bank_marketing_data_full, aes(factor(y), previous)) + geom_boxplot(aes(fill = factor(y)))
p_previous

boxplot(bank_marketing_data_full$age~bank_marketing_data_full$y) 

barplot(table(y,age), beside=T, legend.text=c("age","term deposit"), main=
          "Distribution of birth months per gender", xlab="month", ylab="frequency")

barplot(table(y,duration))
barplot(table(y,pdays))
barplot(table(y,month))
barplot(table(y,age))

# . Duration - call duration
# . Pdays - Number of days since last contact
# . Month - month of contact
# . Age - customer age
# . Contact - cellular/ Telephone
# . Job


bank_marketing_data_full$lnage <- log(bank_marketing_data_full$age)

pander(summary(bank_marketing_data_full$lnage))
summary(bank_marketing_data_full$lnage)

# Normal probability plot
# A normal probability plot is a plot for a continuous variable that helps to determine whether a 
# sample is drawn from a normal distribution. If the data is drawn from a normal distribution, 
# the points will fall approximately in a straight line. 
# If the data points deviate from a straight line in any systematic way, 
# it suggests that the data is not drawn from a normal distribution.1

#https://www.statmethods.net/graphs/density.html

pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 

?qqnorm
qqnorm(bank_marketing_data_full$age)
qqline(bank_marketing_data_full$age,col = "steelblue",lwd=2)

qqnorm(bank_marketing_data_full$lnage)
qqline(bank_marketing_data_full$lnage,col = "steelblue",lwd=2)

# Filled Density Plot
d <- density(bank_marketing_data_full$age)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")

# Filled Density Plot
d <- density(bank_marketing_data_full$lnage)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")


hist(bank_marketing_data_full$age,breaks = 20,col="blue",main = "distribution of age") 

hist(bank_marketing_data_full$lnage,breaks = 20,col="blue",main = "distribution of age") 

ggplot(bank_marketing_data_full, aes(x = lnage)) + geom_histogram(binwidth = 0.1, col = "white")

## Age looks like it is right skewed, might as well try transformation
hist(bank_marketing_data_full$duration,breaks = 20,col="green",main = "distribution of call-duration")



nrow(bank_marketing_data_full[bank_marketing_data_full$duration > 3600, ])
bank_marketing_data_full <- bank_marketing_data_full %>% filter(duration < 3600) 

ln_duration <- log(bank_marketing_data_full$duration)
ggplot(bank_marketing_data_full, aes(x = ln_duration)) + geom_histogram(binwidth = 0.1) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#transforming it to cuberoot makes the data into normal distribution

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



#- NORMALIZA??O
#- modelo simplificado... distribuiçao gaussiana (ou aprox. gaussiana)

#- MULTICOLINEARIDADE
#- ANOVA
#- Ver mais que fazer neste ponto
#- cor[c(1,11,15,...)] mostra as correla??es, ou ent?o com os plots / boxplot(y~varquantitativa), 
#uma vai ser var quantitativa idade e vamos ver as idades para as pessoas que fizeram ou nao emprestimo, e se ambas estiverem muito parecidas quer dizer que a idade nao influencia muito o resultado. Usar o boxplot e o p-value para verificar se bate certo.


#################Correlation Analysis#################
#It can tell if predictor is a good predictor or not a good predictor
#This analysis can help us decide if we can drop some columns/predictors 
#depending upon its correlation with the outcome variable
pairs.panels(bank_marketing_data_full[, c(11:11,21)])
str(bank_marketing_data_full)


#pairs.panels(bank_marketing_data_full[, c(8:14,21)])
#pairs.panels(bank_marketing_data_full[, c(15:21)])


####################################################################
#TRAINING DATA / TEST DATA (splitting data)
#creating train and test datasets
#analisar a proporçao de acertos. remover o y no test data.

require(caTools)  # loading caTools library
## Loading required package: caTools
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(bank_marketing_data_full,SplitRatio = 0.85) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train_data =subset(bank_marketing_data_full,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test_data=subset(bank_marketing_data_full, sample==FALSE)
dim(train_data)
dim(test_data)

###############################################################
#Logistic Regression with cross validation

model<-glm(y~.,data = train_data,family = binomial)
summary(model)


#Checking variable importance for GLM
varImp(object=model)

## Prediction for glm model
test_result<-predict(model,test_data,type = "response")
test_result <- ifelse(test_result > 0.5,1,0)

test_result<-round(test_result,0)
test_result<-as.factor(test_result)
levels(test_result)<-c("No to despoist","yes to deposit")
actual1<-test_data[,21]
levels(actual1)<-c("No to despoist","yes to deposit")

## confusion matrix for glm model
conf1<-confusionMatrix(actual1,test_result,positive = "yes to deposit")
conf1 # from the glm model we can see that accuracy is 90.6%

install.packages("ROCR")
library(ROCR)

pr <- prediction(test_result, test_data$y)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#Area under ROC curve

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#################  Logistic Regression with k-fold cross-validation
#Perform k-fold Cross Validation i.e. 10 folds to understand the average error across the 10 folds.


bank_marketing_data_full=bank_marketing_data_full[bank_marketing_data_full$loan!='unknown',]

str(bank_marketing_data_full)
# define training control
Control1<-trainControl(method = "cv",number = 10 , savePredictions = TRUE)

# train the model 
mod_fit<-train(y ~ ., data = bank_marketing_data_full, method = "glm", family="binomial",trControl = Control1,tuneLength = 5)
summary(mod_fit)


# make predictions
#, type='response'
predictions<-  predict(mod_fit, newdata=bank_marketing_data_full)

# append predictions
bank_marketing_data_full<- cbind(bank_marketing_data_full,predictions)

confusionMatrix(bank_marketing_data_full$predictions, bank_marketing_data_full$y)


################# FEATURES SIGNIFICATIVAS P-VALUE NA REGRESS?O LOGISTICA
#- vari?veis nao significativas podemos remover -> simplificacao do problema sem grande efeito no resultado final.
#- podemos tambem usar LASSO para descobrir as vars preditivas.
summary(mod_fit)



#####################Lasso Regression - Variable selection
#url : http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
#When you have multiple variables in your logistic regression model, it might be useful to find a reduced set of variables resulting to an optimal performing model (see Chapter @ref(penalized-regression)).
#Penalized logistic regression imposes a penalty to the logistic model for having too many variables. This results in shrinking the coefficients of the less contributive variables toward zero. This is also known as regularization.

#The most commonly used penalized regression include:
  
#ridge regression: variables with minor contribution have their coefficients close to zero. However, all the variables are incorporated in the model. This is useful when all variables need to be incorporated in the model according to domain knowledge.
#lasso regression: the coefficients of some less contributive variables are forced to be exactly zero. Only the most significant variables are kept in the final model.
#elastic net regression: the combination of ridge and lasso regression. It shrinks some coefficients toward zero (like ridge regression) and set some coefficients to exactly zero (like lasso regression)


require(glmnet)
library(glmnet)

# Dumy code categorical predictor variables
x = model.matrix(y~., bank_marketing_data_full)[,-1];
# Convert the outcome (class) to a numerical variable
y <- ifelse(train_data$y == "yes", 1, 0)

#We'll use the R function glmnet() [glmnet package] for computing penalized logistic regression.
# alpha: the elasticnet mixing parameter. Allowed values include:
#   "1": for lasso regression
#   "0": for ridge regression
#   a value between 0 and 1 (say 0.3) for elastic net regression.



# Find the best lambda using cross-validation
set.seed(123) 
cv.ridge <- glmnet(x, y, family = "binomial", alpha = 0, lambda = NULL)
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 0, family = "binomial",
                lambda = cv.ridge$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(y ~., test_data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
observed.classes <- test_data$y
mean(predicted.classes == observed.classes)

cv.ridge <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.ridge)


cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, cv.ridge$lambda.min)

###############################################################
#####################Ridge Regression

dim(train_data)
dim(test_data)

require(glmnet)
library(glmnet)
set.seed(123) 

# Dumy code categorical predictor variables
x = model.matrix(y~., train_data)[,-1];
# Convert the outcome (class) to a numerical variable
y <- ifelse(train_data$y == "yes", 1, 0)

#We'll use the R function glmnet() [glmnet package] for computing penalized logistic regression.
# alpha: the elasticnet mixing parameter. Allowed values include:
#   "1": for lasso regression
#   "0": for ridge regression
#   a value between 0 and 1 (say 0.3) for elastic net regression.

glmnet(x, y, family = "binomial", alpha = 0, lambda = NULL)

# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)



#######################################################################################################################




#################################### A PARTIR DAQUI ? LIXO ####################################


#Data Correlation Analysis
## Correlation of Age to Term Deposit
## Correlation of Job to Term Deposit
## Correlation of Marital to Term Deposit
## Correlation of Education to Term Deposit
## Correlation of Housing to Term Deposit
## Correlation of Loan to Term Deposit
## Correlation of Housing and Loan to Term Deposit




##################################################################
######################### END OF PROJECT #########################
##################################################################







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
  
  
https://stackoverflow.com/questions/33470373/applying-k-fold-cross-validation-model-using-caret-package


#----------------------------------------------------
#------------- duration+pdays+month+age+contact+job
Control1<-trainControl(method = "cv",number = 10 , savePredictions = TRUE)
mod_fit<-train(y ~ duration+pdays+month+age+contact+job, data = bank_marketing_data_full, method = "glm", family="binomial",trControl = Control1,tuneLength = 5)
summary(mod_fit)


pred = predict(mod_fit, newdata=bank_marketing_data_full)
#table(pred,test_data$y)
confusionMatrix(data=pred, bank_marketing_data_full$y)

# The following variables seem to be the most relevant inputs in 
# predicting the Success rate of bank direct marketing campaign
# . Duration - call duration
# . Pdays - Number of days since last contact
# . Month - month of contact
# . Age - customer age
# . Contact - cellular/ Telephone
# . Job

#----------------------------------------------------
