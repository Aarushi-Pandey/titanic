#setting and checking working directory
setwd("C:/Users/pande/Downloads/titanic")
getwd()

#reading data
train <- read.csv(file= "train.csv",header=TRUE)
test <- read.csv(file="test.csv",header=TRUE)

?rbind
#combining data
full <- rbind(train[-2],test)

#viewing data
head(full)
str(full)

#to find out number of na values
sapply(full, function(x)sum(is.na(x)))
sapply(full, function(x)sum(is.na(x)))/nrow(train)

#turning columns into factors
full$Pclass<- factor(full$Pclass)
full$SibSp<- factor(full$SibSp)
full$Parch<- factor(full$Parch)
full$Embarked<- factor(full$Embarked)

#installing and loading ggplot2
install.packages("ggplot2")
library("ggplot2")
?ggplot2
?geom_histogram

#plotting bar graphs
ggplot(full, aes(x=Pclass))+geom_bar()  #result: 3>1>2 class
ggplot(full, aes(x=Sex))+geom_bar()     #result: males almost double than females
ggplot(full, aes(x=SibSp))+geom_bar()   #2-8 levels should be combined imo
ggplot(full, aes(x=Parch))+geom_bar()   #2-8 levels should be combined imo
ggplot(full, aes(x=Embarked))+geom_bar() #S is way more than C which is double of Q. 
                                        #need to remove ""

?aes()
?geom_bar

#turning columns into factors
str(train)
train$Pclass<- factor(train$Pclass)
train$SibSp<- factor(train$SibSp)
train$Parch<- factor(train$Parch)
train$Embarked<- factor(train$Embarked)
train$Survived<- factor(train$Survived)

ggplot(train, aes(x=Survived))+geom_bar()  #more people die than survive

#more complex graphs
?geom_bar
?fill

#S- ~60% died, Q- ~60% died, C- ~40% died
ggplot(train, aes(factor(Embarked),fill=Survived))+geom_bar()

#need to change values a bit
#0- 60% died, 1-40% died, 2-50% died
ggplot(train, aes(factor(Parch),fill=Survived))+geom_bar()

#too many values in x axis- need better visibility
ggplot(train, aes(factor(Fare),fill=Survived))+geom_bar()

#need to change values a bit
#0- 70% died, 1-40% died, 2-50% died
ggplot(train, aes(factor(SibSp),fill=Survived))+geom_bar()

#female- 25% died, male- 80% died (strong correlation)
PclassVsSurvival <-ggplot(train, aes(factor(Sex),fill=Survived))+geom_bar()+
  xlab("Ticket class")+ylab("Gender"
  )+theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = 
      element_blank())+scale_fill_discrete(name="Survived",breaks=c(0,1),
      labels=c("No", "Yes"))+ggtitle("Gender vs Number of people")+
  theme(plot.title = element_text(hjust= 0.5))

#1- 40%, 2- 50%, 3-75% (correlation)
sexVsSurvival <- ggplot(train, aes(factor(Pclass),fill=Survived)
       )+geom_bar()+xlab("Ticket class")+ylab("Number of people"
      )+theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = 
      element_blank())+scale_fill_discrete(name="Survived",breaks=c(0,1),
      labels=c("No", "Yes"))+ggtitle("Ticket class vs Number of people")+
      theme(plot.title = element_text(hjust= 0.5))

?ggtitle

#combining last 2 graphs proved to be too time-consuming

#improve data given:
#1. remove "" from Embarked

#checking for na values (no na values)
sum(is.na(full$Embarked))

#checking for "" values (2 such values found)
sum(full$Embarked=="")

#since "S" has the maximum values, assigning "S" to those 2 "" values
full$Embarked[full$Embarked ==""] <-"S"

#somehow the levels changed to 1-4 so gotta fix that
#converting to numberic type
full$Embarked<- as.numeric(full$Embarked) 
#changing value of each level
full$Embarked[is.na(full$Embarked)] <-"C"
full$Embarked[full$Embarked=="1"] <-"S"
full$Embarked[full$Embarked=="4"] <-"S"
full$Embarked[full$Embarked=="3"] <-"Q"
#converting back to factor
full$Embarked<- factor(full$Embarked)

#doing the same to train
train$Embarked[train$Embarked ==""] <-"S"
train$Embarked<- as.numeric(train$Embarked)
#changing value of each level
train$Embarked[is.na(train$Embarked)] <-"C"
train$Embarked[train$Embarked=="1"] <-"S"
train$Embarked[train$Embarked=="2"] <-"C"
train$Embarked[train$Embarked=="4"] <-"S"
train$Embarked[train$Embarked=="3"] <-"Q"
#changing back to factor
train$Embarked<- factor(train$Embarked)
#plotting graph to check
ggplot(train, aes(factor(Embarked),fill=Survived))+geom_bar()

#2. combine sibsp and parch 2-8 into 2+ 

#convert back to numeric type
full$SibSp <- as.numeric(full$SibSp)

#assign value of "2" for 2 or more siblings/spouses
full$SibSp[full$SibSp >2] <-2

#convert back to factor
full$SibSp <- factor(full$SibSp)

#checking levels
levels(full$SibSp)[levels(full$SibSp)=="2"] <- "2+"

#do the same for train in order to plot the graph better
train$SibSp <- as.numeric(train$SibSp)
train$SibSp[train$SibSp >2] <-2
train$SibSp <- factor(train$SibSp)
levels(train$SibSp)[levels(train$SibSp)=="2"] <- "2+"
ggplot(train, aes(factor(SibSp),fill=Survived))+geom_bar()

#do the same for parch
train$Parch <- as.numeric(train$Parch)
train$Parch[train$Parch >2] <-2
train$Parch <- factor(train$Parch)
levels(train$Parch)[levels(train$Parch)=="2"] <- "2+"
ggplot(train, aes(factor(Parch),fill=Survived))+geom_bar()

#now for full data
full$Parch <- as.numeric(full$Parch)
full$Parch[full$Parch >2] <-2
full$Parch <- factor(full$Parch)
levels(full$Parch)[levels(full$Parch)=="2"] <- "2+"


#3. remove age since its abt 30% na values
sapply(full, function(x)sum(is.na(x)))/nrow(train)

full<- full[-5]
str(full)

#do the same for train
train<- train[-6]
str(full)

#4. find avg and substitute na values with it for fare
sapply(full, function(x)sum(is.na(x)))/nrow(full)

#only one value is NA
sum(is.na(full$Fare))

#finding avg
avg<- sum(full$Fare, na.rm = TRUE)/(nrow(full)-1)

#assigning avg to na value
full$Fare[is.na(full$Fare)]<-avg

#do the same with train
#no NA values
sum(is.na(train$Fare))

#5. make a title variable with miss, mrs etc
full$Name

#used code from: https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
?gsub

#to see how many titles there are
table(full$Title)

#reassigned some values
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title != 'Miss' & full$Title != 'Master' & full$Title != 'Mr' & full$Title != 'Mrs']<- 'Other' 

#to see how many titles there are
table(full$Title)

#do the same with train data
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)

#to see how many titles there are
table(train$Title)

#reassigned some values
train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs' 
train$Title[train$Title != 'Miss' & train$Title != 'Master' & train$Title != 'Mr' & train$Title != 'Mrs']<- 'Other' 

#to see how many titles there are
table(train$Title)
#6. remove cabin since most values are ""- useless
sum(train$Cabin =="")

#more than 50%
sum(train$Cabin =="")/nrow(full)

full<- full[-9]

str(full)

#do the same with train data
sum(train$Cabin =="")/nrow(train)

train<- train[-10]

str(train)

#7. make children, adult, elderly column(?)from age variable
#cant do since age had alotta na values so i removed it

#8. might be able to find relationship between pclass and fare

#definite relationship- pay more for higher class
ggplot(full, aes(x=full$Pclass, y=full$Fare))+geom_boxplot()

#time to make models

#install required package
install.packages("randomForest")
library("randomForest")

#setting seed
set.seed(4)

#making first model
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare+ Embarked+ Title, data= train)
?randomForest

#accidentally made 4 plots in 1 page (idk how)
plot(rf_model)
?plot

#apparently can find out importance of variables too
#will do this later
importance    <- importance(rf_model)

#last step-predict
test <- full[892:1309,]
str(test)
str(full)
str(train)

prediction <- predict(rf_model, test)

#make csv file with prediction
solution1 <- data.frame(test$PassengerId, prediction)
str(solution1)
?data.frame

colnames(solution1)

#renaming columns
names(solution1)[names(solution1) == "test.PassengerId"] <- "PassengerId"
names(solution1)[names(solution1) == "prediction"] <- "Survived"

#creating solution file
write.csv(solution1, file="solution1.csv", row.names = F)

#got 0.76076 (score)

#use different predicting models
#1. make sex into factor

#installing package to combine test and train data
install.packages("dplyr")
library("dplyr")

#binding train and test into full
full <- bind_rows(train,test)
?bind_rows

#making it into factor
full$Sex <- factor(full$Sex)
str(full)

#making title into a factor too
table(full$Title)
full$Title <- factor(full$Title)
str(full)

#splitting into train and test
train <- full[1:891,]
test <- full[892:1309,]

#using rf_model to predict again
#creating a model using random forest again
rf_model2 <- randomForest(factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare+ Embarked+ Title, data= train)

#plotting model (which is too small)
plot(rf_model)

#predicting 2nd time
prediction2 <- predict(rf_model2, test)
solution2 <- data.frame(test$PassengerId, prediction2)

colnames(solution2)

#renaming columns
names(solution2)[names(solution2) == "test.PassengerId"] <- "PassengerId"
names(solution2)[names(solution2) == "prediction2"] <- "Survived"

#creating solution file
write.csv(solution2, file="solution2.csv", row.names = F)

#score is 0.75837 this time

#gonna try to experiment with variables a bit
#creating a model using random forest again
rf_model3 <- randomForest(factor(Survived) ~ Pclass + Sex + Fare+ Embarked+ Title, data= train)

#plotting model (which is too small)
plot(rf_model3)

#predicting 2nd time
prediction3 <- predict(rf_model3, test)
solution3 <- data.frame(test$PassengerId, prediction3)

colnames(solution3)

#renaming columns
names(solution3)[names(solution3) == "test.PassengerId"] <- "PassengerId"
names(solution3)[names(solution3) == "prediction3"] <- "Survived"

#creating solution file
write.csv(solution3, file="solution3.csv", row.names = F)

#received better score- 0.78229
#can mess around with factors more later

#2. lazy prediction model
#idk what i can do so skip for now- shouldve done in the beginning

#3. logit prediction model
#i.e. logistic regression model

#very good summary
summary(full)

#making linear model
?glm

mylogit<- glm(Survived ~ Pclass + Title, data=train, family= "binomial")
summary(mylogit)

#logically im supposed to remove a few variables nd then check (after submitting)
#will do so later

#making a prediction and displaying it
train.probs <- predict(mylogit, data=train,type =  "response")
table(train$Survived,train.probs>0.5)

#81% accurate with train data
(478+249)/(478+249+71+93)

#predicting with test data
test.probs <- predict(mylogit, newdata=test,type =  "response")

#idk what this is supposed to do since we dont know survived
table(test$Survived,test.probs>0.5)

#rounding values
test.probs[test.probs <= 0.5] <-0
test.probs[test.probs > 0.5] <-1

#making a submission
solution4 <- data.frame(test$PassengerId, test.probs)

colnames(solution4)

#renaming columns
names(solution4)[names(solution4) == "test.PassengerId"] <- "PassengerId"
names(solution4)[names(solution4) == "test.probs"] <- "Survived"

#creating solution file
write.csv(solution4, file="solution4.csv", row.names = F)

#pretty low score- 0.71291
#i'll remove some factors and try again- got 0.73444, 0.77511, 0.77511 (again), 0.77272
#looks like i cant exceed that value

#will try again
#making model
?lm
mylogit2<- lm(Survived ~ as.numeric(Pclass)+as.numeric(Sex)+as.numeric(SibSp)+Fare+as.numeric(Embarked)+as.numeric(Title), data=train)
#i keep getting warning messages
summary(mylogit2)

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")

install.packages("installr") 
library(installr)
updateR()

#installing required packages
install.packages("olsrr",type="source")
#package not opening coz of error
library("olsrr")
?library
getwd()

#taking awhile so chose variables on own
#i keep getting warning messages
ols_step_forward_p(mylogit2)
?ols_step_forward_p
#4. dtree prediction model

set.seed(12)
#making model from decision tree
#install and load required packages first
install.packages("rpart")
library("rpart")

?rpart
#creating model
dt_model <- rpart(Survived~ Pclass + Sex + SibSp + Parch + Fare+ Embarked+ Title, data=train, method="class")

#packages to plot model
install.packages("rpart.plot")
library("rpart.plot")

?rpart.plot

#plotting model
rpart.plot(dt_model, extra=3, fallen.leaves = T)
#title, pclass, fare, embarked r imp

#package for confusion matrix
install.packages("caret")
#for some reason it is not loading
library("caret")

#predicting train data to see accuracy
pre_tdt <- predict(dt_model, data=train, type="class")

?confusionMatrix

#confusion matrix with accuracy of 0.8384%
confusionMatrix(pre_tdt, train$Survived)

#i dont really know the importance of this
##There is chance of overfitting in Single tree, So I will go for cross validation using '10 fold techinque'
set.seed(12)
cv.10 <- createMultiFolds(train$Survived, k = 10, times = 10)
?createMultiFolds

# Control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)

##Train the data
Model_CDT <- train(x = train_val[,-7], y = train_val[,7], method = "rpart", tuneLength = 30,
                   trControl = ctrl)
?train

#need to understand above steps more
#idk what im supposed to do now...

#making another model
rf1<- randomForest(Survived~ Pclass + Sex + Fare+ Embarked+ Title, data=train, ntree=100, importance= T)
rf1

#more accurate without SibSp + Parch
varImpPlot(rf1)

#ig i have to predict with random forest but might have to do other steps first
plot(rf1)

#predicting 2nd time
prediction4 <- predict(rf1, test)
solution4 <- data.frame(test$PassengerId, prediction4)

colnames(solution4)

#renaming columns
names(solution4)[names(solution4) == "test.PassengerId"] <- "PassengerId"
names(solution4)[names(solution4) == "prediction4"] <- "Survived"

#creating solution file
write.csv(solution4, file="solution5.csv", row.names = F)

#score= 0.77033
#changed no. of trees from 1000 to 100 and score=0.78229
#out of submissions too
#5. featured ci/first forest model
#pretty sure i did this too
#6. search kaggle - did a bit

#might need to do more feature engineering
#1. trying svm model
#installing required packages
install.packages("e1071")
library("e1071")

#making an svm model
set.seed(123)
caret_svm <- svm(Survived ~ Pclass + Sex + Fare+ Embarked+ Title, data = train, scale = FALSE, kernel = "radial", cost = 5)
summary(caret_svm)

#plotting model
#idk how to do this yet
plot.svm(caret_svm)

#making prediction with test data
#predicting for train data instead of test data for some reason
#will try again later
?predict
train_pre<- predict(caret_svm, data=test, type="response")
train_pre

#86.86% accuracy with train data (without scale, kernel and cost it drops to 81.59%)
mean(train_pre==train$Survived)

#values of train being predicted only
?predict
prediction6<- predict(caret_svm, newdata=test[-2], type="response")
prediction6

#no need for this rounding off
prediction6[prediction6 <= 0.5] <-0
prediction6[prediction6 > 0.5] <-1

#making a submission
solution6 <- data.frame(test$PassengerId, prediction6)

colnames(solution6)

#renaming columns
names(solution6)[names(solution6) == "test.PassengerId"] <- "PassengerId"
names(solution6)[names(solution6) == "prediction6"] <- "Survived"

#creating solution file
write.csv(solution6, file="solution6.csv", row.names = F)

#got lower score than expected-0.75358

#trying caret package
install.packages("caret")
library("caret")
#getting same problem as before in loading the library

#look at more solutions to titanic challenge to get inspo
#1. relearn logit(done),linear regression, rforest, svm
#i. practicing logit (done above)
#2. find out how to display confusion matrix- done

#understanding functions
?read.csv
?gsub
?paste
?rep
?rpart.plot
