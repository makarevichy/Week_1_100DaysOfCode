#BASELINE. I don't join train and test files, though that would be logical. Now we go the hard way :)

library(stringr)
library(tidyverse)
library(car)
library(mice)
library(caret)
library(doBy)
# load train dataset. You can download the training file on https://www.kaggle.com/c/titanic/data
kaggle <- read.csv(paste0(getwd(),'/Day_3/train.csv'), header = T, sep = ',')
kaggle$Title <-  kaggle$Name %>% 
  str_extract(., "\\w+\\.") %>% 
  str_sub(.,1, -2)
#function for recode titles
change.titles <- function(data, old_title, new_title) {
  for (title in old_title) {
    data$Title[data$Title == title] <- new_title
  }
  return (data$Title)
}
kaggle$Title <- change.titles(kaggle, 
                              c("Capt", "Col", "Don", "Dr", 
                                "Jonkheer", "Lady", "Major", 
                                "Rev", "Sir", "Countess"),
                              "Aristocratic")
kaggle$Title <- change.titles(kaggle, c("Ms"), 
                              "Mrs")
kaggle$Title <- change.titles(kaggle, c("Mlle", "Mme"), "Miss")
kaggle$Title <- as.factor(kaggle$Title)
kaggle[c(62, 830), 'Embarked'] <- 'C'
kaggle$Embarked <- factor(kaggle$Embarked, c('C', 'Q', 'S'))
kaggle$Survived <- as.factor(kaggle$Survived)
kaggle$Pclass <- as.factor(kaggle$Pclass)
kaggle$Fare[kaggle$Fare == 0] <- NA
tab <- doBy::summaryBy(Fare ~ Embarked+Pclass, data = kaggle, FUN = median, na.rm = T)
kaggle$Fare[kaggle$Embarked =='S' & kaggle$Pclass == '1' & is.na(kaggle$Fare)] <- tab$Fare.median[7]
kaggle$Fare[kaggle$Embarked =='S' & kaggle$Pclass == '2' & is.na(kaggle$Fare)] <- tab$Fare.median[8]
kaggle$Fare[kaggle$Embarked =='S' & kaggle$Pclass == '3' & is.na(kaggle$Fare)] <- tab$Fare.median[9]
kaggle$Ticket <- str_extract_all(kaggle$Ticket, '([^ ]\\d+\\d$)', simplify = T) %>% 
  str_extract_all('^.', simplify = T)
kaggle$Ticket <- car::recode(kaggle$Ticket, "'' = NA")
kaggle[is.na(kaggle$Ticket), "Ticket"] <- kaggle[is.na(kaggle$Ticket), "Pclass"]
nam <- names(kaggle)[c(1, 4, 11)]
#imputation missing
imp <- mice(data = kaggle[,!names(kaggle) %in% nam], seed = 42)
# take 2 column for imput
imp_kaggle <- complete(imp, 4)
kaggle$Age <- imp_kaggle$Age
kaggle <- kaggle[,!names(kaggle) %in% nam]
TransPred <- c('Age', 'Fare')
prevar <- preProcess(kaggle[,TransPred])
TransVar <- predict(prevar, kaggle[,TransPred])
kaggle[,TransPred] <- TransVar
matrix_kaggle <- model.matrix(Survived~., data = kaggle[,-c(4:8)])
kaggle1 <- data.frame(Survived = kaggle$Survived, matrix_kaggle[,-1],
                      Age = kaggle$Age,
                      Fare = kaggle$Fare,
                      Ticket = kaggle$Ticket,
                      SibSp = kaggle$SibSp,
                      Parch = kaggle$Parch)
kaggle1 <- kaggle1 %>%
  mutate(famil = SibSp + Parch)
#we will not split the table on train, test
kaggle_train <- kaggle1
control<-trainControl(method="repeatedcv",number=10,repeats=3)
set.seed(7)
fit.svL <- train(Survived~.,data = kaggle_train, method="svmLinear", trControl=control)
fit.svR <- train(Survived~.,data = kaggle_train, method="svmRadial", trControl=control,
                 tuneGrid = expand.grid(sigma = seq(0.075, 0.10, 0.005), C = seq(0.3, 0.7, 0.1)))
fit.rf <- train(Survived~.,data = kaggle_train, method="rf", trControl=control)
fit.gbm <- train(Survived~.,data = kaggle_train, method="gbm", trControl=control)
caret.models <-  list(SVML = fit.svL,
                      SVMR = fit.svR,
                      RF = fit.rf,
                      GBM = fit.gbm)
#result models on test. Best results svmlinear, svmRadial.
summary(resamples(caret.models), metric = "Accuracy")

#load test dataset. You can download the testing file on https://www.kaggle.com/c/titanic/data
kaggle_work <- read.csv(paste0(getwd(),'/Day_3/test.csv'), header = T, sep = ',')
kaggle_work$Fare[kaggle_work$Fare == 0] <- NA
kaggle_work$Title <-  kaggle_work$Name %>% 
  str_extract(., "\\w+\\.") %>% 
  str_sub(.,1, -2)
kaggle_work$Title <- change.titles(kaggle_work, 
                                   c("Col", "Dona", "Dr","Rev"),
                                   "Aristocratic")
kaggle_work$Title <- change.titles(kaggle_work, c("Ms"), 
                                   "Mrs")
kaggle_work$Title <- as.factor(kaggle_work$Title)
kaggle_work$Pclass <- as.factor(kaggle_work$Pclass)
#delete variables
kaggle_work <- kaggle_work[,!names(kaggle_work) %in% nam]
tab1 <- summaryBy(Fare ~ Embarked+Pclass, data = kaggle_work, FUN = median, na.rm = T)
kaggle_work$Fare[kaggle_work$Embarked =='S' & kaggle_work$Pclass == '1' & is.na(kaggle_work$Fare)] <- tab1$Fare.median[7]
kaggle_work$Fare[kaggle_work$Embarked =='S' & kaggle_work$Pclass == '3' & is.na(kaggle_work$Fare)] <- tab1$Fare.median[9]
kaggle_work$Ticket <- str_extract_all(kaggle_work$Ticket, '([^ ]\\d+\\d$)', simplify = T) %>% 
  str_extract_all('^.', simplify = T)
kaggle_work$Ticket <- car::recode(kaggle_work$Ticket, "'' = NA")
kaggle_work[is.na(kaggle_work$Ticket), "Ticket"] <- kaggle_work[is.na(kaggle_work$Ticket), "Pclass"]
imp1 <- mice(data = kaggle_work, seed = 42)
#take 1 column
imp1_kaggle <- complete(imp1, 5)
kaggle_work$Age <- imp1_kaggle$Age
prevar1 <- preProcess(kaggle_work[,TransPred])
TransVar1 <- predict(prevar1, kaggle_work[,TransPred])
kaggle_work[,TransPred] <- TransVar1
kaggle_work <- data.frame(Survived = rep(1, nrow(kaggle_work)), kaggle_work)
matrix_kaggle1 <- model.matrix(Survived~., data = kaggle_work[,-c(4:8)])
kaggle2 <- data.frame(matrix_kaggle1[,-1],
                      Age = kaggle_work$Age, 
                      Fare = kaggle_work$Fare,
                      Ticket = kaggle_work$Ticket,
                      SibSp = kaggle_work$SibSp,
                      Parch = kaggle_work$Parch)
kaggle2 <- kaggle2 %>% 
  mutate(famil = SibSp + Parch)
pred.svL <- predict(fit.svL, kaggle2)
pred.svR <- predict(fit.svR, kaggle2)
pred.rf <- predict(fit.rf, kaggle2)
pred.gbm <- predict(fit.gbm, kaggle2)
submission_svL <- data.frame(PassengerId = seq(892, 1309, 1), Survived = pred.svL)
submission_svR <- data.frame(PassengerId = seq(892, 1309, 1), Survived = pred.svR)
submission_rf <- data.frame(PassengerId = seq(892, 1309, 1), Survived = pred.rf)
submission_gbm <- data.frame(PassengerId = seq(892, 1309, 1), Survived = pred.gbm)
write.table(submission_svL, file = 'Day_3/gender_submission_svL.csv', append = F, quote = F, sep = ',', row.names = F, col.names = T)
write.table(submission_svR, file = 'Day_3/gender_submission_svR.csv', append = F, quote = F, sep = ',', row.names = F, col.names = T)
write.table(submission_rf, file = 'Day_3/gender_submission_rf.csv', append = F, quote = F, sep = ',', row.names = F, col.names = T)
write.table(submission_gbm, file = 'Day_3/gender_submission_gbm.csv', append = F, quote = F, sep = ',', row.names = F, col.names = T)
