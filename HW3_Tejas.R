#Loading the packages
library(dplyr)
library(ggplot2)
library(caTools) # splits
library(GGally)
library(MASS)
library(rpart) # CART
library(rpart.plot) # CART plotting
library(caret) # cross validation
#install.packages(c("randomForest", "gbm"))
library(randomForest)
library(gbm)
library(ROCR)

#Importing the dataset
ldata <- read.csv("Letters242.csv")

#Question 3: Part a
#Exploratory data analysis
library(reshape2)
expl.data <- melt(ldata)

ggplot(ldata, aes(x = factor(ldata$letter), y = onpix)) + geom_boxplot(aes(fill = ldata$letter))

#Question 3: Part b
#Making predictions on B only
ldata$isB <- as.factor(ldata$letter == "B")

#Splitting dataset into train and test sets
set.seed(623)
train.ids = sample(nrow(ldata), 0.65*nrow(ldata))
ldata.train = ldata[train.ids,]
ldata.test = ldata[-train.ids,]

#Baseline model accuracy
#Baseline model always predicts that letter is 'not B'
table(ldata.train$isB)
table(ldata.test$letter)
table(ldata.test$isB)
print(paste("Accuracy of Baseline Model = ", 827/(827+264)))

#using logistic regression to predict "B"

mod.log = glm(isB ~ . - letter, data=ldata.train, family="binomial")
summary(mod.log)
pred.log <- predict(mod.log, newdata = ldata.test, type = 'response')
summary(pred.log)
cm.log <- table(ldata.test$isB, pred.log > 0.5)
print(paste("Accuracy of logistic regression: ", sum(diag(cm.log))/sum(cm.log)))

#ROC and AUC for logistic model
rocr.log.pred <- prediction(pred.log, ldata.test$isB)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)

#Training CART using cross validation
set.seed(623)
cpValues = data.frame(cp = seq(0, .04, by=.002))
mod.cart <- train(isB ~. -letter,
                  data = ldata.train,
                  method = "rpart",
                  tuneGrid = cpValues,
                  trControl = trainControl(method = "cv", number=10),
                  metric = "Accuracy")

summ <- summary(mod.cart$results$Accuracy)
mod.cart$results$cp[mod.cart$results$Accuracy == summ[6]]

#Finding best model and making predictions
mod.cart$bestTune
mod.cart.final <- mod.cart$finalModel
prp(mod.cart.final, digits=3)

pred.cart = predict(mod.cart.final, newdata=ldata.test, type="class")
cm.cart <- table(ldata.test$isB, pred.cart)
cm.cart
print(paste("Accuracy of CART: ", sum(diag(cm.cart))/sum(cm.cart)))

#Using Random forests for predictions
set.seed(623)
mod.rf <- randomForest(isB ~. -letter, data = ldata.train)

pred.rf <- predict(mod.rf, newdata = ldata.test) 

importance(mod.rf)

cm.rf <- table(ldata.test$isB, pred.rf)
cm.rf
print(paste("Accuracy of Random Forest: ", sum(diag(cm.rf))/sum(cm.rf)))


#Question 3: Part c
#Baseline model that always predicts the most frequent letter in dataset
table(ldata.train$letter)
#We see that letter 'P' is most frequent, so the basline model will always predict 'P'.
table(ldata.test$letter)
print(paste("Accuracy of Baseline Model: ", 258/nrow(ldata.test)))

#LDA model and its accuracy
set.seed(623)
mod.lda <- lda(letter ~. -isB, data=ldata.train)

pred.lda <- predict(mod.lda, newdata=ldata.test)
pred.lda.class <- pred.lda$class

cm.lda <- table(ldata.test$letter, pred.lda.class)
print(paste("Accuracy of LDA: ", sum(diag(cm.lda))/sum(cm.lda)))

#CART Model with cross validation
set.seed(623)
cpVal <- data.frame(cp=seq(0, 0.1, 0.0001))
mod.cart2 <- train(letter ~ . - isB,
                     data = ldata.train,
                     method = "rpart",
                     tuneGrid = cpVal,
                     trControl = trainControl(method = "cv", number=10),
                     metric = "Accuracy")

#Choosing best cp value
#mod.cart2$results
#summ <- summary(mod.cart2$results$Accuracy)
#mod.cart2$results$cp[mod.cart2$results$Accuracy == summ[6]]
mod.cart$bestTune
mod.cart2.final <- mod.cart2$finalModel

prp(mod.cart2.final, digits=3)

pred.cart2 = predict(mod.cart2.final, newdata=ldata.test, type="class")
cm.cart2 <- table(ldata.test$letter, pred.cart2)
cm.cart2
print(paste("Accuracy of CART: ", sum(diag(cm.cart2))/sum(cm.cart2)))

#Random Forest model

set.seed(623)
mod.rf2 <- randomForest(letter ~. -isB, data = ldata.train)

pred.rf2 <- predict(mod.rf2, newdata = ldata.test) 

#importance(mod.rf2)

cm.rf2 <- table(ldata.test$letter, pred.rf2)
cm.rf2
print(paste("Accuracy of Random Forest: ", sum(diag(cm.rf2))/sum(cm.rf2)))

#Random Forest with cross validation
set.seed(623)
mod.rf2.cv <- train(letter ~. -isB,
                   data = ldata.train,
                   method = "rf",
                   tuneGrid = data.frame(mtry=1:16),
                   trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                   metric = "Accuracy")

mod.rf2.cv$results
mod.rf2.cv$bestTune
#mod.rf2.cv
mod.rf2.cv.final <- mod.rf2.cv$finalModel
mod.rf2.cv.final$mtry
pred.rf2.cv <- predict(mod.rf2.cv.final, newdata = ldata.test)
cm.rf2.cv <- table(ldata.test$letter, pred.rf2.cv)
cm.rf2.cv
print(paste("Accuracy of Random Forest with CV: ", sum(diag(cm.rf2.cv))/sum(cm.rf2.cv)))

#Using boosting for predictions
set.seed(623)
mod.gbm <- gbm(letter ~. -isB,
                 data = ldata.train,
                 distribution = "multinomial",
                 n.trees = 22400,
                 interaction.depth = 10)

pred.gbm <- predict(mod.gbm, newdata = ldata.test, n.trees=22400, type = 'response')
#summary(mod.gbm)

pred.gbm.fact = apply(pred.gbm, 1, which.max)
pred.gbm.fact = factor(pred.gbm.fact, levels = c(1,2,3,4), labels = c("A", "B", "P", "R"))

cm.gbm <- table(ldata.test$letter, pred.gbm.fact)
cm.gbm
print(paste("Accuracy of Boosting: ", sum(diag(cm.gbm))/sum(cm.gbm)))
