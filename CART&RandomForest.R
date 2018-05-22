str(stevens) 

# Docket is unique identifier, # then 6 ind vars excluding year.

# reverse: 1 - reverse, 0 - affirm

library(caTools)
set.seed(3000)

# 70 % in training set
spl = sample.split(stevens$Reverse, SplitRatio=.70)

Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# -- CART MODEL using rpart -- 
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method='class', minbucket=25)
prp(StevensTree)

PredictCart = predict(StevensTree, newdata=Test, type='class')
table(Test$Reverse,PredictCart)

#accuracy
(41+71)/(41+36+22+71) # 66% 

# Our Cart model beats baseline model.

# EVALUATE MODEL -- ROC CURVE
library(ROCR)
# without type class
PredictROC = predict(StevensTree, newdata=Test)
# probabilities.

# 1st Arg- 2nd column of predict ROC, 2nd Arg- true outcome values
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf,colorize=TRUE)



# RANDOM FOREST MODEL 
install.packages("randomForest")
library(randomForest)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, nodesize=25, ntree=200)
# Train$Reverse = as.factor(Train$Reverse)
#> Test$Reverse = as.factor(Test$Reverse)
PredictForest = predict(StevensForest, newdata=Test)

table(Test$Reverse, PredictForest)
# acc = 67%. 

# Cross - Validation 
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds = trainControl(method='cv',number=10)
cpGrid= expand.grid(.cp=seq(0.01,.5,.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method='rpart',trControl=numFolds, tuneGrid=cpGrid)

# new CART model, changing minbucket parameter

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data=Train, method='class',cp=0.18)
PredictCV = predict(StevensTreeCV, newdata=Test, type='class')
table(Test$Reverse,PredictCV)
# accuracy  59 + 64 / total 
# 72.4% level of accuracy