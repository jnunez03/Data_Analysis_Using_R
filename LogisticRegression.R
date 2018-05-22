str(quality)

# To get a value counts of poor care 
table(quality$PoorCare) # poorcare = 1, goodcare=0

# standard baseline method - predict the most frequent outcome for all observations
# we would take 98/131 = 75% accuracy

# we want to beat this with logistic regression 

# We have to split into training-testing set
# We need to add a package
install.packages("caTools")
library(caTools)


set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio= .75)
# take a look at split
split 
# True means train set, false means test!

qualityTrain = subset(quality, split==TRUE)
qualityTest = subset(quality, split == FALSE)

# Check how many observations are inside
nrow(qualityTrain) # 99 
nrow(qualityTest) # 32

# - -  Model Building -  - 

qualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(qualityLog)
# We see that coefficients are positive  - Officevisits and narcotics lead to poorcare.
# 

# check AIC. minimum AIC is best
# Prediction on training

predictTrain = predict(qualityLog, type="response")
summary(predictTrain)  # probabilities between 0 and 1.

tapply(predictTrain, qualityTrain$PoorCare, mean) # avg prediction for each of the true outcomes.
# poor care cases probability = .44, true good care cases, we predict avg probability of .19.


     # Assessing Accuracy - Thresholding 

# first arg - what we want to label the rows by. 2nd arg- label columns by
table(qualityTrain$PoorCare,predictTrain > 0.5)
#    FALSE TRUE
# 0    70    4
# 1    15   10

#  4 + 15 mistakes 
# sensitivity
10/25 
# specificity
70/74

# increase threshold
table(qualityTrain$PoorCare,predictTrain > 0.7)
#   FALSE TRUE
# 0    73    1
# 1    17    8


#       -  -   HOW TO SELECT THRESHOLD: ROC CURVE  - -
install.packages("ROCR")
library(ROCR)

# Training set predictions - predictTrain
# 1st - prediction we made with model
# 2nd - true outcomes qualityTrain$PoorCare
ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
# add color
plot(ROCRperf, colorize=TRUE)
# add values
plot(ROCRperf, colorize=TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

#     -- How to assess the strength of our model --

predictTest = predict(qualityLog, type="response", newdata=qualityTest)
summary(predictTest)

table(qualityTest$PoorCare,predictTest > 0.3) # our confusion matrix

#     FALSE TRUE
# 0    19    5
# 1     2    6

# overall 32 cases.
# correct 19 + 5 times. overall accuracy 19 + 5 / 32.
# false positive rate 5 / 24,