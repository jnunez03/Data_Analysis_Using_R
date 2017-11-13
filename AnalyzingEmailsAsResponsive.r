
emails = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/0aa8d267819eed19111e3d9e06811745/asset-v1:MITx+15.071x+2T2017+type@asset+block/energy_bids.csv",stringsAsFactors = FALSE)
str(emails)
# 855 observations 2 variables

emails$email[1]
# break it down
strwrap(emails$email[1])

# doesnt have to do with energy schedules or bids, so it is not responsive to our query.
emails$responsive[1] # 0
strwrap(emails$email[2]) #most of it is forwarded. # this is responsive to our query. check...
emails$responsive[2] # 1 . so it is.

table(emails$responsive)  #small amount that are responsive only 139. 

# Pre Processing..

library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]]) # same email from before..    

#convert to lowercase, delete punctuation & stopwords, stem the document
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

# now its ready for ML models. 

#BAG of Words.
#document term matrix
dtm = DocumentTermMatrix(corpus)
dtm # a lot of terms ,, 22,000!

dtm = removeSparseTerms(dtm, 0.97)
dtm # 788 terms

labeledTerms = as.data.frame(as.matrix(dtm)) # frequencies of the words.
# add in outcome variable, whether or not email was responsive.

labeledTerms$responsive = emails$responsive
str(labeledTerms)  # Lots of variables , 789 variables, the last 1 is outcome variable. 788 indep variables. 

# models. split data...

library(caTools)
set.seed(144)

spl = sample.split(labeledTerms$responsive, .7)
train = subset(labeledTerms, spl ==TRUE)
test1 = subset(labeledTerms, spl == FALSE)
library(rpart)
library(rpart.plot)

emailCART = rpart(responsive~., data=train, method="class")
prp(emailCART)
# IF california appears at least twice in an email, were going to take the right part and predict it as responsive!
#Jeff is enron's CEO.. who was actually jailed for fraud in company. 

# EVALUATION on test set.
pred = predict(emailCART, newdata=test1)
pred[1:10,] # we want all columns, thats why we have comma. 
# left column is predicted probability of doc being non-responsive, right is responsive probability.
# we want to extract the predicted probability of the document being responsive. so rightmost column.
# create object called pred.prob and select 2nd column

pred.prob = pred[,2] # test set predicted probabilities.
# we interested in the accuracy of our model on test set.
table(test1$responsive, pred.prob>=.5)

#accuracy 195+25/total ... =.856

# compare to baseline model, always going to predict non-responsive... 
table(test1$responsive)  #83.7% accuracy. 

# assign higher cost to false negatives then to false positives, because actual people can manually review. 

# ROC CURVE. .

#understand performance at different cutoffs. 
install.packages("ROCR",dep=T)
library(ROCR)
#1st arg is prob of responsive, 2nd arg is true outcomes of responsive
predROCR = prediction(pred.prob,test1$responsive)
perfROCR = performance(predROCR, "tpr", "fpr") # extract true positive rate and false pos rate
plot(perfROCR, colorize=TRUE)

## . .   Best cutoff is dependent on cost to false pos, and true pos ,
# we favor high sensitivity, right at the blue , 70% true pos, false pos about 20%, accidents 20% of non responsive docs,
# vast majority are non repsonsive, so result in decrease in manual effort for detecting.
# blue color on y axis, threshold at about .15. 

# USE ROCR package to comput auc value,

performance(predROCR, "auc")@y.values
# means, our model can differetiate between a randomly selected responsive and non responsive document 80% of the time. 
