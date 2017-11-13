# Turning Tweets Into Knowledge 

""" 
TEXT ANALYTICS 
"""

# read in data.
tweets = read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/15d60df8e1e31cd062da4535a753eaf9/asset-v1:MITx+15.071x+2T2017+type@asset+block/tweets.csv",stringsAsFactors = FALSE)

# 1181 observations of 2 variables. Tweet & Avg sentiment score.
str(tweets)  

# New variable for negative tweets with clear negative sentiments.

#True if score is less then or equal to -1. False if it is greater than -1. 
tweets$Negative = as.factor(tweets$Avg <= -1)
# Lets take a look at this variable using table
table(tweets$Negative)
# About 15% are negative tweets. 

# Pre=process data to use bag of words technique, tm- text mining package
install.packages("tm")

library(tm)

install.packages("SnowballC")
# Loads package
library(SnowballC)
# collection of documents = corpus

corpus = Corpus(VectorSource(tweets$Tweet))
corpus # 1181 docs.
corpus[[1]] # first tweet.

# Change all text in tweets to lowercase.

corpus = tm_map(corpus, tolower)
corpus[[1]] # To show its lowercase.

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
summary(corpus)

# Remove stop words in our tweets. 
stopwords("english")[1:10]

# Which stopwords do we want to remove, as well as apple. 
corpus = tm_map(corpus, removeWords,c("apple", stopwords("english")))
corpus[[1]]  # less characters after removing all stopwords and punctuation.... 

# stem document. # takes off endings of words. 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]  # even less characters now. 

## Document Term Matrix.  
"""
Generates a matrix where the rows correspond to documents(tweets) & columns correspond to words in those tweets.
Values in matrix are # of times that word appears in each document. Generate it and call it frequencies.
"""

frequencies = DocumentTermMatrix(corpus)
frequencies   #1181 tweets , terms = 3289.  Inspect it.
inspect(frequencies[1000:1005, 505:515]) # sparse = many zeros..

findFreqTerms(frequencies, lowfreq=20)   #low freq = to min # of times a term must appear to be displayed..
# A lot of terms that will be useless.  
#More terms = more independent variables.. ratio to indep variables to observations will affect
# the model on how good it generalizes.. remove these terms

# sparsity threshold,If we say .995,  that means keep trms that appear in .5% or more of the tweets. About 6 or more tweets.
sparse = removeSparseTerms(frequencies,.995)
sparse
# 309 terms in our sparse matrix
# convert into data frame to be used in predictive model.

tweetsSparse = as.data.frame(as.matrix(sparse))
# This converts our variable names to make sure they are appropriate.
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
# lets add our dependent variable to this data set.
tweetsSparse$Negative = tweets$Negative # Original negative variable from the tweets data frame.

# We will split our data, putting 70% of data into training set. 
# We will need library caTools so that we can use sample.split function. seed(123)
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
#Training set 
trainSparse = subset(tweetsSparse, split==TRUE) # take observations for which split == true
testSparse = subset(tweetsSparse, split==FALSE)

# Data now ready for prediction model. CART and LogReg to predict negative sentiment. 
# install packages

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~., data=trainSparse, method="class")
prp(tweetCART)  # plot it

# if freak is in tweet, predict negative sentiment (TRUE)
#IF freak is not in the tweet, but hate is, predict true.
# IF both aren't in tweet, but wtf is, then predict True or Negative Sentiment
# IF none, predict false, or not negative sentiment,

# Evaluate Numerical performance by using test set.
predictCART = predict(tweetCART, newdata=testSparse, type="class") # class for class predictions
# Confusion Matrix.
table(testSparse$Negative, predictCART)
#Accuracy is  add diagonals / total # of obs. 
(294+18)/(294+6+37+18) # 88% 

# Let's compare this to a simpe baseline model that always predicts non-negative,
# Accuracy of baseline model, make table of just the outcome variable Negative.
table(testSparse$Negative)
# 300 with nonneg, 55 with negative.  Accuracy is 300/355.  = .845, Our CART Model does better.

# Let's try random forest     ## Takes longer due to many variables.
library(randomForest)
set.seed(123)
# ~. means use all variables
tweetRF = randomForest(Negative ~., data=trainSparse)

#
predictRF = predict(tweetRF, newdata=testSparse)

# confusion matrix. # give it actual outcomes, then our predictions.
table(testSparse$Negative, predictRF)
(293+21)/(293+21+7+34)  # accuracy .885 

# However CART is more interpretable
# If we were to use cross-validation to pick the cp parameter for the CART model, the accuracy would increase to about the same
# as random forest.
# Using bag of words approach and these models, we can reasonably predict sentiment even with small data set. 
