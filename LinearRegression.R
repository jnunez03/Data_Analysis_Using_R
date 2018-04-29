# Load Data
wine <- read_csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/834e8a91ad31bfcdf317bf6356b808f2/asset-v1:MITx+15.071x+2T2017+type@asset+block/wine.csv")
wine_test <- read_csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/11433f68d6c74e205f3dfa73dc4711c3/asset-v1:MITx+15.071x+2T2017+type@asset+block/wine_test.csv")
View(wine_test)
str(wine) # details of variables
summary(wine)

# regression model with 1 predictor

model1 = lm(formula = Price ~ AGST, data=wine)
summary(model1)

# Sum Square Errors. 
model1$residuals

SSE = sum(model1$residuals^2)
SSE

Model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(Model2)
# Multiple R^2 and adjusted R^2 increased.
SSE = sum(Model2$residuals^2)
SSE

Model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop,data=wine)
summary(Model3)
SSE = sum(Model3$residuals^2)
SSE #decreased

# If a coefficient is not significantly different from 0, then we should
# remove the variable from our model.

# Standard error column gives a measure of how much the coefficient is likely to 
# vary from the estimate value.

# t-value is the estimate divided by the standard error.
# The larger the t-value is the more likely it is to be significant. 

# last column; less value is the less likely that our coefficient is 0.
# IF abs of T is small, it will be large. 

# The Stars show significance. 3 stars is highest level of significance.
# p-value less than .001.
# 
# Make a new model taking out FrancePop. Before we saw it wasn't significant. 
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data =wine)
summary(model4)

# All of a sudden, Age is now significant. Before it wasn't. This is
# Multi-collinearity. Adjusted-R^2 increased. 
# Age and France-Pop were highly-correlated.

cor(wine$WinterRain, wine$Price) # correlation
cor(wine$Age, wine$FrancePop) # highly correlated.

cor(wine) #similar to a heatmap.

# lets see what would happen if we took Age and FrancePop out at the same time.

model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
# Rsquared dropped. Model with Age has higher Rsquared. 
# Age makes more intuitive sense since the age of wine makes it more expensive. 

# Typically a correlation greater than .7 or less then -.7 is cause for concern. 

# ---

#    Accuracy of model on Test data is: Out of Sample Accuracy. 
str(wine_test)
PredictTest = predict(model4, newdata=wine_test)
PredictTest 
# actual was 6.95, 6.5 from str(wine_test)

SSE = sum((wine_test$Price - PredictTest)^2)
SST = sum((wine_test$Price - mean(wine$Price))^2)
1 - SSE/SST  #.79

# Test R^2 needs to be considered, Train R2 can increase by test can fluctuate. 




