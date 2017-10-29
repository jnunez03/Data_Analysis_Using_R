# Link to CSV
# https://prod-edxapp.edx-cdn.org/assets/courseware/v1/3ef78a720083c74cd2649289a475158d/asset-v1:MITx+15.071x+2T2017+type@asset+block/USDA.csv


# Read the csv file
 # USDA = read.csv("USDA.csv")
# Structure of the dataset
  str(USDA)
# Statistical summary
  summary(USDA)
  
  # max level of sodium was 38758 which is insanely high so we will
  # investigate further below. 

# Vector notation
  USDA$Sodium
# Finding the index of the food with highest sodium levels
  which.max(USDA$Sodium) #256th variable is highest.
  
# Get names of variables in the dataset
  names(USDA)  #Description gives us the food names.
  
# Get the name of the food with highest sodium levels
  USDA$Description[265]  #Salt was the reason of high level of sodium.. ofcourse.
  
# Create a subset of the foods with sodium content above 10,000mg
  HighSodium = subset(USDA, Sodium>10000)
# Count the number of rows, or observations
nrow(HighSodium)  # how many observations or foods are in high sodium
# only 10, so we'll output them below. 

# Output names of the foods with high sodium content
  HighSodium$Description
  #We know caviar has high sodium. but don't see it, so we need to
  #track down caviar in description.
# Finding the index of CAVIAR in the dataset
  match("CAVIAR", USDA$Description)   # its 4154th observation
# Find amount of sodium in caviar using bracket notation.
  USDA$Sodium[4154]  # IT shows caviar has 1500 mg of sodium
# Doing it in one command! v 
  USDA$Sodium[match("CAVIAR", USDA$Description)]
  
# Summary function over Sodium vector
  summary(USDA$Sodium)  #We see the mean.
# Standard deviation
  sd(USDA$Sodium, na.rm = TRUE)  # Have to remove NA values for sd.
  # we see it comes out to 1045.
  # mean is 322.1, that means caviar is above 1 standard deviation 
  # away from the mean in sodium content.
   
  #plotting....

# Scatter Plots
  plot(USDA$Protein, USDA$TotalFat)
# Add xlabel, ylabel and title & making it red.
  plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
# Creating a histogram takes 1 var as input..
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C")
# Add limits to x-axis
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100))
# Specify breaks of histogram to actually see the plot!
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100), breaks=100)
  # 2000/100, each cell is 20mg long.
  # we want 0 to 100 to have 100 cells each 1 mg in length.
  #more than 5000 have less than 1mg of vitamin C.
  hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100), breaks=2000)
# Boxplots
  boxplot(USDA$Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar")
  


# Creating a variable that takes value 1 if the food has higher sodium than average, 0 otherwise
  HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
  str(HighSodium)
# Adding the variable to the dataset
  USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
  str(USDA) # will show new variable high sodium
  
# Similarly for HighProtein, HigCarbs, HighFat
  USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
  USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
  USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
 str(USDA) #new added variables.

#SUMMARY TABLES, how can we see relationships between varibles...
 ###  Tapply takes in three arguments third argument is applied, like "mean" or "sd", or "max".
 # don't forget to remove NA entries.
 
# How many foods have higher sodium level than average?
  table(USDA$HighSodium)  
  # 2090 have higher sodium than average.
  
# How many foods have both high sodium and high fat?
  table(USDA$HighSodium, USDA$HighFat)
  
  
# Average amount of iron sorted by high and low protein?
  tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
  
# foods with low protein have on average 2.55mg of iron & foods with high protein have on average
# 3.19728 mg of iron.
  
# Maximum level of Vitamin C in foods with high and low carbs? 
  # sorting it according to the high Carbs vector, 
  tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
# Using summary function with tapply
  tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)
