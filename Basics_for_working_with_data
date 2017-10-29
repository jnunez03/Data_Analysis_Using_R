 #Vectors
c(2,3,5,8,13)
Country = c("Brazil", "China", "India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country  #Prints out all values
LifeExpectancy # Prints out all values
Country[1]  # Prints Brazil
LifeExpectancy[3] # prints 65
Sequence = seq(0,100,2)  # sequence of #'s. 
Sequence # prints, 0 to 100, in increments of 2. 



# Data Frames
CountryData = data.frame(Country, LifeExpectancy) # Combines into "table". 
CountryData
CountryData$Population = c(199000,1390000,1240000,7997,318000)
# "$" Links new data into dataframe. 
CountryData  #Updated data frame. with updated 3rd column, population.

# Merge 2 data frames together....using rbind.
Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)
NewCountryData
AllCountryData = rbind(CountryData, NewCountryData)
AllCountryData         # Prints out 



# Loading csv files
WHO = read.csv("WHO.csv")
str(WHO)   #structure of data.
summary(WHO)

# Subsetting
WHO_Europe = subset(WHO, Region == "Europe")  #Data set, region, to pick subset.
str(WHO_Europe)

# Writing csv files
write.csv(WHO_Europe, "WHO_Europe.csv")

# Removing variables
rm(WHO_Europe)


# Basic data analysis 
WHO$Under15 # shows the under15 vector of dataframe WHO

mean(WHO$Under15) # Avg % of the pop under 15, is 28.7.
sd(WHO$Under15)
summary(WHO$Under15) # This gives IQR of variable .

which.min(WHO$Under15) # shows us row of country who has min value in that variable
WHO$Country[86]  # Japan is the country who has 13% of pop under 15.

which.max(WHO$Under15)
WHO$Country[124]

# Scatterplot
plot(WHO$GNI, WHO$FertilityRate)

# Subsetting
# Seen from plot...
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5) 
nrow(Outliers)  # nrow gives # of rows, with parameters in outliers variable.
Outliers[c("Country","GNI","FertilityRate")] #Shows the values of 3 variables.


# Histograms
hist(WHO$CellularSubscribers)

# Boxplot
boxplot(WHO$LifeExpectancy ~ WHO$Region)  #How life expectancy varies with (region - x axis)

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region")

# Summary Tables
table(WHO$Region)  #similiar to summary, counts # of observations in each region.

tapply(WHO$Over60, WHO$Region, mean)   ## Splits obs (over 60) by region and computes mean,
tapply(WHO$LiteracyRate, WHO$Region, min)     ## same,but we have values of NA.
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)       #removes all NA. 
