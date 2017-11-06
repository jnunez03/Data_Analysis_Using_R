mvt <- read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/96f9b8f751467da3a4b8a5be33e32905/asset-v1:MITx+15.071x+2T2017+type@asset+block/mvtWeek1.csv")



# Each observation represents a motor vehicle theft., arrest indicated whether an arrest was made.
# find max of variable  which.max gives us the index.
which.max(mvt$ID)
mvt$ID[18134]   # 9181151 is the max. 
# Could also just run this..
max(mvt$ID)

 # min of variable beat. # also by using summary(mvt)
min(mvt$Beat)

 # How many observations have value TRUE in arrest variable. 
trueArrest = subset(mvt, Arrest==TRUE)
nrow(trueArrest)  #15536
 #LocationDescription ALLEY?
alleys = subset(mvt, LocationDescription=="ALLEY")
nrow(alleys) #2308 
# also could be read from table(mvt$LocationDescription)


 # lets look at date entries. 
str(mvt$Date)
 # its in month/day/year hour:min

mvt$Date[1] #first entry 
  
#   Lets convert.
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
# Median Date? 
summary(DateConvert)
 
# Extract month and day of the week and add these variables to data frame.
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
# Replace old date variable with DateConvert 
mvt$Date = DateConvert

# fewest motor vehicle thefts? 
table(mvt$Month) #Feb

# which weekday was most?
table(mvt$Weekday)

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Arrest, mvt$Month)


# Plot to see how crime changed over time in Chicago.

hist(mvt$Date, breaks=100)

boxplot(mvt$Date ~ mvt$Arrest)
## Arrest=TRUE is definitely skewed towards the bottom of the plot, meaning that there were more crimes for which arrest
# were made in the first half of the time period.

# What proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Arrest, mvt$Year)    # 2152 / 2152 + 18517


# top 5 locations where motor vehicle thefts occur?
sort(table(mvt$LocationDescription))

# Create a subset of data, only taking observations for which theft happened in 1 of these 5 locations
top5 = subset(mvt, LocationDescription=="STREET"|LocationDescription=="GAS STATION"|LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)"
              |LocationDescription=="ALLEY"|LocationDescription=="DRIVE-WAY - RESIDENTIAL")
str(top5)

# ALSO BY DOING
# toplocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
# top5 = subset(mvt, LocationDescription %in% toplocations)

# Table will have unneccesary stuff cause R remembers original data. Refresh the variable.
top5$LocationDescription = factor(top5$LocationDescription)
table(top5$LocationDescription)

# Which location has a much higher arrest rate. 

table(top5$LocationDescription, top5$Arrest)

# Which day of the week do most motor vehicle thefts at gas stations happen ?

table(top5$LocationDescription, top5$Weekday)
