#++++++++++++++++++++  STOCK DYNAMICS   ++++++++++++++++++# 

#Read in our csv files...
IBM <- read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/4fc08d10f171aacf2ef61c6b4b5bb4d8/asset-v1:MITx+15.071x+2T2017+type@asset+block/IBMStock.csv")
GE <- read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/448b8be4693d913c2b5153be0c0e25d6/asset-v1:MITx+15.071x+2T2017+type@asset+block/GEStock.csv")
ProcterGamble <- read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/bb6ed54230b5b2e29fb66819ed535da0/asset-v1:MITx+15.071x+2T2017+type@asset+block/ProcterGambleStock.csv")
CocaCola <- read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/4c30fd7f4f55e537989ca13a6db36289/asset-v1:MITx+15.071x+2T2017+type@asset+block/CocaColaStock.csv")
Boeing <- read.csv("https://prod-edxapp.edx-cdn.org/assets/courseware/v1/2e8c9fb294db48e5a999c747b317722d/asset-v1:MITx+15.071x+2T2017+type@asset+block/BoeingStock.csv")

str(IBM) # monthly data for 40 years, 12*40 = 480 Observations

# Before working with the data set. Convert the dates into formats that R can understand.
# First argument in as.Date function is Variable to convert and 2nd is format we want it in.
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#  Earliest Year? .. use str() or summary()

summary(IBM)   
# latest year? ...2009.
#  mean stock price of IBM ? 144.38
summary(GE)
# Min stock price of GR in this time period.. 9.294
summary(CocaCola)
# max stock price of cocacola 146.58

# Standard Deviation of ProcterGamble's Stock Price ?
sd(ProcterGamble$StockPrice)


# Visualizing The Stock.

# type"l" Is for continuous time period. 
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
# lines function plots on same plot previously coded.
lines(ProcterGamble$Date, ProcterGamble$StockPrice, lty=2,col="blue")

# Draw vertical line at certain date on plot.
# To show the point at which both stocks dropped in March 2000, stock market crash.
abline(v=as.Date(c("2000-03-01")),lwd=2)


# Visualizing from 1995 - 2005 
plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432], type="l",col="red", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], lty=2,col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], lty=3,col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], lty=4,col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], lty=5,col="black")
# It would make sense to change the axis titles, due to all companies being plotted, not just one.
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)
# To check which of these companies were affected by Asia economic crisis in october of 1997, 
# We put lines in september & November



# ====  Monthly Trends ==== #
# Do stocks tend to be higher of lower in certain months. Use tapply command to calculate the mean, by months
tapply(IBM$StockPrice,Month, mean)
# this gives us alphabetical order.. I want Chronological order.
Month = factor(months(IBM$Date),levels=month.name)
# Run this line first the re run tapply, for months to be ordered Chronologically.

#    * You could repeat this for the other companies *   #

Month1 = factor(months(GE$Date), levels=month.name)
Month2 = factor(months(CocaCola$Date), levels=month.name)
tapply(GE$StockPrice,Month1, mean)
tapply(CocaCola$StockPrice,Month2, mean)
