################################################################################
#Air France Internet Marketing Case Study
#Brian Kurniawan
#Hult International Business School
#Data Science: R - MsBA1
################################################################################

#Calling the Air France Excel File
library(readxl)
airfrance <- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls",
                        sheet = "DoubleClick")
View(airfrance)

#Analyzing and understanding the data within Air France
summary(airfrance)
subset(airfrance)

#Creating a new data frame composed of only the variables we deem important
new_airfrance <- airfrance[, c("Publisher Name", "Keyword", "Match Type", "Campaign", "Keyword Group",
                               "Status", "Search Engine Bid", "Clicks", "Avg. Cost per Click",
                               "Impressions", "Engine Click Thru %", "Trans. Conv. %",
                               "Total Cost/ Trans.", "Amount", "Total Cost", "Total Volume of Bookings")]

new_airfrance <- as.data.frame(new_airfrance)

#Checking if the new_airfrance data set has any NA values
sum(is.na(new_airfrance))

#Changing variable types to numeric
new_airfrance$`Publisher Name` <-  as.numeric(as.factor(new_airfrance$`Publisher Name`))
#Google-Global = 1, Google-US = 2, MSN-Global = 3, MSN-US = 4, Overture-Global = 5, Overture-US = 6, Yahoo-US = 7
table(new_airfrance$`Publisher Name`)

new_airfrance$`Match Type` <- as.numeric(as.factor(new_airfrance$`Match Type`))
#Advance = 1, Broad = 2, Exact = 3, N/A = 4, Standard = 5
table(new_airfrance$`Match Type`)

new_airfrance$Campaign <- as.numeric(as.factor(new_airfrance$Campaign))
#Air France Branded = 1, Air France Branded & French Destinations = 2, Air France Global Campaign = 3
#Business Class = 4, French Destinations = 5, General Terms = 6, Geo Targeted Atlanta = 7, Geo Targeted Boston = 8
#Geo Targeted Chicago = 9, Geo Targeted Cincinnati = 10, Geo Targeted DC = 11, Geo Targeted Detroit = 12
#Geo Targeted Houston = 13, Geo Targeted Los Angeles = 14, Geo Targeted Miami = 15, Geo Targeted New York = 16
#Geo Targeted Philadelphia = 17, Geo Targeted San Francisco = 18, Geo Targeted Seattle = 19, Google_Yearlong 2006 = 20
#Outside Western Europe = 21, Paris & France Terms = 22, Unassigned = 23, Western Europe Destinations = 24
table(new_airfrance$Campaign)

new_airfrance$`Keyword Group` <- as.numeric(as.factor(new_airfrance$`Keyword Group`))
table(new_airfrance$`Keyword Group`)

new_airfrance$Status <- as.numeric(as.factor(new_airfrance$Status))
#Deactivated = 1, Live = 2, Paused = 3, Sent = 4, Unavailable = 5
table(new_airfrance$Status)

#Adding a new variable to the data set - PoA (Probability of Action)
#PoA = Engine Click Thru % * Trans. Conv. %
#How many people perform an action before getting to the website * How many people perform an action after getting to the website
#PoA is the probability of an action being performed on the website based on visits before and after getting to it
new_airfrance$PoA <- new_airfrance$`Engine Click Thru %` * new_airfrance$`Trans. Conv. %`

#Adding a new variable to the data set - RoA (Return on Advertising Expenditure)
#RoA = Amount / Total Cost
#
new_airfrance$RoA <- new_airfrance$Amount / new_airfrance$`Total Cost`

#CORRELATE PUBLISHER, KEYWORD GROUP, CTR, TCR, POA, AND ROA

install.packages("ggpubr")
library("ggpubr")
cor(new_airfrance)

#scatter plot between publiher and POA
plot(x=new_airfrance$'RoA', y=new_airfrance$'Publisher Name', type='p')

library(ggplot2)
ggplot()+
  geom_bar(data= new_airfrance, aes(x=new_airfrance$'Publisher Name'))

ggplot()+
  geom_bar(data= new_airfrance, aes(x=new_airfrance$'Campaign'))



#publisher name - ROA
table(new_airfrance$RoA)
ggplot(data=new_airfrance) +
  geom_point(aes(x=new_airfrance$'Publisher Name', y=PoA))

#publisher name - ROA
ggplot(data=new_airfrance) +
  geom_point(aes(x=new_airfrance$'Publisher Name', y=RoA))

ggplot(data=new_airfrance, aes(x=RoA, y=PoA, color = 'Publisher Name', xlim(0,1) )) +
  geom_point(alpha = 0.2, shape = 10, size = 0.1, )

airfrance_scatter <- ggplot() +
  geom_point(data=new_airfrance, aes(x=new_airfrance$"Publisher Name", y=PoA), color ="red", alpha = 0.5)+
  geom_point(data=new_airfrance, aes(x=new_airfrance$"Publisher Name", y=RoA), color="blue", alpha = 0.5)
  
airfrance_scatter2 <- ggplot() +
  geom_point(data=new_airfrance, aes(x=PoA, y=Campaign), color ="red", alpha = 0.5)+
  geom_point(data=new_airfrance, aes(x=RoA, y=Campaign), color="blue", alpha = 0.5)

library(plotly) 

ggplotly(airfrance_scatter)
ggplotly(airfrance_scatter2)
