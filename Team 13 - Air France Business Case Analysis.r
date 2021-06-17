###############################################################################
###### Pirate R Team 13 (MSBA 4) by 17th Dec, 2020  ###########################
###### Daniel Goldshtein/Kyoji Lida/Martin Dankl/Tanvi Gupta/Yan Yuan #########
###### AirFrance Case Business Analysis #######################################
###############################################################################



####################### Step 1 Importing Dataset ##############################

#install.packages("tidyverse")
library(tidyverse)
# open excel file
library(readxl)
air_france_s2<- Air_France_Case_Spreadsheet_Supplement <- read_excel("Desktop/03. Master of Business Analytics/Semester A/01. R Data Science/Team Assignment /Data/Air France Case Spreadsheet Supplement.xls", 
                                                                     sheet = "DoubleClick")
View(air_france_s2)

##################### Step 2 Massaging & Analysis Data  ########################
#Convert to data frame
my_data <- as.data.frame(air_france_s2)

#normalizing data to get better insights
my_normalize <- function(x){
  my_min <- min(x, na.rm = T)
  my_max <- max(x, na.rm = T)
  min_max <- (x-my_min)/(my_max-my_min)
  return(min_max)
}#closing my normalize

my_data$seb_norm <- my_normalize(x=my_data$`Search Engine Bid`)
my_data$clicks_norm <- my_normalize(x=my_data$Clicks)
my_data$clickcharges_norm <- my_normalize(x=my_data$`Click Charges`)
my_data$avgcpc_norm <- my_normalize(x=my_data$`Avg. Cost per Click`)
my_data$Impressions_norm <- my_normalize(x=my_data$`Impressions`)
my_data$ectp_norm <- my_normalize(x=my_data$`Engine Click Thru %`)
my_data$avgpos_norm <- my_normalize(x=my_data$`Avg. Pos.`)
my_data$tranc_norm <- my_normalize(x=my_data$`Trans. Conv. %`)
my_data$tct_norm <- my_normalize(x=my_data$`Total Cost/ Trans.`)
my_data$amount_norm <- my_normalize(x=my_data$Amount)
my_data$tcost_norm <- my_normalize(x=my_data$`Total Cost`)
my_data$tvob_norm <- my_normalize(x=my_data$`Total Volume of Bookings`)

# get the summary of total volume of bookings
summary(my_data$`Total Volume of Bookings`)

# the average of total volume of bookings is 0.87, so we create binary colums
# useing for loop the replace the data: 0(the book num <1) is failure and 1 (the book num > =1) is success
for (i in 1:nrow(my_data)){
  if(my_data$`Total Volume of Bookings`[i]<=5){my_data$binary[i] <- as.numeric(0)}
  
  else if (my_data$`Total Volume of Bookings`[i]> 5){my_data$binary[i] <- as.numeric(1)}
}

# build the logistic regression to check the relevant variables
my_logit_norm <- glm(binary ~ seb_norm+ clicks_norm+ clickcharges_norm+ avgcpc_norm
                     + ectp_norm+ avgpos_norm+tranc_norm + amount_norm +tcost_norm,
                     data = my_data, family = "binomial")
summary(my_logit_norm)

# regression with non-normalized data
my_logit <- glm(binary ~ `Click Charges`+ `Engine Click Thru %` + `Engine Click Thru %`
                + `Trans. Conv. %` + `Total Cost/ Trans.` + Amount + `Total Cost`,
                data = my_data, family = "binomial")
summary(my_logit)

# from the regression models, we can see the amount and cost are significant
# we start to build the relationship between these variables and get deeper insights

# calculating the data to check the total revenue
my_data$Net_Revenue <- (my_data$Amount - my_data$`Total Cost`)

# here we calculating the ROA
my_data$ROA <- my_data$Net_Revenue / my_data$`Total Cost` 

# here we calculate the average revenue per booking
my_data$Average_Revenue_per_Booking <- my_data$Amount / my_data$`Total Volume of Bookings`

# here is our probability of booking
my_data$Probability_of_Booking <- (my_data$`Trans. Conv. %` * my_data$`Engine Click Thru %`) /10000

# here is the cost of booking
my_data$Cost_booking <- my_data$`Total Cost` / my_data$`Total Volume of Bookings`

# sum of the Net_Revenue 
sum(my_data$Net_Revenue)

# sum of different label's Net_Revenue
my_net <- aggregate(my_data$Net_Revenue, list(my_data$`Publisher Name`), sum)

# sum of different label's ROA
my_ROA <- aggregate(my_data$ROA, list(my_data$`Publisher Name`), sum)

##################### Step 3 Plotting Data   ####################################
#create the rank of average cost per click by labels
install.packages("data.table")
library(caret)
library(lattice)
library("data.table")
library("dplyr")
library("ggplot2")
library("ROCR")

#Plot 1 - creating plot of the publishers and ROA
ggplot(my_data, aes(x = reorder(`Publisher Name`, ROA), y = ROA, fill = `Publisher Name`)) + 
  geom_col() + scale_fill_brewer(palette = "Greens") + theme_light()+ coord_cartesian(xlim = c(0, 7))+
  labs(x = "Publisher Name", y = "ROA", title = "ROA per Publisher") + 
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

#Plot 2 - creating plot of the publishers and net revenue
ggplot(my_data, aes(x = reorder(`Publisher Name`, Net_Revenue), y = Net_Revenue, fill = `Publisher Name`)) + 
  geom_col() + scale_fill_brewer(palette = "Reds") + theme_light()+ coord_cartesian(xlim = c(0, 7))+
  labs(x = "Publisher Name", y = "Net_Revenue", title = "Net Revenue per Publisher") + 
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

#Plot 3 - creating plot of the publishes and the total cost
ggplot(my_data, aes(x = reorder(`Publisher Name`, `Total Cost`), y = `Total Cost`, fill = `Publisher Name`)) + 
  geom_col() + scale_fill_brewer(palette = "Blues") + theme_light()+ coord_cartesian(xlim = c(0, 7))+
  labs(x = "Publisher Name", y = "Total Cost", title = "Total Cost per Publisher") + 
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

#Plot 4 - creating plot of average cost per click and publishers
ggplot(my_data, aes(x= `Publisher Name`, y= `Avg. Cost per Click`)) +
  geom_segment( aes(x= `Publisher Name`, xend= `Publisher Name`, y=0, yend=`Avg. Cost per Click`)) +
  geom_point( size=7, color="blue", fill=alpha("white", 0.3), alpha=0.7, shape=21, stroke=2) 

#Plot 5 - creating plot of sum of click and publishers
ggplot(my_data, aes(x= `Publisher Name`, y= `Click Charges`)) +
  geom_segment( aes(x= `Publisher Name`, xend= `Publisher Name`, y=0, yend=`Click Charges`)) +
  geom_point( size=7, color="orange", fill=alpha("white", 0.3), alpha=0.7, shape=21, stroke=2) 

#Plot 6 - creating plot of total volume of bookings and publishers
ggplot(my_data, aes(x=`Publisher Name`, y= `Total Volume of Bookings`)) +
  geom_segment( aes(x=`Publisher Name`, xend=`Publisher Name`, y=0, yend= `Total Volume of Bookings`), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#Plot 7 - creating plot of the booking probability and publishers
# create a new column for probability of booking
aggregate(my_data$Probability_of_Booking, by=list(Category=my_data$`Publisher Name`), FUN=sum, na.rm = TRUE)
# plot
ggplot(my_data, aes(x=`Publisher Name`, y= Probability_of_Booking)) +
  geom_segment( aes(x=`Publisher Name`, xend= `Publisher Name`, y=0, yend=Probability_of_Booking)) +
  geom_point( size=7, color="green", fill=alpha("red", 0.3), alpha=0.7, shape=21, stroke=2) 
gg_base <- ggplot(data = my_data, aes(x = `Publisher Name`, y = Net_Revenue))

#Plot 8 - creating plot of Trans. Conv. % and publishers
ggplot(my_data,aes(`Publisher Name`,`Trans. Conv. %`, fill=`Trans. Conv. %`))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text)+  
  ggtitle("Publisher's Trans. Conv. %")+theme_bw()

#Plot 9 - creating plot of Average_revenue_per_booking and publishers
# create a new column for average revenue per booking
aggregate(my_data$Average_Revenue_per_Booking, by=list(Category=my_data$`Publisher Name`), FUN=sum, na.rm = TRUE)
#Plot
gg_base <- ggplot(data = my_data, aes(x = `Publisher Name`, y = Average_Revenue_per_Booking))

gg_base +
  geom_point(
    aes(size = `Publisher Name`,
        color = Average_Revenue_per_Booking),
    alpha = 0.5
  ) +
  theme_minimal()

###############################################################################
##################### Business Insights from Data Analysis ####################
###############################################################################

# According to our logistic regression model results and real life business 
# experiences, we decided to use belowing features for further analysis:
# 1. Amount
# 2. Total Cost
# 3. Total Volume of Bookings
# 4. Trans. Conv. %
# 5. Engine Click Thru %

# Above 9 plots help us better understand each publisher's performance in terms of cost and ROA.
# The plots showed below business insights for us:
# 1.Google US had the highest total cost but also had highest ROA and Net Revenue
# 2.Google US had the highest total volume of bookings
# 3.Yahoo US showed the highest probability of booking, Google US ranked sencond place
# 4.Yahoo US showed the highest average revenue per booking 
#Based on the above analysis we can conclude that 
#a) Google is the most successful company among the four companies in this case. Google earns the highest revene although it has the highest cost. 
#b) Generaly speaking, the business in US are better than the business in the Global level.












