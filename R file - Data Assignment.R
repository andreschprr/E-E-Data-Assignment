####################################
# Data Assignment
# Author: Andres Chaparro Altamirano
# Date: November/2019
####################################

#Load libraries
library(ggplot2)
library(dplyr)
library(car)
library(fastDummies)
library(repmis)
library(lubridate)
library(ggpubr)
library(lfe)

#Loading data and setting wd

main_folder <- "~/Desktop"
setwd(main_folder)

China_data <-"https://github.com/andreschprr/E-E-Data-Assignment/blob/master/Question1.Rdata?raw=true"
part1data <- "https://github.com/andreschprr/E-E-Data-Assignment/blob/master/part1.Rdata?raw=true"
part2data <- "https://github.com/andreschprr/E-E-Data-Assignment/blob/master/part2.Rdata?raw=true"

source_data(China_data)
source_data(part1data)
source_data(part2data)


############
#Part 1
############

#Histogram

APIdata$Date <- as.Date(APIdata$Date, "%m/%d/%Y")
c <- ggplot(APIdata, aes(API)) + geom_histogram(mapping = NULL, data = NULL, stat = "bin",
                 position = "stack", colour = "white", binwidth = NULL, bins = 40,
                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  geom_vline(xintercept = 100,col="red")
print(c)


############
#Part 2.1
############

#####################################################################################

#2.1.1

#Summary table. Subsetting only the three variables that we care about

part1_1data <- cbind(part1$PM10, part1$PM25, part1$NO2)
colnames(part1_1data) <- c("PM10", "PM25", "NO2")
summary(part1_1data)

#####################################################################################

#2.1.2

#Grouping the data by date, getting the average mean per day and plotting a scatter plot

part1_2data <- part1 %>% group_by(date)

data_plot1_2 <- part1_2data %>% summarise(
  PM25 = mean(PM25)
)

theme_set(theme_bw())
plot1_2 <- ggplot(aes(x = date, y = PM25), data = data_plot1_2) + geom_point() + geom_smooth() 

print(plot1_2 + labs( title= "Average PM25 per Day (Nov 2015 - Apr 2016)", y="Average PM25", x = "Date"))

#####################################################################################

#2.1.3

#Grouping the data by hour, then getting the average mean by hour, plotting a line

part1_3data <- part1 %>% group_by(Hour)

data_plot1_3 <- part1_3data %>% summarise(
  PM25 = mean(PM25)
)

plot1_3 <- ggplot(aes(x = Hour, y = PM25), data = data_plot1_3) + geom_line() +
  geom_vline(xintercept = 8,col="springgreen3", linetype = "dashed")+
  geom_vline(xintercept = 20,col="springgreen3", linetype = "dashed")

print(plot1_3 + labs( title= "Average PM25 per Hour (Nov 2015 - Apr 2016)", y="Average PM25", x = "Hour of the day"))

#The policy is in place from 8:00 until 20:00. As we can see in the graph from 8:00 to 12:00 the pollution is at some of its highest levels of the day. However after 12:00 the pollution decreases for most of the day considerably and starts to pick up after 20:00. The highest levels of pollution actually occur before and after the policy is in place. It looks like the policy may not be addressing the main problem, although there can be other factors that may be causing PM25 pollution at night/morning and have nothing to do with driving

#####################################################################################

#2.1.4

#Removing non Delhi data. Grouping by station, getting average PM25 by stations. Plotting columns

part1_Delhi <- subset(part1, Delhi == 1)
part1_4data <- part1_Delhi %>% group_by(station_name)

data_plot1_4 <- part1_4data %>% summarise(
  PM25 = mean(PM25)
)


plot1_4 <- ggplot(aes(x = station_name, y = PM25), data = data_plot1_4) + geom_col()+
  geom_hline(yintercept = mean(data_plot1_4$PM25),col="springgreen3", linetype = "dashed")

print(plot1_4 + labs( title= "Average PM25 in Delhi stations (Nov 2015 - Apr 2016)", y="Average PM25", x = "Dehli Station"))


#Variations in pollution can occur due to the geographic distribution of stations. Industrial zones or zones with a lot of traffic are expected to have higher pollution levels than less populated areas, or areas without high-pollutant industries. Wind and other geological factors also influence levels of pollution

#####################################################################################

#2.1.5

#Using the data from the previous question but ordered from more PM25 to less. 

data1_5 <- data_plot1_4[order(data_plot1_4$PM25, decreasing = TRUE),]

#Generate 7 vectors. Each has between 1 and 7 PM25 measures. 1 being the case where only one station is active

d1_5 <- data1_5[1:1,2]
d2_5 <- data1_5[1:2,2]
d3_5 <- data1_5[1:3,2]
d4_5 <- data1_5[1:4,2]
d5_5 <- data1_5[1:5,2]
d6_5 <- data1_5[1:6,2]
d7_5 <- data1_5[1:7,2]

#Get the mean of the PM25 measures in each vector, then unite them under a single dataframe

plot_data1_5 <- cbind(as.numeric(colMeans(d1_5)), 
as.numeric(colMeans(d2_5)), 
as.numeric(colMeans(d3_5)), 
as.numeric(colMeans(d4_5)), 
as.numeric(colMeans(d5_5)), 
as.numeric(colMeans(d6_5)), 
as.numeric(colMeans(d7_5)))

plot_data1_5_new <- cbind.data.frame(plot_data1_5)
plot_data1_5_new_t <- t(plot_data1_5_new)

#Add new columnd data with x-axis values (1 to 7), add label to the columns and plot with a line
new_col_data <- 1:7
new_col <- data_frame(new_col_data)
data_to_plot_15 <- cbind(new_col, plot_data1_5_new_t)
names(data_to_plot_15) <- c("number", "PM25")

plot1_5 <- ggplot(aes(x = number, y = PM25), data = data_to_plot_15) + geom_line() + geom_point()
print(plot1_5 + labs( title= "Average PM25 in Delhi (Nov 2015 - Apr 2016)", y="Average PM25", x = "Number of stations considered"))


#The pollution only considering the first station is 248.5 PM25 average a day for the available data. Considering all stations, the average PM25 per day goes down to 176.43. This is a 72 points decrease, or 30% decline over the initial value. 


############
#Part 2.2
############

#####################################################################################

#2.2.1


t_start = part2$time[which(part2$date==dmy("14-12-2015"))][1]
t_end = part2$time[which(part2$date==dmy("16-01-2016"))][1]
t_policy = part2$time[which(part2$date==dmy("01-01-2016"))][1]

data = part2 %>% filter(time>t_start & time<t_end)
data = data %>% group_by(Delhi,time) %>% 
  summarise(pm25=mean(PM25,na.rm=TRUE))

data$Delhi=as.factor(data$Delhi)

p = ggline(data, "time", "pm25",
           shape = "Delhi",
           color = "Delhi", palette = c("magenta4", "lightsteelblue"),
           numeric.x.axis = TRUE)

p = p + geom_vline(xintercept = t_policy)
p


#Initially the levels of PM25 went down in the first couple of days both in Delhi and elsewhere. However the next few days we can see the highest numbers for the period. Then we have an extreme downward trend and a rebound. It is difficult to get a definitive conclusion about the impact of the policy since there is so much variation between days.

#####################################################################################

#2.2.2

diff = data$pm25[which(data$Delhi=="1")] - data$pm25[which(data$Delhi=="0")]
difference = cbind(data$time[which(data$Delhi=="1")], diff)
difference = as.data.frame(difference)
names(difference)=c("time","pm25")
ggscatter(data=difference,y="pm25",x="time",color = "lightblue3", shape = 8, size = 2)


#####################################################################################

#2.2.3

#Use only Delhi data

part2Delhi <- subset(part2, Delhi == 1)

#Divide df in 3 parts. Before 2016, Between Jan 1st and Jan 15th, After Jan 15th

part2Delhi_bp <- subset(part2Delhi, date < "2016-01-01")
part2Delhi_p <- subset(part2Delhi, date > "2015-12-31" & date < "2016-01-16")
part2Delhi_ap <- subset(part2Delhi, date > "2016-01-15")

#Create new dummy variable on whether the policy was in place.

part2Delhi_bp$Policy <- 0
part2Delhi_p$Policy <- 1
part2Delhi_ap$Policy <- 0

#Reunite the three dataframes and do a linear regression using the new Policy variable

part2_3 <- rbind(part2Delhi_bp, part2Delhi_p, part2Delhi_ap)

reg2_3 <- felm(PM25~1+Policy|station_id|0|0,data= part2_3)

summary(reg2_3)

#We find a positive relationship between Policy in place and PM25 levels. This is to say that on the time that the policy was in place we saw an increase in PM25. The estimates are significant under 99% confidence interval.
#The coefficienct for Policy can be explained by other factors. One can be that it was because of the expected increase in pollution that the policy was put in place. There can be other variables that we are ommitting, however it doesn't paint a positive picture for the policy.


#####################################################################################

#2.2.4

#Using the data from the previous question, run another regression

reg2_4 <- felm(PM25~1+Policy*time|station_id|0|0, data = part2_3)
summary(reg2_4)

#We still have all estimates significant at the 99%. In this case the Policy coefficient is still positive. Nevertheless, both time and Policy*time are negative. The coefficient of Policy is the effect of Policy only when time is zero. It is kind of difficult to picture. The coefficient of time is the effect of time on PM25 when Policy is zero. This would measure how much is the pollution increasing or decreasing through time. Finally the coefficient of time*Policy is the effect of time and Policy together. So the effect of time when the policy is active. We could argue that as time passes when the policy is active, more people adapt and reduce car usage, thus reducing PM25. This is a possible explanation but not by any means the only thing we could interpret from this data. 

#####################################################################################

#2.2.5

#Same process of creating dummy if the policy was in place but this time for data inside and outside Delhi

part2_bp <- subset(part2, date < "2016-01-01")
part2_p <- subset(part2, date > "2015-12-31" & date < "2016-01-16")
part2_ap <- subset(part2, date > "2016-01-15")

part2_bp$Policy <- 0
part2_p$Policy <- 1
part2_ap$Policy <- 0

part2_5 <- rbind(part2_bp, part2_p, part2_ap)

#Use data to run a regression

reg2_5 <- felm(PM25~1+Delhi*Policy|time|0|0,data=part2_5)
summary(reg2_5)

#We still have all estimates significant at the 99%. In this case the $Policy$ coefficient is still positive. Nevertheless, both time and $Policy*time$ are negative. The coefficient of $Policy$ is the effect of the policy only when time is zero, so what is the effect of the Policy when the policy is not active. This may sound weird but in some cases, people can adapt depending their expectations. If you know that tomorrow you are not going to have hot water you may decide to shower today in the evening instead. So even if the event is not in place it influenced another time. However, this wasn't the case here. Finally the coefficient of $Policy*time$ is the effect of time and Policy together. So the effect of time when the policy is active. We could argue that as time passes when the policy is active, more people adapt and reduce car usage, thus reducing PM25. This is a possible explanation but not by any means the only thing we could interpret from this data. 

#####################################################################################

#2.2.6

#Same process of creating dummy if the policy was in place but for data from part 1


part1_bp <- subset(part1, date < "2016-01-01")
part1_p <- subset(part1, date > "2015-12-31" & date < "2016-01-16")
part1_ap <- subset(part1, date > "2016-01-15")

part1_bp$Policy <- 0
part1_p$Policy <- 1
part1_ap$Policy <- 0

part2_6 <- rbind(part1_bp, part1_p, part1_ap)

#Similar process of creating dummy if the policy was in place but this time also considering the hour


part2_morning_p <- subset(part2_6, Hour < 8)
part2_effect_p <- subset(part2_6, Hour > 7 & Hour < 21)
part2_late_p <- subset(part2_6, Hour > 20)

part2_morning_p$Policy2 <- 0
part2_effect_p$Policy2 <- 1
part2_late_p$Policy2 <- 0

part2_6_2 <- rbind(part2_morning_p, part2_effect_p, part2_late_p)

#Unite two constraints (hour and date), so that the policy variables is positive in both

part2_6_2$Policy3 <- part2_6_2$Policy + part2_6_2$Policy2

part2_6_3 <- dummy_cols(part2_6_2, select_columns = "Policy3")
part2_6_3$True_Policy <- part2_6_3$Policy3_2

#Run regression with this new data

reg2_6 <- felm(PM25~1+Policy+Policy:Delhi|time+station_id+Hour|0|0, data = part2_6_3)
summary(reg2_6)

#Every coefficient is statistically significant. The $Polcy*Delhi$ interaction term is negative which means that there is a decrease in PM25 particles in Dehli during the Policy. This is evidence that there is a strong relationship between the policy being in place in a particular location and a decrease in PM25 pollution










