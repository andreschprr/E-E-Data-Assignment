####################################
# Data Assignment
# Author: Andres Chaparro Altamirano
# Date: November/2019
####################################

library(ggplot2)
library(dplyr)
library(car)
library(fastDummies)

main_folder <- "~/Desktop/Data Assignment"
setwd(main_folder)
China_data <- "~/Desktop/Data Assignment/Question1.RData"
part1data <- "~/Desktop/Data Assignment/part1.RData"
part2data <- "~/Desktop/Data Assignment/part2.RData"

############
#Part 1
############

load(China_data) 

APIdata$Date <- as.Date(APIdata$Date, "%m/%d/%Y")

c <- ggplot(APIdata, aes(API)) 

c + geom_histogram(mapping = NULL, data = NULL, stat = "bin",
                 position = "stack", colour = "white", binwidth = NULL, bins = 40,
                 na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  geom_vline(xintercept = 100,col="red")

############
#Part 2.1
############

load(part1data)

#####################################################################################

#2.1.1
part1_1data <- cbind(part1$PM10, part1$PM25, part1$NO2)
colnames(part1_1data) <- c("PM10", "PM25", "NO2")
summary(part1_1data)

#####################################################################################

#2.1.2

part1_2data <- part1 %>% group_by(date)

data_plot1_2 <- part1_2data %>% summarise(
  PM25 = mean(PM25)
)

theme_set(theme_bw())
plot1_2 <- ggplot(aes(x = date, y = PM25), data = data_plot1_2) + geom_point() + geom_smooth() 

print(plot1_2 + labs( title= "Average PM25 per Day (Nov 2015 - Apr 2016)", y="Average PM25", x = "Date"))

#####################################################################################

#2.1.3
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

data1_5 <- data_plot1_4[order(data_plot1_4$PM25, decreasing = TRUE),]

d1_5 <- data1_5[1:1,2]
d2_5 <- data1_5[1:2,2]
d3_5 <- data1_5[1:3,2]
d4_5 <- data1_5[1:4,2]
d5_5 <- data1_5[1:5,2]
d6_5 <- data1_5[1:6,2]
d7_5 <- data1_5[1:7,2]

plot_data1_5 <- cbind(as.numeric(colMeans(d1_5)), 
as.numeric(colMeans(d2_5)), 
as.numeric(colMeans(d3_5)), 
as.numeric(colMeans(d4_5)), 
as.numeric(colMeans(d5_5)), 
as.numeric(colMeans(d6_5)), 
as.numeric(colMeans(d7_5)))

plot_data1_5_new <- cbind.data.frame(plot_data1_5)
plot_data1_5_new_t <- t(plot_data1_5_new)
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

load(part2data)

#####################################################################################

#2.2.1

part2_1data <- part2 %>% group_by(date, Delhi)


data_plot2_1 <- part2_1data %>% summarise(
  PM25 = mean(PM25)
)

data_plot2_11 <- subset(data_plot2_1, date > "2015-11-30" & date < "2016-01-16")

ggplot(data=data_plot2_11, aes(x=date, y=PM25, group=Delhi, colour = Delhi))+ 
  geom_line()+
  geom_vline(xintercept = as.POSIXct("2016-01-01"), col = "gold2", linetype = "dashed")

#Initially the levels of PM25 went down in the first couple of dats both in Delhi and elsewhere. However the next few days we can see the highest numbers for the period. Then we have an extreme downward trend and a rebound. It is difficult to get a definitive conclusion about the impact of the policy since there is so much variation between days.

#####################################################################################

#2.2.2

data_2_2 <- data_plot2_1 %>% summarise(
  PM25 = mean(PM25)
)
  
data_2_2_Delhi <- subset(data_plot2_1, Delhi == 1)
data_2_2_NoDelhi <- subset(data_plot2_1, Delhi == 0)
data_2_2_merged <- merge(x = data_2_2_Delhi, y = data_2_2_NoDelhi, by = "date")
data_2_2_merged$DiffPM25 <- data_2_2_merged$PM25.x - data_2_2_merged$PM25.y 

plot2_2 <- ggplot(aes(x = date, y = DiffPM25), data = data_2_2_merged) + geom_point()
print(plot2_2 + labs( title= "Difference in daily PM25 between Delhi and outside Delhi", y="Avg PM25 in Delhi - Avg PM25 outside Delhi", x = "Date"))

#####################################################################################

#2.2.3

part2Delhi <- subset(part2, Delhi == 1)

part2Delhi_bp <- subset(part2Delhi, date < "2016-01-01")
part2Delhi_p <- subset(part2Delhi, date > "2015-12-31" & date < "2016-01-16")
part2Delhi_ap <- subset(part2Delhi, date > "2016-01-15")

part2Delhi_bp$Policy <- 0
part2Delhi_p$Policy <- 1
part2Delhi_ap$Policy <- 0

part2_3 <- rbind(part2Delhi_bp, part2Delhi_p, part2Delhi_ap)

reg2_3 <- lm(PM25 ~ station_id + Policy, data = part2_3)
summary(reg2_3)

#We find a positive relationship between Policy in place and PM25 levels. This is to say that on the time that the policy was in place we saw an increase in PM25. The estimates are significant under 99% confidence interval.
#The coefficienct for Policy can be explained by other factors. One can be that it was because of the expected increase in pollution that the policy was put in place. There can be other variables that we are ommitting, however it doesn't paint a positive picture for the policy.


#####################################################################################

#2.2.4

reg2_4 <- lm(PM25 ~ station_id + Policy*time, data = part2_3)
summary(reg2_4)

#We still have all estimates significant at the 99%. In this case the Policy coefficient is still positive. Nevertheless, both time and Policy*time are negative. The coefficient of Policy is the effect of Policy only when time is zero. It is kind of difficult to picture. The coefficient of time is the effect of time on PM25 when Policy is zero. This would measure how much is the pollution increasing or decreasing through time. Finally the coefficient of time*Policy is the effect of time and Policy together. So the effect of time when the policy is active. We could argue that as time passes when the policy is active, more people adapt and reduce car usage, thus reducing PM25. This is a possible explanation but not by any means the only thing we could interpret from this data. 

#####################################################################################

#2.2.5

part2_bp <- subset(part2, date < "2016-01-01")
part2_p <- subset(part2, date > "2015-12-31" & date < "2016-01-16")
part2_ap <- subset(part2, date > "2016-01-15")

part2_bp$Policy <- 0
part2_p$Policy <- 1
part2_ap$Policy <- 0

part2_5 <- rbind(part2_bp, part2_p, part2_ap)


reg2_5 <- lm(PM25 ~ time + Policy*Delhi, data = part2_5)
summary(reg2_5)

#In this case the coefficients from $Policy*Delhi$ and $Delhi$ are not statistically significant at a 95% Confidence Interval. This is to say that we can't really draw conclusions from this regression. Because the policy is in Delhi, the value we would be interested is $Policy*Delhi$ because that is the effect of the policy on Delhi. The policy shouldn't have an big effect outside of Delhi, and we are not looking at differences between Delhi and the outside so our attention should be on $Policy*Delhi$.

#####################################################################################

#2.2.6

part1_bp <- subset(part1, date < "2016-01-01")
part1_p <- subset(part1, date > "2015-12-31" & date < "2016-01-16")
part1_ap <- subset(part1, date > "2016-01-15")

part1_bp$Policy <- 0
part1_p$Policy <- 1
part1_ap$Policy <- 0

part2_6 <- rbind(part1_bp, part1_p, part1_ap)


part2_morning_p <- subset(part2_6, Hour < 8)
part2_effect_p <- subset(part2_6, Hour > 7 & Hour < 21)
part2_late_p <- subset(part2_6, Hour > 20)

part2_morning_p$Policy2 <- 0
part2_effect_p$Policy2 <- 1
part2_late_p$Policy2 <- 0

part2_6_2 <- rbind(part2_morning_p, part2_effect_p, part2_late_p)

part2_6_2$Policy3 <- part2_6_2$Policy + part2_6_2$Policy2

part2_6_3 <- dummy_cols(part2_6_2, select_columns = "Policy3")
part2_6_3$True_Policy <- part2_6_3$Policy3_2

reg2_6 <- lm(PM25 ~ date + station_id + Hour + Policy*Delhi, data = part2_6_3)
summary(reg2_6)

#Every coefficient is statistically significant except the $Polcy*Delhi$ interaction term. The result in this case makes a case for a really small decrease in pollution due to the policy for stations in Delhi. However, the result is too small and not statistically significant so we can't draw many conclussions.










