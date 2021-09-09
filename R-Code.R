pacman :: p_load(pacman, tidyverse, dplyr, ggplot2)

#Part 1.1 

#Question 1

#An anomaly according to NASA, refers to when the conditions depart from the average conditions for a particular place at a given time of the year. 
#Absolute temperature on the other hand is measured from absolute 0 not from a particular reference point which would vary from one case to the other. 
#It makes ample sense to use temperature anomalies as we are measuring the temperature of land-ocean in the Northern Hemisphere, 
#from 1880 - 2016 with 1951 - 1980 as a reference line. In this way we can see how the temperature deviates and changes with respect to the reference line rather than absolute zero which would not make much sense here. 

tempdata <- read.csv("north_hemp_dat.csv",
                     skip = 1, na.strings = "***") 

#Question 2 

tempdata %>% 
  ggplot(aes(x = Year, y = Jan)) + 
  geom_line() + 
  labs(x = "Year", y = "Temperature Anomaly (degrees Celcius)", 
       title = "Northern Hemisphere Temperatures for January (1880 - 2019)")

#Question 3 

library(reshape)

df <- tempdata %>% 
  select(Year, DJF, MAM, JJA, SON)

df1 <- melt(df, id.vars = "Year")

colnames(df1)[2] <- "Seasons"

ggplot(df1, aes(x = Year, y = value, color = Seasons)) + 
  geom_line() + 
  labs(y = "Temperature Anomaly (degrees Celcius)", title = "Northern Hemisphere Temperatures for the Seasons (1880 - 2019)") + 
  scale_color_manual(values = c("black", "orange", "maroon", "skyblue"))

#Question 4

#part a
tempdata %>% 
  ggplot(aes(x = Year, y = J.D)) + 
  geom_line(col = "maroon") + 
  geom_hline(yintercept = 0) + 
  geom_text(aes(2000, 0, label = "1950 - 1980 average", vjust = 1.2)) + 
  labs(y = "Temperature anomaly (degrees celcius)", title = "Northern Hemisphere Average Annual Temperature (1880 - 2019)")
  
#part b
# The charts suggest that as time increases, the temperature anomaly increases as well. It is a positive relationship. 

#Question 5

#The month data shows us the foundation of the trend/patterns. We can understand how the movement of one month can impact the movement of several months in a season and henceforth the movement of a year. 
#For the monthly data we see there is a gradual increase in the temperature anomaly over time, especially from 1960 onwards, which is even more drastic for the seasonal and yearly data. Concurrently, 
#we can gather an overall picture from the yearly data as opposed to the seasonal or monthly data. We can see the overall trend in temperature anomaly over time for all months. The overall picture allows us to make policy-related decisions more holistically. 

#Question 6 

#part a 
#The y axes are not the same. For the other charts the y axis depicts the Temperature anomaly, while figure 1.4 shows the deviation from 1961-1990 mean temperature. 
#However, the x axes are the same as they are all measuring time, except figure 1.4 takes time from the year 1000 rather than 1880. 
#Because the lower limit of the x axis for figure 1.4 is further behind the pattern for the line chart is a bit different from the others. The lines that have been created 
#do have the same shape as figure 1.4 if we are to consider 1880 - 2019. There is an upward tick in the temperature as the years progress on. 
#Another difference is that figure 1.4 is a long run graph that is taking a long time span from 1000 - 2006 as opposed to our more short run plot from 1880 - 2019. 

#part b 
#The observed patterns in our charts are not unusual because if we observe 1880 onwards in figure 1.4 it is following the same general pattern as the graphs that were created. 

#part c
#The government should definitely be concerned about climate change as there has been a serious shift in temperature difference within the 2000s with respect to both the 
#1961 - 1990 reference point and also the 1951 - 1980 average. The difference is increasing over time and the government needs to act in order to prevent the temperatures from rising any further. 

#Part 1.2 

#Question 1

tempdata$Period <- 
  factor(NA, levels = 
           c("1880-1920","1921-1950","1951-1980", "1981-2010"), 
         ordered = TRUE)

tempdata$Period[(tempdata$Year >= 1880) &
                  (tempdata$Year <= 1920)] <- "1880-1920"
tempdata$Period[(tempdata$Year > 1920) &
                  (tempdata$Year < 1951)] <- "1921-1950"
tempdata$Period[(tempdata$Year > 1950) &
                  (tempdata$Year < 1981)] <- "1951-1980"
tempdata$Period[(tempdata$Year > 1980) &
                  (tempdata$Year < 2011)] <- "1981-2010"

temp_summer <- c(tempdata$Jun, tempdata$Jul, tempdata$Aug)

temp_Period <- 
  c(tempdata$Period, tempdata$Period, tempdata$Period)

temp_Period <- factor(temp_Period, 
                      levels = 1:nlevels(tempdata$Period), 
                      labels = levels(tempdata$Period))

var1 <- hist(temp_summer[(temp_Period == "1951-1980")], 
     plot = FALSE)[1]

var2 <- hist(temp_summer[(temp_Period == "1951-1980")], 
             plot = FALSE)[2]

var1 <- as.data.frame(var1)
var2 <- as.data.frame(var2)
var2[nrow(var2) + 1, ] <- NA


freq_table1 <- cbind(var1, var2)

var3 <- hist(temp_summer[(temp_Period == "1981-2010")], 
             plot = FALSE)[1]

var4 <- hist(temp_summer[(temp_Period == "1981-2010")], 
             plot = FALSE)[2]

var3 <- as.data.frame(var3)
var4 <- as.data.frame(var4)

var4[nrow(var4) + 1, ] <- NA

freq_table2 <- cbind(var3, var4)


#Question 2


#part a 
hist(temp_summer[(temp_Period == "1951-1980")], 
     plot = TRUE, main = "Frequency Histogram for 1951 - 1980", 
     xlab = "Temperature Anomaly (Degrees Celcius)", ylab = "Frequency") 


hist(temp_summer[(temp_Period == "1981-2010")], 
     plot = TRUE, main = "Frequency Histogram for 1981 - 2010", 
     xlab = "Temperature Anomaly (Degrees Celcius)", ylab = "Frequency") 

#part b 

#From the plots what can be observed is that the degree range is much wider for the 1981 - 2010 data with values ranging from 
#-0.2 to 0.9 as opposed to -0.3 to 0.25. This shows that there has been a greater uptick in the temperature anomalies, that too with greater frequency. 
#The 1951-1980 histogram is more centrally distributed than the 1981 - 2010 histogram which is skewed to the left. 

#Question 3

temp_all_months <- subset(tempdata, 
                          (Year >= 1951 & Year <= 1980))
temp_51to80 <- unlist(temp_all_months[, 2:13])
perc <- quantile(temp_51to80, c(0.3, 0.7))   
p30 <- perc[1]
p30 #the cold threshold 

p70 <- perc[2]
p70 #the hot threshold

#Question 4 
temp_all_months2 <- subset(tempdata, 
                           (Year >= 1981 & Year <= 2010))

temp_81to10 <- temp_all_months2[2:13]

filtered_temp81to10 <- temp_81to10 %>% 
  filter_all(all_vars(. > 0.11))

nrow(filtered_temp81to10)/nrow(temp_81to10)

#This is 57%. This shows that 57% of the total observations are hot 
#using the 7th decile of the 51-80 period. 30% of the observations were hot 
#in 51-80 and now it is 57%, this is clearly showcasing that we are experiencing 
#hotter weather more frequently. 

#Question 5

#part a

#1920 - 1951 period
new <- tempdata %>% 
  filter(Year > 1920 & Year < 1951)

table20_51 <- data.frame(mean_DJF = mean(new$DJF), var_DJF = var(new$DJF), 
                         mean_MAM = mean(new$MAM), var_MAM = var(new$MAM), 
                         mean_JJA = mean(new$JJA), var_JJA = var(new$JJA), 
                         mean_SON = mean(new$SON), var_SON = var(new$SON))

#1951 - 1980 period 
new <- tempdata %>% 
  filter(Year > 1950 & Year < 1981)

table51_80 <- data.frame(mean_DJF = mean(new$DJF), var_DJF = var(new$DJF), 
                         mean_MAM = mean(new$MAM), var_MAM = var(new$MAM), 
                         mean_JJA = mean(new$JJA), var_JJA = var(new$JJA), 
                         mean_SON = mean(new$SON), var_SON = var(new$SON))

#1981 - 2010 period 
new <- tempdata %>% 
  filter(Year > 1980 & Year < 2011)


table81_2010 <- data.frame(mean_DJF = mean(new$DJF), var_DJF = var(new$DJF), 
                         mean_MAM = mean(new$MAM), var_MAM = var(new$MAM), 
                         mean_JJA = mean(new$JJA), var_JJA = var(new$JJA), 
                         mean_SON = mean(new$SON), var_SON = var(new$SON))

#part b 

#For DJF the variance decreased marginally in the 51-80 period but then drastically 
#rose in the 81-10 period. Similarly for MAM, JJA and SON the variance changes marginally for the 51-80 
#period but then sharply rises for the 81-10 period showcasing that for all seasons 
#there is far more variability in the 81-10 period. 

#Question 6

#Based on the findings it is quite evident that there is more variability in the 
#weather and the uncertainty will only increase over time. It is therefore prudent
#for the government to spend more money on mitigating these effects. 

#Part 1.3

library(readxl)
co2_dat <- read_excel("~/Downloads/1_CO2-data.xlsx")

#Question 1

#The link on the website is not working for me to use it as a reference. 

#Question 2

#Interpolated basically signifies estimated data, which are values that are not observed but fall within the sample space. 
#Trend on the other hand refers to the real pattern over time. 
#There might be seasonal variations in the co2 levels because plants take in co2 during the spring and summer and release it during the fall and winter. 


#Question 3

co2_dat_new <- co2_dat %>% 
  filter(Year >= 1960)

p1 <- co2_dat_new %>% 
  ggplot(aes(x = Year, y = Interpolated)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = lm) + 
  labs(x = "Time", y = "Interpolated CO2 levels", title = "Plot of Interpolated CO2 levels vs. Time")

p2 <- co2_dat_new %>% 
  ggplot(aes(x = Year, y = Trend)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = lm) + 
  labs(x = "Time", y = "Trend CO2 levels", title = "Plot of Trend CO2 levels vs. Time")

plot_grid(p1, p2)

#Question 4

#part a 

trend_dat_Jan <- co2_dat_new %>% 
  select(Year, Trend, Month) %>% 
  filter(Month == 1)

tempdata_new <- tempdata %>% 
  filter(Year >= 1960) %>% 
  select(Jan, Year)

q4_dat <- merge(tempdata_new, trend_dat_Jan, by = "Year")

plot1 <- q4_dat %>% 
  ggplot(aes(x = Jan, y = Trend)) + 
  geom_point() + 
  labs(x = "Temperature Anomaly (Degrees Celcius)", y = "Trend CO2 levels", 
       title = "Scatterplot of Trend CO2 levels vs. Temperature Anomaly (Degrees Celcius) for January from 1960-2017")

#part b 

cor.test(q4_dat$Jan, q4_dat$Trend, method = 'pearson')

#The pearson correlation coefficient is 0.8212 which is very close to 1, indicating that there is a strong correlation between the temperature anomaly and the trend co2 levels.

#part c

#The main issue with using the pearson coefficient is that it assumes that there is a linear relationship.

#Question 5

q5_dat_feb <- tempdata %>% 
  filter(Year >= 1960) %>% 
  select(Feb, Year)

trend_dat_feb <- co2_dat_new %>% 
  select(Year, Trend, Month) %>% 
  filter(Month == 2)

q5_dat <- merge(q5_dat_feb, trend_dat_feb, by = "Year")

plot2 <- q5_dat %>% 
  ggplot(aes(x = Feb, y = Trend)) + 
  geom_point() + 
  labs(x = "Temperature Anomaly (Degrees Celcius)", y = "Trend CO2 levels", 
       title = "Scatterplot of Trend CO2 levels vs. Temperature Anomaly (Degrees Celcius) for February from 1960-2017")


feb_cor <- cor.test(q5_dat$Feb, q5_dat$Trend, method = 'pearson') [4]
jan_cor <- cor.test(q4_dat$Jan, q4_dat$Trend, method = 'pearson') [4]

table(feb_cor, jan_cor)
plot_grid(plot1, plot2)

#The charts and coefficients for both Jan and Feb show that there is a 
#strong correlation between the temperature anomaly and the trend co2 levels.

#Question 6

#part a

#A spurious correlation is where variables are correlated but are not causally related to one another. 
#A correlation merely shows whether there is an association between two variables, where if one moves the other also moves either in the same or opposite direction. 
#Just because they are correlated they are not causally related, which means that there may not be a logical explanation or real-world implication for the movement. 

#part b 

#A spurious correlation with co2 levels might be; there is a strong correlation between co2 levels and literacy in a particular country. 
#Now because there is a correlation between literacy and co2, they may not be exactly causally related because we have no way of inferring whether 
#increased learning or literacy has an impact on the co2 levels itself. It might be the additional income earned and occupations attained through increased literacy 
#that might lead to increased use of natural gas, among other things. These are all hidden and is not the direct causal factor. 







