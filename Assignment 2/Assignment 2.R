# Exercise 1
# a.)
redwine = read.csv("winequality-red.csv", sep = ",", header = TRUE)


# b.)
median(redwine$quality)

mean(redwine$alcohol)


# c.)
library(ggplot2)
ggplot(redwine, aes(x = pH, y = quality)) + 
  geom_point() +  
  labs(x = "pH Level", y = "Alcohol Quality", title = "pH Level vs. Alcohol Quality") +
  theme(plot.title = element_text(hjust = 0.5))


# d.)
redwine$ALevel = " "
for (i in redwine$alcohol)
{
  if (i > 10.2)
    redwine$ALevel[which(redwine$alcohol == i)] = "High"
  else
    redwine$ALevel[which(redwine$alcohol == i)] = "Medium"
}

sulphate_to_chloride = redwine$sulphates / redwine$chlorides
redwine$ALevel = as.factor(redwine$ALevel)
ggplot(redwine, aes(x = ALevel, y = sulphate_to_chloride)) + 
  geom_boxplot() + 
  labs(y = "Sulphate to Chloride Ratio", title = "Sulphate to Chloride Ratio Boxplot") + 
  theme(plot.title = element_text(hjust = 0.5))

sum(redwine$ALevel == "High")
# There are 757 samples in the "High" category.


# e.)
ggplot(redwine, aes(x = total_sulfur_dioxide)) +
  geom_histogram() + 
  labs(x = "Total Sulfur Dioxide", y = "Count", title = "Total Sulfur Dioxide Histogram") +
  theme(plot.title = element_text(hjust = 0.5))


# f.)
ggplot(redwine, aes(x = pH, y = alcohol)) + 
  geom_point() +
  labs(x = "pH Level", y = "Alcohol Level", title = "pH Level vs. Acohol Level") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(redwine, aes(x = residual_sugar, y = alcohol)) + 
  geom_point() +
  labs(x = "Residual Sugars", y = "Alcohol Level", title = "Residual Sugars vs. Alcohol Level") +
  theme(plot.title = element_text(hjust = 0.5))

# From the two plots, it can be hypothesized that there are much more people who prefer acidic, less alcoholic and 
# less sweet wine. According to the first plot, there is an abundant amount of people who prefer an acidic wine which 
# is less alcoholic, and based on the second plot, more people preferred less sweetened wine regardless of 
# whether they preferred strong or weaker alcohol.




# Exercise 2
forest_fires = read.csv("forestfires.csv", sep = ",", header = TRUE)
forest_fires$DC = as.integer(forest_fires$DC)

# a.)
# X: Quantitative
# Y: Quantitative
# month: Qualitative
# day: Qualitative
# FFMC: Quantitative
# DMC: Quantitative
# DC: Quantitative
# ISI: Quantitative
# temp: Quantitative
# RH: Quantitative
# wind: Quantitative
# rain: Quantitative
# area: Quantitative


# b.)
predictor_r = c("X min", "X max", "Y min", "Y max", "FFMC min", "FFMC max", "DMC min", "DMC max", "DC min", "DC max",
                "ISI min", "ISI max", "temp min", "temp max", "RH min", "RH max", "wind min", "wind max", "rain min", 
                "rain max", "area min", "area max")

range = c(range(forest_fires$X), range(forest_fires$Y), range(forest_fires$FFMC), range(forest_fires$DMC), 
          range(forest_fires$DC), range(forest_fires$ISI), range(forest_fires$temp), range(forest_fires$RH),
          range(forest_fires$wind), range(forest_fires$rain), range(forest_fires$area))
ranges = data.frame(predictor_r, range)


predictor = c("X", "Y", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area")

mean = c(mean(forest_fires$X), mean(forest_fires$Y), mean(forest_fires$FFMC), mean(forest_fires$DMC), 
         mean(forest_fires$DC), mean(forest_fires$ISI), mean(forest_fires$temp), mean(forest_fires$RH),
         mean(forest_fires$wind), mean(forest_fires$rain), mean(forest_fires$area))
means = data.frame(predictor, mean)

standard_deviation = c(sd(forest_fires$X), sd(forest_fires$Y), sd(forest_fires$FFMC), sd(forest_fires$DMC), 
                       sd(forest_fires$DC), sd(forest_fires$ISI), sd(forest_fires$temp), sd(forest_fires$RH),
                       sd(forest_fires$wind), sd(forest_fires$rain), sd(forest_fires$area))
stand_devs = data.frame(predictor, standard_deviation)


week = c(sum(forest_fires$day == "mon"), sum(forest_fires$day == "tue"), sum(forest_fires$day == "wed"),
         sum(forest_fires$day == "thu"), sum(forest_fires$day == "fri"), sum(forest_fires$day == "sat"),
         sum(forest_fires$day == "sun"))

# Sunday has the most frequent fires out of all the days.


# c.)
new_range = c(range(forest_fires$X[c(1:39, 81:517)]), range(forest_fires$Y[c(1:39, 81:517)]), 
              range(forest_fires$FFMC[c(1:39, 81:517)]), range(forest_fires$DMC[c(1:39, 81:517)]), 
              range(forest_fires$DC[c(1:39, 81:517)]), range(forest_fires$ISI[c(1:39, 81:517)]), 
              range(forest_fires$temp[c(1:39, 81:517)]), range(forest_fires$RH[c(1:39, 81:517)]),
              range(forest_fires$wind[c(1:39, 81:517)]), range(forest_fires$rain[c(1:39, 81:517)]), 
              range(forest_fires$area[c(1:39, 81:517)]))
new_ranges = data.frame(predictor_r, new_range)

new_mean = c(mean(forest_fires$X[c(1:39, 81:517)]), mean(forest_fires$Y[c(1:39, 81:517)]), 
             mean(forest_fires$FFMC[c(1:39, 81:517)]), mean(forest_fires$DMC[c(1:39, 81:517)]), 
             mean(forest_fires$DC[c(1:39, 81:517)]), mean(forest_fires$ISI[c(1:39, 81:517)]), 
             mean(forest_fires$temp[c(1:39, 81:517)]), mean(forest_fires$RH[c(1:39, 81:517)]),
             mean(forest_fires$wind[c(1:39, 81:517)]), mean(forest_fires$rain[c(1:39, 81:517)]), 
             mean(forest_fires$area[c(1:39, 81:517)]))
new_means = data.frame(predictor, new_mean)

new_standard_deviation = c(sd(forest_fires$X[c(1:39, 81:517)]), sd(forest_fires$Y[c(1:39, 81:517)]), 
                           sd(forest_fires$FFMC[c(1:39, 81:517)]), sd(forest_fires$DMC[c(1:39, 81:517)]), 
                           sd(forest_fires$DC[c(1:39, 81:517)]), sd(forest_fires$ISI[c(1:39, 81:517)]), 
                           sd(forest_fires$temp[c(1:39, 81:517)]), sd(forest_fires$RH[c(1:39, 81:517)]),
                           sd(forest_fires$wind[c(1:39, 81:517)]), sd(forest_fires$rain[c(1:39, 81:517)]), 
                           sd(forest_fires$area[c(1:39, 81:517)]))
new_stand_devs = data.frame(predictor, new_standard_deviation)


# d.)
filter_data = forest_fires[(forest_fires$wind > 4), ]$month
forest_fires_new = data.frame(filter_data)

ggplot(forest_fires_new, aes(x = filter_data)) + 
  geom_bar() + 
  labs(x = "Month", y = "Count", title = "Forest Fire Frequency (Wind > 4)") +
  theme(plot.title = element_text(hjust = 0.5))

# August has the highest frequency of forest fires with wind > 4.


# e.)
ggplot(forest_fires, aes(x = DMC, y = ISI)) + 
  geom_point() + 
  labs(x = "DMC", y = "ISI", title = "DMC vs. ISI") +
  geom_smooth(method = 'lm') +
  theme(plot.title = element_text(hjust = 0.5))

correlation_matrix = matrix(0, nrow = length(forest_fires$DMC))
correlation_matrix[,1] = correlation_matrix[,1] + 1:517
correlation_matrix = cbind(correlation_matrix, forest_fires$DMC)
correlation_matrix = cbind(correlation_matrix, forest_fires$ISI)
colnames(correlation_matrix) = c("No.", "DMC", "ISI")


# f.)
# In order to predict wind speed based on the other variables, pairing it with
# FFMC, DMC, ISI,temp, and/or RH would be useful. This is because creating a plot
# with both wind speed and one other variables shows a set of points all relating to a 
# wind speed staying constant regardless of the other variable at random. In turn,
# this allows us to better understand the wind speed as it may stay constant for several 
# points of the other variable, or it might change.

