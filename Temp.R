# Title: Analysis of Montreal Temperature Data

# Description: This script performs various analyses on Montreal temperature data, 
# including organizing by month,
# Calculating mean temperatures, exploring data characteristics, conducting PCA, 
# and applying linear regression.


# Implementation of the dataset in RStudio
install.packages("fda")
library(fda)
data(MontrealTemp)

# Assigning the data based on months
January <- t(MontrealTemp[, 1:31])
January

February <- t(MontrealTemp[, 32:59])
February

March <- t(MontrealTemp[, 60:90])
March

April <- t(MontrealTemp[, 91:120])
April

May <- t(MontrealTemp[, 121:151])
May

June <- t(MontrealTemp[, 152:181])
June

July <- t(MontrealTemp[, 182:212])
July

August <- t(MontrealTemp[, 213:243])
August

September <- t(MontrealTemp[, 244:273])
September

October <- t(MontrealTemp[, 274:304])
October

November <- t(MontrealTemp[, 305:334])
November

December <- t(MontrealTemp[, 335:365])
December

# Calculation of the mean temperatures in degrees Celsius for each month
mean(January)  # -10.53055
mean(February)  # -9.062605
mean(March)  # -2.627324
mean(April)  # 5.652941
mean(May)  # 12.95949
mean(June)  # 18.0148
mean(July)  # 20.83472
mean(August)  # 19.46112
mean(September)  # 14.57931
mean(October)  # 8.332827
mean(November)  # 1.699412
mean(December)  # -6.524632

# Calculation of the overall mean temperature in degrees Celsius from 1961 to 1994
total_mean <- (mean(January) + mean(February) + mean(March) + mean(April) + mean(May) + mean(June) + mean(July) + mean(August) + mean(September) + mean(October) + mean(November) + mean(December)) / 12
total_mean  # 6.065793

# Arranging in ascending order the values of the mean temperatures of each month
-10.53055
-9.062605
-6.524632
-2.627324
1.699412
5.652941
8.332827
12.95949
14.57931
18.0148
19.46112
20.83472

# Calculation of the mode, which is the most frequent value in a data series
sort(table(MontrealTemp), decreasing = TRUE)  # The mode of the sample is 18.9°C

# Calculation of the variance on the mean temperatures of different months
xi <- c(-10.53055, -9.062605, -6.524632, -2.627324, 1.699412, 5.652941, 8.332827, 12.95949, 14.57931, 18.0148, 19.46112, 20.83472)
mean(xi^2) - mean(xi)^2
variance <- sqrt(mean(xi^2) - mean(xi)^2)
variance  # 10.89001

# Data cleaning
# To find missing values:
is.na(MontrealTemp)
# The result shows that there are no missing values

# PCA
install.packages("fda")
library(fda)
install.packages("FactoMineR")
library(FactoMineR)  # to display graphics

data(MontrealTemp)

res.pca <- PCA(MontrealTemp, graph = TRUE)
res.pca
# Display eigenvalues
res.pca$eig
# Display the contribution of each variable to each axis
res.pca$var$contrib

# Linear Regression

Days <- 1:365  # Vectors for the days of the year
Temperature_1961 <- MontrealTemp[1,]
Temperature_1962 <- MontrealTemp[2,]
Temperature_1970 <- MontrealTemp[10,]
Temperature_1980 <- MontrealTemp[20,]
# Perform linear regression between the temperature variable and the Date variable
model1 <- lm(Temperature_1961 ~ Days, data = as.data.frame(MontrealTemp))
model2 <- lm(Temperature_1962 ~ Days, data = as.data.frame(MontrealTemp))
model3 <- lm(Temperature_1970 ~ Days, data = as.data.frame(MontrealTemp))
model4 <- lm(Temperature_1980 ~ Days, data = as.data.frame(MontrealTemp))

# Display the results
summary(model1)
summary(model2)
summary(model3)
summary(model4)

plot(model1, main = "Linear regression 1961")
plot(model2, main = "Linear regression 1962")
plot(model3, main = "Linear regression 1970")
plot(model4, main = "Linear regression 1980")

# For the means
Mean <- c(-10.53055, -9.062605, -6.524632, -2.627324, 1.699412, 5.652941, 8.332827, 12.95949, 14.57931, 18.0148, 19.46112, 20.83472)
Month <- 1:12
mean_model <- lm(Mean ~ Month, data = as.data.frame(MontrealTemp))
summary(mean_model)

plot(mean_model, main = "Linear regression of monthly means")
