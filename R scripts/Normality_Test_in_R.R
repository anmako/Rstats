## Normality Test in R

library("dplyr")
library("ggpubr")

# Store the data in the variable my_data
my_data <- ToothGrowth


## Check your data
set.seed(1234)
dplyr::sample_n(my_data, 10)

## Visual Methods

# Density plot and Q-Q plot can be used to check normality visually.

# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.

ggdensity(my_data$len, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")

## Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.

ggqqplot(my_data$len)

library("car")
qqPlot(my_data$len)

## There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk???s test.

# Shapiro-Wilk???s method is widely recommended for normality test and it provides better power than K-S. It is based on the correlation between the data and the corresponding normal scores.

shapiro.test(my_data$len)


