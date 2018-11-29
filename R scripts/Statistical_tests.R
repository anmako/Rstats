## Basic Statistical Tests Explained with R

## (1) Shapiro Test: Testing for normality

normaly_disb <- rnorm(100, mean = 5, sd = 1)
shapiro.test(normaly_disb)

not_normally_dist <- runif(100)
shapiro.test(not_normally_dist)

## If p-Value is less than the significance level (0.05), the null-hypothesis that it is normally distributed can be rejected.


## (2) One Sample t-test

set.seed(100)
x <- rnorm(50, mean = 10, sd = 0.5)
t.test(x, mu = 10)

## In above case, the p-Value is not less than significance level of 0.05, therefore the null hypothesis that the mean=10 cannot be rejected. Also note that the 95% confidence interval range includes the value 10 within its range.


## (3) Wilcoxon Signed Rank Test

# To test the mean of a sample when normal distribution is not assumed. Wilcoxon signed rank test can be an alternative to t-Test, especially when the data sample is not assumed to follow a normal distribution. It is a non-parametric method used to test if an estimate is different from its true value.
numeric_vector <- c(20, 29, 24, 19, 20, 22, 28, 23, 19, 19)
wilcox.test(numeric_vector, mu=20, conf.int = TRUE)


## (4) Two Sample t-Test and Wilcoxon Rank Sum Test

# Both t.Test and Wilcoxon rank test can be used to compare the mean of 2 samples. The difference is t-Test assumes the samples being tests is drawn from a normal distribution, while, Wilcoxon???s rank sum test does not.

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

wilcox.test(x, y, alternative = "g")


t.test(1:10, y = c(7:20)) 

#What if we want to do a 1-to-1 comparison of means for values of x and y?

t.test(x, y, paired = TRUE) 
wilcox.test(x, y, paired = TRUE)



## (5) Shapiro Test
# To test if a sample follows a normal distribution

set.seed(100)
normaly_disb <- rnorm(100, mean=5, sd=1)
shapiro.test(normaly_disb)

set.seed(100)
not_normaly_disb <- runif(100)  #uniform distribution
shapiro.test(not_normaly_disb)

## (6) Kolmogorov And Smirnov Test
#Kolmogorov-Smirnov test is used to check whether 2 samples follow the same distribution.

x <- rnorm(50)
y <- runif(50)
ks.test(x, y)

x <- rnorm(50)
y <- rnorm(50)
ks.test(x, y)


## (7) Fisher???s F-Test
#Fisher???s F test can be used to check if two samples have same variance.

x <- rnorm(50)
y <- runif(50)
var.test(x, y)

x <- rnorm(50)
y <- rnorm(50)
var.test(x, y)


## (8) Chi Squared Test
#Chi-squared test in R can be used to test if two categorical variables are dependent, by means of a contingency table.
#Example use case: You may want to figure out if big budget films become box-office hits. We got 2 categorical variables (Budget of film, Success Status) each with 2 factors (Big/Low budget and Hit/Flop), which forms a 2 x 2 matrix.


chisq.test(table(categorical_X, categorical_Y), correct = FALSE)
summary(table(categorical_X, categorical_Y))

#There are two ways to tell if they are independent:

#By looking at the p-Value: If the p-Value is less that 0.05, we fail to reject the null hypothesis that the x and y are independent. So for the example output above, (p-Value=2.954e-07), we reject the null hypothesis and conclude that x and y are not independent.

#From Chi.sq value: For 2 x 2 contingency tables with 2 degrees of freedom (d.o.f), if the Chi-Squared calculated is greater than 3.841 (critical value), we reject the null hypothesis that the variables are independent. To find the critical value of larger d.o.f contingency tables, use qchisq(0.95, n-1), where n is the number of variables.


## (9) Correlation
#To test the linear relationship of two continuous variables

cor.test(cars$speed, cars$dist)

## (10) More Commonly Used Tests
fisher.test(contingencyMatrix, alternative = "greater")  # Fisher's exact test to test independence of rows and columns in contingency table
friedman.test()  # Friedman's rank sum non-parametric test 

#The package lawstat has a good collection. The outliers package has a number of test for testing for presence of outliers.

install.packages("lawstat")
library(lawstat)






