---
  title: "Text_Exercise_3"
author: "Anurag Shukla"
date: "10/30/2018"
output: html_document
---
  
  # Text Exercise - 3 
  
  # Loading the data
  
  setwd("~/Desktop/github/Rstats/data")
library(readxl)
modspec <- read_xlsx("Modspec.xlsx")

# Exploring the data

str(modspec)
summary(modspec)


OBS               INTRATE            INFL             PROD              UNEMPL       
Length:660         Min.   : 0.070   Min.   :-1.959   Min.   :-15.0610   Min.   :-4.9497  
Class :character   1st Qu.: 3.000   1st Qu.: 1.954   1st Qu.:  0.7964   1st Qu.: 0.6991  
Mode  :character   Median : 5.220   Median : 3.173   Median :  3.3541   Median : 2.0054  
Mean   : 5.348   Mean   : 3.920   Mean   :  2.9246   Mean   : 1.7747  
3rd Qu.: 7.122   3rd Qu.: 4.729   3rd Qu.:  5.9683   3rd Qu.: 3.0386  
Max.   :19.100   Max.   :14.592   Max.   : 13.3821   Max.   : 5.6352  
COMMPRI             PCE            PERSINC            HOUST        
Min.   :-73.529   Min.   :-3.368   Min.   :-4.9073   Min.   :-54.797  
1st Qu.:-16.926   1st Qu.: 5.031   1st Qu.: 0.9854   1st Qu.:-11.180  
Median :  1.609   Median : 6.518   Median : 2.1287   Median :  1.472  
Mean   :  4.694   Mean   : 6.853   Mean   : 2.1392   Mean   :  1.809  
3rd Qu.: 20.859   3rd Qu.: 8.876   3rd Qu.: 3.2992   3rd Qu.: 13.327  
Max.   :241.667   Max.   :13.574   Max.   : 7.3561   Max.   : 96.189  



# (a) Use general-to-specific to come to a model. Start by regressing the federal funds rate on the other 7 variables and eliminate 1 variable at a time.

(i) 

model1 <- lm(INTRATE ~ INFL + PROD + UNEMPL + COMMPRI + PCE + PERSINC + HOUST, data = modspec)
summary(model1)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.221161   0.244995  -0.903   0.3670    
INFL         0.696059   0.062229  11.185  < 2e-16 ***
  PROD        -0.057743   0.039900  -1.447   0.1483    
UNEMPL       0.102481   0.096757   1.059   0.2899    
COMMPRI     -0.005521   0.002974  -1.857   0.0638 .  
PCE          0.344380   0.069455   4.958 9.08e-07 ***
  PERSINC      0.246999   0.060590   4.077 5.13e-05 ***
  HOUST       -0.019411   0.004672  -4.155 3.68e-05 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.188 on 652 degrees of freedom
Multiple R-squared:  0.6385,	Adjusted R-squared:  0.6346 
F-statistic: 164.5 on 7 and 652 DF,  p-value: < 2.2e-16

(ii) Removing the UNEMPL variable from the model as it is not significant and it is the variable with the highest value.

model1 <- lm(INTRATE ~ INFL + PROD + COMMPRI + PCE + PERSINC + HOUST, data = modspec )
summary(model1)

> model1 <- lm(INTRATE ~ INFL + PROD + COMMPRI + PCE + PERSINC + HOUST, data = modspec )
> summary(model1)

Call:
  lm(formula = INTRATE ~ INFL + PROD + COMMPRI + PCE + PERSINC + 
       HOUST, data = modspec)

Residuals:
  Min      1Q  Median      3Q     Max 
-7.5322 -1.4982 -0.1005  1.3882  7.6954 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.290851   0.236016  -1.232   0.2183    
INFL         0.693309   0.062180  11.150  < 2e-16 ***
  PROD        -0.025460   0.025752  -0.989   0.3232    
COMMPRI     -0.006514   0.002822  -2.308   0.0213 *  
  PCE          0.368561   0.065602   5.618 2.86e-08 ***
  PERSINC      0.251581   0.060441   4.162 3.57e-05 ***
  HOUST       -0.021023   0.004417  -4.760 2.39e-06 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.188 on 653 degrees of freedom
Multiple R-squared:  0.6379,	Adjusted R-squared:  0.6346 
F-statistic: 191.7 on 6 and 653 DF,  p-value: < 2.2e-16

(iii) 
Leaving PROD variable, all other variables are significant. So we can remove the PROD variable from the model 

model1 <- lm(INTRATE ~ INFL + COMMPRI + PCE + PERSINC + HOUST, data = modspec )
summary(model1)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.240119   0.230366  -1.042  0.29764    
INFL         0.717527   0.057152  12.555  < 2e-16 ***
  COMMPRI     -0.007501   0.002640  -2.841  0.00464 ** 
  PCE          0.340525   0.059156   5.756 1.32e-08 ***
  PERSINC      0.240242   0.059342   4.048 5.77e-05 ***
  HOUST       -0.020530   0.004389  -4.678 3.52e-06 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.188 on 654 degrees of freedom
Multiple R-squared:  0.6374,	Adjusted R-squared:  0.6346 
F-statistic: 229.9 on 5 and 654 DF,  p-value: < 2.2e-16

After removing UNEMPL and PROD, all other variables in the given model are significant, we can go ahead with this model. 

Calculating the AIC and BIC for the final model 

s <- sqrt(deviance(model1)/df.residual(model1))
k <- length(model1$coefficients) -1 
n <- nrow(modspec)
AIC <- log(s^2) + 2 * k /n = 1.580988
BIC <- log(s^2) +  k * log(n) / n = 1.61502





# (b) From specific to general model 

Round 1: R-sqaure value - 0.5591 
model1 <- lm(INTRATE ~ INFL, data = modspec )
Round 2: R-sqaure value - 0.5734 
model1 <- lm(INTRATE ~ INFL + PROD, data = modspec )
Round 3: R-sqaure value - 0.5935
model1 <- lm(INTRATE ~ INFL + PROD + UNEMPL, data = modspec )
Round 4: R-sqaure value - 0.5952
model1 <- lm(INTRATE ~ INFL + PROD + UNEMPL + COMMPRI, data = modspec )
Round 5: R-sqaure value -  0.6147 
model1 <- lm(INTRATE ~ INFL + PROD + UNEMPL + COMMPRI + PCE, data = modspec )
Round 6: R-sqaure value - 0.6255 
model1 <- lm(INTRATE ~ INFL + PROD + UNEMPL + COMMPRI + PCE + PERSINC, data = modspec )
Round 7: R-sqaure value - 0.6346 
model1 <- lm(INTRATE ~ INFL + PROD + UNEMPL + COMMPRI + PCE + PERSINC + HOUST, data = modspec )

As the variables UNEMPL and PROD have become insignificant, we can try removing these variables from the model and then see whether it impacts the overall R-square of the model. 

model1 <- lm(INTRATE ~ INFL + COMMPRI + PCE + PERSINC + HOUST, data = modspec )

As the adjusted R-square and multiple R-square for this model are 0.6374 and 0.6346 and removal for two variables did not impact the adjusted R-square, we can go ahead with the model. 

Final outcome is same as question (a). AIC and BIC of the model remain the same. 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.240119   0.230366  -1.042  0.29764    
INFL         0.717527   0.057152  12.555  < 2e-16 ***
  COMMPRI     -0.007501   0.002640  -2.841  0.00464 ** 
  PCE          0.340525   0.059156   5.756 1.32e-08 ***
  PERSINC      0.240242   0.059342   4.048 5.77e-05 ***
  HOUST       -0.020530   0.004389  -4.678 3.52e-06 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.188 on 654 degrees of freedom
Multiple R-squared:  0.6374,	Adjusted R-squared:  0.6346 
F-statistic: 229.9 on 5 and 654 DF,  p-value: < 2.2e-16





# (c) Compare your model from (a) and the Taylor rule of equation (1). Consider R2, AIC and BIC. Which of the models do you prefer?

Comparing the model obtained in (a) with the Taylor rule of equation (1)

Taylor rule of equation

modelTaylor <- lm(INTRATE ~ INFL + PROD, data = modspec)
summary(modelTaylor)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.24890    0.17619   7.088 3.51e-12 ***
  INFL         0.97498    0.03273  29.785  < 2e-16 ***
  PROD         0.09472    0.01971   4.805 1.92e-06 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.364 on 657 degrees of freedom
Multiple R-squared:  0.5747,	Adjusted R-squared:  0.5734 
F-statistic: 443.9 on 2 and 657 DF,  p-value: < 2.2e-16

Calculating AIC and BIC for the Taylor rule of equation model. 

> s <- sqrt(deviance(modelTaylor)/df.residual(modelTaylor))
> k <- length(modelTaylor$coefficients) -1 
> n <- nrow(modspec)
> AIC <- log(s^2) + 2 * k /n 
> BIC <- log(s^2) +  k * log(n) / n 

AIC and BIC values are 1.726704 and 1.740316 respectively. 

While comparing models, a lower values of AIC and BIC model is preferred. Hence, we perfer reducted model obtained in (a). Reduced Model (a) also has the greater R-square than the Taylor rule of Equation. 




# (d) Test the Taylor rule of equation (1) using the RESET test, Chow break and forecast test (with in both tests as break date January 1980) and a Jarque-Bera test. What do you conclude?

Taylor rule of equation is given as below: 
  modelTaylor <- lm(INTRATE ~ INFL + PROD, data = modspec)

## RESET testing

library(lmtest)
resettest(modelTaylor, power = 2, type = "fitted", data = modspec)

RESET test

data:  modelTaylor
RESET = 2.5371, df1 = 1, df2 = 656, p-value = 0.1117

## chow break testing

install.packages("gap")
library(gap)

grp <- modspec[modspec < 1980, ]
x1 <- grp[, c("INFL", "PROD")]; y1 <- data.frame( INTRATE = grp["INTRATE"] )
grp <- modspec[modspec$Year >= 1980, ]
x2 <- grp[, c("INFL", "PROD")]; y2 <- data.frame( INTRATE = grp["INTRATE"] )

# chow.test
chow.test(y1, x1, y2, x2)

# final results for question (d) are following: 


Test name	        Test statistic	      p-value	    F distribution     	Test result
RESET	            F= 2,537	            0.112	      F(1/656)	          H0: Not rejected
Chow-break	      F= 28,735	            0.000	      F(3/654)	          H0: Rejected
Chow-forecast	    F= 5,511	            0.000	      F(420/237)	        H0: Rejected
Jarque-Bera	      JB= 12,444	          0.002	      X2(2)	              H0: Rejected


