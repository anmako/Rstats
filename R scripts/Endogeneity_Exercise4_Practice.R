title: "Endogeneity_TextExercise4_Anurag"
author: "Anurag Shukla"
date: "11/5/2018"
output: word_document


## Loading the data and looking at the structure 

setwd("~/Desktop/github/Rstats/data")
library(readxl)
endogeneity <- read_xls("Endogeneity.xls")
endogeneity$nbr <- 1:nrow(endogeneity)
endogeneity$exper2 <- endogeneity$exper^2
endogeneity$age2 <- endogeneity$age^2

str(endogeneity)


# output for str(endogeneity)

summary(endogeneity)



## Use OLS to estimate the parameters of the model

(a) OLS regression 

logw=??1+??2???educ+??3???exper+??4???exper2+??5???smsa+??6???south+??

model1 <- lm(logw ~ educ + exper + exper2 + smsa + south, data = endogeneity)


  
## The 0.082 educ coefficient means that with each additional year of schooling, the log wage increases by about 0.082. 
  
  
  
(b) It is possible that there might be other variables such as social category, income, instric ability etc., which might impact both the outcome variable wages, and the independent variables such as education and experience. 
In that case, these variables would be endogenous and the OLS estimates would be biased and inconsistent. So, these estimates might not be useful. 



(c) age and age2 can be used as instruments for exper and exper2. The reason is that age works as an exogenous variable. It is related to experience as the younger one is, the less experiecne one would be. One cannot have long experience and young age at the same time. Same is true for square of age variable, which age2. 




(d) First Stage Regression 

model2 <- lm(educ ~ age + age2 + smsa + south + nearc + daded + momed, data = endogeneity)
summary(model2)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -5.652354   3.976343  -1.421 0.155277    
age          0.989610   0.278714   3.551 0.000390 ***
  age2        -0.017019   0.004838  -3.518 0.000441 ***
  smsa         0.529566   0.101504   5.217 1.94e-07 ***
  south       -0.424851   0.091037  -4.667 3.19e-06 ***
  nearc        0.264554   0.099085   2.670 0.007626 ** 
  daded        0.190443   0.015611  12.199  < 2e-16 ***
  momed        0.234515   0.017028  13.773  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.326 on 3002 degrees of freedom
Multiple R-squared:  0.2466,	Adjusted R-squared:  0.2448 
F-statistic: 140.4 on 7 and 3002 DF,  p-value: < 2.2e-16


## Conclusion: The additional instruments (age, age2, nearc, daded, and momed) are significantly correlated with the education.

Similarly, peforming the first stage regression for exper and exper2. 

model3 <- lm(exper ~ age + age2 + smsa + south + nearc + daded + momed, data=endogeneity)
summary(model3)

model4 <- lm(exper2 ~ age + age2 + smsa + south + nearc + daded + momed, data= endogeneity)
summary(model4)



(e)

endogeneity$instrumented.educ <- model2$fitted.values
endogeneity$instrumented.exper <- model3$fitted.values
endogeneity$instrumented.exper2 <- model4$fitted.values

2SLS with endogeneity correction for the education and experience: 
  
  finalmodel <- lm(logw ~ instrumented.educ + instrumented.exper + instrumented.exper2 + smsa + south, data = endogeneity)
summary(finalmodel)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)          4.4169039  0.1178606  37.476  < 2e-16 ***
  instrumented.educ    0.0998429  0.0067128  14.874  < 2e-16 ***
  instrumented.exper   0.0728669  0.0170667   4.270 2.02e-05 ***
  instrumented.exper2 -0.0016393  0.0008559  -1.915   0.0555 .  
smsa                 0.1349370  0.0171240   7.880 4.54e-15 ***
  south               -0.1589869  0.0160170  -9.926  < 2e-16 ***
  
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3925 on 3004 degrees of freedom
Multiple R-squared:  0.2192,	Adjusted R-squared:  0.2179 
F-statistic: 168.6 on 5 and 3004 DF,  p-value: < 2.2e-16

## Both the models (OLS and 2SLS) look a bit similar, and that both education and experience still have a positive effect while the squared experience still has a negative effect to logw.




(f) Sargan test for validity of the instruments

endogeneity$logw2SLS <- 4.417 + 0.100 * endogeneity$educ + 0.073 * endogeneity$exper - 0.002 * endogeneity$exper2 + 0.135 * endogeneity$smsa - 0.159 * endogeneity$south

endogeneity$res2SLS <- endogeneity$logw - endogeneity$logw2SLS

summary(endogeneity$res2SLS)


finalmodel <- lm(logw ~ instrumented.educ + instrumented.exper + instrumented.exper2 + smsa + south, data = endogeneity)

coefs2SLS <- matrix(summary(finalmodel)$coefficients[,1])

X <- cbind(1, endogeneity$educ, endogeneity$exper, endogeneity$exper2, endogeneity$smsa, endogeneity$south)

colnames(X) <- c('(Intersept)', 'educ', 'exper', 'exper2', 'smsa', 'south')

endogeneity$logw2SLS <- X %*% coefs2SLS

endogeneity$res2SLS <- endogeneity$logw - endogeneity$logw2SLS

summary(endogeneity$res2SLS)

model5 <- lm(res2SLS ~ smsa + south + age + age2 + nearc + daded + momed, data = endogeneity)

sargan.tstat <- nrow(endogeneity) * summary(model5)$r.squared

sargan.tstat

## 3.702389