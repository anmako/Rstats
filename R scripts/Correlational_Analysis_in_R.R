

## Correlation Analysis in R

#Pearson correlation formula
#Spearman correlation formula
# Kendall correlation formula

cor(x, y, method = c("pearson", "kendall", "spearman"))
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

install.packages("corrplot")
library(corrplot)
library(RColorBrewer)

M <-cor(mtcars)

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



source("http://www.sthda.com/upload/rquery_cormat.r")
mydata <- mtcars[, c(1,3,4,5,6,7)]
require("corrplot")
rquery.cormat(mydata)