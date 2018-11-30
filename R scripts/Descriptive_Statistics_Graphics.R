## Descriptive Statistics and Graphics

my_data <- iris

# Compute the mode
# install.packages("modeest")
library(modeest)
mfv(my_data$Sepal.Length)

#sapply() function

# Compute the mean of each column
sapply(my_data[, -5], mean)

# Compute quartiles
sapply(my_data[, -5], quantile)


## stat.desc() function

install.packages("pastecs")
library(pastecs)

# Compute descriptive statistics
library(pastecs)
res <- stat.desc(my_data[, -5])
round(res, 2)

## Installation and loading ggpubr

install.packages("ggpubr")
library(ggpubr)

## Box plots

ggboxplot(my_data, y= "Sepal.Length", width = 0.5)


## Histogram 

gghistogram(my_data, x = "Sepal.Length", bins = 9, 
            add = "mean")

## Empirical cumulative distribution function (ECDF)

ggecdf(my_data, x = "Sepal.Length")


## Q-Q plots

#QQ plots is used to check whether the data is normally distributed.

ggqqplot(my_data, x = "Sepal.Length")

 ## Descriptive statistics by groups

library(dplyr)

group_by(my_data, Species) %>% 
  summarise(
    count = n(), 
    mean = mean(Sepal.Length, na.rm = TRUE),
    sd = sd(Sepal.Length, na.rm = TRUE)
  )

## Graphics for grouped data:

library("ggpubr")
# Box plot colored by groups: Species
ggboxplot(my_data, x = "Species", y = "Sepal.Length",
          color = "Species",
          palette = c("#00AFBB", "#E7B800", "#FC4E07"))


# Stripchart colored by groups: Species
ggstripchart(my_data, x = "Species", y = "Sepal.Length",
             color = "Species",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             add = "mean_sd")


## Frequency tables 

# Hair/eye color data
df <- as.data.frame(HairEyeColor)
hair_eye_col <- df[rep(row.names(df), df$Freq), 1:3]
rownames(hair_eye_col) <- 1:nrow(hair_eye_col)
head(hair_eye_col)




