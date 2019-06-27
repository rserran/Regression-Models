## Coursera JHU Data Science Certificate
## Data analysis practice - Regression Models

install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

devtools::install_github("jhudsl/matahari")
library(matahari)

dance_start(value = FALSE, contents = FALSE)

dim(college)

View(college)

summary(college)

college$major_category <- as.factor(college$major_category)

library(dplyr)
library(ggplot2)
library(forcats)
theme_set(theme_light())

## filter interdisciplinary major category
college <- college %>% 
        filter(major_category != "Interdisciplinary")

## boxplot major category and median (income)
g <- ggplot(college, aes(x = major_category, y = median)) + 
        geom_point(cex = 4, alpha = 0.5) + 
        geom_smooth(method = "lm", lwd = 2, colour = "blue") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

## reorder major_category by median
college %>% 
        ggplot(aes(x = reorder(major_category, median), y = median, fill = major_category)) + 
        geom_boxplot() + 
        xlab("Major Category") + 
        ylab("Income") + 
        theme(legend.position = "none") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
## linear regression for median ~ major category
fit <- lm(median ~ major_category, data = college)
summary(fit)

## p-value > 0.05 -> null hypothesis is not rejected (there is not significant
## correlation between median income and college major category)

## linear regression for median ~ major category without the intercept
fit1 <- lm(median ~ major_category - 1, data = college)
summary(fit1)

## lm without the intercept calculates the median means for each college major category

anova(fit)

## Kruskal-Wallis test
## The Kruskal-Wallis test is a nonparametric (distribution free) test and assess
## for significant differences on a continuous dependent variable by a categorical
## independent variable

## histogram median
hist(college$median, breaks = 20)

library(psych)
describe(college$median)

## skewness is 2.01 and kurtosis is 7.3 (indicating nonnormality)

## Shapiro-Wilk normality test
shapiro.test(college$median)

## The Shapiro-Wil test results in a p-value of 0. The null hypothesis is rejected
## (variable distribution is normal) in favor of the alternate (variable distribution
## is not normal)

kruskal.test(median ~ major_category, data = college)

## p-value > 0.05 -> null hypothesis is not rejected (there is not significant
## correlation between median income and college major category)

dance_save("./college_major_analysis.rds")
