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

library(dplyr)
library(ggplot2)

college_income <- college %>% 
     group_by(major_category) %>% 
     summarise(median_income = median(median)) %>% 
     arrange(median_income)

g <- ggplot(college_income, aes(x = reorder(major_category, median_income), y = median_income, group = 1)) + 
     geom_line() + 
     geom_smooth(method = "lm", se = FALSE) + 
     xlab("Major Category") + 
     ylab("Income") + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

fit <- lm(median_income ~ major_category - 1, data = college_income)
summary(fit)

dance_save("./college_major_analysis.rds")
