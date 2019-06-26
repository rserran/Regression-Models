library(ggplot2)
library(forcats)
theme_set(theme_light())

g <- ggplot(college, aes(x = major_category, y = median)) + 
     geom_point(cex = 4, alpha = 0.5) + 
     geom_smooth(method = "lm", lwd = 2, colour = "blue")
g

college %>% 
     mutate(major_category = fct_reorder(major_category, median, fun = median)) %>% 
                 ggplot(aes(x = reorder(major_category, median), y = median, fill = major_category)) + 
                 geom_boxplot() + 
                 xlab("Major Category") + 
                 ylab("Income") + 
     theme(legend.position = "none")

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


     
     
     

     

