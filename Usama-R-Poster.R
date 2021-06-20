library(tidyverse)
library(ggfortify)
library(GGally)
library(olsrr)


happiness <- read_csv("world-happiness-report.csv") %>% as_tibble()
str(happiness)

model <- lm(life_expectancy~Log_GDP_Capita+Social_support+Freedom_to_make_life_choices+
              Generosity+Perceptions_of_corruption, data = happiness)

summary(model)

autoplot(model)

ggpairs(happiness[,3:9])

model_selection <- ols_step_both_aic(model, details = T)

happiness2 <- happiness[-c(145,130,106),]

model2 <- lm(life_expectancy~Log_GDP_Capita+Social_support+Freedom_to_make_life_choices+
              Generosity+Perceptions_of_corruption, data = happiness2)
summary(model2)

model_selection2 <- ols_step_both_aic(model2, details = T)

autoplot(model2)


outlier_location <- sapply(happiness[,3:9],function(X){which(X%in%boxplot.stats(X)$out)})
total <- (sort(unique(unlist(outlier_location))))
happiness3 <- happiness[-total,]
model3 <- lm(life_expectancy~Log_GDP_Capita+Social_support+Freedom_to_make_life_choices+
              Generosity+Perceptions_of_corruption, data = happiness3)
summary(model3)
model_selection3 <- ols_step_both_aic(model3, details = T)
autoplot(model3)
