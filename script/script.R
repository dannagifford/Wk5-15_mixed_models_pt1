library(tidyverse)
library(lme4)
library(emmeans)

# Is height predicted by gender?
# Create gender_height_data
subject <- seq(1:8)
gender <- factor(c(rep("male", 4), rep("female", 4)))
height <- c(170, 180, 175, 185, 160, 170, 165, 165)
gender_height_data <- tibble(subject, gender, height)

# fit a linear model where gender is used to predict height
height_model <- lm(height ~ gender, data = gender_height_data)
summary(height_model)

gender_height_data %>%
  ggplot(aes(x = gender, y = height, group = 1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Is height predicted by age?
# create age_height_data
age <- c(22, 21, 19, 23, 15, 17, 16, 17)
age_height_data <- tibble(subject, age, height)

age_model <- lm(height ~ age, data = age_height_data)
summary(age_model)
