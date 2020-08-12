library(tidyverse)
library(lme4)
library(lmerTest)
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
# Create age_height_data
age <- c(22, 21, 19, 23, 15, 17, 16, 17)
age_height_data <- tibble(subject, age, height)

age_model <- lm(height ~ age, data = age_height_data)
summary(age_model)

age_height_data %>%
  ggplot(aes(x = age, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Linear mixed models
# Create mixed_model_data - 10 subject, 10 items, 2 conditions, with 4 repeats
# just to create a totally fictitious data set that allows the models to be built
# We use set.seed() to ensure the random numbers in rt can be reproduced
subject <- factor(c(rep(1:10, each = 5), rep(1:10, each = 5)))
condition <- factor(c(rep("small", times = 50), rep("large", times = 50)))
item <- factor(rep(seq(1:5), times = 20))
set.seed(9999)
rt1 <- as.integer(rnorm(50, 800, 100))
set.seed(1234)
rt2 <- as.integer(rnorm(50, 900, 100))
rt <- c(rt1, rt2)

mixed_model_data <- tibble(subject, item, condition, rt)

mixed_model_data <- rbind(mixed_model_data, mixed_model_data, 
                          mixed_model_data, mixed_model_data)

mixed_model_data %>% 
  group_by(condition) %>%
  summarise(mean_rt = mean(rt), sd_rt = sd(rt))

# Build our mixed model
mixed_model <- lmer(rt ~ condition + (1 | subject) + (1 | item), 
                    data = mixed_model_data)
summary(mixed_model)

# Testing the fixed effect using Likelihood Ratio Test
# Build a new model dropping the fixed effect we want to test
mixed_model_null <- lmer(rt ~ (1 | subject) + (1 | item), 
                         data = mixed_model_data)
anova(mixed_model, mixed_model_null)

coef(mixed_model)

# Let's now add slopes for our two random effects
mixed_model_slopes <- lmer(rt ~ condition + (1 + condition | subject)
                           + (1 + condition | item), data = mixed_model_data)

# Let's plot the intercepts and slopes where the intercept corresponds to 
# the large condition level, and the slope is the difference between it and
# the small condition level
# First by subjects
subject_intercepts <- coef(mixed_model_slopes)$subject[1]
subject_slopes <- coef(mixed_model_slopes)$subject[2]

subject_coefs <- tibble(subject = c(seq(1:10), seq(1:10)),  
                        condition = c(rep("large", times = 10), 
                                      rep("small", times = 10)),
                        rt = unlist(rbind(subject_intercepts, 
                                   (subject_intercepts + subject_slopes))))

subject_coefs %>%
  ggplot(aes(x = condition, y = rt, group = subject, label = subject)) +
  geom_point() + 
  geom_line() +
  geom_text(check_overlap = TRUE, nudge_x = .05)
  
subject_coefs %>%
  filter(subject == "1" | subject == "3") %>%
  ggplot(aes(x = condition, y = rt, group = subject, label = subject)) +
  geom_point() + 
  geom_line() +
  geom_text(check_overlap = TRUE, nudge_x = .05)

item_intercepts <- coef(mixed_model_slopes)$item[1]
item_slopes <- coef(mixed_model_slopes)$item[2]

item_coefs <- tibble(item = c(seq(1:5), seq(1:5)),  
                        condition = c(rep("large", times = 5), 
                                      rep("small", times = 5)),
                        rt = unlist(rbind(item_intercepts, 
                                          (item_intercepts + item_slopes))))

item_coefs %>%
  ggplot(aes(x = condition, y = rt, group = item, label = item)) +
  geom_point() + 
  geom_line() +
  geom_text(check_overlap = TRUE, nudge_x = .05)

item_coefs %>%
  filter(item == "2" | item == "5") %>%
  ggplot(aes(x = condition, y = rt, group = item, label = item)) +
  geom_point() + 
  geom_line() +
  geom_text(check_overlap = TRUE, nudge_x = .05)
