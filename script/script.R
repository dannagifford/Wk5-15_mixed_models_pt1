library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

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

check_model(mixed_model_slopes)

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
  
# Let's just look at subjets 1 and 3 - for subject 1 we can see the differece
# between conditions looks pretty smaall compared to the difference for 
# subject 3
subject_coefs %>%
  ggplot(aes(x = condition, y = rt, group = subject, label = subject)) +
  geom_point(colour = "grey") + 
  geom_line(colour = "grey") +
  geom_text(check_overlap = TRUE, nudge_x = .05, colour = "grey") +
  geom_point(data = filter(subject_coefs, subject == "1" | subject == "3")) +
  geom_line(data = filter(subject_coefs, subject == "1" | subject == "3")) +
  geom_text(data = filter(subject_coefs, subject == "1" | subject == "3"), 
            check_overlap = TRUE, nudge_x = .05)

# Now let's extract and plot the item intercepts and slopes
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

# Let's just look at items 1 and 3 - for these items, the reaction times for
# the `small` condition are pretty much the same - but for the `large` condition
# item 1 is about 25 ms. slower than for item 3
item_coefs %>%
  ggplot(aes(x = condition, y = rt, group = item, label = item)) +
  geom_point(colour = "grey") + 
  geom_line(colour = "grey") +
  geom_text(check_overlap = TRUE, nudge_x = .05, colour = "grey") +
  geom_point(data = filter(item_coefs, item == "1" | item == "3")) +
  geom_line(data = filter(item_coefs, item == "1" | item == "3")) +
  geom_text(data = filter(item_coefs, item == "1" | item == "3"), 
            check_overlap = TRUE, nudge_x = .05)

# 1 factorial mixed model
# first read in the data
factor_1 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/factor_1.csv")

tidied_factor_1_data <- factor_1 %>% 
  transmute(subject = factor(Subject), item = factor(Item), 
            condition = factor(Condition), gaze = Gaze)

str(tidied_factor_1_data)

tidied_factor_1_data %>%
  ggplot(aes(x = condition, y = gaze, colour = condition)) +
  geom_violin(width = .5) +
  geom_jitter(width = .1, alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  labs(x = "Condition",
       y = "Gaze Duration (ms.)") +
  guides(colour = FALSE) +
  coord_flip()

factor_1_model <- lmer(gaze ~ condition + (1 + condition | subject) + 
                         (1 + condition | item), data = tidied_factor_1_data) 

factor_1_model <- lmer(gaze ~ condition + (1 + condition | subject) + 
                         (1 | item), data = tidied_factor_1_data) 

factor_1_model <- lmer(gaze ~ condition + (1 | subject) + (1 | item), 
                       data = tidied_factor_1_data) 

check_model(factor_1_model)
