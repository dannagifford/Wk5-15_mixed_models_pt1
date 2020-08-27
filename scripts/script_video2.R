library(tidyverse)
library(lme4) # mixed models package
library(lmerTest) # approximate p-values in mixed models
library(emmeans) # for follow up tests
library(performance) # check model assumptions
library(visdat) # visualise our data

# Let's look at a 2 x 2 design
factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/2x2.csv")

tidied_factorial_data <- factorial_data %>%
  transmute(subject = factor(Subject), item = factor(Item), RT = RT,
            context = factor(Context), sentence = factor(Sentence))

tidied_factorial_data %>%
  group_by(context, sentence) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

vis_dat(tidied_factorial_data)
vis_miss(tidied_factorial_data)

tidied_factorial_data %>%
  filter(!is.na(RT)) %>%
  group_by(context, sentence) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

tidied_factorial_data %>%
  filter(!is.na(RT)) %>%
  ggplot(aes(x = context:sentence, y = RT, colour = context:sentence)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs(x = "Context X Sentence",
       y = "RT (ms.)") +
  theme_minimal() +
  coord_flip()

ggsave("plot2.jpg")

contrasts(tidied_factorial_data$context) <- matrix(c(.5, -.5))
contrasts(tidied_factorial_data$sentence) <- matrix(c(.5, -.5))

factorial_model <- lmer(RT ~ context * sentence + 
                          (1 + context * sentence | subject) +
                          (1 + context * sentence | item), 
                        data = tidied_factorial_data)

# Simplify model

factorial_model <- lmer(RT ~ context * sentence + 
                          (1 + context + sentence | subject) +
                          (1 + context * sentence | item), 
                        data = tidied_factorial_data)

check_model(factorial_model)

summary(factorial_model)

emmeans(factorial_model, pairwise ~ context*sentence, adjust = "none")

factorial_model_gamma <- glmer(RT ~ context * sentence + 
                                 (1 | subject) +
                                 (1 | item), 
                               data = tidied_factorial_data,
                               family = Gamma)

check_model(factorial_model_gamma)

summary(factorial_model_gamma)

