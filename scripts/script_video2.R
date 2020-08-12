library(tidyverse)
library(lme4) # mixed models package
library(lmerTest) # approximate p-values in mixed models
library(emmeans) # allow us to run follow up tests
library(performance) # check model assumptions
library(arm) # for binned residuals plot

# Let's look at a 2 x 2 design
factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/2x2.csv")

tidied_factorial_data <- factorial_data %>%
  transmute(subject = factor(Subject), item = factor(Item), RT = RT,
            context = factor(Context), sentence = factor(Sentence))

tidied_factorial_data %>%
  group_by(sentence, context) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

tidied_factorial_data %>%
  filter(!is.na(RT)) %>%
  group_by(sentence, context) %>%
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

tidied_factorial_data %>%
  filter(!is.na(RT)) %>%
  ggplot(aes(x = sentence:context, y = RT, colour = sentence:context)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE) +
  labs(x = "Sentence X Context",
       y = "RT (ms.)") +
  theme_minimal() +
  coord_flip()

contrasts(tidied_factorial_data$context) <- matrix(c(.5, -.5))
contrasts(tidied_factorial_data$sentence) <- matrix(c(.5, -.5))

factorial_model <- lmer(RT ~ context * sentence + 
                          (1 + context * sentence | subject) +
                          (1 + context * sentence | item), 
                        data = tidied_factorial_data)

#### ADD FUNCTION TO SIMPLIFY MODEL ####

check_model(factorial_model)

confint(factorial_model)

emmeans(factorial_model, pairwise ~ context*sentence, adjust = "none")

factorial_model_gamma <- glmer(RT ~ context * sentence + 
                                 (1 | subject) +
                                 (1 | item), 
                               data = tidied_factorial_data,
                               family = Gamma)

check_model(factorial_model_gamma)

summary(factorial_model_gamma)

# Let's look at some binomial data
regressions_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/regressions.csv")

str(regressions_data)

tidied_regressions_data <- regressions_data %>%
  transmute(subject = factor(Subject), item = factor(Item), 
            condition = factor(Condition), DV = DV)

tidied_regressions_data %>%
  group_by(condition) %>%
  summarise(mean_DV = mean(DV))

binomial_model <- glmer(DV ~ condition + (1 + condition | subject) +
                        (1 + condition | item), data = tidied_regressions_data,
                        family = binomial)
# Need to simplify model

binomial_model <- glmer(DV ~ condition + (1 | subject), 
                        data = tidied_regressions_data,
                        family = binomial)

summary(binomial_model)

binomial_model_null <- glmer(DV ~ (1 | subject), 
                        data = tidied_regressions_data,
                        family = binomial)

anova(binomial_model, binomial_model_null)

# We would expect 95# of residuals to fall between jagged line (+- 2SEs)
binnedplot(fitted(binomial_model), resid(binomial_model,type="response"))
