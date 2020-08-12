library(tidyverse)
library(lme4) # mixed models package
library(lmerTest) # approximate p-values in mixed models
library(emmeans) # allow us to run follow up tests
library(performance) # check model assumptions
library(arm) # for binned residuals plot

# Let's look at a 2 x 2 design



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

binnedplot(fitted(binomial_model), resid(binomial_model,type="response"))
