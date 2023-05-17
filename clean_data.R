install.packages("haven")
install.packages("dplyr")
install.packages("car")
install.packages("emmeans")
install.packages("effectsize")
install.packages("tidyr")
install.packages("broom")
install.packages("ggplot2")


## Load packages ##
library(haven)
library(dplyr)
library(car)
library(tidyr)
library(broom)
library(ggplot2)

## Input ##
cito <- read_sav("data/PTW+CITO+evaluatie+en+focus_May+5,+2023_14.47.sav")
rm(cito_removeNA)

## Transformation ##
# Replace NA with 0 in CITO_DO_CITO_metnorm and CITO_DO_CITO
cito$CITO_DO_CITO_metnorm <- ifelse(is.na(cito$CITO_DO_CITO_metnorm),0,cito$CITO_DO_CITO_metnorm)
cito$CITO_DO_CITO <- ifelse(is.na(cito$CITO_DO_CITO),0,cito$CITO_DO_CITO)

# Remove missing value of answers to questions.
cito_removeNA <- cito %>% 
  filter (!is.na(cito$eval_training_1))
         
# Create dummy variable for treatment and control group
cito_removeNA <- cito_removeNA %>% mutate(condition = ifelse(CITO_DO_CITO_metnorm == "1",1,0))

# Calculate average of 2 items measuring whether taking training is evaluated negative to positive. (1-9 scale)
cito_removeNA <- cito_removeNA %>% mutate (mean_neg_pos = (eval_training_1 + eval_training_2 )/2)

# Run analysis for research question 1
model1 = lm(mean_neg_pos ~ condition, data = cito_removeNA)
tidy_model1 <- tidy(model1)

# Run analysis for research question 2
model2 = lm(eval_child_1 ~ condition, data = cito_removeNA)
tidy_model2 <- tidy(model2)

# Run analysis for research question 3
model3 = lm(eval_child_2 ~ condition, data = cito_removeNA)
tidy_model3 <- tidy(model3)

# Combine the tidy outputs into one table
combined_table <- bind_rows(
  "Research Question 1" = tidy_model1,
  "Research Question 2" = tidy_model2,
  "Research Question 3" = tidy_model3
)

