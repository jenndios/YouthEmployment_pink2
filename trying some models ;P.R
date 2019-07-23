# We want to predict whether someone will work or not 

#LOAD Data 

library(tidyverse)
library(lubridate)
df <- read.csv("data/raw/teaching_training_data.csv")
view(df)

# create new data frame to make our model 

#create an age variable from the DOB variable 
df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))
df_model <- df %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(province))
df_model <- df_model %>% 
  group_by(gender, age, province) %>% 
  summarise(prob_working= sum(working)/length(working))
view(df_model)

# applying caret workshop to our project 

library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)

work <- read.csv("data/raw/teaching_training_data.csv",
                  header = TRUE)
view(work)

work <- work %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

view(work)
sapply(work, class) 


work <- mutate(work,
                age = factor(age),
                num = factor(work(num>0, 1, 0)))
summary(work)

workV2 <- work %>%
  select(numchildren, age, gender, peoplelive, province, working, unid, volunteer, leadershiprole, givemoney_yes, financial_situation_now, anygrant)

view (workV2)  

#lets remove all NAs

workV2 <- workV2 %>% 
  filter(!is.na(numchildren)) %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(peoplelive)) %>% 
  filter(!is.na(province)) %>% 
  filter(!is.na(working)) %>% 
  filter(!is.na(volunteer)) %>% 
  filter(!is.na(leadershiprole)) %>% 
  filter(!is.na(givemoney_yes)) %>% 
  filter(!is.na(financial_situation_now)) %>% 
  filter(!is.na(anygrant)) %>% 
  distinct(unid, .keep_all = TRUE)
view (workV2)

 

#### Cross Validation
# CV is a validation technique where we retrain our model on different splits of our 
# data to get an 'average performance' 
# For more information on cross validation: https://towardsdatascience.com/cross-validation-70289113a072

# To control validation techniques during training we can use the train control function
install.packages("e1071")

trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

model_rpart <- train(as.factor(working)~ numchildren + age + gender + peoplelive + province + volunteer + leadershiprole + givemoney_yes + financial_situation_now + anygrant, data=workV2, method='rpart', trControl = trControl)

model_rpart


model_ada <- train(as.factor(working)~ numchildren + age + gender + peoplelive + province + volunteer + leadershiprole + givemoney_yes + financial_situation_now + anygrant, data=workV2, method='ada', trControl = trControl)
model_ada


           