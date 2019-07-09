# METADATA ====
# Description: Data wrangling and visualisation of youth data
# Created: 2019-06-30 (Neil Rankin)
# Updated: 2019-07-04 (Neil Rankin)
# Reviewed: NA

# SUMMARY: Script for wrangling and visualisation exercises on Employment Challenge data



# RESOURCES

# dplyr (tidyverse) https://dplyr.tidyverse.org/articles/dplyr.html
# ggplot (also tidyverse) https://ggplot2.tidyverse.org/
# lubridate https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html



# INITIALISE ====

#> Script-secific libraries ----

# Here is where we load libraries

#lubridate - times and dates
library(lubridate)
#tidyverse - graphing and deplyr
library(tidyverse)


# LOAD DATA ====
#df is data frame, read.csv() will load that data 
df <- read.csv("data/raw/teaching_training_data.csv")

# Notice the path (it is machine independent)

# Look at the data - what do we see?

table(df$gender)

<<<<<<< HEAD
#df is original data
#wanna see everyone once, so used distinct to find individuals who have a distinct id and gender
unique_individuals <- df %>% 
  distinct(unid, gender)

table(unique_individuals$gender)
=======
unique_individuals <- df %>% 
  distinct(unid, gender)
>>>>>>> 13dd72e042d39c924dc984ad28c8c985b20a5400

# WRANGLING & FEATURE ENGINEERING ====

# Often an iterative process:
#   - Look at data
#   - Create feature
#   - Look again

# View data

view(df)


# Start with communication score

table(df$com_score)

# ggplot

ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score))

# explanation of what each component is


# How does this differ by gender


ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score, fill = gender))


ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score, fill = gender), position = "dodge")

# what can we learn from this figure?


# Now age

# How would we calculate this?

# introducing `mutate` and some lubridate functions

#mutate creates a new column and interval is from lubridate, special date subtraction
df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

# see lubridate link (https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)


ggplot(data = df) + 
  geom_bar(mapping = aes(x = age))

# How does this differ by gender

#default is to stack the box, if i "dodge" it, boxes go side by side
ggplot(data = df) + 
  geom_bar(mapping = aes(x = age, fill = gender), position = "dodge")

# what does `dodge` do?


# A different approach

#geom_density is like geom_smooth but it makes the middle more dense
#age at survey includes the age with the month
ggplot(data = df) + 
  geom_density(mapping = aes(x = age_at_survey, fill = gender), alpha = 0.3)

# NAs and older people cloud our view

# tidyverse::filter
df_without_gender_NAs <- df %>% 
  filter(!is.na(gender))

ggplot(data = df_without_gender_NAs) + 
  geom_density(mapping = aes(x = age, colour = gender), alpha = 0.3) + 
  xlim(15, 35)


# group_by
# we might want to count the number of surveys people have done

df <- df %>% 
  group_by(unid) %>% 
  mutate(total_surveys = max(survey_num)) %>% 
  ungroup()

<<<<<<< HEAD
#summarise bigger data frame and compresses it by the groupby variable, so for this
#num of indivduals, wanna look one by one, so groub by indiv, and transforms each indiv to have one row

=======
>>>>>>> 13dd72e042d39c924dc984ad28c8c985b20a5400
df_unid <- df %>% 
  group_by(unid) %>% 
  summarise(total_surveys = max(survey_num)) %>% 
  ungroup()

<<<<<<< HEAD
=======

>>>>>>> 13dd72e042d39c924dc984ad28c8c985b20a5400
# Always good practise to ungroup() since later you might forget that you have grouped and operations willbe affected by it



# create other data frames to create descriptive stats (and other things)

df_gender_cft <- df_without_gender_NAs %>% 
  group_by(gender) %>% 
  filter(!is.na(cft_score)) %>% 
  summarise(cft_mean = mean(cft_score), nobs = n())



#let us group by province
unique_individuals_by_id <- df %>% 
  distinct(unid, province)

count_by_province <- data.frame(table(unique_individuals_by_id$province))
  
ggplot(data = unique_individuals_by_id) +
  geom_bar(mapping = aes(x=province, fill=max(count_by_province$Freq)))


#average min and max monthly pay by province

df_monthlypay_by_province <- df %>% 
  distinct(unid, province, monthly_pay)
#^^ couldn't do this because there's a prob with the data, people didn't fill it out to it's entirety

#scatterplot of gender by monthly pay

unique_indiv_gender <- df %>% 
  distinct(unid, gender, monthly_pay)
  
#finish this problem above ^


#THIS IS MY HOMEWORK:

#avg age of women compared to avg age of men 
average_age_by_gender <- df %>%
  distinct(gender, age) %>% 
  group_by(gender) %>% 
  filter(!is.na(age)) %>% 
  summarise(avg_age = mean(age)) %>% 
  ungroup()

#amnt of people to answer each round of survey
total_people_by_survey_round <- df %>%
  group_by(survey_date_month) %>% 
  summarise(total_people = max(survey_num)) %>% 
  ungroup()
# I didn't finish^


# save your 'featured engineered' data

saveRDS(df, file = "data/processed/processed_data.RDS")


