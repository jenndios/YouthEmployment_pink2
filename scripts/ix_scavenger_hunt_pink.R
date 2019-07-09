# METADATA ====
# Description: Data scavenger hunt solutions (pink)
# Created: 2019-07-07 (Neil Rankin)
# Updated: 2019-07-07 (Neil Rankin)
# Reviewed: NA



# INITIALISE ====

#> Script-secific libraries ----

# Here is where we load libraries
library(lubridate)
library(tidyverse)


# LOAD DATA ====

df <- read.csv("data/raw/teaching_training_data.csv")

# WRANGLING

df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))



# QUESTIONS
# *1*. What is the average age of women compared to men across all observations
# in the data (i.e. not the unique observations but all times that people are
# seen)? Hint: you can use `group_by()` and then `summarize()`.


q1 <- df %>% 
  group_by(gender) %>% 
  summarise(ave_age = mean(age, na.rm = TRUE), 
            ave_age_at_survey = mean(age_at_survey, na.rm = TRUE))
# you have to 'remove' the NAs when taking the mean
view(q1)
# Females are about 5 months older than males on average (remember these are decimals not months after the decimal point)

# 2. How many people answer each round of the survey? Hint: you could use
# `table()` or even `group_by()` and `summarize()`
  
table(df$survey_num)

# or

q2 <- df %>% 
  group_by(survey_num) %>% 
  summarise(nobs = n())

# 3. How does the gender balance change between the sample of all respondents
# and the sample of distinct people? Hint: use the `distinct()` function we used
# in class to identify distinct people.

# calculating proportions for the sample as a whole
q3_whole <- df %>%
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarise(nobs = n()) %>% 
  mutate(total_n = sum(nobs)) %>% 
  mutate(proportion = nobs/total_n)

# you could also do this like this:
q3_whole_a <- df %>%
  filter(!is.na(gender)) %>% 
  summarise(prop = mean(as.integer(gender) - 1))

# think about what happened when you converted gender into numeric

q3_distinct <- df %>%
  filter(!is.na(gender)) %>% 
  distinct(unid, gender) %>% 
  group_by(gender) %>% 
  summarise(nobs = n()) %>% 
  mutate(total_n = sum(nobs)) %>% 
  mutate(proportion = nobs/total_n)

# a small fall in the proportion of males

# 4. What is the average duration of a job for those who have a `job_start_date`
# and `job_end_date`? Hint: you may need to convert these using `lubridate` and
# the `ymd` function. Have a look at the RStudio cheatsheet for lubridate

q4 <- df %>% 
  filter(!is.na(job_start_date) & !is.na(job_leave_date)) %>% 
  mutate(job_duration = interval(job_start_date, job_leave_date)/months(1)) %>% 
  summarise(mean_job_duration = mean(job_duration))

# 5. Do you see any patterns in `NA`s across the variables in columns
# `volunteer:numearnincome`? What might this tell you about the source of this
# data?
  
q5 <- df %>% 
  select(volunteer:numearnincome)

# one way to look at this would be to do some 'cross-tabulations'
table(q5$volunteer, q5$leadershiprole, exclude=NULL)

# the `exclude=NULL` argument includes NAs

# many of these variables are missing together.
# one explanation for this is that the questions asked at baseline changed
# and that sections might have been removed or added together

# 6. A stretch question: What is the average duration between surveys? Hint: Use
# `group_by()`, `dplyr::lag`
# (https://dplyr.tidyverse.org/reference/lead-lag.html) and maybe some
# `lubridate` and the code we used to calculate age.

# make sure you sort
q6 <- df %>% 
  select(unid, survey_num, survey_date_month) %>% 
  group_by(unid) %>% 
  arrange(survey_num) %>% 
  mutate(date_last_survey = lag(survey_date_month, default = NA)) %>% 
  filter(!is.na(date_last_survey)) %>% 
  ungroup()


# pause here to check whether the new variable has worked
# for some reason I get lagged values for survey 1.
# this should not be the case, so I will drop these

# now average duration
q6 <- q6 %>% 
  filter(survey_num != 1) %>% 
  mutate(survey_gap = interval(date_last_survey, survey_date_month)/months(1)) %>% 
  summarise(mean_survey_gap = mean(survey_gap))

#7. Compare `peoplelive`, `peoplelive_15plus` and `numchildren`. How reliable do
#you think these variables are?

# You could do this visually or create some tables

# You might think that peoplelive == peoplelive_15plus + numchildren i.e. the
# number of people you live with would be the number of 'adults' + the number of
# children

# However this might create mistakes: People might have children who are over 15
# Or (very common in SA) children may not live with parents but live with
# grandparents or extended family

# 8. Are women more likely to live in larger households (`peoplelive`) compared
# to men?

# we could do this with `summarise`
# I'm going to do it with a regression
reg_q8 <- lm(as.numeric(peoplelive) ~ gender, data = df)
summary(reg_q8)

# this tells me that males live in housholds which have on average half a person
# less
# but also have a look at the data - you might need to clean it up a bit

# 9. Are women more likely to have children compared to men?

# Summarise will work here or use a regression

q9 <- df %>% 
  select(gender, numchildren) %>% 
  mutate(have_children = case_when(numchildren == "0" ~ FALSE, 
                                   !is.na(numchildren) ~ TRUE))
# tabulate to check whether you have created the variable correctly
table(q9$numchildren, q9$have_children)

reg_q9 <- lm(have_children ~ gender, data = q9)
summary(reg_q9)

# Yes - about 53% of women have childern but only ~22% of men do (0.53-0.31)

# 10. ggplot()