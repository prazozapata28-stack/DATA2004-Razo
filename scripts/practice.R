#chapter 7 Notes
#Paulina Razo-Zapata

#download readr package with tidyverse
library(tidyverse)

read_csv('students.csv')

#get rid of N/A values
students <- read.csv('students.csv', na=c("N/A", ""))

#rename columns with back ticks
students<-students %>%
  rename(student_id = 'Student.ID',
         full_name = 'Full.Name',
         favorite_food = 'favourite.food',
         meal_plan = 'mealPlan')

# change meal plan from being a categorical variable to a factor
students<-students|> 
  mutate(meal_plan = factor(meal_plan))

students

# fix the age column thats characteristics instead of numerical number
library(janitor)
students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )


#practice putting in values
read_csv(
  "a,b,c
  1,2,3
  4,5,6"
)
