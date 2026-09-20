# packages
library(tidyverse)

# import and look 
data <- read_csv(
  "co-est2025-alldata.csv",
  locale = locale(encoding = "Latin1"), 
  col_types = cols(.default = col_character())
)

glimpse(data)
names(data)

# What can we determine from this? What can we not? 
## Not much there are code works for variables and confusing column names

# What have you been given? How would you figure out what this csv is without
# being told?
## There is a lot of numbers that have no meaning unless the code manual is went and found

# Find the documentation and record 
## Who produced this? U.S. Census Bureau
## What does the file contain? Population divisions at the state and county level
## What time period does it cover? April 1, 2020 to July 1, 2025

# Build a diagnostic view
## Find the documentation and look at the code above. What variables determine what one 
## row represents? 

##SUMLEV is the variable that determines the what each row represents as its the far left variable and contains county or state
##looked through the codebook
data |> 
  count(SUMLEV)


# Looking at your table:
## Does every row appear to represent th same kind of geographic observation?
## Which row(s) look different? 
## What in your table tells you this? 

data |> 
  slice_head(n=5) #shows that each state in the SUMLEV is then followed by all the counties in that respective state

## Is there a variable that appears to encode that difference? 

# Declare the grain (grain is always formatted in a particular way)
## one row represents state/ county mixed grain of population

# Let's convert two columns to numeric 
## `POPESTIMATE2025`
## `NPOPCHG2025`

data_num<- data |> 
  mutate(POPESTIMATE2025_NUM = as.numeric(POPESTIMATE2025),
         NPOPCHG2025_NUM = as.numeric(NPOPCHG2025))

# What is the total population in the US in 2025 according to this file? 

data_num |> 
  filter(SUMLEV == "050") |> 
  summarise(total_pop = sum(POPESTIMATE2025_NUM))

# Which Kentucky counties grew the most from 2024 to 2025? 

data_kentucky <- data_num |> 
  filter(STNAME == "Kentucky", 
         SUMLEV!= "040") |> 
  arrange(desc(NPOPCHG2025)) 

data_kentucky_sel <- data_num |> 
  filter(STNAME == "Kentucky", 
         SUMLEV!= "040") |> 
  arrange(desc(NPOPCHG2025_NUM)) |> 
  select(CTYNAME, NPOPCHG2025_NUM)



table(data$STNAME)

# How many counties or county-equivalent records are in this file? 

data_cty <- data |> 
  filter(SUMLEV == "050")

glimpse(data_cty)

#easier way

count(data_cty)

#3144 counties or county-like



