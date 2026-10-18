# packages 
library(tidyverse)

# data
requests <- read_csv("nyc311_2023_sample_50000.csv.gz",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

glimpse(requests) 

# what does one row represent? how do you know? different complaints/ requests filed in different boroughs in ny
# what identifies one request? how do you know? the unique key represents one request
nrow(requests)
n_distinct(requests$unique_key)

# take a look at our columns. what are the data types? We are looking at dates, agencies referred, and explanations and location

# pay attention to the *_date columns. what types are those? 
class(requests$created_date)
class(requests$closed_date)
class(requests$resolution_date)


# the columns are dates and times, but is that their actual data type being recognized? 
# no they are recognized as characters 

# let's see how this is a problem by dealing with a seeming easy question: 
# how long did each request take to close? either within the same day and no more then ten (only based off the first few entries)

# what if we just tried the most straightforward option: subtraction? 

# what's the problem here? they are characters and wont subract

# we can try another approach, let's use day() to pull the day out of a date. 
# bracket the first row of created_date and let's just use day() on it. 
requests$created_date[1]
day(requests$created_date[1])
month(requests$created_date[1])
year(requests$created_date[1])

# what is the value? where is that number even coming from?? it is saying the day is 20 and coming from the year 2023, it says the month is 7, and the year is 2 from 02

# let's take a look at day day(), month(), and year() do under the hood. 
as.Date(requests$created_date[1])

# as.Date tries %Y-%m-%d so it's interpreting "02/07/2023 02:38:31 PM" as 
# July 20th, 0002 AD. 

# this is the problem even if we do it in lubridate
ymd(requests$created_date[1])

# let's figure out what's up with our representation. 
# select created_date and look at the first 10 rows. 
requests |> 
  select(created_date) |> 
  slice_head(n = 10)

# look at those 10 lines. before we can choose a parser, what do we need to know? 
## how the dates are formatted

# took at look at line 4 in the output. 
# 01/12/2023 08:36:16 AM

# what is the date? 
# Jan 12, 2023, 08:36:16 AM or Dec 1, 2023, 08:36:16 AM
## Jan 12, 2023


# how could you get evidence from the data itself? imagine you're in a scenario where the
# answer isn't abundantly clear from that output. hint: regex can help with things like this. 
## the months number cannot be greater than 12

# what does your regex idea tell you about the data? 
requests |>
  filter(str_detect(created_date, "^\\d{2}/(1[3-9]|2\\d|3[01])/"))

# first column is month and second column is date

# since we've done that, we can actually do our parsing now. 
# create a new column called created. keep created_date. 
# how do we do this? 
requests <- requests |> 
  mutate(created_wrong = mdy_hms(created_date))

requests <- requests |> 
  mutate(created = mdy_hms(created_date, 
                           tz = "America/New_York"))

requests$created[1] - requests$created_wrong[1]

?mdy_hms


# let's check out something interesting about parsing. what are the arguments 
# that are allowed in our mdy_hms() function? 
## tz = you can specify which time zone to align parsing date with

# let's return to our new created column. are there any potential problems here? 
## could we have accidentally created some missing values? maybe am and pm
## what do you need to know to assess this? how many 

# are there any problems that could occur even if we didn't have any missing values at all? 
# use this as an example: 01/05/2023 01:30:00 PM
# could something be potentially risky with that? how can we check on 01:30:00 PM?
requests |> 
  count(hour = hour(created)) |> 
  print(n = 24)

# what does this tell you? how many observations per hour of the day in military time. kind of consistent for a working/ awake day

# briefly, what about chronological operations? 
requests |> 
  summarise(
    earliest = min(created, na.rm = TRUE),
    latest = max(created, na.rm = TRUE)
  )

# what would this give us on the original character column? guess first. earliest "year"?


requests |> 
  summarise(
    earliest_string = min(created_date, na.rm = TRUE),
    latest_string = max(created_date, na.rm = TRUE)
  )


## gave it to us flipped

# here are a few things to do on your own: 

# first, make a plot of requests per week. however, notice there isn't a week column. 
# how should you make one? 

requests_wk<-requests |> 
  mutate(week = floor_date(created, "week")) |> 
  count(week)


requests_wk |> 
  ggplot(aes(x= week, y = n))+
  geom_col()

# second, here is a larger bit of practice. 

## your question: which city agencies close 311 requests fastest?

requests <- requests |> 
  mutate(closed = mdy_hms(closed_date, 
                           tz = "America/New_York"))
requests_large<-requests|> 
  mutate(close_time = as.numeric(difftime(closed, created, units = "hours"))) |> 
  group_by(agency) |> 
  summarise(total = n(), 
            med_hours = median(close_time[!is.na(close_time)& close_time >= 0]),
            sum_hours = sum(!is.na(close_time) & close_time >= 0)) |> 
  filter(total>= 500) |> 
  arrange(med_hours) 
  


## you should report: 
### how many requests it received. 

## NYPD - 21750
## DHS - 708
## DOHMH - 1263
## DEP 2657
## DOT - 3036
## DSNY - 4615
## HBD - 10648
## DOB 1516 
## EDC - 903
## DPR - 2028
## TLC - 541

### how many you were able to measure. 
## NYPD - 21748
## DHS - 409
## DOHMH - 1200
## DEP 2636
## DOT - 2964
## DSNY - 4585
## HBD - 10588
## DOB 1515
## EDC - 673
## DPR - 1858
## TLC - 513 

### a typical time to close. 
## NYPD - 0.77 hr
## DHS - 8.29 hr
## DOHMH - 15.81 hr
## DEP 21.23 hr
## DOT - 41.07 hr
## DSNY - 47.70 hr
## HBD - 103.64 hr
## DOB 313.36 hr
## EDC - 911.02 hr
## DPR - 1077.22 hr
## TLC - 1407.16 hr

## you should limit your results to only include agencies that had at least 500 requests.  
### 

