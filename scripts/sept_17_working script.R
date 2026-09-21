library(tidyverse)

crashes <- read_csv("crashes.csv")
persons <- read_csv("person.csv")

glimpse(crashes)
glimpse(persons)

# What does one row represent in each table?
## Each row in crash represents one crash incident in New York
## Each row in people represents the individual people involved in accidents

# What variable appears in both? Does it do the same job in both?
## Both data sets have a Crash Date variable and collision ID so they can be matched up

crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

# You work for the NYC Department of Transportation and you have been given a media request: 
# How do person-level injury outcomes compare across boroughs? 
## Brooklyn has the hughest amount of people who were either injured or killed amoungst the specifies data. staten island has some of the lowest injury and killed counts


# Can you answer this with one dataframe alone? Probably not, or else we wouldn't do this on 
# our join day :)

# So we have to put these together. To do this, we have to work with keys. 

# A PRIMARY KEY uniquely identifies a row in its own table.
# A FOREIGN KEY points at another table's primary key.

# How should we figure out which one COLLISION_ID is?
## Collision ID is the primary key of crashes because each crash has its own individual unique collision ID

]

# Why does COLLISION_ID repeat in the person table?
## multiple people could've been in the same accident

# Could we try to learn something about that wreck with 52 people in it? Let's use the persons df
persons_core |> 
  count(COLLISION_ID) |> 
  arrange(desc(n))

persons_52 <- persons_core |> 
  filter(COLLISION_ID == 4724524)

## The collision included a lot of young kids between the ages of 9 and 10, you can assume it was a classroom trip or before or after school transportation

# Do we have any missing keys?

# Cardinality

# Cardinality is how many rows on each side can share a key value. 
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides

# Which one do we have? What does that tell you about what a join will
# do to our rows?

nrow(crashes_core)
nrow(persons_core)
n_distinct(persons_core$COLLISION_ID)

# Two of those numbers are close but not equal. What does that difference
# tell you before we join anything? 
## There are almost the same amount of crashes and collision IDs in persons core, but there is likely documentation
## missing from the persons data to account for all of the documented collisions. data for collisions without people data is irrelevant for this 

# Let's do our join. What should we join by? What's your guess for the number of rows? 
## joing collisions to persons and there should be about 317941 rows

# We'll start by doing a join that keeps observations where both cases exist. 
# Can anyone remember which join this is? 
## inner_join 

per_crash<- persons_core |> 
    inner_join(
      crashes_core,
      by = "COLLISION_ID")

# Use the console if you don't remember. 

# What does one row represent now? Is that the same as before?
## Each row represents an individual person by their unique ID

# Now the other one. inner_join keeps rows that matched in BOTH tables.
# Which one keeps every row of the LEFT table whether it matched or not.

per_crash_left <- persons_core |> 
  left_join(
    crashes_core,
    by = "COLLISION_ID")

nrow(per_crash)
nrow(per_crash_left)

# Where did the difference come from?

# What is one row in crash_people_inner? What is one row in
# crash_people_left? Are they the same question?

nrow(per_crash_left)
sum(!is.na(per_crash_left$UNIQUE_ID))

# If someone asked how many people were involved in crashes, which of
# those two numbers would you hand them?

# When would you want inner_join? When would you want left_join?

# We said this was one-to-many. We can say that in the code and make R
# check it for us.

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-many")

crashes_core |> 
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-one")

# What happened on the second one? Why would you want that?

# COVERAGE is which rows on each side found a match. The table got
# bigger, so nothing was lost, right?

crashes_core |> 
  anti_join(persons_core, join_by(COLLISION_ID)) |> 
  nrow()

persons_core |> 
  anti_join(crashes_core, join_by(COLLISION_ID)) |> 
  nrow()


# anti_join keeps left rows with NO match and adds no columns. Does the
# order matter here? Are those two lines asking the same question?

# What kind of crash has no person records?

# We can make R shout about this one too.

persons_core |> 
  left_join(crashes_core, join_by(COLLISION_ID), unmatched = "error")

# What does that argument buy you?

# The four mutating joins:
# inner_join = rows that matched in both
# left_join = every row in the LEFT table, matched or not
# right_join = every row in the right table
# full_join = everything from both

# Given all that, which one do you want for person-level outcomes with
# borough attached? Which table goes on the left?

##### Your turn #####

# Build a person-level table that includes borough.

# Before you write anything:
## What should one row represent when you're done?
## Which table goes on the left?

# Then:
## Join them.
## Declare the cardinality with relationship = and see if R agrees.
## Use anti_join() to look at whatever didn't match.
## Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.

# Some of those rows will have no borough. Before you filter them out:
# how many are there, and are they all missing for the same reason?
