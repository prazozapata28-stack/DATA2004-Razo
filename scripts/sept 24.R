# Strings and categorical representation

library(tidyverse)

pa <- read_delim(
  "fec_pa_2024_strings.psv",
  delim = "|",
  quote = "",
  na = character(),
  col_types = cols(.default = col_character())
)

# only contributions > $200 are included
# only three of the original columsn are there. record_id is mine, not the FEC's
# helps to protect anonymity. 

glimpse(pa)

nrow(pa)
n_distinct(pa$record_id)

# What does one row represent? Is one row one contributor?
# each row is one contributer and the each individual contribution

# Which variable are we interested in today? empoyers

# Our task: who employs the people giving money to campaigns?

pa |> 
  count(employer, sort = TRUE) |> 
  print(n = 25)

# Same question as the vehicles. Is this counting employers?  no

# Look for ways someone could say they are self employed. 
# What about unemployed? not, self, 

n_distinct(pa$employer)

# Does that mean there are 6,950 different employers? no, different unique variables

# Normalizing representation
examp_text <- "Hello, this  is an example. "

str_to_upper(examp_text)
str_trim(examp_text)
str_squish(examp_text)

# let's create employer_normalized, where we make every observation in all caps
# and we remove traiing and leading white spaces. 

pa_cl <- pa |> 
  mutate(norm_empl = employer |> 
           str_squish() |> 
           str_to_upper())

# That barely changed. Why not? everything was already very clean
# What does that tell you about how this data was recorded?


# What's actually inconsistent?
pa |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF",
      "ELECTRICIANS LOCAL 98", "ELECTRICIANS LOCAL98",
      "HIGHMARK INC", "HIGHMARK, INC.", "HIGHMARK HEALTH"
    )
  ) |> 
  count(employer, sort = TRUE)

# Which of these differences are just representation? the names of the companies
# Which might actually mean different things?
# Which would you change without outside information? the names of the comapnies

# Punctuation 
## let's get rid of those hyphenations now. 
# we can use str_replace_all(fixed()) to do this. 
# should we use str_squish again?

pa_clean <- pa_cl|> 
  mutate(
    employer_no_hyphen = norm_empl |> 
      str_replace_all(fixed("-"), " ") |> 
      str_squish()
  )

pa_clean |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF"
    )
  ) |> 
  count(employer_no_hyphen, sort = TRUE)

# What collapsed? What didn't?

# SELF looks like it belongs with SELF EMPLOYED. Does it?
# What if Self is a company? An abbreviation for something else?

# Removing a hyphen changed how two words were written.
# Merging SELF would be a claim about who these people are.


# Exact replacement
# When you know exactly which value you want to change, change that
# value and nothing else.

# Here's where we can use case_when()

pa_clean <- pa_clean |> 
  mutate(
    employer_clean = case_when(
      norm_empl == "NOT-EMPLOYED" ~ "NOT EMPLOYED",
      norm_empl == "SELF-EMPLOYED" ~ "SELF EMPLOYED",
      norm_empl == "ELECTRICIANS LOCAL98" ~ "ELECTRICIANS LOCAL 98",
      norm_empl == "HIGHMARK, INC." ~ "HIGHMARK INC",
      TRUE ~ employer_clean
    )
  )

# Notice this matches on employer_normalized, not employer_no_hyphen.
# If you had already stripped punctuation, would
# "HIGHMARK, INC." still be there for case_when to find?


# Why keep raw column?
pa_clean |> 
  filter(employer != employer_clean) |> 
  count(employer, employer_clean, sort = TRUE) |> 
  print(n = 61)

# I wrote four replacement rules. Why did more rows than that change?

# Find ORRICK HERRINGTON  SUTCLIFFE LLP. Why is there a double space
# in the middle of a law firm's name? What did str_squish() do to it? got rid of extra space

pa_clean |> 
  select(employer, employer_normalized, employer_clean) |> 
  distinct() |> 
  arrange(employer_clean) |> 
  print(n = 30)

# What would we lose if we overwrote employer? lose the original data to compare with


# Weekly 2

coffee <- read_csv("data/coffee_ratings.csv")

coffee |> 
  count(producer, sort = TRUE)

n_distinct(coffee$producer)

# Tuesday: Regular expression 

# Today I told you which values to look at. What if I hadn't?
# Would you read all 6,950?

# Tuesday: how do you find suspicious or related values without
# reading every one?