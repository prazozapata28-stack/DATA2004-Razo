library(tidyverse)
library(readr)

fishing <- read_csv("erie(Erie).csv")

glimpse(fishing)

# What does one row represent?

## One row represents region-year weight (pounds) of caught fish. 

# How would we look at our grain over time by region? 

fishing %>%
  ggplot(aes(x = "Year", y = "Grand Total"))
fishing |> 
  ggplot(aes(x = Year, y = `Grand Total`,
             )) +
  geom_line()

fish_reg <- fishing |> 
  pivot_longer(
    cols = !c (Year, Lake, Species, 'U.S. Total', 'Grand Total',Comments),
    names_to = "Region",
    values_to = "Quantity",
  )

fish_reg |> 
  ggplot(aes(x = Year, y = `Quantity`, color = Region
  )) +
  geom_line()

# What goes on the y-axis?
## Quantity

# What goes on the color aesthetic if we want one line per region?
# aes(x= Year, y= Grand Total, color = Region)

# Are these data tidy? If not, which one breaks the rules of tidy data?
## No, the data is not tidy, there is a lot of NA values

# Let's predict something

nrow(fishing)

# We're about to move 7 columns into one. How many rows should we have?
## same number of rows

# Pivot 

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = 4:10 # what does this mean?
  )

fishing |> 
  rename(
    `Comments ` = `Comments`
  ) |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = contains(" ") # what does this mean? Select columns where there is a space
  )
  
fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = contains(" ") # what does this mean? to merge all of the columns if not specified which
  )

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = !c(Year, Lake, Species, Comments) # what does this mean? pivot everything except year,lake, species, and comments
  )

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = c("Michigan (MI)", "New York (NY)", "Ohio (OH)",
             "Pennsylvania (PA)", "U.S. Total", "Canada (ONT)",
             "Grand Total") # what does this mean? to pivot all the columns listed
  )

nrow(fishing_long)
nrow(fishing_long) / nrow(fishing)

# Did we get what we predicted?
## No we cleaned it up so that extra, unnecessary rows with no data are cleared

# Which one of these four pivots would you use? Why not some of the others? 
## I would use the one that specifically specifies what I am merging because, especially with a large dataset, you do not want to write out every single column youre intending to merge

# let's check out what we made
fishing_long |> 
  distinct(region)

# Are all seven of these the same kind of thing? What's the grain? #there are states, provinces and then there is a US total, and a grand total, 
# they are not the same type of grain because one grain only has data from a state, others a province, and country which are all on very different scales

# Let's see what these 7 regions are. Let's look at Lake Whitefish in 1885 then just look at the 
# region and values. 

fishing_long |> 
  filter(Year == 1885, Species == "Lake Whitefish") |> 
  select(region, values)

# some numbers are much greater because of the scale and the idea that there are makeups of the regions within other regions

# So how many levels are stacked into this one column now? And what happens if we just add the catch? 

fishing_long |> 
  summarise(total = sum(values, na.rm = TRUE))

#this number includes ALL of the regions which is not accurate

fishing_long |> 
  filter(region != "Grand Total") |> 
  summarise(total = sum(values, na.rm = TRUE))

#this number is also still too large and contains multiplicities of the same count

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  summarise(total = sum(values, na.rm = TRUE))

#this data only lookds at the regions that arent US total and Grand total to get the true population

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  ggplot(aes(x = Year, y = values, color = Species)) +
  geom_line()

# Why isn't the first total just triple the third total?
# Because the third number should be about the grandtotal number and the US total does not include canada so it will have missing values and there for not equivalent to the Grand Total

# Now we can plot it. But if we do below, it's a bit hectic and ugly. 

fishing_long |> 
  filter(
    !region %in% c("U.S. Total", "Grand Total"),
    !is.na(values)
  ) |> 
  mutate(species = fct_lump_n(Species, 6)) |> 
  ggplot(aes(x = Year, y = values, color = species)) +
  geom_line() 

# So let's trim it down to the top 6 species. We can do this in dplyr directly 
# using fct_lump_n(). Then let's use a better color paletter :)

fishing_long |> 
  filter(
    !region %in% c("U.S. Total", "Grand Total"),
    !is.na(values)
  ) |> 
  mutate(species = fct_lump_n(Species, 6)) |> 
  ggplot(aes(x = Year, y = values, color = species)) +
  scale_color_manual(values = palette.colors(7, "Okabe-Ito")) +
  geom_line() 

# let's take it back the other way. What if we widened it by region? How many rows? There are 2561
fishing_long |> 
  select(Year, Species, region, values) |> 
  pivot_wider(names_from = region, values_from = values) |> 
  print(width = Inf)

# Now let's break. Choose a different sheet in the spreadsheet. 
# What is the grain? 
# What is the total catch for that lake? 
## What distinct regions are you left with? 
## How many rows did you start with? How many did your pivot have?

## grain: 