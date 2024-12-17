# Load necessary libraries
library(AQEval)  # For the dataset
library(dplyr)   # For data manipulation


##### CATEGORIZING #######

# Categorize wind direction (wd) into 4 cardinal directions
df <- df %>%
  mutate(
    wd_category4 = case_when(
      wd >= 315 | wd < 45  ~ "North",
      wd >= 45 & wd < 135 ~ "East",
      wd >= 135 & wd < 225 ~ "South",
      wd >= 225 & wd <= 315 ~ "West",
      TRUE ~ NA_character_
    )
  )

# Convert wd_category4 to a factor
df$wd_category4 <- factor(df$wd_category4, levels = c("North", "East", "South", "West"))

# Categorize wind direction (wd) into 8 cardinal directions
df <- df %>%
  mutate(
    wd_category8 = case_when(
      (wd >= 337.5 | wd < 22.5) ~ "N",
      wd >= 22.5 & wd < 67.5 ~ "NE",
      wd >= 67.5 & wd < 112.5 ~ "E",
      wd >= 112.5 & wd < 157.5 ~ "SE",
      wd >= 157.5 & wd < 202.5 ~ "S",
      wd >= 202.5 & wd < 247.5 ~ "SW",
      wd >= 247.5 & wd < 292.5 ~ "W",
      wd >= 292.5 & wd < 337.5 ~ "NW",
      TRUE ~ NA_character_
    )
  )


# Convert wd_category8 to a factor
df$wd_category8 <- factor(df$wd_category8, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))


Sys.setlocale("LC_TIME", "C")
# Categorize date into months
df <- df %>%
  mutate(
    month = format(date, "%B")
  )

# Convert month to a factor
df$month <- factor(df$month, levels = month.name)



# Categorize date into season 
library(dplyr)
library(lubridate)

df <- df %>%
  mutate(season = case_when(
    month(date) %in% c(3, 4, 5) ~ "Spring",   
    month(date) %in% c(6, 7, 8) ~ "Summer",    
    month(date) %in% c(9, 10, 11) ~ "Autumn",  
    month(date) %in% c(12, 1, 2) ~ "Winter",   
    TRUE ~ NA_character_
  ))

# as factor
df$season <- factor(df$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

