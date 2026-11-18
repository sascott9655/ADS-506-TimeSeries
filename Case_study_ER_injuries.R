# Learning notes based on "Mastering Shiny" (Wickham), Ch. 4.
# Not my original work. See README for attribution.
# Source: https://mastering-shiny.org/


library(shiny)
library(vroom)
library(tidyverse)

# We’re going to explore data from the National Electronic Injury Surveillance
# System (NEISS), collected by the Consumer Product Safety Commission. This is a
# long-term study that records all accidents seen in a representative 
# sample of hospitals in the United States. You can find out more about this 
# data set at https://github.com/hadley/neiss.

# focus on just the data from 2017

dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population

# Data exploration
# We’ll use 649, “toilets” as source of study. 

toilet_injur <- injuries |>
  filter(prod_code == 649)
  nrow(selected)
  
# Perform some basic summaries looking at the location, body part,
# and diagnosis of toilet related injuries.

toilet_injur  |> count(location, wt = weight, sort = TRUE)  

toilet_injur  |> count(body_part, wt = weight, sort = TRUE)

toilet_injur  |> count(diag, wt = weight, sort = TRUE)

# Most toilet injuries occur at home.

summary <- toilet_injur |> 
  count(age, sex, wt = weight)
  summary

summary |> 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

# Build a rate column because they are fewer older people than younger people
# making plot skewed for older people. Rate is divided by 10,000.

summary <- toilet_injur  |> 
  count(age, sex, wt = weight) |> 
  left_join(population, by = c("age", "sex")) |>
  mutate(rate = n / population * 1e4)
summary

summary |> 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")


#Exploring 10 random narratives

toilet_injur |> 
  sample_n(10) |> 
  pull(narrative)






