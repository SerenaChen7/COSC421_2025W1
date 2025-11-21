install.packages(c("readr", "dplyr", "stringr", "tidyr"))
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(igraph)

recipes <- read_csv("food_recipes.csv")
glimpse(recipes)


#Clean Data
setwd("C:/Users/serena/COSC421_2025W1")
library(readr); library(dplyr); library(stringr); library(tidyr)
recipes <- read_csv("food_recipes.csv")
nrow(recipes) 
names(recipes)
recipes$ingredients[1]
sum(grepl("\\|", recipes$ingredients), na.rm = TRUE)

recipes <- recipes %>%
  mutate(
    ingredients = tolower(ingredients),
    ingredients = str_replace_all(ingredients, "\\[|\\]", ""),
    ingredients = str_replace_all(ingredients, "\\s+", " "),
    ingredients = str_trim(ingredients)
  )
recipes$ingredients[1]

recipes$ingredient_list <- strsplit(recipes$ingredients, "\\|")
recipes$ingredient_list[[1]]
length(recipes$ingredient_list[[1]])


recipes <- recipes %>%
  mutate(
    cuisine_group = case_when(
      str_detect(cuisine,  regex("indian|thai|chinese|korean|japanese|vietnamese|asian", ignore_case = TRUE)) |
        str_detect(category, regex("indian|thai|chinese|korean|japanese|vietnamese|asian", ignore_case = TRUE)) ~ "Asian",
      
      str_detect(cuisine,  regex("american|mexican|italian|french|spanish|greek|british|european|western", ignore_case = TRUE)) |
        str_detect(category, regex("american|mexican|italian|french|spanish|greek|british|european|western", ignore_case = TRUE)) ~ "Western",
      
      TRUE ~ "Other"
    )
  )
table(recipes$cuisine_group)

recipes %>%
  filter(cuisine_group == "Other") %>%
  select(cuisine, category) %>%
  distinct() %>%
  head(30)


unique_others <- unique(recipes$cuisine[recipes$cuisine_group == "Other"])
length(unique_others)
unique_others
