########################################
# 0. Setup
########################################
rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(igraph)

setwd("D:/COSC421_2025W1")   # adjust if needed

########################################
# 1. Read data (use base read.csv, not readr)
########################################
recipes_raw <- read.csv(
  "food_recipes.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

########################################
# 2. Define cuisine groups
########################################
indian_subregions <- tolower(c(
  "Karnataka","Mughlai","Andhra","Bengali Recipes","Himachal","Punjabi",
  "Maharashtrian Recipes","Tamil Nadu","Rajasthani","Kerala Recipes",
  "Oriya Recipes","Kashmiri","North Karnataka","Gujarati Recipes",
  "Goan Recipes","Uttarakhand-North Kumaon","North East India Recipes",
  "Sri Lankan","Uttar Pradesh","Chettinad","Sindhi","Bihari","Udupi",
  "Awadhi","Haryana","Coastal Karnataka","South Karnataka","Malabar",
  "Mangalorean","Parsi Recipes","Nepalese","Afghan","Assamese",
  "Hyderabadi","Lucknowi","Malvani","Pakistani","Kongunadu","Coorg",
  "Konkan","Bangladeshi","Nagaland"
))
chinese_regions   <- tolower(c("Sichuan"))
indonesian_list   <- tolower(c("Indonesian"))
malaysian_list    <- tolower(c("Malaysian"))

western_regex <- paste(
  c("american","mexican","italian","french","spanish","greek","british",
    "european","western","continental","mediterranean","swedish","german",
    "portuguese","russian","dutch","polish","austrian","swiss","belgian",
    "canadian","australian","new zealand"),
  collapse = "|"
)

########################################
# 3. Clean + classify + ingredient_list
########################################
recipes <- recipes_raw %>%
  as_tibble() %>%
  mutate(
    recipe_id = row_number(),
    cuisine   = str_to_lower(str_trim(cuisine)),
    
    # clean ingredients text
    ingredients = ingredients %>%
      str_to_lower() %>%
      str_replace_all("\\[|\\]", "") %>%
      str_replace_all("\"", "") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim(),
    
    # split into list column by "|"
    ingredient_list = str_split(ingredients, "\\|"),
    
    # major Asian label
    major_asian = case_when(
      cuisine %in% indian_subregions ~ "Indian",
      cuisine %in% chinese_regions   ~ "Chinese",
      cuisine %in% indonesian_list   ~ "Indonesian",
      cuisine %in% malaysian_list    ~ "Malaysian",
      str_detect(cuisine, "chinese")    ~ "Chinese",
      str_detect(cuisine, "japanese")   ~ "Japanese",
      str_detect(cuisine, "korean")     ~ "Korean",
      str_detect(cuisine, "thai")       ~ "Thai",
      str_detect(cuisine, "vietnamese") ~ "Vietnamese",
      str_detect(cuisine, "indian")     ~ "Indian",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    cuisine_group = case_when(
      !is.na(major_asian) ~ "Asian",
      str_detect(cuisine, western_regex) ~ "Western",
      TRUE ~ "Other"
    )
  )

# check counts – should be ~1863 Asian and 1863 Western
table(recipes$cuisine_group, useNA = "ifany")

########################################
# 4. Long format: one row per (recipe_id, ingredient)
########################################
recipes_long <- recipes %>%
  unnest(cols = ingredient_list) %>%
  mutate(
    ingredient = str_to_lower(str_trim(ingredient_list))
  ) %>%
  filter(
    !is.na(ingredient),
    ingredient != ""
  ) %>%
  select(recipe_id, cuisine, cuisine_group, ingredient)

# sanity check
head(recipes_long)
str(recipes_long)

########################################
# 5. Build co-occurrence edges via self-join
########################################
# Helper: from a (recipe_id, ingredient) data frame, get edge list
build_edge_df <- function(df) {
  df %>%
    select(recipe_id, ingredient) %>%
    distinct() %>%                     # one ingredient per recipe
    inner_join(., ., by = "recipe_id", suffix = c(".x", ".y")) %>%
    # keep each unordered pair once (lexicographic order)
    filter(ingredient.x < ingredient.y) %>%
    count(ingredient.x, ingredient.y, name = "weight") %>%
    rename(from = ingredient.x, to = ingredient.y)
}

# Wrapper: build igraph network
build_network <- function(df) {
  edges <- build_edge_df(df)
  if (nrow(edges) == 0) {
    return(make_empty_graph(directed = FALSE))
  }
  graph_from_data_frame(edges, directed = FALSE)
}

########################################
# 6. Asian / Western / Global networks
########################################
asian_df   <- recipes_long %>% filter(cuisine_group == "Asian")
western_df <- recipes_long %>% filter(cuisine_group == "Western")

asian_net   <- build_network(asian_df)
western_net <- build_network(western_df)
global_net  <- build_network(recipes_long)

asian_net
western_net
global_net

########################################
# 7. Centrality metrics
########################################
compute_metrics <- function(g) {
  if (vcount(g) == 0) return(data.frame())
  data.frame(
    ingredient  = V(g)$name,
    degree      = degree(g),
    betweenness = betweenness(g, directed = FALSE, normalized = TRUE),
    eigen       = eigen_centrality(g)$vector
  ) %>%
    arrange(desc(degree))
}

asian_metrics   <- compute_metrics(asian_net)
western_metrics <- compute_metrics(western_net)
global_metrics  <- compute_metrics(global_net)

# look at top ingredients
head(asian_metrics, 10)
head(western_metrics, 10)
head(global_metrics, 10)

########################################
# 8. Network-level stats
########################################
asian_stats <- data.frame(
  network    = "Asian",
  nodes      = vcount(asian_net),
  edges      = ecount(asian_net),
  avg_degree = mean(degree(asian_net)),
  density    = edge_density(asian_net)
)

western_stats <- data.frame(
  network    = "Western",
  nodes      = vcount(western_net),
  edges      = ecount(western_net),
  avg_degree = mean(degree(western_net)),
  density    = edge_density(western_net)
)

global_stats <- data.frame(
  network    = "Global",
  nodes      = vcount(global_net),
  edges      = ecount(global_net),
  avg_degree = mean(degree(global_net)),
  density    = edge_density(global_net)
)

asian_stats
western_stats
global_stats

########################################
# 9. Bridge ingredients (fusion connectors)
########################################
asian_ingredients   <- unique(asian_df$ingredient)
western_ingredients <- unique(western_df$ingredient)

global_metrics <- global_metrics %>%
  mutate(
    in_asian   = ingredient %in% asian_ingredients,
    in_western = ingredient %in% western_ingredients,
    bridge     = in_asian & in_western
  )

bridge_ingredients <- global_metrics %>%
  filter(bridge) %>%
  arrange(desc(betweenness)) %>%
  slice(1:30)

bridge_ingredients

########################################
# 10. Community detection (clusters)
########################################
global_comm <- cluster_louvain(global_net)

community_membership <- data.frame(
  ingredient = names(membership(global_comm)),
  community  = membership(global_comm)
)

head(community_membership, 20)
