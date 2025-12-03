# 0. Setup
rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(igraph)

setwd("C:/Users/chent/COSC421_2025W1")# adjust if needed


# 1. Read data (use base read.csv, not readr)
recipes_raw <- read.csv(
  "food_recipes.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)


# 2. Define cuisine groups
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


# 3. Clean + classify + ingredient_list
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


# 4. Long format: one row per (recipe_id, ingredient)
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


# 5. Build co-occurrence edges via self-join
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


# 6. Asian / Western / Global networks
asian_df   <- recipes_long %>% filter(cuisine_group == "Asian")
western_df <- recipes_long %>% filter(cuisine_group == "Western")

asian_net   <- build_network(asian_df)
western_net <- build_network(western_df)
global_net  <- build_network(recipes_long)

asian_net
western_net
global_net


# 7. Centrality metrics
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


# 8. Network-level stats
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


# 9. Bridge ingredients (fusion connectors)
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


# 10. Community detection (clusters)
global_comm <- cluster_louvain(global_net)

community_membership <- data.frame(
  ingredient = names(membership(global_comm)),
  community  = membership(global_comm)
)

head(community_membership, 20)


# 11.1 Asian network (subgraph of top-degree nodes)

# Common palette for degree-based coloring
deg_palette <- colorRampPalette(c("#fee8c8", "#fdbb84", "#e34a33"))

# Use the 200 nodes with the highest degree in the Asian network
deg_asian <- degree(asian_net)
top_asian_nodes <- names(sort(deg_asian, decreasing = TRUE))[1:200]
asian_sub <- induced_subgraph(asian_net, vids = top_asian_nodes)

# Node colors based on degree
asian_node_deg <- degree(asian_sub)
asian_col_vec <- deg_palette(100)
asian_node_col <- asian_col_vec[cut(asian_node_deg, breaks = 100)]

set.seed(123)
layout_asian <- layout_with_fr(asian_sub, niter = 1500)

plot(
  asian_sub,
  layout       = layout_asian,
  vertex.size  = 4,
  vertex.label = NA,
  vertex.color = asian_node_col,
  edge.color   = rgb(0.7, 0.7, 0.7, 0.2),
  edge.width   = 0.3,
  main         = "Asian Ingredient Network (Top 200, Degree-Colored)"
)


# 11.2 Western network (subgraph of top-degree nodes)

deg_west <- degree(western_net)
top_west_nodes <- names(sort(deg_west, decreasing = TRUE))[1:200]
western_sub <- induced_subgraph(western_net, vids = top_west_nodes)

west_node_deg <- degree(western_sub)
west_col_vec  <- deg_palette(100)
west_node_col <- west_col_vec[cut(west_node_deg, breaks = 100)]

set.seed(123)
layout_west <- layout_with_fr(western_sub, niter = 1500)

plot(
  western_sub,
  layout       = layout_west,
  vertex.size  = 4,
  vertex.label = NA,
  vertex.color = west_node_col,
  edge.color   = rgb(0.7, 0.7, 0.7, 0.2),
  edge.width   = 0.3,
  main         = "Western Ingredient Network (Top 200, Degree-Colored)"
)


# 11.3 Global network with Louvain communities (subgraph)

# Use the 300 nodes with the highest degree in the global network
deg_global <- degree(global_net)
top_global_nodes <- names(sort(deg_global, decreasing = TRUE))[1:300]
global_sub <- induced_subgraph(global_net, vids = top_global_nodes)

# Run Louvain on the subgraph
global_sub_comm <- cluster_louvain(global_sub)
comm_membership <- membership(global_sub_comm)

# Community-based colors
comm_ids <- as.integer(factor(comm_membership))
comm_palette <- rainbow(length(unique(comm_ids)))
comm_colors  <- comm_palette[comm_ids]

set.seed(123)
layout_global <- layout_with_fr(global_sub, niter = 1500)

plot(
  global_sub,
  layout       = layout_global,
  vertex.size  = 4,
  vertex.label = NA,
  vertex.color = comm_colors,
  edge.color   = rgb(0.7, 0.7, 0.7, 0.2),
  edge.width   = 0.3,
  main         = "Global Ingredient Network (Top 300, Louvain Communities)"
)


# 11.4 Degree distribution for global network
deg_global_full <- degree(global_net)

hist(
  deg_global_full,
  breaks = 100,
  main   = "Degree Distribution (Global Network)",
  xlab   = "Degree",
  ylab   = "Frequency",
  col    = "grey80",
  border = "white"
)


# 11.5 Top 15 ingredients by degree (global)
top_deg <- global_metrics %>%
  arrange(desc(degree)) %>%
  slice(1:15)

op <- par(no.readonly = TRUE)
par(mar = c(10, 4, 4, 2))

barplot(
  top_deg$degree,
  names.arg = top_deg$ingredient,
  las       = 2,
  main      = "Top 15 Ingredients by Degree (Global Network)",
  ylab      = "Degree",
  col       = "grey80",
  border    = "white"
)


# 11.6 Top 15 bridge ingredients by betweenness
top_bridge <- bridge_ingredients %>% slice(1:15)

barplot(
  top_bridge$betweenness,
  names.arg = top_bridge$ingredient,
  las       = 2,
  main      = "Top 15 Bridge Ingredients (Betweenness)",
  ylab      = "Betweenness (normalized)",
  col       = "grey80",
  border    = "white"
)