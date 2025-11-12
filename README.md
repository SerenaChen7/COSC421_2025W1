# COSC421_2025W1

# Recipe Ingredient Co-occurrence Networks  
*COSC 421 – Network Science, UBC Okanagan*  
*Team 9: Serena Chen & Tianxing (Eric) Chen*

---

## Project Overview
This project explores how **Asian and Western cuisines** are connected through shared ingredients.  
We construct **ingredient co-occurrence networks** where  
- **Nodes** represent unique ingredients  
- **Edges** represent pairs of ingredients that appear together in recipes  

By analyzing these networks, we aim to discover:
1. Which ingredients are most central in the global network  
2. How Asian and Western cuisines form distinct clusters  
3. Which ingredients act as bridges between cuisines (fusion potential)  
4. What global patterns reveal about ingredient diversity  

---

## Data Source
We use publicly available **Kaggle recipe datasets**, such as:  
- [Asian and Indian Cuisines](https://www.kaggle.com/datasets/hoandan/asian-and-indian-cuisines)  
- (Optionally) [Western Food Recipes Dataset](https://www.kaggle.com/)  

Each dataset provides recipes in tabular form, with binary indicators (0 / 1) showing whether each ingredient appears in a recipe.

---

## Data Processing Steps
1. **Load datasets** (`asian_indian_recipes.csv`, etc.) in R  
2. **Clean column names & select ingredient columns**  
3. **Construct 0–1 ingredient matrix** (rows = recipes, columns = ingredients)  
4. **Compute co-occurrence matrix** using  
   ```R
   C <- t(M) %*% M
