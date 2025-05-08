# Workshop Scripts: R Programming and Statistical Modeling

This folder contains a collection of foundational R scripts for educational and analytical purposes. Each script introduces core programming concepts or statistical techniques through applied examples.

## Contents

### 1. `basic R file.R`

Introduces basic R syntax and workflow:
- Simulates a dataset with group structure
- Demonstrates data inspection and manipulation
- Performs basic descriptive statistics and a two-sample *t*-test
- Applies grouped summaries using `dplyr`

### 2. `data set transformations.R`

Covers advanced data wrangling and simulation:
- Simulates reaction time data with nested structure (blocks × participants)
- Applies `pivot_wider()` to explore long vs. wide data formats
- Demonstrates dataset merging (`merge()`, `left_join()`, etc.)

fun stuff:
- Applies linear mixed-effects modeling (`lmer()`), ANOVA, and plotting with `ggplot2`
- Ends with a short exploratory factor analysis on simulated Likert-type survey data

### 3. `distributions and estimate parameters.R` (advanced!)

Focuses on statistical distributions and parameter estimation:
- Simulates dice rolls and visualizes outcomes
- Explores maximum likelihood estimation (MLE) for:
  - Binomial parameters (log-likelihood functions)
  - Normal distribution (using `mle()` from `stats4`)
  - Linear regression models (via `mle` and `optim`)
- Compares MLE-based results to classical `lm()` regression

### 4. `for loop in action.R`

Explores control structures in R:
- Demonstrates nested and conditional `for` loops
- Applies loops for data inspection and transformation on `cars` and `iris` datasets
- Implements subset sampling and looped ANOVA testing
- Concludes with a `while` loop simulating repeated sampling under mean constraints

---

> These scripts aim to bridge the gap between statistical theory and reproducible R practice. For full reproducibility, ensure required packages (e.g., `tidyverse`, `psych`, `afex`, `ggstatsplot`, `lme4`) are installed.

## Folders

### 1. `R_Project`

Contains a modular pipeline for data processing and statistical analysis:

- **`dataPreperation.R`**:  
  Loads raw CSV data (`tramo1998etal_twins.csv`) using `haven` and `tidyverse`, performs basic cleaning (e.g., recoding gender variable), and saves the cleaned dataset as an `.rds` file for reproducible downstream use.

- **`dataAnalyses.R`**:  
  Loads the preprocessed data (`tramo1998etal_twins_clean.rds`) and conducts basic linear regression analysis using the built-in `cars` dataset. It demonstrates graphical output generation (`Cairo`), model fitting (`lm()`), and formatted reporting with `stargazer` and `report`.

> These scripts exemplify a typical R workflow: separating data preparation from analysis while leveraging reproducible outputs and clean structure.


### 2. `modular programming`

Implements a structured, modular [Quarto](https://quarto.org/) project for reproducible and scalable R workflows:

- **`advanced_modular_R_file.qmd`**:  
  A Quarto document demonstrating a way of **modular programming** for tasks such as data loading, cleaning, analysis, and visualization. It promotes script separation and functional reuse to enhance clarity and maintainability.

- **Folder structure:**
  - `data/`, `functions/`, `outputs/`, `code snippets/`: Logical compartments for raw data, user-defined functions, graphical outputs, and reusable code blocks.
  - `rsconnect/`: Configuration for deployment (e.g., Shiny or Quarto publishing) - *not shown here*.
  - `advanced_modular_R_file_files/`: Auto-generated folder for Quarto rendering.
  - `Library_subset.bib`: Contains bibliographic and SPSS data resources used in demonstrations.

> This project illustrates best practices in scientific programming: encapsulating code logic, externalizing functions, and using a reproducible markdown-based report as a high-level controller.