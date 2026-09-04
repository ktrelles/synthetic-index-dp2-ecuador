# ============================================================

# SYNTHETIC DP2 INDEX — ECUADOR

# 02. DP2 BY DIMENSION

# ============================================================

library(dplyr)
library(readr)

# ------------------------------------------------------------

# Function to calculate the initial DP2 distance

# ------------------------------------------------------------

calculate_frechet_distance <- function(data, positive_vars, negative_vars) {

variables <- c(positive_vars, negative_vars)

# Distance from the ideal value

data <- data %>%
mutate(
across(
all_of(positive_vars),
~ abs(. - max(., na.rm = TRUE)),
.names = "d_{.col}"
),
across(
all_of(negative_vars),
~ abs(. - min(., na.rm = TRUE)),
.names = "d_{.col}"
)
)

# Standard deviation

data <- data %>%
mutate(
across(
all_of(variables),
~ sd(., na.rm = TRUE),
.names = "sd_{.col}"
)
)

# Standardized distances

data <- data %>%
mutate(
across(
starts_with("d_"),
~ . / get(
sub("^d_", "sd_", cur_column())
),
.names = "{.col}_std"
)
)

# Initial Frechet distance

data <- data %>%
mutate(
FD = rowSums(
across(ends_with("_std")),
na.rm = TRUE
)
)

return(data)
}

# ------------------------------------------------------------

# Iterative DP2 calculation

# ------------------------------------------------------------

calculate_dp2 <- function(data, variables, tolerance = 1e-5,
max_iter = 20) {

data <- data %>%
mutate(D0 = FD)

iteration <- 1
difference <- Inf

while (iteration <= max_iter && difference > tolerance) {

```
previous_D <- paste0("D", iteration - 1)
current_D  <- paste0("D", iteration)

# Correlation with the previous distance
correlations <- sapply(
  variables,
  function(variable) {
    cor(
      data[[previous_D]],
      data[[variable]],
      use = "complete.obs"
    )
  }
)

# Order variables by correlation
ordered_variables <- names(
  sort(correlations, decreasing = TRUE)
)

# Sequential regressions
r_squared <- numeric(length(ordered_variables))

for (i in seq_along(ordered_variables)) {

  if (i == 1) {
    r_squared[i] <- 0
  } else {

    dependent <- ordered_variables[i]
    predictors <- ordered_variables[1:(i - 1)]

    formula <- as.formula(
      paste(
        dependent,
        "~",
        paste(predictors, collapse = " + ")
      )
    )

    model <- lm(formula, data = data)

    r_squared[i] <- summary(model)$r.squared
  }
}

names(r_squared) <- ordered_variables

# Calculate corrected distances
for (variable in ordered_variables) {

  corrected_distance <-
    r_squared[variable] *
    data[[paste0(variable, "_std")]]

  data[[paste0(variable, "_corrected_", iteration)]] <-
    corrected_distance
}

corrected_columns <- paste0(
  ordered_variables,
  "_corrected_",
  iteration
)

data[[current_D]] <- rowSums(
  data[, corrected_columns, drop = FALSE],
  na.rm = TRUE
)

# Difference between consecutive iterations
difference_column <- paste0("difference_", iteration)

data[[difference_column]] <-
  data[[current_D]] - data[[previous_D]]

difference <- max(
  abs(data[[difference_column]]),
  na.rm = TRUE
)

message(
  "Iteration ", iteration,
  " | Maximum difference: ",
  round(difference, 8)
)

iteration <- iteration + 1
```

}

return(data)
}

# ============================================================

# HOUSEHOLDS

# ============================================================

households <- read_csv("data/processed/Households.csv")

household_positive <- c(
"H04_I",
"H0701_I",
"H0702_I",
"H1004_I",
"H1005_I",
"H1006_I",
"H1011_I"
)

household_negative <- c(
"H03_I",
"H06_I"
)

household_variables <- c(
household_positive,
household_negative
)

households <- calculate_frechet_distance(
households,
positive_vars = household_positive,
negative_vars = household_negative
)

households <- calculate_dp2(
households,
variables = household_variables
)

write_csv(
households,
"results/DP2_Households.csv"
)

# ============================================================

# POPULATION

# ============================================================

population <- read_csv("data/processed/Population.csv")

population_positive <- c(
"P19_I"
)

population_negative <- c(
"education_I",
"unemployment_I",
"illiteracy_I"
)

population_variables <- c(
population_positive,
population_negative
)

population <- calculate_frechet_distance(
population,
positive_vars = population_positive,
negative_vars = population_negative
)

population <- calculate_dp2(
population,
variables = population_variables
)

write_csv(
population,
"results/DP2_Population.csv"
)

# ============================================================

# HOUSING

# ============================================================

housing <- read_csv("data/processed/Housing.csv")

housing_positive <- c(
"V01_2_I",
"V04_I",
"V06_I",
"V08_I",
"V12_I",
"housing_deficit_I"
)

housing_negative <- c(
"V01_1_I",
"V03_I",
"V05_I",
"V07_I",
"V09_I",
"V10_I",
"V11_I",
"overcrowding_I"
)

housing_variables <- c(
housing_positive,
housing_negative
)

housing <- calculate_frechet_distance(
housing,
positive_vars = housing_positive,
negative_vars = housing_negative
)

housing <- calculate_dp2(
housing,
variables = housing_variables
)

write_csv(
housing,
"results/DP2_Housing.csv"
)

# ============================================================

# End of script

# ============================================================
