# ============================================================

# SYNTHETIC DP2 INDEX — ECUADOR

# 03. PCA AND VARIABLE SELECTION

# ============================================================

library(dplyr)
library(readr)
library(FactoMineR)
library(stringr)

# ------------------------------------------------------------

# Min-max normalization

# ------------------------------------------------------------

normalize <- function(x) {
(x - min(x, na.rm = TRUE)) /
(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# ------------------------------------------------------------

# PCA variable selection function

# ------------------------------------------------------------

select_pca_variables <- function(data, indicator_pattern) {

indicators <- names(data)[
str_detect(names(data), indicator_pattern)
]

data <- data %>%
mutate(
across(
all_of(indicators),
normalize,
.names = "norm_{.col}"
)
)

pca_data <- data %>%
select(starts_with("norm_"))

pca <- PCA(
pca_data,
scale.unit = TRUE,
ncp = ncol(pca_data),
graph = FALSE
)

cumulative_variance <- cumsum(pca$eig[, 2])

n_components <- which(
cumulative_variance >= 70
)[1]

pca_scores <- as.data.frame(
pca$ind$coord[, 1:n_components]
)

data <- bind_cols(data, pca_scores)

pca_variables <- names(data)[
str_detect(names(data), "^Dim\.")
]

correlation_data <- data %>%
select(all_of(c(indicators, pca_variables)))

correlation_matrix <- cor(
correlation_data,
use = "pairwise.complete.obs"
)

indicator_pca_correlations <-
correlation_matrix[indicators, pca_variables]

selected_variables <- rownames(
indicator_pca_correlations
)[
apply(
indicator_pca_correlations,
1,
function(x) any(x > 0.80)
)
]

return(
list(
data = data,
pca = pca,
selected_variables = selected_variables
)
)
}

# ============================================================

# HOUSEHOLDS

# ============================================================

households <- read_csv(
"data/processed/Households.csv"
)

household_pca <- select_pca_variables(
households,
indicator_pattern = "^H.*_I$"
)

print(household_pca$selected_variables)

# ============================================================

# HOUSING

# ============================================================

housing <- read_csv(
"data/processed/Housing.csv"
)

housing_pca <- select_pca_variables(
housing,
indicator_pattern = "^V.*_I$"
)

print(housing_pca$selected_variables)

# ============================================================

# POPULATION

# ============================================================

population <- read_csv(
"data/processed/Population.csv"
)

population_pca <- select_pca_variables(
population,
indicator_pattern = "^(education|P19|unemployment|illiteracy)_I$"
)

print(population_pca$selected_variables)

# ============================================================

# End of script

# ============================================================
