# ============================================================

# SYNTHETIC DP2 INDEX — ECUADOR

# 04. GLOBAL SYNTHETIC INDEX

# ============================================================

library(dplyr)
library(readr)

# ------------------------------------------------------------

# Load processed datasets

# ------------------------------------------------------------

housing <- read_csv(
"data/processed/Housing.csv"
)

households <- read_csv(
"data/processed/Households.csv"
)

population <- read_csv(
"data/processed/Population.csv"
)

# ------------------------------------------------------------

# Merge the three dimensions

# ------------------------------------------------------------

global_data <- housing %>%
left_join(
households,
by = "CANTON"
) %>%
left_join(
population,
by = "CANTON"
)

# ------------------------------------------------------------

# Variables selected for the global DP2 index

# ------------------------------------------------------------

positive_variables <- c(
"H04_I",
"H0701_I",
"H0702_I",
"H1004_I",
"H1005_I",
"H1006_I",
"H1011_I",
"P19_I"
)

negative_variables <- c(
"H06_I",
"education_I",
"illiteracy_I",
"V11_I"
)

global_variables <- c(
positive_variables,
negative_variables
)

# ------------------------------------------------------------

# Calculate Frechet distance

# ------------------------------------------------------------

global_data <- calculate_frechet_distance(
global_data,
positive_vars = positive_variables,
negative_vars = negative_variables
)

# ------------------------------------------------------------

# Calculate global DP2 index

# ------------------------------------------------------------

global_data <- calculate_dp2(
global_data,
variables = global_variables
)

# ------------------------------------------------------------

# Final results

# ------------------------------------------------------------

final_distance <- max(
names(global_data)[
grepl("^D[0-9]+$", names(global_data))
]
)

global_results <- global_data %>%
select(
CANTON,
I10,
all_of(final_distance)
) %>%
rename(
DP2_Index = all_of(final_distance)
) %>%
arrange(DP2_Index)

# ------------------------------------------------------------

# Distance to the ideal

# ------------------------------------------------------------

global_results <- global_results %>%
mutate(
Ideal_Distance =
max(DP2_Index, na.rm = TRUE) - DP2_Index
) %>%
arrange(Ideal_Distance)

# ------------------------------------------------------------

# Top 10 cantons closest to the ideal

# ------------------------------------------------------------

top_10 <- global_results %>%
slice_head(n = 10)

print(top_10)

# ------------------------------------------------------------

# Save results

# ------------------------------------------------------------

write_csv(
global_results,
"results/Global_DP2_Index.csv"
)

write_csv(
top_10,
"results/Top_10_Cantons.csv"
)

# ============================================================

# End of script

# ============================================================
