# ============================================================

# SYNTHETIC DP2 INDEX — ECUADOR

# 01. DATA PREPARATION

# ============================================================

library(dplyr)
library(readr)

# ------------------------------------------------------------

# 1. File paths

# ------------------------------------------------------------

# Use relative paths so that the project is reproducible

# across different computers.

census_path <- "data/raw"
output_path <- "data/processed"

# ------------------------------------------------------------

# 2. Household data

# ------------------------------------------------------------

households <- read_csv(
file.path(census_path, "BDD_HOG_CPV2022_CANT.csv"),
locale = locale(encoding = "UTF-8")
)

households <- households %>%
mutate(
across(
c(H03, H04, H06, H0701, H0702, H0703,
H1004, H1005, H1006, H1011, CANTON, I10),
as.numeric
)
)

# Calculate indicators at the canton level

households <- households %>%
group_by(CANTON) %>%
mutate(
total_households = n(),

```
H03_count   = sum(H03 == 1, na.rm = TRUE),
H04_count   = sum(H04 == 1, na.rm = TRUE),
H06_count   = sum(H06 %in% c(3, 4), na.rm = TRUE),
H0701_count = sum(H0701 == 1, na.rm = TRUE),
H0702_count = sum(H0702 == 1, na.rm = TRUE),
H0703_count = sum(H0703 == 1, na.rm = TRUE),
H1004_count = sum(H1004 == 1, na.rm = TRUE),
H1005_count = sum(H1005 == 1, na.rm = TRUE),
H1006_count = sum(H1006 == 1, na.rm = TRUE),
H1011_count = sum(H1011 == 1, na.rm = TRUE)
```

) %>%
ungroup()

# Convert counts into proportions

households <- households %>%
mutate(
H03_I   = H03_count / total_households,
H04_I   = H04_count / total_households,
H06_I   = H06_count / total_households,
H0701_I = H0701_count / total_households,
H0702_I = H0702_count / total_households,
H0703_I = H0703_count / total_households,
H1004_I = H1004_count / total_households,
H1005_I = H1005_count / total_households,
H1006_I = H1006_count / total_households,
H1011_I = H1011_count / total_households
) %>%
distinct(CANTON, .keep_all = TRUE) %>%
select(
CANTON, I10,
H03_I, H04_I, H06_I, H0701_I, H0702_I,
H0703_I, H1004_I, H1005_I, H1006_I, H1011_I
)

write_csv(
households,
file.path(output_path, "Households.csv")
)

# ------------------------------------------------------------

# 3. Population data

# ------------------------------------------------------------

population <- read_csv(
file.path(census_path, "BDD_POB_CPV2022_CANT.csv"),
locale = locale(encoding = "UTF-8")
)

population <- population %>%
mutate(
across(
c(P15, P17R, P19, P22, P25, GEDAD, ETAEDAD,
ESCOLA, ANALF, CANTON, I10),
as.numeric
)
)

population <- population %>%
group_by(CANTON) %>%
mutate(
total_population = n(),

```
education_count =
  sum(P15 == 2 & ETAEDAD %in% c(1, 2), na.rm = TRUE),

P19_count =
  sum(P19 == 2, na.rm = TRUE),

unemployed_count =
  sum(P22 == 7 & P25 == 1, na.rm = TRUE),

illiteracy_count =
  sum(ANALF == 1, na.rm = TRUE)
```

) %>%
ungroup()

population <- population %>%
mutate(
education_I = education_count / total_population,
P19_I       = P19_count / total_population,
unemployment_I = unemployed_count / total_population,
illiteracy_I   = illiteracy_count / total_population
) %>%
distinct(CANTON, .keep_all = TRUE) %>%
select(
CANTON, I10,
education_I, P19_I, unemployment_I, illiteracy_I
)

write_csv(
population,
file.path(output_path, "Population.csv")
)

# ------------------------------------------------------------

# 4. Housing data

# ------------------------------------------------------------

housing <- read_csv(
file.path(census_path, "BDD_VIV_CPV2022_CANT.csv"),
locale = locale(encoding = "UTF-8")
)

housing <- housing %>%
mutate(
across(
c(V01, V03, V04, V05, V06, V07, V08, V09,
V10, V11, V12, V15, DEF_HAB, CANTON, I10),
as.numeric
)
)

housing <- housing %>%
mutate(
overcrowding = TOTPER / V15,
overcrowding_indicator = if_else(overcrowding > 3, 1, 0)
) %>%
group_by(CANTON) %>%
mutate(
V01_1_count = sum(V01 %in% c(4, 6, 7), na.rm = TRUE),
V01_2_count = sum(V01 == 19, na.rm = TRUE),
V03_count   = sum(V03 == 5, na.rm = TRUE),
V04_count   = sum(V04 == 1, na.rm = TRUE),
V05_count   = sum(V05 %in% c(6, 7, 8), na.rm = TRUE),
V06_count   = sum(V06 == 1, na.rm = TRUE),
V07_count   = sum(V07 %in% c(7, 8), na.rm = TRUE),
V08_count   = sum(V08 == 1, na.rm = TRUE),
V09_count   = sum(V09 == 4, na.rm = TRUE),
V10_count   = sum(V10 %in% c(4, 5), na.rm = TRUE),
V11_count   = sum(V11 %in% c(4, 5, 6, 7), na.rm = TRUE),
V12_count   = sum(V12 == 1, na.rm = TRUE),
overcrowding_count =
sum(overcrowding_indicator == 1, na.rm = TRUE),
housing_deficit_count =
sum(DEF_HAB == 1, na.rm = TRUE),
total_housing_units = n()
) %>%
ungroup()

housing <- housing %>%
mutate(
V01_1_I = V01_1_count / total_housing_units,
V01_2_I = V01_2_count / total_housing_units,
V03_I   = V03_count / total_housing_units,
V04_I   = V04_count / total_housing_units,
V05_I   = V05_count / total_housing_units,
V06_I   = V06_count / total_housing_units,
V07_I   = V07_count / total_housing_units,
V08_I   = V08_count / total_housing_units,
V09_I   = V09_count / total_housing_units,
V10_I   = V10_count / total_housing_units,
V11_I   = V11_count / total_housing_units,
V12_I   = V12_count / total_housing_units,
overcrowding_I =
overcrowding_count / total_housing_units,
housing_deficit_I =
housing_deficit_count / total_housing_units
) %>%
distinct(CANTON, .keep_all = TRUE) %>%
select(
CANTON, I10,
V01_1_I, V01_2_I, V03_I, V04_I, V05_I,
V06_I, V07_I, V08_I, V09_I, V10_I, V11_I,
V12_I, overcrowding_I, housing_deficit_I
)

write_csv(
housing,
file.path(output_path, "Housing.csv")
)

# ============================================================

# End of script

# ============================================================
