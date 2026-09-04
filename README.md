# Synthetic Index DP2: Cantonal Well-being in Ecuador

## Índice Sintético DP2: Bienestar Cantonal en Ecuador

[![R](https://img.shields.io/badge/R-4.x-blue)](https://www.r-project.org/)
[![Data](https://img.shields.io/badge/Data-Ecuador%20Census%202022-green)](https://www.ecuadorencifras.gob.ec/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## 🇬🇧 English

### Overview

This repository presents the construction of a **Synthetic DP2 Index** designed to measure relative cantonal well-being in Ecuador.

The index integrates three dimensions:

- 🏠 **Housing**
- 👥 **Population**
- 🏡 **Household**

The analysis is based on data from the **2022 National Census of Ecuador** and was implemented in **R**, using Principal Component Analysis (PCA) as part of the analytical framework.

The entire workflow was automated to promote:

- Reproducibility
- Transparency
- Scalability
- Territorial comparability

### 📊 Main Result

The analysis identifies the **10 cantons closest to the ideal value (1)** across the evaluated dimensions.

These cantons stand out due to relatively favorable conditions related to housing, demographic characteristics, and household contexts.

### 🎯 Why this index?

Synthetic territorial indicators can help to:

- Identify territories with relatively high levels of well-being.
- Detect territorial disparities.
- Compare territories using a multidimensional perspective.
- Support evidence-based public policy.
- Identify potential priority areas for intervention.
- Facilitate applied territorial research.

### 🔬 Analytical Workflow

```text
2022 National Census
        ↓
Data preparation
        ↓
Indicator construction
        ↓
Standardization
        ↓
Principal Component Analysis (PCA)
        ↓
DP2 Synthetic Index
        ↓
Distance to the ideal
        ↓
Cantonal ranking
        ↓
Top 10 visualization
