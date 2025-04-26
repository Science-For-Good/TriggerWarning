# Grant Termination Analysis

Analysis of terminated NIH and NSF grants containing "trigger terms" to determine how frequently these terms are used in scientific versus political/social contexts.

## Key Findings

- 60% of trigger term occurrences represent "false positives" - words used in legitimate scientific contexts
- Certain terms show particularly high false positive rates: "critical" (80%), "climate" (72%), "trans" (68%)
- Scientific fields like Biology and Physics had significantly higher false positive rates
- A significant spike in grant terminations occurred during 2022-2023

## Contents

- `data/`: Contains the raw data files
- `R/analysis/`: R scripts for analysis
- `R/style_guide/`: Style guide for visualizations
- `images/`: Generated visualizations and analysis results

## How to Run

1. Install required R packages: tidyverse, ggplot2, scales
2. Run the visualization script: `Rscript R/analysis/05_visualizations.R`
EOF
