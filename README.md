# TriggerWarning: Analysis of Terminated Federal Research Grants

A comprehensive analysis of recently terminated NIH and NSF grants containing specific "trigger terms" to determine how frequently these terms appear in specialized technical scientific contexts versus interdisciplinary social scientific contexts.

## Key Findings

- 40.9% of trigger term occurrences represent specialized technical scientific usage rather than interdisciplinary social scientific usage
- Certain terms show particularly high rates of specialized technical scientific usage: "racial inequality" (near 100%), "pregnant persons" (near 100%), "person-centered care" (near 100%)
- Scientific fields like Biology and Physics show distinct patterns of terminology usage in specialized technical contexts
- A dramatic spike in grant terminations occurred in March 2025 (1,827 grants), followed by a reduced but substantial number in April 2025 (562 grants)
- Research-intensive universities (R1) and schools of medicine bore the brunt of terminations
- Significant regional variations exist, with the Northeast experiencing the highest number of terminations (720)
- Different patterns emerged across NIH institutes (NIMH, NIMHD most affected) and NSF directorates (STEM Education, SBE most affected)

![Overall Context Analysis](output/figures/terms/overall_context_donut.png)

## Project Structure

```bash
TriggerWarning/
├── R/                      # R code directory
│   ├── analysis/           # Analysis scripts
│   │   ├── gemini_api_integration.R
│   │   └── nlp_integration.R
│   ├── data/               # Data processing utilities
│   │   ├── data_processing.R
│   │   └── utils.R
│   ├── main.R              # Main execution script
│   └── visualization/      # Visualization scripts by category
│       ├── agency_visualizations.R
│       ├── congressional_district_visualizations.R
│       ├── grant_type_visualizations.R
│       ├── institution_visualizations.R
│       ├── nlp_visualizations.R
│       ├── regional_visualizations.R
│       ├── styles/         # Visualization style guides
│       ├── term_analysis.R
│       └── time_visualizations.R
├── data/                   # Raw data files
│   ├── Terminated_Grants_Explorer_TAGGS.csv
│   ├── nih_terminations_airtable_20250429.csv
│   ├── nsf_terminations_airtable_20250425.csv
│   └── triggerterms.csv
└── output/                 # Output data and figures
    ├── data/               # Processed data files
    │   ├── congressional_district/
    │   ├── context_analysis/
    │   ├── grant_type/
    │   ├── institution/
    │   ├── nlp_analysis/
    │   └── term_counts/
    └── figures/            # Generated visualizations
        ├── agency/
        ├── grant_types/
        ├── institutions/
        ├── nlp/
        ├── regional/
        └── time/
```

## Methods
This project uses a multi-faceted methodology combining:

1. *Natural Language Processing*: Advanced text analysis techniques to process grant abstracts and identify term usage patterns
2. *AI-Assisted Context Analysis*: Integration with Google's Gemini API for contextual classification of terminology usage
3. *Multi-dimensional Analysis*: Examination of patterns across agencies, directorates, institutions, grant types, regions, and time

## Getting Started
### Prerequisites

Required R packages:

```r
install.packages(c("tidyverse", "ggplot2", "scales", "tidytext", "sf", "httr", "jsonlite"))
```

### Workflow

1. Process data:
```r
source("R/data/data_processing.R")
```

2. Run NLP analysis:

```r
source("R/analysis/nlp_integration.R")
```

3. Generate visualizations (by category or all):

```r
# Run all visualizations
source("R/main.R")

# Or run specific visualization scripts
source("R/visualization/agency_visualizations.R")
source("R/visualization/nlp_visualizations.R")
source("R/visualization/time_visualizations.R")
```

4. Generate report:
```r
rmarkdown::render("report.md")
```

## Output Reports
The analysis results are available in multiple formats:

- *HTML Report*: Interactive web version with navigation
- *PDF Report*: Printable document format
- *Word Document*: Editable format for further customization

## Project Team
This project is an initiative of Science For Good, dedicated to providing objective, data-driven analysis of science policy decisions and their impacts on the research community.


  