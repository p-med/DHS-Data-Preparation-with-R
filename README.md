# Mozambique DHS 2022 Data Processing Pipeline

A comprehensive R script for processing and aggregating Demographic and Health Surveys (DHS) data from Mozambique's 2022 survey to cluster-level indicators suitable for spatial analysis and modeling.

## Overview

This script transforms individual and household-level DHS survey data into cluster-level aggregated indicators, enabling spatial analysis of health and demographic outcomes across Mozambique. The pipeline handles complex survey structures, applies proper weighting, and produces analysis-ready datasets with geographic coordinates.

## Features

- **Multi-Module Integration**: Merges data from 13+ DHS modules (household, women, men, children, geographic)
- **Standardized Indicators**: Calculates internationally comparable health and demographic indicators following DHS methodology
- **Survey Weights**: Properly applies household and individual sampling weights for representative estimates
- **Spatial Integration**: Includes cluster coordinates for GIS analysis and mapping
- **Reproducible Pipeline**: Clear documentation and modular structure for transparency and replication

## Calculated Indicators

### Maternal and Reproductive Health
- Women married by age 15 and 18 (ages 20-24)
- Age at first birth (women 20-24, 25-29)
- Skilled delivery assistance
- Facility-based deliveries

### Child Health and Nutrition
- Child stunting prevalence (height-for-age z-score < -2 SD)
- Birth registration (children under 5)

### Literacy
- Women's literacy rates (ages 15-49)
- Men's literacy rates (ages 15-49)

### Gender-Based Violence
- Physical, sexual, and emotional violence by intimate partners (past 12 months)
- Composite indicator for any violence

### Family Planning
- Current use of any contraceptive method

## Requirements

### R Packages
```r
tidyverse      # Data manipulation and visualization
haven          # DHS labeled variables
labelled       # Variable labeling
expss          # Tables with labeled data
openxlsx       # Excel export
naniar         # Missing data handling
here           # Relative file paths
survey         # Survey-weighted statistics
foreign        # DBF file import
stringr        # String manipulation
```

### Input Data
- **DHS Individual Recode (IR)**: Women's questionnaire data (MZIQ80 files)
- **DHS Men's Recode (MR)**: Men's questionnaire data (MZMR80 files)
- **DHS Geographic Data (GE)**: Cluster coordinates (MZGE81FL.dbf)

Data must be obtained from the [DHS Program](https://dhsprogram.com/data/available-datasets.cfm) with proper authorization.

## Usage

### 1. Data Setup
```r
# Place DHS data files in the following structure:
project/
├── Data/
│   ├── MZIQ80/          # Women's recode files
│   ├── MZMR80/          # Men's recode files
│   └── GE_File/         # Geographic data
└── Data_Prep_MZ2022DHS.R
```

### 2. Run the Script
```r
source("Data_Prep_MZ2022DHS.R")
```

### 3. Output
The script generates `MZ2022DHS_Indicators.csv` containing:
- One row per cluster
- Numerator and denominator for each indicator
- Survey weights
- Geographic coordinates (latitude/longitude)
- Cluster metadata (urban/rural classification, survey identifiers)

## Data Structure

### Output Variables
Each indicator follows the pattern:
- `[INDICATOR]_num`: Numerator (count of individuals meeting criteria)
- `[INDICATOR]_den`: Denominator (eligible population)
- Calculate prevalence as: `(num / den) * 100`

### Key Variables
- `DHSCLUST`: Cluster ID (primary key)
- `LATNUM`, `LONGNUM`: Cluster coordinates (GPS)
- `URBAN_RURA`: Urban/rural classification
- `wt`: Survey weight for cluster
- `survey_id`: Survey identifier (MZ2022DHS)

## Methodology Notes

### Survey Weighting
- Household weights (HV005) and individual weights (V005) are converted from integer format by dividing by 1,000,000 (DHS standard)
- Cluster-level weights are preserved for proper statistical analysis

### Denominators
Each indicator uses appropriate denominators following DHS methodology:
- Marriage indicators: Women ages 20-24
- First birth indicators: Women ages 20-24 or 25-29 (as specified)
- Literacy: All women/men ages 15-49
- Violence: Ever-married women or women with intimate partner history
- Contraception: All women of reproductive age

### Missing Data
- NA values are replaced with 0 after aggregation to handle clusters with no observations
- Original missing codes from DHS data are preserved during indicator calculation

## Applications

This processed dataset is suitable for:
- Spatial analysis and hotspot detection
- Geographic correlation studies
- Machine learning and predictive modeling
- Multi-level regression analysis
- GIS mapping and visualization
- Policy targeting and resource allocation

## Limitations

- Cluster coordinates are displaced for confidentiality (DHS standard: ±0-5 km)
- Small sample sizes in some clusters may affect indicator reliability
- Temporal snapshot (single cross-sectional survey)

## Citation

If using this script or derivative data, please cite:
```
IMASIDA 2022: Inquérito de Indicadores de Imunização, Malária e HIV/SIDA em Moçambique
Ministério da Saúde (MISAU), Instituto Nacional de Estatística (INE), e ICF. 2023.
Maputo, Moçambique.
```

## Author

**Paulo Medina**  
Geospatial Data Scientist | MS Geographic Information Sciences  
University of Maryland

## License

This script is provided for research and educational purposes. DHS data usage is subject to the terms and conditions of the DHS Program.

## Contact

For questions or collaboration opportunities, please open an issue or reach out via GitHub.

---

**Note**: This script processes sensitive health data. Ensure compliance with all ethical guidelines and data use agreements when working with DHS data.