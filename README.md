# Code accompanying the manuscript
This repository contains the R code used for the analyses reported in the manuscript 
Mental health support seeking behaviour during during hot weatherin summertime weather: Daily time-series regression analysis of almost 2 million calls made to NHS mental health hotlines in England
Prof Gesche M. Huebner (PhD)1*, Kai Wan (PhD)2, Charles H. Simpson (DPhil)3, Prof Clare Heaviside (PhD)3, Prof Anna Mavrogianni (PhD)3, Prof Shakoor Hajat (PhD)2
, submitted to *The Lancet Planetary Health*.

The analyses examine associations between environmental exposures, including temperature and solar radiation, and daily mental health hotline call counts across NHS trusts in England. 
The modelling workflow includes trust-specific distributed lag non-linear models (DLNMs), second-stage meta-analysis, sensitivity analyses, and generation of manuscript figures.

## Repository contents

| File | Description |
|---|---|
| `1_data_prep_manuscript.R` | Prepares and merges the hotline and environmental data, derives analysis variables, restricts the main analysis to the warm season (May–September), applies data-quality and outlier procedures, and produces descriptive summaries and additional datasets used in sensitivity analyses. |
| `2.stage1_DLNM_MtS_manuscript.R` | Conducts the first-stage trust-specific analyses. This includes model selection, DLNM estimation for temperature and solar radiation, extraction of relative risks and coefficients/variance-covariance matrices, and sensitivity analyses for alternative lags, exclusion of solar radiation, the post-COVID period, and relative humidity. |
| `3.stage2_meta_manuscript.R` | Conducts the second-stage meta-analysis using `mixmeta`, pooling trust-specific associations and generating pooled exposure-response estimates for the main and sensitivity analyses. |
| `4.plot_manuscript.R` | Generates the main and sensitivity-analysis figures from the trust-specific and pooled model results. |

## Data availability

The individual-level/source analysis data required to run these scripts are **not included in this repository** because they are subject to data access and sharing restrictions.

Researchers with authorised access to the underlying data can use these scripts to reproduce the analysis after arranging the input files in the expected folder structure. 
Details of the data sources and access arrangements should be obtained from the data availability statement in the manuscript.

No confidential or restricted data should be uploaded to this repository.

## Expected directory structure

The scripts use relative file paths and expect directories similar to the following:

```text
repository/
├── 1_data_prep_manuscript.R
├── 2.stage1_DLNM_MtS_manuscript.R
├── 3.stage2_meta_manuscript.R
├── 4.plot_manuscript.R
├── input/
│   ├── combined_data_daily_hotlines.csv
│   ├── weighted_met_variables/
│   └── nhs-trusts-to-regions-lookup/
└── output/
    └── warm months/
        └── sensitivity_analysis/
```

The `input/` directory and restricted data files are not distributed with this repository.

## Software and R packages

The analysis was conducted in R.

The scripts use the following R packages: `dplyr`, `tidyr`, `lubridate`, `ggplot2`, `dlnm`, `data.table`, `mixmeta`, `paletteer`, `cowplot`, and `ggpubr`, together with the base/recommended R package `splines`.

## Running the analysis

Where the required data and intermediate files are available, the intended order of the scripts is:

1. `1_data_prep_manuscript.R`
2. `2.stage1_DLNM_MtS_manuscript.R`
3. `3.stage2_meta_manuscript.R`
4. `4.plot_manuscript.R`

Please note that these scripts reflect the analysis workflow used for the manuscript. Some commands that write intermediate files are commented out in the current scripts, and later scripts may therefore expect intermediate files that were generated during the original analysis. Users reproducing the workflow from the raw authorised data may need to uncomment the relevant `write.csv()` commands and create the required `output/` directories.


## Contact

For questions about the analysis or code, please contact:

Kai Wan
Kai.Wan@lshtm.ac.uk
London School of Hygiene and Tropical Medicine
