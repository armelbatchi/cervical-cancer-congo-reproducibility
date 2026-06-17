# Cervical cancer prevention and education gaps in the Republic of the Congo

This repository contains the reproducibility materials for a secondary public-data analysis of cervical cancer burden, prevention gaps, and readiness for WHO cervical cancer elimination targets in the Republic of the Congo.

## Contents

- `manuscript/cervical_cancer_congo_reproducible_manuscript.Rmd`: clean R Markdown manuscript and analysis workflow.
- `analysis/01_run_all.R`: R code extracted from the R Markdown file.
- `analysis/render_manuscript.R`: script to render the manuscript and regenerate outputs.
- `analysis/packages.txt`: R packages used by the workflow.
- `data/README.md`: data provenance and reuse notes.
- `metadata/.zenodo.json`: Zenodo metadata template.
- `CITATION.cff`: citation metadata template.
- `docs/DATA_AVAILABILITY_STATEMENT.txt`: text to paste into the journal manuscript after the Zenodo DOI is created.

## How to reproduce

1. Install R and RStudio.
2. Open the repository root folder.
3. Run:

```r
source("analysis/render_manuscript.R")
```

The manuscript and generated files will be written to `outputs/` and to the results folder created by the R Markdown workflow.

## Data sources

The analysis uses secondary public data only. Values are entered in the R Markdown workflow from official public sources described in the manuscript, including IARC GLOBOCAN, the ICO/IARC HPV Information Centre, WHO country profiles and WHO elimination targets, and public population or health-system indicators where used.

## Privacy

No individual-level data are included. No patient data, names, emails, addresses, or private institutional records are included.

## License

Code is released under the MIT License. Public-source indicators remain subject to the terms of the original data providers. Cite the original data providers when reusing the data.
