#/bin/bash
## Batch job for the R-script
echo "Update data for Data Disecter R-shiny app"
R CMD BATCH ~/git/esd_Rshiny/metnod/update-data.R
