# Cultural Values in India – WVS Automation (R)

This repository contains R scripts used in the thesis **"Power Distance, Religiosity, and Economic Development in a Collectivist Society:
A Cultural Analysis of India"** (2025, University of Bologna) to:

- Harmonize World Values Survey (WVS) data across waves
- Perform Principal Component Analysis (PCA) on political trust and religious belief items
- Construct Power Distance and Disbelief indices
- Visualize time-series and correlation trends
- Enable automated replication with future WVS releases

## Structure

- `preprocess_wvs.R`: Cleans and harmonizes WVS data across waves
- `pdi_index_pca.R`: Constructs a Power Distance Index using PCA
- `disbelief_index_pca.R`: Builds a Disbelief Index using PCA
- `timeseries_plots.R`: Produces line plots over time
- `correlation_plots.R`: Scatterplots with GDP and cultural indices

## Data

Located in https://www.worldvaluessurvey.org/WVSContents.jsp

## Outputs

Graphs saved as .png files.

## Requirements

To ensure compatibility read requirements.txt

## Citation

> Ciapparelli, L. (2025). *Power Distance, Religiosity, and Economic Development in a Collectivist Society:
A Cultural Analysis of India*. Second Cycle Degree/Two Year Master in Service Management, University of Bologna. [GitHub Repository](https://github.com/luigiciapparelli/wvs-india-cultural-analysis)
