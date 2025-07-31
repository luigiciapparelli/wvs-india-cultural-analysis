# Cultural Values in India – WVS Automation (R)

This repository contains the full analytical framework developed for the thesis:

**"Power Distance, Religiosity, and Economic Development in a Collectivist Society:  
A Cultural Analysis of India"**  
*(2025, Second Cycle Degree in Service Management, University of Bologna)*

The codebase enables:

- Harmonization of World Values Survey (WVS) data across multiple waves (1990–2020)
- Principal Component Analysis (PCA) on political trust and religious belief items
- Construction of two original indices:
  - **Power Distance Index (PDI)** – based on hierarchical authority perceptions
  - **Disbelief Index (DI)** – reflecting secularization and religious skepticism
- Time-series and correlation plotting to visualize cultural evolution
- Modular automation designed for future WVS data integration (e.g., Wave 8+)

---

## Repository Structure

| File/Folder                  | Description |
|-----------------------------|-------------|
| `scripts/preprocess_wvs.R`  | Cleans and harmonizes WVS raw data |
| `scripts/build_pdi_index.R` | Constructs Power Distance Index using PCA |
| `scripts/build_disbelief_index.R` | Constructs Disbelief Index via PCA |
| `scripts/plot_timeseries.R` | Generates cultural trend plots |
| `scripts/correlation_disbelief_gdp.R` | Correlates indices with economic variables |
| `outputs/`                  | Time-series and correlation graphs (PNG format) |
| `data/`                     | Example data (synthetic or anonymized for privacy) |
| `requirements.txt`          | Required R packages and versions |

---

## Data Sources

WVS raw data must be downloaded separately from:  
[https://www.worldvaluessurvey.org/WVSContents.jsp](https://www.worldvaluessurvey.org/WVSContents.jsp)

To protect licensing restrictions, only demo files or minimal samples are provided in `/data/`.

---

## Outputs

Final graphs are saved in `/outputs/`, including:
- Time-series of Power Distance and Disbelief indices
- Correlations with GDP per capita
- Cross-wave comparison of cultural dimensions

---

## Requirements

To replicate results:
- R ≥ 4.0
- Install packages listed in `requirements.txt` using:
```r
install.packages(readLines("requirements.txt"))
