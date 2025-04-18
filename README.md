
# Replication Exercise

This repository contains materials related to the following publication:

**Ahmad, Saad, Nuno Limão, Sarah Oliver, and Serge Shikher (2023).**  
*"Brexit Uncertainty and Its (Dis)Service Effects."*  
*American Economic Journal: Economic Policy*, 15(4): 459–485.  
DOI: [https://doi.org/10.1257/pol.20200808](https://doi.org/10.1257/pol.20200808)

---

## 📄 Article Access

The original article is copyrighted by the American Economic Association and cannot be redistributed here.  
You can access the article and its appendices via the publisher’s website using the DOI above.

---

## 🔁 Replication Materials Attribution

The original replication package was downloaded from the ICPSR AEA Data Repository:  
[https://doi.org/10.3886/E173961V1](https://doi.org/10.3886/E173961V1)

All code and data are licensed under the Creative Commons Attribution 4.0 International License (CC BY 4.0).  
You may redistribute, adapt, and build upon this material for any purpose, even commercially, **provided appropriate credit is given**.

License details: [https://creativecommons.org/licenses/by/4.0/](https://creativecommons.org/licenses/by/4.0/)

---

## 📁 Repository Structure

This repository is organized into three folders:

- `Data/` — Raw data and dataset construction scripts  
- `Results - Original/` — Authors’ original code and output  
- `Results - Replication/` — Modified code and outputs, including robustness extensions

---

## 🛠 Execution Instructions

To replicate this study from scratch:

1. Clone the repository:
   ```bash
   git clone https://github.com/[your-username]/[repo-name]
   cd [repo-name]
   ```

2. Download these two large input datasets and place them in the `Data/` folder:
   - [`ITPD_E_R01.csv`](https://www.usitc.gov/data/gravity/itpd_e_r01.zip)
   - [`release_2.0_2000_2016.csv`](https://www.usitc.gov/data/gravity/release_2.0_2000_2016.zip)

3. Install the required Stata packages (in the Stata console):
   ```stata
   ssc install reghdfe, replace
   ssc install ppmlhdfe, replace
   ssc install hdfe, replace
   ssc install ftools, replace
   ssc install ranktest, replace
   ssc install ivreg2, replace
   ssc install ivreghdfe, replace
   ssc install outreg2, replace
   ssc install estout, replace
   ```

4. Run the following commands in your terminal or batch mode:
   ```bash
   cd Data
   stata -b do build_brexit_services_datasets_replication.do

   cd ../Results - Replication
   stata -b do main_figures_tables.do
   stata -b do appendix_figures_tables.do
   ```

---

## 📄 Replication Report

The full replication analysis, including robustness checks and methodological extensions, is available in:  
**`Ahmad_et_al_2023_Replication.pdf`**
