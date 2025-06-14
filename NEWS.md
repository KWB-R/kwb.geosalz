# [kwb.geosalz 0.7.1](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.7.1) <small>2025-06-10</small>

* Plotting: filter out conductivity values (>20000 us) and improve x-axis labelling 
for combined measurement chain & operation plot (lable every 3 month instead of 
each month)

* Fix GH-actions workflow by bumping version numbers vor cache and checkout from 
v2 to v4

# [kwb.geosalz 0.7.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.7.0) <small>2024-03-19</small>

* Add combined plot for wells with measurement chains (i.e. 9,10,13) showing 
EC (top plot and daily abstraction rates of this well (middle plot) and total 
daily wellfield production rate (bottom plot). These plots were automatically 
created with the article [Measurement Chains](../articles/measurement-chains.html)
and uploaded to the same cloud folder where already the measurement chains data
and pdf files are located. For this the latest well operation export needs to be 
uploaded to the KWB cloud folder `../messketten/BWB_Brunnen_Prozessdaten`. 

# [kwb.geosalz 0.6.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.6.0) <small>2023-04-14</small>

* Fix GitHub Actions failure: 
    - Add dependency on kwb.nextcloud (required by vignette
    measurement-chains.Rmd)
    - Consider change of variable name in new Wasserportal API in vignette
    wasserportal.Rmd
* Fix bug in `create_sftp_connection()`: provide variable `con_vars` (again)
* Add some error handling to vignette measurement-chains.Rmd

# [kwb.geosalz 0.5.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.5.0) <small>2022-10-11</small>

* Bugfix: fix `read_lab_bwb()` leading to crash in case of importing `Hydrochemie_Gesamt_Geosalz_v1.0.6.xlsx`
(see here for [fix](https://github.com/KWB-R/kwb.geosalz/commit/9ea71f2eecaa1ba31ea34d780447969e47d317a8])


* Add `stop_if_duplicated_samples_found()` for `read_lab_bwb()` and `read_isotopes()` to 
throw error in case imported dataset contains duplicated `sample_id` 


**Articles**

- Added [Measurement Chains](../articles/measurement-chains.html) data import 
workflow for `electrical conductivity` and `temperature`, which will be installed 
in productions wells in September (K10: 2022-09-27)  and October 2022 (K9: 2022-10-17,
K13: 2022-10-16). Data in SFTP server is downloaded, aggregated and exported to 
the restricted KWB cloud each weekday at 07:00:00 UTC. (see [.github/pkgdown.yml#L11](https://github.com/KWB-R/kwb.geosalz/blob/master/.github/workflows/pkgdown.yaml#L11)).


# [kwb.geosalz 0.4.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.4.0) <small>2022-08-25</small>


**Functions**

* **Emshoff 91 .ods Import**: 
  - `create_emshoff91_import()`
  - `read_emshoff91_ods()` reads single `.ods` files
  -  `read_multiple_emshoff91_ods()` wrapper around `read_emshoff91_ods()` 
  reading multiple `.ods` files and returning a list

* **Hydrochemistry**: 

  - added `read_lab_bwb()` to import lab data of BWB and `read_isotopes()` to 
  import isotope data analysed by UFZ
  
  - added `get_phreeqc_data()` to filter out parameters imported by `read_lab_bwb()`
  which are required by PhreeqC. In order to transform these filtered data 
  the function `convert_phreeqc_input_to_wide()` can be used. Finally the filtered 
  data (as retrieved by `get_phreeqc_data()`) is used by `prepare_phreeqc_input()`
  for creating the PhreeqC input file.

* **Wasserportal**: added `convert_to_sf()` to convert xy-coordinates (i.e. UTM_33N 
with crs = 25833) to a spatial feature object (with crs = 4326)

**Articles**

- Added [Emshoff 91](../articles/emshoff91.html) data import of ods files (formerly 
saved in `Quattro Pro` (and converted with help of `LibreOffice (7.0)` on `Ubuntu` 
to `.ods` and `.xlsx`. However, latter not used due to possible mis-translation!  


- Added [Hydrochemistry](../articles/hydrochemistry.html) data preparation workflow 
of lab data for PhreeqC  

- Added [Wasserportal](../articles/wasserportal.html) data availability for southern 
part of SVM Friedrichshagen


# [kwb.geosalz 0.3.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.3.0) <small>2022-06-20</small>

Adding functionality for checking data availability for SVM Friedrichshagen based on available BWB data from pre-project phase (digitalised at KWB) and open-data from [Wasserportal Berlin](https://wasserportal.berlin.de)

**Functions**

* Add `convert_to_sf()` converting different CRS projections to `4326` (OSM map used as background map)


**Workflows**

* Added new workflow [Wasserportal](../articles/wasserportal.html) for  checking available data from [Wasserportal Berlin](https://wasserportal.berlin.de)

* Updated workflow [Data Availability WW Friedrichshagen](../articles/data-availability_fri.html) checking available data from pre-project phase (see: [workflow](../articles/workflow.html) article)


# [kwb.geosalz 0.2.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.2.0) <small>2022-06-01</small>

* Add `get_foerdermengen_gal_fri()` for getting annual abstraction values of 
well field galeries for waterworks Friedrichshagen

* Add workflow [Data Availability WW Friedrichshagen](../articles/data-availability_fri.html) checking available data 
from pre-project phase (see: [workflow](../articles/workflow.html) article)

# [kwb.geosalz 0.1.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.1.0) <small>2019-02-13</small>

* Release on Zenodo [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2563870.svg)](https://doi.org/10.5281/zenodo.2563870)


# kwb.geosalz 0.1.0.9000 <small>unreleased</small>

Wrapped [KWB-R/GeoSalz@88ae17](https://github.com/KWB-R/GeoSalz/commit/88ae176dda93ba7b8c1fea6895b00b119704ec13) 
into R package "kwb.geosalz" for better workflow documentation 


**Note:**
***Formalising the R scripts into the R package lead to major improvements (bug 
removal) due to the build-in "Check" functionality for R packages!***


* Integrated R functions
* Integrated "main.R" into "vignettes/workflow.Rmd" for documenting workflow

* see http://style.tidyverse.org/news.html for writing a good `NEWS.md`


