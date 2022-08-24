# [kwb.geosalz 0.4.0](https://github.com/KWB-R/kwb.geosalz/releases/tag/v0.4.0) <small>2022-08-22</small>


**Functions**

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

- Added [Wasserportal](../articles/wasserportal.html) data availability for southern 
part of SVM Friedrichshagen

- Added [Hydrochemistry](../articles/hydrochemistry.html) data preparation workflow 
of lab data for PhreeqC  


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


