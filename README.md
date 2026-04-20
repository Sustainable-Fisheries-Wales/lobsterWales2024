### 2024 Welsh lobster stock assessment 
This repository contains code used for the 2024 exploratory stock assessment work (data processing and assessment model fitting using Stock Synthesis (SS3)) for the Welsh stock of European lobster (Homarus gammarus).

##### Time series of the spawning stock biomass (SSB), fishing mortality (F), relative SSB and F with Bmsy and Fsmy, recruitment, and landings of the Welsh lobster stock during 1983–2024.
<img src="https://github.com/Sustainable-Fisheries-Wales/lobsterWales2024/blob/main/plots/lobster2024.png?raw=true" width="500"> 

##### Kobe plot of SSB/Bmsy vs. F/Fsmy of the Welsh lobster stock during 1983–2024.
<img src="https://github.com/Sustainable-Fisheries-Wales/lobsterWales2024/blob/main/plots/lobster2024_kobeplot.png?raw=true" width="500"> 

#### File list

R (folder)

`observer_data.entrycheck_wales.R`

`observer_dataprocessing_wales.R`

`landings_data.processing_wales_lobster.R`

`cefas.lt.comp_dataprocessing_wales_lobster.R` 

`compute_cpue_wales_lobster.R`

`compute_observer.size.comp_wales_lobster.R`

`compute_observer.size.comp_lfd_wales_lobster.R`

`standardize_cpue_wales_lobster.R`

`ss_model_fitting_diagnostics.R`


#### Description

R: This folder contains R files for data processing and model-fitting

`observer_data.entrycheck_wales.R`: a script for assessing the structure and type of raw input data files

`observer_dataprocessing_wales.R`: a script for preprocessing the observer sampling data

`landings_data.processing_wales_lobster.R`: a script for preprocessing the iFISH landings data

`cefas.lt.comp_dataprocessing_wales_lobster.R`: a script for preprocessing the Cefas port sampling length composition data

`compute_cpue_wales_lobster.R`: a script for computing catch rate from the observer sampling data

`compute_observer.size.comp_wales_lobster.R`: a script for computing proportional length frequency distributions from the observer sampling data

`compute_observer.size.comp_lfd_wales_lobster.R`: a script for computing length frequency distributions weighted by cpue from the observer sampling data for ELEFAN growth parameter estimation

`standardize_cpue_wales_lobster.R`: a script for standardizing observer CPUE using vector autoregressive spatio-temporal (VAST) models 

`ss_model_fitting_diagnostics.R`: a script for running Stock Synthesis model-fitting and model diagnostics
