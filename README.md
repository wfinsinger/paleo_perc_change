<a href="https://doi.org/10.5281/zenodo.10075146"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.10075146.svg" alt="DOI"></a>


This repository contains data and code necessary to reproduce the results shown 
in the manuscript "Rates of palaeoecological change can inform ecosystem 
restoration" (https://doi.org/10.5194/egusphere-2023-2623 and https://doi.org/10.5194/bg-21-1629-2024).

The repository includes:
- a script to get the raw data (see folder "data_raw") that are required for 
    further data analyses into shape;
- three scripts to reproduce the Figures shown in the manuscript ("01_", "02_",
    and "03_").
- In addition, it includes:
  - functions to read files that were manually downloaded from the Neotoma
    Explorer App (https://apps.neotomadb.org/explorer/) into R, and reshape them
    further data analyses, and
  - 'utility' functions that are required to run a function that was modified
    from the R-Ratepol package (Mottl et al., 2021), as the 'utility' functions
    are not exported by the R-Ratepol package.
