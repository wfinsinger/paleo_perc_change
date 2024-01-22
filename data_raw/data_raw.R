# File name: data_raw.r
# Loads raw data and saves it in the "./data_in/" folder as *.RData format.

rm(list = ls())

# Load new R function ---------------------------------------------------------
source("../R/neotoma_explorer_read.r")


# Load raw data ---------------------------------------------------------------

### Pollen harmonisation table ------------------------------------------------
## Source: https://figshare.com/ndownloader/files/26821949

if (!file.exists("Mottl_etal_EU_HarmonizationTable.xlsx")) {
  harm_url <- "https://figshare.com/ndownloader/files/26821949/Mottl_etal_EU_HarmonizationTable.xlsx"
  download.file(harm_url,
                "Mottl_etal_EU_HarmonizationTable.xlsx")
}
harm <- readxl::read_xlsx("Mottl_etal_EU_HarmonizationTable.xlsx",
                          sheet = 2)

#### Correct an error in the harmonization table
## as the "Cannabis sativa-type" is incorrectly assigned to "Fraxinus" (Row 318)
harm[318, 2] <- "Cannabis/Humulus"
harm <- dplyr::rename(harm, original_name = NeotomaTaxonName)


### Pollen data ---------------------------------------------------------------
## Source: https://doi.org/10.21233/cd0e-0190

if (!file.exists("AVG0702_dataset24094.csv")) {
  print("Download pollen data using the NeotomaExplorer App")
  print("(1): go to https://apps.neotomadb.org/Explorer/?datasetids=24094")
  print("(2): select the pollen record,")
  print("(3): click on the floppy-disc icon in the top-right corner to save
        the 'AVG0702_dataset24094.csv',")
  print("(4): move that file into the './data_in' folder.")
}
dset_pollen <- nxpl_read(dset_name = "AVG0702",
                         dset_path = "AVG0702_dataset24094.csv",
                         age_row = 6, first_datarow = 8)

## Modify data set, specifically:
## - transform 'Calibrated radiocarbon years BP' to 'CE years'
dset_pollen$list_ages$ages$age[is.na(dset_pollen$list_ages$ages$age)] <- 0
dset_pollen$list_ages$ages$age <- 1950 - dset_pollen$list_ages$ages$age
dset_pollen$age_units <- "CE years"



### Diatom data ------------------------------------------------
## From: Finsinger et al. (2006)
##  https://doi.org/10.1007/s10933-006-0002-x)
## Source: contributor

if (!file.exists("AVG0702_diatom_counts.csv")) {
  print("Fatal error: diatom dataset file is missing")
}
dset_diatoms <- nxpl_read(dset_name = "AVG0702",
                  dset_path = "AVG0702_diatom_counts.csv",
                  age_row = 6, first_datarow = 9)

## Modify data set, specifically:
## - transform 'Calibrated radiocarbon years BP' to 'CE years'
dset_diatoms$list_ages$ages$age[is.na(dset_diatoms$list_ages$ages$age)] <- 0
dset_diatoms$list_ages$ages$age <- 1950 - dset_diatoms$list_ages$ages$age
dset_diatoms$AgeUnits <- "CE years"


##### Load AVG0702 DI-TP ------------------------------------------------------
## From: Finsinger et al. (2006)
##  https://doi.org/10.1007/s10933-006-0002-x)
## Source: contributor

if (!file.exists("AVG-TP-LOG.csv")) {
  print("Fatal error: DI-TP dataset file is missing")
}
di_tp <- read.csv("AVG-TP-LOG.csv")

## Modify di_tp:
di_tp <- dplyr::rename(di_tp, "depth" = "Depth..cm.")
di_tp <- dplyr::rename(di_tp, "CE_years" = "Age.yrs.AD")
di_tp <- dplyr::rename(di_tp, "di_tp" = "TP")
# check for occurrence of NA values
if (sum(!complete.cases(di_tp)) > 0) {
  print("Warning: some rows contain missing values")
}

### Check depths and ages in the data sets ------------------------------------
if (all(di_tp$depth == dset_diatoms$list_ages$ages$depth) == FALSE) {
  print("Warning: depth values are different in the two data sets")
}
if (all(di_tp$CE_years == dset_diatoms$list_ages$ages$age) == FALSE) {
  print("Warning: age values are different in the two data sets")
}

# Write output ----------------------------------------------------------------
save(harm, file = "../data_in/harm.RData")
save(dset_pollen, file = "../data_in/dset_pollen.RData")
save(dset_diatoms, file = "../data_in/dset_diatoms.RData")
save(di_tp, file = "../data_in/di_tp.RData")

# END -------------------------------------------------------------------------
