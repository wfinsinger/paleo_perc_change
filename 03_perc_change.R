# First, the rate of change shows how quickly an ecological indicator is
# changing over time;
# the average per decade change in the indicator is expressed as
# - a percentage of the estimated value for 1970 (or, if later, for the
# beginning of the timeseries).

# Second, the current status is shown as
# - a percentage of the inferred or estimated natural baseline level (i.e.,
# the value in a pristine or at least much less impacted – e.g.,
# pre-industrial–world), showing how much remains (see Figures 2.2.8–2.2.20).



rm(list = ls())

# Load libraries & tools ------------------------------------------------------
library(dplyr)
#devtools::install_github("HOPE-UIB-BIO/R-Ratepol-package")
# library(RRatepol)
# example_data <- RRatepol::example_data
source("./R/change_perc_baseline.r")
source("./R/change_perc_stepwise.r")


# Load datasets ---------------------------------------------------------------
load("./data_out/01_AVG0702_trsh.rds")
trsh <- dset_harm_grps
trsh2 <- dplyr::select(trsh, sample_id, sum_trsh)
rm(dset_harm_grps, trsh)

load("./data_out/01_AVG0702_trsh_ages.rds")
trsh_ages <- dset_harm_ages
rm(dset_harm_ages)

load("./data_out/02_di_tp.rds")
tp <- di_tp_exp
rm(di_tp_exp)

load("./data_out/02_di_tp_ages.rds")
tp_ages <- dset_perc_ages
rm(dset_perc_ages)


## Calculate percentage change per time interval (here, a decade) -------------

## Tree-cover % change per decade (with whiskers and standardized):
pdf("./figures_out/Figure_3c_trsh_stepwise.pdf", height = 5)
trsh_stepwise <- change_perc_stepwise(data_source_ecolind = trsh2,
                                      data_source_ages = trsh_ages,
                                      ecolind_name = "Arboreal pollen",
                                      age_scale = "CE years",
                                      change_by = 10, n_shift = 4, n_rand = 99,
                                      add_whiskers = TRUE,
                                      time_standardisation = 10)
dev.off()


## Tree-cover % change per decade (w/o whiskers):
trsh_stepwise <- change_perc_stepwise(data_source_ecolind = trsh2,
                                      data_source_ages = trsh_ages,
                                      ecolind_name = "Arboreal pollen",
                                      age_scale = "CE years",
                                      change_by = 10, n_shift = 4, n_rand = 99,
                                      add_whiskers = FALSE,
                                      time_standardisation = 10)


## Tree-cover % change per decade (w/o whiskers and not standardized):
trsh_stepwise <- change_perc_stepwise(data_source_ecolind = trsh2,
                                      data_source_ages = trsh_ages,
                                      ecolind_name = "Arboreal pollen",
                                      age_scale = "CE years",
                                      change_by = 10, n_shift = 4, n_rand = 99,
                                      add_whiskers = FALSE,
                                      time_standardisation = NULL)


## Tree-cover % change per decade (with whiskers and not standardized):
trsh_stepwise <- change_perc_stepwise(data_source_ecolind = trsh2,
                                      data_source_ages = trsh_ages,
                                      ecolind_name = "Arboreal pollen",
                                      age_scale = "CE years",
                                      change_by = 10, n_shift = 4, n_rand = 99,
                                      add_whiskers = TRUE,
                                      time_standardisation = NULL)


## DI-TP % change per decade (with whiskers and standardized):
pdf("./figures_out/Figure_3d_ditp_stepwise.pdf", height = 5)
ditp_stepwise <- change_perc_stepwise(data_source_ecolind = tp,
                                      data_source_ages = tp_ages,
                                      ecolind_name = "DI-TP",
                                      age_scale = "CE years",
                                      change_by = 10, n_shift = 4, n_rand = 99,
                                      add_whiskers = TRUE,
                                      time_standardisation = 10)
dev.off()


## DI-TP % change per decade (w/o whiskers and standardized):
ditp_stepwise <- change_perc_stepwise(data_source_ecolind = tp,
                                      data_source_ages = tp_ages,
                                      ecolind_name = "DI-TP",
                                      age_scale = "CE years",
                                      change_by = 10, n_shift = 4, n_rand = 99,
                                      add_whiskers = FALSE,
                                      time_standardisation = 10)



# Calculate percentage of the inferred or estimated baseline level ------------

## Tree-cover % change relative to 1970-1979 CE yrs:
pdf("./figures_out/Figure_3e_trsh_1970-1979.pdf",
    height = 5, fonts = "Helvetica")
trsh_baseline <- change_perc_baseln(data_source_ecolind = trsh2,
                                          data_source_ages = trsh_ages,
                                          baseln_interval = c(1970, 1979),
                                          baseln_estimated = NULL,
                                          bin_size = 10,
                                          ecolind_name = "Arboreal pollen",
                                          desired_direction = "increase",
                                          age_scale = "CE years",
                                          plotit = TRUE)
dev.off()


## Tree-cover % change relative to 1940-1950 CE yrs:
pdf("./figures_out/Figure_A2_trsh_1940-1950.pdf", height = 5)
trsh_baseline <- change_perc_baseln(data_source_ecolind = trsh2,
                                    data_source_ages = trsh_ages,
                                    baseln_interval = c(1940, 1950),
                                    baseln_estimated = NULL,
                                    bin_size = 10,
                                    ecolind_name = "Arboreal pollen",
                                    desired_direction = "increase",
                                    age_scale = "CE years",
                                    plotit = TRUE)
dev.off()


## Di-TP % change relative to 1800-1899 CE yrs:
pdf("./figures_out/Figure_3f_ditp_1800-1899.pdf", height = 5)
ditp_baseline <- change_perc_baseln(data_source_ecolind = tp,
                                          data_source_ages = tp_ages,
                                          baseln_interval = c(1800, 1899),
                                          baseln_estimated = NULL,
                                          bin_size = 10,
                                          ecolind_name = "DI-TP",
                                          desired_direction = "decrease",
                                          age_scale = "CE years",
                                          plotit = TRUE)
dev.off()


## END ------------------------------------------------------------------------
