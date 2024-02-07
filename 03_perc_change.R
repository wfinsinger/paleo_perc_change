# Calculates and produces plots showing the:
# 1) the rate of change of the indicators as a percentage of change
#   per-unit-time to show how quickly the system changed throughout and its
#   direction;
#
# 2) the percentage of the inferred or estimated reference level (such as the
#   value in a pristine or at least much less impacted world, or the value in a
#   more impacted world) showing how much there is after, and if applicable
#   also before, the reference period. Thus, overall illustrating the status
#   relative to the reference period (or the reference value); and
# - if a reference period was specified, also the rate of change as the average
#   per unit-time (here a decade) percentage change in the indicator relative
#   to the reference period (or reference value), showing how quickly an
#   ecological indicator has changed over time since the reference period.

rm(list = ls())

# Load libraries & tools ------------------------------------------------------
library(dplyr)
source("./R/change_perc_relto.r")
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



# Calculate changes relative to the reference period (or ref. level) ----------

## Tree-cover change relative to 1970-1979 CE yrs:
pdf("./figures_out/Figure_3e_trsh_1970-1979.pdf",
    height = 5, fonts = "Helvetica")
trsh_relto_70s <- change_perc_relto(data_source_ecolind = trsh2,
                                   data_source_ages = trsh_ages,
                                   reference_interval = c(1970, 1979),
                                   reference_level_estimated = NULL,
                                   bin_size = 10,
                                   ecolind_name = "Arboreal pollen",
                                   desired_direction = "increase",
                                   age_scale = "CE years",
                                   plotit = TRUE)
dev.off()


## Tree-cover change relative to 1940-1950 CE yrs:
pdf("./figures_out/Figure_A2_trsh_1940-1950.pdf", height = 5)
trsh_relto_40s <- change_perc_relto(data_source_ecolind = trsh2,
                                   data_source_ages = trsh_ages,
                                   reference_interval = c(1940, 1950),
                                   reference_level_estimated = NULL,
                                   bin_size = 10,
                                   ecolind_name = "Arboreal pollen",
                                   desired_direction = "increase",
                                   age_scale = "CE years",
                                   plotit = TRUE)
dev.off()

## Tree-cover change relative to an arbitrarily chosen value of 60%:
trsh_relto_60perc <- change_perc_relto(data_source_ecolind = trsh2,
                                      data_source_ages = trsh_ages,
                                      reference_interval = NULL,
                                      reference_level_estimated = 60,
                                      reference_level_desc = "% tree cover",
                                      bin_size = 10,
                                      ecolind_name = "Arboreal pollen",
                                      desired_direction = "increase",
                                      age_scale = "CE years",
                                      plotit = TRUE)

## Di-TP change relative to 1800-1899 CE yrs:
pdf("./figures_out/Figure_3f_ditp_1800-1899.pdf", height = 5)
ditp_relto_19th <- change_perc_relto(data_source_ecolind = tp,
                                    data_source_ages = tp_ages,
                                    reference_interval = c(1800, 1899),
                                    reference_level_estimated = NULL,
                                    bin_size = 10,
                                    ecolind_name = "DI-TP",
                                    desired_direction = "decrease",
                                    age_scale = "CE years",
                                    plotit = TRUE)
dev.off()


## END ------------------------------------------------------------------------
