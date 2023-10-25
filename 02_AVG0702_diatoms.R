
# load("./data_out/02_AVG0702_diatoms.rda")

rm(list = ls())

# Load data & tools -----------------------------------------------------------

### Libraries -----------------------------------------------------------------
library(rioja)
library(dplyr)
#devtools::install_github("HOPE-UIB-BIO/R-Ratepol-package")
library(RRatepol)


### R functions from source ---------------------------------------------------

## New R functions
source("./R/neotoma_explorer_read.r")
source("./R/neotoma_explorer_perc.r")

## Modified from RRatepol v1.2.1
source("./R/fc_plot_RoC_seq4.r")

## As of RRatepol v1.2.1
source("./R/util_make_trend.r")
source("./R/util_check_class.r")
source("./R/util_check_col_names.r")
source("./R/util_check_vector_values.r")
source("./R/util_paste_as_vector.r")
source("./R/util_output_comment.r")


### Load data -----------------------------------------------------------------

##### Load AVG0702 diatom counts and sample ages ------------------------------
load("./data_in/dset_diatoms.RData")
dset <- dset_diatoms

##### Load AVG0702 DI-TP ------------------------------------------------------
load("./data_in/di_tp2.RData")



# Plot resolution of the record -----------------------------------------------
dset_min_age <- min(dset$list_ages$ages$age)
dset_resol <- abs(c(diff(dset$list_ages$ages$age), NA))
summary(dset_resol)

pdf("./figures_out/Figure_A1b_diatoms_resol.pdf")
par(fig = c(0, 0.9, 0, 1), mar = c(5,6,5,3))
plot(dset$list_ages$ages$age, dset_resol, type = "h", lwd = 2,
     xlim = c(1840, 2010), ylim = NULL,
     xlab = "Age (CE yrs)",
     ylab = "Resolution\n (years between samples)",
     main = "AVG-diatoms")
par(fig = c(0.65, 1, 0, 1), new = TRUE)
boxplot(dset_resol, yaxt = "n")
axis(4)
dev.off()
layout(1)


# Transform data --------------------------------------------------------------

### Calculate diatom percentages ----------------------------------------------
dset_perc <- nxpl_perc(d_in = dset, d_sum = NULL,
                           plotit = F,
                           x_lim = c(120, -60))

rioja::strat.plot(dset_perc$perc[ ,40:60],
                  yvar = dset_perc$list_ages$ages$age,
                  scale.percent = TRUE,
                  y.rev = FALSE, plot.poly = TRUE, exag = TRUE, exag.mult = 5,
                  cex.xlabel = 0.7)



# Plot synthetic diatom % diagram ---------------------------------------------
d_perc <- dplyr::select(dset_perc$perc, c("Cyclotella cyclopuncta",
                                         "Cyclotella comensis",
                                         "Fragilaria crotonensis",
                                         "Asterionella formosa",
                                         "Stephanodiscus parvus",
                                         "Cyclostephanos invisitatus",
                                         "Cyclostephanos dubius",
                                         "Cyclotella rossii",
                                         "Cyclotella ocellata",
                                         "Fragilaria brevistriata"))


pdf("./figures_out/Figure_2b_AVG0702_diatoms.pdf")
rioja::strat.plot(d_perc,
                  yvar = dset$list_ages$ages$age,
                  scale.percent = TRUE, plot.poly = TRUE, plot.bar = FALSE,
                  col.poly = "blue",
                  y.rev = TRUE, cex.xlabel = 0.7,
                  wa.order = "none", ylabel = "Age (CE years)")
dev.off()


# Prepare data.frame for output -----------------------------------------------
di_tp_exp <- data.frame(dset_perc$counts$sample_id,
                        di_tp2$`TP (log)`)
names(di_tp_exp) <- c("sample_id", "di_tp")



# Calculate RoC with RRatepol -------------------------------------------------

n_indiv_min <- min(rowSums(dset$counts[2:dim(dset$counts)[2]]))

### RRatepol settings ---------------------------------------------------------
age.uncertainty <- NULL
smooth.method <- "age.w" # as of Mottl et al. (2021)
smooth.N.points <- 1
smooth.age.range <- max(dset_resol, na.rm = T) + 1 # there's at least one data point
                                                  # for each smoothing window.
DC.metric <- "chisq"  # as of Mottl et al. (2021)
Working.Units <- "MW"  # as of Mottl et al. (2021)
bin.size <- max(dset_resol, na.rm = T) + 1 # bin.size <- 50 in of Mottl et al. (2021)
Number.of.shifts <- 5  # as of Mottl et al. (2021)
bin.selection <- "random"
standardise.user <- TRUE
N.individuals <- 195 ## as of 01_AVG0702_pollen.R
n.rand <- 999 # n.rand <- 1000 in Mottl et al. (2021) !!
time.standardisation <- 50


### RoC RRatepol - All taxa ---------------------------------------------------

dset_roc <-
  RRatepol::estimate_roc(data_source_community = dset_perc$counts,
                            data_source_age = dset_perc$list_ages$ages[ ,c(1,3)],
                            age_uncertainty = age.uncertainty,
                            smooth_method = smooth.method,
                            smooth_n_points = smooth.N.points,
                            smooth_age_range = smooth.age.range,
                            dissimilarity_coefficient = DC.metric,
                            working_units = Working.Units,
                            bin_size = bin.size,
                            number_of_shifts = Number.of.shifts,
                            bin_selection = bin.selection,
                            standardise = standardise.user,
                            n_individuals = N.individuals,
                            tranform_to_proportions = TRUE,
                            rand = n.rand,
                            interest_threshold = NULL,
                            time_standardisation = time.standardisation,
                            verbose = F)


dset_peak <-
  RRatepol::fc_detect_peak_points(dset_roc, sel_method = "threshold")

RRatepol::plot_roc(dset_peak, age_threshold = NULL,
                 roc_threshold = max(dset_roc$ROC),
                 peaks = TRUE,
                 trend = "threshold")

pdf("./figures_out/Figure_3b_RoC_AVG_diatoms.pdf")
fc_plot_RoC_seq4(dset_peak, age_threshold = NULL, age_scale = "CE",
                 Roc_threshold = max(dset_roc$ROC),
                 Peaks = TRUE,
                 trend = "threshold",
                 cvar = di_tp2, cvar_col = "blue", aside = TRUE)
dev.off()


# Output - save data ---------------------------------------------------------
write.csv(di_tp_exp, "./data_out/02_di_tp.csv",
          row.names = FALSE)
write.csv(dset_perc$list_ages$ages, "./data_out/02_di_tp_ages.csv",
          row.names = FALSE)

save.image("./data_out/02_AVG0702_diatoms.rda")

## END R script 02 ------------------------------------------------------------
