# Loads the pollen data set from Lago Grande di Avigliana (Finsinger et al.,
# 2006),
# performs exploratory data analysis, and
# the rate-of-change analysis (RoC) using the 'RRatepoll' R package.
# The output data is saved to the 'data_out' folder,
#   figures are saved in the 'figures_out' folder.

rm(list = ls())

# Load libraries, tools, and data ---------------------------------------------

### Load libraries ------------------------------------------------------------
library(rioja)
library(dplyr)
#devtools::install_github("HOPE-UIB-BIO/R-Ratepol-package")
library(RRatepol)


### Load R functions from source ----------------------------------------------

## New R functions
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


# Load data -------------------------------------------------------------------

### Pollen harmonization table ------------------------------------------------
load("./data_in/harm.RData")


### Pollen and spores counts, with sample ages --------------------------------
load("./data_in/dset_pollen.RData")

dset <- dset_pollen
rm(dset_pollen)


# Filter data -----------------------------------------------------------------

## Remove samples with pollen sums < 200 terrestrial pollen grains
p_counts <- dset$counts %>% dplyr::select(which(
  dset$taxa$element == "pollen" | dset$taxa$element == "spore"))
p_counts_sum <- rowSums(p_counts[2:dim(p_counts)[2]])
summary(p_counts_sum)
ind_sample_remove <- which(p_counts_sum < 200)

if (length(ind_sample_remove) > 0) {
  dset$list_ages$ages <- dset$list_ages$ages[-ind_sample_remove, ]
  dset$list_ages$age_range <- dset$list_ages$age_range[-ind_sample_remove, ]
  dset$labo <- dset$labo[-ind_sample_remove + 5, ]
  dset$counts <- dset$counts[-ind_sample_remove, ]
  rownames(dset$list_ages$ages) <- NULL
  rownames(dset$list_ages$age_range) <- NULL
  rownames(dset$counts) <- NULL
}

p_counts <- dset$counts %>% dplyr::select(which(
  dset$taxa$element == "pollen"))
p_counts_sum <- c(NA, rowSums(p_counts[2:dim(p_counts)[2]]))
summary(p_counts_sum)


# Plot resolution of the record -----------------------------------------------
dset_min_age <- min(dset$list_ages$ages$age)
dset_resol <- abs(c(diff(dset$list_ages$ages$age), NA))
summary(dset_resol)

pdf("./figures_out/Figure_A1a_pollen_resol.pdf")
par(fig = c(0, 0.9, 0, 1), mar = c(5,6,5,3))
plot(dset$list_ages$ages$age, dset_resol, type = "h", lwd = 2,
     xlim = c(1840, 2010), ylim = NULL,
     xlab = "Age (CE yrs)",
     ylab = "Resolution\n (years between samples)",
     main = "AVG - pollen")
par(fig = c(0.65, 1, 0, 1), new = TRUE)
boxplot(dset_resol, yaxt = "n")
axis(4)
dev.off()

layout(1)


# Transform data --------------------------------------------------------------

### Calculate pollen percentages ----------------------------------------------
# based on terrestrial pollen sum (TRSH + UPHE)
dset_perc <- nxpl_perc(d_in = dset, d_sum = "terrestrial",
                       taxa_excl = "Humulus/Cannabis-type",
                       plotit = F,
                       x_lim = NULL)
perc_counts_sum <- rowSums(dset_perc$counts[2:dim(dset_perc$counts)[2]])
min(perc_counts_sum)

## and make a plot to see the pollen percentages
rioja::strat.plot(dset_perc$perc[ ,2:30],
                  yvar = dset_perc$list_ages$ages$age,
                  scale.percent = TRUE,
                  y.rev = FALSE, cex.xlabel = 0.7)


### Harmonize taxonomy --------------------------------------------------------

dset_harm <- dset_perc

##### Harmonize counts --------------------------------------------------------
dset_harm$counts <- dset_perc$counts %>%
  tidyr::pivot_longer(cols = -sample_id, names_to = "original_name",
                      values_to = "counts") %>% # turn into long format
  dplyr::left_join(harm,
                   by = "original_name") %>% # link with the harmonization table
  dplyr::group_by(sample_id, MHVar.2) %>% # group by `harm_name`
  dplyr::summarise(.groups = "keep",
                   counts = sum(counts)) %>% # sum all taxa in the same group
  tidyr::pivot_wider(names_from = "MHVar.2",
                     values_from = "counts") # turn back to the wide format
rowSums(dset_harm$counts[ ,2:dim(dset_harm$counts)[2]])


##### Harmonize percentages ---------------------------------------------------
dset_harm$perc <- dset_perc$perc %>%
  tidyr::pivot_longer(cols = -sample_id, names_to = "original_name",
                      values_to = "perc") %>% # turn into long format
  dplyr::left_join(harm,
                   by = "original_name") %>% # link with the harmonization table
  dplyr::group_by(sample_id, MHVar.2) %>% # group by `harm_name`
  dplyr::summarise(.groups = "keep",
                   perc = sum(perc)) %>% # sum all taxa in the same group
  tidyr::pivot_wider(names_from = "MHVar.2",
                     values_from = "perc") # turn back to the wide format
rowSums(dset_harm$perc[ ,2:dim(dset_harm$perc)[2]])


##### Check harmonized percentages --------------------------------------------
rioja::strat.plot(dset_harm$perc[ ,2:30],
                  yvar = dset$list_ages$ages$age,
                  scale.percent = TRUE,
                  y.rev = FALSE, cex.xlabel = 0.7,
                  wa.order = "bottomleft")



##### Plot selected taxa % ----------------------------------------------------
colMax <- function(data) sapply(data, max, na.rm = TRUE)
p_harm_perc3 <- dset_harm$perc[ ,2:dim(dset_harm$perc)[2]]
p_harm_perc3 <- cbind(p_harm_perc3, dset_harm$grps[ ,2], dset_perc$perc$Secale)
names(p_harm_perc3)
p_harm_perc3 <- dplyr::rename(p_harm_perc3,
                              "Trees and shrubs" = "dset_harm$grps[, 2]")
p_harm_perc3 <- dplyr::rename(p_harm_perc3, "Secale" = "dset_perc$perc$Secale")

rioja::strat.plot(p_harm_perc3,
                  yvar = dset$list_ages$ages$age,
                  scale.percent = TRUE,
                  y.rev = TRUE, cex.xlabel = 0.7,
                  wa.order = "topleft")

p_harm_perc3 <- dplyr::select(p_harm_perc3, c("Trees and shrubs",
                                              "Secale",
                                              "Juglans",
                                              "Juniperus-type",
                                              "Alnus",
                                              "Quercus",
                                              "Castanea sativa",
                                              "Salix",
                                              "Betula", "Platanus",
                                              "Fagus",
                                              "Fraxinus"))

pdf("./figures_out/Figure_2a_AVG0702_pollen.pdf")
rioja::strat.plot(p_harm_perc3,
                  yvar = dset$list_ages$ages$age,
                  scale.percent = TRUE, plot.poly = TRUE, plot.bar = FALSE,
                  col.poly = "darkgreen",
                  y.rev = TRUE, cex.xlabel = 0.7,
                  wa.order = "none", ylabel = "Age (CE years)")
dev.off()


# Prepare data.frame for cvar (TRSH) ------------------------------------------
trsh <- as.data.frame(cbind(dset_harm$list_ages$ages$age,
                            dset_harm$grps$sum_trsh))
colnames(trsh) <- c("age", "Trees and shrubs (%)")
trsh$up <- trsh[ ,2]
trsh$dw <- trsh[ ,2]


# Calculate compositional RoC with R-Ratepol ----------------------------------

n_indiv_min <- min(rowSums(dset_harm$counts[2:dim(dset_harm$counts)[2]]))

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
N.individuals <- n_indiv_min # here = 195 pollen grains counted per sample.
n.rand <- 999 # n.rand <- 1000 in Mottl et al. (2021) !!
time.standardisation <- 50


### Calculate RoC -------------------------------------------------------------

dset_roc <-
  RRatepol::estimate_roc(data_source_community = dset_harm$counts,
                         data_source_age = dset_harm$list_ages$ages[ ,c(1,3)],
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

dset_roc_peak <-
  RRatepol::detect_peak_points(dset_roc, sel_method = "threshold")

RRatepol::plot_roc(dset_roc_peak, age_threshold = NULL,
                   roc_threshold = max(dset_roc$ROC),
                   peaks = TRUE,
                   trend = "threshold")
dev.off()

pdf("./figures_out/Figure_3a_RoC_pollen_nshift5.pdf")
fc_plot_RoC_seq4(dset_roc_peak, age_threshold = NULL, age_scale = "CE",
                 Roc_threshold = max(dset_roc$ROC),
                 Peaks = TRUE,
                 trend = "threshold",
                 cvar = trsh, cvar_col = "lightgreen",
                 aside = TRUE)
dev.off()



# # Output - save data --------------------------------------------------------
dset_harm_ages <- dset_harm$list_ages$ages
dset_harm_grps <- dset_harm$grps

save(dset_harm_grps, file = "./data_out/01_AVG0702_trsh.rds")
save(dset_harm_ages, file = "./data_out/01_AVG0702_trsh_ages.rds")

save.image("./data_out/01_AVG0702_RoC_pollen.rda")

## END R script 01 ------------------------------------------------------------
