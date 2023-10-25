## R code to interpolate pollen data to equally-spaced time intervals,
## and then calculate rates-of-change (RoC).
## The input file should have samples in rows, and taxon names as column headers.
## NB: first check the sample resolution, then set the parameters, and then
##      run the code.


RoC_estimate <- function(d=NULL, d_counts=NULL, d_age=NULL,
                         DC_metric="chord",
                         transf_to_prop=T, interpolate=T,
                         sample_res_want=NULL) {

  ## Uncomment to test code
  ## Parameters the for function
  # Sopp <- NeotomaExpl_read(dset_name = "Soppensee",
  #                          dset_path = "./Data-In/Soppensee_dataset44723.csv",
  #                          age_row = 6, first_datarow = 7)
  #
  # d = Sopp
  # d_counts = Sopp$counts_terr
  # d_age = Sopp$list_ages$ages$age
  # DC_metric = "chi.square"
  # #DC_metric = "chord"
  # transf_to_prop = T
  # interpolate = T
  # sample_res_want = 50


  #### Extract data ########################################################
  d_name <- d$Dataset
  d_counts <- d_counts[ ,-1]
  taxa_names <- colnames(d_counts)
  min(rowSums(d_counts))
  max(rowSums(d_counts))

  if (DC_metric == "chisq") {
    DC_metric <- "chi.square"
  }


  #### Should data be transformed to proportions? ############################
  if (transf_to_prop == T) {
    d_use <- d_counts / rowSums(d_counts)
  } else {
    d_use <- d_counts
  }

  #### Should data be interpolated to equally spaced data points? ############

  if (interpolate == T) {

    # Set parameters
    bin_young <- min(d_age)
    bin_old <- max(d_age)


    # Prepare empty matrix where data will be stored
    d_age_want <- seq(from = bin_young, to = bin_old, by = sample_res_want)
    nTaxa <- dim(d_use)[2]
    d_interp <- as.data.frame(matrix(data = NA,
                                     nrow = length(d_age_want),
                                     ncol = nTaxa))
    d_interp <- cbind(d_age_want, d_interp)

    for (i in 1:nTaxa) {
      d_approx_i <- stats::approx(x = d_age, y = d_use[ ,i],
                                  xout = d_age_want, method = "linear")$y
      d_interp[ ,i + 1] <- d_approx_i
    }

    colnames(d_interp) [2:dim(d_interp)[2]] <- taxa_names

    d_use2 <- d_interp

  } else {
    d_use2 <- d_use
  }



  ### Calculate Rates-of-change ###################################

  ages_roc <- d_use2$d_age_want
  d_use2 <- d_use2[ ,-1]

  d_RoC <- as.data.frame(matrix(NA, nrow = length(ages_roc), ncol = 1))

  for (j in 1:(length(ages_roc) - 1)) {
    time1 <- d_use2[j, ]
    time2 <- d_use2[(j + 1), ]

    if (DC_metric == "chord") {
      d_RoC[j, ] <- analogue::distance(time2,
                                       time1,
                                       method = DC_metric)
    }
    if (DC_metric == "chi.square") {
      d_RoC[j, ] <- analogue::distance(sqrt(time2),
                                       sqrt(time1),
                                       method = DC_metric)
    }
  }

  d_RoC <- cbind(ages_roc, d_RoC)
  colnames(d_RoC) <- c("Age", "RoC")


  ## Gather data for output ############
  output <- list(d_name = d_name, RoC = d_RoC, DC_metric = DC_metric,
                 interpolate = interpolate)
  return(output)
}
