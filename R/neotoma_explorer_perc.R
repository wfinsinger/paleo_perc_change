nxpl_perc <- function(d_in = NULL, d_sum = NULL, taxa_excl = NULL, plotit = T,
                      x_lim = NULL, y_lim = c(0,100)) {

  # d_in = dset
  # d_sum = "terrestrial"
  # # d_sum = NULL
  # taxa_excl = "Humulus/Cannabis-type"
  # # taxa_excl = c("Abies", "Humulus/Cannabis-type")
  # # taxa_excl = NULL
  # plotit = T
  # x_lim = c(150, -60)


  ## Load libraries -----------------------------------------------------------
  require(dplyr)


  ## Get data -----------------------------------------------------------------
  d_counts <- d_in$counts[ ,-1]
  sample_id <- d_in$list_ages$ages$sample_id
  age <- d_in$list_ages$ages$age


  ## Get counts for selected groups and excluded taxa -------------------------
  if (is.null(d_sum)) {
    d_counts2 <- d_counts %>% dplyr::select(-one_of(taxa_excl))
    d_taxa <- d_in$taxa %>% dplyr::filter(!name %in% taxa_excl)
  }
  if (!is.null(d_sum)) {
    if (d_sum == "terrestrial") {
      d_taxa <- d_in$taxa %>% dplyr::filter(
        group %in% c("TRSH", "UPHE") & element == "pollen") %>%
        dplyr::filter(!name %in% taxa_excl)

      d_counts2 <- d_counts %>% dplyr::select(which(
        (d_in$taxa$group == "TRSH" | d_in$taxa$group == "UPHE") &
          d_in$taxa$element == "pollen")) %>%
        dplyr::select(-one_of(taxa_excl))

    }
  }

  ## Calculate percentages ----------------------------------------------------
  d_sum <- rowSums(d_counts2)
  d_perc <- as.data.frame(matrix(data = NA, nrow = nrow(d_counts2),
                                 ncol = ncol(d_counts2)))

  for (i in 1:dim(d_counts2)[1]) {
    #i=1
    d_perc[i, ] <- d_counts2[i, ] / d_sum[i] * 100
  }
  colnames(d_perc) <- colnames(d_counts2)


  ## Get percentages of Groups ------------------------------------------------
  d_trsh <- rowSums(d_perc[which(d_taxa$group == "TRSH" &
                                   d_taxa$element == "pollen")])
  d_uphe <- 100 - d_trsh


  ## Format data for RRatepol -------------------------------------------------
  d_counts2 <- cbind(sample_id, d_counts2)
  d_grps <- data.frame(sample_id, d_trsh, d_uphe)
  colnames(d_grps) <- c("sample_id", "sum_trsh", "sum_uphe")

  d_perc <- cbind(sample_id, d_perc)
  colnames(d_perc)[1] <- "sample_id"


  ## Plot figure --------------------------------------------------------------
  if (plotit == T) {

    if (is.null(x_lim) == T) {
      x_lim <- c(max(age), min(age))
    }
    y_lim <- c(min(c(d_trsh, d_uphe)), max(c(d_trsh, d_uphe)))

    par(mfrow = c(1,1), mar = c(5,5,2,2), oma = c(1,1,1,1))
    plot(age, d_trsh, type = "l", lwd = 2,
         xlim = x_lim, ylim = y_lim,
         xlab = d_in$AgeUnits, ylab = "Pollen (%)")
    lines(age, d_uphe, lwd = 2, col = "red")
    legend("topleft", legend = c("TRSH", "UPHE"),
           col =  c("black", "red"), lwd = c(2,2), bty = "n")
  }

  return(list(Dataset = d_in$Dataset, list_ages = d_in$list_ages,
              AgeUnits = d_in$AgeUnits, taxa = d_taxa, counts = d_counts2,
              perc = d_perc, grps = d_grps))

}
