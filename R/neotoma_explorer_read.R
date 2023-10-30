## Read a dataset saved with NeotomaExplorer

nxpl_read <- function(dset_name = NULL, dset_path = NULL,
                      age_row = NULL, first_datarow = NULL) {

  ## Set variables
  # rm(list = ls())
  # dset_name = "AVG0702"
  # dset_path = "../data_raw/AVG0702_diatom_counts.csv"
  # age_row = 6
  # first_datarow = 9


  ## Loads the Metadata (sample depths and ages) ------------------------------
  d <- read.csv(dset_path, nrows = first_datarow - 1, header = T)


  ## Loads the data (the counts) ----------------------------------------------
  taxa_counts <- read.csv(dset_path, skip = first_datarow, header = F)


  ## gather Metadata ----------------------------------------------------------
  sample_id <- d[which(d$name == "Sample ID"), 6:dim(d) [2]]
  sample_id <- t(sample_id)

  depth <- as.numeric(d[which(d$name == "Depth"), 6:dim(d) [2]])
  depth[is.na(depth) == T] <- 0 # as top samples identified as "modern"

  AgeUnits <- d[age_row, 2]

  ages <- d[age_row, 6:dim(d) [2]]
  ages <- unlist(strsplit(as.character(ages), split = "/"))
  ages[ages == "--"] <- NA
  ages <- as.numeric(ages)

  WantAgesUp <- seq(from = 1, to = length(ages), by = 3)
  WantAgesMean <- seq(from = 2, to = length(ages), by = 3)
  WantAgesLow <- seq(from = 3, to = length(ages), by = 3)

  AgesUp <- ages[WantAgesUp]
  AgesMean <- ages[WantAgesMean]
  AgesLow <- ages[WantAgesLow]

  ages <- data.frame(sample_id, depth, AgesMean)
  colnames(ages) <- c("sample_id", "depth", "age")

  age_range <- data.frame(sample_id, AgesUp, AgesLow)
  colnames(age_range) <- c("sample_id", "age_upper", "age_lower")


  ## Gather taxa names and counts ---------------------------------------------
  taxa <- taxa_counts[, 1:5]
  colnames(taxa) <- colnames(d) [1:5]

  counts <- taxa_counts[, 6:dim(taxa_counts) [2]]
  counts[is.na(counts) == T] <- 0

  labo <- taxa_counts[which(taxa$group == "LABO"), 1:5]
  if (dim(labo)[1] > 0) {
    labo <- cbind(labo, counts[which(taxa$group == "LABO"), ])
    counts <- counts[-which(taxa$group == "LABO"), ]
    taxa <- taxa[-which(taxa$group == "LABO"), ]
  }

  counts <- data.frame(sample_id, t(counts))
  colnames(counts) [1] <- "sample_id"
  colnames(counts) [2:dim(counts) [2]] <- taxa$name

  row.names(ages) <- NULL
  row.names(age_range) <- NULL
  row.names(counts) <- NULL
  row.names(labo) <- NULL


  ## Prepare output -----------------------------------------------------------
  output <- list(Dataset = dset_name,
                 list_ages = list(ages = ages, age_range = age_range),
                 AgeUnits = AgeUnits, taxa = taxa, labo = labo,
                 counts = counts)
  return(output)
}
