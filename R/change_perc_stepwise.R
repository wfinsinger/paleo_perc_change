#' Percentage change per unit time with irregularly spaced observations
#'
#' @param data_source_ecolind a data.frame with 2 columns: sample_id (chr), the
#'                      variable being used (num).
#' @param data_source_ages a data.frame with 3 columns: sample_id (chr), sample
#'                  depth (num), and sample age (num).
#' @param ecolind_name optional. Character describing the ecological indicator.
#' @param age_scale Required. A character describing the age scale. Currently
#'                  only "CE years" (default) is available.
#' @param change_by Required. Specifies the time interval (in years) over which
#'            the percentage change values will be averaged. By default, the
#'            data points are grouped into working units (WUs) of 10 years
#'            (thus, change_by = 10), which gives per-decade rate of percentage
#'            change between consecutive time intervals.
#' @param n_shift numerical (integer). Determines the number of shifts to set up
#'            the WUs. By default, \code{n_shift = 0}, which implies that only
#'            one set of WUs will be created.
#' @param time_standardisation If specified, the rate of change is referred
#'            relative to the number of years given by the argument 'change_by'.
#'            With time_standardisation = NULL (default), the rate of change
#'            is calculated relative to the time interval between the
#'            randomly selected samples of two adjacent WUs.
#' @param plotit logical. Determines if a plot is returned to the environment.
#'               By default, plotit = TRUE.
#' @param n_rand Integer. Specifies the number of times a random sample is
#'                selected from each of the WUs in each set of WUs.
#'                By default, \code{n_rand = 99}.
#' @param add_whiskers Logical. Determines if whiskers illustrating the
#'                      variability of the data are added to the plot. By
#'                      default, \code{add_whiskers = TRUE}.
#' @param y_lim Optional. A vector to specify the y-axis range limits. By
#'                      default, \code{y_lim = NULL}.
#'
#' @author Walter Finsinger

change_perc_stepwise <- function(data_source_ecolind = NULL,
                                 data_source_ages = NULL,
                                 ecolind_name = NULL,
                                 age_scale = "CE years",
                                 change_by = 10,
                                 n_shift = 0,
                                 time_standardisation = NULL,
                                 n_rand = 99,
                                 add_whiskers = TRUE,
                                 y_lim = NULL) {


  # Load required packages ---------------------------------------------------
  require(dplyr)


  # Extract values from arguments -------------------------------------------
  v <- data_source_ecolind[, 2]
  v_ages <- data_source_ages[, 3]
  m <- data.frame(v_ages, v)


  # Make WUs: 'group samples by time interval' as of 'change_by' --------------

  ### Define WUs and shifted WUs ----------------------------------------------
  shifts <- seq(from = 0,
                to = change_by - 1,
                by = change_by/(n_shift + 1))
  t_want <- seq(from = min(v_ages) - min(v_ages) %% change_by,
                to = max(v_ages) - max(v_ages) %% change_by,
                by = change_by)

  ### Loop through the shifted WUs (if n_shift > 0) ---------------------------
  df_list <- list()
  mc_gather <- list()

 for (p in 1:length(shifts)) {
    t_want_p <- sort(t_want + shifts[p])
    df <- m %>%
      dplyr::mutate(time_interval_start =
                      t_want_p[findInterval(v_ages, t_want_p)])


    #### Insert missing time intervals -----------------------------------------
    time_intervals_missing <- setdiff(t_want_p, df$time_interval_start)
    if (length(time_intervals_missing) > 0) {
      df_missing <- data.frame(v_ages = NA, v = NA,
                               time_interval_start = time_intervals_missing)
      df <- df %>%
        dplyr::rows_insert(df_missing, by = "time_interval_start") %>%
        dplyr::arrange(-time_interval_start)
    }
    df$time_interval_end <- df$time_interval_start + (change_by - 1)

    rm(df_missing, time_intervals_missing)



    ### Calculate percentage change between consecutive bins ------------------

    #### Take n_rand times a random sample from each WU -----------------------

    ## Prepare an empty matrix where data will be stored
    mc_summary <- data.frame(unique(df$time_interval_start))
    colnames(mc_summary) <- "time_interval_start"
    df_list <- list()

    ## Loop through n_rand:
    for (i in 1:n_rand) {
       # i = 1
      df_i <- df %>% dplyr::group_by(time_interval_start) %>%
        dplyr::slice_sample(n = 1) %>%
        dplyr::arrange(-time_interval_start)
      df_i <- as.data.frame(df_i)

      ### Calculate annual change between samples from consecutive bins -------
      df_i <- df_i %>%
        dplyr::mutate(previous_v = dplyr::lead(v),
                      previous_t = dplyr::lead(v_ages),
                      change_v = v - previous_v,
                      annual_change = change_v / (v_ages - previous_t))


      ### Standardize annual change by time interval  -------------------------

      ## Do not standardize, thus roc_change_by_perc = percentage change by
      ## difference between sample ages:
      if (is.null(time_standardisation)) {
        df_i <- df_i %>%
          dplyr::mutate(
            time_standardised_change = NA,
            roc_change_by_perc = (change_v / previous_v) * 100)
      } else {  ## roc_change_by_perc are re-scaled by time_standardization
        df_i <- df_i %>%
          dplyr::mutate(
            time_standardised_change = annual_change * time_standardisation,
            roc_change_by_perc = (time_standardised_change / previous_v) * 100)
      }

      ### Write data in the list ----------------------------------------------
      df_list[[i]] <- df_i$roc_change_by_perc
    }

    ### Gather data generated in the loop -------------------------------------
    mc_result <- data.frame(simplify2array(df_list))

    ### Calculate summary stats for each WU -----------------------------------
    mc_summary <- mc_result %>%
      dplyr::rowwise() %>%
      dplyr::summarise(
        change_mean = mean(dplyr::c_across(tidyselect::starts_with("X")),
                           na.rm = TRUE))
    mc_summary[,2] <- mc_result %>%
      dplyr::rowwise() %>%
      dplyr::summarise(
        change_median = median(dplyr::c_across(tidyselect::starts_with("X")),
                               na.rm = TRUE))
    mc_summary[,3] <- mc_result %>%
      dplyr::rowwise() %>%
      dplyr::summarise(
        upper_75th = quantile(dplyr::c_across(tidyselect::starts_with("X")),
                              probs = 0.75, na.rm = TRUE))
    mc_summary[,4] <- mc_result %>%
      dplyr::rowwise() %>%
      dplyr::summarise(
        lower_25th = quantile(dplyr::c_across(tidyselect::starts_with("X")),
                              probs = 0.25, na.rm = TRUE))

    mc_summary$bin_start <- df_i$time_interval_start
    mc_summary$bin_end <- df_i$time_interval_end
    mc_summary$previous_bin_start <- mc_summary$bin_start - change_by

    mc_gather[[p]] <- mc_summary
 }

  ## Gather data into one data frame
  mc <- dplyr::bind_rows(mc_gather) %>% dplyr::arrange(-bin_start) %>%
    dplyr::relocate(starts_with("bin"), .before = everything()) %>%
    dplyr::relocate(starts_with("previous"), .before = everything())


  # Plot results --------------------------------------------------------------
  mc <- mc %>% na.omit

  pl <- ggplot2::ggplot(data = mc) +
    ggplot2::ggtitle(label = ecolind_name,
                     subtitle = paste0("mean % change per ",
                                       change_by," yrs")) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 14)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_col(
      mapping = ggplot2::aes(x = previous_bin_start + change_by/2,
                             y = change_mean,
                             fill = change_mean > 0), alpha = 0.8) +
    ggplot2::labs(x = age_scale, y = "Percentage change") +
    ggplot2::scale_fill_manual(name = "",
                               values = c("#5e3c99", "#e66101"),
                               labels = c("Decrease", "Increase")) +
    ggplot2::geom_abline(mapping = ggplot2::aes(slope = 0, intercept = 0))

  if (any(!is.null(y_lim))) {
    pl <- pl +
      ggplot2::ylim(y_lim)
  }

  if (add_whiskers == TRUE) {
    pl <- pl +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(x = previous_bin_start + change_by/2,
                               ymin = lower_25th,
                               ymax = upper_75th,
                               y = change_median), color = "grey")
  }
  print(pl)

  # END -----------------------------------------------------------------------
  return(mc)
}
