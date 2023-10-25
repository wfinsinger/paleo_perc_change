#' Percentage of the inferred or estimated baseline level
#'
#' Calculates the percentage change relative to a baseline interval (or an
#' inferred or estimated reference value) for all data points of an ecological
#' indicator to show how much remains after the baseline period and, if
#' applicable, how much was present before the baseline period.
#'
#'
#' @data_source_ecolind a data.frame with 2 columns: sample_id (chr), and
#'                      the variable being used (num).
#' @data_source_ages a data.frame with 3 columns: sample_id (chr),
#'                      sample depth (num), and sample age (num).
#' @baseln_interval Numerical. The boundaries (in years) for the baseline
#'                  interval (start, end), e.g. with CE years: c(1970, 1979).
#' @baseln_estimated Numerical. A vector that sets the value of the estimated
#'                  baseline level. If the length of the vector is >1, the mean
#'                  of the values will be used.
#' @ecolind_name character. Describes the type of ecological indicator.
#' @desired_direction Character. Describes which direction of change relative
#'                    to the baseline value is desired (and thus marked in
#'                    green in the plots). The argument can take one of two
#'                    strings: "increase" (by default), or "decrease".
#' @bin_size numerical. Determines the time interval (in years) over which the
#'              percentage change values will be averaged. By default,
#'              bin_size = 10, i.e. the per-decade average percentage change.
#' @age_scale a character describing the age_scale used. Currently two options
#'            are available: "CE years" and "calBP".
#' @plotit logical. Determines if a plot is returned. By default, plotit = TRUE.
#'
#' @author Walter Finsinger


change_perc_baseln <- function(data_source_ecolind = NULL,
                               data_source_ages = NULL,
                               baseln_interval = NULL,
                               baseln_estimated = NULL,
                               ecolind_name = NULL,
                               desired_direction = "increase",
                               bin_size = 10,
                               age_scale = "CE years",
                               plotit = TRUE) {


  # Initial check-ups ---------------------------------------------------------
  require(dplyr)

  if (is.null(baseln_interval) & is.null(baseln_estimated)) {
    print("Fatal error: arguments baseln_interval and baseln_estimated cannot be both NULL")
    stop()
  }

  if (!is.null(baseln_interval) & !is.null(baseln_estimated)) {
    print("Fatal error: either baseln_interval or baseln_estimated must be == NULL")
    stop()
  }

  if (desired_direction %in% c("increase", "decrease") == FALSE) {
    print("Fatal error: set 'desired_direction = 'increase' or 'decrease' ")
    stop()
  }

  # Extract values from arguments ---------------------------------------------
  v <- data_source_ecolind[, 2]
  v_ages <- data_source_ages[, 3]

  if (!is.null(baseln_estimated)) {
    baseln_interval <- c(NA, NA)
  }
  baseln_start <- baseln_interval[1]
  baseln_end <- baseln_interval[2]


  ## Get baseline value(s) and the average baseline value  --------------------
  if (is.null(baseln_estimated)) {
    v_baseln_values <- v[v_ages >= baseln_start & v_ages <= baseln_end]
    v_baseln_mean <- mean(v_baseln_values)
  } else {
    v_baseln_values <- baseln_estimated
    v_baseln_mean <- mean(v_baseln_values)
  }

  # Calculate percentage change from v_baseln_mean for all observations -------
  m <- data.frame(v_ages, v)
  mc <- m %>%
    dplyr::mutate(change = v - v_baseln_mean,
                  change_percentage = (change / v_baseln_mean) * 100)

  ## Remove observations falling in the baseline interval ---------------------
  if (!is.null(baseln_interval)) {
    mc_baseln <- mc %>%
      dplyr::filter(v_ages >= baseln_start & v_ages <= baseln_end)
    mc <- mc %>%
      dplyr::filter(v_ages < baseln_start | v_ages > baseln_end)
  } else {
    mc_baseln <- NULL
  }

  ## Add a factor to identify decrease vs increase
  if (desired_direction == "increase") {
    mc$direction <- factor(ifelse(mc$change > 0, "desired", "undesired"))
  } else {
    mc$direction <- factor(ifelse(mc$change < 0, "desired", "undesired"))
  }

  # Calculate mean percentage change by bin -----------------------------------
  ## as of the argument 'bin_size'
  m <- mc %>%
    dplyr::group_by(time_interval_start = (v_ages) -
                      v_ages %% bin_size) %>%
    dplyr::mutate(time_interval_end = time_interval_start +
                    bin_size - 1)
  change_bins <- m %>%
    dplyr::group_by(time_interval_start) %>%
    dplyr::summarise(change_bins_mean = mean(change_percentage))
  rm(m)


  # Calculate rate of mean percentage change by bin wrt baseline interval -----
  ## as of the argument 'bin_size'
  if (!is.null(mc_baseln)) {
    change_bins_pre_baseln <- change_bins %>%
      dplyr::filter(time_interval_start < baseln_start) %>%
      dplyr::mutate(time_diff = abs(
        time_interval_start - baseln_start - bin_size/2)) %>%
      dplyr::mutate(ropc = change_bins_mean / (time_diff/bin_size))

    change_bins_post_baseln <- change_bins %>%
      dplyr::filter(time_interval_start > baseln_end) %>%
      dplyr::mutate(time_diff = abs(
        time_interval_start - baseln_end + bin_size/2)) %>%
      dplyr::mutate(ropc = change_bins_mean / (time_diff/bin_size))

    change_bins <- dplyr::bind_rows(change_bins_pre_baseln,
                                    change_bins_post_baseln)
  }


  # Plot results --------------------------------------------------------------
  pl <-
    ggplot2::ggplot(data = mc[complete.cases(mc$change_percentage), ]) +
    ggplot2::ggtitle(label = ecolind_name,
                     subtitle = paste0("(% change, relative to ",
                                       baseln_interval[1],"-",
                                       baseln_interval[2], " ", age_scale,
                                       ")")) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 14)) +
    # ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13)) +
    # ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::geom_col(ggplot2::aes(x = v_ages,
                                   y = change_percentage,
                                   fill = direction)) +
    ggplot2::scale_fill_manual(values = c("desired" = "green",
                                          "undesired" = "red")) +
    ggplot2::labs(x = age_scale, y = "% change", ) +
    ggplot2::geom_abline(mapping = ggplot2::aes(slope = 0, intercept = 0))

  if (!is.null(mc_baseln)) {
    pl <- pl +
      ggplot2::geom_col(data = mc_baseln,
                        ggplot2::aes(x = v_ages,
                                     y = change_percentage,
                                     fill = "grey"), alpha = 0.8)
  }

  if (!is.null(bin_size) & !is.null(mc_baseln)) {
    pl <- pl +
      ggplot2::geom_line(data = change_bins_post_baseln,
                         ggplot2::aes(x = time_interval_start + bin_size/2,
                                      y = ropc), linewidth = 1) +
      ggplot2::geom_line(data = change_bins_pre_baseln,
                         ggplot2::aes(x = time_interval_start + bin_size/2,
                                      y = ropc), linewidth = 1)
  }
  print(pl)

  # Gather data for output ----------------------------------------------------
  output <- list(mc = mc,
                 ecolind_name = ecolind_name, bin_size = bin_size,
                 change_bins = change_bins,
                 baseln_interval = baseln_interval, age_scale = age_scale,
                 mc_baseln = mc_baseln,
                 mean_baseln_value = v_baseln_mean)

  # END -----------------------------------------------------------------------
  return(output)
}
