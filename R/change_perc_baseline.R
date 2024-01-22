#' Change relative to reference period (or reference level)
#'
#' Calculates the percentage change relative to a reference period (or
#' relative to an otherwise inferred or estimated reference value) for all data
#' points of an ecological indicator to show how much remains after the
#' reference period and, if applicable, how much was present before that
#' period.
#'
#' In addition, and only if a reference period has been defined, the rate of
#' mean percentage change per unit-time will be estimated as the change between
#' the average value in two time intervals divided by the number of bin_sizes
#' that separate them. For instance, with bin_size = 10 years, the change
#' would be divided by the number of decades to provide a per-decade rate of
#' change. Note that this is the average rate of net change over the time span
#' being considered, whether or not the change was linear.
#'
#' @data_source_ecolind a data.frame with 2 columns: sample_id (chr), and
#'                      the variable being used (num).
#' @data_source_ages a data.frame with 3 columns: sample_id (chr),
#'                      sample depth (num), and sample age (num).
#' @bin_size Required, numerical. Determines the time interval (in years) over
#'           which the percentage change values will be averaged. By default,
#'           bin_size = 10, i.e. the per-decade average percentage change.
#' @desired_direction Required, character. Describes which direction of change
#'                    relative to the reference value is desired (and thus
#'                    marked in green in the plots). The argument can take one
#'                    of two strings: "increase" (by default), or "decrease".
#' @reference_interval The boundaries (in years) for the reference
#'                  period (start, end), e.g. c(1970, 1979). By default,
#'                  reference_interval = NULL. However, values defining the
#'                  \code{reference_interval} are required if
#'                  reference_level_estimated = NULL.
#' @reference_level_estimated A vector that sets the value of the estimated
#'                  reference level (by default,
#'                  reference_level_estimated = NULL).
#'                  If the length of the vector is >1, the mean of the values
#'                  will be used. NB: this is required if
#'                  'reference_interval = NULL.
#' @reference_level_desc Optional. A character string to add additional
#'                        information on the reference level (e.g. units,
#'                        source).
#' @ecolind_name Optional, character. Describes the type of ecological
#'                indicator.
#' @age_scale a character string describing the age_scale used. By default,
#'            age_scale = "CE years".
#' @plotit logical. Determines if a plot is returned. By default,
#'          plotit = TRUE.
#'
#' @author Walter Finsinger

change_perc_relto <- function(data_source_ecolind = NULL,
                               data_source_ages = NULL,
                               bin_size = 10,
                               desired_direction = "increase",
                               reference_interval = NULL,
                               reference_level_estimated = NULL,
                               reference_level_desc = NULL,
                               ecolind_name = NULL,
                               age_scale = "CE years",
                               plotit = TRUE) {

  # Initial check-ups ---------------------------------------------------------
  require(dplyr)

  if (is.null(reference_interval) & is.null(reference_level_estimated)) {
    print("Fatal error: arguments 'reference_interval' and
          'reference_level_estimated' cannot both be NULL")
    stop()
  }

  if (!is.null(reference_interval) & !is.null(reference_level_estimated)) {
    print("Fatal error: either 'reference_interval == NULL' or
          'reference_level_estimated == NULL'")
    stop()
  }

  if (desired_direction %in% c("increase", "decrease") == FALSE) {
    print("Fatal error: set 'desired_direction = 'increase' or 'decrease' ")
    stop()
  }

  # Extract values from arguments ---------------------------------------------
  v <- data_source_ecolind[, 2]
  v_ages <- data_source_ages[, 3]

  if (!is.null(reference_level_estimated)) {
    reference_interval <- c(NA, NA)
  }
  reference_period_start <- reference_interval[1]
  reference_period_end <- reference_interval[2]


  ## Get reference value(s) and the average baseline value  -------------------
  if (is.null(reference_level_estimated)) {
    v_reference_values <-
      v[v_ages >= reference_period_start & v_ages <= reference_period_end]
    v_reference_mean <- mean(v_reference_values)
  } else {
    v_reference_values <- reference_level_estimated
    v_reference_mean <- mean(v_reference_values)
  }

  # Calculate percentage change from v_reference_mean for all observations -------
  m <- data.frame(v_ages, v)
  mc <- m %>%
    dplyr::mutate(change = v - v_reference_mean,
                  change_percentage = (change / v_reference_mean) * 100)

  ## Remove observations falling in the baseline interval ---------------------
  if (any(!is.na(reference_interval))) {
    mc_excl_reference_period <- mc %>%
      dplyr::filter(v_ages >= reference_period_start &
                      v_ages <= reference_period_end)
    mc <- mc %>%
      dplyr::filter(v_ages < reference_period_start |
                      v_ages > reference_period_end)
    mc_excl_reference_period$direction <- "reference"
  } else {
    mc_excl_reference_period <- NULL
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


  # Calculate rate of mean percentage change per unit-time --------------------
  # as the change between the average values in two time intervals divided by
  # the number of bin_sizes that separate them.
  if (!is.null(mc_excl_reference_period)) {
    rate_change_bins_post_ref_period <- change_bins %>%
      dplyr::filter(time_interval_start > reference_period_end) %>%
      dplyr::mutate(bin_diff =
                      abs(time_interval_start -
                            (floor(reference_period_end/bin_size) * bin_size)) /
                      bin_size) %>%
      dplyr::mutate(ropc = change_bins_mean / bin_diff)
  } else {
    rate_change_bins_post_ref_period <- NULL
  }


  # Plot results --------------------------------------------------------------
  if (!is.null(mc_excl_reference_period)) {
    mc <- dplyr::bind_rows(mc, mc_excl_reference_period) %>%
      dplyr::arrange(desc(v_ages))
    pl <-
      ggplot2::ggplot(data = mc[complete.cases(mc$change_percentage), ]) +
      ggplot2::ggtitle(label = ecolind_name,
                       subtitle = paste0(
                         "(% change and rate of change, relative to ",
                         reference_interval[1],"-",
                         reference_interval[2], " ", age_scale,
                         ")")) +
      ggplot2::theme_classic() +
      ggplot2::theme(text = ggplot2::element_text(size = 14)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_col(ggplot2::aes(x = v_ages,
                                     y = change_percentage,
                                     fill = direction)) +
      ggplot2::scale_fill_manual(name = "", values = c("desired" = "green",
                                                       "reference" = "grey",
                                                       "undesired" = "red")) +
      ggplot2::labs(x = age_scale, y = "% change") +
      ggplot2::geom_abline(mapping = ggplot2::aes(slope = 0, intercept = 0)) +
      ggplot2::geom_line(data = rate_change_bins_post_ref_period,
                         ggplot2::aes(x = time_interval_start,
                                      y = ropc, colour = "Rate"),
                         linewidth = 1) +
      ggplot2::scale_color_manual("", values = "black")
  } else {
    pl <-
      ggplot2::ggplot(data = mc[complete.cases(mc$change_percentage), ]) +
      ggplot2::ggtitle(label = ecolind_name,
                       subtitle = paste0("(% change, relative to ",
                                         reference_level_estimated,
                                         reference_level_desc, ")")) +
      ggplot2::theme_classic() +
      ggplot2::theme(text = ggplot2::element_text(size = 14)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_col(ggplot2::aes(x = v_ages,
                                     y = change_percentage,
                                     fill = direction)) +
      ggplot2::scale_fill_manual(name = "", values = c("desired" = "green",
                                                       "undesired" = "red")) +
      ggplot2::labs(x = age_scale, y = "% change") +
      ggplot2::geom_abline(mapping = ggplot2::aes(slope = 0, intercept = 0))
  }
  print(pl)

  # Gather data for output ----------------------------------------------------
  output <- list(mc = mc, ecolind_name = ecolind_name, bin_size = bin_size,
                 rate_change_post_ref_period = rate_change_bins_post_ref_period,
                 baseline_interval = reference_interval, age_scale = age_scale,
                 mc_excl_reference_period = mc_excl_reference_period,
                 mean_reference_value = v_reference_mean)

  # END -----------------------------------------------------------------------
  return(output)
}
