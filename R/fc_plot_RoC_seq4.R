## Plot the Rate-of-Change sequence
##
## Modified from the "fc_plot_RoC_sequence()" function that is included in the
## RRatepol R package (Mottl et al., 2021). The modified function allows
## plotting the rate-of-change record along another ecological indicator record (identified with the argument "cvar"), and extends the age scale from the
## selected age_threshold to the min(age).
## It also allows setting the age scale units, with two options for the
## argument "age_scale":
##  age_scale = "CE" (for Common Era years), and
##  age_scale = "calBP" (for calibrated radiocarbon years).

fc_plot_RoC_seq4 <- function(data_source, age_threshold = NULL, age_scale = NULL,
                             Roc_threshold = NULL,
                             Peaks = FALSE, trend = NULL,
                             cvar = NULL, cvar_col = "gray30",
                             aside = TRUE) {


  # Check-ups -----------------------------------------------------------------
  util_check_class("data_source", "data.frame")
  util_check_col_names("data_source", c("Age", "ROC", "ROC_up", "ROC_dw"))
  util_check_class("age_threshold", c("NULL", "numeric"))


  cvar_name <- colnames(cvar)[2]
  colnames(cvar) <- c("age", "cvar_ts", "cvar_dw", "cvar_up")

  if (is.null(age_scale) == TRUE) {
    stop(print('set "age_scale = calBP" or "age_scale = CE"'))
  }


  ### Get sample ages ---------------------------------------------------------
  if (age_scale == "calBP") {
    if (is.null(age_threshold) == TRUE) {
      age_threshold <- max(data_source$Age)
    }
    data_source_filter <- data_source %>% dplyr::filter(.data$Age <=
                                                          age_threshold)
    dsf_age_old <- max(data_source_filter$Age)
    dsf_age_young <- min(data_source$Age)
    cvar_age_old <- max(cvar$age)
    cvar_age_young <- min(cvar$age)
    age_young <- min(dsf_age_young, cvar_age_young)
    age_old <- max(dsf_age_old, cvar_age_old)
    x_lab <- "Age (cal yr BP)"
  }
  if (age_scale == "CE") {
    if (is.null(age_threshold) == TRUE) {
      age_threshold <- min(data_source$Age)
    }
    data_source_filter <- data_source %>% dplyr::filter(.data$Age >=
                                                          age_threshold)
    dsf_age_old <- min(data_source_filter$Age)
    dsf_age_young <- max(data_source$Age)
    cvar_age_old <- min(cvar$age)
    cvar_age_young <- max(cvar$age)
    age_young <- max(dsf_age_young, cvar_age_young)
    age_old <- min(dsf_age_old, cvar_age_old)
    x_lab <- "CE (years)"
  }


  ### Get threshold -----------------------------------------------------------
  util_check_class("Roc_threshold", c("NULL", "numeric"))

  if (is.null(Roc_threshold) == TRUE) {
    Roc_threshold <- max(data_source$ROC_up)
  }

  ### Further check-ups -------------------------------------------------------
  util_check_class("Peaks", "logical")
  util_check_class("trend", c("NULL", "character"))


  # Make plot -----------------------------------------------------------------

  ### Plot with cvar ----------------------------------------------------------
  if (aside == TRUE) {

    cvar_res <-
      ggplot2::ggplot(cvar,
                      mapping = ggplot2::aes(y = cvar_ts,
                                             x = age)) +
      ggplot2::theme_classic() +
      # ggplot2::scale_x_continuous(limits = c(age_young, age_old)) +
      ggplot2::geom_vline(xintercept = age_old,
                          colour = "gray90", linewidth = 0.1) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = cvar_up,
                                                  ymax = cvar_dw),
                           fill = "gray90") +
      ggplot2::geom_line(alpha = 1, linewidth = 1, color = cvar_col) +
      ggplot2::labs(x = "", y = cvar_name)
    if (age_scale == "calBP") {
      cvar_res <- cvar_res +
        ggplot2::scale_x_continuous(limits = c(age_old, age_young),
                                    trans = "reverse")
    }
    if (age_scale == "CE") {
      cvar_res <- cvar_res +
        ggplot2::scale_x_continuous(limits = c(age_old, age_young))
    }

    p_res <-
      ggplot2::ggplot(data_source_filter,
                      mapping = ggplot2::aes(y = .data$ROC, x = .data$Age)) +
      ggplot2::theme_classic() +
      # ggplot2::scale_x_continuous(limits = c(age_young, age_old)) +
      ggplot2::geom_vline(xintercept = age_old,
                          colour = "gray90", linewidth = 0.1) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = .data$ROC_up,
                                                  ymax = .data$ROC_dw),
                           fill = "gray90") +
      ggplot2::geom_line(alpha = 1, linewidth = 1, color = "gray30") +
      ggplot2::labs(x = x_lab, y = "Rate of change score")
    if (age_scale == "calBP") {
      p_res <- p_res +
        ggplot2::scale_x_continuous(limits = c(age_old, age_young),
                                    trans = "reverse")
    }
    if (age_scale == "CE") {
      p_res <- p_res +
        ggplot2::scale_x_continuous(limits = c(age_old, age_young))
    }
  }

  ### classic RRatepol plot ---------------------------------------------------
  if (aside == FALSE) {

    ## Get cvar for ages in data_source_filter
    dsf_age <- data_source_filter$Age
    data_source_filter$cvar <- approx(cvar_age, cvar$cvar_ts, xout = dsf_age)$y

    ## Get top_age and bot_age:
    ids <- seq(1, dim(data_source_filter)[1], 1)
    age_diff <- c(diff(data_source_filter$Age), NA)
    age_diff[length(age_diff)] <- age_diff[length(age_diff) - 1]
    age_top <- data_source_filter$Age - age_diff/2
    age_bot <- data_source_filter$Age + age_diff/2
    y_up <- data_source_filter$ROC_up
    y_dw <- data_source_filter$ROC_dw
    y_m <- data_source_filter$ROC
    value <- data_source_filter$cvar

    values <- data.frame(ids = ids, value = value)
    s1 <- data.frame(ids = ids, id = ids + 0.1, age = age_bot,
                     y = y_dw, y_m = y_m)
    s2 <- data.frame(ids = ids, id = ids + 0.2, age = age_top,
                     y = y_dw, y_m = y_m)
    s3 <- data.frame(ids = ids, id = ids + 0.3, age = age_top,
                     y = y_up, y_m = y_m)
    s4 <- data.frame(ids = ids, id = ids + 0.4, age = age_bot,
                     y = y_up, y_m = y_m)
    positions <- rbind(s1,s2,s3,s4)
    positions <- positions[order(positions$id), ]

    # Currently we need to manually merge the two together
    datapoly <- merge(values, positions, by = c("ids"))

    p_res <- ggplot2::ggplot(datapoly,
                             mapping = ggplot2::aes(x = age,
                                                    y = y)) +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(trans = "reverse") +
      ggplot2::geom_polygon(ggplot2::aes(fill = value, group = ids)) +
      ggplot2::scale_fill_viridis_c(option = "B", direction = -1,
                                    ggplot2::guides(title = cvar_name))
    p_res <-
      p_res + ggplot2::geom_line(data_source_filter,
                                 mapping = ggplot2::aes(y = .data$ROC,
                                                        x = .data$Age),
                                 linewidth = 1, color = "gray50") +
      ggplot2::geom_hline(yintercept = 0, color = "gray30", lty = 3) +
      ggplot2::labs(x = "Age (cal yr BP)", y = "Rate of change score")
  }

  ## Add trend ----------------------------------------------------------------
  if (is.null(trend) == FALSE) {
    util_check_vector_values("trend", c("threshold", "trend_linear",
                                        "trend_non_linear"))
    if (Peaks == FALSE) {
      util_output_comment(msg = paste("'trend' has been set to NOT 'NULL',",
                                      "'Peaks' will be plotted"))
      Peaks <- TRUE
    }
    if (trend == "threshold") {
      p_res <- p_res +
        ggplot2::geom_hline(yintercept = stats::median(data_source_filter$ROC),
                            color = "blue", linewidth = 1)
    }
    if (trend == "trend_linear") {
      p_res <- p_res +
        ggplot2::geom_line(data = data.frame(
          ROC = util_make_trend(data_source = data_source,
                                sel_method = "linear"),
          Age = data_source$Age), color = "blue", linewidth = 1)
    }
    if (trend == "trend_non_linear") {
      p_res <- p_res +
        ggplot2::geom_line(data = data.frame(
          ROC = util_make_trend(data_source = data_source,
                                sel_method = "non_linear"),
          Age = data_source$Age), color = "blue", linewidth = 1)
    }
  }
  ## Add peaks ----------------------------------------------------------------
  if (Peaks == TRUE) {
    util_check_col_names("data_source", "Peak")
    d_peaks <- data_source_filter %>% dplyr::filter(.data$Peak == TRUE)
    d_peaks$y <- max(d_peaks$ROC_up)
    p_res <- p_res +
      ggplot2::geom_point(d_peaks,
                          mapping = ggplot2::aes(x = .data$Age,
                                                 y = .data$y),
                          color = "red", alpha = 1, size = 2, shape = 3)
  }

  if (aside == FALSE) {
    ps <- p_res
  }
  if (aside == TRUE) {
    ps <- list(p_res = p_res, cvar_res = cvar_res)
    ps <- ggpubr::ggarrange(ps$cvar_res, ps$p_res,
                            ncol = 1, nrow = 2, align = "v")
  }
  return(ps)
}
