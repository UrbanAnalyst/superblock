#' @export
plot.sb_parking <- function (x, ...) {

    # suppress no visible binding notes:
    prop <- time <- what <- NULL

    requireNamespace ("ggplot2", quietly = TRUE)

    dat <- data.frame (
        prop = rep (x$prop, times = 4L),
        time = c (
            x$d0_50 + x$dwalk_50,
            x$d0_75 + x$dwalk_75,
            x$d0_90 + x$dwalk_90,
            x$time_to_parking + x$time_from_parking
        ),
        what = rep (c ("park50", "park75", "park90", "garage"), each = nrow (x))
    )

    index <- grep ("park", dat$what)
    dat$what [index] <- paste0 (gsub ("park", "park(", dat$what [index]), "%)")

    ggplot2::theme_set (ggplot2::theme_minimal ())
    ggplot2::ggplot (dat, ggplot2::aes (x = prop, y = time, colour = what)) +
        ggplot2::geom_line (lty = 2) +
        ggplot2::geom_smooth (method = "loess", se = FALSE) +
        ggplot2::xlab ("Proportion of parking spaces occupied") +
        ggplot2::ylab ("Time (minutes)") +
        ggplot2::scale_y_log10 () +
        ggplot2::theme (
            legend.position = "inside",
            legend.position.inside = c (0.2, 0.8)
        )
}
