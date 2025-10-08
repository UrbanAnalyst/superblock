#' @export
plot.sb_parking <- function (x, ...) {

    # suppress no visible binding notes:
    prop <- time <- what <- NULL

    requireNamespace ("ggplot2", quietly = TRUE)

    dcols <- grep ("^d0\\_[0-9]+", names (x), value = TRUE)
    quantiles <- gsub ("^d0\\_", "", dcols)
    times <- lapply (quantiles, function (q) {
        x [[paste0 ("d0_", q)]] + x [[paste0 ("dwalk_", q)]]
    })
    time_parking <- x$time_to_parking + x$time_from_parking
    time_names <- paste0 ("park", quantiles)

    dat <- data.frame (
        prop = rep (x$prop, times = 4L),
        time = c (do.call (c, times), time_parking),
        what = rep (c (time_names, "garage"), each = nrow (x))
    )

    index <- grep ("park", dat$what)
    dat$what [index] <- paste0 (gsub ("park", "park(", dat$what [index]), "%)")

    ggplot2::theme_set (ggplot2::theme_minimal ())
    ggplot2::ggplot (dat, ggplot2::aes (x = prop, y = time, colour = what)) +
        ggplot2::geom_line (lty = 2) +
        ggplot2::geom_smooth (method = "loess", formula = "y ~ x", se = FALSE) +
        ggplot2::xlab ("Proportion of parking spaces occupied") +
        ggplot2::ylab ("Time (minutes)") +
        ggplot2::scale_y_log10 () +
        ggplot2::theme (
            legend.position = "inside",
            legend.position.inside = c (0.2, 0.8)
        )
}
