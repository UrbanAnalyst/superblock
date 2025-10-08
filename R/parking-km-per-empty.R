#' Total kilometres driven to fill all parking spaces
#'
#' @inheritParams sb_parking_times
#' @param n_unfilled Number of parking spaces left unfilled. Each final space
#' takes far longer than the previous one, so that setting this to any value >
#' 0 will generally reduce the final estimate of total distance driven.
#'
#' @noRd
sb_parking_km_per_empty <- function (osmdat,
                                     prop_min = 0.7,
                                     n_props = 10,
                                     ntrials = 10,
                                     n_unfilled = 0) {

    requireNamespace ("pbapply", quietly = TRUE)

    net <- parking_as_dodgr_net (osmdat)
    emap <- make_edge_to_edge_map (net)
    emap_rev <- make_edge_to_edge_map (net, rev = TRUE)

    prop_full <- seq (prop_min, 1, length.out = n_props + 1)
    prop_full <- prop_full [-length (prop_full)]

    res <- pbapply::pblapply (prop_full, function (p) {
        simulate_km_per_empty (
            net,
            emap,
            emap_rev,
            prop_full = p,
            ntrials = ntrials,
            n_unfilled = n_unfilled
        )
    })

    data.frame (
        prop_full = prop_full,
        y = unlist (res),
        n_unfilled = as.character (n_unfilled)
    )
}

#' Run one simulation of total kilometres driven to fill all spaces, dividing
#' the final result by number of spaces filled.
#' @noRd
simulate_km_per_empty <- function (net,
                                   emap,
                                   emap_rev,
                                   prop_full,
                                   ntrials,
                                   n_unfilled = 0) {

    x <- rcpp_park_fill (
        net, emap, emap_rev,
        prop_full = prop_full,
        ntrials = ntrials,
        n_unfilled = n_unfilled
    )
    total_parking_spaces <- sum (net$np)
    n_empty <- ceiling (total_parking_spaces * (1 - prop_full)) - n_unfilled
    mean (x) / (1000 * n_empty)
}
