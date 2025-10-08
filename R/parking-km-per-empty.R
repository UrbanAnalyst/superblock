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

    # The value of 'n' passed here doesn't matter at all
    d_to_parking <- km_per_empty_to_parking (osmdat)

    # Convert all distances to km:
    data.frame (
        prop_full = prop_full,
        d = unlist (res) / 1000,
        n_unfilled = as.character (n_unfilled),
        d_to_parking = mean (d_to_parking, na.rm = TRUE) / 1000
    )
}

#' Run one simulation of total kilometres driven to fill all spaces, dividing
#' the final result by number of spaces filled.
#'
#' @return A named vector of distances from each building to nearest parking
#' facility. The equivalent kilometres travelled per empty space can then be
#' obtained by sampling the equivalent number of these distances and
#' aggregating.
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
    mean (x) / n_empty
}

#' Equilvalent km per empty car park for travelling to nearest parking
#' facilities.
#' @noRd
km_per_empty_to_parking_internal <- function (osmdat) {

    b <- buildings_to_highways (osmdat)
    net <- dodgr::weight_streetnet (
        osmdat$dat_sc,
        wt_profile = "motorcar",
        turn_penalty = TRUE
    )
    v <- dodgr::dodgr_vertices (net)
    v_from <- v [which (v$id %in% net$.vx0), ]
    index <- dodgr::match_points_to_verts (v_from, b [, c ("lon", "lat")])
    from_ids <- v$id [index]

    to_pts <- sf::st_coordinates (osmdat$parking_facilities)
    v_to <- v [which (v$id %in% net$.vx1), ]
    index <- dodgr::match_points_to_verts (v_to, to_pts)
    to_ids <- v_to$id [index]

    # Then route from buildings to parking facilities, but only start at first
    # vertex that is not within the bounding polygon.
    hws_within <- osmdat$highways |>
        dplyr::filter (!name %in% osmdat$hw_names)
    nodes_within <- sf::st_coordinates (hws_within) |>
        rownames ()

    if (length (to_ids) == 0L) {
        cli::cli_abort ("No parking facilities found")
    }

    paths <- dodgr::dodgr_paths (net, from = from_ids, to = to_ids)
    path_ends <- lapply (paths, function (i) {
        ends_i <- lapply (i, function (j) {
            this_path <- j [which (!j %in% nodes_within)]
            c (j [1], j [length (j)], this_path [c (1, length (this_path))])
        })
        do.call (rbind, ends_i)
    })
    path_ends <- data.frame (do.call (rbind, path_ends))
    rownames (path_ends) <- NULL
    names (path_ends) <- c ("b_from", "p_to", "b_from_remap", "p_to_remap")

    index <- match (from_ids, path_ends$b_from)
    # But not all paths are traversable, so revert any that aren't back to
    # original start points:
    index_na <- which (is.na (index))
    from_xy <- net [match (from_ids [index_na], net$.vx0), c (".vx0_x", ".vx0_y")]
    b_from_xy <- net [match (path_ends$b_from_remap, net$.vx0), c (".vx0_x", ".vx0_y")]
    names (from_xy) <- names (b_from_xy) <- c ("x", "y")
    index_min <- geodist::geodist_min (from_xy, b_from_xy)
    index [index_na] <- index_min

    from_ids <- path_ends$b_from_remap [index]
    # Those are then from vertices closest to each building which head directly
    # to a parking facility.
    dmat <- dodgr::dodgr_distances (net, from = from_ids, to = to_ids)
    dvec <- apply (dmat, 1, min, na.rm = TRUE)

    return (dvec)
}

km_per_empty_to_parking <- memoise::memoise (km_per_empty_to_parking_internal)
