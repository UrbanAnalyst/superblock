#' Map building polygons to nearest highway nodes, and also add estimated
#' number of parking spaces for each building.
#'
#' @noRd
buildings_to_highways <- function (osmdat) {

    hws <- reduce_osm_highways (osmdat$highways, osmdat$hw_names)
    xy_hw <- sf::st_coordinates (hws) [, 1:2]
    # Those are reduced to within bounding polygon, and include node osm_id
    # values as rownames.

    xy <- lapply (osmdat$buildings$geometry, function (b) {
        xy_b <- sf::st_coordinates (b) [, 1:2]
        i_b <- geodist::geodist_min (xy_hw, xy_b)
        d <- geodist::geodist (xy_hw, xy_b [i_b, ], paired = TRUE)
        i <- which.min (d)
        xy_hw [i, , drop = FALSE]
    })
    xy <- do.call (rbind, xy)
    # xy then has a node number from the highway data.

    # Convert node number to a way id from the actual highway.
    hw_ids <- lapply (seq_len (nrow (hws)), function (i) {
        g <- hws$geometry [[i]]
        node_id <- rownames (g)
        way_id <- hws$osm_id [i]
        cbind (way_id = rep (way_id, length (node_id)), node_id)
    })
    hw_id_dat <- data.frame (do.call (rbind, hw_ids))

    # Then re-map node ids to way ids:
    index <- match (rownames (xy), hw_id_dat$node_id)
    rownames (xy) <- hw_id_dat$way_id [index]

    p <- car_parking_areas (osmdat) # add "num_parking_spaces" to hw data
    index <- match (rownames (xy), p$osm_id)

    data.frame (
        osm_id = osmdat$buildings$osm_id,
        hw_id = rownames (xy),
        lon = xy [, 1],
        lat = xy [, 2],
        n_parking_spaces = p$num_parking_spaces [index]
    )
}

parking_time_matrix <- function (osmdat) {

    b <- buildings_to_highways (osmdat)

    net <- dodgr::weight_streetnet (
        osmdat$dat_sc,
        wt_profile = "motorcar",
        turn_penalty = TRUE
    )
    v <- dodgr::dodgr_vertices (net)
    index <- dodgr::match_points_to_verts (v, b [, c ("lon", "lat")])
    b$node_id <- v$id [index]

    tmat <- dodgr::dodgr_dists (net, from = b$node_id, to = b$node_id)
    rownames (tmat) <- colnames (tmat) <- b$osm_id

    list (buildings = b, tmat = tmat)
}
