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
    index_table <- table (index)
    index_divisors <- index_table [match (index, names (index_table))]
    n_spaces <- as.numeric (p$num_parking_spaces [index] / index_divisors)

    data.frame (
        osm_id = osmdat$buildings$osm_id,
        hw_id = rownames (xy),
        lon = xy [, 1],
        lat = xy [, 2],
        n_parking_spaces = n_spaces
    )
}

parking_time_matrix <- function (osmdat) {

    # suppress no visible binding notes:
    name <- NULL

    b <- buildings_to_highways (osmdat)

    # Travel times between all pairs of buildings, so between all buildings and
    # parking spaces.
    tmats <- lapply (c ("motorcar", "foot"), function (wp) {

        net <- dodgr::weight_streetnet (
            osmdat$dat_sc,
            wt_profile = wp,
            turn_penalty = TRUE
        )

        v <- dodgr::dodgr_vertices (net)
        index <- dodgr::match_points_to_verts (v, b [, c ("lon", "lat")])
        vert_ids <- v$id [index]

        tmat <- dodgr::dodgr_times (net, from = vert_ids, to = vert_ids)
        rownames (tmat) <- colnames (tmat) <- b$osm_id

        return (tmat)
    })

    # Then to find travel times to major parking facilities, first find for
    # each building the nearest entry point in the bounding highways.
    net <- dodgr::weight_streetnet (
        osmdat$dat_sc,
        wt_profile = "motorcar",
        turn_penalty = TRUE
    )
    v <- dodgr::dodgr_vertices (net)
    index <- dodgr::match_points_to_verts (v, b [, c ("lon", "lat")])
    from_ids <- v$id [index]

    to_pts <- sf::st_coordinates (osmdat$parking_facilities)
    index <- dodgr::match_points_to_verts (v, to_pts)
    to_ids <- v$id [index]

    # Then route from buildings to parking facilities, but only start at first
    # vertex that is not within the bounding polygon.
    hws_within <- osmdat$highways |>
        dplyr::filter (!name %in% osmdat$hw_names)
    nodes_within <- sf::st_coordinates (hws_within) |>
        rownames ()

    # Only run this if there are parking facilities:
    if (length (to_ids) > 0L) {
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
        from_ids <- path_ends$b_from_remap [index]
        # Those are then from vertices closest to each building which head directly
        # to a parking facility.

        tmat <- dodgr::dodgr_times (net, from = from_ids, to = to_ids)
        tvec <- apply (tmat, 1, min, na.rm = TRUE)
        parking_ids <- names (tvec)
        b$time_to_parking <- unname (tvec)

        # Then finally time from parking back to buildings:
        net <- dodgr::weight_streetnet (
            osmdat$dat_sc,
            wt_profile = "foot"
        )
        tmat <- dodgr::dodgr_times (net, from = parking_ids, to = to_ids)
        b$time_from_parking <- apply (tmat, 1, min, na.rm = TRUE)
    }

    list (buildings = b, tmat_car = tmats [[1]], tmat_foot = tmats [[2]])
}

parking_as_dodgr_net <- function (osmdat) {

    # suppress no visible binding notes:
    key <- value <- name <- object_ <- .vx0 <- .vx1 <-
        edge_ <- d <- highway <- lanes <- osm_id <-
        num_parking_spaces <- edge_new <- NULL

    requireNamespace ("fs")

    net <- dodgr::weight_streetnet (
        osmdat$dat_sc,
        wt_profile = "motorcar",
        turn_penalty = TRUE
    )

    # Reduce highways to those both within and surrounding defined bounding
    # polygon:
    ids <- unique (osmdat$highways$osm_id)
    names <- osmdat$dat_sc$object |>
        dplyr::filter (key == "name") |>
        dplyr::select (!key) |>
        dplyr::rename (name = value)
    these_names <- c (unique (osmdat$highways$name, osmdat$hw_names))
    names <- names |>
        dplyr::filter (name %in% osmdat$highways$name)
    hws <- reduce_osm_highways (osmdat$highways, osmdat$hw_names)
    ids <- unique (c (ids, names$object_), hws$osm_id, )
    net <- dplyr::filter (net, object_ %in% ids)

    names <- osmdat$dat_sc$object |>
        dplyr::filter (key == "name") |>
        dplyr::select (!key) |>
        dplyr::rename (name = value)
    net <- dplyr::left_join (net, names, by = "object_") |>
        dplyr::select (.vx0, .vx1, edge_, d, object_, highway, lanes, name)

    # Extract car parking data, and add original OSM id values from full graph:
    parking <- car_parking_areas (osmdat) |>
        sf::st_drop_geometry () |>
        dplyr::select (osm_id, num_parking_spaces)
    index <- match (parking$osm_id, net$object_)
    parking$edge_old <- net$edge_ [index]

    netc <- dodgr::dodgr_contract_graph (net)

    # Then read cached edge map and aggregate all parking on to contracted
    # edges:
    hashc <- attr (netc, "hashc")
    tmpfiles <- fs::dir_ls (fs::path_temp (), regexp = paste0 (hashc, "\\.Rds$"))
    edge_map <- readRDS (grep ("edge", tmpfiles, value = TRUE))
    parking <- dplyr::left_join (parking, edge_map, by = "edge_old")

    index <- which (is.na (parking$edge_new))
    parking$edge_new [index] <- parking$edge_old [index]
    parking <- dplyr::group_by (parking, edge_new) |>
        dplyr::summarise (num_parking_spaces = sum (num_parking_spaces)) |>
        dplyr::ungroup () |>
        dplyr::rename (edge_ = edge_new, np = num_parking_spaces)

    netc <- dplyr::left_join (netc, parking, by = "edge_")
    index <- which (is.na (netc$np))
    netc$np [index] <- 0L

    return (netc)
}

make_edge_to_edge_map <- function (graph, rev = FALSE) {

    if (!rev) {
        ret <- lapply (net$.vx1, function (i) which (net$.vx0 == i) - 1L)
    } else {
        ret <- lapply (net$.vx0, function (i) which (net$.vx1 == i) - 1L)
    }
    return (ret)
}

# Frmo dodgr:
swap_cols <- function (x, cola, colb) {
    temp <- x [[cola]]
    x [[cola]] <- x [[colb]]
    x [[colb]] <- temp
    return (x)
}

#' Convert a contracted network weighted for motorcar routing to an equivalent
#' pedestrian network.
#'
#' @param walk_speed In km/hr
#' @noRd
net_to_walk <- function (net, walk_speed = 5.0) {

    net_new <- net
    net_new <- swap_cols (net_new, ".vx0", ".vx1")
    net_new$edge_ <- paste0 (net_new$edge_, "_rev")

    v_old <- paste0 (net$.vx0, "-", net$.vx1)
    v_new <- paste0 (net_new$.vx0, "-", net_new$.vx1)
    index <- which (!v_new %in% v_old)
    net_new <- net_new [index, ]

    net_new <- rbind (net, net_new)

    # Then adjust distances to walking times:
    net_new$d <- net_new$d * 60 / (walk_speed * 1000)

    # Create new df to remove all dodgr attributes:
    data.frame (
        from = net_new$.vx0,
        to = net_new$.vx1,
        edge_ = net_new$edge_,
        d = net_new$d
    )
}


parking_time_simulate <- function (net,
                                   net_walk,
                                   emap,
                                   emap_rev,
                                   prop_full,
                                   start_edge,
                                   ntrials) {

    res <- rcpp_park_search (
        net, emap, emap_rev,
        prop_full = prop_full,
        start_edge = start_edge,
        ntrials = ntrials
    )
    res <- res [which (res$d > 0), ]
    if (nrow (res) == 0L) {
        return (c (NA_real_, NA_real_))
    }

    vfr <- rep (net$.vx0 [start_edge], times = ntrials)
    vto <- net$.vx1 [res$edge]
    d_walk <- dodgr::dodgr_dists (net_walk, from = vfr, to = vto, pairwise = TRUE)

    # Then subtract average of half the walking times from first and last
    # edges:
    t_minus <- (net_walk$d [start_edge] + net_walk$d [res$edge]) / 2
    d_walk <- d_walk - t_minus

    # Convert initial distance to time:
    m_to_time <- 60 / 10000 # 10km / hr
    d0 <- res$d * m_to_time

    c (mean (d0), mean (d_walk))
}
