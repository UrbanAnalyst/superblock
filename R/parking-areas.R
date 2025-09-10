car_parking_areas <- function (osmdat, add_parking_osm_ids = NULL) {

    lane_width <- 4

    hws <- reduce_osm_highways (osmdat$highways, osmdat$hw_names)
    hws$lanes [which (is.na (hws$lanes))] <- 1
    hws$lanes <- as.numeric (hws$lanes)
    hws$road_area <- sf::st_length (hws) * units::set_units (hws$lanes * lane_width)
    hws <- parking_structure (hws)

    if (!is.null (add_parking_osm_ids)) {
        index <- which (as.numeric (hws$parking_area) > 0)
        parking_mean <- mean (hws$parking_area [index])
        index <- match (add_parking_osm_ids, hws$osm_id)
        hws$parking_area [index] <- parking_mean
    }

    return (hws)
}

parking_structure <- function (hws) {

    # Depth of parking spaces out into the street
    depths <- c (3, 4, 5) # (parallel, diagonal, perpendicular)
    # And equivalent lengths along the street
    lengths <- c (5, 3, 3)

    parking <- sf::st_drop_geometry (hws [, grep ("parking", names (hws))])
    hws <- hws [, which (!grepl ("parking", names (hws)))]

    parking <-
        parking [, which (!grepl ("condition|restriction", names (parking)))]
    parking <- parking [, apply (parking, 2, function (i) any (!is.na (i)))]

    side_dir <- function (parking, side = "left", depths = depths) {

        parking_here <- parking [, grep (side, names (parking)), drop = FALSE]
        dirs <- c ("no|false", "parallel", "diagonal", "perpendicular")
        conditions <- lapply (dirs, function (d) {
            apply (
                parking_here,
                1,
                function (i) any (grepl (d, i, ignore.case = TRUE))
            )
        })
        conditions <- do.call (cbind, conditions)

        parking_space <- rep (0, nrow (parking))
        for (i in 2:4) {
            parking_space [which (conditions [, i])] <- depths [i - 1L]
        }

        # And set any "no" or "false" flags to no parking space:
        parking_prohibited <- conditions [, 1]

        return (cbind (parking_space, parking_prohibited))
    }

    dirs <- c ("left", "right", "both")
    parking <- lapply (
        dirs,
        function (i) side_dir (parking, i, depths)
    )

    parking_prohibited <- lapply (parking, function (i) which (i [, 2] == 1))
    hws$parking_prohibited <- NA_character_
    hws$parking_prohibited [parking_prohibited [[1]]] <- "left"
    hws$parking_prohibited [parking_prohibited [[2]]] <- "right"
    hws$parking_prohibited [parking_prohibited [[3]]] <- "both"

    parking_sides <- parking [[1]] [, 1] + parking [[2]] [, 1]
    parking_both <- parking [[3]] [, 1] * 2
    parking_space <- apply (cbind (parking_sides, parking_both), 1, max)
    hws$parking_area <- sf::st_length (hws) * units::set_units (parking_space, "m")

    # Then estimate number of parking spaces:
    hw_lens <- as.numeric (sf::st_length (hws))
    n_spaces <- lapply (lengths, function (l) {
        res <- lapply (
            parking,
            function (p) {
                index <- which (p [, 1] > 0 & p [, 1] %% l == 0)
                index_ids <- match (names (index), hws$osm_id)
                n <- floor (hw_lens [index_ids] / l)
                cbind (n, names (index))
            }
        )
        do.call (rbind, res)
    })
    n_spaces <- do.call (rbind, n_spaces)
    n_spaces <- data.frame (
        osm_id = n_spaces [, 2],
        n = as.integer (n_spaces [, 1])
    ) |>
        dplyr::group_by (osm_id) |>
        dplyr::summarise (n = sum (n)) |>
        dplyr::ungroup ()

    hws$num_parking_spaces <- rep (0L, nrow (hws))
    hws$num_parking_spaces [match (n_spaces$osm_id, hws$osm_id)] <- n_spaces$n

    return (hws)
}
