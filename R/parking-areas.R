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

    parking <- sf::st_drop_geometry (hws [, grep ("parking", names (hws))])
    hws <- hws [, which (!grepl ("parking", names (hws)))]

    parking <-
        parking [, which (!grepl ("condition|restriction", names (parking)))]
    parking <- parking [, apply (parking, 2, function (i) any (!is.na (i)))]

    side_dir <- function (parking, side = "left") {

        widths <- c (3, 4, 5) # (parallel, diagonal, perpendicular)

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
            parking_space [which (conditions [, i])] <- widths [i = 1L]
        }

        return (parking_space)
    }

    parking_sides <- side_dir (parking, "left") + side_dir (parking, "right")
    parking_both <- side_dir (parking, "both") * 2
    parking_space <- apply (cbind (parking_sides, parking_both), 1, max)
    parking_area <- sf::st_length (hws) * units::set_units (parking_space, "m")

    hws$parking_area <- parking_area
    return (hws)
}
