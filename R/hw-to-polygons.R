#' Convert one highway as an \pkg{sf} "linestring" object into a polygon
#' defined by nearest points in "polygons".
#'
#' @param xy Two-column matrix of spatial coordinates
#' @param polygons An \pkg{sf} `data.frame` of "polygon" objects.
#' @return A matrix of coordinates tracing a polygon connecting nearest points
#' in the "polygons" argument, and tracing around the input line, "xy".
#' @noRd
hw_to_polygon <- function (xy, polygons) {

    xy0 <- xy
    bb <- sf::st_bbox (polygons)
    xy <- extend_xy_to_bb (xy, bb)

    xy_sf_p <- sfheaders::sf_polygon (xy) |> sf::st_sf (crs = 4326)
    index <- sf::st_within (all_polys, xy_sf_p, sparse = FALSE) [, 1]
    these_polys <- all_polys [which (index), ]
    those_polys <- all_polys [which (!index), ]

    get_nearest_poly_points <- function (xy, polys) {
        g_pts <- sf::st_coordinates (polys) [, 1:2]
        index <- geodist::geodist_min (xy, g_pts)
        g_pts [index, ]
    }
    line1 <- get_nearest_poly_points (xy0, these_polys)
    line2 <- get_nearest_poly_points (xy0, those_polys)

    this_poly <- rbind (line1, apply (line2, 2, rev), line1 [1, ])
    # this_poly_sf <- sfheaders::sf_linestring (this_poly) |> sf::st_sf (crs = 4326)
    # mapdeck::mapdeck () |> mapdeck::add_path (this_poly_sf)
    return (this_poly)
}

#' Extend line coordinate matrices to edge of bounding box
#'
#' @param xy Two-column matrix of coordinates
#' @param bb Vector of length 4 with (xmin, ymin, xmax, ymax)
#' @return Modified version of xy, with extra first row extrapolating first
#' edge out to bounding box, and extra final row extrapolating last edge.
#' @noRd
extend_xy_to_bb <- function (xy, bb) {

    # Extend line of 'xy0' to edges of geometry:
    get1mult <- function (xy_add1, bb2, xy1) {
        b <- ifelse (xy_add1 > 0, bb2 [2], bb2 [1])
        abs (ceiling ((b - xy1) / xy_add1))
    }
    get_mult <- function (xy_add, bb, xy) {
        max (c (
            get1mult (xy_add [1], bb [c (1, 3)], xy [1, 1]),
            get1mult (xy_add [2], bb [c (2, 4)], xy [1, 2])
        ))
    }

    # Head, with initial "-" because head is 1:2, but need sequence 2:1:
    xy_add <- -apply (utils::head (xy, n = 2), 2, diff)
    m <- get_mult (xy_add, bb, xy)
    xy <- rbind (head (xy, n = 1) + m * xy_add, xy)
    # Tail:
    xy_add <- apply (utils::tail (xy, n = 2), 2, diff)
    m <- get_mult (xy_add, bb, xy)
    xy <- rbind (xy, tail (xy, n = 1) + m * xy_add)

    xrange <- range (xy [, 1])
    yrange <- range (xy [, 2])

    index_x <- match (xrange, xy [, 1])
    move_x <- all (index_x %in% c (1, nrow (xy)))
    if (move_x) {
        # Extreme x values are 1st and last points
        if (xy [1, 1] == xrange [1] && xy [1, 2] == yrange [1]) {
            # bottom left -> bottom right
            xynew <- c (xrange [2], yrange [1])
        } else if (xy [1, 1] == xrange [1] && xy [1, 2] == yrange [2]) {
            # top left -> top right
            xynew <- c (xrange [2], yrange [2])
        } else if (xy [1, 1] == xrange [2] && xy [1, 2] == yrange [1]) {
            # bottom right -> bottom left
            xynew <- c (xrange [1], yrange [1])
        } else if (xy [1, 1] == xrange [2] && xy [1, 2] == yrange [2]) {
            # bottom right -> bottom left
            xynew <- c (xrange [2], yrange [1])
        }
    } else {
        # Extreme x values are somewhere inbetween, so connect to y-points
        if (xy [1, 1] == xrange [1] && xy [1, 2] == yrange [2]) {
            # top left -> bottom left
            xynew <- c (xrange [1], yrange [1])
        } else if (xy [1, 1] == xrange [1] && xy [1, 2] == yrange [1]) {
            # bottom left -> top left
            xynew <- c (xrange [2], yrange [2])
        } else if (xy [1, 1] == xrange [2] && xy [1, 2] == yrange [2]) {
            # top right -> bottom right
            xynew <- c (xrange [2], yrange [1])
        } else if (xy [1, 1] == xrange [2] && xy [1, 2] == yrange [1]) {
            # bottom right -> top right
            xynew <- c (xrange [2], yrange [2])
        }
    }
    xy <- rbind (xy, xynew, xy [1, ])

    return (xy)
}
