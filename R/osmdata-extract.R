#' Extract all data needed for analyses for an area within a bounding box and
#' bounded by a series of named highways.
#'
#' @param bbox Bounding box as vector of (xmin, ymin, xmax, ymax).A
#' @param hw_names Character vector naming highways which entirely surround
#' desired area. Highway names can be entrered in any order.
#' @param outer Single value or logical vector of same length as `hw_names`,
#' specifying whether perimeter of desired area should be traced along outer
#' (TRUE) or inner (FALSE) ways.
#'
#' @return A list of data components, each in \pkg{sf} format.
#' @export
sb_osmdata_extract <- function (bbox, hw_names, outer = TRUE) {

    bounding_poly <- extract_bounding_polygon (bbox, hw_names, outer)
    hws <- extract_osm_highways (bbox, bounding_poly)
    buildings <- extract_osm_buildings (bbox, bounding_poly)
    open_spaces <- extract_osm_open_spaces (bbox, bounding_poly)

}

extract_bounding_polygon <- function (bbox, hw_names, outer = TRUE) {
    p <- connect_highways (highways = highways, bbox = bbox, outer = outer)
    p <- sf::st_polygon (list (p)) |>
        sf::st_sfc (crs = 4326)
    sf::st_sf (geometry = p)
}

extract_osm_highways <- function (bbox, bounding_poly) {

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_feature (key = "highway") |>
        osmdata::osmdata_sf ()

    index <- sf::st_within (dat$osm_lines$geometry, bounding_poly, sparse = FALSE)
    index <- which (index [, 1])

    hws <- dat$osm_lines [index, ]
    nms <- names (hws) [which (!names (hws) == "geometry")]
    has_data <- vapply (nms, function (n) any (!is.na (hws [[n]])), logical (1L))
    has_data <- c (which (has_data), which (nms == "geometry"))
    hws [, has_data]
}

extract_osm_buildings <- function (bbox, bounding_poly) {

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_feature (key = "building") |>
        osmdata::osmdata_sf ()
    index <- sf::st_within (dat$osm_polygons, bounding_poly, sparse = FALSE)
    dat$osm_polygons [which (index [, 1]), ]
}

extract_osm_open_spaces <- function (bb0x, bounding_poly) {

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_features (list (
            leisure = c ("park", "playground"),
            natural = NULL
        )) |>
        osmdata::osmdata_sf ()

    index <- sf::st_within (dat$osm_polygons, bounding_poly, sparse = FALSE)
    open_spaces <- dat$osm_polygons [which (index [, 1]), ]
    open_spaces <- open_spaces [which (!open_spaces$access %in% c ("private", "customers")), ]

    # Then reduce to enclosing polygons:
    index <- sf::st_intersects (open_spaces)
    index_to_merge <- sort (unique (unlist (lapply (index, function (i) i [-1]))))
    index_to_keep <- seq_len (nrow (open_spaces)) [-index_to_merge]
    for (i in index_to_keep) {
        index_i <- index [[i]]
        # open_spaces [i, ] <- sf::st_union (open_spaces [index_i, ])
        open_spaces$geometry [i] <- sf::st_union (open_spaces$geometry [index_i])
    }
    open_spaces [index_to_keep, ]
}
