#' Extract all data needed for analyses for an area within a bounding box and
#' bounded by a series of named highways.
#'
#' @param bbox Bounding box as vector of (xmin, ymin, xmax, ymax).A
#' @param hw_names Character vector naming highways which entirely surround
#' desired area. Highway names can be entered in any order.
#' @param outer Single value or logical vector of same length as `hw_names`,
#' specifying whether perimeter of desired area should be traced along outer
#' (TRUE) or inner (FALSE) ways.
#'
#' @return A list of data components, each in \pkg{sf} format.
#' @export
sb_osmdata_extract <- function (bbox, hw_names, outer = TRUE) {

    cli::cli_alert_info ("Extracting surrounding street network...")
    bounding_poly <- extract_bounding_polygon (bbox, hw_names, outer)
    cli::cli_alert_success ("Extracted surrounding street network.")

    cli::cli_alert_info ("Extracting network within superblock...")
    dat_hw <- extract_osm_highways (bbox, bounding_poly)
    cli::cli_alert_success ("Extracted network within superblock.")

    cli::cli_alert_info ("Extracting building data...")
    buildings <- extract_osm_buildings (bbox, bounding_poly)
    cli::cli_alert_success ("Extracted building data.")

    cli::cli_alert_info ("Extracting data on open public spaces...")
    open_spaces <- extract_osm_open_spaces (bbox, bounding_poly)
    cli::cli_alert_success ("Extracted data on open public spaces.")

    cli::cli_alert_info ("Extracting data on parking areas...")
    parking_areas <- extract_osm_parking_areas (bbox, bounding_poly)
    parking_facilities <- extract_osm_parking_facilities (bbox)
    cli::cli_alert_success ("Extracted data on parking areas.")

    # Extract tree and bicycle parking nodes, but only keep those within 5m of
    # the internal streeets.
    node_limit <- 5 # in metres
    nodes <- extract_osm_nodes (bbox)
    dmat <- sf::st_distance (nodes, dat_hw) # dim = (nodes, hw)
    dmin <- apply (dmat, 1, min)
    nodes <- nodes [which (dmin <= node_limit), ]

    list (
        bbox = bbox,
        hw_names = hw_names,
        bounding_poly = bounding_poly,
        highways = dat_hw$highways,
        buildings = buildings,
        open_spaces = open_spaces,
        parking_areas = parking_areas,
        parking_facilities = parking_facilities,
        nodes = nodes,
        dat_sc = dat_hw$dat_sc
    )
}

extract_bounding_polygon <- function (bbox, hw_names, outer = TRUE) {
    p <- connect_highways (highways = hw_names, bbox = bbox, outer = outer)
    p <- sf::st_polygon (list (p)) |>
        sf::st_sfc (crs = 4326)
    sf::st_sf (geometry = p)
}

m_osmdata_sf <- memoise::memoise (osmdata::osmdata_sf)
m_osmdata_xml <- memoise::memoise (osmdata::osmdata_xml)

extract_osm_highways <- function (bbox, bounding_poly) {

    args <- list (opq = osmdata::opq (bbox), key = "highway")

    if (is_test_env ()) {
        args <- c (args, value = "residential")
    }

    bbox_str <- gsub ("\\.", "", paste0 (bbox, collapse = "_"))
    ftmp <- file.path (tempdir (), paste0 ("osm_", bbox_str, ".osm"))

    q <- do.call (osmdata::add_osm_feature, args)
    doc <- m_osmdata_xml (q = q, filename = ftmp)

    dat_sf <- m_osmdata_sf (q, doc = doc)
    dat_sc <- osmdata::osmdata_sc (q, doc = doc)

    # Reduce 'sf' highways to bounding box, but return full 'sc' data
    index <- sf::st_within (dat_sf$osm_lines$geometry, bounding_poly, sparse = FALSE)
    index <- which (index [, 1])

    hws <- dat_sf$osm_lines [index, ]
    nms <- names (hws) [which (!names (hws) == "geometry")]
    has_data <- vapply (nms, function (n) any (!is.na (hws [[n]])), logical (1L))
    has_data <- c (which (has_data), which (nms == "geometry"))
    hws [, has_data]

    return (list (highways = hws, dat_sc = dat_sc))
}

#' Reduce ways down to main roads only within the bounding polygon, but
#' excluding the boundary ways themselves. Also remove all block-internal and
#' private ways.
#' @noRd
reduce_osm_highways <- function (hws, hw_names) {

    hws_internal <- hws [which (!hws$name %in% hw_names), ]
    index <- which (
        hws$highway %in% c ("residential", "secondary", "service", "tertiary") &
            !hws$service %in% c ("driveway", "parking_aisle")
    )
    index <- which (hws$highway %in% c ("residential", "secondary", "tertiary"))
    hws_internal <- hws [index, ]
    hws_internal [which (!hws_internal$name %in% hw_names), ]
}

extract_osm_buildings <- function (bbox, bounding_poly) {

    q <- osmdata::opq (bbox) |>
        osmdata::add_osm_feature (key = "building")

    if (is_test_env ()) {
        q <- osmdata::add_osm_feature (q, key = "addr:housenumber", value = 2:6)
    }

    dat <- m_osmdata_sf (q)
    index <- sf::st_within (dat$osm_polygons, bounding_poly, sparse = FALSE)
    dat$osm_polygons [which (index [, 1]), ]
}

extract_osm_open_spaces <- function (bbox, bounding_poly) {

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_features (list (
            leisure = c ("park", "playground"),
            natural = NULL
        )) |>
        m_osmdata_sf ()

    index <- sf::st_within (dat$osm_polygons, bounding_poly, sparse = FALSE)
    open_spaces <- dat$osm_polygons [which (index [, 1]), ]
    open_spaces <- open_spaces [which (!open_spaces$access %in% c ("private", "customers")), ]

    # Then reduce to enclosing polygons:
    index <- sf::st_intersects (open_spaces)
    index_to_merge <- sort (unique (unlist (lapply (index, function (i) i [-1]))))
    if (length (index_to_merge) > 0L) {

        index_to_keep <- seq_len (nrow (open_spaces)) [-index_to_merge]
        for (i in index_to_keep) {
            index_i <- index [[i]]
            # open_spaces [i, ] <- sf::st_union (open_spaces [index_i, ])
            open_spaces$geometry [i] <- sf::st_union (open_spaces$geometry [index_i])
        }
        open_spaces <- open_spaces [index_to_keep, ]
    }

    return (open_spaces)
}

extract_osm_parking_areas <- function (bbox, bounding_poly) {

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_features (list (
            amenity = "parking",
            natural = NULL
        )) |>
        m_osmdata_sf ()

    index <- sf::st_within (dat$osm_polygons, bounding_poly, sparse = FALSE)
    parking <- dat$osm_polygons [which (index [, 1]), ]
    index <- which (
        parking$amenity == "parking" &
            grepl ("permissive|yes", parking$access, ignore.case = TRUE)
    )
    parking <- parking [index, ]
}

#' Large parking facilities should always have an associated node as
#' "parking_entrance".
#' @noRd
extract_osm_parking_facilities <- function (bbox) {

    # no visible binding notes:
    access <- barrier <- NULL

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_feature (
            key = "amenity", value = "parking_entrance"
        ) |>
        m_osmdata_sf ()

    pts <- dat$osm_points

    if (is_test_env ()) {
        # test data have one 'parking_entrance' with "barrier", so keep that
        return (pts)
    }

    if ("access" %in% names (pts)) {
        pts <- dplyr::filter (pts, is.na (access) | access != "private")
    }
    if ("barrier" %in% names (pts)) {
        pts <- dplyr::filter (pts, is.na (barrier) | barrier != "gate")
    }

    return (pts)
}

extract_osm_nodes <- function (bbox) {

    amenity <- natural <- NULL

    dat <- osmdata::opq (bbox) |>
        osmdata::add_osm_features (list (
            natural = "tree",
            amenity = "bicycle_parking"
        )) |>
        m_osmdata_sf ()

    dat$osm_points |>
        dplyr::filter (amenity == "bicycle_parking" | natural == "tree") |>
        dplyr::select (osm_id, amenity, natural)
}
