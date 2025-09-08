# Functions to inspect OSM elements with missing or inadequate data.

#' Identify and optionally browse Open Street Map (OSM) highways with missing
#' parking data.
#'
#' @param osmdat Main object returned by \link{sb_osmdata_extract} function.
#' @param min_len Minimal length in metres of ways to inspect. Shorter ways are
#' likely just connector elements which often neither have not require parking
#' data.
#' @param browse If `TRUE` (default), open OSM pages for all identified ways in
#' default web browser.
#' @return (Invisibly) A `data.frame` object with columns of OSM identifier
#' values for each identified way, and corresponding URLs.
#' @export
no_parking_ways <- function (osmdat, min_len = 20, browse = TRUE) {

    parking <- car_parking_areas (osmdat)

    lens <- as.numeric (sf::st_length (parking))
    index <- which (
        as.numeric (parking$parking_area) == 0 &
            is.na (parking$parking_prohibited) &
            lens > min_len
    )

    ret <- data.frame (osm_id = character (0), url = character (0))
    if (length (index) > 0) {
        osm_id <- parking$osm_id [index]
        urls <- paste0 ("https://openstreetmap.org/way/", osm_id)
        if (browse) {
            val <- lapply (urls, utils::browseURL)
        }

        ret <- data.frame (osm_id = osm_id, url = urls)
    }

    invisible (ret)
}

#' Identify and optionally browse Open Street Map (OSM) buildings with missing
#' data on numbers of levels.
#'
#' @inheritParams no_parking_ways
#' @export
no_building_floors <- function (osmdat, browse = TRUE) {

    # suppress no vis binding note:
    building <- NULL

    these_types <- c ("apartments", "residential", "yes")
    b <- dplyr::filter (osmdat$buildings, building %in% these_types)

    index <- which (is.na (b$`building:levels`))
    ret <- data.frame (
        osm_id = character (0),
        url = character (0),
        street = character (0),
        housenumber = character (0),
        levels = double (0),
        roof = double (0)
    )

    if (length (index) > 0) {

        osm_id <- b$osm_id [index]
        urls <- paste0 ("https://openstreetmap.org/way/", osm_id)
        if (browse) {
            val <- lapply (urls, utils::browseURL)
        }

        ret <- data.frame (
            osm_id = osm_id,
            url = urls,
            street = b$`addr:street` [index],
            housenumber = b$`addr:housenumber` [index],
            levels = b$`building:levels` [index],
            roof = b$`roof:levels` [index]
        )
    }

    invisible (ret)

}
