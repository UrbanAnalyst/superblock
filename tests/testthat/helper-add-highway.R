#' Add an extra internal highway to memoised data so 'hws_to_polygons()' can
#' work
helper_extra_highway <- function (osmdat) {

    h_from <- dplyr::filter (
        osmdat$highways,
        grepl ("Emdener", osmdat$highways$name)
    )
    xy_from <- apply (sf::st_coordinates (h_from), 2, mean) [1:2]
    h_to <- dplyr::filter (
        osmdat$highways,
        grepl ("Dortmund", osmdat$highways$name)
    )
    xy_to <- apply (sf::st_coordinates (h_to), 2, mean) [1:2]
    l <- sf::st_linestring (rbind (xy_from, xy_to)) |>
        sf::st_sfc (crs = 4326)
    d <- as.numeric (sf::st_length (l))
    l <- sf::st_transform (l, "+proj=merc +a=6378137 +b=6378137")
    n <- ceiling (d / 10)
    ints <- seq (0, d, length.out = n)
    hw <- sf::st_line_interpolate (l, ints) |>
        sf::st_combine () |>
        sf::st_cast ("LINESTRING") |>
        sf::st_transform (4326)
    hw <- sf::st_sf (name = "name", highway = "residential", geometry = hw)
    osmdat$highways <- dplyr::bind_rows (osmdat$highways, hw)

    return (osmdat)
}
