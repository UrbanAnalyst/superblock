#' Print screen summary of effect of superblock on public space.
#'
#' @param osmdat Object returned from \link{sb_osmdata_extract}.
#' @param hw_polygons Object returned from \link{hws_to_polygons}.
#' @param add_parking_osm_ids Optional list of 'OSM' id values for any ways
#' which have no parking specified in OpenStreetMap, which are then assigned
#' average values from all other ways which do have parking specified.
#' @export
sb_summary <- function (osmdat, hw_polygons = NULL, add_parking_osm_ids = NULL) {

    if (is.null (hw_polygons)) {
        hw_polygons <- hws_to_polygons (osmdat)
    }

    hws <- car_parking_areas (osmdat, add_parking_osm_ids = add_parking_osm_ids)
    car_parks_per_res <- sb_car_spaces_per_resident (osmdat)

    a_tot <- as.numeric (sf::st_area (osmdat$bounding_poly) / 10000)
    a_tot_f <- format (a_tot, digits = 3)
    a_bldg <- as.numeric (sum (sf::st_area (osmdat$buildings))) / 10000
    a_open <- as.numeric (sum (sf::st_area (osmdat$open_spaces))) / 10000
    a_hw <- as.numeric (sum (sf::st_area (hw_polygons))) / 10000
    a_road <- as.numeric (sum (hws$road_area)) / 10000
    a_parking <- as.numeric (sum (hws$parking_area)) / 10000

    a_bldg_prop <- format (100 * a_bldg / a_tot, digits = 3)
    a_open_prop <- format (100 * a_open / a_tot, digits = 3)
    a_hw_prop <- format (100 * a_hw / a_tot, digits = 3)
    a_parking_prop <- format (100 * a_parking / a_hw, digits = 3)
    a_road_prop <- format (100 * a_road / a_hw, digits = 3)
    a_no_cars <- format (100 * (a_hw - a_road - a_parking) / a_hw, digits = 3)
    a_carpark_per_res <- format (100 * car_parks_per_res, digits = 2)

    a_public <- a_open + a_hw - a_road - a_parking
    a_public_prop <- format (100 * a_public / a_tot, digits = 3)
    a_public_adj <- a_open + a_hw
    a_public_prop_adj <- format (100 * a_public_adj / a_tot, digits = 3)
    public_incr <- format (100 * (a_public_adj / a_public - 1), digits = 2)

    ul1 <- cli::cli_ul ()
    cli::cli_li ("Total area = {a_tot_f} hectares")
    cli::cli_li ("Proportion buildings: {a_bldg_prop}%")
    cli::cli_li ("Proportion open space: {a_open_prop}%")
    cli::cli_li ("Proportion roads: {a_hw_prop}%, of which:")
    ul2 <- cli::cli_ul ()
    cli::cli_li ("{a_road_prop}% is for car roads")
    cli::cli_li ("{a_parking_prop}% is for parked cars, and")
    cli::cli_li ("{a_no_cars}% remains for everybody else.")
    cli::cli_end (ul2)
    cli::cli_li ("There are street parking spaces for {a_carpark_per_res}% of all residents.")
    cli::cli_li ("So {a_carpark_per_res}% of residents occupy {a_parking_prop}% of all space")
    cli::cli_text ()
    cli::cli_li ("Total proportion of public space: {a_public_prop}")
    cli::cli_li ("Total proportion of public space as superblock: {a_public_prop_adj}")
    cli::cli_li ("Increase in public space as superblock: {public_incr}%")
    cli::cli_end (ul1)
}
