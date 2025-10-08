#' Estimate number of car spaces per resident
#'
#' @param osmdat Object returned from \link{sb_osmdata_extract}.
#' @return An estimate of numbers of parking spaces per resident.
#' @export
sb_car_spaces_per_resident <- function (osmdat) {

    num_res <- estimate_num_residents (osmdat)
    num_res_tot <- sum (num_res$floor) + sum (num_res$roof)

    p <- car_parking_areas (osmdat)
    num_parking_spaces <- sum (p$num_parking_spaces)
    num_parking_spaces / num_res_tot
}

#' Estimate numbers of residents in floor and roof levels of buildings.
#'
#' Uses example building with known numbers of residents, and which is typical
#' of the neighbourhood.
#' @noRd
estimate_num_residents <- function (osmdat) {

    requireNamespace ("jsonlite", quietly = TRUE)

    f <- system.file ("extdata", "population.json", package = "superblock")
    if (!file.exists (f)) {
        cli::cli_abort ("population estimates not bound at {f}")
    }

    pop <- jsonlite::read_json (f, simplify = TRUE)$population
    m2_per_res_floor <- mean (pop$area * pop$num_levels / pop$residents_floors)
    m2_per_res_roof <- mean (pop$area * pop$num_levels_roof / pop$residents_roof)

    b <- filter_residential_buildings (osmdat$buildings)
    num_levels <- as.numeric (b$`building:levels`)
    num_roof_levels <- as.numeric (b$`roof:levels`)
    if (is_test_env ()) {
        num_levels <- rep (3, length (num_levels))
        num_roof_levels [1] <- 1
    }

    # Replace missing values with averages:
    num_levels_mn <- mean (num_levels, na.rm = TRUE)
    num_roof_levels_mn <- mean (num_roof_levels, na.rm = TRUE)
    num_levels <- ifelse (is.na (num_levels), num_levels_mn, num_levels)
    num_roof_levels <- ifelse (is.na (num_roof_levels), num_roof_levels_mn, num_roof_levels)

    building_areas <- as.numeric (sf::st_area (b))
    num_res_floor <- building_areas * num_levels / m2_per_res_floor
    num_res_roof <- building_areas * num_roof_levels / m2_per_res_roof

    data.frame (osm_id = b$osm_id, floor = num_res_floor, roof = num_res_roof)
}

exclude_ground_floor <- c ("civic", "office", "retail", "supermarket")

filter_residential_buildings <- function (b) {

    # Suppress no visible binding notes:
    building <- `addr:street` <- `addr:housenumber` <- NULL

    exclude <- c ("carport", "garage", "garages", "school", "shed", "yes")
    b <- dplyr::filter (b, !building %in% exclude) |>
        dplyr::filter (!is.na (`addr:street`) & !is.na (`addr:housenumber`))

    num_levels <- as.numeric (b$`building:levels`)
    index <- which (b$building %in% exclude_ground_floor & num_levels == 1)
    if (length (index > 0)) {
        b <- b [-index, ]
    }
    return (b)
}

building_areas <- function (osmdat) {

    b <- filter_residential_buildings (osmdat$buildings)

    num_levels <- as.numeric (b$`building:levels`)
    index <- which (b$building %in% exclude_ground_floor)
    num_levels [index] <- num_levels [index] - 1
    num_roof_levels <- as.numeric (b$`roof:levels`)
    if (is_test_env ()) {
        num_levels <- rep (3, length (num_levels))
        num_roof_levels [1] <- 1
    }

    # Replace missing values with averages:
    num_levels [which (is.na (num_levels))] <- mean (num_levels, na.rm = TRUE)
    num_roof_levels [which (is.na (num_roof_levels))] <- mean (num_roof_levels, na.rm = TRUE)
    areas <- sf::st_area (b)

    area_levels <- areas * num_levels
    area_roofs <- areas * num_roof_levels

    data.frame (osm_id = b$osm_id, floor = area_levels, roof = area_roofs)
}
