#' Estimate number of car spaces per resident
#'
#' @param osmdat Object returned from \link{sb_osmdata_extract}.
#' @return An estimate of numbers of parking spaces per resident.
#' @export
sb_car_spaces_per_resident <- function (osmdat) {

    a_per_res <- area_per_resident (osmdat)
    a_per_res_floor <- 10 * floor (mean (a_per_res$floor) / 10)
    a_per_res_roof <- 10 * floor (mean (a_per_res$roof) / 10)

    a_building <- building_areas (osmdat)
    area_floor <- sum (a_building$floor)
    area_roof <- sum (a_building$roof)

    num_res_floor <- as.numeric (area_floor) / a_per_res_floor
    num_res_roof <- as.numeric (area_roof) / a_per_res_roof
    num_res <- num_res_floor + num_res_roof

    p <- car_parking_areas (osmdat)
    num_parking_spaces <- sum (p$num_parking_spaces)
    num_parking_spaces / num_res
}

#' Estimate floor and roof area per resident.
#'
#' Uses example building with known numbers of residents, and which is typical
#' of the neighbourhood.
#' @noRd
area_per_resident <- function (osmdat) {

    num_residents_floors <- 21
    num_residents_roof <- 2.5
    area <- 181 # m2

    b <- filter_residential_buildings (osmdat$buildings)
    num_levels <- as.numeric (b$`building:levels`)
    num_roof_levels <- as.numeric (b$`roof:levels`)

    # Replace missing values with averages:
    num_levels_mn <- mean (num_levels, na.rm = TRUE)
    num_roof_levels_mn <- mean (num_roof_levels, na.rm = TRUE)
    num_levels <- ifelse (is.na (num_levels), num_levels_mn, num_levels)
    num_roof_levels <- ifelse (is.na (num_roof_levels), num_roof_levels_mn, num_roof_levels)

    a_per_res_floors <- area * num_levels / num_residents_floors
    # This is artifically inflated, because roof areas are always smaller:
    a_per_res_roof <- area * num_roof_levels / num_residents_roof

    data.frame (osm_id = b$osm_id, floor = a_per_res_floors, roof = a_per_res_roof)
}

exclude_ground_floor <- c ("civic", "office", "retail", "supermarket")

filter_residential_buildings <- function (b) {

    `addr:street` <- `addr:housenumber` <- NULL

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

    # Replace missing values with averages:
    num_levels [which (is.na (num_levels))] <- mean (num_levels, na.rm = TRUE)
    num_roof_levels [which (is.na (num_roof_levels))] <- mean (num_roof_levels, na.rm = TRUE)
    areas <- sf::st_area (b)

    area_levels <- areas * num_levels
    area_roofs <- areas * num_roof_levels

    data.frame (osm_id = b$osm_id, floor = area_levels, roof = area_roofs)
}
