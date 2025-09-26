#' Estimate floor and roof area per resident.
#'
#' Uses example building with known numbers of residents, and which is typical
#' of the neighbourhood.
#' @noRd
area_per_resident <- function (osmdat) {

    street <- "Papenburger Straße"
    housenumber <- 2
    num_residents_floors <- 21
    num_residents_roof <- 2.5

    b <- osmdat$buildings

    i <- which (
        b$`addr:street` == street & b$`addr:housenumber` == housenumber
    )
    area <- sf::st_area (osmdat$buildings [i, ]) # in m^2

    num_levels <- as.numeric (b$`building:levels` [i])
    num_roof_levels <- as.numeric (b$`roof:levels` [i])
    num_roof_levels <- ifelse (is.na (num_roof_levels), 0, num_roof_levels)
    a_per_res_floors <- area * num_levels / num_residents_floors
    a_per_res_floors <- 10 * floor (a_per_res_floors / 10)
    # This is artifically inflated, because roof areas are always smaller:
    a_per_res_roof <- area * num_roof_levels / num_residents_roof
    a_per_res_roof <- 10 * floor (a_per_res_roof / 10)

    c (floor = a_per_res_floors, roof = a_per_res_roof)
}

building_areas <- function (osmdat) {

    exclude <- c ("carport", "garage", "garages", "school", "shed", "yes")
    b <- osmdat$buildings |>
        dplyr::filter (!building %in% exclude) |>
        dplyr::filter (!is.na (`addr:street`) & !is.na (`addr:housenumber`))

    exclude_ground_floor <- c ("civic", "office", "retail", "supermarket")
    num_levels <- as.numeric (b$`building:levels`)
    index <- which (b$building %in% exclude_ground_floor & num_levels == 1)
    if (length (index > 0)) {
        b <- b [-index, ]
        num_levels <- as.numeric (b$`building:levels`)
        index <- which (b$building %in% exclude_ground_floor)
    }
    num_levels [index] <- num_levels [index] - 1
    num_roof_levels <- as.numeric (b$`roof:levels`)

    # Replace missing values with averages:
    num_levels [which (is.na (num_levels))] <- mean (num_levels, na.rm = TRUE)
    num_roof_levels [which (is.na (num_roof_levels))] <- mean (num_roof_levels, na.rm = TRUE)
    areas <- sf::st_area (b)

    area_levels <- areas * num_levels
    area_roofs <- areas * num_roof_levels

    c (floor = sum (area_levels), roof = sum (area_roofs))
}
