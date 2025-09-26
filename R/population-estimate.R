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
    area <- as.numeric (sf::st_area (osmdat$buildings [i, ])) # in m^2

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
