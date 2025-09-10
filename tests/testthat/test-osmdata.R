test_that ("osm extraction", {

    bbox <- c (7.6413, 51.9553, 7.6454, 51.9567)
    hw_names <- c (
        "Dortmunder Straße",
        "Wolbecker Straße",
        "Emdener Straße",
        "Schillerstraße"
    )

    memoise_osmdata_calls (bbox, hw_names)

    osmdat <- withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        sb_osmdata_extract (bbox, hw_names)
    )

    expect_type (osmdat, "list")
    nms <- c (
        "bbox", "hw_names", "bounding_poly", "highways",
        "buildings", "open_spaces", "parking_areas",
        "parking_facilities", "dat_sc"
    )
    expect_named (osmdat, nms)

    expect_type (osmdat$bbox, "double")
    expect_length (osmdat$bbox, 4L)

    expect_identical (hw_names, osmdat$hw_names)

    expect_s3_class (osmdat$bounding_poly, "sf")
    expect_named (osmdat$bounding_poly, "geometry")
    expect_equal (nrow (osmdat$bounding_poly), 1L)
    # not expect equal because geometry type is a factor:
    expect_true (sf::st_geometry_type (osmdat$bounding_poly) == "POLYGON")

    expect_s3_class (osmdat$highways, "sf")
    expect_gt (nrow (osmdat$highways), 2L)
    expect_gt (ncol (osmdat$highways), 5L)
    expect_true (
        all (c ("osm_id", "name", "highway", "geometry") %in%
            names (osmdat$highways))
    )
    geom_types <- sf::st_geometry_type (osmdat$highways)
    expect_true (all (geom_types == "LINESTRING"))

    expect_s3_class (osmdat$buildings, "sf")
    expect_gt (nrow (osmdat$buildings), 2L)
    expect_gt (ncol (osmdat$buildings), 5L)
    expect_true (
        all (c ("osm_id", "addr:street", "geometry") %in%
            names (osmdat$buildings))
    )
    geom_types <- sf::st_geometry_type (osmdat$buildings)
    expect_true (all (geom_types == "POLYGON"))

    expect_s3_class (osmdat$open_spaces, "sf")
    expect_equal (nrow (osmdat$open_spaces), 0L) # There are none
    expect_true (
        all (c ("osm_id", "name", "geometry") %in%
            names (osmdat$open_spaces))
    )

    expect_s3_class (osmdat$parking_areas, "sf")
    expect_equal (nrow (osmdat$parking_areas), 0L) # There are none
    expect_true (
        all (c ("osm_id", "name", "geometry") %in%
            names (osmdat$parking_areas))
    )

    expect_s3_class (osmdat$parking_facilities, "sf")
    expect_equal (nrow (osmdat$parking_facilities), 0L) # There are none
    expect_true (
        all (c ("osm_id", "geometry") %in%
            names (osmdat$parking_facilities))
    )
})
