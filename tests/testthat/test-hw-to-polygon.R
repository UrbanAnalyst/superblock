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

    osmdat <- helper_extra_highway (osmdat)

    hw_polys <- hws_to_polygons (osmdat, interval = 0)

    expect_s3_class (hw_polys, "sf")
    expect_equal (nrow (hw_polys), 1L) # only 1 polygon
    expect_named (hw_polys, "geometry")
    expect_true (sf::st_geometry_type (hw_polys) == "POLYGON")
})
