test_that ("summary function", {

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

    hws <- car_parking_areas (osmdat)
    expect_s3_class (hws, "sf")
    expect_equal (nrow (hws), 1L) # only single extra highway added above
    expect_true (sf::st_geometry_type (hws) == "LINESTRING")
    expect_true ("parking_area" %in% names (hws))

    expect_snapshot (sb_summary (osmdat))
})
