test_that ("hw-to-polygon", {

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

    b <- buildings_to_highways (osmdat)

    expect_s3_class (b, "data.frame")
    expect_named (b, c ("osm_id", "hw_id", "lon", "lat", "n_parking_spaces"))
    expect_true (all (b$osm_id %in% osmdat$buildings$osm_id))
    expect_true (all (b$hw_id %in% osmdat$highways$osm_id))
    expect_type (b$n_parking_spaces, "integer")
})
