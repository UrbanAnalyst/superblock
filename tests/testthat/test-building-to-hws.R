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
    expect_type (b$n_parking_spaces, "double")

    dat <- withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        parking_time_matrix (osmdat)
    )

    expect_type (dat, "list")
    expect_named (dat, c ("buildings", "tmat_car", "tmat_foot"))
    identical_cols <- c ("osm_id", "hw_id", "lon", "lat")
    expect_identical (b [, identical_cols], dat$buildings [, identical_cols])
    expect_true (all (b$n_parking_spaces >= 0))
    # test data insert fake parking spaces in 'parking_time_matrix()':
    expect_true (all (dat$buildings$n_parking_spaces > 0))

    for (what in c ("tmat_car", "tmat_foot")) {
        expect_type (dat [[what]], "double")
        expect_length (attr (dat [[what]], "dim"), 2L) # matrix
        expect_length (unique (attr (dat [[what]], "dim")), 1L) # square
    }
})
