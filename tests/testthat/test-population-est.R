test_that ("car parks per population", {

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

    spaces_per_res <- withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        sb_car_spaces_per_resident (osmdat)
    )
    expect_type (spaces_per_res, "double")
    expect_length (spaces_per_res, 1L)
    expect_true (spaces_per_res > 0)
    expect_true (spaces_per_res < 1)
})
