test_that ("parking times", {

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

    times <- withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        sb_parking_times (osmdat, ntrials = 10L)
    )

    expect_s3_class (times, "data.frame")
    expect_s3_class (times, "sb_parking")
    expect_named (
        times,
        c (
            "prop", "d0_mn", "d0_50", "d0_75", "d0_90", "dwalk_mn", "dwalk_50",
            "dwalk_75", "dwalk_90", "time_to_parking", "time_from_parking"
        )
    )

    prop_min <- 0.7 # default value
    n_props <- 10L
    prop <- get_prop_sequence (prop_min = prop_min, n_props = n_props)
    expect_equal (nrow (times), length (prop))
    expect_identical (prop, times$prop)
    expect_length (unique (times$time_to_parking), 1L)
    expect_length (unique (times$time_from_parking), 1L)

    # expect all other values to increase:
    nms <- names (times) [which (!grepl ("^(prop|time)", names (times)))]
    # dwalk_mean actually decreases
    nms <- setdiff (nms, "dwalk_mn")
    for (n in nms) {
        mod <- lm (times [[n]] ~ times$prop)
        slope <- summary (mod)$coefficients [2]
        expect_true (slope > 0)
    }
})
