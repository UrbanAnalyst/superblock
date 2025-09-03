test_that ("osm extraction", {

    bbox <- c (7.6413, 51.9553, 7.6454, 51.9567)
    hw_names <- c (
        "Dortmunder Straße",
        "Wolbecker Straße",
        "Emdener Straße",
        "Schillerstraße"
    )

    # osmdata calls are memoised, so store as mock results here
    withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        bounding_poly <- httptest2::with_mock_dir ("osm-poly", {
            extract_bounding_polygon (bbox, hw_names)
        })
    )
    # highways <- httptest2::with_mock_dir ("osm-hw", {
    #     extract_osm_highways (bbox, bounding_poly)
    # })
    # buildings <- httptest2::with_mock_dir ("osm-bldg", {
    #     extract_osm_buildings (bbox, bounding_poly)
    # })
    # open_spaces <- httptest2::with_mock_dir ("osm-open", {
    #     extract_osm_open_spaces (bbox, bounding_poly)
    # })

    # osmdat <- sb_osmdata_extract (bbox, hw_names)
})
