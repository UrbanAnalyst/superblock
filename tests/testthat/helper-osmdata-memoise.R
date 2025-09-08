memoise_osmdata_calls <- function (bbox, hw_names) {

    bounding_poly <- httptest2::with_mock_dir ("osm-poly", {
        extract_bounding_polygon (bbox, hw_names)
    })

    highways <- withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        httptest2::with_mock_dir ("osm-hw", {
            extract_osm_highways (bbox, bounding_poly)
        })
    )
    buildings <- withr::with_envvar (
        list ("SUPERBLOCK_TESTS" = "true"),
        httptest2::with_mock_dir ("osm-bldg", {
            extract_osm_buildings (bbox, bounding_poly)
        })
    )
    open_spaces <- httptest2::with_mock_dir ("osm-open", {
        extract_osm_open_spaces (bbox, bounding_poly)
    })

    parking <- httptest2::with_mock_dir ("osm-parking", {
        extract_osm_parking_areas (bbox, bounding_poly)
    })
}
