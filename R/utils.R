is_test_env <- function () {
    identical (Sys.getenv ("SUPERBLOCK_TESTS", "nope"), "true")
}
