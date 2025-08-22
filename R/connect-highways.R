#' connect_highways
#'
#' Takes a list of highways names which must enclose an internal area, and
#' returns a \code{SpatialLines} object containing a sequence of OSM nodes which
#' cyclically connect all highways. Will fail if the streets do not form a
#' cycle.
#'
#' @param highways A vector of highway names passed directly to the Overpass
#' API. Wildcards and whitespaces are `.'; for other options see online help for
#' the overpass API.
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.
#' @return A single set of \code{SpatialPoints} containing the lat-lon
#' coordinates of the cyclic line connecting all given streets.
#'
#' @note \enumerate{
#' \item \code{connect_highways} is primarily intended to provide a means to
#' define boundaries of groups which can then be highlighted using
#' \code{\link{add_osm_groups}}.
#' \item This function can not be guaranteed failsafe owing both to the
#' inherently unpredictable nature of OpenStreetMap, as well as to the unknown
#' relationships between named highways.#' }
#'
#' @seealso \code{\link{add_osm_groups}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' highways <- c (
#'     "Monmouth.St", "Short.?s.Gardens", "Endell.St", "Long.Acre",
#'     "Upper.Saint.Martin"
#' )
#' # Note that dots signify "anything", including whitespace and apostrophes,
#' # and that '?' denotes optional previous character and so here matches
#' # both "Shorts Gardens" and "Short's Gardens"
#' \dontrun{
#' highways1 <- connect_highways (highways = highways, bbox = bbox)
#' highways <- c ("Endell.St", "High.Holborn", "Drury.Lane", "Long.Acre")
#' highways2 <- connect_highways (highways = highways, bbox = bbox)
#' }
#'
#' @export
connect_highways <- function (highways, bbox, outer = TRUE) {

    if (missing (highways)) {
        stop ("A vector of highway names must be given")
    }
    if (missing (bbox)) {
        stop ("A bounding box must be given")
    }
    if (length (outer) == 1L) {
        outer <- rep (outer, length (ways))
    }
    stopifnot (length (outer) == length (highways))

    # Uses extract_highways to generate a list of highways, each component of
    # which is a spatially ordered list of distinct segments. Then uses
    # connect_highways to generate a fundamental cycle connecting all listed
    # highways (through forcing connections between highways if necessary).
    # This latter list simply means that components of each highway connect
    # cyclically with components of other highways, yet each highway itself may
    # still not necessarily internally connect. Thus the last remaining task of
    # this routine is to ensure that the internal components of each highway are
    # actually all connected.

    # Start by getting the list of highway components which have been
    # sequentially joined and ordered into a minimal set
    ways <- extract_highways (highway_names = highways, bbox = bbox)
    i0 <- which (sapply (ways, length) == 0)
    if (any (i0)) {
        for (i in i0) {
            warning (highways [i], " contains no data")
        }
    }
    while (any (sapply (ways, length) == 0)) {

        i0 <- which (sapply (ways, length) == 0)
        ways [[i0 [1]]] <- NULL # nolint
    }

    conmat <- get_conmat (ways)
    cycles <- try (ggm::fundCycles (conmat), TRUE)

    path <- NULL
    if (is.null (cycles) || is (attr (cycles, "condition"), "simpleError")) {
        warning ("There are no cycles in the listed highways")
    } else {

        cyc <- cycles [[which.max (sapply (cycles, nrow))]]
        if (nrow (cyc) < length (ways)) {
            warning ("Cycle unable to be extended through all ways",
                call. = FALSE
            )
        }

        # shortest path through the entire cycle:
        path <- sps_through_cycle (ways, cyc, outer = outer)
    }

    return (path)
}


#' get_conmat
#'
#' Get connection matrix between a list of ways
#'
#' @param ways Either a full list of list of ways to be connected, or a single
#' list of ways.
#'
#' @return Binary connectivity matrix between all ways
#'
#' @noRd
get_conmat <- function (ways) {

    conmat <- array (FALSE, dim = rep (length (ways), 2))

    for (i in seq (ways)) {

        if (is.list (ways [[i]])) {

            wi <- do.call (rbind, ways [[i]])
            ref <- ways
        } else {

            wi <- ways [[i]]
            ref <- ways
        }
        ref [[i]] <- NULL
        if (is.list (ref [[1]])) {
            ref <- lapply (ref, function (i) do.call (rbind, i))
        }

        convec <- vapply (
            ref, function (i) {
                any (rownames (i) %in% rownames (wi))
            },
            logical (1)
        )

        indx <- seq (ways) [!(seq (ways)) %in% i]
        conmat [i, indx] <- conmat [indx, i] <- convec
    }

    return (conmat)
}


#' shortest path through entire cycle of ways
#'
#' @param ways List of ways to be connected
#' @param cyc Cycle extracted from \code{extract_cycle}
#'
#' @return Matrix of cyclically connected coordinates encircling all ways
#'
#' @noRd
sps_through_cycle <- function (ways, cyc, outer = TRUE) {

    cyc <- rbind (cyc, cyc [1, ])
    xy0 <- apply (do.call (rbind, do.call (c, ways)), 2, mean)

    # Make a dodgr graph from input of (lon, lat) with rownames
    make_dodgr <- function (g, rev = FALSE) {
        if (!rev) {
            index_t <- seq_len (nrow (g)) [-1]
            index_f <- index_t - 1L
        } else {
            index_f <- seq_len (nrow (g)) [-1]
            index_t <- index_f - 1L
        }
        data.frame (
            from_id = rownames (g) [index_f],
            from_lon = g [index_f, 1],
            from_lat = g [index_f, 2],
            to_id = rownames (g) [index_t],
            to_lon = g [index_t, 1],
            to_lat = g [index_t, 2],
            d = geodist::geodist (g, sequential = TRUE),
            row.names = NULL
        )
    }

    # First reduce lists of ways doen to only those which connect with ways in
    # other groups, along with paths through intermediate ways:
    for (i in seq_len (nrow (cyc)) [-1]) {

        c0 <- cyc [i - 1, 2] # the current way
        cf <- cyc [i - 1, 1] # the 'from' way
        ct <- cyc [i, 2] # the 'to' way

        nf <- rownames (do.call (rbind, ways [[cf]]))
        nt <- rownames (do.call (rbind, ways [[ct]]))
        w0 <- ways [[c0]]
        w0nodes <- rownames (do.call (rbind, w0))

        nodes_start <- nf [which (nf %in% w0nodes)]
        nodes_end <- nt [which (nt %in% w0nodes)]

        w0_dodgr <- lapply (w0, function (w) {
            rbind (make_dodgr (w, FALSE), make_dodgr (w, TRUE))
        })
        w0_dodgr <- do.call (rbind, w0_dodgr)
        paths <- dodgr::dodgr_paths (w0_dodgr, from = nodes_start, to = nodes_end)

        w0_flat <- do.call (rbind, w0)
        paths <- do.call (c, paths)
        index <- which (vapply (paths, length, integer (1L)) > 0L)
        ways [[c0]] <- unname (lapply (paths [index], function (p) {
            w0_flat [match (p, rownames (w0_flat)), ]
        }))
        # Rm any duplicated ways:
        nodes <- lapply (ways [[c0]], function (w) sort (rownames (w)))
        index <- which (duplicated (nodes))
        if (length (index) > 0L) {
            ways [[c0]] <- ways [[c0]] [-index]
        }
        if (length (ways [[c0]]) > 1L) {
            # Then remove strict sub-sets:
            combs <- utils::combn (length (ways [[c0]]), m = 2L)
            combs <- cbind (combs, combs [2:1, ])
            subsets <- apply (combs, 2, function (m) {
                n1 <- rownames (ways [[c0]] [[m [1]]])
                n2 <- rownames (ways [[c0]] [[m [2]]])
                all (n2 %in% n1)
            }, simplify = TRUE)
            if (any (subsets)) {
                subsets <- sort (unique (combs [2, which (subsets)]))
                ways [[c0]] <- ways [[c0]] [-subsets]
            }
        }

        dmax <- lapply (
            ways [[c0]],
            function (w) max (geodist::geodist (xy0, w))
        )
        index <- ifelse (outer [c0], which.max (dmax),  which.min (dmax))
        ways [[c0]] <- ways [[c0]] [index]
    }

    ways <- do.call (c, ways)
    ways <- ways [rev (seq_along (ways))]

    thepath <- do.call (rbind, ways)
    thepath <- unique (thepath)
    thepath <- rbind (thepath, utils::head (thepath, 1L))

    return (thepath)
}
