#include "utils.h"
#include "park_search.h"

void parksearch::makeEdgeMaps (
    const Rcpp::List edge_map_in,
    const Rcpp::List edge_map_rev_in,
    EdgeMapType &edgeMap,
    EdgeMapType &edgeMapRev) {

    for (int i = 0; i < edge_map_in.size (); i++) {
        std::vector <size_t> edge_vec = Rcpp::as <std::vector <size_t>> (edge_map_in [i]);
        std::unordered_set <size_t> edgeSet;
        for (auto s: edge_vec) {
            edgeSet.emplace (s);
        }
        edgeMap.emplace (i, edgeSet);
    }

    for (int i = 0; i < edge_map_rev_in.size (); i++) {
        std::vector <size_t> edge_vec = Rcpp::as <std::vector <size_t>> (edge_map_rev_in [i]);
        std::unordered_set <size_t> edgeSet;
        for (auto s: edge_vec) {
            edgeSet.emplace (s);
        }
        edgeMapRev.emplace (i, edgeSet);
    }
}

std::vector <size_t> parksearch::randomOrder (const int ntotal, const size_t n) {

    std::vector <double> xrand = Rcpp::as<std::vector<double>> (Rcpp::runif (ntotal));

    std::vector <size_t> indices(static_cast<size_t>(ntotal));
    std::iota(indices.begin(), indices.end(), 0);
    std::sort(indices.begin(), indices.end(),
              [&xrand](size_t i, size_t j) { return xrand[i] < xrand[j]; });

    std::vector <size_t> res (indices.begin(), indices.begin() + static_cast<long>(n));

    return res;
}

std::vector <double> parksearch::fillParkingSpaces (
    const std::vector <int> &num_spaces,
    const double prop_full) {

    const size_t n = num_spaces.size();
    std::vector <double> p_empty (n, 0.0);

    int ntotal = 0;
    for (auto i: num_spaces) {
        ntotal += i;
    }
    const size_t ntotal_t = static_cast<size_t>(ntotal);
    std::vector <size_t> allSpaces;
    allSpaces.reserve(ntotal_t);

    for (size_t i = 0; i < n; i++) {
        for (auto j = 0; j < num_spaces [i]; j++) {
            allSpaces.push_back(i);
        }
    }

    const size_t nfull = static_cast<size_t>(floor(prop_full * ntotal));
    std::vector <size_t> index = parksearch::randomOrder(ntotal, nfull);

    std::vector <size_t> fullSpaces (ntotal_t, 0L);
    for (auto i: index) {
        fullSpaces[allSpaces[i]]++;
    }

    for (size_t i = 0; i < n; i++) {
        if (num_spaces [i] > 0) {
            double ns_i = static_cast<double>(num_spaces[i]);
            double fs_i = static_cast<double>(fullSpaces[i]);
            p_empty [i] = 1.0 - fs_i / ns_i;
        }
    }

    return p_empty;
}

std::vector<double> parksearch::oneParkSearch (
    const parksearch::EdgeMapType &edgeMap,
    const parksearch::EdgeMapType &edgeMapRev,
    const std::vector <double> &dist,
    const std::vector <double> &d_to_empty,
    const std::vector <double> &p_empty,
    const size_t nedges,
    const size_t start_edge
) {

    std::vector <int> nvisits (nedges, 0L);

    double search_dist = 0;
    bool found = false;
    int n_iter = 0L;

    size_t i = start_edge - 1L; // Convert 1-based R value to 0-based C++
    // Record 2nd edge for accurate calculation of distances when park is in
    // first edge.
    int second_edge = -1;

    while (!found && n_iter < 1000L) {

        n_iter++;

        if (p_empty [i] < 1.0e-12) {
            search_dist += dist [i];
        } else {
            search_dist += d_to_empty [i];
            break;
        }

        nvisits [i]++;

        int next_i = -1;
        int nextVisits = 99999L;
        std::unordered_set <size_t> edgeSet = edgeMap.at (i);
        if (edgeSet.size () == 0) {
            edgeSet = edgeMapRev.at (i);
            search_dist += dist [i];
        }
        for (auto s: edgeSet) {
            if (nvisits [s] < nextVisits) {
                nextVisits = nvisits [s];
                next_i = static_cast<int>(s);
            }
        }

        if (next_i > -1) {
            i = static_cast<size_t>(next_i);
        }
        if (second_edge < 0) {
            second_edge = static_cast<int>(i);
        }
    }

    if (n_iter >= 1000) {
        search_dist = -1;
    }


    std::vector<double> res = { static_cast<double>(i), static_cast<double>(second_edge), search_dist };

    return res;
}

void parksearch::fill_d_to_empty (
    const std::vector <int> &num_spaces,
    const std::vector <double> &dist,
    std::vector <double> &d_to_empty,
    const double prop_full) {

    const size_t nedges = d_to_empty.size();
    std::fill (d_to_empty.begin(), d_to_empty.end(), 0.0);

    for (size_t i = 0; i < nedges; i++) {
        if (num_spaces [i] > 0) {
            const size_t n_exp = static_cast<size_t>(floor(num_spaces[i] * prop_full));
            double dprop = utils::expected_min_d (static_cast<size_t>(num_spaces [i]), n_exp);
            d_to_empty[i] = dist[i] * dprop / num_spaces[i];
        }
    }
}

//' rcpp_park_search
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::DataFrame rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::List edge_map_in,
        const Rcpp::List edge_map_rev_in,
        const double prop_full,
        const int start_edge,
        const int ntrials)
{

    Rcpp::RNGScope scope;

    parksearch::EdgeMapType edgeMap, edgeMapRev;
    parksearch::makeEdgeMaps (edge_map_in, edge_map_rev_in, edgeMap, edgeMapRev);

    std::vector <double> dist = graph ["d"];
    std::vector <int> num_spaces = graph ["np"];
    size_t nedges = static_cast <size_t> (graph.nrow ());

    Rcpp::IntegerVector edge (ntrials), edge2 (ntrials);
    Rcpp::NumericVector d (ntrials);

    for (size_t n = 0; n < static_cast<size_t>(ntrials); n++) {

        std::vector <double> p_empty = parksearch::fillParkingSpaces (num_spaces, prop_full);

        std::vector <double> d_to_empty(nedges, 0.0);
        parksearch::fill_d_to_empty(num_spaces, dist, d_to_empty, prop_full);

        std::vector<double> res = parksearch::oneParkSearch (
            edgeMap, edgeMapRev, dist, d_to_empty, p_empty, nedges,
            static_cast<size_t>(start_edge)
        );

        edge(n) = static_cast<int>(round(res[0])) + 1L; // Conver back to 1-based R indexing
        edge2(n) = static_cast<int>(round(res[1]));
        if (edge2(n) > 0) { edge2(n)++; }
        d(n) = res[2];
    }

    Rcpp::DataFrame res = Rcpp::DataFrame::create (
        Rcpp::Named ("edge") = edge,
        Rcpp::Named ("next_edge") = edge2,
        Rcpp::Named ("d") = d,
        Rcpp::_["stringsAsFactors"] = false
    );

    return res;
}

//' rcpp_park_fill
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_park_fill (const Rcpp::DataFrame graph,
        const Rcpp::List edge_map_in,
        const Rcpp::List edge_map_rev_in,
        const double prop_full,
        const int ntrials)
{

    Rcpp::RNGScope scope;

    parksearch::EdgeMapType edgeMap, edgeMapRev;
    parksearch::makeEdgeMaps (edge_map_in, edge_map_rev_in, edgeMap, edgeMapRev);

    std::vector <double> dist = graph ["d"];
    std::vector <int> num_spaces = graph ["np"];
    size_t nedges = static_cast <size_t> (graph.nrow ());

    int ntotal = 0;
    for (auto n: num_spaces) {
        ntotal += n;
    }
    int n_full = floor (ntotal * prop_full);

    Rcpp::NumericVector d (ntrials, 0.0);

    for (size_t n = 0; n < static_cast<size_t>(ntrials); n++) {

        double prop_i = prop_full;

        // Incrementally fill spaces and add all distances:
        for (int i = n_full; i < ntotal; i++) {

            std::vector <double> p_empty = parksearch::fillParkingSpaces (num_spaces, prop_i);

            std::vector <double> d_to_empty(nedges, 0.0);
            parksearch::fill_d_to_empty(num_spaces, dist, d_to_empty, prop_i);

            // oneParkSearch assumes start_edge is 1-based R index:
            int start_edge = Rcpp::as<int>(Rcpp::runif(1, 0, nedges)) + 1L;
            bool found = false;
            int nattempts = 0;
            std::vector<double> res;
            while (!found && nattempts < 100) {
                res = parksearch::oneParkSearch (
                    edgeMap, edgeMapRev, dist, d_to_empty, p_empty, nedges,
                    static_cast<size_t>(start_edge)
                );
                found = res[2] > 0;
                nattempts++;
            }

            if (res[2] > 0) {
                d[n] += res[2];
            }

            prop_i = static_cast<double>(i) / static_cast<double>(ntotal);
        }

    }

    return d;
}
