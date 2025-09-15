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

std::vector <size_t> parksearch::randomOrder (size_t ntotal, size_t n) {

    std::vector <double> xrand = Rcpp::as<std::vector<double>> (Rcpp::runif (ntotal));

    std::vector <size_t> indices(ntotal);
    std::iota(indices.begin(), indices.end(), 0);
    std::sort(indices.begin(), indices.end(),
              [&xrand](size_t i, size_t j) { return xrand[i] < xrand[j]; });

    std::vector <size_t> res (indices.begin(), indices.begin() + n);

    return res;
}

std::vector <double> parksearch::fillParkingSpaces (std::vector <int> num_spaces, double prop_full) {

    const size_t n = num_spaces.size();
    std::vector <double> p_empty (n, 0.0);

    size_t ntotal = 0;
    for (auto i: num_spaces) {
        ntotal += i;
    }
    std::vector <size_t> allSpaces;
    allSpaces.reserve(ntotal);

    for (size_t i = 0; i < n; i++) {
        for (auto j = 0; j < num_spaces [i]; j++) {
            allSpaces.push_back(i);
        }
    }

    const size_t nfull = floor(prop_full * ntotal);
    std::vector <size_t> index = parksearch::randomOrder(ntotal, nfull);

    std::vector <size_t> fullSpaces (ntotal, 0L);
    for (auto i: index) {
        fullSpaces[allSpaces[i]]++;
    }

    for (int i = 0; i < n; i++) {
        if (num_spaces [i] > 0) {
            p_empty [i] = 1 - fullSpaces [i] / static_cast<double>(num_spaces [i]);
        }
    }

    return p_empty;
}

double parksearch::oneParkSearch (
    const parksearch::EdgeMapType &edgeMap,
    const parksearch::EdgeMapType &edgeMapRev,
    std::vector <double> &dist,
    std::vector <double> &p_empty,
    const size_t nedges,
    const size_t start_vert
) {

    std::vector <int> nvisits (nedges, 0L);

    double search_dist = 0;
    bool found = false;
    size_t n_iter = 0L;

    size_t i = start_vert - 1L; // Convert 1-based R value to 0-based C++

    while (!found && n_iter < 1000) {

        n_iter++;

        if (p_empty [i] < 1.0e-12) {
            search_dist += dist [i];
        } else {
            // search_dist += d_to_empty [i];
            search_dist += dist [i];
            break;
        }

        nvisits [i]++;

        int next_i = -1;
        int nextVisits = 99999L;
        std::unordered_set <size_t> edgeSet = edgeMap.at (i);
        if (edgeSet.size () == 0) {
            edgeSet = edgeMapRev.at (i);
        }
        for (auto s: edgeSet) {
            if (nvisits [s] < nextVisits) {
                nextVisits = nvisits [s];
                next_i = s;
            }
        }

        if (next_i > -1) {
            i = next_i;
        }
    }

    return search_dist;
}

//' rcpp_park_search
//'
//' @noRd
// [[Rcpp::export]]
double rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::List edge_map_in,
        const Rcpp::List edge_map_rev_in,
        const double prop_full,
        const int start_vert)
{

    Rcpp::RNGScope scope;

    parksearch::EdgeMapType edgeMap, edgeMapRev;
    parksearch::makeEdgeMaps (edge_map_in, edge_map_rev_in, edgeMap, edgeMapRev);

    std::vector <int> num_spaces = graph ["np"];
    std::vector <double> p_empty = parksearch::fillParkingSpaces (num_spaces, prop_full);

    std::vector <double> dist = graph ["d"];
    std::vector <double> d_to_empty = graph ["d_to_empty"];

    size_t nedges = static_cast <size_t> (graph.nrow ());
    std::vector <int> nvisits (nedges, 0L);

    double search_dist = parksearch::oneParkSearch (
        edgeMap, edgeMapRev, dist, p_empty, nedges, start_vert);

    double junk = utils::expected_min_d (100L, 75L);
    Rcpp::Rcout << junk << std::endl;

    return search_dist;
}
