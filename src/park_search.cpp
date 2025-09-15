#include "park_search.h"

//' rcpp_park_search
//'
//' @noRd
// [[Rcpp::export]]
double rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::List edge_map_in,
        const Rcpp::List edge_map_rev_in,
        const int start_vert)
{

    std::unordered_map <size_t, std::unordered_set <size_t> > edgeMap, edgeMapRev;

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

    std::vector <double> dist = graph ["d"];
    std::vector <double> p_empty = graph ["p_empty"];
    std::vector <double> d_to_empty = graph ["d_to_empty"];

    size_t nedges = static_cast <size_t> (graph.nrow ());
    std::vector <int> nvisits (nedges, 0L);

    double search_dist = 0;
    bool found = false;
    size_t n_iter = 0L;

    int i = start_vert - 1L; // Convert 1-based R value to 0-based C++

    while (!found && n_iter < 1000) {

        n_iter++;

        if (p_empty [i] == 0) {
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
