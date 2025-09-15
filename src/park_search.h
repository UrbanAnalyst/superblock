#pragma once

#include <vector>
#include <algorithm>
#include <numeric>
#include <unordered_map>
#include <unordered_set>

#include <Rcpp.h>

namespace parksearch {

typedef std::unordered_map <size_t, std::unordered_set <size_t> > EdgeMapType;

void makeEdgeMaps (
    const Rcpp::List edge_map_in,
    const Rcpp::List edge_map_rev_in,
    EdgeMapType &edgeMap,
    EdgeMapType &edgeMapRev);

std::vector <size_t> randomOrder (size_t ntotal, size_t n);

std::vector <double> fillParkingSpaces (std::vector <int> num_spaces, double prop_full);

void fill_d_to_empty (
    const std::vector <int> &num_spaces,
    const std::vector <double> &dist,
    std::vector <double> &d_to_empty,
    const double prop_full);

double oneParkSearch (
    const parksearch::EdgeMapType &edgeMap,
    const parksearch::EdgeMapType &edgeMapRev,
    std::vector <double> &dist,
    std::vector <double> &p_empty,
    const size_t nedges,
    const size_t start_vert
);

} // end namespace parksearch

double rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::List edge_map_in,
        const Rcpp::List edge_map_rev_in,
        const double prop_full,
        const int start_vert);
