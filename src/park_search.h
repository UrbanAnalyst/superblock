#pragma once

#include <vector>
#include <unordered_map>
#include <unordered_set>

#include <Rcpp.h>

class DGraph;

double rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::List edge_map_in,
        const Rcpp::List edge_map_rev_in,
        const int start_vert);
