#pragma once

#include <memory>
#include <vector>
#include <algorithm> // std::fill, std::reverse
#include <iostream>
#include <fstream>

#include <Rcpp.h>

#include "dgraph.h"

class DGraph;

namespace parksearch {

size_t make_vert_map (const Rcpp::DataFrame &vert_map_in,
        const std::vector <std::string> &vert_map_id,
        const std::vector <size_t> &vert_map_n,
        std::map <std::string, size_t> &vert_map);
} // end namespace parksearch

Rcpp::NumericMatrix rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in);

int rcpp_test ();
