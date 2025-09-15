#include "dgraph.h"
#include "park_search.h"

// # nocov start
template <typename T>
void inst_graph (std::shared_ptr<DGraph> g, size_t nedges,
        const std::map <std::string, size_t>& vert_map,
        const std::vector <std::string>& from,
        const std::vector <std::string>& to,
        const std::vector <T>& dist)
{
    for (size_t i = 0; i < nedges; ++i)
    {
        size_t fromi = vert_map.at(from [i]);
        size_t toi = vert_map.at(to [i]);
        g->addNewEdge (fromi, toi, dist [i], i);
    }
}
// # nocov end

size_t parksearch::make_vert_map (const Rcpp::DataFrame &vert_map_in,
        const std::vector <std::string> &vert_map_id,
        const std::vector <size_t> &vert_map_n,
        std::map <std::string, size_t> &vert_map)
{
    for (size_t i = 0;
            i < static_cast <size_t> (vert_map_in.nrow ()); ++i)
    {
        vert_map.emplace (vert_map_id [i], vert_map_n [i]);
    }
    size_t nverts = static_cast <size_t> (vert_map.size ());
    return (nverts);
}

//' rcpp_park_search
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in)
{
    std::vector <std::string> from = graph [".vx0"];
    std::vector <std::string> to = graph [".vx1"];
    std::vector <double> dist = graph ["d"];
    std::vector <double> wt = dist;

    size_t nedges = static_cast <size_t> (graph.nrow ());
    std::map <std::string, size_t> vert_map;
    const size_t nfrom = static_cast <size_t> (vert_map_in.nrow());
    const size_t nto = nfrom;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <size_t> vert_map_n = vert_map_in ["id"];
    size_t nverts = parksearch::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::shared_ptr<DGraph> g = std::make_shared<DGraph>(nverts);
    inst_graph (g, nedges, vert_map, from, to, dist);

    std::vector<double> d (nverts);

    // initialise dout matrix to NA
    Rcpp::NumericVector na_vec = Rcpp::NumericVector (nfrom * nto,
            Rcpp::NumericVector::get_na ());
    Rcpp::NumericMatrix dout (static_cast <int> (nfrom),
            static_cast <int> (nto), na_vec.begin ());

    for (size_t i = 0; i < nfrom; i++)
    {
        Rcpp::checkUserInterrupt ();
    }
    return (dout);
}
