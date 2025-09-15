
#include "park_search.h"

#include "dgraph.h"

// # nocov start
template <typename T>
void inst_graph (std::shared_ptr<DGraph> g, size_t nedges,
        const std::map <std::string, size_t>& vert_map,
        const std::vector <std::string>& from,
        const std::vector <std::string>& to,
        const std::vector <T>& dist,
        const std::vector <T>& wt)
{
    for (size_t i = 0; i < nedges; ++i)
    {
        size_t fromi = vert_map.at(from [i]);
        size_t toi = vert_map.at(to [i]);
        g->addNewEdge (fromi, toi, dist [i], wt [i], i);
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

// Flows from the pathfinder output are reallocated based on matching vertex
// pairs to edge indices. Note, however, that contracted graphs frequently
// have duplicate vertex pairs with different distances. The following
// therefore uses two maps, one to hold the ultimate index from vertex
// pairs, and the other to hold minimal distances. This is used in flow routines
// only.
void parksearch::make_vert_to_edge_maps (const std::vector <std::string> &from,
        const std::vector <std::string> &to, const std::vector <double> &wt,
        std::unordered_map <std::string, size_t> &verts_to_edge_map,
        std::unordered_map <std::string, double> &verts_to_dist_map)
{
    for (size_t i = 0; i < from.size (); i++)
    {
        std::string two_verts = "f" + from [i] + "t" + to [i];
        verts_to_edge_map.emplace (two_verts, i);
        if (verts_to_dist_map.find (two_verts) == verts_to_dist_map.end ())
            verts_to_dist_map.emplace (two_verts, wt [i]);
        else if (wt [i] < verts_to_dist_map.at (two_verts))
        {
            verts_to_dist_map [two_verts] = wt [i];
            verts_to_edge_map [two_verts] = i;
        }
    }
}

//' rcpp_get_sp_dists
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_park_search (const Rcpp::DataFrame graph,
        const Rcpp::DataFrame vert_map_in,
        Rcpp::IntegerVector fromi,
        Rcpp::IntegerVector toi_in,
        const std::string& heap_type)
{
    std::vector <size_t> toi =
        Rcpp::as <std::vector <size_t> > ( toi_in);
    size_t nfrom = static_cast <size_t> (fromi.size ());
    size_t nto = static_cast <size_t> (toi.size ());

    std::vector <std::string> from = graph ["from"];
    std::vector <std::string> to = graph ["to"];
    std::vector <double> dist = graph ["d"];
    std::vector <double> wt = graph ["d_weighted"];

    size_t nedges = static_cast <size_t> (graph.nrow ());
    std::map <std::string, size_t> vert_map;
    std::vector <std::string> vert_map_id = vert_map_in ["vert"];
    std::vector <size_t> vert_map_n = vert_map_in ["id"];
    size_t nverts = parksearch::make_vert_map (vert_map_in, vert_map_id,
            vert_map_n, vert_map);

    std::shared_ptr<DGraph> g = std::make_shared<DGraph>(nverts);
    inst_graph (g, nedges, vert_map, from, to, dist, wt);

    std::vector<double> w (nverts);
    std::vector<double> d (nverts);
    std::vector<long int> prev (nverts);

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
