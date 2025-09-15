
#include "utils.h"
#include <Rcpp.h>

double utils::choose(size_t n, size_t k) {
    if (k < 0 || k > n) {
        return 0.0;
    }
    if (k == 0 || k == n) {
        return 1.0;
    }

    if (k > n - k) {
        k = n - k;
    }

    double result = 1.0;
    for (size_t i = 1; i <= k; i++) {
        result = result * (n - k + i) / static_cast<double>(i);
    }

    return result;
}

// Sample 'n' values from a total of 'd', and return the expected minimum of the
// unsampled values.
double utils::expected_min_d(size_t d, size_t n) {

    double comb_total = utils::choose(d, n);
    double val = 0.0;

    size_t n_to_i = 1;
    size_t n_lt_i = 0;

    for (size_t i = 1; i <= d; i++) {

        double prob_all_n_lt_sampled = 0.0;
        if (n_lt_i <= n) {
            prob_all_n_lt_sampled = utils::choose (d - n_lt_i, n - n_lt_i) / comb_total;
        }

        double prob_all_n_sampled = 0.0;
        if (n_to_i <= n) {
            prob_all_n_sampled = utils::choose (d - n_to_i, n - n_to_i) / comb_total;
        }

        n_to_i += (i + 1);
            n_lt_i += i;

        double p = prob_all_n_lt_sampled - prob_all_n_sampled;

        val = val + i * p;
    }

    return val;
}
