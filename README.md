<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/UrbanAnalyst/superblock/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UrbanAnalyst/superblock/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/UrbanAnalyst/superblock/graph/badge.svg)](https://app.codecov.io/gh/UrbanAnalyst/superblock)

# superblock

An R package to analyse potential
[superblocks](https://doi.org/10.1016%2Fj.cities.2024.105609).

## Example

An example of the “Hansaviertel” of [Münster,
Germany](https://www.openstreetmap.org/#map=17/51.955569/7.639795).
First load the library and extract the necessary data from Open Street
Map (OSM):

``` r
library (superblock)
bbox <- c (7.63099, 51.95048, 7.66402, 51.96142)
hw_names <- c ("Bremer Straße", "Bremer Platz", "Wolbecker Straße", "Hansaring")
outer <- c (T, F, T, T)
osmdat <- sb_osmdata_extract (bbox, hw_names, outer = outer)
```

The `sb_summary()` function then summarises change in publicly
accessible area if the area is converted to a superblock:

``` r
sb_summary (osmdat)
#> • Total area = 22.8 hectares
#> • Proportion buildings: 36.6%
#> • Proportion open space: 7.36%
#> • Proportion roads: 15.7%, of which:
#>   • 32.5% is for car roads
#>   • 39.9% is for parked cars, and
#>   • 27.6% remains for everybody else.
#> • There are street parking spaces for 7.8% of all residents.
#> • So 7.8% of residents occupy 39.9% of all space
#> 
#> • Total proportion of public space: 11.7
#> • Total proportion of public space as superblock: 23.1
#> • Increase in public space as superblock: 97%
```
