<!-- README.md is generated from README.Rmd. Please edit that file -->

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

Not all ways in OSM specify whether and how cars are parked. Ones which don't
can be specified by OSM id values, ensuring that total parking areas are
accurately calculated.

``` r
add_parking_osm_ids <- c (
    "10876650", "10879386", "24420125", "43026134", "43026139", "50163329",
    "148585465", "283680476", "721322833", "721738382", "756777549", "1122509887"
)
```

The `sb_summary()` function summarises change in publicly accessible area if
the area is converted to a superblock:

``` r
sb_summary (osmdat, add_parking_osm_ids = add_parking_osm_ids)
#> • Total area = 22.8 hectares
#> • Proportion buildings: 36.6%
#> • Proportion open space: 7.36%
#> • Proportion roads: 15.7%, of which:
#>   • 32.5% is for car roads
#>   • 32.5% is for parked cars, and
#>   • 34.9% remains for everybody else.
#> 
#> • Total proportion of public space: 12.9
#> • Total proportion of public space as superblock: 23.1
#> • Increase in public space as superblock: 80%
```
