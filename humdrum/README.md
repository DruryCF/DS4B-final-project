
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dependencies

humdrum relies heavily on tidyverse packages, specifically data.table,
dplyr, readxl, and tidyr.

# <img src="https://github.com/DruryCF/DS4B-final-project/assets/141908184/817df146-5e8c-4057-abd1-84fb62d81808" height="100">humdrum

<!-- badges: start -->
<!-- badges: end -->

The goal of humdrum is to interface with the AvoNet dataset and take the
humdrum out of exploratory data analysis. Humdrum can install, load,
tidy, clean, extract, and summarise AvoNet.

The AvoNet dataset contains functional trait data for 90,020 individual
birds representing 11,009 extant species sampled from 181 countries
(Tobias et al., 2022). It includes 11 continuous morphological traits
(such as beak and wing dimensions; and tail, tarsus, and body size), 6
ecological variables, as well as geographic data. AvoNet also includes
species averages according to three different taxonomy classifications
to enable integration with other databases.

AvoNet is difficult to explore and interact with as it is stored across
11 related excel spreadsheets. Other datasets stored similarly
(e.g. AusTraits, Atlas of Living Australia) have dedicated R-packages to
streamline their use (Falster et al., 2021; Westgate et al., 2023). Some
of AvoNet is incorporated in the ‘traitdata’ package, however this (at
the time of writing) is only a single observed individual per species
according to the BirdLife taxonomy classification (RS-eco, 2022). An
R-package that interfaces with the entirety of AvoNet would facilitate
EDA and potentially aid in addressing specific research questions.

Humdrum has functions to:

\* install and load AvoNet as a relational
database (stored as a list of tibbles)

\* tidy data when loading
(largely correcting variable class and renaming as AvoNet is already in
tidy format)

\* clean data when loading (toggled based on user
preference)

\* facilitate Exploratory Data Analysis (EDA) (extract
defined variables according to different taxonomies and apply summary
statistics)
## Installation

You can install the development version of humdrum from
[GitHub](https://github.com/DruryCF/DS4B-final-project) with:

``` r
# install.packages("devtools")
devtools::install_github("DruryCF/DS4B-final-project")
```

## Example

This basic example demonstrates workflow with humdrum:

``` r
library(humdrum)

avonet_install(filename = "raw_data/avonet_dataset.xlsx")
#> Downloading AvoNet...

avonet <- avonet_load(filename = "raw_data/avonet_dataset.xlsx", clean = "all")

# As the avonet object prints many tibbles, here's some key ones:
# This tibble is built entirely by humdrum to describe what each tibble contains:
avonet$description 
#> # A tibble: 12 × 2
#>    tibble_name                 description                                      
#>    <chr>                       <chr>                                            
#>  1 description                 This tibble; describes tibbles in AvoNet dataset 
#>  2 metadata                    Definitions and descriptions of variables and ke…
#>  3 birdlife                    Averages across functional traits for species ac…
#>  4 ebird                       Averages across functional traits for species ac…
#>  5 birdtree                    Averages across functional traits for species ac…
#>  6 pigot                       The functional trait dataset using birdtree taxo…
#>  7 raw_data                    Functional trait observations for individual bir…
#>  8 mass_sources                Details sources from which the mass trait values…
#>  9 data_sources                Details sources of raw_data such as museum where…
#> 10 measurers                   Gives the full names of measurers linked to the …
#> 11 crosswalk_birdlife_ebird    Shows how the ebird species concepts map onto th…
#> 12 crosswalk_birdlife_birdtree Shows how the birdtree species concepts map onto…

# This is the original Metadata excel spreadsheet with renamed variables:
avonet$metadata
#> # A tibble: 71 × 5
#>    variable description                              variable.types units source
#>    <chr>    <chr>                                    <chr>          <chr> <chr> 
#>  1 sequence "Unique sequence identifier used by Bir… ID             <NA>  "HBW-…
#>  2 species1 "Species taxonomy according to BirdLife… categorical    <NA>  "HBW-…
#>  3 family1  "Family-level taxonomy according to Bir… categorical    <NA>  "HBW-…
#>  4 order1   "Order-level taxonomy according to Bird… categorical    <NA>  "HBW-…
#>  5 species2 "Species taxonomy according to eBird (f… categorical    <NA>  "eBir…
#>  6 family2  "Family-level taxonomy according to eBi… categorical    <NA>  "eBir…
#>  7 order2   "Order-level taxonomy according to eBir… categorical    <NA>  "eBir…
#>  8 species3 "Species taxonomy and nomenclature acco… categorical    <NA>  "Jetz…
#>  9 family3  "Family-level taxonomy and nomenclature… categorical    <NA>  "Jetz…
#> 10 order3   "Order-level taxonomy according to Bird… categorical    <NA>  "Jetz…
#> # ℹ 61 more rows

# This is the original AVONET_Raw_Data excel spreadsheet with renamed variables and excluded observations outlined in the avonet_exclude object written to the global environment (also removes empty variables with embargoed data):
avonet$raw_data
#> # A tibble: 90,019 × 24
#>    avibase.id       species1_birdlife      species2_ebird    ebird.species.group
#>    <chr>            <chr>                  <chr>             <chr>              
#>  1 AVIBASE-B3F5E5E2 Abeillia abeillei      Abeillia abeillei Abeillia abeillei  
#>  2 AVIBASE-B3F5E5E2 Abeillia abeillei      Abeillia abeillei Abeillia abeillei  
#>  3 AVIBASE-B3F5E5E2 Abeillia abeillei      Abeillia abeillei Abeillia abeillei  
#>  4 AVIBASE-B3F5E5E2 Abeillia abeillei      Abeillia abeillei Abeillia abeillei  
#>  5 AVIBASE-B3F5E5E2 Abeillia abeillei      Abeillia abeillei Abeillia abeillei  
#>  6 AVIBASE-684016CB Abroscopus albogularis Abroscopus albog… Abroscopus albogul…
#>  7 AVIBASE-684016CB Abroscopus albogularis Abroscopus albog… Abroscopus albogul…
#>  8 AVIBASE-684016CB Abroscopus albogularis Abroscopus albog… Abroscopus albogul…
#>  9 AVIBASE-684016CB Abroscopus albogularis Abroscopus albog… Abroscopus albogul…
#> 10 AVIBASE-684016CB Abroscopus albogularis Abroscopus albog… Abroscopus albogul…
#> # ℹ 90,009 more rows
#> # ℹ 20 more variables: species3_birdtree <chr>, data.type <fct>, source <chr>,
#> #   specimen.number <chr>, sex <fct>, age <fct>, country_wri <chr>,
#> #   beak.length_culmen <dbl>, beak.length_nares <dbl>, beak.width <dbl>,
#> #   beak.depth <dbl>, tarsus.length <dbl>, wing.length <dbl>,
#> #   kipps.distance <dbl>, secondary1 <dbl>, hand_wing.index <dbl>,
#> #   tail.length <dbl>, measurer <chr>, protocol <fct>, publication <chr>

# Extract data from $raw_data:
extract <- avonet_extract_intraspecies(filename = "raw_data/avonet_dataset.xlsx",
                            taxon_system = "birdtree",
                            clean = "all")
#> 
#> taxon system:birdtree

# Apply summary statistics by species and sex:
avonet_summary(data = extract,
               group = c("species", "sex"),
               stats = c("mean", "median", "max", "min", "sd"),
               tidy = FALSE)
#> Warning: There were 5822 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `beak.length_culmen = .Primitive("max")(beak.length_culmen,
#>   na.rm = TRUE)`.
#> ℹ In group 13: `species = "Acanthidops bairdii"`, `sex = F`.
#> Caused by warning:
#> ! no non-missing arguments to max; returning -Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 5821 remaining warnings.
#> Warning: There were 5822 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `beak.length_culmen = .Primitive("min")(beak.length_culmen,
#>   na.rm = TRUE)`.
#> ℹ In group 13: `species = "Acanthidops bairdii"`, `sex = F`.
#> Caused by warning:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 5821 remaining warnings.
#> 
#> If present, NA values are included in count
#> # A tibble: 219,010 × 9
#>    species           sex   trait            count  mean median   max   min    sd
#>    <chr>             <fct> <chr>            <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1 Abeillia abeillei F     beak.length_cul… 2     14.3   14.3   14.8  13.8 0.707
#>  2 Abeillia abeillei F     beak.length_nar… 2     10     10     10.5   9.5 0.707
#>  3 Abeillia abeillei F     beak.width       2      1.1    1.1    1.2   1   0.141
#>  4 Abeillia abeillei F     beak.depth       2      1.75   1.75   2     1.5 0.354
#>  5 Abeillia abeillei F     tarsus.length    2      7.55   7.55   8     7.1 0.636
#>  6 Abeillia abeillei F     wing.length      2     45     45     45    45   0    
#>  7 Abeillia abeillei F     kipps.distance   2     30.5   30.5   31    30   0.707
#>  8 Abeillia abeillei F     secondary1       2     14.5   14.5   15    14   0.707
#>  9 Abeillia abeillei F     hand_wing.index  2     67.8   67.8   68.9  66.7 1.57 
#> 10 Abeillia abeillei F     tail.length      2     28.5   28.5   30    27   2.12 
#> # ℹ 219,000 more rows
```

# AvoNet

If publishing, be sure to cite the original paper that released AvoNet:

Tobias, J.A., Sheard, C., Pigot, A.L. et al. (2022). AVONET:
morphological, ecological and geographical data for all birds. *Ecology
Letters, 25*(3), 581–597. <https://doi.org/10.1111/ele.13898>

# References

Falster, D., Gallagher, R., Wenk, E.H. et al. (2021). AusTraits, a
curated plant trait database for the Australian flora. *Sci Data, 8*,
254. <https://doi.org/10.1038/s41597-021-01006-6>

RS-eco (2022). *traitdata: Easy access to various ecological trait
data*. R package version 0.0.1, <https://github.com/RS-eco/traitdata>

Tobias, J.A., Sheard, C., Pigot, A.L. et al. (2022). AVONET:
morphological, ecological and geographical data for all birds. *Ecology
Letters, 25*(3), 581–597. <https://doi.org/10.1111/ele.13898>

Westgate, M., Kellie, D., Stevenson, M., Newman, P. (2023). *galah:
Biodiversity Data from the Living Atlas Community*. R package version
1.5.3, <https://CRAN.R-project.org/package=galah>

Note: Package was built using the devtools package following
<https://r-pkgs.org/>

Hexsticker was built using the hexSticker package following
<https://www.youtube.com/watch?v=O34vzdHOaEk>
