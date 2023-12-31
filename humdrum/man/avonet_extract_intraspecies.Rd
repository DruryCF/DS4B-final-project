% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avonet_extract.R
\name{avonet_extract_intraspecies}
\alias{avonet_extract_intraspecies}
\title{Extract data from raw_data; bypasses avonet_load()}
\usage{
avonet_extract_intraspecies(
  filename,
  taxon_system = "birdlife",
  traits = NULL,
  clean = "none"
)
}
\arguments{
\item{filename}{A character describing the file path to the downloaded avonet dataset (same as filename in avonet_install()).}

\item{taxon_system}{A character defining the taxon classification system to use. Define as 'birdlife', 'ebird', or 'birdtree'.}

\item{traits}{A character vector describing the non-taxonomy variables to extract (usually traits).}

\item{clean}{A character vector defining cleaning options. Defaults to 'none' which loads all observations. 'all' will implement all cleaning options. 'gapspecies' will remove species with no observations. 'unknown' will remove observations of unknown species. 'empty' will remove the locality and country variables which are (at time of writing) embargoed.}
}
\value{
A tibble with the extracted data from $raw_data
}
\description{
Extract data from raw_data; bypasses avonet_load()
}
\examples{
avonet_install(filename = "raw_data/avonet_dataset.xlsx")
avonet_extract_intraspecies(filename = "raw_data/avonet_dataset.xlsx", taxon_system = "birdlife", traits = c("sex", ".length"), clean = "all")
}
