% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avonet_load.R
\name{avonet_load}
\alias{avonet_load}
\title{Load and tidy avonet dataset}
\usage{
avonet_load(filename, clean = "none")
}
\arguments{
\item{filename}{A character describing the file path to the downloaded avonet dataset (same as filename in avonet_install())}

\item{clean}{A character vector defining cleaning options. Defaults to 'none' which loads all observations. 'all' will implement all cleaning options. 'gapspecies' will remove species with no observations from $raw_data and place them in their own $gapspecies tibble. 'unknown' will remove observations of unknown species. 'empty' will remove the locality and country variables which are (at time of writing) embargoed.}
}
\value{
A list of tibbles comprising the avonet dataset; if clean != 'none', will write avonet_exclude object to global environment detailing removed observations.
}
\description{
Load and tidy avonet dataset
}
\examples{
avonet_install(filename = "raw_data/avonet_dataset.xlsx")
avonet_load(filename = "raw_data/avonet_dataset.xlsx", clean = "all")
}
