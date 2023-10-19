#' Extract data from raw_data; bypasses avonet_load()
#'
#' @param filename A character describing the file path to the downloaded avonet dataset (same as filename in avonet_install()).
#' @param taxon_system A character defining the taxon classification system to use. Define as 'birdlife', 'ebird', or 'birdtree'.
#' @param traits A character vector describing the non-taxonomy variables to extract (usually traits).
#' @param clean A character vector defining cleaning options. Defaults to 'none' which loads all observations. 'all' will implement all cleaning options. 'gapspecies' will remove species with no observations. 'unknown' will remove observations of unknown species. 'empty' will remove the locality and country variables which are (at time of writing) embargoed.
#'
#' @return A tibble with the extracted data from $raw_data
#' @export
#'
#' @importFrom "dplyr" "filter" "mutate" "%>%" "select" "tibble" "rename" "contains" "case_when" "rowwise" "ungroup" "everything" "all_of" "row_number"
#' @importFrom "stats" "setNames"
#'
#' @examples
#' avonet_install(filename = "raw_data/avonet_dataset.xlsx")
#' avonet_extract_intraspecies(filename = "raw_data/avonet_dataset.xlsx", taxon_system = "birdlife", traits = c("sex", ".length"), clean = "all")
avonet_extract_intraspecies <- function(filename,
                                        taxon_system = "birdlife",
                                        traits = NULL,
                                        clean = "none") {
  t_c <- taxon_system
  avonet <- avonet_load(filename = filename, clean = clean)
  data <- avonet$raw_data

  # Extract taxon systems

  taxon_systems <- list()

  birdlife <- avonet$birdlife %>%
    select(order1, family1, species1) %>%
    rowwise() %>%
    mutate(
      genus = strsplit(species1, " ")[[1]][1], # Code written by ChatGPT to extract genus names from species
      .before = species1
    ) %>%
    rename(
      species = species1,
      order = order1,
      family = family1
    )

  ebird <- avonet$ebird %>%
    select(order2, family2, species2) %>%
    rowwise() %>%
    mutate(
      genus = strsplit(species2, " ")[[1]][1], # Code written by ChatGPT to extract genus names from species
      .before = species2
    ) %>%
    rename(
      species = species2,
      order = order2,
      family = family2
    )

  birdtree <- avonet$birdtree %>%
    select(order3, family3, species3) %>%
    rowwise() %>%
    mutate(
      genus = strsplit(species3, " ")[[1]][1], # Code written by ChatGPT to extract genus names from species
      .before = species3
    ) %>%
    rename(
      species = species3,
      order = order3,
      family = family3
    )

  taxon_systems$birdlife <- birdlife
  taxon_systems$ebird <- ebird
  taxon_systems$birdtree <- birdtree

  # Retrieve taxon system

  if (tolower(t_c) == "birdlife") { # use of tolower() for case insensitive if() taken from stack overflow (https://stackoverflow.com/questions/51488049/r-case-insensitive-if-check)

    t_system <- taxon_systems$birdlife

    message("\ntaxon system:", tolower(t_c))
  } else {
    if (tolower(t_c) == "ebird") {
      t_system <- taxon_systems$ebird
      message("\ntaxon system:", tolower(t_c))
    } else {
      if (tolower(t_c) == "birdtree") {
        t_system <- taxon_systems$birdtree
        message("\ntaxon system:", tolower(t_c))
      } else {
        stop(paste(t_c, "is not a recognised taxonomy classification. Try 'birdlife', 'ebird', or 'birdtree'"))
      }
    }
  }

  # add taxon system to raw_data -> method adapted from StackOverflow (https://stackoverflow.com/questions/50615116/renaming-character-variables-in-a-column-in-data-frame-r)

  taxon_new <- t_system[1:3]

  taxon_species <- t_system[4]

  taxon_names_lookup_o <- setNames(c(taxon_new$order), c(taxon_species$species))
  taxon_names_lookup_f <- setNames(c(taxon_new$family), c(taxon_species$species))
  taxon_names_lookup_g <- setNames(c(taxon_new$genus), c(taxon_species$species))

  ## retrieve taxon specific species variable name in raw_data

  if (tolower(t_c) == "birdlife") {
    i <- 1
  }

  if (tolower(t_c) == "ebird") {
    i <- 2
  }

  if (tolower(t_c) == "birdtree") {
    i <- 3
  }

  species_variable_name <- paste("species", i, "_", t_c, sep = "")

  ## Reassign species to new taxon level

  data <- data %>%
    rename(species = all_of(species_variable_name))

  data$order <- as.character(taxon_names_lookup_o[data$species])
  data$family <- as.character(taxon_names_lookup_f[data$species])
  data$genus <- as.character(taxon_names_lookup_g[data$species])

  ## Rearrange column order

  data <- data %>%
    select(order, family, genus, species, everything())

  ## Drop unneeded taxon variables

  data <- data %>%
    select(!contains(c("ebird", "birdlife", "birdtree")))

  # Select traits

  if (!is.null(traits)) {
    data <- data %>%
      select(order, family, genus, species, contains(c(traits)))
  }

  # Return data

  return(data)
}

#' Extract data from interspecies mean tibbles (i.e. $birdlife, $ebird, $birdtree); bypasses avonet_load()
#'
#' @param filename A character describing the file path to the downloaded avonet dataset (same as filename in avonet_install()).
#' @param taxon_system A character defining the taxon classification system to use. Define as 'birdlife', 'ebird', or 'birdtree'; accesses the specified tibble.
#' @param traits A character vector describing the non-taxonomy variables to extract (usually traits).
#'
#' @return A tibble with the extracted data from $birdlife, $ebird, or $birdtree
#' @export
#'
#' @importFrom "dplyr" "filter" "mutate" "%>%" "select" "tibble" "rename" "contains" "case_when" "rowwise" "ungroup" "everything" "all_of"
#' @importFrom "stats" "setNames"
#'
#' @examples
#' avonet_install(filename = "raw_data/avonet_dataset.xlsx")
#' avonet_extract_interspecies(filename = "raw_data/avonet_dataset.xlsx", traits = c("tail.length", "wing.length"))
avonet_extract_interspecies <- function(filename,
                                        taxon_system = "birdlife",
                                        traits = NULL) {
  t_c <- taxon_system
  avonet <- avonet_load(filename = filename)

  if (taxon_system == "birdlife") {
    data <- avonet$birdlife

    # Extract genus from species

    data <- data %>%
      rowwise() %>%
      mutate(genus = strsplit(species1, " ")[[1]][1]) %>% # Code written by ChatGPT to extract genus names from species
      ungroup()

    # Rearrange column order

    data <- data %>%
      select(order1, family1, genus, species1, everything()) %>%
      rename(order = order1, family = family1, species = species1)
  }

  if (taxon_system == "ebird") {
    data <- avonet$birdlife

    # Extract genus from species

    data <- data %>%
      rowwise() %>%
      mutate(genus = strsplit(species2, " ")[[1]][1]) %>% # Code written by ChatGPT to extract genus names from species
      ungroup()

    # Rearrange column order

    data <- data %>%
      select(order2, family2, genus, species2, everything()) %>%
      rename(order = order2, family = family2, species = species2)
  }

  if (taxon_system == "birdtree") {
    data <- avonet$birdtree

    # Extract genus from species

    data <- data %>%
      rowwise() %>%
      mutate(genus = strsplit(species3, " ")[[1]][1]) %>% # Code written by ChatGPT to extract genus names from species
      ungroup()

    # Rearrange column order

    data <- data %>%
      select(order3, family3, genus, species3, everything()) %>%
      rename(order = order3, family = family3, species = species3)
  }

  # Select traits

  if (!is.null(traits)) {
    data <- data %>%
      select(order, family, genus, species, contains(c(traits)))
  }

  # Return data

  message("\ntaxon system:", tolower(t_c))
  return(data)
}
