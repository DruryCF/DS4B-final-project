#' Load and tidy avonet dataset
#'
#' @param filename A character describing the file path to the downloaded avonet dataset (same as filename in avonet_install())
#' @param clean A character vector defining cleaning options. Defaults to 'none' which loads all observations. 'all' will implement all cleaning options. 'gapspecies' will remove species with no observations from $raw_data and place them in their own $gapspecies tibble. 'unknown' will remove observations of unknown species. 'empty' will remove the locality and country variables which are (at time of writing) embargoed.
#'
#' @return A list of tibbles comprising the avonet dataset; if clean != 'none', will write avonet_exclude object to global environment detailing removed observations.
#' @export
#'
#' @importFrom "dplyr" "filter" "mutate" "%>%" "select" "tibble" "rename" "contains" "case_when"
#' @importFrom "stats" "setNames"
#'
#' @examples
#' avonet_install(filename = "raw_data/avonet_dataset.xlsx")
#' avonet_load(filename = "raw_data/avonet_dataset.xlsx", clean = "all")
avonet_load <- function(filename,
                        clean = "none") {
  # store avonet as list of tibbles

  avonet <- list()

  names <- c("metadata", "birdlife", "ebird", "birdtree", "pigot", "raw_data", "mass_sources", "data_sources", "measurers", "crosswalk_birdlife_ebird", "crosswalk_birdlife_birdtree")

  for (i in 1:11) {
    x <- setNames(list(suppressWarnings(readxl::read_xlsx(filename, sheet = i, na = "NA"))), names[i])
    avonet <- append(avonet, x)
  }

  # Rename variables

  ## Make variables lowercase across tibbles

  colnames(avonet$metadata) <- tolower(colnames(avonet$metadata))
  colnames(avonet$birdlife) <- tolower(colnames(avonet$birdlife))
  colnames(avonet$ebird) <- tolower(colnames(avonet$ebird))
  colnames(avonet$birdtree) <- tolower(colnames(avonet$birdtree))
  colnames(avonet$pigot) <- tolower(colnames(avonet$pigot))
  colnames(avonet$raw_data) <- tolower(colnames(avonet$raw_data))
  colnames(avonet$mass_sources) <- tolower(colnames(avonet$mass_sources))
  colnames(avonet$data_sources) <- tolower(colnames(avonet$data_sources))
  colnames(avonet$measurers) <- tolower(colnames(avonet$measurers))
  colnames(avonet$crosswalk_birdlife_ebird) <- tolower(colnames(avonet$crosswalk_birdlife_ebird))
  colnames(avonet$crosswalk_birdlife_birdtree) <- tolower(colnames(avonet$crosswalk_birdlife_birdtree))

  ## Make variable character values lowercase in metadata tibble -> method adapted from StackOverflow (https://stackoverflow.com/questions/50615116/renaming-character-variables-in-a-column-in-data-frame-r)

  variable_old_name <- c(avonet$metadata$variable)

  variable_new_name <- c(tolower(avonet$metadata$variable))

  variable_lookup <- stats::setNames(variable_new_name, variable_old_name)

  avonet$metadata$variable <- as.character(variable_lookup[avonet$metadata$variable])

  ## Remove space in 'variable types' in metadata tibble

  avonet$metadata <- avonet$metadata %>%
    rename(variable.types = `variable types`)

  ## Replace '-' in hand-wing.index

  avonet$birdlife <- avonet$birdlife %>%
    rename(`hand_wing.index` = `hand-wing.index`)
  avonet$ebird <- avonet$ebird %>%
    rename(`hand_wing.index` = `hand-wing.index`)
  avonet$birdtree <- avonet$birdtree %>%
    rename(`hand_wing.index` = `hand-wing.index`)
  avonet$raw_data <- avonet$raw_data %>%
    rename(`hand_wing.index` = `hand-wing.index`)

  avonet$metadata <- avonet$metadata %>%
    mutate(variable = case_when(
      variable == "hand-wing.index" ~ "hand_wing.index",
      variable != "hand-wing.index" ~ variable
    ))

  # Correct variable class

  ## raw_data

  avonet$raw_data$data.type <- avonet$raw_data$data.type %>%
    as.factor()
  avonet$raw_data$protocol <- avonet$raw_data$protocol %>%
    as.factor()
  avonet$raw_data$age <- avonet$raw_data$age %>%
    as.factor()
  avonet$raw_data$sex <- avonet$raw_data$sex %>%
    as.factor()

  ## birdlife

  avonet$birdlife$sequence <- avonet$birdlife$sequence %>%
    as.character()
  avonet$birdlife$migration <- avonet$birdlife$migration %>%
    as.factor()
  avonet$birdlife$habitat.density <- avonet$birdlife$habitat.density %>%
    as.factor()
  avonet$birdlife$habitat <- avonet$birdlife$habitat %>%
    as.factor()
  avonet$birdlife$trophic.level <- avonet$birdlife$trophic.level %>%
    as.factor()
  avonet$birdlife$trophic.niche <- avonet$birdlife$trophic.niche %>%
    as.factor()
  avonet$birdlife$primary.lifestyle <- avonet$birdlife$primary.lifestyle %>%
    as.factor()
  avonet$birdlife$inference <- avonet$birdlife$inference %>%
    as.factor()

  ## ebird

  avonet$ebird$migration <- avonet$ebird$migration %>%
    as.factor()
  avonet$ebird$habitat.density <- avonet$ebird$habitat.density %>%
    as.factor()
  avonet$ebird$habitat <- avonet$ebird$habitat %>%
    as.factor()
  avonet$ebird$trophic.level <- avonet$ebird$trophic.level %>%
    as.factor()
  avonet$ebird$trophic.niche <- avonet$ebird$trophic.niche %>%
    as.factor()
  avonet$ebird$primary.lifestyle <- avonet$ebird$primary.lifestyle %>%
    as.factor()
  avonet$ebird$inference <- avonet$ebird$inference %>%
    as.factor()

  ## birdtree

  avonet$birdtree$migration <- avonet$birdtree$migration %>%
    as.factor()
  avonet$birdtree$habitat.density <- avonet$birdtree$habitat.density %>%
    as.factor()
  avonet$birdtree$habitat <- avonet$birdtree$habitat %>%
    as.factor()
  avonet$birdtree$trophic.level <- avonet$birdtree$trophic.level %>%
    as.factor()
  avonet$birdtree$trophic.niche <- avonet$birdtree$trophic.niche %>%
    as.factor()
  avonet$birdtree$primary.lifestyle <- avonet$birdtree$primary.lifestyle %>%
    as.factor()
  avonet$birdtree$inference <- avonet$birdtree$inference %>%
    as.factor()
  avonet$birdtree$species.status <- avonet$birdtree$species.status %>%
    as.factor()

  ## pigot

  avonet$pigot$inference <- avonet$pigot$inference %>%
    as.factor()

  # Add tibble to 'avonet' to explain each tibble

  describe_sheets <- setNames(
    list(tibble(
      tibble_name = c("description", names(avonet)),
      description = c(
        "This tibble; describes tibbles in AvoNet dataset",
        "Definitions and descriptions of variables and key terms",
        "Averages across functional traits for species according to BirdLife International taxonomy classification (excludes intraspecies variation); includes traits recorded only at the species level and not the individual such as mass and geographic data",
        "Averages across functional traits for species according to eBird taxonomy classification (excludes intraspecies variation); includes traits recorded only at the species level and not the individual such as mass and geographic data",
        "Averages across functional traits for species according to BirdTree taxonomy classification (excludes intraspecies variation); includes traits recorded only at the species level and not the individual such as mass and geographic data",
        "The functional trait dataset using birdtree taxonomy built by Pigot and published in 2020 that the raw_data added to",
        "Functional trait observations for individual birds; can be used for intraspecies variation and to separate values by sex",
        "Details sources from which the mass trait values were derived or taken",
        "Details sources of raw_data such as museum where specimens are held or field measurements",
        "Gives the full names of measurers linked to the code given in raw_data and the number of individuals that they measured",
        "Shows how the ebird species concepts map onto the birdlife species concepts",
        "Shows how the birdtree species concepts map onto the birdlife species concepts"
      )
    )),
    nm = "description"
  )


  avonet <- append(avonet, describe_sheets, after = 0)

  # Optional cleaning

  if (clean != "none") {
    ## Create empty avonet_clean list

    avonet_clean <- list()

    ## add index variable to enable identification of excluded data

    avonet_clean$raw_data <- avonet$raw_data %>%
      mutate(
        index = row_number(),
        .before = 0
      )

    if (clean %in% c("gapspecies", "all")) {
      ## Relocate raw_data with measurer = gapspecies to avonet$gapspecies

      avonet_clean$gapspecies <- avonet_clean$raw_data %>%
        filter(measurer == "GAPSPECIES") %>%
        select(contains("species"), "avibase.id")

      avonet_clean$raw_data <- avonet_clean$raw_data %>%
        filter(measurer != "GAPSPECIES")

      avonet$gapspecies <- avonet_clean$gapspecies
    }

    if (clean %in% c("unknown", "all")) {
      ## Remove observations with unknown species
      avonet_clean$raw_data <- avonet_clean$raw_data %>%
        filter(!is.na(avibase.id))
    }

    if (clean %in% c("empty", "all")) {
      ## Remove variables with embargoed data
      avonet_clean$raw_data <- avonet_clean$raw_data %>%
        select(!c(locality, country))
    }

    ## Identify excluded observations

    avonet_dirty <- list()

    avonet_dirty$raw_data <- avonet$raw_data %>%
      mutate(
        index = row_number(),
        .before = 0
      )

    avonet_exclude <- list()

    avonet_exclude$raw_data <- avonet_dirty$raw_data %>%
      filter(!(index %in% c(avonet_clean$raw_data$index)))

    ## Remove index variable from $raw_data

    avonet_clean$raw_data <- avonet_clean$raw_data %>%
      select(!index)

    ## Override dirty data with clean

    avonet$raw_data <- avonet_clean$raw_data

    ## Write excluded observations to global environment
    avonet_exclude <<- avonet_exclude
  }

  # Return avonet

  return(avonet)
}
