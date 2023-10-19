#' Apply summary statistics to extracted data
#'
#' @param data A tibble produced by an avonet_extract_*() function
#' @param group A character vector to summarise by (e.g. c("species", "sex"))
#' @param stats A character vector of summary statistics (e.g. c("mean", "median", "sd"))
#' @param tidy A logical value defining tibble format; FALSE returns long, visually preferable form while TRUE returns wide, statistically preferable form
#'
#' @return A tibble summarising extracted data
#' @export
#'
#' @importFrom "dplyr" "filter" "mutate" "%>%" "select" "tibble" "rename" "contains" "case_when" "rowwise" "ungroup" "everything" "all_of" "row_number" "group_by" "across" "summarise" "summarise_at" "relocate" "n"
#' @importFrom "stats" "setNames"
#' @importFrom "tidyr" "pivot_longer"
#' @importFrom "data.table" ":="
#'
#' @examples
#' avonet_install(filename = "raw_data/avonet_dataset.xlsx")
#' extract <- avonet_extract_intraspecies(filename = "raw_data/avonet_dataset.xlsx", taxon_system = "birdlife", traits = c("sex", ".length"), clean = "all")
#' avonet_summary(data = extract, group = c("species", "sex"), stats = c("mean", "median", "max", "min", "sd"), tidy = FALSE)
avonet_summary <- function(data,
                           group = c("species"),
                           stats = c("mean"),
                           tidy = FALSE) {
  # group observations

  extract <- data %>%
    ungroup() %>%
    group_by(across(all_of(group)))

  # identify numeric traits

  traits <- extract %>%
    ungroup() %>%
    names()

  traits_numeric <- c()

  for (i in 1:length(traits)) {
    trait_name <- traits[i]
    assign(traits[i], extract[[i]])

    if (is.numeric(extract[[i]])) {
      traits_numeric <- append(traits_numeric, traits[i])
    }
  }

  if (tidy == FALSE) {
    for (i in 1:length(stats)) {
      # apply statistics and create trait variable

      stat <- extract %>%
        summarise_at(traits_numeric, c(get(stats[i])), na.rm = T) %>%
        pivot_longer(
          cols = traits_numeric,
          names_to = "trait",
          values_to = stats[i]
        )

      stat_name <- paste(stats[i], "_x", sep = "")

      assign(stat_name, stat)
    }

    if (length(stats) > 1) {
      # combine computed statistics into single tibble

      stat_name <- paste(stats[1], "_x", sep = "")

      summary <- get(stat_name, envir = environment()) # envir = environment() was written by ChatGPT to access the local function environment in which stat_name is stored

      stat_values_index <- length(group) + 2

      for (i in 2:length(stats)) {
        stat_name <- paste(stats[i], "_x", sep = "")

        stat_function_name <- stats[i]

        stat_values <- get(stat_name, envir = environment())[[stat_values_index]]

        summary <- summary %>%
          ungroup() %>%
          mutate(!!stat_function_name := stat_values)
      }
    } else {
      # Retrieve single computed statistic

      stat_name <- paste(stats[1], "_x", sep = "")

      summary <- get(stat_name, envir = environment())
    }

    # Add count data

    count <- extract %>%
      summarise(count = n())

    name <- group[length(group)]

    count <- count %>%
      rename(var = name)

    group_lookup <- setNames(c(count$count), c(count$var))

    summary <- summary %>%
      rename(var = name)

    summary$count <- as.character(group_lookup[summary$var])

    summary <- summary %>%
      rename(!!name := var) %>%
      relocate(count,
        .after = trait
      )
  } else {
    if (tidy == TRUE) {
      # compute summary statistics

      count_values_index <- length(group) + 1

      summary <- extract %>%
        summarise_at(traits_numeric, stats, na.rm = T)

      count <- extract %>%
        summarise(n())

      summary <- summary %>%
        ungroup() %>%
        mutate(
          count = count[[count_values_index]],
          .after = length(group)
        )
    }
  }

  message("\nIf present, NA values are included in count")


  # Return summary
  return(summary)
}
