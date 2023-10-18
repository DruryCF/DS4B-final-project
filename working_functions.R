# Install data

avonet_install <- function(filename){
  
  message("Downloading AvoNet...\n")
  
  download.file(url = "https://figshare.com/ndownloader/files/34480856?private_link=b990722d72a26b5bfead", destfile = filename, mode = "wb")
  
}



# Load data

avonet_load <- function(filename,
                        clean = "none") {
  
  # store avonet as list of tibbles
  
  avonet <- list()
  
  names <- c("metadata", "birdlife", "ebird", "birdtree", "pigot", "raw_data", "mass_sources", "data_sources", "measurers", "crosswalk_birdlife_ebird", "crosswalk_birdlife_birdtree")
  
  for (i in 1:11) {
    x <- setNames(list(suppressWarnings(read_xlsx(filename, sheet = i, na = "NA"))), names[i])
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
  
  variable_lookup <- setNames(variable_new_name, variable_old_name)
  
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
    mutate(variable = case_when(variable == "hand-wing.index" ~ "hand_wing.index",
                                variable != "hand-wing.index" ~ variable))
  
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
  
  describe_sheets <- setNames(list(tibble(tibble_name = c("description", names(avonet)),
                                          description = c("This tibble; describes tibbles in AvoNet dataset",
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
                                   )
                              ),
                              nm = "description")
  
  
  avonet <- append(avonet, describe_sheets, after = 0)
  
  # Optional cleaning
  
  if (clean != "none") {
    
    # Create empty avonet_clean list
    
    avonet_clean <- list()
    
    # add index variable to enable identification of excluded data
    
    avonet_clean$raw_data <- avonet$raw_data %>%
      mutate(index = row_number(),
             .before = 0)
    
    if (clean %in% c("gapspecies", "all")) {
      
      # Relocate raw_data with measurer = gapspecies to avonet$gapspecies
      
      avonet_clean$gapspecies <- avonet_clean$raw_data %>% 
        filter(measurer == "GAPSPECIES") %>% 
        select(contains("species"), "avibase.id")
      
      avonet_clean$raw_data <- avonet_clean$raw_data %>% 
        filter(measurer != "GAPSPECIES")
      
      avonet$gapspecies <- avonet_clean$gapspecies
      
    }
    
    if (clean %in% c("unknown", "all")) {
      
      avonet_clean$raw_data <- avonet_clean$raw_data %>% 
        filter(!is.na(avibase.id))
      
    }
    
    if (clean %in% c("empty", "all")) {
      
      avonet_clean$raw_data <- avonet_clean$raw_data %>% 
        select(!c(locality, country))
      
    }
    
    # Identify excluded observations
    
    avonet_dirty <- list()
    
    avonet_dirty$raw_data <- avonet$raw_data %>% 
      mutate(index = row_number(),
             .before = 0)
    
    avonet_exclude <- list()
    
    avonet_exclude$raw_data <- avonet_dirty$raw_data %>% 
      filter(!(index %in% c(avonet_clean$raw_data$index)))
    
    # Remove index variable from $raw_data
    
    avonet_clean$raw_data <- avonet_clean$raw_data %>% 
      select(!index)
    
    # Override dirty data with clean
    
    avonet$raw_data <- avonet_clean$raw_data
    
    # Write excluded observations to global environment
    avonet_exclude <<- avonet_exclude
    
  }
  
  # Return avonet
  
  return(avonet)
  
}



# extract data from raw_data

avonet_extract_intraspecies <- function (filename,
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
    mutate(genus = strsplit(species1, " ")[[1]][1], # Code written by ChatGPT to extract genus names from species
           .before = species1) %>%
    rename(species = species1,
           order = order1,
           family = family1)
  
  ebird <- avonet$ebird %>% 
    select(order2, family2, species2) %>% 
    rowwise() %>% 
    mutate(genus = strsplit(species2, " ")[[1]][1], # Code written by ChatGPT to extract genus names from species
           .before = species2) %>%
    rename(species = species2,
           order = order2,
           family = family2)
  
  birdtree <- avonet$birdtree %>% 
    select(order3, family3, species3) %>% 
    rowwise() %>% 
    mutate(genus = strsplit(species3, " ")[[1]][1], # Code written by ChatGPT to extract genus names from species
           .before = species3) %>%
    rename(species = species3,
           order = order3,
           family = family3)
  
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



# extract data from species average tibbles

avonet_extract_interspecies <- function (filename,
                                         taxon_system = "birdlife",
                                         traits = NULL) {
  
  t_c <- taxon_system
  avonet <- avonet_load(filename = filename)
  
  if (taxon_system == "birdlife") {
    
    data <- avonet$birdlife
    
    # Extract genus from species
    
    data <- data %>% 
      rowwise() %>% 
      mutate(genus = strsplit(species1, " ")[[1]][1]) %>%  # Code written by ChatGPT to extract genus names from species
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
      mutate(genus = strsplit(species2, " ")[[1]][1]) %>%  # Code written by ChatGPT to extract genus names from species
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
      mutate(genus = strsplit(species3, " ")[[1]][1]) %>%  # Code written by ChatGPT to extract genus names from species
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



# summary function

avonet_summary <- function (data,
                            group = c("species"),
                            stats = c("mean"),
                            tidy = FALSE
) {
  
  extract <- data %>% 
    ungroup() %>% 
    group_by(across(all_of(group)))
  
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
    
    for(i in 1:length(stats)) {
      
      stat <- extract %>% 
        summarise_at(all_of(traits_numeric), c(get(stats[i])), na.rm = T) %>% 
        pivot_longer(cols = traits_numeric,
                     names_to = "trait",
                     values_to = all_of(stats[i]))
      
      stat_name <- paste(stats[i], "_x", sep = "")
      
      assign(stat_name, stat)
    }
    
    if (length(stats) > 1) {
      
      stat_name <- paste(stats[1], "_x", sep = "")
      
      summary <- get(stat_name, envir = environment())  # envir = environment() was written by ChatGPT to access the local function environment in which stat_name is stored
      
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
               .after = trait)
    
  } else {
    
    if (tidy == TRUE) {
      
      count_values_index <- length(group) + 1
      
      summary <- extract %>% 
        summarise_at(traits_numeric, stats, na.rm = T)
      
      count <- extract %>% 
        summarise(n())
      
      summary <- summary %>% 
        ungroup() %>% 
        mutate(count = count[[count_values_index]],
               .after = length(group))
      
    }
  } 
  
  message("\nIf present, NA values are included in count")
  
  return(summary)
  
}