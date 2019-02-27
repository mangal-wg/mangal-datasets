# Set libraries
library(tidyr)
library(dplyr)
library(readr)
library(forcats)
library(purrr)
library(tibble)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)
library(fs)
library(mangal)
#------------------------------
  # Metadata
#------------------------------

lat  <- 35.20674
lon  <- -76.230878
srid <- 4326

folder_name <- "copeland_1974" # Name of the subfolder in mangal-datasets
food_web_name <- "WEB16"

name_file <- read_csv("./Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% paste0(food_web_name, ".csv")) %>%
  split(.$web)

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence of absence of a recorded interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi        = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "http://hdl.handle.net/2027/uc1.31822000610949", # See page 318 of the document
             data_url  = "https://globalwebdb.com/",
             author    = "B. J.",
             year      = "1974",
             bibtex    = "@inbook{Copeland_1974, Address = {Washington, D.C.}, Author = {Odum, Howard T. and McMahan, E. A and Copeland, B. J. and United States. and Conservation Foundation}, Chapter = {Oligohaline regime}, Number = {IV.C-8}, Pages = {315-357, p.318}, Publisher = {Conservation Foundation in cooperation with National Oceanic and Atmospheric Administration, Office of Coastal Environment}, Title = {Coastal ecological systems of the United States}, url = {http://hdl.handle.net/2027/uc1.31822000610949}, Volume = {II}, Year = {1974}}")


users <- list(name         = "Clément VIOLET",
              email        = "clement.violet@etudiant.univ-brest.fr",
              orcid        = "0000-0001-6217-5891",
              organization = "Université de Bretagne Occidentale",
              type         = "administrator")


# enviro <- list(name  = "attribute name",
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)


dataset <- list(name        = "copeland_1974",
                 date        = "1974-01-01",
                 description = "Food web of the oligohaline systeme",
                 public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(name        = "copeland_1974",
                 date        = "1974-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Food web of the oligohaline systeme",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "1974-01-01",
              method        = "null",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
# Cleaning matrix
#------------------------------

data_matrice <- paste0("./Trophic-metacommunities-master/Trophic_metacom_meta_analysis/interaction matrices/",
                              food_web_name, ".csv") %>%
  map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>%
  map(~rename(.x, sp_id = X1))
data_matrice[[1]][1,1] <- NA # Remove file identifier use  NA value

data_col_name <- data_matrice %>%
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~unname(.x))

data_row_name <- data_matrice %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
  map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
  map(~unlist(.x)) %>%
  map(~unname(.x))

FW_name <- data_matrice %>% # Construct the Food Web matric to inject
  map(~filter(.x, !is.na(select(.x,1)))) %>%
  map2(modify(data_row_name, ~.x), ~mutate(.x, sp_id = .y)) %>%
  map(~column_to_rownames(.x, "sp_id")) %>% # Store sp name as row name
  map2(modify(data_col_name, ~.x), ~{`names<-`(.x, .y)}) %>%
  map(~rownames_to_column(.x, "sp_taxon_2")) %>%
  map(~gather(.x, "sp_taxon_1", "value", -sp_taxon_2)) %>% #Convert large df to long format
  map(~select(.x, sp_taxon_1, sp_taxon_2, value)) %>%
  map(~filter(.x, value != 0)) %>%# Remove 0 interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hytoplan.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)")
                                , "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Dd]ebris.*)|(.*[Dd]etri*)"), "commensalism", .x$type))) %>%
  flatten_df()

source("sep_pooled.R")
FW_name <- sep_pooled(FW_name, "(\\s-\\s)")

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data

taxa <- FW_name[1] %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
  map(~gather(.x, id, sp)) %>%
  modify(~deframe(.x)) %>%
  map(~unique(.x)) %>%
  map(~enframe(.x, name = NULL, value = "original_name"))

sp_name_for_this_web <- name_file %>%
  map(~select(.x, original_name, scientific_name)) %>%
  map(~mutate(.x, scientific_name = if_else(is.na(.x$scientific_name), original_name, scientific_name))) %>%
  map(~deframe(.x))

taxa_taxize <- FW_name[2] %>% # Use taxize to correct depooled species names
  map(~gnr_resolve(.x, canonical = FALSE, best_match_only = T)) # Check for wrong names

taxa_name_not_known <- taxa_taxize %>% # Names not recognized by Global Names Recognition and Discovery.
  map(~attributes(.x)$not_known) %>% 
  map(~enframe(.x, name = NULL, value = "user_supplied_name")) %>% # Create a df simillar to sp_name_cleaned to bind it whith sp_name_cleaned
  map(~mutate(.x, submitted_name = paste0(str_to_upper(str_extract(.x$user_supplied_name, ".{1}")), str_remove(.x$user_supplied_name, ".{1}")))) %>%
  map(~mutate(.x, matched_name = submitted_name,
              data_source_title = NA_character_,
              score = NA_real_))

taxa_taxize <- taxa_taxize %>% # Putting all taxize results together
  map2(taxa_name_not_known, ~bind_rows(.x, .y)) %>%
  map(~select(.x, user_supplied_name, matched_name)) %>%
  map(~deframe(.x))

taxa_df <- taxa %>%
  map2(sp_name_for_this_web, ~mutate(.x, name_clear = str_replace_all(.x$original_name, .y))) %>%
  map2(taxa_taxize, ~mutate(.x, name_clear = str_replace_all(.x$original_name, .y))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$")))

taxa_df[[1]]$name_clear[9] <- "Small anellovirus"
taxa_df <- unname(taxa_df)

FW_name[[2]] <- NULL
FW_name <- unname(FW_name)
## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- taxa_df %>%
  map(~unlist(.x$name_clear)) %>%
  map(~unname(.x)) %>%
  flatten_chr() %>%
  unique() %>%
  map_chr(~{modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", str_replace_all(.x, " ", "%20")))}) %>%
  map_chr(~str_replace_all(.x, ".*,%20.*", "_")) %>%
  map_chr(~str_replace_all(.x, ".*%20-%20.*", "-")) %>%
  map_chr(~str_replace_all(.x, "\\.%20", "__")) %>%
  keep(~length(content((GET(url = .x, config = add_headers("Content-type" = "application/json","Authorization" = paste("bearer", readRDS(".httr-oauth"))))))) == 0) %>%
  map_chr(~str_remove_all(.x, fixed("http://poisotlab.biol.umontreal.ca/api/v2/taxonomy/?name="))) %>%
  map_chr(~str_replace_all(.x, fixed("%20"), " ")) %>%
  map_chr(~str_replace_all(.x, fixed("__"), ". ")) %>%
  map_chr(~str_replace_all(.x, fixed("_"), ", "))

taxa_back_df <- taxa_back_df %>%
  enframe(name = NULL, value = "name") %>%
  mutate(bold = as.double(unlist({map(.$name, ~get_boldid(.x, row = 5, verbose = FALSE)[1])})),
         eol = as.double(unlist({map(.$name, ~get_eolid(.x, row = 5, verbose = FALSE, key = 110258)[1])})),
         tsn = as.double(unlist({map(.$name, ~get_tsn(.x, row = 5, verbose = FALSE)[1])})),
         ncbi = as.double(unlist({map(.$name, ~get_uid(.x, row = 5, verbose = FALSE, key = "679d0a26947d9b6432371b268ec0c7b39b08")[1])}))) # Add API KEy for NCBI

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#-------------------------------------------
# Writing taxa interaction and trait table
#-------------------------------------------

write.csv2(x = taxa_back_df, file = paste0(folder_name,"/data/",folder_name, "_taxonomy.csv"), row.names = FALSE)

if(is.null(names(taxa_df)) == TRUE){
  
  taxa_df %>%
    walk(~write.csv2(x = taxa_df, file = paste0(folder_name,"/data/",folder_name, "_node.csv"), row.names = FALSE))
  
}else{
  
  taxa_df %>%
    names() %>%
    walk(~write.csv2(x = taxa_df[[.]], file = paste0(folder_name,"/data/",folder_name, "_", str_replace_all(., "\\s", "_"), "_node.csv"), row.names = FALSE))
  
}

if(is.null(names(FW_name)) == TRUE){
  
  FW_name %>%
    walk(~write.csv2(x = FW_name, file = paste0(folder_name,"/data/",folder_name, "_inter.csv"), row.names = FALSE))
  
}else{
  
  FW_name %>%
    names() %>%
    walk(~write.csv2(x = FW_name[[.]], file = paste0(folder_name,"/data/",folder_name, "_", str_replace_all(., "\\s", "_"), "_inter.csv"), row.names = FALSE))
}


#------------------------------
# Throwing injection functions
#------------------------------

## Metadata
POST_attribute(attr_inter)
# POST_attribute(attr1)
# POST_attribute(attr2)
POST_ref(ref)
POST_users(users)
# POST_environment(enviro, attr_##)
POST_dataset(dataset, users, ref)

## Network
POST_network(network_lst = network, dataset = dataset, users = users, enviro = NULL)

## Taxonomy
map(taxa_back_df, ~POST_taxonomy(.x))

## Node
map(taxa_df, ~POST_node(.x, network))
POST_node(taxa_df[[1]], network)

## Interaction
map(FW_name, ~POST_interaction(.x, inter = inter, enviro = NULL, attr = attr_inter, users, network = network))
POST_interaction(FW_name[[1]], inter = inter, enviro = NULL, attr = attr_inter, users, network = network)
rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)