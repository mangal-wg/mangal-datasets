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

library(mangal)
#------------------------------
  # Metadata
#------------------------------

lat  <- 46.265856
lon  <- -124.212950
srid <- 4326

folder_name <- "brodeur_1992" # Name of the subfolder in mangal-datasets
food_web_name <- c("WEB290", "WEB291", "WEB292", "WEB293")

name_file <- read_csv("Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% paste0(food_web_name, ".csv")) %>%
  split(.$web)

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Dietary matrix",
                   table_owner = "interactions",
                   description = "Proportions of the consumer diets (sp_name_1) made up by the prey (sp_name_2)",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi       = "110.3354/meps084101",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.1016%2Fj.ecss.2006.10.013",
             data_url  = "https://globalwebdb.com/",
             author    = "RD Brodeur",
             year      = "1992",
             bibtex    = "@article{Brodeur_1992, doi = {10.3354/meps084101}, url = {https://doi.org/10.3354%2Fmeps084101}, year = 1992, publisher = {Inter-Research Science Center}, volume = {84}, pages = {101--119}, author = {RD Brodeur and WG Pearcy}, title = {Effects of environmental variability on trophic interactions and food web structure in a pelagic upwelling ecosystem }, journal = {Marine Ecology Progress Series}}")

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


dataset <- list(name        = "brodeur_1992",
                 date        = "1981-05-01",
                 description = "Food web of coastal upwelling pelagic zone off Oregon and Washington",
                 public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(brodeur_1981 = list(name             = "brodeur_1981",
                                    date             = "1981-05-01",
                                    lat              = lat,
                                    lon              = lon,
                                    srid             = srid,
                                    description      = "Food web of coastal upwelling pelagic zone off Oregon and Washington",
                                    public           = TRUE,
                                    all_interactions = FALSE),
                brodeur_1982 = list(name             = "brodeur_1982",
                                    date             = "1982-05-01",
                                    lat              = lat,
                                    lon              = lon,
                                    srid             = srid,
                                    description      = "Food web of coastal upwelling pelagic zone off Oregon and Washington",
                                    public           = TRUE,
                                    all_interactions = FALSE),
                brodeur_1983 = list(name             = "brodeur_1983",
                                    date             = "1983-05-01",
                                    lat              = lat,
                                    lon              = lon,
                                    srid             = srid,
                                    description      = "Food web of coastal upwelling pelagic zone off Oregon and Washington",
                                    public           = TRUE,
                                    all_interactions = FALSE),
                brodeur_1984 = list(name             = "brodeur_1984",
                                    date             = "1984-06-01",
                                    lat              = lat,
                                    lon              = lon,
                                    srid             = srid,
                                    description      = "Food web of coastal upwelling pelagic zone off Oregon and Washington",
                                    public           = TRUE,
                                    all_interactions = FALSE))


inter <- list(brodeur_1981 = list(taxon_1_level = "taxon",
                                  taxon_2_level = "taxon",
                                  date          = "1981-05-01",
                                  direction     = "directed",
                                  method        = "observation",
                                  description   = "null",
                                  public        = TRUE,
                                  lat           = lat,
                                  lon           = lon,
                                  srid          = srid),
              brodeur_1982 = list(taxon_1_level = "taxon",
                                  taxon_2_level = "taxon",
                                  date          = "1982-05-01",
                                  direction     = "directed",
                                  method        = "observation",
                                  description   = "null",
                                  public        = TRUE,
                                  lat           = lat,
                                  lon           = lon,
                                  srid          = srid),
              brodeur_1983 = list(taxon_1_level = "taxon",
                                  taxon_2_level = "taxon",
                                  date          = "1983-05-01",
                                  direction     = "directed",
                                  method        = "observation",
                                  description   = "null",
                                  public        = TRUE,
                                  lat           = lat,
                                  lon           = lon,
                                  srid          = srid),
              brodeur_1984 = list(taxon_1_level = "taxon",
                                  taxon_2_level = "taxon",
                                  date          = "1984-06-01",
                                  direction     = "directed",
                                  method        = "observation",
                                  description   = "null",
                                  public        = TRUE,
                                  lat           = lat,
                                  lon           = lon,
                                  srid          = srid))


#------------------------------
# Cleaning matrix
#------------------------------

# Open file

data_matrice <- paste0("Trophic-metacommunities-master/Trophic_metacom_meta_analysis/interaction matrices/",
                       food_web_name, ".csv") %>%
  map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>%
  map(~rename(.x, sp_id = X1)) %>%
  set_names(c("1981", "1982", "1983","1984"))
data_matrice[[1]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[2]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[3]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[4]][1,1] <- NA # Remove file identifier use  NA value

file_col_name <- data_matrice %>%
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~unname(.x)) %>%
  map(~str_remove_all(.x, "\\(|\\)"))

file_row_name <- data_matrice %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
  map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
  map(~unlist(.x)) %>%
  map(~unname(.x)) %>%
  map(~str_remove_all(.x, "\\(|\\)"))

FW_name <- data_matrice %>%
  map(~select(.x, -1)) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map2(file_row_name, ~mutate(.x, sp_taxon_1 = .y)) %>%
  map(~column_to_rownames(.x, "sp_taxon_1")) %>%
  modify_depth(~as.numeric(.x), .depth = 2) %>%
  map2(file_col_name, ~`names<-`(.x, .y)) %>%
  map(~rownames_to_column(.x, "sp_taxon_1")) %>%
  map(~gather(.x, "sp_taxon_2", "value", -sp_taxon_1)) %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2, value)) %>%
  map(~filter(.x, value != 0)) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Pp]roducers.*)")
                              , "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Dd]ebris.*)|(.*[Dd]etri*)"), "commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Cc]arcasse.*)"), "scavenger", .x$type)))

source("sep_pooled_many.R")
FW_name <- sep_pooled_many(FW_name, "(\\s-\\s)|(\\sand\\s)")

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data

taxa <- FW_name[[1]] %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
  map(~gather(.x, id, sp)) %>%
  modify(~deframe(.x)) %>%
  map(~unique(.x)) %>%
  map(~enframe(.x, name = NULL, value = "original_name"))

sp_name_for_this_web <- name_file %>%
  map(~select(.x, original_name, scientific_name)) %>%
  map(~mutate(.x, scientific_name = if_else(is.na(.x$scientific_name), original_name, scientific_name))) %>%
  map(~deframe(.x))

taxa_taxize <- FW_name[[2]] %>% # Use taxize to correct depooled species names
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
  map(~deframe(.x)) %>%
  compact() %>%
  flatten_chr() %>%
  .[!duplicated(.)]

taxa_taxize["Pteropods Limacina helicina"] <- "Limacina helicina"
taxa_taxize["Cnidarians Hydromedusae"] <- "Hydromedusae"
taxa_taxize["Scyphomedusae"] <- "Scyphomedusae"
taxa_taxize["Clionidae"] <- "Clionidae"
taxa_taxize["Copepods Neocalanus cristatus"] <- "Neocalanus cristatus"
taxa_taxize["N. plumchrus"] <- "Neocalanus plumchrus"
taxa_taxize["Polychaetes Tomopteris sp."] <- "Tomopteris"
taxa_taxize["Pelagagobia sp."] <- "Pelagagobia"

taxa_df <- taxa %>%
  map2(sp_name_for_this_web, ~mutate(.x, name_clear = str_replace_all(.x$original_name, .y))) %>%
  map(~mutate(.x, name_clear = str_replace_all(.x$name_clear, taxa_taxize))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$")))

FW_name[[2]] <- NULL
FW_name <- purrr::flatten(FW_name)

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

#------------------------------
# Writing taxa and interaction table
#------------------------------

# write.csv2(x         = taxa_back_df,
#            file      = paste0(getwd(), "/", folder_name, "/data/", folder_name, "_taxonomy.csv"),
#            row.names = FALSE)

if(is.null(names(taxa_df)) == TRUE){ # Control flow statement if there is multiple dataset in this paper.
  
  taxa_df %>%
    walk(~write.csv2(.x, 
                     file      = paste0(getwd(), "/", folder_name, "/data/", folder_name, "_node.csv"), 
                     row.names = FALSE))
  
  
}else{
  
  taxa_df %>%
    names() %>%
    walk(~write.csv2(x         = taxa_df[[.]],
                     file      = paste0(getwd(), "/", folder_name, "/data/", folder_name, "_", ., "_node.csv"),
                     row.names = FALSE))
  
}

if(is.null(names(FW_name)) == TRUE){
  
  FW_name %>%
    walk(~write.csv2(.x, 
                     file      = paste0(folder_name,"/data/",folder_name, "_inter.csv"), 
                     row.names = FALSE))
  
}else{
  
  FW_name %>%
    names() %>%
    walk(~write.csv2(FW_name[[.]], 
                     file      = paste0(folder_name,"/data/",folder_name, "_", ., "_inter.csv"),
                     row.names = FALSE))
}


# trait_df %>%
#   names() %>%
#   walk(~write.csv2(x = trait_df[[.]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_", ., "_trait.csv"), row.names = FALSE))
# 
# taxa_back_df <-  paste0("mangal-datasets/", folder_name, "/data/", folder_name, "_taxa_back.csv") %>%
#   read_csv2(col_types = cols("c", "d", "d", "d", "d"))
# 
# taxa_df <-  paste0("mangal-datasets/", folder_name, "/data/") %>%
#   dir_ls() %>%
#   as.character() %>%
#   str_subset(fixed("taxa.csv")) %>%
#   map(~read_csv2(.x, col_types = cols("c", "c")))
# 
# FW_name <-  paste0("mangal-datasets/", folder_name, "/data/") %>%
#   dir_ls() %>%
#   as.character() %>%
#   str_subset(fixed("inter.csv")) %>%
#   map(~read_csv2(.x, col_types = cols("c", "c", "i")))
# 
# trait_df <-  paste0("mangal-datasets/", folder_name, "/data/") %>%
#   dir_ls() %>%
#   as.character() %>%
#   str_subset(fixed("trait.csv")) %>%
#   map(~read_csv2(.x, col_types = cols("c", "c", "d")))


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
map(network,~POST_network(network_lst = .x, dataset = dataset, users = users, enviro = NULL))

## Taxonomy
POST_taxonomy(taxa_back_df)

## Node
map2(taxa_df, network, ~POST_node(.x, .y))


## Interaction
# map(FW_name, ~POST_interaction(.x, inter = inter, enviro = NULL, attr = attr_inter, users, network = network))
# l <- list(FW_name, inter, network)
# pmap(list(FW_name, inter, network), ~POST_interaction(inter_df = ..1, inter = ..2, enviro = NULL, attr = attr_inter, users = users, network = ..3))
for(i in 1:length(FW_name)){
  
  POST_interaction(inter_df = FW_name[[i]], inter = inter[[i]], attr = attr_inter, users = users, network = network[[i]])
  
}

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)