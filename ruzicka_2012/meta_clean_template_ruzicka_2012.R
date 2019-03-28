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
library(measurements)
library(mangal)

#------------------------------
# Metadata
#------------------------------

lat  <- c(42, 42, 48.34, 48.34, 42)
lon  <- c(-126, -124, -124, -126, -126)
srid <- 4326

folder_name <- "ruzicka_2012" # Name of the subfolder in mangal-datasets
food_web_name <- c("WEB320", "WEB321", "WEB322", "WEB323", "WEB324")

name_file <- read_csv("Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% paste0(food_web_name, ".csv")) %>%
  split(.$web)

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Dietary matrix",
                   table_owner = "interactions",
                   description = "Proportions of the consumer diets made up by the prey.",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi       = "10.1016/j.pocean.2012.02.002",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.1016%2Fj.pocean.2012.02.002",
             data_url  = "https://globalwebdb.com/",
             author    = "RD Brodeur",
             year      = "2012",
             bibtex    = "@article{Ruzicka_2012, doi = {10.1016/j.pocean.2012.02.002}, url = {https://doi.org/10.1016%2Fj.pocean.2012.02.002}, year = 2012, month = {sep}, publisher = {Elsevier {BV}}, volume = {102}, pages = {19--41}, author = {James J. Ruzicka and Richard D. Brodeur and Robert L. Emmett and John H. Steele and Jeannette E. Zamon and Cheryl A. Morgan and Andrew C. Thomas and Thomas C. Wainwright}, title = {Interannual variability in the Northern California Current food web structure: Changes in energy flow pathways and the role of forage fish, euphausiids, and jellyfish}, journal = {Progress in Oceanography}}")

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


dataset <- list(name        = "ruzicka_2012",
                 date        = "2012-01-01",
                 description = "Food web of the Northern California Current",
                 public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(list(name        = "2003_ruzicka_2012",
                     date        = "2003-06-01",
                     lat              = lat,
                     lon              = lon,
                     srid             = srid,
                     description      = "Food web of the Northern California Current in 2003",
                     public           = TRUE,
                     all_interactions = FALSE),
                list(name        = "2004_ruzicka_2012",
                     date        = "2004-06-01",
                     lat              = lat,
                     lon              = lon,
                     srid             = srid,
                     description      = "Food web of the Northern California Current in 2004",
                     public           = TRUE,
                     all_interactions = FALSE),
                list(name        = "2005_ruzicka_2012",
                     date        = "2005-06-01",
                     lat              = lat,
                     lon              = lon,
                     srid             = srid,
                     description      = "Food web of the Northern California Current in 2005",
                     public           = TRUE,
                     all_interactions = FALSE),
                list(name        = "2006_ruzicka_2012",
                     date        = "2006-06-01",
                     lat              = lat,
                     lon              = lon,
                     srid             = srid,
                     description      = "Food web of the Northern California Current in 2006",
                     public           = TRUE,
                     all_interactions = FALSE),
                list(name        = "2007_ruzicka_2012",
                     date        = "2007-06-01",
                     lat              = lat,
                     lon              = lon,
                     srid             = srid,
                     description      = "Food web of the Northern California Current in 2007",
                     public           = TRUE,
                     all_interactions = FALSE))


inter <- list(list(taxon_1_level = "taxon",
                   taxon_2_level = "taxon",
                   date          = "2003-06-01",
                   direction     = "directed",
                   method        = "model",
                   description   = "null",
                   public        = TRUE,
                   lat           = lat,
                   lon           = lon,
                   srid          = srid),
              list(taxon_1_level = "taxon",
                   taxon_2_level = "taxon",
                   date          = "2004-06-01",
                   direction     = "directed",
                   method        = "model",
                   description   = "null",
                   public        = TRUE,
                   lat           = lat,
                   lon           = lon,
                   srid          = srid),
              list(taxon_1_level = "taxon",
                   taxon_2_level = "taxon",
                   date          = "2005-06-01",
                   direction     = "directed",
                   method        = "model",
                   description   = "null",
                   public        = TRUE,
                   lat           = lat,
                   lon           = lon,
                   srid          = srid),
              list(taxon_1_level = "taxon",
                   taxon_2_level = "taxon",
                   date          = "2006-06-01",
                   direction     = "directed",
                   method        = "model",
                   description   = "null",
                   public        = TRUE,
                   lat           = lat,
                   lon           = lon,
                   srid          = srid),
              list(taxon_1_level = "taxon",
                   taxon_2_level = "taxon",
                   date          = "2007-06-01",
                   direction     = "directed",
                   method        = "model",
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
  set_names("2003", "2004", "2005", "2006", "2007")
data_matrice[[1]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[2]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[3]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[4]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[5]][1,1] <- NA # Remove file identifier use  NA value

file_col_name <- data_matrice %>%
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~unname(.x))

file_row_name <- data_matrice %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
  map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
  map(~unlist(.x)) %>%
  map(~unname(.x))

FW_name <- data_matrice %>%
  map(~select(.x, -1)) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map2(file_row_name, ~mutate(.x, sp_taxon_2 = .y)) %>%
  map(~column_to_rownames(.x, "sp_taxon_2")) %>%
  modify_depth(~as.numeric(.x), .depth = 2) %>%
  map2(file_col_name, ~`names<-`(.x, .y)) %>%
  map(~rownames_to_column(.x, "sp_taxon_2")) %>%
  map(~gather(.x, "sp_taxon_1", "value", -sp_taxon_2)) %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2, value)) %>%
  map(~filter(.x, !sp_taxon_1 %in% c("(1 - Sum)", "Sum"), !sp_taxon_2 %in% c("(1 - Sum)", "Sum"))) %>%
  map(~filter(.x, value != 0)) %>%
  map(~mutate(.x, sp_taxon_1 = str_replace_all(.x$sp_taxon_1, "&", "and"),
                  sp_taxon_2 = str_replace_all(.x$sp_taxon_2, "&", "and"))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Ss]ilicoflag.*)|
                                                           (.*[Pp]roducers.*)|(.*[Mm]acrocystis.*)|(.*[Pp]terygophora.*)|(.*[Ss]eaweed.*)|(Micro-epiphytes)|
                                                           (Macro-epiphytes)|(.*[Aa]scophyllum).*|(.*[Ee]nteromorpha.*)|(.*[Ff]ucus.*)|(.*[Uu]lva.*)|
                                                           (.*[Zz]ostera.*)"), "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Dd]ebris.*)|(.*[Dd]etritu*.(?!.*fish.*))|(DOM)|(Discard)|([Dd]issolved.*organic.*)"),
                                                          "commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Cc]arcasse.*)"), "scavenger", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Ii]mport.*)"), "unknown", .x$type)))

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data

taxa <- FW_name %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
  map(~gather(.x, id, sp)) %>%
  modify(~deframe(.x)) %>%
  map(~unique(.x)) %>%
  map(~enframe(.x, name = NULL, value = "original_name"))

sp_name_for_this_web <- name_file %>%
  map(~select(.x, original_name, scientific_name)) %>%
  map(~mutate(.x, scientific_name = if_else(is.na(.x$scientific_name), original_name, scientific_name))) %>% walk(~print(.x, n= Inf)) %>%
  map(~mutate(.x, original_name = str_replace_all(.x$original_name, "&", "and"),
                  scientific_name = str_replace_all(.x$scientific_name, "&", "and"))) %>%
  map(~deframe(.x))

taxa_df <- taxa %>%
  map2(sp_name_for_this_web, ~mutate(.x, name_clear = str_replace_all(.x$original_name, .y))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "^.*[:punct:]\\s"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "[:punct:]"))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$")))


## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- taxa_df %>%
  map(~unlist(.x$name_clear)) %>%
  map(~unname(.x)) %>%
  flatten_chr() %>%
  unique() %>%
  map_chr(~{modify_url(server, path = paste0("/api/v2/","taxonomy?name=", str_replace_all(.x, " ", "%20")))}) %>%
  map_chr(~str_replace_all(.x, ".*,%20.*", "_")) %>%
  map_chr(~str_replace_all(.x, ".*%20-%20.*", "-")) %>%
  map_chr(~str_replace_all(.x, "\\.%20", "__")) %>%
  keep(~length(content((GET(url = .x, config = add_headers("Content-type" = "application/json","Authorization" = paste("bearer", readRDS(".httr-oauth"))))))) == 0) %>%
  map_chr(~str_remove_all(.x, fixed("http://poisotlab.biol.umontreal.ca/api/v2/taxonomy?name="))) %>%
  map_chr(~str_replace_all(.x, fixed("%20"), " ")) %>%
  map_chr(~str_replace_all(.x, fixed("__"), ". ")) %>%
  map_chr(~str_replace_all(.x, fixed("_"), ". "))

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
#POST_taxonomy(taxa_back_df)

## Node
map2(taxa_df, network, ~POST_node(.x, .y))

## Interaction
# map(FW_name, ~POST_interaction(.x, inter = inter, enviro = NULL, attr = attr_inter, users, network = network))
# l <- list(FW_name, inter, network)
# pmap(list(FW_name, inter, network), ~POST_interaction(inter_df = ..1, inter = ..2, enviro = NULL, attr = attr_inter, users = users, network = ..3))
for(i in 1:length(FW_name)){
  
  POST_interaction(inter_df = FW_name[[i]], inter = inter[[i]], attr = attr_inter, users = users, network = network[[i]])
  cat(paste0(round(i/length(FW_name)*100, 2), " % terminé\n"))
}

network_id <- content(GET(url =paste0("http://poisotlab.biol.umontreal.ca/api/v2/network?dataset_id=80"))) %>% map_int(~.$id)
l <- list()
for(i in 1:length(network_id)){
  l[[i]] <- httr::DELETE(url = paste0("http://poisotlab.biol.umontreal.ca/api/v2/network/", network_id[i]), 
                         config = add_headers("Content-tpe" = "application/json","Authorization" = paste("bearer", readRDS(".httr-oauth"))))
}
map(l, ~.x$status_code)

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)