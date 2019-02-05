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

lat  <- IMPRECIS
lon  <- -124.5
srid <- 4326

folder_name <- "ruzicka_2012" # Name of the subfolder in mangal-datasets
food_web_name <- c("WEB320", "WEB321", "WEB322", "WEB323", "WEB324")

name_file <- read_csv("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% food_web_name) %>%
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
                 date        = "2003-05-01",
                 description = "Food web of the Northern California Current",
                 public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(name        = "ruzicka_2012",
                 date        = "2003-05-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Food web of the Northern California Current",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2003-05-01",
              direction     = "direct",
              # type          = "predation",
              method        = "null",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
# Cleaning matrix
#------------------------------

# Open file

## Species file
# data_file <- paste0("mangal-datasets/", folder_name, "/raw/") %>% #Getting all the file into one list
#   dir_ls() %>%
#   as.character() %>%
#   map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>% #Read all file
#   map(~slice(.x, 1:(nrow(.x)-2))) %>%
#   set_names(c("2003", "2004", "2005", "2006", "2007"))
# data_file[[1]][1,1] <- NA_character_
# data_file[[2]][1,1] <- NA_character_
# data_file[[3]][1,1] <- NA_character_
# data_file[[4]][1,1] <- NA_character_
# data_file[[5]][1,1] <- NA_character_

data_matrice <- paste0("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/interaction matrices/",
                       food_web_name, ".csv") %>%
  map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>%
  map(~rename(.x, sp_id = X1)) %>%
  set_names("2003", "2004", "2005", "2006", "2007")
data_matrice[[1]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[2]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[3]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[4]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[5]][1,1] <- NA # Remove file identifier use  NA value

# file_col_name <- data_file %>% # Saving column sp name of all file
#   map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
#   map(~select(.x, -1)) %>% # Remove the column sp id to convert
#   map(~unname(unlist(.x)))

file_col_name <- data_matrice %>%
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~unname(.x))

# file_row_name <- data_file %>%
#   map(~select(.x, 1)) %>%
#   map(~slice(.x, 2:nrow(.x))) %>%
#   map(~unname(unlist(.x)))

file_row_name <- data_matrice %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
  map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
  map(~unlist(.x)) %>%
  map(~unname(.x))

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
  map(~filter(.x, !sp_taxon_1 %in% c("(1 - Sum)", "Sum"), !sp_taxon_2 %in% c("(1 - Sum)", "Sum"))) %>%
  map(~filter(.x, value != 0)) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Ss]ilicoflag.*)|
                                                           (.*[Pp]roducers.*)|(.*[Mm]acrocystis.*)|(.*[Pp]terygophora.*)|(.*[Ss]eaweed.*)|(Micro-epiphytes)|
                                                           (Macro-epiphytes)|(.*[Aa]scophyllum).*|(.*[Ee]nteromorpha.*)|(.*[Ff]ucus.*)|(.*[Uu]lva.*)|
                                                           (.*[Zz]ostera.*)"), "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Dd]ebris.*)|(.*[Dd]etritu*.(?!.*fish.*))|(DOM)|(Discard)|([Dd]issolved.*organic.*)"),
                                                          "commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Cc]arcasse.*)"), "scavenger", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Ii]mport.*)"), "unknown", .x$type)))

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
  map(~mutate(.x, scientific_name = if_else(is.na(.x$scientific_name), original_name, scientific_name))) %>%
  map(~deframe(.x))

taxa_df <- taxa %>%
  map2(sp_name_for_this_web, ~mutate(.x, name_clear = str_replace_all(.x$original_name, .y))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "^.*[:punct:]\\s"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "[:punct:]"))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$")))

# taxa <- FW_name %>%
#   map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
#   map(~gather(.x, id, sp)) %>%
#   modify(~deframe(.x)) %>%
#   map(~unique(.x)) %>%
#   map(~enframe(.x, name = NULL, value = "sp")) %>%
#   map(~mutate(.x, id = row_number())) %>% # Create an id to reorder the tab
#   map(~mutate(.x, letter = NA_character_))
# taxa$`2003`$letter <- rep("A", times = length(taxa$`2003`$letter))
# taxa$`2004`$letter <- rep("B", times = length(taxa$`2004`$letter))
# taxa$`2005`$letter <- rep("C", times = length(taxa$`2005`$letter))
# taxa$`2006`$letter <- rep("D", times = length(taxa$`2006`$letter))
# taxa$`2007`$letter <- rep("E", times = length(taxa$`2007`$letter))
# 
# 
# taxa <- taxa %>%
#   map(~mutate(.x, id = as.character(.x$id))) %>%
#   map(~unite(.x, id_letter, id, letter))

## Checking taxa and creating taxa_df

# taxa_corrected <- taxa %>%
#   map(~mutate(.x, sp = str_remove_all(.x$sp, "\\s\\(.*\\)$"))) %>% # Correct vernacular name at the lowest level possible (species)
#   map(~mutate(.x, sp = str_remove_all(.x$sp, "^.*:\\s"))) %>%
#   map(~mutate(.x, sp = str_replace_all(.x$sp, c("large phytoplankton" = "Phytoplankton", 
#                                                 "small phytoplankton" = "Phytoplankton", "micro-zooplankton" = "Zooplankton", 
#                                                 "copepods" = "Copepoda", "small jellyfish" = "Medusozoa", 
#                                                 "large jellyfish" = "Medusozoa", "pteropods" = "Pteropoda", 
#                                                 "pelagic amphipods" = "Amphipoda", "pelagic shrimp" = "Decapoda", "other macro-zooplankton" = "Zooplankton", 
#                                                 "E. pacifica" = "Euphausia pacifica", "T. spinifera" = "Thysanoessa spinifera",
#                                                 "mysids" = " Mysida", "cephalopod aggregate" = "Cephalopoda", "smelt aggregate" = "Osmeridae",
#                                                 "shad" = "Alosinae", "sardine" = " Clupeidae", "herring" = "Clupeidae", "anchovy" = "Engraulidae", 
#                                                 "coho yearling" = "Oncorhynchus tshawytscha", "Chinook subyearling" = "Oncorhynchus tshawytscha",
#                                                 "Chinook yearling" = "Oncorhynchus tshawytscha", "other juvenile salmon" = "Salmonidae",
#                                                 "other epibenthic shrimp" = "Decapoda", "American shad" = "Alosa sapidissima",
#                                                 "benthic amphipods, isopods, and cumaceans" = "Malacostraca", "coho" = "Oncorhynchus kisutch",
#                                                 "Chinook" = "Oncorhynchus tshawytscha", "other salmon aggregate" = "Salmonidae", "saury" = "Cololabis adocetus",
#                                                 "hake" = "Merlucciidae", "jack mackerel" = "Trachurus", "Pacific mackerel" = "Scomber australasicus", "dogfish aggregate" = "Squalidae",
#                                                 "flatfish" = "Pleuronectiformes", "skates & rays" = "Rajidae", "echinoderms" = "Echinodermata", "bivalves" = "Bivalvia", 
#                                                 "Dungeness crab" = "Metacarcinus magister", "Tanner crab" = "Chionoecetes", "grenadier" = "Punica granatum", "sablefish" = "Anoplopoma fimbria",
#                                                 "common murre" = "Uria aalge", "gulls & terns" = "Laridae", "shark aggregate" = "Selachii", 
#                                                 "sooty shearwaters" = "Ardenna grisea", "large pelagic seabirds" = "Aves", "other pelagic seabirds" = "Aves",
#                                                 "alcids" = "Alcidae", "storm-petrels" = "Hydrobatidae", "gray whales" = "Eschrichtius robustus", "baleen whales" = "Mysticeti",
#                                                 "small pinnipeds" = "Pinnipedia", "large pinnipeds" = "Pinnipedia", "toothed whales" = "Odontoceti"))))
# 
# taxa_checked <- taxa_corrected %>%
#   map(~.x$sp) %>%
#   map(~gnr_resolve(.x, canonical = FALSE, best_match_only = T)) # Check for wrong names
# taxa_name_not_known <- taxa_checked %>%
#   map(~attributes(.x)$not_known) %>% # Names not recognized by Global Names Recognition and Discovery.
#   map(~enframe(.x, name = NULL, value = "user_supplied_name")) %>% # Create a df simillar to sp_name_cleaned to bind it whith sp_name_cleaned
#   map(~mutate(.x, submitted_name = paste0(str_to_upper(str_extract(.x$user_supplied_name, ".{1}")), str_remove(.x$user_supplied_name, ".{1}")))) %>%
#   map(~mutate(.x, matched_name = submitted_name,
#               data_source_title = NA_character_,
#               score = NA_real_))
# 
# taxa_df <-  taxa_checked %>%
#   map2(taxa_name_not_known, ~bind_rows(.x, .y)) %>%
#   map(~select(.x, user_supplied_name, matched_name)) %>%
#   map2(taxa_corrected, ~left_join(.x, .y, by = c("user_supplied_name" = "sp"))) %>%
#   map2(taxa, ~left_join(.x, .y, by = "id_letter")) %>%
#   map(~filter(.x, !duplicated(.x$sp))) %>%
#   map(~select(.x, sp, matched_name)) %>%
#   map(~`names<-`(.x, c("original_name", "name_clear")))
# 
# taxa_df$`2003`$name_clear[7] <- "Small invertebrate larvae"
# taxa_df$`2003`$name_clear[31] <- "Misc. epifauna"
# taxa_df$`2003`$name_clear[32] <- "Misc. epifauna"
# taxa_df$`2003`$name_clear[34] <- "Misc. small benthic fishes"
# 
# taxa_df$`2004`$name_clear[7] <- "Small invertebrate larvae"
# taxa_df$`2004`$name_clear[32] <- "Misc. epifauna"
# taxa_df$`2004`$name_clear[33] <- "Misc. epifauna"
# taxa_df$`2004`$name_clear[35] <- "Misc. small benthic fishes"
# 
# taxa_df$`2005`$name_clear[7] <- "Small invertebrate larvae"
# taxa_df$`2005`$name_clear[29] <- "Misc. epifauna"
# taxa_df$`2005`$name_clear[30] <- "Misc. epifauna"
# taxa_df$`2005`$name_clear[32] <- "Misc. small benthic fishes"
# 
# taxa_df$`2006`$name_clear[7] <- "Small invertebrate larvae"
# taxa_df$`2006`$name_clear[32] <- "Misc. epifauna"
# taxa_df$`2006`$name_clear[33] <- "Misc. epifauna"
# taxa_df$`2006`$name_clear[35] <- "Misc. small benthic fishes"
# 
# taxa_df$`2007`$name_clear[7] <- "Small invertebrate larvae"
# taxa_df$`2007`$name_clear[31] <- "Misc. epifauna"
# taxa_df$`2007`$name_clear[32] <- "Misc. epifauna"
# taxa_df$`2007`$name_clear[34] <- "Misc. small benthic fishes"

## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- taxa_df %>%
  map(~unlist(.x$name_clear)) %>%
  map(~unname(.x)) %>%
  flatten_chr() %>%
  unique() %>%
  map_chr(~{modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", str_replace_all(.x, " ", "%20")))}) %>%
  map_chr(~str_replace_all(.x, ",%20", "_")) %>%
  map_chr(~str_replace_all(.x, "%20-%20", "-")) %>%
  map_chr(~str_replace_all(.x, "\\.%20", "__")) %>%
  keep(~length(content((GET(url = .x, config = add_headers("Content-type" = "application/json","Authorization" = paste("bearer", readRDS("mangal-datasets/.httr-oauth"))))))) == 0) %>%
  map_chr(~str_remove_all(.x, fixed("http://poisotlab.biol.umontreal.ca/api/v2/taxonomy/?name="))) %>%
  map_chr(~str_replace_all(.x, fixed("%20"), " ")) %>%
  map_chr(~str_replace_all(.x, fixed("__"), ". ")) %>%
  map_chr(~str_replace_all(.x, fixed("_"), ", ")) %>%
  map_chr(~str_replace_all(.x, fixed("-"), " - "))


taxa_back_df <- taxa_back_df %>%
  enframe(name = NULL, value = "name") %>%
  mutate(bold = as.double(unlist({map(.$name,~get_boldid(.x, row = 5, verbose = FALSE)[1])})),
         eol = NA_real_, #Add NA in eol column : See taxize issue : #718 EOL: maybe completely remove the data source from taxize
         tsn = as.double(unlist({map(.$name,~get_tsn(.x, row = 5, verbose = FALSE)[1])})),
         ncbi = as.double(unlist({map(.$name,~get_uid(.x, row = 5, verbose = FALSE)[1])}))) 


#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_taxa_back.csv"), row.names = FALSE)

if(is.null(names(taxa_df)) == TRUE){
  
  taxa_df %>%
    walk(~write.csv2(x = taxa_df, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_taxa.csv"), row.names = FALSE))
  
}else{
  
  taxa_df %>%
    names() %>%
    walk(~write.csv2(x = taxa_df[[.]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_", ., "_taxa.csv"), row.names = FALSE))
  
}

if(is.null(names(FW_name)) == TRUE){
  
  FW_name %>%
    walk(~write.csv2(x = FW_name, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_inter.csv"), row.names = FALSE))
  
}else{
  
  FW_name %>%
    names() %>%
    walk(~write.csv2(x = FW_name[[.]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_", ., "_inter.csv"), row.names = FALSE))
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

POST_attribute(attr_inter)

# POST_attribute(attr1)
# POST_attribute(attr2)

POST_ref(ref)
POST_user(users)

# POST_environment(enviro, attr_##)

POST_dataset(dataset, users, ref)

# POST_network(network_lst = , enviro = enviro, dataset, users)
# POST_network(network_lst = inter, dataset, users) # Work
map(network, ~POST_network(network_lst = .x, dataset = dataset, users = users))


POST_taxa_back(taxa_back_df)
# POST_taxon(taxa_df) # Work
map(taxa_df, ~POST_taxon)

# POST_traits(trait_df, network)

# POST_interaction(inter_df = FW_name[[1]], inter = inter, enviro = enviro, attr = attr_inter, users)
# POST_interaction(inter_df = FW_name[[1]], inter = inter, attr = attr_inter, users) # work
map2(FW_name, inter, ~POST_interaction(inter_df = .x, inter = .y, attr = attr_inter, users))
rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)