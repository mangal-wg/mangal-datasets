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

lat  <- 42.605557
lon  <- -70.665625
srid <- 4326

folder_name <- "dexter_1947" # Name of the subfolder in mangal-datasets
food_web_name <- c("WEB51", "WEB110", "WEB111", "WEB112", "WEB113")

name_file <- read_csv("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% food_web_name) %>%
  split(.$web)

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "DESCRIPTION",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

ref <- list(doi       = "10.2307/1948658",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.2307%2F1948658",
             data_url  = "https://globalwebdb.com/",
             author    = "Ralph W. Dexter",
             year      = "1947",
             bibtex    = "@inbook{Copeland_1974, Address = {Washington, D.C.}, Author = {Odum, Howard T. and McMahan, E. A and Copeland, B. J. and United States. and Conservation Foundation}, Chapter = {Oligohaline regime}, Number = {IV.C-8}, Pages = {315-357}, Publisher = {Conservation Foundation in cooperation with National Oceanic and Atmospheric Administration, Office of Coastal Environment}, Title = {Coastal ecological systems of the United States}, url = {http://hdl.handle.net/2027/uc1.31822000610949}, Volume = {II}, Year = {1974}}")


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


dataset <- list(name        = "dexter_1947",
                 date        = "1933-06-01",
                 description = "Food web of the tidal inlet at Cape Ann",
                 public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(name        = "dexter_1947",
                 date        = "1933-06-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Food web of the tidal inlet at Cape Ann",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "1933-06-01",
              direction     = "directed",
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

# sp_name <- paste0("mangal-datasets/",folder_name,"/raw/species") %>%
#   dir_ls() %>%
#   as.character() %>%
#   map(~read_table(.x, skip  = 0, col_names = TRUE, col_type = cols(.default = col_character()), na = "")) %>%
#   map(~select_if(.x, ~!sum(is.na(.x)) >2)) %>%
#   map(~slice(.x, 1:nrow(.x)-1)) %>%
#   map(~select(.x, -1)) %>%
#   map(~unlist(.x)) %>%
#   map(~unname(.x)) %>%
#   map(~as.factor(.x)) %>%
#   map(~fct_inorder(.x)) %>%
#   set_names(c("High salt marsh", "Low salt marsh", "Mudflat", "Rocky shore", "Shallow sublittoral"))
# 
# data_matrice  <- paste0("mangal-datasets/",folder_name,"/raw/food_web") %>%
#   dir_ls() %>%
#   as.character() %>%
#   map(~read_table(.x, skip  = 0, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>%
#   map(~select_if(.x, ~!sum(is.na(.x)) > 2)) %>%
#   map(~filter(.x, !is.na(X5))) %>%
#   map(~rename(.x, sp_id = 1)) %>%
#   map(~mutate(.x, sp_id = str_replace(.x$sp_id, fixed("113"), NA_character_))) %>%
#   map(~mutate(.x, sp_id = str_replace(.x$sp_id, fixed("112"), NA_character_))) %>%
#   map(~mutate(.x, sp_id = str_replace(.x$sp_id, fixed("111"), NA_character_))) %>%
#   map(~mutate(.x, sp_id = str_replace(.x$sp_id, fixed("110"), NA_character_))) %>%
#   map(~mutate(.x, sp_id = str_replace(.x$sp_id, fixed("51"), NA_character_))) %>%
#   set_names(c("High salt marsh", "Low salt marsh", "Mudflat", "Rocky shore", "Shallow sublittoral"))

data_matrice <- paste0("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/interaction matrices/",
                       food_web_name, ".csv") %>%
  map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>%
  map(~rename(.x, sp_id = X1)) %>%
  set_names("Shallow sublittoral", "Rocky shore", "Mudflat", "Low salt marsh", "High salt marsh")
data_matrice[[1]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[2]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[3]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[4]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[5]][1,1] <- NA # Remove file identifier use  NA value

# data_col_name <- data_matrice %>%
#   map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
#   modify(~select(.x, -1)) %>% # Remove the column sp id to convert
#   map(~unlist(.x)) %>% # Breaking each df into vector
#   map(~as.integer(.x)) %>% # Coerce to integer linking it to sp_name
#   map2(sp_name, ~{.x <- levels(.y)[.x]})

data_col_name <- data_matrice %>%
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~unname(.x))

# data_row_name <- data_matrice %>%
#   map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
#   map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
#   map(~unlist(.x)) %>% # Breaking each df into vector
#   map(~as.integer(.x)) %>% # Coerce to integer linking it to sp_name
#   map2(sp_name, ~{.x <- levels(.y)[.x]}) # Apply the correct level to the corect sp number.

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
  map(~rownames_to_column(.x, "sp_taxon_1")) %>%
  map(~gather(.x, "sp_taxon_2", "value", -sp_taxon_1)) %>% #Convert large df to long format
  map(~filter(.x, value != 0)) %>% # Remove 0 interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Ss]ilicoflag.*)|(.*[Pp]roducers.*)")
                              , "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(organic\\sdebris)"), "commensalism", .x$type))) %>%
  # map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(organic\\sdebris)|(.*[Dd]etri*)"), "commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_1, "(.*[Cc]arcasse.*)"), "scavenger", .x$type)))

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
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$")))

# taxa <- FW_name %>%
#   map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
#   map(~gather(.x, id, sp)) %>%
#   modify(~deframe(.x)) %>%
#   map(~unique(.x)) %>%
#   map(~enframe(.x, name = NULL, value = "sp")) %>%
#   map(~mutate(.x, id = row_number())) # Create an id to reorder the tab
# 
# taxa$`High salt marsh`$letter <- rep("A", times = length(taxa$`High salt marsh`$id))
# taxa$`Low salt marsh`$letter <- rep("B", times = length(taxa$`Low salt marsh`$id))
# taxa$Mudflat$letter <- rep("C", times = length(taxa$Mudflat$id))
# taxa$`Rocky shore`$letter <- rep("D", times = length(taxa$`Rocky shore`$id))
# taxa$`Shallow sublittoral`$letter <- rep("E", times = length(taxa$`Shallow sublittoral`$id))
# 
# taxa <- taxa %>%
#   map(~mutate(.x, id = as.character(.x$id))) %>%
#   map(~unite(.x, id_letter, id, letter))
# 
# ## Checking taxa and creating taxa_df
# 
# taxa_corrected <- taxa %>%
#   map(~mutate(.x, sp = str_remove_all(.x$sp, "\\s\\(.*\\)$"))) %>% # Correct vernacular name at the lowest level possible (species)
#   map(~mutate(.x, sp = str_replace_all(.x$sp, c("Chalina" = "Haliclona", "Lichenophora" = "Lichenopora", 
#                                                 "annelids" = "Annelida", "Crago" = "Crangon"))))
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
#   map(~select(.x, sp, matched_name)) %>%
#   map(~`names<-`(.x, c("original_name", "name_clear")))
# 
# taxa_df[[1]]$name_clear[2] <- "Organic debris"
# taxa_df[[1]]$name_clear[5] <- "Melampus, Littorina littorea, L. saxatilis"
# taxa_df[[1]]$name_clear[6] <- "Orchestia, Philoscia, Anurida, Cylisticus"
# taxa_df[[1]]$name_clear[7] <- "Fundulus, fish fry"
# taxa_df[[1]]$name_clear[8] <- "Corvus, Pisobia"
# taxa_df[[2]]$name_clear[2] <- "Ascophyllum, Fucus, other algae"
# taxa_df[[2]]$name_clear[3] <- "Plankton, detritus"
# taxa_df[[2]]$name_clear[4] <- "Organic debris"
# taxa_df[[2]]$name_clear[5] <- "Littorina littorea, L. obtusata, L. saxatilis"
# taxa_df[[2]]$name_clear[6] <- "Mytilus, Balanus"
# taxa_df[[2]]$name_clear[7] <- "Anurida, Orchestia, Gammarus"
# taxa_df[[2]]$name_clear[8] <- "Carcinides, Cancer"
# taxa_df[[2]]$name_clear[11] <- "Fish fry"
# taxa_df[[2]]$name_clear[12] <- "Butorides, Corvus"
# taxa_df[[3]]$name_clear[2] <- "Organic debris"
# taxa_df[[3]]$name_clear[3] <- "Plankton and detritus"
# taxa_df[[3]]$name_clear[4] <- "Chaetomorpha, Ulva, other algae"
# taxa_df[[3]]$name_clear[5] <- "Lumbrinereis, Clymenella"
# taxa_df[[3]]$name_clear[6] <- "Mya, Ensis, Macoma, Solemya"
# taxa_df[[3]]$name_clear[7] <- "Fundulus, fish fry"
# taxa_df[[3]]$name_clear[9] <- "Littorina littorea, Onoba"
# taxa_df[[3]]$name_clear[10] <- "Gammarus, Talorchestia"
# taxa_df[[3]]$name_clear[11] <- "Glycera, Nereis, Cerebratulus"
# taxa_df[[3]]$name_clear[13] <- "Polinices, Nassarius"
# taxa_df[[3]]$name_clear[15] <- "Megaceryle, Sterna"
# taxa_df[[3]]$name_clear[17] <- "Pagurus, Cancer, Carcinides, Myoxocephalus"
# taxa_df[[3]]$name_clear[18] <- "Tautogolabrus, Pseudopleuronectes"
# taxa_df[[4]]$name_clear[1] <- "Ascophyllum, Fucus, Ulva, other algae"
# taxa_df[[4]]$name_clear[2] <- "Plankton, suspended detritus"
# taxa_df[[4]]$name_clear[3] <- "Organic debris"
# taxa_df[[4]]$name_clear[4] <- "Mytilus, Balanus"
# taxa_df[[4]]$name_clear[5] <- "Sertularia, Metridium, Obelia, Clava"
# taxa_df[[4]]$name_clear[7] <- "Gammarus, Orchestia"
# taxa_df[[4]]$name_clear[8] <- "Acmaea, Crepidula"
# taxa_df[[4]]$name_clear[9] <- "Littorina littorea, L. obtusata, L. saxatilis"
# taxa_df[[4]]$name_clear[10] <- "Thais, Asterias"
# taxa_df[[4]]$name_clear[11] <- "Carcinides, Cancer"
# taxa_df[[4]]$name_clear[13] <- "Myoxocephalus, Tautogolabrus, Fundulus"
# taxa_df[[5]]$`name_clear`[1] <- "Plankton and detritus" # Manually editing some wrong name added by Global Names Recognition and Discovery.
# taxa_df[[5]]$`name_clear`[2] <- "Organic debris"
# taxa_df[[5]]$`name_clear`[4] <- "Fundulus, fish fry"
# taxa_df[[5]]$`name_clear`[6] <- "Mytilus, Gemma"
# taxa_df[[5]]$`name_clear`[7] <- "Abietinaria, Sertularia, Metridium"
# taxa_df[[5]]$`name_clear`[14] <- "Scomber, Clupea"
# taxa_df[[5]]$`name_clear`[17] <- "Pagurus, Cancer"
# taxa_df[[5]]$`name_clear`[18] <- "Pomatomus, Poronatus"

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