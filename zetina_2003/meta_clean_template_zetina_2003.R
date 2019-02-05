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

lat  <- 22.985307
lon  <- -106.10993
srid <- 4326

folder_name <- "zetina_2003" # Name of the subfolder in mangal-datasets
food_web_name <- "WEB267"

name_file <- read_csv("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv", 
                      col_type = cols(.default = col_character()))
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

ref <- list(doi       = "10.1016/s0272-7714(02)00410-9",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://doi.org/10.1016%2Fs0272-7714%2802%2900410-9",
             data_url  = "https://globalwebdb.com/",
             author    = "Manuel J. Zetina-Rejon",
             year      = "2003",
             bibtex    = "@article{Zetina_Rej_n_2003, doi = {10.1016/s0272-7714(02)00410-9}, url = {https://doi.org/10.1016%2Fs0272-7714%2802%2900410-9}, year = 2003, month = {aug}, publisher = {Elsevier {BV}}, volume = {57}, number = {5-6}, pages = {803--815}, author = {Manuel J. Zetina-Rej{\'{o}}n and Francisco Arregu{\i}{\'}n-S{\'{a}}nchez and Ernesto A. Ch{\'{a}}vez}, title = {Trophic structure and flows of energy in the Huizache{\textendash}Caimanero lagoon complex on the Pacific coast of Mexico},journal = {Estuarine, Coastal and Shelf Science}}")

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


dataset <- list(name        = "zetina_2003",
                 date        = "2003-01-01",
                 description = "Dietary matrix of the Huizache–Caimanero lagoon",
                 public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(name        = "zetina_2003",
                 date        = "2003-01-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Dietary matrix of the Huizache–Caimanero lagoon",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(taxon_1_level = "taxon",
              taxon_2_level = "taxon",
              date          = "2003-01-01",
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
#   map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) #Read all file

data_file <- paste0("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/interaction matrices/",
                       food_web_name, ".csv") %>%
  map(~read_csv(.x, skip  = 1, col_names = FALSE, col_type = cols(.default = col_character()), na = "")) %>%
  map(~rename(.x, sp_id = X1))
data_file[[1]][1,1] <- NA # Remove file identifier use  NA value

# file_col_name <- data_file %>% # Saving column sp name of all file
#   map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
#   map(~select(.x, -1)) %>% # Remove the column sp id to convert
#   map(~unname(unlist(.x))) 

file_col_name <- data_file %>%
  map(~slice(.x,1)) %>% # Select first line of each df to create column with name of species
  modify(~select(.x, -1)) %>% # Remove the column sp id to convert
  map(~unlist(.x)) %>% # Breaking each df into vector
  map(~unname(.x))

# file_row_name <- data_file %>%
#   map(~select(.x, 1)) %>%
#   map(~slice(.x, 2:nrow(.x))) %>%
#   map(~unname(unlist(.x)))

file_row_name <- data_file %>%
  map(~filter(.x, !is.na(select(.x,1)))) %>% # Remove the line containing all sp id
  map(~select(.x, 1)) %>%  # Select the column containing the species integer in its row
  map(~unlist(.x)) %>%
  map(~unname(.x))

FW_name <- data_file %>%
  map(~select(.x, -1)) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map2(file_row_name, ~mutate(.x, sp_taxon_2 = .y)) %>%
  map(~column_to_rownames(.x, "sp_taxon_2")) %>%
  modify_depth(~as.numeric(.x), .depth = 2) %>%
  map2(file_col_name, ~`names<-`(.x, .y)) %>%
  map(~rownames_to_column(.x, "sp_taxon_2")) %>%
  map(~gather(.x, "sp_taxon_1", "value", -sp_taxon_2)) %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2, value)) %>%
  map(~filter(.x, value != 0)) %>%
map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Pp]roducers.*)")
                              , "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Dd]ebris.*)|(.*[Dd]etri*)"), "commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Cc]arcasse.*)"), "scavenger", .x$type)))
#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
# taxa <- FW_name %>%
#   map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
#   map(~gather(.x, id, sp)) %>%
#   modify(~deframe(.x)) %>%
#   map(~unique(.x)) %>%
#   map(~enframe(.x, name = NULL, value = "original_name"))

taxa <- FW_name %>%
  map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
  map(~gather(.x, id, sp)) %>%
  modify(~deframe(.x)) %>%
  map(~unique(.x)) %>%
  map(~enframe(.x, name = NULL, value = "original_name"))

## Checking taxa and creating taxa_df

sp_name_for_this_web <- name_file %>%
  filter(web == food_web_name) %>%
  select(original_name, scientific_name) %>%
  mutate(scientific_name = if_else(is.na(.$scientific_name), original_name, scientific_name)) %>%
  deframe()

taxa_df <- taxa %>%
  map(~mutate(.x, name_clear = str_replace_all(.x$original_name, sp_name_for_this_web))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$")))

# sp_name <- FW_name %>%
#   map(~select(.x, sp_taxon_1, sp_taxon_2)) %>%
#   map(~gather(.x, id, sp)) %>%
#   map(~select(.x, -1)) %>%
#   unlist() %>%
#   unname() %>%
#   unique() %>%
#   str_replace_all(c("Scianids" = "Cynoscion xanthulum", "Elopids" = "Elops affinis", "Lutjanids" = "Lutjanus novemfasciatus",
#                     "Carangids" = "Carangidae", "Centropomids" = "Centropomidae", "Ariids" = "Arius guatemalensis", 
#                     "Haemulids" = "Haemulidae", "Pleuronectoids" = "Cynoglossus zanzibarensis", "Callinectes" = "Callinectes arcuatus",
#                     "Belonoids" = "Belonoidei", "Clupeoids" = "Clupeoidei", "Gerreids" = "Gerreidae", "Poeciliids" = "Poeciliidae",
#                     "Gobioids" = "Dormitator latrifons", "Mugilids" = "Mugil cephalus", "Palaemonids" = "Macrobranchium",
#                     "Litopenaeus" = "Penaeus", "Bivalves" = "Bivalvia", "Chanids" = "Chanos chanos", "Polychaetes" = "Polychaeta",
#                     "Gastropods" = "Gastropoda"))
# 
# sp_name_checked <- sp_name %>%
#   gnr_resolve(canonical = FALSE, best_match_only = T)
# 
# sp_name_not_known <- attributes(sp_name_checked)$not_known %>% # Names not recognized by Global Names Recognition and Discovery.
#   enframe(name = NULL, value = "user_supplied_name") %>% # Create a df simillar to sp_name_cleaned to bind it whith sp_name_cleaned
#   mutate(., submitted_name = paste0(str_to_upper(str_extract(.$user_supplied_name, ".{1}")), str_remove(.$user_supplied_name, ".{1}"))) %>%
#   mutate(matched_name = submitted_name,
#          data_source_title = NA_character_,
#          score = NA_real_)
# 
# taxa_df_global <- sp_name_checked %>% # Taxa resolved
#   bind_rows(sp_name_not_known) %>% # Taxa not resolved
#   select(user_supplied_name, matched_name) %>% # Select only two column of interest
#   left_join(enframe(sp_name, name = NULL, value = "sp"), by = c("user_supplied_name" = "sp")) %>% # Join the table without taxa resolved
#   `names<-`(c("original_name", "name_clear"))
# 
# taxa_df_global$name_clear <- taxa_df_global$name_clear %>%
#   str_remove_all("\\ssp.*$") %>%
#   str_replace_all(fixed(". "), "_") %>%
#   str_remove_all(fixed("."))
# 
# taxa_df_global$name_clear[14] <- "Dormitator latrifons"
# 
# taxa_df <- taxa %>%
#   modify(~inner_join(.x, taxa_df_global, by = "original_name"))

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
  keep(~length(content((GET(url = .x, config = add_headers("Content-type" = "application/json","Authorization" = paste("bearer", readRDS("mangal-datasets/.httr-oauth"))))))) == 0) %>%
  map_chr(~str_remove_all(.x, fixed("http://poisotlab.biol.umontreal.ca/api/v2/taxonomy/?name="))) %>%
  map_chr(~str_replace_all(.x, fixed("%20"), " ")) %>%
  map_chr(~str_replace_all(.x, fixed("__"), ". ")) %>%
  map_chr(~str_replace_all(.x, fixed("_"), ", "))

taxa_back_df <- taxa_back_df %>%
  enframe(name = NULL, value = "name") %>%
  mutate(bold = as.double(unlist({map(.$name,~get_boldid(.x, row = 5, verbose = FALSE)[1])})),
         eol = NA_real_, #Add NA in eol column : See taxize issue : #718 EOL: maybe completely remove the data source from taxize
         tsn = as.double(unlist({map(.$name,~get_tsn(.x, row = 5, verbose = FALSE)[1])})),
         ncbi = as.double(unlist({map(.$name,~get_uid(.x, row = 5, verbose = FALSE, key = "679d0a26947d9b6432371b268ec0c7b39b08")[1])}))) # Add API KEy for NCBI

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_taxa_back.csv"), row.names = FALSE)
write.csv2(x = taxa_df[[1]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_taxa.csv"), row.names = FALSE)
write.csv2(x = FW_name[[1]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_inter.csv"), row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

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
POST_network(network_lst = , enviro = enviro, dataset, users)
POST_taxa_back(taxa_back)
POST_taxon(taxa_df)
# POST_traits(trait_df, network)
POST_interaction(inter_df = FW_name[[1]], inter = inter, enviro = enviro, attr = attr_inter, users)

rm(lat, lon, srid, attr_inter, ref, users, enviro, dataset, trait, network, inter, taxa_df, taxa_back_df, FW_name)