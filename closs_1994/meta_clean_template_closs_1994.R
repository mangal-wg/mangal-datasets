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

lat  <- -37.615317
lon  <- 144.281862
srid <- 4326

folder_name <- "closs_1994" # Name of the subfolder in mangal-datasets
food_web_name <- paste0("WEB", 297:308) # Name of the dataset in Trophic metacommunities-master/Trophic_metacom_meta_analysis/Data

name_file <- read_csv("Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% paste0(food_web_name, ".csv")) %>%
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

ref <- list(doi       = "10.2307/2937053",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://doi.org/10.2307%2F2937053",
            data_url  = "https://globalwebdb.com/",
            author    = "G. P. Closs",
            year      = "1994",
            bibtex    = "@article{Closs_1994, doi = {10.2307/2937053}, url = {https://doi.org/10.2307%2F2937053}, year = 1994, month = {feb}, publisher = {Wiley}, volume = {64}, number = {1}, pages = {1--21}, author = {G. P. Closs and P. S. Lake}, title = {Spatial and Temporal Variation in the Structure of an Intermittent-Stream Food Web}, journal = {Ecological Monographs}}")

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


dataset <- list(name        = "closs_1994",
                date        = "1994-01-01",
                description = "Food web of australian interminttent-stream",
                public      = TRUE)


# trait <- list(date = "1111-11-11")


network <- list(Site_A_may_1985 = list(name             = "lerderderg_site_A_may_1985",
                                       date             = "1985-05-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site A",
                                       public           = TRUE,
                                       all_interactions = FALSE),
                Site_B_may_1985 = list(name             = "lerderderg_site_B_may_1985",
                                       date             = "1985-05-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site B",
                                       public           = TRUE,
                                       all_interactions = FALSE),
                Site_C_may_1985 = list(name             = "lerderderg_site_C_may_1985",
                                       date             = "1985-05-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site C",
                                       public           = TRUE,
                                       all_interactions = FALSE),
          Site_A_september_1985 = list(name             = "lerderderg_site_A_september_1985",
                                       date             = "1985-09-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site A",
                                       public           = TRUE,
                                       all_interactions = FALSE),
          Site_B_september_1985 = list(name             = "lerderderg_site_B_september_1985",
                                       date             = "1985-09-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site B",
                                       public           = TRUE,
                                       all_interactions = FALSE),
          Site_C_september_1985 = list(name             = "lerderderg_site_C_september_1985",
                                       date             = "1985-09-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site C",
                                       public           = TRUE,
                                       all_interactions = FALSE),
           Site_A_december_1985 = list(name             = "lerderderg_site_A_december_1985",
                                       date             = "1985-12-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site A",
                                       public           = TRUE,
                                       all_interactions = FALSE),
           Site_B_december_1985 = list(name             = "lerderderg_site_B_december_1985",
                                       date             = "1985-12-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the  Lerderderg River site B",
                                       public           = TRUE,
                                       all_interactions = FALSE),
           Site_C_december_1985 = list(name             = "lerderderg_site_C_december_1985",
                                       date             = "1985-12-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site C",
                                       public           = TRUE,
                                       all_interactions = FALSE),
              Site_A_march_1986 = list(name             = "lerderderg_site_A_march_1986",
                                       date             = "1986-03-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site A",
                                       public           = TRUE,
                                       all_interactions = FALSE),
              Site_B_march_1986 = list(name             = "lerderderg_site_B_march_1986",
                                       date             = "1986-03-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site B",
                                       public           = TRUE,
                                       all_interactions = FALSE),
              Site_C_march_1986 = list(name             = "lerderderg_site_C_march_1986",
                                       date             = "1986-03-01",
                                       lat              = lat,
                                       lon              = lon,
                                       srid             = srid,
                                       description      = "Food web of the Lerderderg River site C",
                                       public           = TRUE,
                                       all_interactions = FALSE))



inter <- list(Site_A_may_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-05-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site A",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
              Site_B_may_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-05-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site B",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
              Site_C_may_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-05-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site C",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
        Site_A_september_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-09-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site A",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
        Site_B_september_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-09-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site B",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
        Site_C_september_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-09-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site C",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
         Site_A_december_1985 = list(taxon_1_level = "taxon",
                                     taxon_2_level = "taxon",
                                     date          = "1985-12-01",
                                     direction     = "directed",
                                     method        = "observation",
                                     description   = "Food web of the Lerderderg River site A",
                                     public        = TRUE,
                                     lat           = lat,
                                     lon           = lon,
                                     srid          = srid),
        Site_B_december_1985 = list(taxon_1_level = "taxon",
                                    taxon_2_level = "taxon",
                                    date          = "1985-12-01",
                                    direction     = "directed",
                                    method        = "observation",
                                    description   = "Food web of the Lerderderg River site B",
                                    public        = TRUE,
                                    lat           = lat,
                                    lon           = lon,
                                    srid          = srid),
        Site_C_december_1985 = list(taxon_1_level = "taxon",
                                    taxon_2_level = "taxon",
                                    date          = "1985-12-01",
                                    direction     = "directed",
                                    method        = "observation",
                                    description   = "Food web of the Lerderderg River site C",
                                    public        = TRUE,
                                    lat           = lat,
                                    lon           = lon,
                                    srid          = srid),
           Site_A_march_1986 = list(taxon_1_level = "taxon",
                                    taxon_2_level = "taxon",
                                    date          = "1986-03-01",
                                    direction     = "directed",
                                    method        = "observation",
                                    description   = "Food web of the Lerderderg River site A",
                                    public        = TRUE,
                                    lat           = lat,
                                    lon           = lon,
                                    srid          = srid),
        Site_B_march_1986 = list(taxon_1_level = "taxon",
                                 taxon_2_level = "taxon",
                                 date          = "1986-03-01",
                                 direction     = "directed",
                                 method        = "observation",
                                 description   = "Food web of the Lerderderg River site B",
                                 public        = TRUE,
                                 lat           = lat,
                                 lon           = lon,
                                 srid          = srid),
        Site_C_march_1986 = list(taxon_1_level = "taxon",
                                 taxon_2_level = "taxon",
                                 date          = "1986-03-01",
                                 direction     = "directed",
                                 method        = "observation",
                                 description   = "Food web of the Lerderderg River site C",
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
  set_names(names(network))
data_matrice[[1]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[2]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[3]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[4]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[5]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[6]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[7]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[8]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[9]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[10]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[11]][1,1] <- NA # Remove file identifier use  NA value
data_matrice[[12]][1,1] <- NA # Remove file identifier use  NA value

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
  map(~filter(.x, value != 0)) %>% # Remove 0 interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Ss]ilicoflag.*)|
                                           (.*[Pp]roducers.*)|(.*[Mm]acrocystis.*)|(.*[Pp]terygophora.*)|(.*[Ss]eaweed.*)|(Micro-epiphytes)|
                                           (Macro-epiphytes)|(.*[Aa]scophyllum).*|(.*[Ee]nteromorpha.*)|(.*[Ff]ucus.*)|(.*[Uu]lva.*)|
                                           (.*[Zz]ostera.*)|(.*[Cc]occolitho.*)|(.*[Ll]eave.*)|(.*[Tt]etraedron.*)|(.*[T]tabellaria.*)|(.*[Nn]avicula.*)|
                                           ()"), "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Dd]etritu.*)|(.*[Dd]etritu.*(?!.*))|(DOM)|(Discard)|([Dd]issolved.*organic.*)|(.*[Ff]ecal.*)")
                                ,"commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Cc]arcasse.*)|(.*[Dd]ead.*)"), "scavenger", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Ii]mport.*)|(.*basic\\sfood.*)"), "unknown", .x$type)))

#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

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
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "^nr\\.\\s"))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\ssp\\..*$"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s-\\sadult.*$"))) %>%
  map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "(\\s[:digit:])")))

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
#POST_taxonomy(taxa_back_df)

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