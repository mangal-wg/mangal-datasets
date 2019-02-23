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
library(readxl)
library(mangal)
#------------------------------
# Metadata
#------------------------------


srid <- 4326

folder_name <- "dunne_2013" # Name of the subfolder in mangal-datasets
food_web_name <- c("Dunne2013PBioSupData.xlsx") # Name of the dataset in Trophic metacommunities-master/Trophic_metacom_meta_analysis/Data

name_dictionnary <- read_csv("Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv",
                      col_type = cols(.default = col_character())) %>%
  filter( web %in% food_web_name) %>%
  split(.$web)

name_file <- read_xlsx(paste0(folder_name, "/raw/Dunne2013PBioSupData.xlsx"), sheet = 2) %>%
  select(-c(seq.int(4,24, 4)))

name_site <- names(name_file) %>% # Get the site name to use it as name for list
  .[!. %in% str_subset(., ".*[:digit:]$")]

name_id <- seq.int(1, 19, 3) %>% # Get the id of each sp name for each site
  map(~select(name_file, .x)) %>%
  map2(name_site, ~mutate(.x, site = .y)) %>%
  map(~`names<-`(.x, c("ID", "site")))

name_type <- seq.int(2, 21, 3) %>% # Get the type of organism (non-parasit/parasit)
  map(~select(name_file, .x)) %>%
  map(~`names<-`(.x, c("Type")))

name_sp_name <- seq.int(3, 22, 3) %>% # Get the name of each sp
  map(~select(name_file, .x)) %>%
  map(~`names<-`(.x, c("Name")))

name_file <- pmap(list(name_id, name_type, name_sp_name), bind_cols) %>% # Mergin each data frame to create one data frame with id, type, 
  map(~filter(.x , ID != "ID")) %>%
  map(~mutate(.x, ID = as.double(.x$ID))) %>%
  map(~filter(.x, !is.na(.x$ID))) %>%
  map(~select(.x, ID, Name)) %>%
  set_names(name_site)


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

ref <- list(doi       = "10.1371/journal.pbio.1001579",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://doi.org/10.1371%2Fjournal.pbio.1001579",
            data_url  = "https://datadryad.org/resource/doi:10.5061/dryad.b8r5c",
            author    = "Jennifer A. Dunne",
            year      = "2013",
            bibtex    = "@article{Dunne_2013, doi = {10.1371/journal.pbio.1001579}, url = {https://doi.org/10.1371%2Fjournal.pbio.1001579}, year = 2013, month = {jun}, publisher = {Public Library of Science ({PLoS})}, volume = {11}, number = {6}, pages = {e1001579}, author = {Jennifer A. Dunne and Kevin D. Lafferty and Andrew P. Dobson and Ryan F. Hechinger and Armand M. Kuris and Neo D. Martinez and John P. McLaughlin and Kim N. Mouritsen and Robert Poulin and Karsten Reise and Daniel B. Stouffer and David W. Thieltges and Richard J. Williams and Claus Dieter Zander}, editor = {Michel Loreau}, title = {Parasites Affect Food Web Structure Primarily through Increased Diversity and Complexity}, journal = {{PLoS} Biology}}")

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


dataset <- list(name        = "dunne_2013",
                date        = "12-06-2013",
                description = "Coastal or estuarine food web",
                public      = TRUE)

# trait <- list(date = "1111-11-11")


network <- list(`Bahia Falsa` = list(name        = "Bahia_Falsa_2005",
                                     date        = "31-12-2005",
                                     lat              = c(30.46, 30.43),
                                     lon              = c(-116.02, -116.03),
                                     srid             = srid,
                                     description      = "Food web of the Bahia Falsa",
                                     public           = TRUE,
                                     all_interactions = FALSE),
      `Carpinteria Salt March` = list(name        = "Carpinteria Salt March",
                                     date        = "31-12-2008",
                                     lat              = c(34.41, 34.40),
                                     lon              = c(-119.53, -119.55),
                                     srid             = srid,
                                     description      = "Food web of the Carpinteria Salt March",
                                     public           = TRUE,
                                     all_interactions = FALSE),
          `Estero de Punta Banda` = list(name        = "Estero de Punta Banda",
                                         date        = "31-12-2005",
                                         lat              = c(31.78, 31.69),
                                         lon              = c(-116.6, -116.65),
                                         srid             = srid,
                                         description      = "Food web of the estuary of Punta Banda",
                                         public           = TRUE,
                                         all_interactions = FALSE),
                `Flensburg Fjord` = list(name        = "Flensburg_Fjord_2000",
                                         date        = "31-12-2000",
                                         lat              = c(54.92, 54.75),
                                         lon              = c(9.25, 9.9),
                                         srid             = srid,
                                         description      = "Food web of the Flensburg Fjord",
                                         public           = TRUE,
                                         all_interactions = FALSE),
                    `Otago Harbor` = list(name        = "Otago_Harbor_2009",
                                          date        = "31-12-2009",
                                          lat              = c(45.78, 45.88),
                                          lon              = c(170.72, 170.50),
                                          srid             = srid,
                                          description      = "Food web of the Otago Harbor",
                                          public           = TRUE,
                                          all_interactions = FALSE),
                `Sylt Tidal Basin` = list(name        = "Sylt_Tidal_Basin_2006",
                                          date        = "31-12-2006",
                                          lat              = c(8.33, 8.67),
                                          lon              = c(54.83, 55.12),
                                          srid             = srid,
                                          description      = "Food web of the Sylt Tidal Basin",
                                          public           = TRUE,
                                          all_interactions = FALSE),
                    `Ythan Estuary` = list(name        = "Ythan_Estuary_1996",
                                           date        = "01-06-1996",
                                           lat              = 57.315338,
                                           lon              = -1.990028,
                                           srid             = srid,
                                           description      = "Food web of the Ythan Estuary",
                                           public           = TRUE,
                                           all_interactions = FALSE))


inter <- list(`Bahia Falsa` = list(taxon_1_level = "taxon",
                                   taxon_2_level = "taxon",
                                   date          = "31-12-2005",
                                   direction     = "directed",
                                   method        = "observation",
                                   description   = "Food web of the Bahia Falsa",
                                   public        = TRUE,
                                   lat           = c(30.46, 30.43),
                                   lon           = c(-116.02, -116.03),
                                   srid          = srid),
  `Carpinteria Salt March` = list(taxon_1_level = "taxon",
                                  taxon_2_level = "taxon",
                                  date          = "31-12-2008",
                                  direction     = "directed",
                                  method        = "observation",
                                  description   = "Food web of the Carpinteria Salt March",
                                  public        = TRUE,
                                  lat           = c(34.41, 34.40),
                                  lon           = c(-119.53, -119.55),
                                  srid          = srid),
    `Estero de Punta Banda` = list(taxon_1_level = "taxon",
                                   taxon_2_level = "taxon",
                                   date          = "31-12-2005",
                                   direction     = "directed",
                                   method        = "observation",
                                   description   = "Food web of the estuary of Punta Banda",
                                   public        = TRUE,
                                   lat           = c(31.78, 31.69),
                                   lon           = c(-116.6, -116.65),
                                   srid          = srid),
          `Flensburg Fjord` = list(taxon_1_level = "taxon",
                                   taxon_2_level = "taxon",
                                   date          = "31-12-2005",
                                   direction     = "directed",
                                   method        = "observation",
                                   description   = "Food web of the Flensburg Fjord",
                                   public        = TRUE,
                                   lat           = c(54.92, 54.75),
                                   lon           = c(9.25, 9.9),
                                   srid          = srid),
             `Otago Harbor` = list(taxon_1_level = "taxon",
                                   taxon_2_level = "taxon",
                                   date          = "31-12-2009",
                                   direction     = "directed",
                                   method        = "observation",
                                   description   = "Food web of the Otago Harbor",
                                   public        = TRUE,
                                   lat           = c(45.78, 45.88),
                                   lon           = c(170.72, 170.50),
                                   srid          = srid),
         `Sylt Tidal Basin` = list(taxon_1_level = "taxon",
                                   taxon_2_level = "taxon",
                                   date          = "31-12-2006",
                                   direction     = "directed",
                                   method        = "observation",
                                   description   = "Food web of the Sylt Tidal Basin",
                                   public        = TRUE,
                                   lat           = c(8.33, 8.67),
                                   lon           = c(54.83, 55.12),
                                   srid          = srid),
            `Ythan Estuary` = list(taxon_1_level = "taxon",
                                   taxon_2_level = "taxon",
                                   date          = "01-06-1996",
                                   direction     = "directed",
                                   method        = "observation",
                                   description   = "Food web of the Ythan Estuary",
                                   public        = TRUE,
                                   lat           = 57.315338,
                                   lon           = -1.990028,
                                   srid          = srid))

#------------------------------
# Cleaning matrix
#------------------------------

# Open file
sheet_to_read <- paste0("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/mangal-datasets/", folder_name, "/raw/", food_web_name) %>%
  excel_sheets() %>%
  .[!. %in% c("Data Notes", "Species Lists ")]

data_matrice <- map(sheet_to_read, ~read_xlsx(paste0("~/Documents/UBO/Cours/Semestre 8/Stage/Mangal/mangal-datasets/", folder_name, "/raw/", food_web_name),
                                              sheet = .x))

predator_only <- data_matrice %>% # Create an edge format food web with only predrator-prey link
  map(~select(.x, c(1:2))) %>%
  map(~`names<-`(.x, c("sp_taxon_1_id", "sp_taxon_2_id"))) %>%
  map(~mutate(.x, value = 1)) %>%
  map2(name_file, ~left_join(.x, .y, by = c("sp_taxon_1_id" = "ID"))) %>%
  map2(name_file, ~left_join(.x, .y, by = c("sp_taxon_2_id" = "ID"))) %>%
  map(~select(.x, Name.x, Name.y, value)) %>%
  map(~`names<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) %>%
  map(~filter(.x, !is.na(.x$sp_taxon_1) & !is.na(.x$sp_taxon_2))) %>%
  set_names(name_site)

parasite_only <- data_matrice %>% # Create an edge format food web with only parasit-host link
  map(~select(.x, c(4:5))) %>%
  map(~`names<-`(.x, c("sp_taxon_1_id", "sp_taxon_2_id"))) %>%
  map(~mutate(.x, value = 1)) %>%
  map2(name_file, ~left_join(.x, .y, by = c("sp_taxon_1_id" = "ID"))) %>%
  map2(name_file, ~left_join(.x, .y, by = c("sp_taxon_2_id" = "ID"))) %>%
  map(~select(.x, Name.x, Name.y, value)) %>%
  map(~`names<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) %>%
  map(~filter(.x, !is.na(.x$sp_taxon_1) & !is.na(.x$sp_taxon_2))) %>%
  map2(predator_only, ~anti_join(.x, .y, by = "sp_taxon_1")) %>%
  map(~mutate(.x, type = "parasitism")) %>%
  set_names(name_site)

FW_name <- data_matrice %>% # Create an edge food web format with predator-prey and parasitism-host link
  map(~select(.x, c(7,8))) %>%
  map(~`names<-`(.x, c("sp_taxon_1_id", "sp_taxon_2_id"))) %>%
  map(~mutate(.x, value = 1)) %>%
  map2(name_file, ~left_join(.x, .y, by = c("sp_taxon_1_id" = "ID"))) %>%
  map2(name_file, ~left_join(.x, .y, by = c("sp_taxon_2_id" = "ID"))) %>%
  map(~select(.x, Name.x, Name.y, value)) %>%
  map(~`names<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) %>%
  map2(parasite_only, ~setdiff(.x, select(.y, -4))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Pp]lant.*)|(.*[Aa]lgae.*)|(.*[Pp]hyto.*)|(.*[Dd]iatoms.*)|(.*[Dd]inofl.*)|(.*[Ss]ilicoflag.*)|
                                           (.*[Pp]roducers.*)|(.*[Mm]acrocystis.*)|(.*[Pp]terygophora.*)|(.*[Ss]eaweed.*)|(Micro-epiphytes)|
                                           (Macro-epiphytes)|(.*[Aa]scophyllum).*|(.*[Ee]nteromorpha.*)|(.*[Ff]ucus.*)|(.*[Uu]lva.*)|
                                           (.*[Zz]ostera.*)|(.*[Cc]occolitho.*)|(.*[Ll]eave.*)"), "herbivory", "predation"))) %>% # Add type interaction
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Dd]etritu.*)|(.*[Dd]etritu.*(?!.*))|(DOM)|(Discard)|([Dd]issolved.*organic.*)|(.*[Ff]ecal.*)")
                                ,"commensalism", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Cc]arcasse.*)"), "scavenger", .x$type))) %>%
  map(~mutate(.x, type = ifelse(str_detect(.x$sp_taxon_2, "(.*[Ii]mport.*)|(.*basic\\sfood.*)"), "unknown", .x$type))) %>%
  map2(parasite_only, ~union_all(.x, .y)) %>%
  set_names(name_site)

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

if(length(name_dictionnary) > 0){ 
  
  sp_name_for_this_web <- name_file %>%
    map(~select(.x, original_name, scientific_name)) %>%
    map(~mutate(.x, scientific_name = if_else(is.na(.x$scientific_name), original_name, scientific_name))) %>%
    map(~deframe(.x))
  
  taxa_df <- taxa %>%
    map2(sp_name_for_this_web, ~mutate(.x, name_clear = str_replace_all(.x$original_name, .y))) %>%
    map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}")))) %>%
    map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "\\s\\(.*\\)$"))) %>%
    map(~mutate(.x, name_clear = str_remove_all(.x$name_clear, "(\\s[:digit:])")))
  
}else{
  
  taxa <- taxa %>%
    map(~mutate(.x, id = row_number())) # Create an id to reorder the tab
  
  taxa_corrected <- taxa %>%
    map(~mutate(.x, sp_corrected = .x$original_name)) %>%
    map(~mutate(.x, sp_corrected = str_remove_all(.x$sp_corrected, "\\s\\(.*\\)|\\(.*\\)|\\s.\\s\\(.*\\)"))) %>% # Correct vernacular name at the lowest level possible (species)
    map(~mutate(.x, sp_corrected = str_remove_all(.x$sp_corrected, "(\\s[:digit:])"))) %>%
    map(~mutate(.x, sp_corrected = str_remove_all(.x$sp_corrected, "\\ssp.*"))) %>%
    map(~mutate(.x, sp_corrected = str_remove_all(.x$sp_corrected, "\"|\\?"))) %>%
    map(~mutate(.x, sp_corrected = str_replace_all(.x$sp_corrected, "-", "\\s"))) %>%
    map(~mutate(.x, sp_corrected = str_replace_all(.x$sp_corrected, "(?<!r)(?<!e)an$", "ea"))) %>%
    map(~mutate(.x, sp_corrected = str_replace_all(.x$sp_corrected, "(?<=e)an$|(?<=r)an$", "a")))
    
  
  ################## WIP
  p <- progress_estimated(length(taxa_corrected))
  
  taxa_scientific_name <- taxa_corrected %>%
    map(~.x$sp_corrected) %>%
    # map(~head(.x, 5)) %>%
    map(~{
      p$tick()$print()
      get_wormsid(.x, searchtype = "scientific")
      # get_tsn(.x, searchtype = "scientific")
    }) %>%
    map(~enframe(.x, name = NULL, value = "worms_id")) %>%
    map(~mutate(.x, id_name = 1:nrow(.)))
  
  taxa_name_not_resolve <- test_scientific_name %>%
    map(~filter(.x, is.na(worms_id)))
  
  p <- progress_estimated(length(taxa_corrected))
  taxa_vernacular_name <- taxa_corrected %>%
    map2(test_name_not_resolve,~filter(.x, id %in% .y$id_name)) %>%
    map(~.x$sp_corrected) %>%
    map(~{
      p$tick()$print()
      get_wormsid(.x, searchtype = "common")
      # get_tsn(.x, searchtype = "scientific")
    }) %>%
    map(~enframe(.x, name = NULL, value = "worms_id")) %>%
    map2(test_name_not_resolve, ~mutate(.x, id_name = .y$id_name))
  
  taxa_all_name <- map2(taxa_scientific_name, taxa_vernacular_name, ~ union(.x, .y)) %>% 
    map(~filter(.x, !(is.na(.x$worms_id) & duplicated(.x$id_name)))) %>%
    map(~mutate(.x, tsn_id = as.numeric(tsn_id))) %>%
    map(~arrange(.x, id_name))
  
  taxa_proper <- taxa_corrected %>%
    map(~.x$worms_id) %>%
    map(~id2name(.x, db = "worms")) %>%
    map(~`attributes<-`(.x, NULL)) %>%
    modify(~modify_if(., ~!is.data.frame(.x), ~data.frame(id = NA_character_, name = NA_character_, rank = NA_character_, status = NA_character_))) %>%
    flatten_dfr() %>%
    mutate(list_original_name = flatten_chr(map2(names(taxa_all_name),  map_int(taxa_all_name, ~nrow(.x)), ~rep(.x, times = .y)))) %>%
    split(.$list_original_name) %>%
    map(~select(.x, name)) %>%
    map(~mutate(.x, id = 1:nrow(.x)))
    
  
  taxa_df <- taxa_corrected %>%
    # map(~head(.x, 5)) %>%
    map2(toto, ~left_join(.x, .y, by = 'id')) %>%
    map(~mutate(.x, name = ifelse(is.na(.x$name), .x$sp_corrected, .x$name))) %>%
    map(~select(.x, original_name, sp_corrected = name))
  
}

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

#-------------------------------------------
# Writing taxa interaction and trait table
#-------------------------------------------

write.csv2(x = taxa_back_df, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_taxa_back.csv"), row.names = FALSE)

if(is.null(names(taxa_df)) == TRUE){
  
  taxa_df %>%
    walk(~write.csv2(x = taxa_df, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_taxa.csv"), row.names = FALSE))
  
}else{
  
  taxa_df %>%
    names() %>%
    walk(~write.csv2(x = taxa_df[[.]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_", str_replace_all(., "\\s", "_"), "_taxa.csv"), row.names = FALSE))
  
}

if(is.null(names(FW_name)) == TRUE){
  
  FW_name %>%
    walk(~write.csv2(x = FW_name, file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_inter.csv"), row.names = FALSE))
  
}else{
  
  FW_name %>%
    names() %>%
    walk(~write.csv2(x = FW_name[[.]], file = paste0("mangal-datasets/", folder_name,"/data/",folder_name, "_", str_replace_all(., "\\s", "_"), "_inter.csv"), row.names = FALSE))
}

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