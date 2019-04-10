# Set libraries
library(reshape2)
library(tidyr)
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


srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "number of individuals in gut",
                   table_owner = "interactions",
                   description = "Number of individuals in the gut content during analysis",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi      = "10.1080/00288330.2004.9517265",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://www.tandfonline.com/doi/abs/10.1080/00288330.2004.9517265",
            data_url  = "https://globalwebdb.com/",
            author    = "Thompson",
            year      = "2004",
            bibtex    = "@article{Thompson_2004, doi = {10.1080/00288330.2004.9517265}, url = {https://doi.org/10.1080%2F00288330.2004.9517265}, year = 2004, month = {sep}, publisher = {Informa {UK} Limited}, volume = {38}, number = {4}, pages = {595--608}, author = {R. M. Thompson and C. R. Townsend}, title = {Land-use influences on New Zealand stream communities: Effects on species composition, functional organisation, and food-web structure}, journal = {New Zealand Journal of Marine and Freshwater Research}}")



users <- list(name         = "Benjamin Mercier",
              email        = "Benjamin.b.mercier@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "User")


# enviro <- list(name  = "attribute name",
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)


datasets <- list(name        = "Thompson_Townsend_2004",
                date        = "1995-11-01",
                description = "Food web structure of stream communities, New Zealand",
                public      = TRUE)


#trait <- list(date = "1111-11-11")

#------------------------------
# Cleaning matrix
#------------------------------
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Thompson_Townsend_2004'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB236', 'WEB237', 'WEB238', 'WEB239', "WEB240", "WEB241",'WEB242', 'WEB243', 'WEB244', 'WEB245', "WEB246", "WEB247")
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB236.csv'), subset(name.dictionary, name.dictionary$web == 'WEB237.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB238.csv'), subset(name.dictionary, name.dictionary$web == 'WEB239.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB240.csv'), subset(name.dictionary, name.dictionary$web == 'WEB241.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB242.csv'), subset(name.dictionary, name.dictionary$web == 'WEB243.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB244.csv'), subset(name.dictionary, name.dictionary$web == 'WEB245.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB246.csv'), subset(name.dictionary, name.dictionary$web == 'WEB247.csv'))
names(names.of.web) <- c('WEB236', 'WEB237', 'WEB238', 'WEB239', "WEB240", "WEB241",'WEB242', 'WEB243', 'WEB244', 'WEB245', "WEB246", "WEB247")

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "Unidentified detritus", "Detritus", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "Plant material", "Plantae", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "Gloecustis (grn alg)", "Gloeocystis", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "Nitzschia sp.", "Nitzschia", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Funciton to depooled the grouped name
# listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\s-\\s)|(\\, )")
# matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
# names(matrices.interaction) <- c('WEB', 'WEB', 'WEB', 'WEB')

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'Plant material' | sp_taxon_1 == 'Achnanthes inflata' | 
                                                                            sp_taxon_1 == "Achnanthes linearis" | sp_taxon_1 == "Achnanthes minutissima" | 
                                                                            sp_taxon_1 == "Aucalosira" | sp_taxon_1 == "Blue green algae" | 
                                                                            sp_taxon_1 == "Cocconeis placentula" | sp_taxon_1 == "Cymbella aspera" | 
                                                                            sp_taxon_1 == "Fragilaria vaucheriae" | sp_taxon_1 == "Gomphonema sp. 3" |
                                                                            sp_taxon_1 == "Gomphonema angustatum" | sp_taxon_1 == "Cymbella kappi" | 
                                                                            sp_taxon_1 == "Navicula avenacea" | sp_taxon_1 == "Surirella elegans" | 
                                                                            sp_taxon_1 == "Gomphoneis herculeana" | sp_taxon_1 == "Unknown green algal cells" | 
                                                                            sp_taxon_1 == "Nitzschia linearis" | sp_taxon_1 == "Pinnularia viridis" | 
                                                                            sp_taxon_1 == "Rhoicosphenia curvata" | sp_taxon_1 == "Audouniella" | 
                                                                            sp_taxon_1 == "Synedra rumpens" | sp_taxon_1 == "Synedra ulna" | 
                                                                            sp_taxon_1 == "Calothrix" | sp_taxon_1 == "Gomphonema intricaturm" |
                                                                            sp_taxon_1 == "Gomphonema truncatum" | sp_taxon_1 == "Melosira varians" | 
                                                                            sp_taxon_1 == "Batrachospermum" | sp_taxon_1 == "Diatoma heimale" | 
                                                                            sp_taxon_1 == "Gomphonema new sp." | sp_taxon_1 == "Gloecustis (grn alg)" | 
                                                                            sp_taxon_1 == "Microspora" | sp_taxon_1 == "Stauroneis" | 
                                                                            sp_taxon_1 == "Navicula unk. Sp. A" | sp_taxon_1 == "Cymbella mulleri" | 
                                                                            sp_taxon_1 == "fungii" | sp_taxon_1 == "Eunotia serpentina" |
                                                                            sp_taxon_1 == "Navicula mutica" | sp_taxon_1 == "Pinnularia viridis" | 
                                                                            sp_taxon_1 == "Navicula dicephala" | sp_taxon_1 == "Surirella sp." | 
                                                                            sp_taxon_1 == "Cymbella cistula" | sp_taxon_1 == "Epithemia sorex" | 
                                                                            sp_taxon_1 == "Gomphonema subclavatum" | sp_taxon_1 == "Meridion circulare" | 
                                                                            sp_taxon_1 == "Encyonema" | sp_taxon_1 == "Pinnularia mesoleptus" | 
                                                                            sp_taxon_1 == "Surirella tenera" | sp_taxon_1 == "Ulothrix" | 
                                                                            sp_taxon_1 == "redalg" | sp_taxon_1 == "Pinnularia gibba" | 
                                                                            sp_taxon_1 == "Pinnularia mesoleptus" | sp_taxon_1 == "fungi" | 
                                                                            sp_taxon_1 == "Gomphonema sp. 4" | sp_taxon_1 == "Nitzschia palea" | 
                                                                            sp_taxon_1 == "Fungi" | sp_taxon_1 == "Gomphonema parvulum" | 
                                                                            sp_taxon_1 == "Stigeoclonium" | sp_taxon_1 == "Gomphonema tennuellum" | 
                                                                            sp_taxon_1 == "Pinnularia borealis" | sp_taxon_1 == "Pinnularia sp (large)" | 
                                                                            sp_taxon_1 == "Closterium" | sp_taxon_1 == "Pleaurotaenium" | 
                                                                            sp_taxon_1 == "gomnew" | sp_taxon_1 == "Gomphonema parvulum" | 
                                                                            sp_taxon_1 == "Navicula cryptocephala" | sp_taxon_1 == "Diatoma elongatum", 'herbivory', 'predation')))

matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == "Unidentified detritus", "commensalism", type)))

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))

#Final matrice with all networks
#matrices.interaction <- taxa_df <- dplyr::bind_rows(matrices.interaction, .id = "network")

#Taxa
taxa.original <- map(matrices.interaction, ~unique(c(as.vector(.x$sp_taxon_2), as.vector(.x$sp_taxon_1))))
taxa.clear <- map(names.of.web, ~deframe(.x)) %>%
              map2(taxa.original, ~str_replace_all(.y, fixed(.x)))

taxons_df <- map2(taxa.original, taxa.clear, ~cbind(.x, .y)) %>%
  map(~data.frame(.x, stringsAsFactors = F)) %>%
  map(~`colnames<-`(.x, c('original_name', 'name_clear'))) %>%
  map(~mutate(.x, name_clear = paste0(str_to_upper(str_extract(.x$name_clear, ".{1}")), str_remove(.x$name_clear, ".{1}"))))
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Unknown green algal cells", "Algae", name_clear)))
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(name_clear == "Nymph", "Invertebrata", name_clear)))

#Final taxa_df with all networks
#taxa_df <- dplyr::bind_rows(taxa_df, .id = "network")

#Taxa_back
taxa_back <- do.call(rbind, taxons_df) 
taxa_back <- taxa_back$name_clear          
taxa_back <- unique(taxa_back)

## Select only taxa not yet in db

server <- "http://poisotlab.biol.umontreal.ca"

taxa_back_df <- data.frame()

for (i in 1:length(taxa_back)) {
  
  path <- modify_url(server, path = paste0("/api/v2/","taxonomy/?name=", str_replace(taxa_back[i], " ", "%20")))
  if (length(content(GET(url = path, config = add_headers("Content-type" = "application/json")))) == 0) {
    
    taxa_back_df[nrow(taxa_back_df)+1, 1] <- taxa_back[i]
  }
}

rm(taxa_back)
names(taxa_back_df) <- c("name")

## Get code by species
taxa_back_df[, "bold"] <- NA
taxa_back_df[, "eol"]  <- NA
taxa_back_df[, "tsn"]  <- NA
taxa_back_df[, "ncbi"] <- NA

### Encore probleme d"identification avec les api... ###

for (i in 1:nrow(taxa_back_df)) {
  try (expr = (taxa_back_df[i, 2] <- get_boldid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 3] <- get_eolid(taxa_back_df[i, 1], row = 5, verbose = FALSE, key = 110258)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 4] <- get_tsn(taxa_back_df[i, 1], row = 5, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 5] <- get_uid(taxa_back_df[i, 1], row = 5, verbose = FALSE)[1]), silent = TRUE)
}
taxa_back_df[1,4] <- "3456"
taxa_back_df[7,4] <- "4810"
taxa_back_df[15,4] <- "4989"
taxa_back_df[16,4] <- "5104"
taxa_back_df[18,4] <- "3634"
taxa_back_df[34,4] <- "598436"
taxa_back_df[98,4] <- "4975"
taxa_back_df[99,4] <- "4816"
taxa_back_df[100,4] <- "3662"
taxa_back_df[109,4] <- "127987"
taxa_back_df[115,4] <- "4462"
taxa_back_df[116,4] <- "4968"
taxa_back_df[118,4] <- "3678"
taxa_back_df[127,4] <- "5381"
taxa_back_df[128,4] <- "128021"

# Writing taxo_back_df
write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa_back.csv'), row.names = FALSE)
saveRDS(taxons_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa.csv'))
saveRDS(matrices.interaction, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.csv'))

#------------------------------
# POST commun table
#------------------------------
POST_attribute(attr = attr_inter)

POST_ref(ref = refs)

POST_users(users = users)

POST_dataset(dataset = datasets, users = users, ref = refs)

POST_taxonomy(taxo = taxa_back_df)

#------------------------------
# Injection loop : Network by network
#------------------------------
meta <- read.table(file = "mangal-datasets-ben/DBase_de_Patrick/Thompson_Townsend_2004/raw/meta.txt", header = T, sep = "\t", stringsAsFactors = F)
for (i in 1:12) {
  
  
  networks <- list(name             = paste0('Thompson_Townsend_2004_',i),
                  date             = "1995-11-01",
                  lat              = meta[i,2],
                  lon              = meta[i,3],
                  srid             = srid,
                  description      = paste("Food web structure of the", meta[i,1], ', New Zealand'),
                  public           = TRUE,
                  all_interactions = FALSE)
  
   inter <- list(date       = "1995-11-01",
              direction     = "directed",
              method        = "Gut content",
              description   = "null",
              public        = TRUE,
              lat           = meta[i,2],
              lon           = meta[i,3],
              srid          = srid)
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxa_back_df, matrices.interaction, names.of.web, name.dictionary, meta, meta_name, filenames, folder.name, i, path, server, foldername, taxa.clear, taxa.original, listafterdepooled)
