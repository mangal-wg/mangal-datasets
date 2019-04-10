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

lat  <- 69.03245049593446
lon  <- -147.71594467717847
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence or absence of interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1111/j.1365-2427.2006.01567.x",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2427.2006.01567.x",
            data_url  = "https://globalwebdb.com/",
            author    = "Parker",
            year      = "2006",
            bibtex    = "@article{PARKER_2006, doi = {10.1111/j.1365-2427.2006.01567.x}, url = {https://doi.org/10.1111%2Fj.1365-2427.2006.01567.x}, year = 2006, month = {jul}, publisher = {Wiley}, volume = {51}, number = {7}, pages = {1249--1263}, author = {STEPHANIE M. PARKER and ALEXANDER D. HURYN}, title = {Food web structure and function in two arctic streams with contrasting disturbance regimes}, journal = {Freshwater Biology}}")



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


datasets <- list(name        = "Parker_Huryn_2006",
                date        = "2002-07-25",
                description = "Food web structure of two arctic streams, Alaska",
                public      = TRUE)


#trait <- list(date = "1111-11-11")

#------------------------------
# Cleaning matrix
#------------------------------
library(purrr)
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Parker_Huryn_2006'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB273', 'WEB274', 'WEB275', 'WEB276')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB273.csv'), subset(name.dictionary, name.dictionary$web == 'WEB274.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB275.csv'), subset(name.dictionary, name.dictionary$web == 'WEB276.csv'))
names(names.of.web) <- c('WEB273', 'WEB274', 'WEB275', 'WEB276')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse( original_name == "vascular plant", "Plantae", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse( original_name == "Diatoma spp.", "Diatoma", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse( original_name == "bryophyte", "Bryophyta", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse( original_name == "Diatoma heimale", "Diatoma heimale", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

# #Funciton to depooled the grouped name
# listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\s-\\s)|(\\, )")
# matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
# names(matrices.interaction) <- c('WEB', 'WEB', 'WEB', 'WEB')

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == "Amphora ovalis" | sp_taxon_1 == "Achnanthes spp." |
                                                                            sp_taxon_1 == "bryophyte" | sp_taxon_1 == "Cocconeis sp." | sp_taxon_1 == "Cymbella spp." |
                                                                            sp_taxon_1 == "Diatoma spp." | sp_taxon_1 == "Fragilaria vaucheriae" | sp_taxon_1 == "Gomphonema angustatum" |
                                                                            sp_taxon_1 == "Meridion circulare" | sp_taxon_1 == "Navicula radiosa v. tenella" | 
                                                                            sp_taxon_1 == "Staurosirella leptostauron" | sp_taxon_1 == "Synedra ulna" | 
                                                                            sp_taxon_1 == "vascular plant" | sp_taxon_1 == "Didymosphenia geminata" | sp_taxon_1 == "Hannaea arcus" |
                                                                            sp_taxon_1 == "Chlorophyta filament" | sp_taxon_1 == "Hydrurus foetidus" | 
                                                                            sp_taxon_1 == "Eunotia sp." | sp_taxon_1 == "Diatoma heimale", 'herbivory', 'predation')))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == "amorphous detritus", "commensalism", type)))
                                                                              
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
taxa_back_df[1,4] <- "4721"
taxa_back_df[5,4] <- "3238"
taxa_back_df[6,4] <- "3952"
taxa_back_df[7,4] <- "590961"
taxa_back_df[9,4] <- "3322"
taxa_back_df[24,4] <- "128401"

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
meta <- read.table(file="mangal-datasets-ben/DBase_de_Patrick/Parker_Huryn_2006/raw/meta.txt", sep = "\t", header = T, stringsAsFactors = F)
meta_name <- c("1","2","3","4")
for (i in 1:4) {
  
  
  networks <- list(name             = paste0("Parker_Huryn_2006_", meta_name[i]),
                   date             = meta[i,2],
                   lat              = lat,
                   lon              = lon,
                   srid             = srid,
                   description      = paste0("Food web structure of an arctic stream (",meta[i,1],"), Alaska"),
                   public           = TRUE,
                   all_interactions = FALSE)
  
  inter <- list(date          = meta[i,2],
                direction     = "directed",
                method        = "Gut content",
                description   = "null",
                public        = TRUE,
                lat           = lat,
                lon           = lon,
                srid          = srid)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxa_back_df, matrices.interaction, names.of.web, name.dictionary, meta, meta_name, filenames, folder.name, i, path, server, foldername, taxa.clear, taxa.original, listafterdepooled)
