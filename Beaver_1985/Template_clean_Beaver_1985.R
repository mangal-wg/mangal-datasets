# Set libraries
library(tidyr)
#library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)

library(mangal)

#------------------------------
# Metadata
#------------------------------


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

refs <- list(doi       = "10.1111/j.1365-2311.1985.tb00720.x",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1365-2311.1985.tb00720.x",
            data_url  = "https://globalwebdb.com/",
            author    = "Beaver",
            year      = "1985",
            bibtex    = "@article{BEAVER_1985, doi = {10.1111/j.1365-2311.1985.tb00720.x},	url = {https://doi.org/10.1111%2Fj.1365-2311.1985.tb00720.x},	year = 1985,	month = {aug},	publisher = {Wiley},	volume = {10},	number = {3},	pages = {241--248},	author = {R. A. BEAVER},	title = {Geographical variation in food web structure in Nepenthes pitcher plants},	journal = {Ecological Entomology}}")


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


datasets <- list(name        = "Beaver_1985",
                #date        = "1111-11-11",
                description = "The geographical variation of food web structure in different species of Nepenthes pitcher plants",
                public      = TRUE)


#trait <- list(date = "1111-11-11")





#------------------------------
# Cleaning matrix
#------------------------------

library(stringr)
library(reshape2)
library(tibble)
library(purrr)
folder.name <- 'Beaver_1985'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)

#Edge format and remove the false interaction
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
                        map(~slice(.x, 2:nrow(.x))) %>%
                        map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
                        map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) %>%
                        map(~subset(.x, .x$value != 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'older organic debris' | sp_taxon_1 == 'recently drowned insects', 'scavenger', 'predation')))

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\/)")
matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
names(matrices.interaction) <- c('WEB131', 'WEB132', 'WEB133', 'WEB134','WEB135')

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))

#Final matrice with all networks
#matrices.interaction <- taxa_df <- dplyr::bind_rows(matrices.interaction, .id = "network")

#--------------------
#Taxa
#--------------------
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB131.csv'), subset(name.dictionary, name.dictionary$web == 'WEB132.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB133.csv'), subset(name.dictionary, name.dictionary$web == 'WEB134.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB135.csv'))
names(names.of.web) <- c('WEB131', 'WEB132', 'WEB133', 'WEB134','WEB135')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name =
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])

#Getting the names that were pooled to put them through gnr_resolve
names_gnr <- unique(unlist(listafterdepooled[2]))
names_gnr <- gnr_resolve(names_gnr, canonical = F, best_match_only = T)
names_gnr <-select(names_gnr, c(1,3))
names(names_gnr) <- c("original_name", "scientific_name")
names.of.web <- map(names.of.web, ~rbind(.x, names_gnr))
  
#Create taxa_df
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


#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa_back.csv'), row.names = FALSE)
saveRDS(taxons_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa.csv'))
saveRDS(matrices.interaction, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.csv'))
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)
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
meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Beaver_1985/raw/meta.txt', header=T, sep='\t')
meta$nom <- meta$network
meta$nom <- str_replace_all(meta$nom, "in", "")
meta$nom <- str_replace_all(meta$nom, "\\s", "_")
meta[3,4] <- "N._distillatoria__Sri_Lanka"

for (i in 1:5) {
  
  networks <- list(name             = paste0("Beaver_1985_", meta[i,4]),
                  #date             = "1111-11-11",
                  lat              = meta[i,2],
                  lon              = meta[i,3],
                  srid             = srid,
                  description      = paste0("The food web structure of ",meta[i,1]),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  
  inter <- list(#date          = "1111-11-11",
                direction     = "directed",
                method        = "Biblio",
                description   = "null",
                public        = TRUE,
                lat           = meta[i,2],
                lon           = meta[i,3],
                srid          = srid)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, names_gnr, listafterdepooled, taxa_back_df, taxa.clear, taxa.original, matrices.interaction, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server)
