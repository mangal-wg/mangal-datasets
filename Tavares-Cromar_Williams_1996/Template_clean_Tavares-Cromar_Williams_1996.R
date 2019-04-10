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

lat  <- 43.96299832810763
lon  <- -79.08227595315839
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "present or absent in gut",
                   table_owner = "interactions",
                   description = "Present or absent in the gut content during analysis",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.2307/2963482",
            jstor     = "https://www.jstor.org/stable/2963482",
            pmid      = "NA",
            paper_url = "https://esajournals.onlinelibrary.wiley.com/doi/abs/10.2307/2963482",
            data_url  = "https://globalwebdb.com/",
            author    = "Tavares-Cromar",
            year      = "1996",
            bibtex    = "@article{Tavares_Cromar_1996, doi = {10.2307/2963482}, url = {https://doi.org/10.2307%2F2963482}, year = 1996, month = {feb}, publisher = {Wiley}, volume = {66}, number = {1}, pages = {91--113}, author = {Annette F. Tavares-Cromar and D. Dudley Williams}, title = {The Importance of Temporal Resolution in Food Web Analysis: Evidence from a Detritus-Based Stream}, journal = {Ecological Monographs}}")



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


datasets <- list(name        = "Tavares-Cromar_Williams_1996",
                date        = "1985-10-01",
                description = "Food web structure of the macroinvertebrate riffle community of Duffin Creek (throughout one year), Ontario, Canada",
                public      = TRUE)


#trait <- list(date = "1111-11-11")






#------------------------------
# Cleaning matrix
#------------------------------
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Tavares-Cromar_Williams_1996'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB280', 'WEB281', 'WEB282', 'WEB283',"WEB284","WEB285","WEB286","WEB287")
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB280.csv'), subset(name.dictionary, name.dictionary$web == 'WEB281.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB282.csv'), subset(name.dictionary, name.dictionary$web == 'WEB283.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB284.csv'), subset(name.dictionary, name.dictionary$web == 'WEB285.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB286.csv'), subset(name.dictionary, name.dictionary$web == 'WEB287.csv'))
names(names.of.web) <- c('WEB280', 'WEB281', 'WEB282', 'WEB283',"WEB284","WEB285","WEB286","WEB287")

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "vascular plant material" | original_name == "Vascular plant material", "Plantae", scientific_name)))
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
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'diatoms' | sp_taxon_1 == 'Diatoms' | 
                                                                            sp_taxon_1 == "fungal hyphae" | sp_taxon_1 == "vascular plant material" |
                                                                            sp_taxon_1 == "other algae" | sp_taxon_1 == "Fungal hyphae" |
                                                                            sp_taxon_1 == "Other algae" | sp_taxon_1 == "Vascular plant material", 'herbivory', 'predation')))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'Detritus' | sp_taxon_1 == 'Microinorganic particles' |
                                                                            sp_taxon_1 == "Chitin fragments" | sp_taxon_1 == "Soft body parts" |
                                                                            sp_taxon_1 == "detritus" | sp_taxon_1 == "microinorganic particles" | 
                                                                            sp_taxon_1 == "chitin fragments" | sp_taxon_1 == "soft body parts", "commensalism", type)))
                                                                              
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
taxa_back_df[6,4] <- "127917"

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
meta <- read.table(file = "mangal-datasets-ben/DBase_de_Patrick/Tavares-Cromar_Williams_1996/raw/meta.txt", header=T, sep="\t", stringsAsFactors = F)
for (i in 1:8) {
  
  
  networks <- list(name             = paste0('Tavares-Cromar_Williams_1996_',i),
                  date             = meta[i,2],
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0("Food web structure of the macroinvertebrate riffle community of Duffin Creek ",meta[i,1],', Ontario, Canada'),
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
