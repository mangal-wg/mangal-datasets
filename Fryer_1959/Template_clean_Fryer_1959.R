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

lat  <- -11.612087
lon  <- 34.301249
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

refs <- list(doi       = "10.1111/j.1469-7998.1959.tb05521.x",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://zslpublications.onlinelibrary.wiley.com/doi/abs/10.1111/j.1469-7998.1959.tb05521.x",
            data_url  = "https://globalwebdb.com/",
            author    = "Fryer",
            year      = "1959",
            bibtex    = "@article{FRYER_1959,	doi = {10.1111/j.1469-7998.1959.tb05521.x}, url = {https://doi.org/10.1111%2Fj.1469-7998.1959.tb05521.x}, year = 1959, month = {aug}, publisher = {Wiley}, volume = {132}, number = {2}, pages = {153--281}, author = {GEOFFREY FRYER}, title = {{THE} {TROPHIC} {INTERRELATIONSHIPS} {AND} {ECOLOGY} {OF} {SOME} {LITTORAL} {COMMUNITIES} {OF} {LAKE} {NYASA} {WITH} {ESPECIAL} {REFERENCE} {TO} {THE} {FISHES}, {AND} A {DISCUSSION} {OF} {THE} {EVOLUTION} {OF} A {GROUP} {OF} {ROCK}-{FREQUENTING} {CICHLIDAE}}, journal = {Proceedings of the Zoological Society of London}}")



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


datasets <- list(name        = "Fryer_1959",
                date        = "1955-01-01",
                description = "Food web structure of different places in Lake Nyasa (Lake Malawi), Malawi",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


inter <- list(date          = "1955-01-01",
              direction     = "directed",
              method        = "Field observation and gut content",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



#------------------------------
# Cleaning matrix
#------------------------------
library(purrr)
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Fryer_1959'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
names(matrices.interaction) <- c('WEB33', 'WEB38', 'WEB39')

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\s-\\s)|(\\, )|-|[:space:]and[:space:]")
matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
names(matrices.interaction) <- c('WEB33', 'WEB38', 'WEB39')
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_2 = ifelse( sp_taxon_2 == "squamipinnis", "T. squamipinnis", sp_taxon_2)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_1 = ifelse( sp_taxon_1 == "squamipinnis", "T. squamipinnis", sp_taxon_1)))
matrices.interaction[[2]][75,3] <- "1"

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'Vallisneria' | sp_taxon_1 == 'Aufwuchs on Vallisneria' |
                                                                            sp_taxon_1 == 'Aufwuchs' | sp_taxon_1 == 'higher plants' |
                                                                            sp_taxon_1 == 'algae' | sp_taxon_1 == "bottom algae", 'herbivory', 'predation')))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'plankton', 'unknown', type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'bottom detritus' | sp_taxon_1 == 'detritus', 'commensalism', type)))
#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))
                                       
#Final matrice with all networks
#matrices.interaction <- taxa_df <- dplyr::bind_rows(matrices.interaction, .id = "network")

#Taxa

name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB33.csv'), subset(name.dictionary, name.dictionary$web == 'WEB38.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB39.csv'))
names(names.of.web) <- c('WEB33', 'WEB38', 'WEB39')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse( original_name == "higher plants", "Plantae", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse( original_name == "Aufwuchs" | original_name == "Aufwuchs on Vallisneria", "Periphyton", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))
names.of.web[[1]] <- rbind(names.of.web[[1]], `colnames<-`(data.frame(c("bottom algae","T. saka","T. squamipinnis"), c("algae","Oreochromis saka","Oreochromis squamipinnis")), c("original_name","scientific_name")))
names.of.web[[1]][26,] <- c("Tilapia shirana", "Oreochromis shirana")
names.of.web[[1]][6,1] <- "detritus"
names.of.web[[2]] <- rbind(names.of.web[[2]], `colnames<-`(data.frame(c("other predatory fishes","P. tropheops","P. minutus","P. auratus","P. fuscus", "Labeotropheus fuelleborni","L. trewavasae", "H. fenestratus"), c("Pisces","Pseudotropheus tropheops","Pseudotropheus minutus","Pseudotropheus auratus","Pseudotropheus fuscus","Labeotropheus fuelleborni","Labeotropheus trewavasae", "Haplochromis fenestratus")), c("original_name","scientific_name")))
names.of.web[[2]][15,1] <- "Haplochromis pardalis"
names.of.web[[2]][18,1] <- "Pseudotropheus elongatus"
names.of.web[[2]][19,1] <- "Haplochromis guentheri"
names.of.web[[3]] <- rbind(names.of.web[[3]], `colnames<-`(data.frame(c("other gastropods", "algae","T. squamipinnis","Haplochromis similis","Haplochromis moori"), c("Gastropoda","algae","Oreochromis squamipinnis","Haplochromis similis","Haplochromis moori")), c("original_name","scientific_name")))
names.of.web[[3]][6,1] <- "Malanoides tuberculata"
names.of.web[[3]][9,1] <- "bottom detritus"
names.of.web[[3]][12,1] <- "Tilapia saka"
names.of.web[[3]][12,2] <- "Oreochromis saka"
names.of.web[[3]][33,1] <- "Barbus innocens"
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "Tilapia shirana", "Oreochromis shiranus", scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == "Tilapia melanopleura", "Oreochromis aureus", scientific_name)))

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
taxa_back_df[39,4] <- "170011"


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
meta <- c('Crocodile Creek', 'a rocky shore', 'a sandy shore')
meta_name <- c('Crocodile_Creek', 'rocky_shore', 'sandy_shore')
for (i in 1:3) {
  
  
  networks <- list(name            = paste0("Fryer_1959_", meta_name[i]),
                  date             = "1955-01-01",
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0('Food web structure of ',meta[i], ', Lake Nyasa (Lake Malawi), Malawi'),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
