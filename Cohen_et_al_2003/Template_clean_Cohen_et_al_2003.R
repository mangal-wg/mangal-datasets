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

lat  <- 46.25131
lon  <- -89.4974112
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

refs <- list(doi       = "10.1073/pnas.232715699",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://www.pnas.org/content/100/4/1781.short",
            data_url  = "https://globalwebdb.com/",
            author    = "Cohen",
            year      = "2003",
            bibtex    = "@article{Cohen_2003,	doi = {10.1073/pnas.232715699}, url = {https://doi.org/10.1073%2Fpnas.232715699}, year = 2003, month = {jan}, publisher = {Proceedings of the National Academy of Sciences}, volume = {100}, number = {4}, pages = {1781--1786}, author = {J. E. Cohen and T. Jonsson and S. R. Carpenter}, title = {Ecological community description using the food web, species abundance, and body size}, journal = {Proceedings of the National Academy of Sciences}}")



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


datasets <- list(name        = "Cohen_et_al_2003",
                date        = "1984-01-01",
                description = "Food web structure of a pelagic community within Tuesday Lake (2 years), Michigan, USA",
                public      = TRUE)


#trait <- list(date = "1111-11-11")






#------------------------------
# Cleaning matrix
#------------------------------
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Cohen_et_al_2003'

#Read the filesConochilus
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB355', 'WEB356')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB355.csv'), subset(name.dictionary, name.dictionary$web == 'WEB356.csv'))
names(names.of.web) <- c('WEB355', 'WEB356')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(split_name == "unclassified flagellates" | split_name == "unclassified microflagellates", "Flagellates", scientific_name)))
names.of.web[[1]][16,5] <- "conochilus"
names.of.web[[1]] <- names.of.web[[1]][-c(17),]
names.of.web <- map(names.of.web, ~select(.x, split_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$split_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$split_name, .x$scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, split_name = paste0(str_to_upper(str_extract(.x$split_name, ".{1}")), str_remove(.x$split_name, ".{1}"))))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = paste0(str_to_upper(str_extract(.x$scientific_name, ".{1}")), str_remove(.x$scientific_name, ".{1}"))))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\s-\\s)")
matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
names(matrices.interaction) <- c('WEB355', 'WEB356')

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'Cryptomonas sp. 1' | sp_taxon_1 == 'Chroococcus dispersus' | 
                                                                            sp_taxon_1 == "Chromulina sp." | sp_taxon_1 == "Selanastrum minutum" | 
                                                                            sp_taxon_1 == "Cryptomonas sp. 2" | sp_taxon_1 == "Dictyosphaerium pulchellum" | 
                                                                            sp_taxon_1 == "Dinobryon sociale" | sp_taxon_1 == "Glenodinium quadridens" | 
                                                                            sp_taxon_1 == "Chroococcus limneticus" | sp_taxon_1 == "Cryptomonas sp. 3" |  
                                                                            sp_taxon_1 == "Cryptomonas sp. 4" | sp_taxon_2 == "Cosmarium sp." | 
                                                                            sp_taxon_1 == "Dactylococcopsis fascicularis" | sp_taxon_1 == "Dictyosphaerium pulchellum" | 
                                                                            sp_taxon_1 == "Dinobryon sertularia" | sp_taxon_1 == "Sphaerocystis schroeteri" | 
                                                                            sp_taxon_1 == "Glenodinium pulvisculus" | sp_taxon_1 == "Oocystis sp. 1" | 
                                                                            sp_taxon_1 == "Oocystis sp. 2" | sp_taxon_1 == "Schroederia setigera" | 
                                                                            sp_taxon_1 == "Microcystis aeruginosa" | sp_taxon_1 == "Quadrigula lacustris" | 
                                                                            sp_taxon_1 == "Quadrigula sp. 2" | sp_taxon_1 == "Synedra sp." | 
                                                                            sp_taxon_1 == "Oscillatoria sp." | sp_taxon_1 == "Ankyra judayi" | 
                                                                            sp_taxon_1 == "Closteriopsis longissimus", 'herbivory', 'predation')))

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))

matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_1 = paste0(str_to_upper(str_extract(.x$sp_taxon_1, ".{1}")), str_remove(.x$sp_taxon_1, ".{1}"))))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_2 = paste0(str_to_upper(str_extract(.x$sp_taxon_2, ".{1}")), str_remove(.x$sp_taxon_2, ".{1}"))))

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

taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Conochilus (solitary)" | original_name == "Conochilus (colonial)", "Conochilus", name_clear)))
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

#changing names and adding taxo_ID
taxa_back_df[1,4] <- "665"
taxa_back_df[5,4] <- "1525"
taxa_back_df[8,4] <- "5927"
taxa_back_df[11,4] <- "58485"
taxa_back_df[14,4] <- "1530"
taxa_back_df[17,4] <- "750"
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(name_clear == "Diaptomus oregonensis", "Skistodiaptomus oregonensi", name_clear)))
taxa_back_df[22,c(1,4)] <- c("Skistodiaptomus oregonensis", "85845")
taxa_back_df[24,4] <- "113924"

# Writing taxo_back_df
write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa_back.csv'), row.names = FALSE)
saveRDS(taxons_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa.csv'))
saveRDS(matrices.interaction, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.csv'))

taxa_back_df <- read.csv2(file = paste0("mangal-datasets-ben/DBase_de_Patrick/",folder.name,"/data/",folder.name,"_taxa_back.csv"))
taxons_df <- readRDS("mangal-datasets-ben/DBase_de_Patrick/Cohen_et_al_2003/data/Cohen_et_al_2003_taxa.csv")
matrices.interaction <- readRDS(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.csv'))

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
meta_description <- c('(1984)', '(1986)')
meta_date <- c("1984-01-01","1986-01-01")
for (i in 1:2) {
  
  
  networks <- list(name            = paste0('Cohen_et_al_2003_',i),
                  date             = meta_date[i],
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0("Food web structure of a pelagic community within Tuesday Lake ",meta_description[i] ,", Michigan, USA"),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  inter <- list(date          = meta_date[i],
              direction     = "directed",
              method        = "unknown",
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
