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

lat  <- 43.7
lon  <- -77
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

refs <- list(doi       = "10.1016/j.ecolmodel.2010.10.024",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://www.sciencedirect.com/science/article/pii/S0304380010005843",
            data_url  = "https://globalwebdb.com/",
            author    = "Stewart",
            year      = "2011",
            bibtex    = "@article{Stewart_2011,	doi = {10.1016/j.ecolmodel.2010.10.024}, url = {https://doi.org/10.1016%2Fj.ecolmodel.2010.10.024}, year = 2011, month = {feb}, publisher = {Elsevier {BV}}, volume = {222}, number = {3}, pages = {692--708}, author = {Thomas. J. Stewart and W. Gary Sprules}, title = {Carbon-based balanced trophic structure and flows in the offshore Lake Ontario food web before (1987{\textendash}1991) and after (2001{\textendash}2005) invasion-induced ecosystem change}, journal = {Ecological Modelling}}")



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


datasets <- list(name        = "Stewart_Sprules_2011",
                date        = "1987-05-01",
                description = "Food web structure of the offshore community at different times, Lake Ontario, Ontario, Canada",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


#------------------------------
# Cleaning matrix
#------------------------------
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Stewart_Sprules_2011'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
matrices.interaction <- map(matrices.interaction, ~cbind(.x, .x[,19], .x[,19], .x[,19]))
matrices.interaction[[1]][,c(20,21,22)] <- lapply(matrices.interaction[[1]][,c(20,21,22)], as.character)
matrices.interaction[[2]][,c(20,21,22)] <- lapply(matrices.interaction[[2]][,c(20,21,22)], as.character)
matrices.interaction[[1]][1,c(19,20,21,22)] <- c("Oncorhynchus mykiss","Salvelinus namaycush","Oncorhynchus kisutch","Salmo trutta")
matrices.interaction[[2]][1,c(19,20,21,22)] <- c("Oncorhynchus mykiss","Salvelinus namaycush","Oncorhynchus kisutch","Salmo trutta")

#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB278', 'WEB279')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB278.csv'), subset(name.dictionary, name.dictionary$web == 'WEB279.csv'))
names(names.of.web) <- c('WEB278', 'WEB279')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~.x[-c(20,22,26),])
names.of.web[[1]]$split_name[c(19,20)] <- c("Smelt algae 1 and older", "Late YOY and older alewife")
names.of.web[[2]]$split_name[c(19,20)] <- c("Smelt algae 1 and older", "Late YOY and older alewife")
names.of.web <- map(names.of.web, ~select(.x, split_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$split_name), ])
names.of.web <- map(names.of.web, ~rbind(.x, `colnames<-`(data.frame(c("Oncorhynchus mykiss","Salvelinus namaycush","Oncorhynchus kisutch","Salmo trutta"),c("Oncorhynchus mykiss","Salvelinus namaycush","Oncorhynchus kisutch","Salmo trutta")), c("split_name","scientific_name"))))
names.of.web[[1]]$scientific_name[c(7,8,14,19)] <- c("Rotifera", "Dreissena","Benthos","osmeridae")
names.of.web[[2]]$scientific_name[c(7,8,14,19)] <- c("Rotifera", "Dreissena","Benthos","osmeridae")
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$split_name, .x$scientific_name)))
names.of.web[[1]]$split_name <- paste0(str_to_upper(str_extract(names.of.web[[1]]$split_name, ".{1}")), str_remove(names.of.web[[1]]$split_name, ".{1}"))
names.of.web[[1]]$scientific_name <- paste0(str_to_upper(str_extract(names.of.web[[1]]$scientific_name, ".{1}")), str_remove(names.of.web[[1]]$scientific_name, ".{1}"))
names.of.web[[2]]$split_name <- paste0(str_to_upper(str_extract(names.of.web[[2]]$split_name, ".{1}")), str_remove(names.of.web[[2]]$split_name, ".{1}"))
names.of.web[[2]]$scientific_name <- paste0(str_to_upper(str_extract(names.of.web[[2]]$scientific_name, ".{1}")), str_remove(names.of.web[[2]]$scientific_name, ".{1}"))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "s and ")
matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
names(matrices.interaction) <- c('WEB278', 'WEB279')
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_1 = ifelse(sp_taxon_1 == "Nanoflagellate", "Nanoflagellates", sp_taxon_1)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_2 = ifelse(sp_taxon_2 == "Nanoflagellate", "Nanoflagellates", sp_taxon_2)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_1 = ifelse(sp_taxon_1 == "Rotifer", "Rotifers", sp_taxon_1)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_2 = ifelse(sp_taxon_2 == "Rotifer", "Rotifers", sp_taxon_2)))

matrices.interaction[[1]]$sp_taxon_1 <- paste0(str_to_upper(str_extract(matrices.interaction[[1]]$sp_taxon_1, ".{1}")), str_remove(matrices.interaction[[1]]$sp_taxon_1, ".{1}"))
matrices.interaction[[1]]$sp_taxon_2 <- paste0(str_to_upper(str_extract(matrices.interaction[[1]]$sp_taxon_2, ".{1}")), str_remove(matrices.interaction[[1]]$sp_taxon_2, ".{1}"))
matrices.interaction[[2]]$sp_taxon_1 <- paste0(str_to_upper(str_extract(matrices.interaction[[2]]$sp_taxon_1, ".{1}")), str_remove(matrices.interaction[[2]]$sp_taxon_1, ".{1}"))
matrices.interaction[[2]]$sp_taxon_2 <- paste0(str_to_upper(str_extract(matrices.interaction[[2]]$sp_taxon_2, ".{1}")), str_remove(matrices.interaction[[2]]$sp_taxon_2, ".{1}"))


#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'Phytoplankton', 'herbivory', 'predation')))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'Dissolved organic carbon' | sp_taxon_1 == "Particulate organic carbon" | 
                                                                            sp_taxon_1 == "Sediment", 'commensalism', type)))

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
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Nanoflagellates", "Ciliophora", name_clear)))
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Rotifers", "Rotifera", name_clear)))
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Smelt YOY", "Osmeridae", name_clear)))
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Early YOY alewife", "Alosa pseudoharengus", name_clear)))

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
taxa_back_df[1,4] <- "46211"
taxa_back_df[2,4] <- "81338"

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
meta <- c('1987-05-01', '2001-05-01')
meta_name <- c("(1987-1991)","(2001-2005)")
for (i in 1:2) {
  
  
  networks <- list(name            = paste0('Stewart_Sprules_2011_',i),
                  date             = meta[i],
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0("Food web structure of the offshore community ",meta_name[i] ," in Lake Ontario, Ontario, Canada"),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  inter <- list(date        = meta[i],
              direction     = "directed",
              method        = "Field and laboratory observation, biblio",
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
