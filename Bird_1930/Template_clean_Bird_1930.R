# Set libraries
library(reshape2)
library(tidyr)
library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)
library(dplyr)
#library(mangal)

#------------------------------
# Metadata
#------------------------------

lat  <- 50.4218
lon  <- -101.04599999999999
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

refs <- list(doi       = "10.2307/1930270",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://esajournals.onlinelibrary.wiley.com/doi/pdf/10.2307/1930270",
            data_url  = "http://digitalcommons.rockefeller.edu/context/cohen_joel_laboratory/article/1000/type/native/viewcontent",
            author    = "Bird",
            year      = "1930",
            bibtex    = "@article{Bird_1930, doi = {10.2307/1930270}, url = {https://doi.org/10.2307%2F1930270}, year = 1930, month = {apr}, publisher = {Wiley}, volume = {11}, number = {2}, pages = {356--442}, author = {Ralph D. Bird}, title = {Biotic Communities of the Aspen Parkland of Central Canada}, journal = {Ecology}}")



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


datasets <- list(name        = "Bird_1930",
                date        = "1928-01-01",
                description = "The biotic interactions of different communities of the Aspen Parkland, North America",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


inter <- list(date          = "1928-01-01",
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
library(tidyverse)

folder.name <- 'Bird_1930'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB23', 'WEB24', 'WEB25', 'WEB26')

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~gather(.x, key = "sp_taxon_2", value = "value", -1)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))
  
#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value > 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction[[1]] <-  mutate(matrices.interaction[[1]], type = ifelse(sp_taxon_1 == 'Agropyron, Stipa, Helianthus', 'herbivory', 'predation'))
matrices.interaction[[2]] <-  mutate(matrices.interaction[[2]], type = ifelse(sp_taxon_1 == 'Salix discolor' | sp_taxon_1 == 'Salix petiolaris' | 
                                                                              sp_taxon_1 == 'Salix longifolia', 'herbivory', 'predation'))
matrices.interaction[[3]] <-  mutate(matrices.interaction[[3]], type = ifelse(sp_taxon_1 == 'Populus - Cornus - Corylus - Pyrola - Aralia' |
                                                                              sp_taxon_1 == 'Populus - Symphoricarpos - Corylus - Prunus - Amelanchier', 'herbivory', 'predation'))
matrices.interaction[[4]] <-  mutate(matrices.interaction[[4]], type = ifelse(sp_taxon_1 == 'Populus - Cornus - Corylus - Pryola - Aralia' |
                                                                              sp_taxon_1 == 'Agrostis - Agropyron - Stipa - Helianthus' | sp_taxon_1 == 'Salix longifolia' |
                                                                              sp_taxon_1 == 'Salix petiolaris' | sp_taxon_1 == 'Salix discolor', 'herbivory', 'predation'))
#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled_many(matrices.interaction, sep = "(\\s-\\s)|(\\, )")
matrices.interaction <- unlist(listafterdepooled[1], recursive = F)
names(matrices.interaction) <- c('WEB23', 'WEB24', 'WEB25', 'WEB26')
matrices.interaction <- map(matrices.interaction, ~.x[!(.x$sp_taxon_1=="etc." | .x$sp_taxon_2=='etc.'),])

matrices.interaction[[3]] <- rbind(matrices.interaction[[3]], matrices.interaction[[3]][c(33,34),])
matrices.interaction[[3]][c(33,34),2] <- "hairy woodpecker"
matrices.interaction[[3]][c(154,155),2] <- "downy woodpecker"
matrices.interaction[[3]] <- rbind(matrices.interaction[[3]], matrices.interaction[[3]][c(73:86),])
matrices.interaction[[3]][c(73:86),2] <- "Cooper's hawk"
matrices.interaction[[3]][c(156:169),2] <- "Sharpshinned hawk"

matrices.interaction[[4]] <- rbind(matrices.interaction[[4]], matrices.interaction[[4]][c(21,22),])
matrices.interaction[[4]][c(21,22),2] <- "hairy woodpecker"
matrices.interaction[[4]][c(169,170),2] <- "downy woodpecker"
matrices.interaction[[4]] <- rbind(matrices.interaction[[4]], matrices.interaction[[4]][c(41:51),])
matrices.interaction[[4]][c(41:51),2] <- "Cooper's hawk"
matrices.interaction[[4]][c(171:181),2] <- "Sharpshinned hawk"

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))

#Final matrice with all networks
#matrices.interaction <- taxa_df <- dplyr::bind_rows(matrices.interaction, .id = "network")

#--------------------
#Taxa
#--------------------
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB23.csv'), subset(name.dictionary, name.dictionary$web == 'WEB24.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB25.csv'), subset(name.dictionary, name.dictionary$web == 'WEB26.csv'))
names(names.of.web) <- c('WEB23', 'WEB24', 'WEB25', 'WEB26')


#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, split_name, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$split_name), ])
df_with_manualones <- data.frame(c("hairy woodpecker","downy woodpecker","Cooper's hawk","Sharpshinned hawk"), c("hairy woodpecker","downy woodpecker","Cooper's hawk","Sharpshinned hawk"),c("Picoides villosus", "Picoides pubescens","Accipiter cooperii","Accipiter striatus"))
colnames(df_with_manualones) <- c("split_name","original_name","scientific_name")
names.of.web <- map(names.of.web, ~rbind(.x, df_with_manualones))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Modifying manualy (pcq j'ai la flemme) the names.of.web to fit the names in matrices.interaction
names.of.web[[1]][4,1] <- "Richardson spermophile (ground squirrel)"
names.of.web[[1]][6,1] <- "vole (Microtus)"
names.of.web[[1]][8,1] <- "13-striped spermophile (ground squirrel)"
names.of.web[[1]][9,1] <- "pocket gopher (Thomomys)"
names.of.web[[3]][1,1] <- "spiders (mature forest)"
names.of.web[[3]][3,1]  <- "insects (mature forest)"
names.of.web[[3]][31,1] <- "redbacked vole (Evolomys)"
names.of.web[[3]][37,1] <- "insects (forest edge)"
names.of.web <- map(names.of.web, ~select(.x, c(1,3)))
names.of.web[[1]] <- names.of.web[[1]][-15,]
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(split_name == "canker", "canker", scientific_name)))
matrices.interaction[[1]]$sp_taxon_1 <- tolower(matrices.interaction[[1]]$sp_taxon_1)
matrices.interaction[[2]]$sp_taxon_1 <- tolower(matrices.interaction[[2]]$sp_taxon_1)
matrices.interaction[[3]]$sp_taxon_1 <- tolower(matrices.interaction[[3]]$sp_taxon_1)
matrices.interaction[[4]]$sp_taxon_1 <- tolower(matrices.interaction[[4]]$sp_taxon_1)
matrices.interaction[[1]]$sp_taxon_2 <- tolower(matrices.interaction[[1]]$sp_taxon_2)
matrices.interaction[[2]]$sp_taxon_2 <- tolower(matrices.interaction[[2]]$sp_taxon_2)
matrices.interaction[[3]]$sp_taxon_2 <- tolower(matrices.interaction[[3]]$sp_taxon_2)
matrices.interaction[[4]]$sp_taxon_2 <- tolower(matrices.interaction[[4]]$sp_taxon_2)
names.of.web[[1]]$split_name <- tolower(names.of.web[[1]]$split_name)
names.of.web[[2]]$split_name <- tolower(names.of.web[[2]]$split_name)
names.of.web[[3]]$split_name <- tolower(names.of.web[[3]]$split_name)
names.of.web[[4]]$split_name <- tolower(names.of.web[[4]]$split_name)
matrices.interaction <- map(matrices.interaction, ~unique(.x))

names.of.web[[1]]$scientific_name <- tolower(names.of.web[[1]]$scientific_name)
names.of.web[[2]]$scientific_name <- tolower(names.of.web[[2]]$scientific_name)
names.of.web[[3]]$scientific_name <- tolower(names.of.web[[3]]$scientific_name)
names.of.web[[4]]$scientific_name <- tolower(names.of.web[[4]]$scientific_name)

taxa.original <- map(matrices.interaction, ~tolower(unique(c(as.vector(.x$sp_taxon_2), as.vector(.x$sp_taxon_1)))))
taxa.clear <- map(names.of.web, ~tolower(deframe(.x))) %>%
              map2(taxa.original, ~str_replace_all(.y, fixed(.x)))
library(plyr)
taxa.clear <- map(taxa.clear, ~mapvalues(.x, c("13-striped spermophile (marmotini)", "insects (araneae)", "spiders (forest edge)"), c("ictidomys tridecemlineatus", "insecta", "araneae")))



#Getting the names that were pooled to put them through gnr_resolve
# names_gnr <- unique(unlist(listafterdepooled[2])) %>%
#              gnr_resolve(canonical = F, best_match_only = T) %>%
#              select(c(1,3)) %>%
#              `names<-`(c("original_name", "scientific_name")) 
# names.of.web <- map(names.of.web, ~rbind(.x, names_gnr))

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
  try (expr = (taxa_back_df[i, 2] <- get_boldid(taxa_back_df[i, 1], row = 10, verbose = FALSE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 3] <- get_eolid(taxa_back_df[i, 1], row = 10, verbose = FALSE, key = 110258)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 4] <- get_tsn(taxa_back_df[i, 1], row = 10, verbose = FALSE, accepted = TRUE)[1]), silent = TRUE)
  try (expr = (taxa_back_df[i, 5] <- get_uid(taxa_back_df[i, 1], row = 10, verbose = FALSE)[1]), silent = TRUE)
}

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
meta <- c('the prairie community', 'the willow community', 'the aspen community', 'all communities')
name_fornetworks <- c('prairie_community', 'willow_community', 'aspen_community', 'all_communities')
for (i in 1:4) {
  
  
  networks <- list(name             = paste0('Bird_1930_',name_fornetworks[[i]]),
                  date             = "1928-01-01",
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0("The biotic interactions of ", meta[i], ' in the Aspen Parkland, North America'),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
