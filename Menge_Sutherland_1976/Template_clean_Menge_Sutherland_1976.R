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

refs <- list(doi       = "10.1086/283073",
            jstor     = "https://www.jstor.org/stable/2459759",
            pmid      = "NA",
            paper_url = "https://www.journals.uchicago.edu/doi/pdfplus/10.1086/283073",
            data_url  = "https://globalwebdb.com/",
            author    = "Menge",
            year      = "1976",
            bibtex    = "@article{Menge_1976,	doi = {10.1086/283073}, url = {https://doi.org/10.1086%2F283073}, year = 1976, month = {may}, publisher = {University of Chicago Press}, volume = {110}, number = {973}, pages = {351--369}, author = {Bruce A. Menge and John P. Sutherland}, title = {Species Diversity Gradients: Synthesis of the Roles of Predation, Competition, and Temporal Heterogeneity}, journal = {The American Naturalist}}")



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


datasets <- list(name        = "Menge_Sutherland_1976",
                date        = "1973-03-01",
                description = "Food web structure of rocky intertidal communities in New England and Washington",
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

folder.name <- 'Menge_Sutherland_1976'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB10', 'WEB11', 'WEB12', 'WEB13')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB10.csv'), subset(name.dictionary, name.dictionary$web == 'WEB11.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB12.csv'), subset(name.dictionary, name.dictionary$web == 'WEB13.csv'))
names(names.of.web) <- c('WEB10', 'WEB11', 'WEB12', 'WEB13')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'detritus', 'commensalism', 'predation')))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'algae', 'herbivory', type)))

matrices.interaction[[1]] <- rbind(matrices.interaction[[1]], c('Mytilus edulis', 'Balanus balanoides', '1', 'competition'))
matrices.interaction[[2]] <- rbind(matrices.interaction[[2]], c('Mytilus edulis', 'Balanus balanoides', '1', 'competition'))
matrices.interaction[[3]] <- rbind(matrices.interaction[[3]], c('Mytilus californianus', 'acorn barnacles', '1', 'competition'))
matrices.interaction[[4]] <- rbind(matrices.interaction[[4]], c('Mytilus californianus', 'acorn barnacles', '1', 'competition'))

matrices.interaction <- map(matrices.interaction, ~cbind(.x, direction="directed", stringsAsFactors = F))
matrices.interaction[[1]][7,5] <- "undirected"
matrices.interaction[[2]][11,5] <- "undirected"
matrices.interaction[[3]][27,5] <- "undirected"
matrices.interaction[[4]][30,5] <- "undirected"

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4, 5)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type", "direction")))

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

taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Thais lapillus", "Nucella lapillus", name_clear)))
taxons_df <- map(taxons_df, ~mutate(.x, name_clear = ifelse(original_name == "Acmaea testudinalis", " 	Tectura testudinalis", name_clear)))

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
taxa_back_df[4,4] <- "89475"
taxa_back_df[7,4] <- "73820"

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
meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Menge_Sutherland_1976/raw/meta.txt', header=T, sep='\t', stringsAsFactors = F)
meta_nom <- c("exposed_New_England","protected_New_England","exposed_Washington","protected_Washington")
for (i in 1:4) {
  
  
  networks <- list(name             = paste0("Menge_Sutherland_1976_", meta_nom[i]),
                  date             = "1973-03-011",
                  lat              = meta[i,2],
                  lon              = meta[i,3],
                  srid             = srid,
                  description      = paste0("Food web structure of ", meta[i,1]),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  inter <- list(date          = "1973-03-01",
                method        = "Field observation/experiment",
                description   = "null",
                public        = TRUE,
                lat           = meta[i,2],
                lon           = meta[i,3],
                srid          = srid)
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
