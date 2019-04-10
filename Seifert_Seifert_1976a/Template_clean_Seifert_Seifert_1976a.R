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

lat  <- 8.7068775
lon  <- -83.48878
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

refs <- list(doi       = "10.1086/283080",
            jstor     = "http://links.jstor.org/sici?sici=0003-0147%28197605%2F06%29110%3A973%3C461%3AACMAOH%3E2.0.CO%3B2-T",
            pmid      = "NA",
            paper_url = "https://www.journals.uchicago.edu/doi/10.1086/283080",
            data_url  = "https://www.globalwebdb.com/",
            author    = "Seifert",
            year      = "1976a",
            bibtex    = "@article{Seifert_1976,	doi = {10.1086/283080},  url = {https://doi.org/10.1086%2F283080},  year = 1976,  month = {may}, publisher = {University of Chicago Press}, volume = {110}, number = {973}, pages = {461--483}, author = {Richard P. Seifert and Florence Hammett Seifert}, title = {A Community Matrix Analysis of Heliconia Insect Communities}, journal = {The American Naturalist}}")



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


datasets <- list(name        = "Seifert_Seifert_1976a",
                date        = "1970-07-01",
                description = "The insect communities in two Heliconia plant species on the Peninsula de Osa, Costa Rica",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


inter <- list(date          = "1970-07-01",
              direction     = "directed",
              method        = "Field observation/experiment",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
# Cleaning matrix
#------------------------------
library(dplyr)
library(stringr)
library(reshape2)
library(tibble)
library(purrr)

folder.name <- 'Seifert_Seifert_1976a'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB141', 'WEB142')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB141.csv'), subset(name.dictionary, name.dictionary$web == 'WEB142.csv'))
names(names.of.web) <- c('WEB141', 'WEB142')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))
names.of.web[[1]] <- mutate(names.of.web[[1]], scientific_name = ifelse(original_name == "floral parts" | original_name == "nectar" | original_name == "young leaves", "Heliconia imbricata", scientific_name))
names.of.web[[2]] <- mutate(names.of.web[[2]], scientific_name = ifelse(original_name == "floral parts" | original_name == "nectar" | original_name == "young leaves", "Heliconia wagneriana", scientific_name))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 

#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'detritus', 'commensalism', 'herbivory')))

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

# Writing taxo_back_df
write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa_back2.csv'), row.names = FALSE)
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
meta <- c('Heliconia imbricata', 'Heliconia wagneriana')
meta_name <- c("Heliconia_imbricata", "Heliconia_wagneriana")
for (i in 1:2) {
  
  
  networks <- list(name             = paste0('Seifert_Seifert_1976a_', meta_name[i]),
                  date             = "1970-07-01",
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste('The insect community in', meta[i], 'on the Peninsula de Osa, Costa Rica'),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
