# Set libraries
library(reshape2)
library(data.table)
library(tidyr)
library(jsonlite)
library(httr)
library(rcrossref)
library(taxize)
library(stringr)
library(tibble)
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

refs <- list(doi       = "10.1007/978-94-017-2934-5_28",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://link.springer.com/article/10.1023/A:1014594408119",
            data_url  = "https://globalwebdb.com/",
            author    = "Alcorlo",
            year      = "2001",
            bibtex    = "@incollection{Alcorlo_2001,	doi = {10.1007/978-94-017-2934-5_28}, url = {https://doi.org/10.1007%2F978-94-017-2934-5_28}, year = 2001, publisher = {Springer Netherlands}, pages = {307--316}, author = {Paloma Alcorlo and Angel Baltan{\'{a}}s and Carlos Montes}, title = {Food-web structure in two shallow salt lakes in Los Monegros ({NE} Spain): energetic vs dynamic constraints}, booktitle = {Saline Lakes}}")



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


datasets <- list(name       = "alcorlo_et_al_2001",
                date        = "1994-11-01",
                description = "The biotic interactions within two shallow salt lake, Los Monegros, Spain",
                public      = TRUE)


#trait <- list(date = "1111-11-11")



#------------------------------
# Cleaning matrix
#------------------------------
library(dplyr)
library(purrr)
library(stringr)
library(reshape2)
library(tibble)

folder.name <- 'Alcorlo_et_al_2001'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('Piñol 94/95', 'La muerte 94/95', 'Piñol 95/96', 'La muerte 95/96')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB334.csv'), subset(name.dictionary, name.dictionary$web == 'WEB335.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB336.csv'), subset(name.dictionary, name.dictionary$web == 'WEB337.csv'))
names(names.of.web) <- c('Piñol 94/95', 'La muerte 94/95', 'Piñol 95/96', 'La muerte 95/96')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, original_name = ifelse(original_name == 'clay+bacteria', 'clay and bacteria', original_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'clay and bacteria', 'bacteria', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Correctiong the scientific name based in the article (not complete in the meta-analysis sheet)
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Hexarthra', 'Hexarthra fennica', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Cletocamptus', 'Cletocamptus retrogressus', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Arctodisptomus', 'Arctodiaptomus salinus', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Fabrea', 'Fabrea salina', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Branchinectella', 'Branchinectella media', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Agabus', 'Agabus nebulosus', scientific_name)))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 


#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'algae', 'herbivory', 'predation')))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'detritus', 'commensalism', type)))

#Adding the different types of method
matrices.interaction <- map(matrices.interaction, ~cbind(.x, method=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, method = ifelse(sp_taxon_2 == 'Branchinectella' | sp_taxon_2 == 'Candela', 'Gut content', 'Laboratory observation and biblio')))

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4, 5)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type", "method")))

#Replacing clay+bacteria by clay and bacteria in the interaction matrices
matrices.interaction <- map(matrices.interaction, ~mutate(.x, sp_taxon_2 = ifelse(sp_taxon_2 == "clay+bacteria", "clay and bacteria", sp_taxon_2)))

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
write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa_back.csv'), row.names = FALSE)
saveRDS(taxons_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa.Rdata'))
saveRDS(matrices.interaction, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.Rdata'))

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
meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Alcorlo_et_al_2001/raw/meta.txt', header=T, sep='\t', stringsAsFactors = F)
meta$Lake <- stringr::str_replace_all(meta$Lake, "\\s", "_")
for (i in 2:4) {
  
  networks <- list(name            = paste0('alcorlo_et_al_2001_',paste0(meta[i,1],"_",meta[i,2])),
                  date             = meta[i,2],
                  lat              = meta[i,3],
                  lon              = meta[i,4],
                  srid             = srid,
                  description      = paste0("The biotic interactions within the ", meta[i,1],', Los Monegros, Spain'),
                  public           = TRUE,
                  all_interactions = FALSE)
    
  inter <- list(date          = meta[i,2],
                direction     = "directed",
                description   = "null",
                public        = TRUE,
                lat           = meta[i,3],
                lon           = meta[i,4],
                srid          = srid)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server)
