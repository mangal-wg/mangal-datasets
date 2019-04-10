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

lat  <- 46.54
lon  <- 6.23
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence or absence of interaction",
                   unit        = "NA")


# attr1 <- list(name        = "name",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "name",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")


refs <- list(doi       = 'https://doi.org/10.1038/280311a0',
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://www.nature.com/articles/280311a0",
            data_url  = "https://www.globalwebdb.com/",
            author    = "Rejmanek and Stary",
            year      = "1979",
            bibtex    = "@article{REJM_NEK_1979, doi = {10.1038/280311a0}, url = {https://doi.org/10.1038%2F280311a0}, year = 1979, month = {jul}, publisher = {Springer Nature}, volume = {280}, number = {5720}, pages = {311--313},  author = {M. REJM{\'{A}}NEK and P. STAR{\'{Y}}}, title = {Connectance in real biotic communities and critical values for stability of model ecosystems}, journal = {Nature}")


users <- list(name         = "Benjamin Mercier",
             email        = "Benjamin.b.mercier@usherbrooke.ca",
             orcid        = "null",
             organization = "Universite de Sherbrooke",
             type         = "user")


datasets <- list(name        = "rejmanek_stary_1979",
                date        = "1979-07-26",
                description = "Canopy interaction of plant-aphid-parasitoid communities, Central Europe",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


inter <- list(date          = "1979-07-26",
              direction     = "directed",
              type          = "parasitism",
              method        = "Field/laboratory observations",
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

#Read the files
filenames <- list.files("mangal-datasets-ben/DBase_de_Patrick/rejmanek_stary_1979/raw", pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
names(matrices.interaction) <- c('WEB145', 'WEB146', 'WEB147', 'WEB148')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB145.csv'), subset(name.dictionary, name.dictionary$web == 'WEB146.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB147.csv'), subset(name.dictionary, name.dictionary$web == 'WEB148.csv'))

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))
  
#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
                        map(~slice(.x, 2:nrow(.x))) %>%
                        map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
                        map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))
                        
#Filter the false interaction  
#web145
WEB145_sp1 <- filter(matrices.interaction[[1]], sp_taxon_1 %in% c('Symydobius oblongus', 'Euceraphis betulae', 'Betulaphis quadrituberculata', 'Calaphis betulicola', 'Betulaphis brevipilosa', 'Hamamelistes betulinus') &
                     sp_taxon_2 %in% c('Aphelinus chaonia', 'Trioxys betulae', 'T. compressicornis', 'Praon flavinode', 'Aphidius aquilus', 'Calaphidius elegans') &
                     value == 0) 
WEB145_sp2 <- filter(matrices.interaction[[1]], value == 1) 
matrices.interaction[[1]] <- rbind(WEB145_sp1, WEB145_sp2)
#web146
WEB146_sp1 <- filter(matrices.interaction[[2]], sp_taxon_1 %in% c('Symydobius oblongus', 'Euceraphis betulae', 'Betulaphis quadrituberculata', 'Calaphis betulicola', 'Betulaphis brevipilosa', 'Hamamelistes betulinus', 'Thelaxes dryophila', 'Stomaphis quercus', 'Tuberculoides annulatus', 'Myzocallis castanicola') &
                     sp_taxon_2 %in% c('Aphelinus chaonia', 'Trioxys betulae', 'T. compressicornis', 'Praon flavinode', 'Aphidius aquilus', 'Calaphidius elegans', 'A. quercicola', 'Aphidencyrtus aphidivorus', 'T. tenuicaudus', 'T. curvicaudus', 'Trioxys pallidus', 'Protaphidius wissmanii', 'Lysiphlebus thelaxis') &
                     value == 0)
WEB146_sp2 <- filter(matrices.interaction[[2]], value == 1)
matrices.interaction[[2]] <- rbind(WEB146_sp1, WEB146_sp2)
#web147
WEB147_sp1 <- filter(matrices.interaction[[3]], sp_taxon_1 %in% c('Cinara pini', 'C. pinea', 'Eulachnus agilis', 'Schizolachnus pineti') &
                       sp_taxon_2 %in% c('Pauesia picta', 'P. laricia', 'P. jezoensis', 'P. pini', 'Praon bicolor', 'Diaeretus leucopterus', 'Pauesia unilachni') &
                       value == 0)
WEB147_sp2 <- filter(matrices.interaction[[3]], value == 1)  
matrices.interaction[[3]] <- rbind(WEB147_sp1, WEB147_sp2) 
#web148
WEB148_sp1 <- filter(matrices.interaction[[4]], sp_taxon_1 %in% c('Thelaxes dryophila', 'Stomaphis quercus', 'Tuberculoides annulatus', 'Myzocallis castanicola') &
                       sp_taxon_2 %in% c('Praon flavinode', 'A. quercicola', 'Aphidencyrtus aphidivorus', 'T. tenvicaudus', 'T. curvicaudus', 'Trioxys pallidus', 'Protaphidius wissmanii', 'Lysiphlebus thelaxis') &
                       value == 0)
WEB148_sp2 <- filter(matrices.interaction[[4]], value == 1)
matrices.interaction[[4]] <- rbind(WEB148_sp1, WEB148_sp2)

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value")))

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
write.csv2(x = taxa_back_df, file = 'mangal-datasets-ben/DBase_de_Patrick/rejmanek_stary_1979/data/rejmanek_stary_1979_taxo_back.csv', row.names = FALSE)
saveRDS(taxons_df, file = "mangal-datasets-ben/DBase_de_Patrick/rejmanek_stary_1979/data/rejmanek_stary_1979_taxa.csv")
saveRDS(matrices.interaction, file = "mangal-datasets-ben/DBase_de_Patrick/rejmanek_stary_1979/data/rejmanek_stary_1979_inter.csv")

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
meta <- c('Betula pendula', 'Quercus and Betula', 'Pinus silvestris', 'Quercus robur')
meta_name <- c("Betula","Quercus_Betula","Pinus","Quercus")
for (i in 1:4) {
  
  
  networks <- list(name             = paste0('rejmanek_stary_1979_', meta_name[i]),
                  date             = "1979-07-26",
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste("System of Ahpid/parasitoid on", meta[i], 'in Central Europe'),
                  public           = TRUE,
                  all_interactions = FALSE)


  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
