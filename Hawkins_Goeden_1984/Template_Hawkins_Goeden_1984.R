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

lat  <- c(33.30420943531294, 34.58573529760528, 34.987090779836365, 33.66120005241923)
lon  <- c(-116.1144026986218, -117.8136213368631, -116.95451293620528, -116.00348229968301)
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

refs <- list(doi       = "10.1111/j.1365-2311.1984.tb00851.x",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2311.1984.tb00851.x",
            data_url  = "http://digitalcommons.rockefeller.edu/context/cohen_joel_laboratory/article/1000/type/native/viewcontent",
            author    = "Hawkins",
            year      = "1984",
            bibtex    = "@article{HAWKINS_1984,	doi = {10.1111/j.1365-2311.1984.tb00851.x},	url = {https://doi.org/10.1111%2Fj.1365-2311.1984.tb00851.x},	year = 1984,	month = {aug},	publisher = {Wiley},	volume = {9},	number = {3},	pages = {271--292},	author = {BRADFORD A. HAWKINS and RICHARD D. GOEDEN},	title = {Organization of a parasitoid community associated with a complex of galls on Atriplex spp. in southern California},	journal = {Ecological Entomology}}")



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


datasets <- list(name        = "Hawkins_Goeden_1984",
                date        = "1979-07-14",
                description = "Food web structure of parasitoid communities associated with galls on Atriplex sp., southern California, USA",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


inter <- list(date          = "1979-07-14",
              direction     = "directed",
              method        = "Field and laboratory observations",
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
folder.name <- 'Hawkins_Goeden_1984'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB168', 'WEB169', 'WEB170', 'WEB171','WEB172', 'WEB173', 'WEB174', 'WEB175', 'WEB176', 'WEB177', 'WEB178')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB168.csv'), subset(name.dictionary, name.dictionary$web == 'WEB169.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB170.csv'), subset(name.dictionary, name.dictionary$web == 'WEB171.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB172.csv'), subset(name.dictionary, name.dictionary$web == 'WEB173.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB174.csv'), subset(name.dictionary, name.dictionary$web == 'WEB175.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB176.csv'), subset(name.dictionary, name.dictionary$web == 'WEB177.csv'),
                     subset(name.dictionary, name.dictionary$web == 'WEB178.csv'))
names(names.of.web) <- c('WEB168', 'WEB169', 'WEB170', 'WEB171','WEB172', 'WEB173', 'WEB174', 'WEB175', 'WEB176', 'WEB177', 'WEB178')

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name), ])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'gall tissue (Atriplex canescens)', 'Atriplex canescens', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'gall tissue (Atriplex polycarpa)', 'Atriplex polycarpa', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(original_name == 'Rileyi piercei', 'Rileya piercei', scientific_name)))
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))
#Modifying special character in the names, so the sending to server doesnt fail
for(i in 1:length(names.of.web)){
  names.of.web[[i]]$original_name <- str_replace(names.of.web[[i]]$original_name, "&", "and")
  names.of.web[[i]]$scientific_name <- str_replace(names.of.web[[i]]$scientific_name, "&", "and")
}

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 


#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value != 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_2 == 'Phyllobaenus atriplexus', 'predation', type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 =="gall tissue (Atriplex canescens)" & (sp_taxon_2 == "Neolasioptera willistoni" | sp_taxon_2 == "Nesolasioptera willistoni (woody/smooth stem maker)" | 
                                                                                                                               sp_taxon_2 == "Ophiomyia atriplicis (leaf cluster bud gall maker)" | sp_taxon_2 == "Asphondylia sp. A (club stem maker)" |
                                                                                                                               sp_taxon_2 == "Asphondylia sp. B (nodular stem gallmaker)" | sp_taxon_2 == "Asphondylia sp. C (woody stem maker)" | 
                                                                                                                               sp_taxon_2 == "Asphondylia sp. D (blister leaf & nipple bud gall maker)" | sp_taxon_2 == "Asphondylia sp. E (tumor stem maker)" |
                                                                                                                               sp_taxon_2 == "Asphondylia sp. F (oval twig maker)" | sp_taxon_2 == "Asphondylia sp. G (oval bud/flower gall maker)" | 
                                                                                                                               sp_taxon_2 == "Asphondylia sp. H (fuzzy bud/flower gall maker)"), "parasitism", type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse((sp_taxon_1 == "Asphondylia sp. E (tumor stem maker)" | sp_taxon_1 == "Asphondylia sp. F (oval twig maker)" | 
                                                                             sp_taxon_1 == "Asphondylia sp. G (oval bud/flower gall maker)" | sp_taxon_1 == "Asphondylia sp. H (fuzzy bud/flower gall maker)" | 
                                                                             sp_taxon_1 == "Asphondylia sp. A (club stem maker)" | sp_taxon_1 == "Asphondylia sp. B (nodular stem gallmaker)" |
                                                                             sp_taxon_1 == "Asphondylia sp. C (woody stem maker)" | sp_taxon_1 == "Asphondylia sp. D (blister leaf & nipple bud gall maker)" | 
                                                                             sp_taxon_1 == "Nesolasioptera willistoni (woody/smooth stem maker)" | sp_taxon_1 == "Neolasioptera willistoni" | sp_taxon_1 == "Ophiomyia atriplicis (leaf cluster bud gall maker)") & 
                                                                                                                                                                                                                (sp_taxon_2 == 'Torymus c. capillaceus' | sp_taxon_2 == "Lestodiplosis sp." | sp_taxon_2 == "Platygaster sp. B" |
                                                                                                                                                                                                                 sp_taxon_2 == "Rileya piercei" | sp_taxon_2 == "Tenuipetiolus medicaginus" | 
                                                                                                                                                                                                                 sp_taxon_2 == "Torymus atriplicis" | sp_taxon_2 == "Spilochalcis sp." | sp_taxon_2 == "Torymus umbilicatus" | 
                                                                                                                                                                                                                 sp_taxon_2 == "Pseudocatolaccus guizoti" | sp_taxon_2 == "Rileya tegularis" | 
                                                                                                                                                                                                                 sp_taxon_2 == "Tetrastichus sp. A" | sp_taxon_2 == "Tetrastichus sp. B" | 
                                                                                                                                                                                                                 sp_taxon_2 == "Tetrastichus sp. C" | sp_taxon_2 == "Eurytoma sp." | sp_taxon_2 == "Galeopsomyia sp. B" | 
                                                                                                                                                                                                                 sp_taxon_2 == "Galeopsomyia sp. D" | sp_taxon_2 == "Galeopsomyia sp. E" | sp_taxon_2 == "Torymus n. sp." | sp_taxon_2 == "Galeopsomyia sp. C" | 
                                                                                                                                                                                                                 sp_taxon_2 == "Platygaster sp. A" | sp_taxon_2 == "Opius sp." | sp_taxon_2 == "Platygaster sp. A"), 'parasitism', type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse((sp_taxon_1 == "gall tissue (Atriplex polycarpa)" | sp_taxon_1 == "gall tissue (Atriplex canescens)") & (sp_taxon_2 == "Torymus c. capillaceus" | sp_taxon_2 == "Rileya tegularis"), "herbivory", type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse((sp_taxon_1 == "gall tissue (Atriplex polycarpa)" | sp_taxon_1 == "gall tissue (Atriplex canescens)" & is.na(type)), "herbivory", type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse((sp_taxon_2 == "Torymus c. capillaceus" & is.na(type)), "parasitism", type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(((sp_taxon_1 == "Asphondylia sp. F" | sp_taxon_1 == "Asphondylia sp. C" | 
                                                                             sp_taxon_1 == "Asphondylia sp. E" | sp_taxon_1 == "Asphondylia sp. G" | 
                                                                             sp_taxon_1 == "Asphondylia sp. B" | sp_taxon_1 == "Asphondylia sp. H" | 
                                                                             sp_taxon_1 == "Asphondylia sp. A" | sp_taxon_1 == "Asphondylia sp. D") & is.na(type)), "parasitism", type)))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(is.na(type), "unknown", type)))

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3, 4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))

#Modifying special character in the names, so the sending to server doesnt fail
for(i in 1:length(matrices.interaction)){
matrices.interaction[[i]]$sp_taxon_1 <- str_replace(matrices.interaction[[i]]$sp_taxon_1, "&", "and")
matrices.interaction[[i]]$sp_taxon_2 <- str_replace(matrices.interaction[[i]]$sp_taxon_2, "&", "and")
}

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
taxa_back_df[11,4] <- "154012"
taxa_back_df[17,4] <- "153969"

# Writing taxo_back_df
write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa_back.csv'), row.names = FALSE)
saveRDS(taxons_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa.csv'))
saveRDS(matrices.interaction, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.csv'))
taxons_df <- readRDS("Hawkins_Goeden_1984_taxa.csv", file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxa.csv'))
matrices.interaction <- readRDS("Hawkins_Goeden_1984_inter.csv", file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_inter.csv'))

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
meta <- read.table(file="mangal-datasets-ben/DBase_de_Patrick/Hawkins_Goeden_1984/raw/meta.txt", header = T, sep="\t", stringsAsFactors = F)
for (i in 1 : length(matrices.interaction)) {
  
  
  networks <- list(name             = paste0("Hawkins_Goeden_1984_",i),
                  date             = "1979-07-14",
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0("Food web structure of the ",meta[i,1],", southern California, USA"),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxa_back_df, matrices.interaction, names.of.web, name.dictionary, meta, meta_name, filenames, folder.name, i, path, server, foldername, taxa.clear, taxa.original, listafterdepooled)
