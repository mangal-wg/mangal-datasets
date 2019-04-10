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

lat  <- 36.016379
lon  <- -78.979442
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

refs <- list(doi       = "10.2307/1943233",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://esajournals.onlinelibrary.wiley.com/doi/pdf/10.2307/1943233",
            data_url  = "http://digitalcommons.rockefeller.edu/context/cohen_joel_laboratory/article/1000/type/native/viewcontent",
            author    = "Savely_1939",
            year      = "1939",
            bibtex    = "@article{Savely_1939, doi = {10.2307/1943233}, url = {https://doi.org/10.2307%2F1943233}, year = 1939, month = {feb}, publisher = {Wiley}, volume = {9}, number = {3}, pages = {321--385}, author = {Harvey Epperson Savely}, title = {Ecological Relations of Certain Animals in Dead Pine and Oak Logs}, journal = {Ecological Monographs}}")


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


datasets <- list(name        = "Savely_1939",
                date        = "1936-09-01",
                description = "The food web structure of dead Quercus sp. and Pinus sp. logs at Duke Forest, North Carolina",
                public      = TRUE)


#trait <- list(date = "1111-11-11")

inter <- list(date          = "1936-09-01",
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

folder.name <- 'Savely_1939'

#Read the files
filenames <- list.files(paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name, '/raw/'), pattern="*.csv", full.names=TRUE)
matrices.interaction <- lapply(filenames, read.csv, stringsAsFactors = F)
#meta <- read.table(file='mangal-datasets-ben/DBase_de_Patrick/Foldername/raw/meta.txt', header=T, sep='\t')
names(matrices.interaction) <- c('WEB180', 'WEB181', 'WEB182', 'WEB183')
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
names.of.web <- list(subset(name.dictionary, name.dictionary$web == 'WEB180.csv'), subset(name.dictionary, name.dictionary$web == 'WEB181.csv'), 
                     subset(name.dictionary, name.dictionary$web == 'WEB182.csv'), subset(name.dictionary, name.dictionary$web == 'WEB183.csv'))
                     
names(names.of.web) <- c('WEB180', 'WEB181', 'WEB182', 'WEB183')

#Fixing the scientific.name in names.of.web because it's not up to date 2019/02/01
names.of.web[[1]] <- mutate(names.of.web[[1]], scientific_name = ifelse(original_name == 'sapwood', 'quercus', scientific_name))
names.of.web[[2]] <- mutate(names.of.web[[2]], scientific_name = ifelse(original_name == 'rotting wood', 'quercus', scientific_name))
names.of.web[[1]] <- mutate(names.of.web[[1]], scientific_name = ifelse(original_name == 'various larvae (unspecified)', 'Insectae', scientific_name))
names.of.web[[2]] <- mutate(names.of.web[[2]], scientific_name = ifelse(original_name == 'pollen (undet.)', 'pollen', scientific_name))
names.of.web[[3]] <- mutate(names.of.web[[3]], scientific_name = ifelse(original_name == 'sapwood', 'pinus', scientific_name))
names.of.web[[3]] <- mutate(names.of.web[[3]], scientific_name = ifelse(original_name == 'various larvae (unsp.)', 'Insectae', scientific_name))
names.of.web[[4]] <- mutate(names.of.web[[4]], scientific_name = ifelse(original_name == 'various larvae (unspecified)', 'Insectae', scientific_name))
names.of.web[[3]] <- mutate(names.of.web[[3]], scientific_name = ifelse(original_name == 'dead cerambycid', 'Cerambycidae', scientific_name))
names.of.web[[2]] <- mutate(names.of.web[[2]], scientific_name = ifelse(original_name == 'various larvae', 'Insectae', scientific_name))

#Getting only the web related names and filling the NA's scientific names with their original name
names.of.web <- map(names.of.web, ~select(.x, original_name, scientific_name))
names.of.web <- map(names.of.web, ~.x[!duplicated(.x$original_name),])
names.of.web <- map(names.of.web, ~mutate(.x, scientific_name = 
                                            ifelse(is.na(.x$scientific_name), .x$original_name, .x$scientific_name)))

#Edge format
matrices.interaction <- map(matrices.interaction, ~`colnames<-`(.x, .x[1,])) %>%
  map(~slice(.x, 2:nrow(.x))) %>%
  map(~melt(.x, id.vars = c(1), na.rm = TRUE)) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value"))) 


#Filter the false interaction# 
matrices.interaction <- map(matrices.interaction, ~subset(.x, .x$value > 0))

#Adding the different types of interaction
matrices.interaction <- map(matrices.interaction, ~cbind(.x, type=NA))
matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_1 == 'fungi' | sp_taxon_1 == 'phloem (Quercus spp.)' |
                                                                               sp_taxon_1 == 'sapwood' | sp_taxon_1 == 'rotting wood' | 
                                                                               sp_taxon_1 == 'fungi (red rot)' | sp_taxon_1 == 'pollen (undet.)' |
                                                                               sp_taxon_1 == 'phloem (Pinus spp.)' | sp_taxon_1 == 'rotting wood (Pinus spp.)', 'herbivory', type)))

matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_2 == 'Tachyta nana' | sp_taxon_2 == 'Chariessa pilosa' | 
                                                                              sp_taxon_2 == 'Alaus oculatus larvae' | sp_taxon_2 == 'Megaselia sp. larvae' |
                                                                              sp_taxon_2 == 'Linotaenia bidens' | sp_taxon_2 == 'Geophilus varians' | sp_taxon_2 == 'Chthonius spinosus' |
                                                                              sp_taxon_2 == 'C. longipalpus' | sp_taxon_2 == 'Pergamasus sp.' | sp_taxon_2 == 'Megisthanus sp.' | 
                                                                              sp_taxon_2 == 'Dendrolaelops sp.' | sp_taxon_2 == 'Staphylinus violacens' | sp_taxon_2 == 'Actium sp.' |
                                                                              sp_taxon_2 == 'Platysoma lecontei' | sp_taxon_2 == 'cecidomyid larvae (undet.)' | sp_taxon_2 == 'dolicopodid larvae (undet.)' |
                                                                              sp_taxon_2 == 'Scolopendra viridis' | sp_taxon_2 == 'Cryptops hyalina' | sp_taxon_2 == 'Otocryptops sexspinosus' |
                                                                              sp_taxon_2 == 'Bothropolys multidentatus' | sp_taxon_2 == 'Chelanops dentatus' | sp_taxon_2 == 'Podothrombium sp.' |
                                                                              sp_taxon_2 == 'Allothrombium pulvinus' | sp_taxon_2 == 'heteropteran (undet.)' | sp_taxon_2 == 'Tachymenis flavicauda' |
                                                                              sp_taxon_2 == 'Saprinus sp.' | sp_taxon_2 == 'Platysoma parallelum' | sp_taxon_2 == 'P. lecontei' |
                                                                              sp_taxon_2 == 'Cylistix cylindrica' | sp_taxon_2 == 'Thanasimus dubius' | sp_taxon_2 == 'Alaus myops larvae' |
                                                                              sp_taxon_2 == 'Temnochila virescens' | sp_taxon_2 == 'Rhizophagus cylindricus' | sp_taxon_2 == 'Sciara coprophila larvae' |
                                                                              sp_taxon_2 == 'Xylophagus fasciatus larvae' | sp_taxon_2 == 'Parasitus sp' | sp_taxon_2 == 'Dendrolaelaps sp.' |
                                                                              sp_taxon_2 == 'Chelanops oblongus' | sp_taxon_2 == 'C. virginnae' | sp_taxon_2 == 'Chelifer cancroides', 'predation', type)))

matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_2 == 'Atanycolus simplex' | sp_taxon_2 == 'Sparhius floridanus' | sp_taxon_2 == 'tachinid larvae' |
                                                                              sp_taxon_2 == 'Theresia monohammi larvae' | sp_taxon_2 == 'Sarcophaga sp. larvae' | sp_taxon_2 == 'Orussus sp. larvae' |
                                                                              sp_taxon_2 == 'Coeloides pissodis larvae', 'parasitism', type)))

matrices.interaction <- map(matrices.interaction, ~mutate(.x, type = ifelse(sp_taxon_2 == 'Camponotus herculeanus' | sp_taxon_2 == 'Crematogaster lineolata' | sp_taxon_2 == 'Ponera coarcta' |
                                                                              sp_taxon_2 =='Proceratium croceum' | sp_taxon_2 == 'Leptothorax curvispinosus' | sp_taxon_2 == 'Pheidole sp.' |
                                                                              sp_taxon_2 == 'Leptinothorax curvispinosus' | sp_taxon_2 == 'Ponera coarctata' | sp_taxon_2 == 'Lasius umbratus', 'scavenger', type)))

#Replacing the columns for : from, to
matrices.interaction <- map(matrices.interaction, ~.x[, c(2, 1, 3,4)]) %>%
  map(~`colnames<-`(.x, c("sp_taxon_1", "sp_taxon_2", "value", "type")))

#Final matrice with all networks
#matrices.interaction <- taxa_df <- dplyr::bind_rows(matrices.interaction, .id = "network")

#Taxa
taxa.original <- map(matrices.interaction, ~unique(c(as.vector(.x$sp_taxon_2), as.vector(.x$sp_taxon_1))))
taxa.clear <- map(names.of.web, ~deframe(.x)) %>%
              map2(taxa.original, ~str_replace_all(.y, fixed(.x)))
  #map2(taxa.original, ~str_replace_all(.y, .x))
  

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
taxa_back_df[18,4] <- "109678"
taxa_back_df[34,4] <- "575995"
taxa_back_df[67,4] <- "678561"
taxa_back_df[132,4] <- "702552"
taxa_back_df[160,4] <- "703431"
taxa_back_df[174,4] <- "199287"

# Writing taxo_back_df
write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',folder.name,'/data/',folder.name,'_taxo_back.csv'), row.names = FALSE)
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
meta <- c('rotting oak logs (1 year)', 'rotting oak logs (> 3 years)', 'rotting pine logs (1 year)', 'rotting pine logs (> 3 years)')
meta_name <- c("oak(1year)", "oak(>3years)", "pine(1year)", "pine(>3years)")
for (i in 1:4) {
  
  
  networks <- list(name             = paste0("Savely_1939_", meta_name[i]),
                  date             = "1936-09-01",
                  lat              = lat,
                  lon              = lon,
                  srid             = srid,
                  description      = paste0('The food web structure of ',meta[i],', Duke Forest, North Carolina'),
                  public           = TRUE,
                  all_interactions = FALSE)
  
  
  POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
  
  POST_node(node_df = taxons_df[[i]], network = networks)
  
  POST_interaction(inter_df = matrices.interaction[[i]], inter = inter, enviro = enviro, attr = attr_inter, users = users, network = networks)
}

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrices.interactions, names.of.web, name.dictionary, meta, filenames, folder.name, i, path, server, foldername)
