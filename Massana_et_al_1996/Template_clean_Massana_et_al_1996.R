# Set libraries
library(tidyr)
#library(jsonlite)
library(httr)
library(data.table)
library(rcrossref)
library(taxize)

library(mangal)

#------------------------------
# Metadata
#------------------------------

lat  <- 42.125707
lon  <- 2.741469
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

refs <- list(doi       = "10.3354/ame011279",
            jstor     = "NA",
            pmid      = "NA",
            paper_url = "https://www.int-res.com/abstracts/ame/v11/n3/p279-288/",
            data_url  = "https://globalwebdb.com/",
            author    = "Massana",
            year      = "1996",
            bibtex    = "@article{Massana_1996,	doi = {10.3354/ame011279}, url = {https://doi.org/10.3354%2Fame011279}, year = 1996, publisher = {Inter-Research Science Center}, volume = {11}, pages = {279--288}, author = {R Massana and J Garcia-Cantizano and C Pedros-Alio}, title = {Components, structure and fluxes of the microbial food web in a small, stratified lake}, journal = {Aquatic Microbial Ecology}}")


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


datasets <- list(name        = "Massana_et_al_1996",
                date        = "1990-01-01",
                description = "Food web structure of species groups with same trophic habits in a small holomictic lake (Lake Ciso), Girona, Spain",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Massana_et_al_1996",
                date             = "1990-01-01",
                lat              = lat,
                lon              = lon,
                srid             = srid,
                description      = "Food web structure of species groups with same trophic habits in a small holomictic lake (Lake Ciso), Girona, Spain",
                public           = TRUE,
                all_interactions = FALSE)


inter <- list(date          = "1990-01-01",
              direction     = "directed",
              method        = "Field/Laboratory observation and biblio",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
# Cleaning matrix
#------------------------------

library(stringr)
library(reshape2)
library(tibble)
foldername <- 'Massana_et_al_1996'
food_web_name <- 'WEB277'
#Read the files
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
#matrice.interaction <- read.csv(file=paste0('mangal-datasets-ben/DBase_de_Patrick/',foldername,'/raw/', food_web_name, '.csv'), header= T, sep=',', stringsAsFactors = F
                                #, skip=1
                                #)
#Getting only the web related names and filling the NA's scientific names with their original name
name.of.web <- subset(name.dictionary, name.dictionary$web == paste0(food_web_name,'.csv'))
name.of.web <- select(name.of.web, original_name, scientific_name)
name.of.web <- name.of.web[!duplicated(name.of.web$original_name), ]
name.of.web[8,c(1,2)] <- "Thermocyclops dybowskii"
name.of.web <- rbind(name.of.web, `colnames<-`(data.frame(c("Polyarthra sp.","Keratella sp.","Coleps sp.","Prorodon sp.","Anuraeopsis fissa","Nauplii sp.","Vorticella sp.", "Paramecium sp.", "Cyclidium sp.", "Strombidium sp.","Chromatium sp.","Amoebobacter sp."), c("Polyarthra","Keratella","Coleps","Prorodon","Anuraeopsis fissa","Nauplii","Vorticella", "Paramecium", "Cyclidium", "Strombidium","Chromatium","Amoebobacter")), c("original_name","scientific_name")))
name.of.web <- name.of.web[-c(3,5,6,7),]
name.of.web$scientific_name[is.na(name.of.web$scientific_name)] <- name.of.web$original_name[is.na(name.of.web$scientific_name)]

#Edge format
# colnames(matrice.interaction) = matrice.interaction[1,]
# matrice.interaction <- matrice.interaction[-1,]
# matrice.interaction <- melt(matrice.interaction, id.vars = c(1), na.rm = TRUE)
# names(matrice.interaction) <- c("sp_taxon_1", "sp_taxon_2", "value")
# matrice.interaction <- subset(matrice.interaction, matrice.interaction$value != 0)
matrice.interaction <- read.csv(file=paste0('mangal-datasets-ben/DBase_de_Patrick/Massana_et_al_1996/raw/matrix.csv'), header= T, sep=';', stringsAsFactors = F)

#Adding the different types of interactions
matrice.interaction[['type']] <- NA
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_2 == 'Cryptomonas' | sp_taxon_2 == 'Other algae' | sp_taxon_2 == 'Chromatium sp.' | sp_taxon_2 == "Amoebobacter sp.", 'herbivory', 'predation'))

#Taxa
taxa.original <- unique(c(as.vector(matrice.interaction$sp_taxon_2), as.vector(matrice.interaction$sp_taxon_1)))
taxa.clear <- str_replace_all(taxa.original, fixed(deframe(name.of.web)))
taxons_df <- data.frame(original_name= taxa.original, name_clear= taxa.clear, stringsAsFactors = F)
taxons_df$name_clear <-  paste0(str_to_upper(str_extract(taxons_df$name_clear, ".{1}")), str_remove(taxons_df$name_clear, ".{1}"))
taxons_df[5,2] <- "Algae"

#Taxa_back
taxa_back <- taxons_df$name_clear
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


#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',foldername,'/data/',foldername,'_taxa_back.csv'), row.names = FALSE)
write.csv2(x = taxons_df, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',foldername,'/data/',foldername,'_taxa.csv'), row.names = FALSE)
write.csv2(x = matrice.interaction, file = paste0('mangal-datasets-ben/DBase_de_Patrick/',foldername,'/data/',foldername,'_inter.csv'), row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# trait_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attribute(attr = attr_inter)
# POST_attributes(attr1)
# POST_attributes(attr2)
POST_ref(ref = refs)
POST_users(users = users)
# POST_environment(enviro, attr_##)
POST_dataset(dataset = datasets, users = users, ref = refs)
POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
POST_taxonomy(taxo = taxa_back_df)
POST_node(node_df = taxons_df, network = networks)
# POST_traits(trait_df)
POST_interaction(inter_df = matrice.interaction, attr = attr_inter, inter = inter, users = users, network = networks, enviro = enviro)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxa_back_df, matrice.interaction, name.dictionary, name.of.web, foldername, food_web_name, i, path, server, taxa.clear, taxa.original)
