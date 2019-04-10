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

lat  <- 40.12517293316066
lon  <- -88.15126506852903
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

refs <- list(doi       = "10.2307/1948602",
            jstor     = "https://www.jstor.org/stable/1948602",
            pmid      = "NA",
            paper_url = "https://www.jstor.org/stable/pdf/1948602.pdf?refreqid=excelsior%3Af69f4db640a00f160fa66423eabb4c38",
            data_url  = "http://digitalcommons.rockefeller.edu/context/cohen_joel_laboratory/article/1000/type/native/viewcontent",
            author    = "Twomey",
            year      = "1945",
            bibtex    = "@article{Twomey_1945, doi = {10.2307/1948602}, url = {https://doi.org/10.2307%2F1948602}, year = 1945, month = {feb}, publisher = {Wiley}, volume = {15}, number = {2}, pages = {173--205}, author = {Arthur C. Twomey}, title = {The Bird Population of an Elm-Maple Forest with Special Reference to Aspection, Territorialism, and Coactions}, journal = {Ecological Monographs}}")


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


datasets <- list(name        = "Twomey_1945",
                date        = "1934-11-01",
                description = "The interactions among bird populations in an elm-maple forest (aestival period), Trelease Woods, Illinois",
                public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Twomey_1945",
                date             = "1934-11-01",
                lat              = lat,
                lon              = lon,
                srid             = srid,
                description      = "The interactions among bird populations in an elm-maple forest (aestival period), Trelease Woods, Illinois",
                public           = TRUE,
                all_interactions = FALSE)


inter <- list(date          = "1934-11-01",
              direction     = "directed",
              method        = "Field observation",
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
foldername <- 'Twomey_1945'
food_web_name <- 'WEB59'
#Read the files
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
matrice.interaction <- read.csv(file=paste0('mangal-datasets-ben/DBase_de_Patrick/',foldername,'/raw/', food_web_name, '.csv'), header= T, sep=',', stringsAsFactors = F
                                #, skip=1
                                )
#Getting only the web related names and filling the NA's scientific names with their original name
name.of.web <- subset(name.dictionary, name.dictionary$web == paste0(food_web_name,'.csv'))
name.of.web <- select(name.of.web, split_name, scientific_name)
name.of.web <- name.of.web[!duplicated(name.of.web$original_name), ]
name.of.web[c(1:6,8),2] <- "Plantae"
name.of.web[7,2] <- "Detritus"
name.of.web[19,2] <- "Insectae"
name.of.web[28,1] <- "maryland yellow-throat"
name.of.web$scientific_name[is.na(name.of.web$scientific_name)] <- name.of.web$original_name[is.na(name.of.web$scientific_name)]
name.of.web$split_name <-  paste0(str_to_upper(str_extract(name.of.web$split_name, ".{1}")), str_remove(name.of.web$split_name, ".{1}"))
name.of.web$scientific_name <-  paste0(str_to_upper(str_extract(name.of.web$scientific_name, ".{1}")), str_remove(name.of.web$scientific_name, ".{1}"))

#Edge format
colnames(matrice.interaction) = matrice.interaction[1,]
matrice.interaction <- matrice.interaction[-1,]
matrice.interaction <- melt(matrice.interaction, id.vars = c(1), na.rm = TRUE)
names(matrice.interaction) <- c("sp_taxon_1", "sp_taxon_2", "value")
matrice.interaction <- subset(matrice.interaction, matrice.interaction$value != 0)

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled(matrice.interaction, sep = "(\\s-\\s|[:space:]and[:space:])")
matrice.interaction <- listafterdepooled[1]
matrice.interaction <- matrice.interaction$depooled
matrice.interaction$sp_taxon_1 <-  paste0(str_to_upper(str_extract(matrice.interaction$sp_taxon_1, ".{1}")), str_remove(matrice.interaction$sp_taxon_1, ".{1}"))
matrice.interaction$sp_taxon_2 <-  paste0(str_to_upper(str_extract(matrice.interaction$sp_taxon_2, ".{1}")), str_remove(matrice.interaction$sp_taxon_2, ".{1}"))


#Adding the different types of interactions
matrice.interaction[['type']] <- NA
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == 'Leaves' | sp_taxon_1 == 'Sap' | sp_taxon_1 == 'Plant juices' | 
                                                                 sp_taxon_1 == 'Roots' | sp_taxon_1 == 'Bark' | sp_taxon_1 == 'Wood' | sp_taxon_1 == 'Flowers' | 
                                                                 sp_taxon_1 == 'Acorns', 'herbivory', 'predation'))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == 'Dead organic material', 'commensalism', type))

matrice.interaction <- matrice.interaction[, c(2, 1, 3, 4)]
matrice.interaction <- `colnames<-`(matrice.interaction, c("sp_taxon_1", "sp_taxon_2", "value", "type"))

#Taxa
taxa.original <- unique(c(as.vector(matrice.interaction$sp_taxon_2), as.vector(matrice.interaction$sp_taxon_1)))
taxa.clear <- str_replace_all(taxa.original, fixed(deframe(name.of.web)))
taxons_df <- data.frame(original_name= taxa.original, name_clear= taxa.clear, stringsAsFactors = F)
taxons_df$name_clear <-  paste0(str_to_upper(str_extract(taxons_df$name_clear, ".{1}")), str_remove(taxons_df$name_clear, ".{1}"))
taxons_df[9,2] <- "Insectae"
taxons_df[14,2] <-"Hylocichla mustelina" 
taxons_df[27,2] <- "Plantae"

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
taxa_back_df[12,4] <- "177921"

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
