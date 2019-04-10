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

lat  <- 53.91219292148898
lon  <- -1.654015486039384
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

refs <- list(doi       = "10.2307/2256044",
             jstor     = "https://www.jstor.org/stable/2256044",
             pmid      = "NA",
             paper_url = "https://www.jstor.org/stable/2256044?casa_token=ASjMO-C34hYAAAAA:1MKrDZq_0SK2Ee8otlX5st9jFQwcPVwiAS6xeRY-BqSOdsT7Oy_GuRRvk_srJd-IktpXAH4EVMM85pxp2bq9vP06LMJAvvQ0wX6NGG80GACArpoQMLXMTA&seq=1#metadata_info_tab_contents",
             data_url  = "https://globalwebdb.com/",
             author    = "Percival",
             year      = "1929",
             bibtex    = "@article{Percival_1929, doi = {10.2307/2256044}, url = {https://doi.org/10.2307%2F2256044}, year = 1929, month = {aug}, publisher = {{JSTOR}}, volume = {17}, number = {2}, pages = {282}, author = {E. Percival and H. Whitehead}, title = {A Quantitative Study of the Fauna of Some Types of Stream-Bed}, journal = {The Journal of Ecology}}")



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


datasets <- list(name        = "Percival_Whitehead_1929",
                 date        = "1926-06-09",
                 description = "General food web structure within streams bed, West Riding of Yorkshire, England",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Percival_Whitehead_1929",
                 date             = "1926-06-09",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "General food web structure within streams bed, West Riding of Yorkshire, England",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "1926-06-09",
              direction     = "directed",
              method        = "unknown",
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
foldername <- 'Percival_Whitehead_1929'
food_web_name <- 'WEB210'
#Read the files
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
matrice.interaction <- read.csv(file=paste0('mangal-datasets-ben/DBase_de_Patrick/',foldername,'/raw/', food_web_name, '.csv'), header= T, sep=',', stringsAsFactors = F
                                #, skip=1
                                )
#Getting only the web related names and filling the NA's scientific names with their original name
name.of.web <- subset(name.dictionary, name.dictionary$web == paste0(food_web_name,'.csv'))
name.of.web <- select(name.of.web, original_name, scientific_name)
name.of.web[56,1] <- "insectivorous birds"
name.of.web[57,1] <- "bats"
name.of.web <- name.of.web[!duplicated(name.of.web$original_name), ]
name.of.web <- name.of.web[-48,]
name.of.web$scientific_name[is.na(name.of.web$scientific_name)] <- name.of.web$original_name[is.na(name.of.web$scientific_name)]
name.of.web <- mutate(name.of.web, scientific_name = ifelse(scientific_name == "diatoms (unspec.)", "Diatoms", scientific_name))
#Edge format
colnames(matrice.interaction) = matrice.interaction[1,]
matrice.interaction <- matrice.interaction[-1,]
matrice.interaction <- melt(matrice.interaction, id.vars = c(1), na.rm = TRUE)
names(matrice.interaction) <- c("sp_taxon_1", "sp_taxon_2", "value")
matrice.interaction <- subset(matrice.interaction, matrice.interaction$value != 0)

#Funciton to depooled the grouped name
listafterdepooled <- sep_pooled(matrice.interaction, sep = "[:space:]&[:space:]")
matrice.interaction <- listafterdepooled[1]
matrice.interaction <- matrice.interaction$depooled
matrice.interaction <- mutate(matrice.interaction, sp_taxon_2 = ifelse(sp_taxon_2 == "bats (unspec.)", "bats", sp_taxon_2))
#matrice.interaction$sp_taxon_1 <-  paste0(str_to_upper(str_extract(matrice.interaction$sp_taxon_1, ".{1}")), str_remove(matrice.interaction$sp_taxon_1, ".{1}"))
#matrice.interaction$sp_taxon_2 <-  paste0(str_to_upper(str_extract(matrice.interaction$sp_taxon_2, ".{1}")), str_remove(matrice.interaction$sp_taxon_2, ".{1}"))

#Adding the different types of interactions
matrice.interaction[['type']] <- NA
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == 'filamentous algae (unspec.)' | sp_taxon_1 == "unicellular algae (unspec.)" |
                                                                 sp_taxon_1 == "diatoms (unspec.)" | sp_taxon_1 == "mosses (unspec.)" | 
                                                                 sp_taxon_1 == "desmids (unspec.)", 'herbivory', 'predation'))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == 'detritus', 'commensalism', type))

#Changing the column's place
matrice.interaction <- matrice.interaction[, c(2, 1, 3, 4)]
matrice.interaction <- `colnames<-`(matrice.interaction, c("sp_taxon_1", "sp_taxon_2", "value", "type"))


#Taxa
taxa.original <- unique(c(as.vector(matrice.interaction$sp_taxon_2), as.vector(matrice.interaction$sp_taxon_1)))
taxa.clear <- str_replace_all(taxa.original, fixed(deframe(name.of.web)))
taxons_df <- data.frame(original_name= taxa.original, name_clear= taxa.clear, stringsAsFactors = F)
taxons_df$name_clear <-  paste0(str_to_upper(str_extract(taxons_df$name_clear, ".{1}")), str_remove(taxons_df$name_clear, ".{1}"))
taxons_df <- mutate(taxons_df, name_clear = ifelse(name_clear == "Perla cephalotes (5 mm)", "Perla cephalotes", name_clear))
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
taxa_back_df[5,4] <- "605174"
taxa_back_df[6,4] <- "601421"
taxa_back_df[19,4] <- "179985"

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
