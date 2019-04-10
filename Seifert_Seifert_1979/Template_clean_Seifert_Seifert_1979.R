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

lat  <- 10.342535
lon  <- -67.688221
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

refs <- list(doi       = "10.2307/1936064",
            jstor     = "https://www.jstor.org/stable/1936064",
            pmid      = "NA",
            paper_url = "https://esajournals.onlinelibrary.wiley.com/doi/abs/10.2307/1936064",
            data_url  = "https://www.globalwebdb.com/",
            author    = "Seifert",
            year      = "1979",
            bibtex    = "@article{Seifert_1979, doi = {10.2307/1936064}, url = {https://doi.org/10.2307%2F1936064}, year = 1979, month = {jun}, publisher = {Wiley}, volume = {60}, number = {3}, pages = {462--467}, author = {Richard P. Seifert and Florence Hammett Seifert}, title = {A Heliconia Insect Community in a Venezuelan Cloud Forest}, journal = {Ecology}}")




users <- list(name         = "Benjamin Mercier",
              email        = "Benjamin.b.mercier@usherbrooke.ca",
              orcid        = "null",
              organization = "Universite de Sherbrooke",
              type         = "User")

# 
# enviro <- list(name  = "attribute name",
#                lat   = lat,
#                lon   = lon,
#                srid  = srid,
#                date  = "1111-11-11",
#                value = 0)


datasets <- list(name        = "Seifert_Seifert_1979",
                date        = "1976-02-01",
                description = "The insect community of Heliconia bihai in a Venezuelan cloud forest",
                public      = TRUE)


#trait <- list(date = "1111-11-11")

networks <- list(name             = "Seifert)Seifert_1979",
                date             = "1976-02-01",
                lat              = lat,
                lon              = lon,
                srid             = srid,
                description      = "The insect community of Heliconia bihai in a Venezuelan cloud forest",
                public           = TRUE,
                all_interactions = FALSE)


inter <- list(date          = "1976-02-01",
              method        = "Field observation/experiment",
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

food_web_name <- 'WEB140'
#Read the files
name.dictionary <- read.csv2(file='Trophic-metacommunities-master/Trophic_metacom_meta_analysis/Data/name_dictionary.csv', header = T, sep=',', stringsAsFactors = F)
matrice.interaction <- read.csv(file=paste0('mangal-datasets-ben/DBase_de_Patrick/Seifert_Seifert_1979/raw/', food_web_name, '.csv'), header= T, sep=',', stringsAsFactors = F
                                #, skip=1
                                )
#Getting only the web related names and filling the NA's scientific names with their original name
name.of.web <- subset(name.dictionary, name.dictionary$web == paste0(food_web_name,'.csv'))
name.of.web <- name.of.web[,c(2,5)]
name.of.web$scientific_name[is.na(name.of.web$scientific_name)] <- name.of.web$original_name[is.na(name.of.web$scientific_name)]
name.of.web <- mutate(name.of.web, scientific_name = ifelse(original_name == "floral parts" | original_name == "nectar", "Heliconia bihai", scientific_name))
name.of.web[7,1] <- "Gillisius sp.3"
#Edge format
colnames(matrice.interaction) = matrice.interaction[1,]
matrice.interaction <- matrice.interaction[-1,]
matrice.interaction <- melt(matrice.interaction, id.vars = c(1), na.rm = TRUE)
names(matrice.interaction) <- c("sp_taxon_1", "sp_taxon_2", "value")
matrice.interaction <- subset(matrice.interaction, matrice.interaction$value != 0)
matrice.interaction <- rbind(matrice.interaction, c('Cephaloleia neglecta', 'Cephaloleia neglecta', '1'), c('Copestuylum roraima', 'Cephaloleia neglecta', '1'), c('Cephaloleia neglecta', 'Gillisius sp. #3', '1'))

#Adding the different types of interaction
matrice.interaction <- cbind(matrice.interaction, type=NA)
matrice.interaction <- mutate(matrice.interaction, type = ifelse(str_detect(matrice.interaction$sp_taxon_1, 'detritus'), 'commensalism','herbivory'))

#Adding interacation that were in the article but not in matrices (competition/symbiotic)
matrice.interaction[['type']][9:10] <- c('competition')
matrice.interaction[['type']][11] <- c('symbiosis')

matrice.interaction <- cbind(matrice.interaction, direction="directed", stringsAsFactors = F)
matrice.interaction[c(9:11),5] <- "undirected"

matrice.interaction <- matrice.interaction[, c(2, 1, 3,4 ,5)]
matrice.interaction <- `colnames<-`(matrice.interaction, c("sp_taxon_1", "sp_taxon_2", "value", "type", "direction"))
matrice.interaction$sp_taxon_1 <- as.character(matrice.interaction$sp_taxon_1)
matrice.interaction <- mutate(matrice.interaction, sp_taxon_1 = ifelse(sp_taxon_1 == "Gillisius sp. #3", "Gillisius sp.3", sp_taxon_1))

#Taxa
taxa.original <- unique(c(as.vector(matrice.interaction$sp_taxon_2), as.vector(matrice.interaction$sp_taxon_1)))
taxa.clear <- str_replace_all(taxa.original, fixed(deframe(name.of.web)))
taxons_df <- data.frame(original_name= taxa.original, name_clear= taxa.clear, stringsAsFactors = F)
taxons_df$name_clear <-  paste0(str_to_upper(str_extract(taxons_df$name_clear, ".{1}")), str_remove(taxons_df$name_clear, ".{1}"))

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

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/DBase_de_Patrick/Seifert_Seifert_1979/data/Seifert_Seifert_1979_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxons_df, file = "mangal-datasets-ben/DBase_de_Patrick/Seifert_Seifert_1979/data/Seifert_Seifert_1979_taxa.csv", row.names = FALSE)
write.csv2(x = matrice.interaction, file = "mangal-datasets-ben/DBase_de_Patrick/Seifert_Seifert_1979/data/Seifert_Seifert_1979_inter.csv", row.names = FALSE)
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

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrice.interaction, name.dictionary, name.of.web, food_web_name, taxa.clear, taxa.original)
