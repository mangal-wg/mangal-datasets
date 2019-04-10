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

lat  <- 50.9166667
lon  <- 11.583333333333334
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence/Absence",
                   table_owner = "interactions",
                   description = "Presence or absence of interaction",
                   unit        = "NA")

body_length <- list(name        = "Body size",
              #table_owner = "traits",
              description = "Average body length",
              unit        = "mm")

# attr2 <- list(name        = "NAME",    #help?
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "10.1002/ecy.2679",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/ecy.2679",
             data_url  = "https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/ecy.2679",
             author    = "Hines",
             year      = "2019",
             bibtex    = "@article{Hines_2019, doi = {10.1002/ecy.2679}, url = {https://doi.org/10.1002%2Fecy.2679}, year = 2019, month = {mar}, publisher = {Wiley}, pages = {e02679}, author = {Jes Hines and Darren P. Giling and Michael Rzanny and Winfried Voigt and Sebastian T. Meyer and Wolfgang W. Weisser and Nico Eisenhauer and Anne Ebeling}, title = {A meta-food web for invertebrate species collected in a european grassland}, journal = {Ecology}}")



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


datasets <- list(name        = "Hines_et_al_2019",
                 date        = "2003-05-13",
                 description = "Meta-food web structure of invertebrate community in a european grassland, Germany",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Hines_et_al_2019",
                 date             = "2003-05-13",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Meta-food web structure of invertebrate community in a european grassland, Germany",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "2003-05-13",
              direction     = "directed",
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

#Read the files
traits <- read.csv(file=paste0('mangal-datasets-ben/New_one/Hines_et_al_2019/raw/Jena_species_traits.csv'), header= T, sep=',', stringsAsFactors = F)
matrice.interaction <- read.csv(file=paste0('mangal-datasets-ben/New_one/Hines_et_al_2019/raw/Jena_trophic_interactions.csv'), header= T, sep=',', stringsAsFactors = F)

traits$genus.species <- str_replace(traits$genus.species, "_", " ")                              

#Edge format
matrice.interaction <- melt(matrice.interaction, id.vars = c(1), na.rm = TRUE)
names(matrice.interaction) <- c("sp_taxon_1", "sp_taxon_2", "value")
matrice.interaction <- subset(matrice.interaction, matrice.interaction$value != 0)
matrice.interaction$sp_taxon_2 <- as.character(matrice.interaction$sp_taxon_2)

#Matching names
matching <- data.frame(traits$species.code, traits$genus.species, stringsAsFactors = F)
matrice.interaction$sp_taxon_1 <- str_replace_all(matrice.interaction$sp_taxon_1, fixed(deframe(matching)))
matrice.interaction$sp_taxon_2 <- str_replace_all(matrice.interaction$sp_taxon_2, fixed(deframe(matching)))


#Getting the reign of each node to check for type of interaction
intertype <- data.frame(traits$genus.species, traits$trophic.group, stringsAsFactors = F)
matrice.interaction$typetaxon1 <- str_replace_all(matrice.interaction$sp_taxon_1, fixed(deframe(intertype)))
matrice.interaction$typetaxon2 <- str_replace_all(matrice.interaction$sp_taxon_2, fixed(deframe(intertype)))

#Adding the different types of interactions (order is important)
matrice.interaction[['type']] <- NA
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == "microbes" | sp_taxon_2 == "microbes", "unspecified", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(typetaxon2 == "omnivore", "predation", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(typetaxon2 == "omnivore" & typetaxon1 == "plant", "herbivory", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(typetaxon1 == "plant", "herbivory", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(typetaxon2 == "detritivore", "detritivore", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(typetaxon2 == "predator" | typetaxon2 == "predatorbrevis" | 
                                                                 typetaxon2 == "predatorferus" | typetaxon2 == "predatorpseudoferus", "predation", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse((typetaxon1 == "herbivore" | typetaxon1 == "detritivore" | typetaxon1 == "Atheta resource" | sp_taxon_1 == "fungi")& typetaxon2 == "herbivore", "unspecified", type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_2 == "Atheta fungi", 'detritivore', type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(typetaxon2 == "Atheta resourcecola", 'predation', type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse((sp_taxon_2 == "Atheta fungicola" & sp_taxon_1 == "fungi"), 'unspecified', type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == "algae" | sp_taxon_1 == "moss", 'herbivory', type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == 'carrion', 'scavenger', type))
matrice.interaction <- mutate(matrice.interaction, type = ifelse(sp_taxon_1 == 'dung' | sp_taxon_1 == "litter", 'detritivore', type))

#Replace columns order (from, to)
matrice.interaction <- matrice.interaction[, c(2, 1, 3, 6)]
matrice.interaction <- `colnames<-`(matrice.interaction, c("sp_taxon_1", "sp_taxon_2", "value", "type"))

#Adding the different method of inference of interaction
matrice.interaction[['method']] <- NA
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 1.0, "Specific feeding interaction reported in the literature", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 2.0, "Generalized feeding interaction reported in the literature", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 3.0, "Trophic level", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 4.0, "Trait-based rule", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 5.1, "Combined trait-based rule (5.1)", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 5.2, "Combined trait-based rule (5.2)", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 5.3, "Combined trait-based rule (5.3)", method))
matrice.interaction <- mutate(matrice.interaction, method = ifelse( value == 5.4, "Combined trait-based rule (5.4)", method))

matrice.interaction$value <- 1

#Change the name of Agonum mülleri to Agonum mulleri so the API doesnt crash while uploading the interactions
matrice.interaction <- mutate(matrice.interaction, sp_taxon_1 = ifelse(sp_taxon_1 == "Agonum mülleri", "Agonum mulleri", sp_taxon_1))
matrice.interaction <- mutate(matrice.interaction, sp_taxon_2 = ifelse(sp_taxon_2 == "Agonum mülleri", "Agonum mulleri", sp_taxon_2))


#Taxa
taxa.original <- unique(c(as.vector(matrice.interaction$sp_taxon_2), as.vector(matrice.interaction$sp_taxon_1)))
taxa.clear <- c()

for (i in 1:length(taxa.original)) {

  if(((str_detect(taxa.original[i], "[:digit:]") == TRUE || str_detect(taxa.original[i], "[:punct:]") == TRUE) &
       str_detect(taxa.original[i], "sp") == TRUE) ||
       str_detect(taxa.original[i], "n\\.i\\.") == TRUE ||
       str_detect(taxa.original[i], "\\([:alpha:]\\)") == TRUE ||
       str_detect(taxa.original[i], "sp$") == TRUE){
      
    taxa.clear[i] <- word(taxa.original[i], start = 1)

  } else {
    taxa.clear[i] <- taxa.original[i]
  }
}

taxa.clear <- gsub("\\s*\\([^\\)]+\\)", "", taxa.clear)
taxons_df <- data.frame(original_name= taxa.original, name_clear= taxa.clear, stringsAsFactors = F)
taxons_df$name_clear <-  paste0(str_to_upper(str_extract(taxons_df$name_clear, ".{1}")), str_remove(taxons_df$name_clear, ".{1}"))
taxons_df[363,2] <- "Thrips angusticeps"
taxons_df <- mutate(taxons_df, name_clear = ifelse(original_name == "microbes", "Microorganisms", name_clear))
taxons_df <- mutate(taxons_df, name_clear = ifelse(original_name == "Medicago x", "Medicago", name_clear))
taxons_df <- mutate(taxons_df, original_name = ifelse(original_name == "Agonum mülleri", "Agonum mulleri", original_name))
taxons_df <- mutate(taxons_df, name_clear = ifelse(name_clear == "Agonum mülleri", "Agonum mulleri", name_clear))


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
taxa_back_df[4,4] <- "29671"
taxa_back_df[37,4] <- "41395"
taxa_back_df[39,4] <- "509393"
taxa_back_df[43,4] <- "193446"
taxa_back_df[47,4] <- "41062"
taxa_back_df[48,4] <- "41163"
taxa_back_df[52,4] <- "93250"
taxa_back_df[76,4] <- "850413"
taxa_back_df[78,4]<- "864202"
taxa_back_df[102,4] <- "104181"
taxa_back_df[114,4] <- "867005"
taxa_back_df[126,4] <- "853465"
taxa_back_df[128,4] <- "854838"
taxa_back_df[129,4] <- "854894"
taxa_back_df[133,4] <- "856063"
taxa_back_df[154,4] <- "864536"
taxa_back_df[155,4] <- "864563"
taxa_back_df[156,4] <- "864576"
taxa_back_df[162,4] <- "889980"
taxa_back_df[164,4] <- "863803"
taxa_back_df[174,4] <- "892022"
taxa_back_df[176,4] <- "892200"
taxa_back_df[184,4] <- "860775"
taxa_back_df[205,4] <- "110879"
taxa_back_df[215,4] <- "109964"
taxa_back_df[226,4] <- "109551"
taxa_back_df[263,4] <- "805848"
taxa_back_df[346,4] <- "695891"
taxa_back_df[431,4] <- "720050"
taxa_back_df[576,4] <- "1087422"
taxa_back_df[577,4] <- "1087426"
taxa_back_df[597,4] <- "110516"
taxa_back_df[621,4] <- "106966"
taxa_back_df[636,4] <- "862575"
taxa_back_df[647,4] <- "1087388"
taxa_back_df[667,4] <- "852103"
taxa_back_df[669,4] <- "861665"
taxa_back_df[670,4] <- "890926"
taxa_back_df[671,4] <- "884121"
taxa_back_df[675,4] <- "109367"
taxa_back_df[682,4] <- "109368"
taxa_back_df[683,4] <- "109370"
taxa_back_df[684,4] <- "109618"
taxa_back_df[690,4] <- "718773"


#------------------------------
# Set traits table
#------------------------------
traits_df <- data.frame(traits$genus.species, name = "body size", traits$size, stringsAsFactors = F)
names(traits_df) <- c("taxon", "name", "value")
traits_df <- mutate(traits_df, taxon = ifelse(taxon == "Agonum mülleri", "Agonum mulleri", taxon))
traits_df$taxon <- as.character(traits_df$taxon)
traits_df$name <- as.character(traits_df$name)
# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = paste0('mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_taxa_back.csv'), row.names = FALSE)
write.csv2(x = taxons_df, file = paste0('mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_taxa.csv'), row.names = FALSE)
write.csv2(x = matrice.interaction, file = paste0('mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_inter.csv'), row.names = FALSE)
 write.csv2(x = traits_df, file = "mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_traits.csv", row.names = FALSE)

# taxa_back_df_1 <- read.csv2("mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_taxa_back.csv", header = TRUE)
# taxons_df <- read.csv2("mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_taxa.csv", header = TRUE)
# matrice.interaction <- read.csv2("mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_inter.csv", header = TRUE)
# traits_df <- read.csv2("mangal-datasets-ben/New_one/Hines_et_al_2019/data/Hines_et_al_2019_traits.csv", header = TRUE)

#------------------------------
# Throwing injection functions
#------------------------------
POST_attribute(attr = attr_inter)
POST_attribute(attr = body_length)
# POST_attributes(attr2)
POST_ref(ref = refs)
POST_users(users = users)
# POST_environment(enviro, attr_##)
POST_dataset(dataset = datasets, users = users, ref = refs)
POST_network(network_lst = networks, enviro = enviro, dataset = datasets, users = users)
POST_taxonomy(taxo = taxa_back_df)
POST_node(node_df = taxons_df, network = networks)
POST_trait(trait_df = traits_df, network = networks, trait = body_length)
POST_interaction(inter_df = matrice.interaction, attr = attr_inter, inter = inter, users = users, network = networks, enviro = enviro)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxa_back_df, matrice.interaction, name.dictionary, name.of.web, foldername, food_web_name, i, path, server, taxa.clear, taxa.original, listafterdepooled)
