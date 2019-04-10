# Set libraries
library(reshape2)
library(tidyr)
#library(jsonlite)
#library(httr)
library(data.table)
library(rcrossref)
library(taxize)
library(stringr)

library(mangal)

#------------------------------
  # Metadata
#------------------------------

lat  <- 28.216667
lon  <- -16.633333
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "pollinator recorded on a flower",
                   table_owner = "interactions",
                   description = "Presence or absence of interaction",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "null")

refs <- list(doi       = "10.1034/j.1600-0587.2003.03443.x",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://onlinelibrary.wiley.com/doi/full/10.1034/j.1600-0587.2003.03443.x",
             data_url  = "http://www.web-of-life.es/map.php#",
             author    = "Dupont",
             year      = "2003",
             bibtex    = "@article{Dupont_2003, doi = {10.1034/j.1600-0587.2003.03443.x}, url = {https://doi.org/10.1034%2Fj.1600-0587.2003.03443.x},	year = 2003,	month = {jun},	publisher = {Wiley},	volume = {26},	number = {3},	pages = {301--310},	author = {Yoko L. Dupont and Dennis M. Hansen and Jens M. Olesen},	title = {Structure of a plant-flower-visitor network in the high-altitude sub-alpine desert of Tenerife, Canary Islands},	journal = {Ecography}}")


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


datasets <- list(name        = "Dupont_et_al_2003",
                 date        = "2001-05-07",
                 description = "Plant-pollinator network in a sub-alpine desert on the island of Tenerife, Canary Islands",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Dupont_et_al_2003",
                 date             = "2001-05-07",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Plant-pollinator network in a sub-alpine desert on the island of Tenerife, Canary Islands",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "2001-05-07",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field observation",
              description   = "Pollinator collecting/eating eather nectar or pollen",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)



#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
FW_name <- read.csv2(file = "mangal-datasets-ben/Ones_done_before_Patrick/Dupont_et_al_2003/raw/M_PL_008.csv", header = TRUE, sep = ",")

# Melt df
FW_name <- melt(FW_name, id.vars = c(1), na.rm = TRUE)
names(FW_name) <- c("sp_taxon_1", "sp_taxon_2", "value")

#remove uncessary string
without_. <- str_remove_all(FW_name$sp_taxon_2, '\\.M_PL_[:digit:]+')
without_. <- gsub("\\.${1}", "", without_.)
without_. <- gsub('.', ' ', without_., fixed=T)
FW_name[,2] <- without_.

# Remove interaction value = 0 (no interaction)
FW_name <- subset(FW_name, FW_name$value != 0)
FW_name <- FW_name[,c(2,1,3)]
FW_name <- `colnames<-`(FW_name, c("sp_taxon_1", "sp_taxon_2", "value"))

#------------------------------
# Set taxo_back and taxon table
#------------------------------

## Get Unique taxon of data
taxa <- c(as.vector(unique(FW_name$sp_taxon_2)), as.vector(unique(FW_name$sp_taxon_1)))

### Check for spelling mistakes... ###
taxa_resolve <- gnr_resolve(taxa, canonical = F, best_match_only = T)
temp <- unlist(attributes(taxa_resolve)$not_known)

if(length(temp) != 0){
  sp_not_known <- data.frame(user_supplied_name=unlist(attributes(taxa_resolve)$not_known) , submitted_name=unlist(attributes(taxa_resolve)$not_known), matched_name=unlist(attributes(taxa_resolve)$not_known), NA, NA)
  names(sp_not_known) <- names(taxa_resolve)
  sp_not_known$submitted_name <-  paste0(str_to_upper(str_extract(sp_not_known$user_supplied_name, ".{1}")), str_remove(sp_not_known$user_supplied_name, ".{1}"))
  sp_not_known$matched_name <- sp_not_known$submitted_name
  taxa_resolve <- rbind(taxa_resolve, sp_not_known)
}
# Create taxa_df
taxons_df <- data.frame(taxa, taxa_resolve$matched_name, stringsAsFactors = F)
names(taxons_df) <- c("original_name", "name_clear")
taxons_df$name_clear <- str_remove_all(taxons_df$name_clear, '\\s\\(.*\\)$')
taxons_df$name_clear <- str_replace_all(taxons_df$name_clear, c('Geron hesperidum Frey, 1936' = 'Geron hesperidum', 'Hylaeus canariensis Erlandsson, 1983'= 'Hylaeus canariensis', 'Osmia canadensis Cresson, 1864'='Osmia canadensis', 'Andrena wollastoni Cockerell, 1922'='Andrena wollastoni', 'Argyranthemum teneriffae Humphries'='Argyranthemum teneriffae'))


### Creating taxa_back
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
taxa_back_df[23,4] <- "758002"
taxa_back_df[38,4] <- "1011199"

#------------------------------
# Set traits table
#------------------------------


# traits_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_traits.csv", header = TRUE)

# traits_df <- melt(traits_df, id.vars = c("taxa"), na.rm = TRUE)
# names(traits_df) <- c("taxa", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Dupont_et_al_2003/data/Dupont_et_al_2003_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxons_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Dupont_et_al_2003/data/Dupont_et_al_2003_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Dupont_et_al_2003/data/Dupont_et_al_2003_inter.csv", row.names = FALSE)
# write.csv2(x = traits_df, file = "mangal-datasets/FW_name/data/FW_name_traits.csv", row.names = FALSE)

# taxa_back_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa_back.csv", header = TRUE)
# taxa_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_taxa.csv", header = TRUE)
# FW_name <- read.csv2("mangal-datasets/FW_name/data/FW_name_inter.csv", header = TRUE)
# traits_df <- read.csv2("mangal-datasets/FW_name/data/FW_name_traits.csv", header = TRUE)

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
POST_interaction(inter_df = FW_name, attr = attr_inter, inter = inter, users = users, network = networks, enviro = enviro)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrice.interaction, FW_name, taxa_back_df, taxa_resolve, i, path, server, taxa, temp, without_., without_str)
