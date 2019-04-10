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

lat  <- 33.4
lon  <- 131.5
srid <- 4326

# Must fill all CAP fields; null fields optional

attr_inter <- list(name        = "Presence of interaction",
                   table_owner = "interactions",
                   description = "Presence near the flowering plant or interacting with it",
                   unit        = "NA")

# attr1 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

# attr2 <- list(name        = "NAME",
#               table_owner = "TABLE_OWNER",
#               description = "DESCRIPTION",
#               unit        = "NA")

refs <- list(doi       = "NA",
             jstor     = "NA",
             pmid      = "NA",
             paper_url = "https://repository.kulib.kyoto-u.ac.jp/dspace/handle/2433/156407",
             data_url  = "http://www.web-of-life.es/map.php",
             author    = "Yamazaki",
             year      = "2003",
             bibtex    = "@article{yamazaki2003flowering, title={Flowering phenology and anthophilous insect community in a grassland ecosystem at Mt. Yufu, Western Japan}, author={YAMAZAKI, Kyoko and KATO, Makoto}, year={2003}, publisher={Kyoto University Graduate School of Human and Environmental Studies}}")


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


datasets <- list(name        = "Yamazaki_Kato_2003",
                 date        = "2001-04-01",
                 description = "Interactions of a plant-pollinator community in a grassland ecosystem at Mt. Yufu, Western Japan",
                 public      = TRUE)


#trait <- list(date = "1111-11-11")


networks <- list(name             = "Yamazaki_Kato_2003",
                 date             = "2001-04-01",
                 lat              = lat,
                 lon              = lon,
                 srid             = srid,
                 description      = "Interactions of a plant-pollinator community in a grassland ecosystem at Mt. Yufu, Western Japan",
                 public           = TRUE,
                 all_interactions = FALSE)


inter <- list(date          = "2001-04-01",
              direction     = "directed",
              type          = "mutualism",
              method        = "Field capture",
              description   = "null",
              public        = TRUE,
              lat           = lat,
              lon           = lon,
              srid          = srid)


#------------------------------
  # Cleaning matrix
#------------------------------

# Open file
FW_name <- read.csv2(file = "mangal-datasets-ben/Ones_done_before_Patrick/Yamazaki_Kato_2003/raw/M_PL_053.csv", header = T, sep=',')

# Melt df
FW_name <- melt(FW_name, id.vars = c(1), na.rm = TRUE)
names(FW_name) <- c("sp_taxon_1", "sp_taxon_2", "value")

#remove uncessary string
without_str2 <- str_remove_all(FW_name$sp_taxon_2, '\\.M_PL_[:digit:]+')
without_str2 <- gsub('\\.', ' ', without_str2)
FW_name[,2] <- without_str2

# Remove interaction value = 0 (no interaction)
FW_name <- subset(FW_name, FW_name$value != 0)
FW_name <- FW_name[,c(2,1,3)]
FW_name <- `colnames<-`(FW_name, c("sp_taxon_1", "sp_taxon_2", "value"))
FW_name$sp_taxon_1 <- str_replace_all(FW_name$sp_taxon_1, "Linnaenya sp1", "Linnaemya sp1")
#------------------------------
# Set taxo_back and taxa table
#------------------------------
# Create taxo_back_df

## Get Unique taxa of data
taxa <- c(as.vector(unique(FW_name$sp_taxon_2)), as.vector(unique(FW_name$sp_taxon_1)))
taxa <- str_replace_all(taxa, "Linnaenya sp1", "Linnaemya sp1")

### Check for spelling mistakes... ###
taxa_resolve <- gnr_resolve(taxa, canonical = T, best_match_only = T)
temp <- unlist(attributes(taxa_resolve)$not_known)

if(length(temp) != 0){
  sp_not_known <- data.frame(user_supplied_name=unlist(attributes(taxa_resolve)$not_known) , submitted_name=unlist(attributes(taxa_resolve)$not_known), matched_name=unlist(attributes(taxa_resolve)$not_known), NA, NA)
  names(sp_not_known) <- names(taxa_resolve)
  sp_not_known$submitted_name <-  paste0(str_to_upper(str_extract(sp_not_known$user_supplied_name, ".{1}")), str_remove(sp_not_known$user_supplied_name, ".{1}"))
  sp_not_known$matched_name2 <- sp_not_known$submitted_name
  taxa_resolve <- rbind(taxa_resolve, sp_not_known)
}
# Create taxa_df
taxons_df <- data.frame(taxa_resolve$user_supplied_name, taxa_resolve$matched_name2, stringsAsFactors = F)
names(taxons_df) <- c("original_name", "name_clear")
taxons_df$name_clear <- str_remove_all(taxons_df$name_clear, '\\s\\(.*\\)$')



#####
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
taxa_back_df[25,4] <- "25335"
taxa_back_df[39,4] <- "780594"
taxa_back_df[63,4] <- "34936"
taxa_back_df[107,4] <- "576051"
taxa_back_df[112,4] <- "752538"
taxa_back_df[119,4] <- "650646"
taxa_back_df[122,4] <- "753000"
taxa_back_df[129,4] <- "616885"
taxa_back_df[134,4] <- "835492"
taxa_back_df[153,4] <- "752183"
taxa_back_df[181,4] <- "650708"
taxa_back_df[195,4] <- "935979"
taxa_back_df[217,4] <- "114676"
taxa_back_df[227,4] <- "650674"

#------------------------------
# Set traits table
#------------------------------

# trait_df <- read.csv2(file = "mangal-datasets/FW_name/data/FW_name_trait.csv", header = TRUE)

# trait_df <- melt(trait_df, id.vars = c("taxon"), na.rm = TRUE)
# names(trait_df) <- c("taxon", "name", "value")

#------------------------------
# Writing taxa and interaction table
#------------------------------

write.csv2(x = taxa_back_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Yamazaki_Kato_2003/data/Yamazaki_Kato_2003_taxa_back.csv", row.names = FALSE)
write.csv2(x = taxons_df, file = "mangal-datasets-ben/Ones_done_before_Patrick/Yamazaki_Kato_2003/data/Yamazaki_Kato_2003_taxa.csv", row.names = FALSE)
write.csv2(x = FW_name, file = "mangal-datasets-ben/Ones_done_before_Patrick/Yamazaki_Kato_2003/data/Yamazaki_Kato_2003_inter.csv", row.names = FALSE)
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
POST_interaction(inter_df = FW_name, attr = attr_inter, inter = inter, users = users, network = networks, enviro = enviro)

rm(lat, lon, srid, attr_inter, refs, users, enviro, datasets, traits, networks, inter, taxons_df, taxo_back_df, matrice.interaction, FW_name, taxa_back_df, taxa_resolve, i, path, server, taxa, temp, without_., without_str)
